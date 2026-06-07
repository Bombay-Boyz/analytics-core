-- | Analytics.Core.Event.Event
--
-- EventBus: a bounded, STM-backed publish/subscribe bus.
--
-- Dependency order:
--   Analytics.Core.Types
--   Analytics.Core.Event.Types
--   ← this module

module Analytics.Core.Event.Event
  ( -- * Bus construction
    EventBus(..)
  , makeBoundedEventBus
  , makeNoOpEventBus
    -- * Subscription record
  , EventSubscription(..)
    -- * Filter application (exposed for testing)
  , matchesFilter
  ) where

import Relude
import Control.Concurrent              (threadDelay)
import Control.Concurrent.Async        (async, cancel, race)
import Control.Concurrent.STM          (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import qualified Data.Set              as Set
import Control.Exception               (catch)
import Data.UUID                       (nil)
import Data.UUID.V4                    (nextRandom)
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as Text

import Analytics.Core.Types
import Analytics.Core.Fact  (factRecordType)
import Analytics.Core.Event.Types

-- ---------------------------------------------------------------------------
-- EventSubscription

data EventSubscription = EventSubscription
  { esId      :: !SubscriptionId
  , esFilter  :: !EventFilter
  , esHandler :: Event -> IO ()
  }

-- ---------------------------------------------------------------------------
-- EventBus record

data EventBus = EventBus
  { ebPublish     :: Event -> IO ()
  , ebSubscribe   :: EventFilter -> (Event -> IO ()) -> IO SubscriptionId
  , ebUnsubscribe :: SubscriptionId -> IO ()
  , ebShutdown    :: IO ()
    -- ^ Cancel the dispatcher thread. Called once by the owner (Runtime)
    -- on shutdown. Idempotent — safe to call more than once.
  }

-- ---------------------------------------------------------------------------
-- Filter application

-- | Decide whether an event should be delivered to a subscription.
-- Pure and exported for property-based testing.
matchesFilter :: EventFilter -> Event -> Bool
matchesFilter AllEvents              _  = True
matchesFilter (EventsOfType types)   ev = eventType ev `elem` toList types
matchesFilter (EventsFromPlugin pid) ev =
  case ev of
    -- Routed by fact type namespace:
    EvFactAsserted          f       -> pluginOwnsType (factRecordType f)
    EvFactRetracted         _ t     -> pluginOwnsType t
    -- EvFactPhysicallyDeleted second field is an administrative reason string,
    -- not a namespaced type — not routable to a plugin subscriber.
    EvFactPhysicallyDeleted _ _     -> False

    -- Routed by carried PluginId:
    EvFactRejected          p _ _   -> p == pid
    EvRuleFired             _ p _ _ -> p == pid
    EvRuleDisabled          _ p _ _ -> p == pid
    EvPluginLoaded          p _     -> p == pid
    EvPluginFailed          p _     -> p == pid
    EvPluginRejected        p _     -> p == pid

    -- Routed by predicate ID namespace ("pluginId:localName"):
    EvContradictionDetected _ _ t   -> pluginOwnsType t

    -- Not plugin-scoped; explicit False preserves -Wincomplete-patterns:
    EvInferenceStarted      _       -> False
    EvInferenceCompleted    _       -> False
    EvInferenceLimitReached _ _     -> False
    EvSnapshotCreated       _       -> False
    EvSubscriberTimeout     _       -> False
    EvRuntimeFault          _ _     -> False
  where
    pluginOwnsType t = (unPluginId pid <> ":") `Text.isPrefixOf` t

-- ---------------------------------------------------------------------------
-- Bounded TBQueue-backed bus

-- | Timeout for a single subscriber handler invocation, in microseconds.
subscriberTimeoutUs :: Int
subscriberTimeoutUs = 5000000

-- | Construct a production-grade bounded event bus and start its dispatcher.
makeBoundedEventBus :: Int -> IO EventBus
makeBoundedEventBus queueBound = do
  queue        <- newTBQueueIO (fromIntegral (max 1 queueBound))
  subsVar      <- newTVarIO (Map.empty :: Map SubscriptionId EventSubscription)
  timedOutVar  <- newTVarIO (Set.empty :: Set SubscriptionId)
  dispThread   <- async (dispatcher queue subsVar timedOutVar)  -- keep handle
  pure EventBus
    { ebPublish     = \ev -> atomically (writeTBQueue queue ev)
    , ebSubscribe   = \filt handler -> do
        sid <- SubscriptionId <$> nextRandom
        let sub = EventSubscription sid filt handler
        atomically $ modifyTVar' subsVar (Map.insert sid sub)
        pure sid
    , ebUnsubscribe = \sid ->
        atomically $ do
          modifyTVar' subsVar     (Map.delete sid)
          modifyTVar' timedOutVar (Set.delete sid)
    , ebShutdown    = cancel dispThread
    }

-- | Dispatcher loop: read one event, deliver to all matching subscribers.
dispatcher
  :: TBQueue Event
  -> TVar (Map SubscriptionId EventSubscription)
  -> TVar (Set SubscriptionId)   -- ^ subscribers that have already timed out
  -> IO ()
dispatcher queue subsVar timedOutVar = forever $ do
  ev   <- atomically (readTBQueue queue)
  subs <- atomically (readTVar subsVar)
  mapM_ (deliverOne queue timedOutVar ev) (Map.elems subs)

-- | Deliver one event to one subscriber under a timeout.
--
-- On timeout, emits 'EvSubscriberTimeout' onto the bus at most once per
-- subscriber. A 'TVar (Set SubscriptionId)' guards against repeated emissions
-- that could saturate the queue if a slow subscriber keeps timing out.
deliverOne
  :: TBQueue Event
  -> TVar (Set SubscriptionId)   -- ^ already-timed-out subscriber IDs
  -> Event
  -> EventSubscription
  -> IO ()
deliverOne queue timedOutVar ev sub
  | not (matchesFilter (esFilter sub) ev) = pure ()
  | otherwise = do
      result <- race
        (threadDelay subscriberTimeoutUs)
        (esHandler sub ev `catch` \(_ :: SomeException) -> pure ())
      case result of
        Right () -> pure ()
        Left  () -> do
          alreadyTimedOut <- readTVarIO timedOutVar
          unless (Set.member (esId sub) alreadyTimedOut) $ atomically $ do
            modifyTVar' timedOutVar (Set.insert (esId sub))
            writeTBQueue queue (EvSubscriberTimeout (esId sub))

-- ---------------------------------------------------------------------------
-- No-op bus (for unit tests only — never use in production)

makeNoOpEventBus :: EventBus
makeNoOpEventBus = EventBus
  { ebPublish     = \_ -> pure ()
  , ebSubscribe   = \_ _ -> pure (SubscriptionId nil)
  , ebUnsubscribe = \_ -> pure ()
  , ebShutdown    = pure ()
  }
