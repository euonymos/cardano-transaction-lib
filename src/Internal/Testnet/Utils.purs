module Ctl.Internal.Testnet.Utils
  ( EventSource(EventSource)
  , TestnetCleanupRef
  , addCleanup
  , after
  , annotateError
  , cleanupOnExit
  , findNodeDirs
  , findTestnetPaths
  , getNodePort
  , getRuntime
  , makeEventSource
  , onLine
  , readGenesisKey
  , readNodes
  , runCleanup
  , scheduleCleanup
  , suppressAndLogErrors
  , tmpdir
  , tmpdirUnique
  , tryAndLogErrors
  , waitFor
  , waitForClose
  , waitForError
  , waitForEvent
  , waitUntil
  , whenError
  ) where

import Contract.Prelude hiding (log)

import Contract.Config as Contract.Config
import Contract.TextEnvelope
  ( TextEnvelope(TextEnvelope)
  , TextEnvelopeType(PaymentSigningKeyShelleyed25519)
  , decodeTextEnvelope
  )
import Contract.Wallet.KeyFile (privatePaymentKeyFromTextEnvelope)
import Control.Alt ((<|>))
import Control.Monad.Error.Class
  ( class MonadError
  , catchError
  , liftMaybe
  , throwError
  )
import Control.Monad.Rec.Class (Step(Done, Loop), tailRecM)
import Control.Parallel (parOneOf, parallel, sequential)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Logging (Logger)
import Ctl.Internal.QueryM.UniqueId (uniqueId)
import Ctl.Internal.Spawn
  ( ManagedProcess(ManagedProcess)
  , OnSignalRef
  , removeOnSignal
  , waitForSignal
  )
import Ctl.Internal.Testnet.Types
  ( GenesisUtxoKeyLocation
  , Node
  , NodeLocation
  , TestnetPaths
  , TestnetRuntime
  )
import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.Posix.Signal (toString) as Signal
import Data.String (Pattern(Pattern))
import Data.String as String
import Data.Time.Duration (class Duration, fromDuration)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Aff (try)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error, message)
import Effect.Random (randomInt)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.ChildProcess as Node.ChildProcess
import Node.Encoding (Encoding(UTF8))
import Node.Encoding as Node.Encoding
import Node.FS.Sync (exists, mkdir) as FSSync
import Node.FS.Sync as Node.FS
import Node.FS.Sync as Node.FS.Sync
import Node.Path (FilePath)
import Node.Process as Process
import Node.ReadLine as RL
import Node.Stream (Readable)

waitFor :: forall a e. EventSource e -> (e -> Maybe a) -> Aff a
waitFor source f = flip tailRecM unit \_ -> do
  event <- waitForEvent source
  pure case f event of
    Just a -> Done a
    Nothing -> Loop unit

getRuntime :: TestnetPaths -> Effect TestnetRuntime
getRuntime paths@{ nodeDirs, testnetDirectory } = do
  nodes <- readNodes nodeDirs testnetDirectory
  pure { nodes, paths }

readNodes :: Array NodeLocation -> FilePath -> Effect (Array Node)
readNodes nodeDirs testnetDir =
  for nodeDirs \{ idx, workdir } -> do
    let socketPath = testnetDir <</>> "socket" <</>> ("node" <> show idx)
    exists <- Node.FS.exists socketPath
    unless exists do
      throwError $ error $ "readNodes: could not find node socket at "
        <> socketPath
    port <- getNodePort testnetDir idx
    pure
      { socket: socketPath
      , port
      , location:
          { idx
          , workdir
          }
      }

-- | Changes TextEnvelope type to match private payment key one and tries to read that.
readTextEnvelopeAsPaymentSkey
  :: FilePath
  -> Effect Contract.Config.PrivatePaymentKey
readTextEnvelopeAsPaymentSkey path = do
  TextEnvelope envelope <-
    liftMaybe (error "Cannot decode skey envelope")
      <<< decodeTextEnvelope
      =<< Node.FS.Sync.readTextFile Node.Encoding.UTF8 path
  let
    envelope' = TextEnvelope
      (envelope { type_ = PaymentSigningKeyShelleyed25519 })
  liftMaybe (error "Cannot decode payment skey from decoded envelope")
    $ privatePaymentKeyFromTextEnvelope envelope'

parseUtxoKeyFilename :: FilePath -> Either Error (Maybe { idx :: Int })
parseUtxoKeyFilename path =
  traverse
    ( map { idx: _ }
        <<< note (error "Can't parse genesis key index")
        <<< Int.fromString
    )
    (String.stripPrefix (Pattern "utxo") path)

readGenesisKeyLocations :: FilePath -> Effect (Array GenesisUtxoKeyLocation)
readGenesisKeyLocations workdir = do
  let keysDir = workdir <</>> "utxo-keys"
  filenames <- Node.FS.readdir keysDir
  map Array.catMaybes
    $ liftEither
    $ for filenames \filename ->
        parseUtxoKeyFilename filename <#> map \{ idx } ->
          { idx
          , path: keysDir <</>> filename <</>> "utxo.skey"
          }

readGenesisKey
  :: GenesisUtxoKeyLocation -> Effect Contract.Config.PrivatePaymentKey
readGenesisKey = readTextEnvelopeAsPaymentSkey <<< _.path

getNodePort :: FilePath -> Int -> Effect UInt
getNodePort testnetDir nodeIdx = do
  let portPath = testnetDir <</>> ("node-data/node" <> show nodeIdx <> "/port")
  portStr <- Node.FS.readTextFile UTF8 portPath
  liftMaybe (error $ "Failed to parse port at " <> portPath) $
    UInt.fromString portStr

findNodeDirs :: FilePath -> Effect (Array NodeLocation)
findNodeDirs workdir = do
  let poolsKeysDir = workdir <</>> "pools-keys"
  Node.FS.readdir poolsKeysDir <#> \subdirs ->
    flip Array.mapMaybe subdirs \dirname -> do
      idx <- Int.fromString =<< String.stripPrefix (Pattern "pool") dirname
      pure
        { idx
        , workdir: poolsKeysDir <</>> dirname
        }

findTestnetPaths :: FilePath -> Effect TestnetPaths
findTestnetPaths workdir = do
  let
    nodeConfigPath = workdir <</>> "configuration.yaml"
    firstNode = "socket/node1/sock"
    nodeSocketPath = workdir <</>> firstNode
  workdirExists <- Node.FS.exists workdir
  configPathExists <- Node.FS.exists nodeConfigPath
  socketPathExists <- Node.FS.exists nodeSocketPath
  unless workdirExists do
    throwError $ error $
      "findTestnetPaths: cardano-testnet working directory not found."
  unless configPathExists do
    throwError $ error $
      "findTestnetPaths: 'configuration.yaml' not found in cardano-testnet \
      \working directory."
  unless socketPathExists do
    throwError $ error
      $ "findTestnetPaths: "
      <> firstNode
      <> " not found in cardano-testnet working directory."
  nodeDirs <- findNodeDirs workdir
  genesisKeys <- readGenesisKeyLocations workdir
  pure
    { testnetDirectory: workdir
    , nodeConfigPath
    , nodeSocketPath
    , genesisKeys
    , nodeDirs
    }

makeEventSource
  :: forall a b c
   . ( { handle :: Either Error a -> Effect Unit }
       -> Effect { unsubscribe :: Effect Unit, outcome :: c }
     )
  -> (a -> Maybe b)
  -> Effect { eventSource :: EventSource b, outcome :: c }
makeEventSource subscribeOnEvents filter = annotateError "make event source" do
  handlers <- Ref.new $ Map.fromFoldable []
  isCanceled <- Ref.new false
  cancelRef <- Ref.new mempty
  let
    markCanceled = Ref.write true isCanceled
    cancel error = Ref.read cancelRef >>= (_ $ error)
    subscribe handler = do
      Ref.read isCanceled >>=
        if _ then
          pure $ Left $ error "Event source is closed."
        else do
          id <- uniqueId "sub"
          let unsubscribe = Ref.modify_ (Map.delete id) handlers
          _ <- Ref.modify_
            (Map.insert id \event -> handler { unsubscribe, event })
            handlers
          pure $ Right { unsubscribe }

  { unsubscribe, outcome } <- subscribeOnEvents
    { handle: \ea -> case ea of
        Left error -> cancel error
        Right a -> case filter a of
          Just b -> do
            Ref.read handlers >>= traverse_ (_ $ Right b)
          Nothing -> pure unit
    }
  flip Ref.write cancelRef \error -> do
    Ref.write mempty cancelRef -- canceler may be called only once
    unsubscribe
    markCanceled
    Ref.read handlers >>= traverse_ \cont ->
      void $ suppressAndLogErrors "makeEventSource:cancel" $ cont $ Left error
    Ref.write (Map.fromFoldable []) handlers

  pure
    { eventSource: EventSource { cancel, subscribe }
    , outcome
    }

type TestnetCleanupRef = Ref (Array (Aff Unit))

addCleanup :: TestnetCleanupRef -> Aff Unit -> Effect Unit
addCleanup = map void <<< flip
  (Ref.modify <<< Array.cons <<< suppressAndLogErrors "[addCleanup][error]: ")

scheduleCleanup
  :: forall a
   . TestnetCleanupRef
  -> Aff a
  -> (a -> Aff Unit)
  -> Aff a
scheduleCleanup cleanupRef create cleanup =
  after create $ liftEffect <<< addCleanup cleanupRef <<< cleanup

-- Similar to `catchError` but preserves the error
whenError :: forall (a :: Type). Aff Unit -> Aff a -> Aff a
whenError whenErrorAction action = do
  res <- try action
  when (isLeft res) whenErrorAction
  liftEither res

-- | Just as a bracket but without the body.
after :: forall a. Aff a -> (a -> Aff Unit) -> Aff a
after first second = Aff.bracket first second pure

-- TODO: remove this function when PS bindings for os.tmpdir are available.
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/726
foreign import tmpdir :: Effect String

tmpdirUnique :: forall m. MonadEffect m => String -> m FilePath
tmpdirUnique suffix =
  liftEffect do
    tmpDir <- tmpdir
    randomStr <- uniqueId ""
    let dir = tmpDir <</>> randomStr <> "-" <> suffix
    dirExists <- FSSync.exists dir
    unless dirExists $ FSSync.mkdir dir
    pure dir

foreign import setLineHandler
  :: RL.Interface -> (String -> Effect Unit) -> Effect OnSignalRef

foreign import setCloseHandler
  :: RL.Interface -> Effect Unit -> Effect OnSignalRef

foreign import setErrorHandler
  :: RL.Interface -> (Error -> Effect Unit) -> Effect OnSignalRef

foreign import onBeforeExit
  :: Effect Unit -> Effect OnSignalRef

foreign import onExit
  :: (Int -> Effect Unit) -> Effect OnSignalRef

foreign import onUncaughtException
  :: (Error -> Effect Unit) -> Effect OnSignalRef

suppressAndLogErrors
  :: forall m. MonadEffect m => MonadError Error m => String -> m Unit -> m Unit
suppressAndLogErrors location = flip catchError $ message
  >>> append ("An error occured and suppressed at " <> location <> ": ")
  >>> log

-- replace with Effect.Console.log to debug. Not providing an option at runtime,
-- because it's just for the CTL developers.
log :: forall m. Monad m => String -> m Unit
log _ = pure unit

newtype EventSource b = EventSource
  { subscribe ::
      ( { unsubscribe :: Effect Unit
        , event :: Either Error b
        }
        -> Effect Unit
      )
      -> Effect (Either Error { unsubscribe :: Effect Unit })
  , cancel :: Error -> Effect Unit
  }

-- | Waits for any event. Note, if the event source throws an async error, any joining process dies.
waitForEvent :: forall a. EventSource a -> Aff a
waitForEvent (EventSource { subscribe }) = annotateError "waitForEvent" $
  Aff.makeAff \cont -> do
    subscriptionResult <- subscribe \{ unsubscribe, event } -> do
      unsubscribe
      cont event
    case subscriptionResult of
      Right { unsubscribe } -> pure $ Aff.Canceler \err -> liftEffect do
        unsubscribe
        cont $ Left $ appendErrorMessage "waitForEvent:canceled" err
      Left subError -> do
        suppressAndLogErrors "waitForEvent:badSubscription"
          $ cont
          $ Left
          $ appendErrorMessage "Failed to subscribe" subError
        pure Aff.nonCanceler

onLine
  :: forall a b
   . Readable a
  -> (String -> Maybe b)
  -> Effect (EventSource b)
onLine readable =
  map _.eventSource <<< makeEventSource \{ handle: mainHandler } -> do
    interface <- RL.createInterface readable mempty
    handlers <- Ref.new []
    lineHandler <- setLineHandler interface \x -> do
      void
        $ suppressAndLogErrors "onLine:setLineHandler"
        $ mainHandler
        $ Right x
    let
      cancel = \err -> do
        Ref.read handlers >>= traverse_ (try <<< removeOnSignal)
        void
          $ suppressAndLogErrors "onLine:cancel"
          $ mainHandler
          $ Left err
    closeHandler <- setCloseHandler interface
      $ cancel
      $ error "Line event source has been closed."
    errorHandler <- setErrorHandler interface cancel
    Ref.write [ lineHandler, closeHandler, errorHandler ] handlers
    pure
      { outcome: unit
      , unsubscribe: do
          cancel $ error "Unsubscribed from line event."
      }

-- | Waits until processe's stdout closes.
-- Assuming this means that process is closed as well.
waitForClose :: ManagedProcess -> Aff Unit
waitForClose (ManagedProcess _ child _) = do
  interface <- liftEffect
    $ flip RL.createInterface mempty
    $ Node.ChildProcess.stdout child
  Aff.makeAff \cont -> do
    { cancel } <- withOneShotHandler \{ justOnce } ->
      setCloseHandler interface $ justOnce $ cont $ Right unit
    pure $ Aff.Canceler \err -> liftEffect do
      cancel
      cont $ Left $ appendErrorMessage "waitForClose has been canceled" err

waitUntil :: forall a d. Duration d => d -> Aff (Maybe a) -> Aff a
waitUntil checkingInterval fa = flip tailRecM unit \_ ->
  fa >>= case _ of
    Nothing -> do
      Aff.delay $ fromDuration checkingInterval
      pure $ Loop unit
    Just x -> pure $ Done x

-- | Waits until processe's stdout closes.
-- Assuming this means that process is closed as well.
waitForError :: ManagedProcess -> Aff Error
waitForError (ManagedProcess _ child _) = do
  interface <- liftEffect
    $ flip RL.createInterface mempty
    $ Node.ChildProcess.stdout child
  Aff.makeAff \cont -> do
    { cancel } <- withOneShotHandler \{ justOnce } ->
      setErrorHandler interface \err -> justOnce $ cont $ Right err
    pure $ Aff.Canceler \err -> liftEffect do
      cancel
      cont $ Left $ appendErrorMessage "waitForClose has been canceled" err

-- | Specifically for nodejs handlers:
-- Makes sure that the callback is called at most once, and unregistering it
-- on cancelation and on the first call.
withOneShotHandler
  :: ({ justOnce :: Effect Unit -> Effect Unit } -> Effect OnSignalRef)
  -> Effect { cancel :: Effect Unit }
withOneShotHandler with = do
  removeHandler <- Ref.new mempty
  isClosedRef <- Ref.new false
  let
    cancel = do
      join $ Ref.read removeHandler
      Ref.write true isClosedRef
  handle <- with
    { justOnce: \oneShotHandler -> do
        -- otherwise it may be triggered multiple times, for unknown reason
        Ref.read isClosedRef >>= flip unless do
          cancel
          oneShotHandler
    }
  Ref.write (removeOnSignal handle) removeHandler
  pure { cancel }

waitForBeforeExit :: Aff Unit
waitForBeforeExit = Aff.makeAff \cont -> do
  { cancel } <- withOneShotHandler \{ justOnce } -> onBeforeExit $ justOnce do
    log "ON BEFORE EXIT"
    cont $ Right unit
  pure $ Aff.Canceler \err -> liftEffect do
    cancel
    suppressAndLogErrors "waitForBeforeExit" $ cont $ Left err

waitForUncaughtException :: Aff Error
waitForUncaughtException = Aff.makeAff \cont -> do
  n <- randomInt 0 100
  { cancel } <- withOneShotHandler \{ justOnce } ->
    onUncaughtException \err -> justOnce do
      log $ "ON UNCAUGHT EXCEPTION " <> show n
      cont $ Right err
  pure $ Aff.Canceler \err -> liftEffect do
    cancel
    suppressAndLogErrors "waitForUncaughtException" $ cont $ Left err

waitForExit :: Aff Int
waitForExit = Aff.makeAff \cont -> do
  { cancel } <- withOneShotHandler \{ justOnce } -> onExit \exitcode -> justOnce
    do
      cont $ Right exitcode
  pure $ Aff.Canceler \err -> liftEffect do
    cancel
    suppressAndLogErrors "waitForExit" $ cont $ Left err

tryAndLogErrors
  :: forall a m
   . MonadEffect m
  => MonadError Error m
  => Maybe Logger
  -> String
  -> m a
  -> m (Either Error a)
tryAndLogErrors logger location =
  try >=> case _ of
    Left err -> do
      maybe log (\l -> liftEffect <<< l Error) logger
        $ "An error occured and suppressed at "
        <> location
        <> ": "
        <> message err
      pure $ Left err
    Right a ->
      pure $ Right a

runCleanup :: Ref (Array (Aff Unit)) -> Aff Unit
runCleanup cleanupRef = do
  log "Cleaning up"
  cleanups <- liftEffect do
    cleanups <- Ref.read cleanupRef
    Ref.write [] cleanupRef
    pure cleanups
  if null cleanups then log "No cleanup needed"
  else do
    sequence_ $ suppressAndLogErrors "runCleanup" <$> cleanups
    log "Cleanup finished"

cleanupOnExit
  :: Ref (Array (Aff Unit))
  -> Aff { fiber :: Aff.Fiber Unit }
cleanupOnExit cleanupRef = do
  log "Cleanup scheduled"
  let
    handle handlers = do
      handler <- sequential do
        ( handlers.onExit
            <$> parallel waitForExit
        )
          <|>
            ( handlers.onUncaughtException
                <$> parallel waitForUncaughtException
            )
          <|>
            ( handlers.onBeforeExit
                <$ parallel waitForBeforeExit
            )
          <|>
            ( handlers.onWaitForSignal
                <$> parallel
                  (parOneOf [ waitForSignal SIGINT, waitForSignal SIGTERM ])
            )
      handler
    cleanup triggeredBy = do
      log $ "Running cleanup on " <> triggeredBy
      runCleanup cleanupRef

  fiber <- Aff.forkAff $ handle
    { onExit: \code -> cleanup $ "exit with " <> show code
    , onUncaughtException: \err -> do
        cleanup "uncaught exception"
        log $ "Failing irrecoverably after the cleanup after error: " <> show
          err
        liftEffect $ Process.exit 7 -- Failing irrecoverably
    , onBeforeExit: cleanup "before exit"
    , onWaitForSignal: cleanup <<< Signal.toString
    }
  pure { fiber }

annotateError
  :: forall (a :: Type) m
   . MonadError Error m
  => String
  -> m a
  -> m a
annotateError withPrefix action =
  catchError action $ throwError <<< appendErrorMessage withPrefix

appendErrorMessage
  :: String
  -> Error
  -> Error
appendErrorMessage withPrefix =
  error <<< append (withPrefix <> ": ") <<< message
