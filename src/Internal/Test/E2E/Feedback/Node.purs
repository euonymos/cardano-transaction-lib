-- | A module for communication between E2E test suite and a headless browser.
-- | This module exposes API for the NodeJS side.
-- | See `Ctl.Internal.Test.E2E.Feedback.Browser` for the corresponding APIs
-- | for the NodeJS side.
module Ctl.Internal.Test.E2E.Feedback.Node
  ( getBrowserEvents
  , subscribeToBrowserEvents
  , setClusterSetup
  ) where

import Prelude

import Aeson (decodeAeson, encodeAeson, parseJsonStringToAeson, stringifyAeson)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.QueryM (ClusterSetup)
import Ctl.Internal.Test.E2E.Feedback (BrowserEvent(Failure, Success))
import Data.Array as Array
import Data.Either (Either(Left), hush, note)
import Data.Foldable (and)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (for, traverse_)
import Effect (Effect)
import Effect.Aff
  ( Aff
  , Canceler(Canceler)
  , Milliseconds(Milliseconds)
  , delay
  , forkAff
  , killFiber
  , launchAff_
  , makeAff
  , try
  )
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error, throw)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1)
import Foreign (unsafeFromForeign)
import Toppokki as Toppokki

-- | React to events raised by the browser
subscribeToBrowserEvents
  :: Boolean
  -> Toppokki.Page
  -> (BrowserEvent -> Effect Unit)
  -> Aff Unit
subscribeToBrowserEvents passBrowserLogs page cont = do
  logs <- liftEffect $ Ref.new ""
  let
    addLogLine line = Ref.modify_ (flip append (line <> "\n")) logs
  liftEffect $ Toppokki.onConsole
    ( mkEffectFn1 \cm -> launchAff_ do
        -- either log right now or safe for later (to log in case of an error)
        if passBrowserLogs then
          Toppokki.consoleMessageText cm >>= liftEffect <<< Console.log <<<
            append "[chrome]: "
        else
          Toppokki.consoleMessageText cm >>= liftEffect <<< addLogLine
    )
    page
  makeAff \f -> do
    liftEffect $ Toppokki.onPageError
      ( mkEffectFn1
          ( \err -> do
              -- log everything if we haven't been passing the logs on the fly
              when (not passBrowserLogs) do
                Ref.read logs >>= Console.log
              f $ Left err
          )
      )
      page
    let
      -- Accepts a number of attempts left.
      -- An attempt is successful if we get at least one event.
      process :: Maybe Int -> Aff Unit
      process attempts = do
        events <- getBrowserEvents page
        continue <- and <$> for events \event -> do
          void $ liftEffect $ try $ cont event
          case event of
            Success -> pure false
            Failure err -> liftEffect $ throw err
            _ -> pure true
        if continue then do
          delay $ Milliseconds $ 1000.0
          if Array.length events == 0 && attempts /= Just 0 then
            process (flip sub one <$> attempts)
          else if attempts == Just 0 then liftEffect $ f $ Left $ error
            "Timeout reached when trying to connect to CTL Contract running\
            \ in the browser. Is there a Contract with E2E hooks available\
            \ at the URL you provided? Did you forget to run `npm run \
            \e2e-serve`?"
          else process Nothing
        else pure unit

    processFiber <- Ref.new Nothing
    launchAff_ do
      liftEffect <<< flip Ref.write processFiber <<< Just =<< forkAff do
        try (process (Just firstTimeConnectionAttempts)) >>= liftEffect <<< f
    pure $ Canceler \e -> do
      liftEffect (Ref.read processFiber) >>= traverse_ (killFiber e)
  where
  -- How many times to try until we get any event?
  firstTimeConnectionAttempts :: Int
  firstTimeConnectionAttempts = 30

getBrowserEvents
  :: Toppokki.Page -> Aff (Array BrowserEvent)
getBrowserEvents page = do
  frgn <- Toppokki.unsafeEvaluateStringFunction collectEventsJS page
  let
    (encodedEvents :: Array String) = unsafeFromForeign frgn
  for encodedEvents \event -> do
    liftEither $ note (error $ "Unable to decode BrowserEvent from: " <> event)
      $ hush
      $ decodeAeson =<< parseJsonStringToAeson event
  where
  collectEventsJS :: String
  collectEventsJS =
    """
      (() => {
        const res = window.ctlE2ECommunications || [];
        window.ctlE2ECommunications = [];
        return res;
      })()
      """

-- | Injects cluster setup value into the JS context of the page, where it can
-- | be retrieved using `getClusterSetup`.
setClusterSetup :: Toppokki.Page -> ClusterSetup -> Aff Unit
setClusterSetup page clusterSetup = do
  let
    jsCode = "(() => window.ctlE2EClusterSetup = ("
      <> stringifyAeson (encodeAeson clusterSetup)
      <> "))()"
  void $ Toppokki.unsafeEvaluateStringFunction jsCode page
