{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import qualified Calamity as C
import qualified Calamity.Cache.InMemory as C
import qualified Calamity.Commands as C
import qualified Calamity.Commands.Context as CC
import qualified Calamity.Metrics.Noop as C
import qualified Calamity.HTTP.Channel as C
import qualified Calamity.Types.Model.Channel.Embed as C

-- import Control.Concurrent
import Control.Lens
import Control.Monad

import qualified Data.Text.Lazy as L
import Data.Text.Strict.Lens

import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Embed as P
import qualified Polysemy.Fail as P

import qualified DiPolysemy as DiP

import TextShow

import System.Environment

tellt :: (C.BotC r, C.Tellable t) => t -> L.Text -> P.Sem r (Either C.RestError C.Message)
tellt t m = C.tell t $ L.toStrict m

embed_options embed = C.CreateMessageOptions {
	C.content = Nothing,
	C.nonce = Nothing,
	C.tts = Nothing,
	C.file = Nothing,
	C.embed = Just embed
}

main :: IO ()
main = do
	token <- view packed <$> getEnv "TOKEN"
	void . P.runFinal . P.embedToFinal .
		C.runCacheInMemory . C.runMetricsNoop .
			C.useConstantPrefix "l." $
				C.runBotIO (C.BotToken $ L.fromStrict token) $ do
				C.addCommands $ do
					C.command @'[] "ping" $ \ctx -> do
						DiP.info . showt $ CC.channel ctx
						void . C.invoke . C.CreateMessage (CC.channel ctx) . embed_options $ C.Embed {
							C.title = Just ":ping_pong: Pong!",
							C.type_ = Nothing,
							C.description = Nothing,
							C.url = Nothing,
							C.timestamp = Nothing,
							C.color = Nothing,
							C.footer = Nothing,
							C.image = Nothing,
							C.thumbnail = Nothing,
							C.video = Nothing,
							C.provider = Nothing,
							C.author = Nothing,
							C.fields = []
						}