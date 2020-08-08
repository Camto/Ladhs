module Main where

import qualified Calamity as C
import qualified Calamity.Cache.InMemory as C
import qualified Calamity.Commands as C
import qualified Calamity.Commands.Context as CC
import qualified Calamity.Metrics.Noop as C
import qualified Calamity.HTTP.Channel as C
import qualified Calamity.HTTP.User as C
--import qualified Calamity.Types.Model.User as C
import qualified Calamity.Types.Model.Channel.Embed as C
import qualified Calamity.Types.Model.Channel.Message as C

-- import Control.Concurrent
import Control.Lens
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Strict.Lens
import qualified Data.Aeson as AS
import qualified Data.HashMap.Strict as HM
import TextShow
import System.Environment

import qualified Polysemy as P
import qualified Polysemy.Async as P
import qualified Polysemy.Embed as P
import qualified Polysemy.Fail as P

import qualified DiPolysemy as DiP

import qualified Utils as U

import Cmds.Say
import Cmds.Ping
import Cmds.Dino

main :: IO ()
main = do
	token <- view packed <$> getEnv "TOKEN"
	-- I know this isn't pure, but the program *should* crash if it can't find all the data files.
	Just (icons :: HM.HashMap T.Text AS.Value) <- U.get_json "icons"
	Just (emojis :: HM.HashMap T.Text T.Text) <- U.get_json "emojis"
	Just (dinos :: HM.HashMap T.Text T.Text) <- U.get_json "dinos"
	void . P.runFinal . P.embedToFinal .
		C.runCacheInMemory . C.runMetricsNoop .
			C.useConstantPrefix "l." .
				C.runBotIO (C.BotToken $ L.fromStrict token) $ do
				err_self <- C.invoke C.GetCurrentUser
				case err_self of
					Right self -> do
						C.addCommands $ do
							say
							ping
							dino icons dinos
						{-C.react @'C.MessageCreateEvt $ \msg ->
							when (
								C.getID (msg ^. #author) /= (self ^. #id)) $ do
							something-}
					Left _ -> P.embed mzero