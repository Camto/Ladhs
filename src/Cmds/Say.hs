module Cmds.Say (say) where

import qualified Calamity as C
import qualified Calamity.Commands as C
import qualified Calamity.Commands.Command as C
import qualified Calamity.HTTP.Channel as C
import qualified Calamity.Cache.Eff as C
import qualified Calamity.Metrics.Eff as C

import Control.Monad
import Control.Lens

import qualified Data.Text.Lazy as L

import qualified Polysemy as P
import qualified Polysemy.Embed as P

import qualified Utils as U

say ::
	P.Sem (
		C.DSLState (
			C.SetupEff
				'[C.ParsePrefix, C.MetricEff, C.CacheEff, P.Embed IO, P.Final IO]))
		C.Command

say = C.command @'[C.KleeneStarConcat L.Text] "say" $ \ctx text -> do
	void $ U.send_text (ctx ^. #channel) text
	void $ U.delete_msg (ctx ^. #channel) $ (ctx ^. #message)