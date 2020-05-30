module Cmds.Dino (dino) where

import qualified Calamity as C
import qualified Calamity.Commands as C
import qualified Calamity.Commands.Command as C
import qualified Calamity.HTTP.Channel as C
import qualified Calamity.Cache.Eff as C
import qualified Calamity.Metrics.Eff as C

import Control.Monad
import Control.Lens
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Aeson as AS
import qualified Data.HashMap.Strict as HM

import qualified Data.Text.Lazy as L

import qualified Polysemy as P
import qualified Polysemy.Embed as P

import qualified Utils as U

dino ::
	(HM.HashMap T.Text AS.Value) ->
	(HM.HashMap T.Text T.Text) ->
	(HM.HashMap T.Text T.Text) ->
	P.Sem (
		C.DSLState (
			C.SetupEff
				'[C.ParsePrefix, C.MetricEff, C.CacheEff, P.Embed IO, P.Final IO]))
		C.Command

dino icons emojis dinos =
	C.command @'[Maybe L.Text] "dino" $ \ctx text ->
		return ()
