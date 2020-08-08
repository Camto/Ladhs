module Cmds.Dino (dino) where

import qualified Calamity as C
import qualified Calamity.Commands as C
import qualified Calamity.Commands.Command as C
import qualified Calamity.HTTP.Channel as C
import qualified Calamity.Cache.Eff as C
import qualified Calamity.Metrics.Eff as C

import Debug.Trace
import Control.Monad
import Control.Lens
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Aeson as AS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified System.Random.Pick as R
import qualified Text.Fuzzy as F
import qualified Safe as S

import qualified Polysemy as P
import qualified Polysemy.Embed as P

import qualified Utils as U

dino ::
	(HM.HashMap T.Text AS.Value) ->
	(HM.HashMap T.Text T.Text) ->
	P.Sem (
		C.DSLState (
			C.SetupEff
				'[C.ParsePrefix, C.MetricEff, C.CacheEff, P.Embed IO, P.Final IO]))
		C.Command

dino icons dinos =
	C.command @'[Maybe L.Text] "dino" $ \ctx name -> do
		dino' <- case name of
			Just name' ->
				return . L.fromStrict . fromMaybe default_dino $
					S.atMay (F.simpleFilter (L.toStrict name') dino_set) 0
			Nothing -> (P.embed $ U.pick_one_hm dinos) <&> L.fromStrict
		let AS.Array dino_icons = icons HM.! "dinos"
		AS.String icon <- P.embed . R.pickOne $ V.toList dino_icons
		void $ U.send_embed (ctx ^. #channel) $
			U.empty_embed
				& #description .~ Just (L.fromStrict . (HM.!) dinos $ L.toStrict dino')
				& #author .~ Just (U.empty_author
					& #name .~ Just (last . L.splitOn "#" $ L.replace "_" " " dino')
					& #url .~ Just (L.append "https://en.wikipedia.org/wiki/" dino')
					& #iconUrl .~ Just (L.fromStrict icon))
	where
		dino_set = HM.keys dinos
		default_dino = dino_set !! 0