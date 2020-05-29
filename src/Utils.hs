{-# LANGUAGE DisambiguateRecordFields #-}

module Utils (
	embed_options
) where

import qualified Calamity as C
import qualified Calamity.Commands as C
import qualified Calamity.Commands.Context as CC
import qualified Calamity.HTTP.Channel as C
import qualified Calamity.Types.Model.Channel.Embed as C

import qualified Data.Word as W

embed_color = fromInteger 0xe07bb8 :: W.Word64

embed_options embed = C.CreateMessageOptions {
	C.content = Nothing,
	C.nonce = Nothing,
	C.tts = Nothing,
	C.file = Nothing,
	C.embed = Just embed
}