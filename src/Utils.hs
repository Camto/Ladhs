{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils (
	embed_color,
	embed_msg,
	text_msg,
	simple_embed,
	send,
	send_embed
) where

import qualified Calamity as C
import qualified Calamity.Commands as C
import qualified Calamity.Commands.Context as CC
import qualified Calamity.HTTP.Channel as C
import qualified Calamity.Types.Model.Channel.Embed as C

import qualified Data.Text.Lazy as L
import qualified Data.Word as W

embed_color = fromInteger 0xe07bb8 :: W.Word64

empty_embed = C.Embed {
	C.title = Nothing,
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

simple_embed title descr = C.Embed {
	C.title = Just title,
	C.type_ = Nothing,
	C.description = Just descr,
	C.url = Nothing,
	C.timestamp = Nothing,
	C.color = Just embed_color,
	C.footer = Nothing,
	C.image = Nothing,
	C.thumbnail = Nothing,
	C.video = Nothing,
	C.provider = Nothing,
	C.author = Nothing,
	C.fields = []
}

embed_msg embed = C.CreateMessageOptions {
	C.content = Nothing,
	C.nonce = Nothing,
	C.tts = Nothing,
	C.file = Nothing,
	C.embed = Just embed
}

text_msg text = C.CreateMessageOptions {
	C.content = Just $ L.toStrict text,
	C.nonce = Nothing,
	C.tts = Nothing,
	C.file = Nothing,
	C.embed = Nothing
}

send channel msg =
	C.invoke $ C.CreateMessage channel msg

send_embed channel embed =
	send channel $ embed_msg embed