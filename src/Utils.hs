{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Utils (
	embed_color,
	embed_msg,
	text_msg,
	simple_embed,
	send,
	send_embed,
	send_text,
	delete_msg
) where

import qualified Calamity as C
import qualified Calamity.Commands as C
import qualified Calamity.Commands.Context as CC
import qualified Calamity.HTTP.Channel as C
import qualified Calamity.Types.Model.Channel.Embed as C

import Control.Lens
import qualified Data.Text.Lazy as L
import qualified Data.Word as W

embed_color = fromInteger 0xe07bb8 :: W.Word64

empty_embed = C.Embed {
	C.title = Nothing,
	C.type_ = Nothing,
	C.description = Nothing,
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

simple_embed title descr =
	empty_embed
		& #title .~ Just title
		& #description .~ Just descr

empty_msg_opts = C.CreateMessageOptions {
	C.content = Nothing,
	C.nonce = Nothing,
	C.tts = Nothing,
	C.file = Nothing,
	C.embed = Nothing
}

embed_msg embed = empty_msg_opts & #embed .~ Just embed

text_msg text = empty_msg_opts & #content .~ (Just $ L.toStrict text)

send chnl msg = C.invoke $ C.CreateMessage chnl msg

send_embed chnl embed = send chnl $ embed_msg embed

send_text chnl text = send chnl $ text_msg text

delete_msg chnl msg = C.invoke $ C.DeleteMessage chnl msg