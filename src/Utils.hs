module Utils (
	get_json,
	embed_color,
	embed_msg,
	text_msg,
	simple_embed,
	send,
	send_embed,
	send_text,
	delete_msg,
	pick_one_hm
) where

import qualified Calamity as C
import qualified Calamity.Commands as C
import qualified Calamity.Commands.Context as CC
import qualified Calamity.HTTP.Channel as C
import qualified Calamity.Types.Model.Channel.Embed as C
-- I know I'm not supposed to import Internal but that's the only way to make the typechecker happy with send's definition, and I wanna use .: dang it!
import Calamity.HTTP.Internal.Request as C

import Data.Composition
import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Word as W
import Data.Colour.SRGB (sRGB24)
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as AS
import qualified Data.HashMap.Strict as HM
import qualified System.Random.Pick as R

import qualified Polysemy as P

get_json :: (AS.FromJSON a) => FilePath -> IO (Maybe a)
get_json filename =
	(B.readFile $ "Data/" <> filename <> ".json") >>= return . AS.decode

embed_color = sRGB24 0xe0 0x7b 0xb8

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
	C.embed = Nothing,
	C.allowedMentions = Nothing
}

embed_msg embed = empty_msg_opts & #embed .~ Just embed

text_msg text = empty_msg_opts & #content .~ (Just $ L.toStrict text)

send ::
	(C.BotC r, C.HasID C.Channel c) =>
	c -> C.CreateMessageOptions ->
		P.Sem r (Either
			C.RestError
			(C.Result (C.ChannelRequest C.Message)))
send = C.invoke .: C.CreateMessage

send_embed chnl embed = send chnl $ embed_msg embed

send_text chnl text = send chnl $ text_msg text

delete_msg ::
	(C.BotC r, C.HasID C.Channel c, C.HasID C.Message m) =>
	c -> m -> P.Sem r (Either C.RestError ())
delete_msg = C.invoke .: C.DeleteMessage

pick_one_hm hm = R.pickOne $ HM.keys hm