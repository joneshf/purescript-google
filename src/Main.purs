module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Generic.Rep (class Generic)
import Network.HTTP.Affjax (AJAX)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:>), Capture, Resource)
import Type.Trout.Client (asClients)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)
import Type.Trout.PathPiece (class ToPathPiece)

main :: forall e. Eff ( ajax :: AJAX, exception :: EXCEPTION, console :: CONSOLE | e) Unit
main = void do
  let messages = foo (UserId "me") (MessageId "0")
  launchAff messages."GET"

newtype UserId
  = UserId String

derive newtype instance toPathPieceUserId :: ToPathPiece UserId

newtype MessageId
  = MessageId String

derive instance genericMessageId :: Generic MessageId _

derive newtype instance toPathPieceMessageId :: ToPathPiece MessageId

instance decodeJsonMessageId :: DecodeJson MessageId where
  decodeJson = genericDecodeJson

newtype ThreadId
  = ThreadId String

derive instance genericThreadId :: Generic ThreadId _

instance decodeJsonThreadId :: DecodeJson ThreadId where
  decodeJson = genericDecodeJson

newtype Message
  = Message
    { id :: MessageId
    , threadId :: ThreadId
    }

derive instance genericMessage :: Generic Message _

instance decodeJsonMessage :: DecodeJson Message where
  decodeJson = genericDecodeJson

type API
  = "https://www.googleapis.com/gmail/v1/users"
    :/ Capture "userId" UserId
    :> "messages"
    :/ Capture "id" MessageId
    :> Resource (Get Message JSON)

api :: Proxy API
api = Proxy

foo :: forall e. UserId -> MessageId -> { "GET" :: Aff ( ajax :: AJAX | e ) Message }
foo = asClients api
