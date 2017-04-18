{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Mail.SendInBlue.Types.Message
    ( -- * Message type, constructor and lenses
      EmailMessage
    , emailMessage
    , emTo
    , emCc
    , emBcc
    , emFrom
    , emReplyTo
    , emSubject
    , emHtmlBody
    , emTextBody
    , emAttachments
    , emImages
      -- * Supporting types
    , Recipient
    , recipient
    , rAddress
    , rName
    , Sender
    , sender
    , sAddress
    , sName
    , FullName
    , fullName
    , Subject
    , subject
    , Header
    , header
    , HtmlBody
    , htmlBody
    , TextBody
    , textBody
    , Attachments
    , uriAttachments
    , base64Attachments
    , UriAttachment
    , uriAttachment
    , Base64Attachment
    , base64Attachment
    , Image
      -- * Response
    , Response(Success, Failure)
    , rsMessage
    , rsData
    , rfMessage
      -- * Re-exports
    , EmailAddress
    , Email.emailAddress
    , URI
    ) where

import           Control.Exception.Safe           (MonadThrow, throw)
import           Control.Monad                    (mzero)
import           Data.Aeson                hiding (Error, Success)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Base64        as Base64
import           Data.CaseInsensitive             (mk)
import qualified Data.HashMap.Strict           as Map
import           Data.List.NonEmpty               (NonEmpty(..))
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text, empty, pack)
import           Data.Text.Encoding               (decodeUtf8)
import qualified Data.Vector                   as Vector
import           GHC.Generics
import           Lens.Micro.TH                    (makeLenses)
import           Mail.SendInBlue.Types.Client
import           Network.URI                      (URI, uriToString)
import           Text.Email.Validate              (EmailAddress)
import qualified Text.Email.Validate           as Email


--------------------------------------------------------------------------------
-- Email message

-- | And 'EmailMessage' stores all the information needed to produce
-- an email message for sending via the SendInBlue API.  A minimal
-- instance must specify a sender, at least one recipient and an HTML
-- body.  Typically this should be constructed using the
-- 'emailMessage' smart constructor and modified via lenses.
data EmailMessage = EmailMessage
    { _emTo          :: !(NonEmpty Recipient)
    -- ^ List of one or more recipients.
    , _emCc          :: ![Recipient]
    -- ^ Recipients to cc the message to.
    , _emBcc         :: ![Recipient]
    -- ^ Recipients that will get a blanc cc of the message.
    , _emFrom        :: !Sender
    -- ^ The sender of the message.
    , _emReplyTo     :: !(Maybe Sender)
    -- ^ Optional sender to use for the Reply-To header.
    , _emSubject     :: !(Maybe Subject)
    -- ^ Optional subject of the message.
    , _emHeaders     :: ![Header]
    -- ^ List of additional headers.
    , _emHtmlBody    :: !HtmlBody
    -- ^ HTML body of the message.
    , _emTextBody    :: !(Maybe TextBody)
    -- ^ Optional alternate text body of the message.
    , _emAttachments :: !(Maybe Attachments)
    -- ^ Set of artefacts to attach to the message.
    , _emImages      :: ![Image]
    -- ^ List of images that can be referenced from the HTML body.
    }

instance ToJSON EmailMessage where
    toJSON em@EmailMessage{..} = object
        [ "to"           .= _emTo
        , "cc"           .= _emCc
        , "bcc"          .= _emBcc
        , "from"         .= _emFrom
        , "replyto"      .= _emReplyTo
        , "subject"      .= _emSubject
        , "headers"      .= _emHeaders
        , "html"         .= _emHtmlBody
        , "text"         .= _emTextBody
        , "attachment"   .= _emAttachments
        , "inline_image" .= _emImages
        ]

-- | Smart constructor for an 'EmailMessage'. This only sets the
-- mandatory field – additional ones can be set via lenses.
emailMessage :: NonEmpty Recipient -> Sender -> HtmlBody -> EmailMessage
emailMessage to from body = EmailMessage
    { _emTo          = to
    , _emCc          = []
    , _emBcc         = []
    , _emFrom        = from
    , _emReplyTo     = Nothing
    , _emSubject     = Nothing
    , _emHeaders     = []
    , _emHtmlBody    = body
    , _emTextBody    = Nothing
    , _emAttachments = Nothing
    , _emImages      = []
    }


--------------------------------------------------------------------------------
-- Sender and recipient types

-- | This is a 'Recipient' of an 'EmailMessage', be it via the @To@,
-- @Cc@ or @Bcc@ headers. It is typically constructed via the
-- 'recipient' smart constructor and modified via lenses.
data Recipient = Recipient
    { rAddress :: !EmailAddress
    , rName    :: !(Maybe FullName)
    }

instance ToJSON Recipient where
    toJSON Recipient{..} =
      object [emailToText rAddress .= maybe empty unFullName rName]

-- | Smart constructor for a 'Recipient' type.
recipient :: EmailAddress -> Recipient
recipient ea = Recipient ea Nothing

-- | This is a 'Sender' of an 'EmailMessage', be it via the @From@ or
-- @Reply-To@ headers. It is typically constructor via the 'sender'
-- smart constructor.
data Sender = Sender
    { sAddress :: !EmailAddress
    , sName    :: !(Maybe FullName)
    }

instance ToJSON Sender where
    toJSON Sender{..} = Array $ Vector.fromList
        [ String $ emailToText sAddress
        , String $ maybe empty unFullName sName
        ]

-- | Smart constructor for the 'Sender' type.
sender :: EmailAddress -> Sender
sender ea = Sender ea Nothing

-- | This is a 'FullName' to go into the 'Sender' or 'Recipient'
-- types. It is typically constructed via the 'fullName' constructor.
newtype FullName = FullName { unFullName :: Text }

-- | Constructor for 'FullName'.
fullName :: Text -> FullName
fullName = FullName

--------------------------------------------------------------------------------
-- Basic message specific sub-types

-- | This is the 'Subject' of an 'EmailMessage'
newtype Subject  = Subject Text deriving Generic
instance ToJSON Subject

-- | Constructor for the 'Subject' type.
subject :: Text -> Subject
subject = Subject

-- | This is a custom header for an 'EmailMessage'.
data Header = Header
    { hName  :: !Text
    , hValue :: !Text
    } deriving Generic

instance ToJSON Header

-- | Constructor for 'Header'. It will throw an exception if the
-- header name is one of the headers reserved by SendInBlue.
--
-- Future versions may to further validations, e.g. verifying that it
-- is valid according to RFC 5322.
header :: MonadThrow m => Text -> Text -> m Header
header name value =
  if mk name `elem` ["x-mailin-custom", "x-mailin-ip", "x-mailin-tag"]
      then throw . Error $ "Use of reserved header '" <> name <> "'"
      else return        $ Header name value

-- | This is the 'HtmlBody' of an 'EmailMessage'.
newtype HtmlBody = HtmlBody Text deriving Generic
instance ToJSON TextBody

-- | Constructor for the 'HtmlBody' type.
htmlBody :: Text -> HtmlBody
htmlBody = HtmlBody

-- | This is the 'TextBody' of an 'EmailMessage'.
newtype TextBody = TextBody Text deriving Generic
instance ToJSON HtmlBody

-- | Constructor for the 'TextBody' type.
textBody :: Text -> TextBody
textBody = TextBody


--------------------------------------------------------------------------------
-- Attachments
--
-- The complexity of the attachment types stem from the API which
-- allows attachments to be either a list of URIs or an associative
-- array of file names and base 64 encoded content. Since the
-- container type is different, these are mutually exclusive.

-- | This is a set of attachments for an 'EmailMessage'. A message can
-- contain several attachments, but they have to be of the same type.
data Attachments
    -- | Create attachments using URIs to reference their data.
    = UriAttachments !(NonEmpty UriAttachment)
    -- | Create attachments, providing theid data directly.
    | Base64Attachments !(NonEmpty Base64Attachment)

-- | This type specifies an attachment indirectly through a URI. The
-- service will fetch the attachment from it and encode it before
-- sending the email.
newtype UriAttachment = UriAttachment URI

-- | This type speficied an attachment directly as a 'ByteString'. The
-- data will be base64 encoded before sending it to the service.
data Base64Attachment = Base64Attachment
    { aFileName :: !Text
    , aPayload  :: !ByteString
    }

instance ToJSON Attachments where
    toJSON (UriAttachments as)
        = Array . Vector.fromList . NonEmpty.toList $ fmap toElement as
      where
        toElement (UriAttachment u)  =
          String . pack $ uriToString id u ""
    toJSON (Base64Attachments as)
        = object . NonEmpty.toList $ fmap toPair as
      where
        toPair Base64Attachment{..} =
          aFileName .= decodeUtf8 (Base64.encode aPayload)

-- | Construct a 'NonEmpty' list of 'UriAttachment'.
uriAttachments :: NonEmpty UriAttachment -> Attachments
uriAttachments = UriAttachments

-- | Construct a 'NonEmpty' list of 'Base64Attachment'.
base64Attachments :: NonEmpty Base64Attachment -> Attachments
base64Attachments = Base64Attachments

-- | Construct a single 'UriAttachment'.
uriAttachment :: URI -> UriAttachment
uriAttachment = UriAttachment

-- | Construct a single 'Base64Attachment'.
base64Attachment :: Text -> ByteString -> Base64Attachment
base64Attachment = Base64Attachment


--------------------------------------------------------------------------------
-- Images

-- | The 'Image' type represent an image that will be attached to the
-- message and can be referenced from an 'HtmlBody'.
data Image = Image
    { iFileName :: !Text
    , iPayload  :: !ByteString
    } deriving Generic

instance ToJSON Image where
    toJSON Image{..} = object
        [ iFileName .= decodeUtf8 (Base64.encode iPayload) ]

-- | Construch an 'Image'.
image :: Text -> ByteString -> Image
image = Image


--------------------------------------------------------------------------------
-- Response

-- | The 'Response' type concodes the outcome of a request. When
-- parsing the JSON response, the @code@ field is typically used to
-- determine the outcome, and will result in a 'Success' or 'Failure'
-- depending on its value.
data Response
    -- | The request succeeded.
    = Success
        { _rsMessage :: !Text
        -- ^ Custom success response message.
        , _rsData    :: !(Maybe Value)
        -- ^ Additional response data.
        }
    | Failure
        { _rfMessage :: !Text
        -- ^ Custom failure response message.
        }
    deriving Generic

instance FromJSON Response where
    parseJSON = withObject "Response" $ \r -> case Map.lookup "code" r of
        Just "success" -> Success <$> r .: "message"
                                  <*> r .: "data"
        Just "failure" -> Failure <$> r .: "message"
        Just x         -> fail "Invalid response code"
        Nothing        -> fail "Missing field 'code'"


--------------------------------------------------------------------------------
-- Utilities

emailToText :: EmailAddress -> Text
emailToText = decodeUtf8 . Email.toByteString


--------------------------------------------------------------------------------
-- Lenses

makeLenses ''EmailMessage
makeLenses ''Recipient
makeLenses ''Sender
makeLenses ''Response
