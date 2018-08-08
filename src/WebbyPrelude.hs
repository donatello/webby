module WebbyPrelude
    ( module Exports

    -- | Custom prelude functions
    , decodeUtf8Lenient
    , parseInt
    ) where

import           Control.Monad.Trans.Resource as Exports (ResourceT,
                                                          liftResourceT,
                                                          runResourceT)
import           Network.HTTP.Types           as Exports
import           Network.Wai                  as Exports
import           Protolude                    as Exports hiding (get, put, (%))

import           Data.Text.Encoding           as Exports (encodeUtf8)

-- Imports for custom functionality coded in this module.
import           Data.Text.Encoding           (decodeUtf8With)
import           Data.Text.Encoding.Error     (lenientDecode)
import qualified Data.Text.Read               as TR

-- Text formatting
import           Formatting                   as Exports (format, sformat, (%))
import           Formatting.ShortFormatters   as Exports (d, sh, st, t)

-- Decode UTF8 leniently (by replacing an invalid input byte with the
-- Unicode replacement character U+FFFD).
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

parseInt :: Integral a => Text -> Maybe a
parseInt t' = hush $ fmap fst $ TR.decimal t'
