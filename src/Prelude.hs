module Prelude
    ( module Exports

    -- | Custom prelude functions
    , parseInt
    , headMay
    ) where

import           Control.Monad.Trans.Resource as Exports (ResourceT,
                                                          liftResourceT,
                                                          runResourceT)
import           Network.HTTP.Types           as Exports
import           Network.Wai                  as Exports
import           Relude                       as Exports hiding (get, put)

-- import           Data.Text.Encoding           (decodeUtf8With)
-- import           Data.Text.Encoding.Error     (lenientDecode)
import qualified Data.Text.Read               as TR

import           UnliftIO.Exception           as Exports (throwIO)

-- Text formatting
import           Formatting                   as Exports (format, sformat, (%))
import           Formatting.ShortFormatters   as Exports (d, sh, st, t)

parseInt :: Integral a => Text -> Maybe a
parseInt t' = either (const Nothing) Just $ fmap fst $ TR.decimal t'

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a:_) = Just a
