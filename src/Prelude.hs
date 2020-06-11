module Prelude
  ( module Exports,
    -- | Custom prelude functions
    parseInt,
    headMay,
  )
where

import Control.Monad.Trans.Resource as Exports
  ( ResourceT,
    liftResourceT,
    runResourceT,
  )
import qualified Data.Text.Read as TR
-- Text formatting
import Formatting as Exports ((%), format, sformat)
import Formatting.ShortFormatters as Exports (d, sh, st, t)
import Network.HTTP.Types as Exports
import Network.Wai as Exports
import Relude as Exports hiding (get, put)
import UnliftIO.Exception as Exports (throwIO)

parseInt :: Integral a => Text -> Maybe a
parseInt t' = either (const Nothing) Just $ fmap fst $ TR.decimal t'

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a : _) = Just a
