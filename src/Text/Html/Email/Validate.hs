module Text.Html.Email.Validate
    ( isValidEmail
    )
where

import           AbMarkdown.ParserCombinators
import           Control.Monad
import           Data.Either                    ( isRight )
import           Data.Text                      ( Text )
import qualified Data.Text                                    as Text

isValidEmail :: Text -> Bool
isValidEmail = isRight . runParser (scanEmail *> endOfInput)

scanEmail :: Parser ()
scanEmail = local *> char '@' *> domain

local :: Parser ()
local = skipWhile1 (inClass "A-Za-z0-9!#$%&'*+/=?^_`{|}~.-")

domain :: Parser ()
domain = () <$ label `sepBy1` char '.'

label :: Parser ()
label = do
    lbl <- Text.intercalate "-" <$> labelChars `sepBy1` char '-'
    guard (Text.length lbl <= 63) <?> "Label is too long"
    where labelChars = takeWhile1 (inClass "A-Za-z0-9")
