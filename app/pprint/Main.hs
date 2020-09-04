import           AbMarkdown.Parser
import           AbMarkdown.Render
import           Prelude
import           Data.Text                                    as T

main :: IO ()
main = do
    md <- getContents
    putStrLn . T.unpack . render . parse [Normalize] $ T.pack md
