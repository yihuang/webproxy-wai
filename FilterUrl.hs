{-# LANGUAGE OverloadedStrings #-}
module FilterUrl where

import Data.Monoid
import Control.Applicative
import System.Environment (getArgs)
import System.IO (openBinaryFile, IOMode(..))
import qualified Data.Enumerator.Binary as E
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Data.Attoparsec.Char8
import Text.HTML.TagStream
import Blaze.ByteString.Builder (toByteString)

type Protocol = ByteString
type Domain = ByteString
type Path = ByteString
url :: Parser (Protocol, Domain, Path)
url = (,,) <$> (string "http://" <|> string "https://")
           <*> takeTill (=='/')
           <*> takeByteString

changeHost :: (ByteString -> ByteString) -> ByteString -> ByteString
changeHost f s = either (const s) changeDomain $ parseOnly url s
  where
    changeDomain (prop, domain, path) =
      S.concat [ prop
               , f domain
               , path
               ]

withHost :: Monad m => (ByteString -> ByteString) -> E.Enumeratee Token Token m b
withHost f = withUrl (changeHost f)

withUrl :: Monad m => (ByteString -> ByteString) -> E.Enumeratee Token Token m b
withUrl f = EL.map filter'
  where filter' :: Token -> Token
        filter' (TagOpen name as close)  = TagOpen name (map filter'' as) close
        filter' t = t
        filter'' :: Attr -> Attr
        filter'' (name, value)
            | name=="href" || name=="src" = (name, f value)
            | otherwise = (name, value)

main :: IO ()
main = do
    [filename] <- getArgs
    h <- openBinaryFile filename ReadMode
    let bufSize = 2
        enum = E.enumHandle bufSize h
    tokens <- E.run_ $ ((enum E.$= tokenStream) E.$= withHost (flip S.append ".proxy.com")) E.$$ EL.consume
    S.putStrLn $ toByteString . mconcat $ map (showToken id) tokens
