{-# LANGUAGE OverloadedStrings #-}
module FilterUrl where

import Data.Monoid
import Control.Applicative
import System.Environment (getArgs)
import System.IO (stdout)
import qualified Data.Enumerator.Binary as E
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Data.Attoparsec.Char8
import Text.HTML.TagStream
import Blaze.ByteString.Builder (toByteString)
import Web.Cookie

type Protocol = ByteString
type Domain = ByteString
type Path = ByteString
url :: Parser (Protocol, Domain, Path)
url = (,,) <$> (string "http://" <|> string "https://")
           <*> takeTill (=='/')
           <*> takeByteString

mapUrlDomain :: (ByteString -> ByteString) -> ByteString -> ByteString
mapUrlDomain f s = either (const s) changeDomain $ parseOnly url s
  where
    changeDomain (prop, domain, path) =
      S.concat [ prop
               , f domain
               , path
               ]

mapCookieDomain :: (ByteString -> ByteString) -> ByteString -> ByteString
mapCookieDomain f s = toByteString . renderSetCookie $ cookie { setCookieDomain=fmap f (setCookieDomain cookie) }
  where cookie = parseSetCookie s

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

mapValue :: (ByteString -> ByteString) -> Token -> Token
mapValue f (TagOpen name as close)  = TagOpen name (map (mapSnd f) as) close 
mapValue _ t = t

main :: IO ()
main = do
    [filename] <- getArgs
    _ <- E.run_ $ E.enumFile filename
             E.$= tokenStream
             E.$= EL.map (mapValue (mapUrlDomain (`S.append` ".proxy")))
             E.$= EL.map (showToken id)
             E.$= EL.map toByteString
             E.$$ E.iterHandle stdout
    return ()
