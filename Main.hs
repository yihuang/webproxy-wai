{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
import Data.Monoid (mempty, mappend, mconcat)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types
import Data.Enumerator (Iteratee)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Blaze.ByteString.Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
-- import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.HTTP.Enumerator as C -- client
import Text.StringLike
import Text.HTML.TagSoup (Tag (..), renderTags, parseTags)

portNumber = 3000 :: Int

hostSuffix :: ByteString
hostSuffix = ".localhost"

blaze :: (Status -> ResponseHeaders -> Iteratee Builder IO a)
      -> Status -> ResponseHeaders -> Iteratee ByteString IO a
blaze f status headers =
  let notEncoding ("Content-Encoding", _) = False
      notEncoding _ = True
      headers' = filter notEncoding headers
      iter = f status headers'
  in  E.joinI $ EL.map fromByteString E.$$ iter

fetch :: C.Request IO
       -> (Status -> ResponseHeaders -> Iteratee Builder IO a)
       -> IO a
fetch req f = C.withManager $ \m ->
                E.run_ $ C.http req (blaze f) m

logReq :: C.Request IO -> IO ()
logReq C.Request{..} = S.putStrLn log
  where log = mconcat
                [ method
                , if secure then "https://" else "http://"
                , host
                , if port==80 then mempty else ":" `mappend` (S.pack $ show port)
                , path
                ]

withUrls :: (ByteString -> ByteString) -> ByteString -> ByteString
withUrls f = renderTags . map tag . parseTags
  where
    tag (TagOpen s a) = TagOpen s $ map attr a
    tag x = x
    refs = ["src", "href"]
    attr (k, v) = (k, if k `elem` refs then f v else v)

appendHost :: ByteString -> ByteString
appendHost url =
    if "http://" `S.isPrefixOf` url
      then handle (S.take 7 url)
      else
        if "https://" `S.isPrefixOf` url
          then handle (S.take 8 url)
          else url
  where
    handle url =
       let (dom, path) = S.break (=='/') url
       in  mconcat [dom, hostSuffix, ":", S.pack (show 3000), path]

app :: Application
app Request{..} = do
    body <- L.fromChunks <$> EL.consume
    let req = mkReq body
    liftIO $ logReq req
    return $ ResponseEnumerator $ fetch req
  where
    fullhost = (S.take
                 (S.length serverName - S.length hostSuffix)
                 serverName)
    (host, port) = S.break (==':') fullhost
    mkReq :: L.ByteString -> C.Request IO
    mkReq body = C.def
      { C.method = requestMethod
      , C.secure = isSecure
      , C.host = host
      , C.port = if S.null port then 80 else read (S.unpack $ S.drop 1 port)
      , C.path = mconcat [rawPathInfo, rawQueryString]
      , C.queryString = []
      , C.requestHeaders = requestHeaders
      , C.requestBody = C.RequestBodyLBS body
      }

main = run portNumber app
