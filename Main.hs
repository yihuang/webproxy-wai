{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
import Data.Maybe
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
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.HTTP.Enumerator as C -- client
import Data.Attoparsec (parseOnly)
import Text.HTML.TagStream (tokenStream, Token, showToken)
import FilterUrl (withHost)

portNumber = 3000 :: Int
domainSuffix = ".proxy"
hostSuffix = S.pack (".proxy:"++show portNumber)

blaze :: (Status -> ResponseHeaders -> Iteratee Builder IO a)
      -> Status -> ResponseHeaders -> Iteratee ByteString IO a
blaze f status headers =
  let notEncoding ("Content-Encoding", _) = False
      notEncoding _ = True
      ct = fromMaybe "text/plain" $ lookup "content-type" headers
      headers' = filter notEncoding headers
      iter' = f status headers'
      iter = E.joinI $ EL.map fromByteString E.$$ iter'
      filterHost = E.joinI . (tokenStream E.$$) . E.joinI . (withHost (flip S.append hostSuffix) E.$$) . E.joinI . (EL.map (showToken id) E.$$)
  in  if ct=="text/html" then filterHost iter' else iter

fetch :: C.Request IO
       -> (Status -> ResponseHeaders -> Iteratee Builder IO a)
       -> IO a
fetch req f = C.withManager $ \m ->
                E.run_ $ C.http req (blaze f) m

logReq :: C.Request IO -> IO ()
logReq rq = S.putStrLn log
  where log = mconcat
                [ C.method rq
                , if C.secure rq then "https://" else "http://"
                , C.host rq
                , if C.port rq==80 then mempty else ":" `mappend` (S.pack . show $ C.port rq)
                , C.path rq
                ]

app :: Application
app Request{..} = do
    -- TODO streamline request body
    body <- L.fromChunks <$> EL.consume
    let req = mkReq body
    liftIO $ logReq req
    return $ ResponseEnumerator $ fetch req
  where
    fullhost = (S.take
                 (S.length serverName - S.length domainSuffix)
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
