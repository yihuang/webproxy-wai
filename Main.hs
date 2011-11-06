{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
import Debug.Trace
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
import qualified Data.CaseInsensitive     as CI
import Text.HTML.TagStream (tokenStream, Token, showToken)
import FilterUrl hiding (main)

portNumber = 80 :: Int
domainSuffix = ".proxy" :: ByteString
hostSuffix = if portNumber==80 then domainSuffix else S.append domainSuffix (S.pack (show portNumber))

changeDomain :: ByteString -> ByteString
changeDomain = (`S.append` hostSuffix)

recoverDomain :: ByteString -> ByteString
recoverDomain host = S.append (S.take (S.length dom - S.length domainSuffix) dom) rest
  where (dom, rest) = S.span (/=':') host

logReq :: C.Request IO -> IO ()
logReq rq = S.putStrLn $ mconcat
              [ C.method rq
              , " "
              , if C.secure rq then "https://" else "http://"
              , C.host rq
              , if C.port rq==80 then mempty else ":" `mappend` (S.pack . show $ C.port rq)
              , C.path rq
              ]

logHeader :: ResponseHeaders -> IO ()
logHeader headers = S.putStrLn $ S.concat (map pp headers)
  where pp (name, value) = mconcat [CI.original name, ": ", value, "\n"]

notEncoding :: Header -> Bool
notEncoding ("Content-Encoding", _) = False
notEncoding ("Content-Length", _) = False
notEncoding _ = True

blaze :: (Status -> ResponseHeaders -> Iteratee Builder IO a)
      -> Status -> ResponseHeaders -> Iteratee ByteString IO a
blaze f status headers =
  let changeHeader (name, value)
        | name=="Location" = (name, mapUrlDomain changeDomain value)
        | name=="Set-Cookie" = (name, mapCookieDomain changeDomain value)
      changeHeader x = x
      ct = fromMaybe "text/html" $ lookup "content-type" headers
      headers' = map changeHeader . filter notEncoding $ headers
      iter = liftIO (logHeader headers) >> f status headers'
  in  if "text/html" `S.isPrefixOf` ct
        then   tokenStream
          E.=$ EL.map (mapValue (mapUrlDomain (`S.append` hostSuffix)))
          E.=$ EL.map (showToken id)
          E.=$ iter
        else   EL.map fromByteString
          E.=$ iter

fetch :: C.Request IO
       -> (Status -> ResponseHeaders -> Iteratee Builder IO a)
       -> IO a
fetch req f = C.withManager $ \m ->
                E.run_ $ C.http req (blaze f) m

app :: Application
app Request{..} = do
    -- TODO streamline request body
    body <- S.concat <$> EL.consume
    let req = mkReq body
    liftIO $ logReq req
    liftIO $ logHeader requestHeaders'
    liftIO $ S.putStrLn body
    return $ ResponseEnumerator $ fetch req
  where
    fullhost = recoverDomain serverName
    (host, port) = S.break (==':') fullhost
    requestHeaders' = map alterDomain . filter notEncoding $ requestHeaders
    alterDomain (name, value)
      | name `elem` map CI.mk ["Origin", "Referer"]
        = (name, mapUrlDomain recoverDomain value)
      | name == "Host"
        = (name, recoverDomain value)
    alterDomain x = x
    mkReq :: S.ByteString -> C.Request IO
    mkReq body = C.def
      { C.method = requestMethod
      , C.secure = isSecure
      , C.host = host
      , C.port = if S.null port then 80 else read (S.unpack $ S.drop 1 port)
      , C.path = mconcat [rawPathInfo, rawQueryString]
      , C.queryString = []
      , C.requestHeaders = requestHeaders'
      , C.requestBody = C.RequestBodyBS body
      }

main = run portNumber app
