module Pandoc.ServiceSpec where

import           Control.Concurrent
import           Pandoc.Service
import           Test.Hspec

import           Network.HTTP
import           Network.Stream

spec :: Spec
spec = do
    around_ withService $
        describe "Integration tests" $ do
            it "returns 400 Bad Request upon an empty request" $ do
                irr <- simpleHTTP $ postRequestWithBody
                    "http://127.0.0.1:8081/convert"
                    "application/json"
                    ""
                resp <- getRes irr
                rspCode resp `shouldBe` (4, 0, 0)

            it "returns 400 Bad Request upon an unknown from format" $ do
                irr <- simpleHTTP $ postRequestWithBody
                    "http://127.0.0.1:8081/convert"
                    "application/json"
                    "{ \"from\": \"unknown format\", \"to\": \"pdf\", \"content\": \"A string\"}"
                resp <- getRes irr
                rspCode resp `shouldBe` (4, 0, 0)

            it "returns 200 upon a valid input markdown when converting to pdf" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $ "{ \"from\": \"markdown\", \"to\": \"pdf\", \"content\": " ++ content ++ "}"

            it "returns 200 upon a valid input json when converting to pdf" $ do
                let content = "[{\"unMeta\":{}},[{\"t\":\"Plain\",\"c\":[{\"t\":\"Str\",\"c\":\"Hello\"}]}]]"
                shouldJustWork $ "{ \"from\": \"json\", \"to\": \"pdf\", \"content\": " ++ content ++ "}"

            it "returns 200 upon a valid input markdown when converting to epub" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $ "{ \"from\": \"markdown\", \"to\": \"epub\", \"content\": " ++ content ++ "}"

            it "returns 200 upon a valid input json when converting to epub" $ do
                let content = "[{\"unMeta\":{}},[{\"t\":\"Plain\",\"c\":[{\"t\":\"Str\",\"c\":\"Hello\"}]}]]"
                shouldJustWork $ "{ \"from\": \"json\", \"to\": \"epub\", \"content\": " ++ content ++ "}"

            it "returns 200 upon a valid input markdown including an external image when converting to pdf" $ do
                let content = "\"# Title\n![Image text](http://placehold.it/10x10)\n\""
                shouldJustWork $ "{ \"from\": \"markdown\", \"to\": \"epub\", \"content\": " ++ content ++ "}"

            it "returns 200 upon a valid input markdown when converting to pdf with empty options" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $ "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": {}, \"content\": " ++ content ++ "}"

            it "returns 200 upon a valid input markdown when converting to pdf with empty reader options" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $ "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"reader\": {} }, \"content\": " ++ content ++ "}"

            it "returns 200 upon a valid input markdown when converting to pdf with empty writer options" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $ "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"writer\": {} }, \"content\": " ++ content ++ "}"

            it "returns 200 upon a valid input markdown when converting to pdf with this example writer option" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $ "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"writer\": { \"dpi\": 500 } }, \"content\": " ++ content ++ "}"

            it "returns 200 upon a valid input markdown when converting to pdf with writer variables" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $ "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"writer\": { \"variables\": [ [ \"toc\", \"true\" ] ] } }, \"content\": " ++ content ++ "}"

shouldJustWork :: String -> IO ()
shouldJustWork request = do
    irr <- simpleHTTP $ postRequestWithBody
        "http://127.0.0.1:8081/convert"
        "application/json"
        request
    resp <- getRes irr
    case rspCode resp of
        (2, 0, 0) -> pure ()
        _         -> expectationFailure $ show (resp, rspBody resp)

getRes :: Result a -> IO a
getRes (Left err) = fail $ show err
getRes (Right a)  = pure a

withService :: IO () -> IO ()
withService func = do
    tid <- forkIO runPandocService
    threadDelay $ 100 * 1000 -- Wait for it to start up
    func
    killThread tid
