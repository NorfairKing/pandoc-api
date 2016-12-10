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
                irr <- simpleHTTP $ postRequestWithBody
                    "http://127.0.0.1:8081/convert"
                    "application/json" $
                    "{ \"from\": \"markdown\", \"to\": \"pdf\", \"content\": " ++ content ++ "}"
                resp <- getRes irr
                rspCode resp `shouldBe` (2, 0, 0)

            it "returns 200 upon a valid input json when converting to pdf" $ do
                let content = "[{\"unMeta\":{}},[{\"t\":\"Plain\",\"c\":[{\"t\":\"Str\",\"c\":\"Hello\"}]}]]"
                irr <- simpleHTTP $ postRequestWithBody
                    "http://127.0.0.1:8081/convert"
                    "application/json" $
                    "{ \"from\": \"json\", \"to\": \"pdf\", \"content\": " ++ content ++ "}"
                resp <- getRes irr
                rspCode resp `shouldBe` (2, 0, 0)

            it "returns 200 upon a valid input markdown when converting to epub" $ do
                let content = "\"# Title\nContent\n\""
                irr <- simpleHTTP $ postRequestWithBody
                    "http://127.0.0.1:8081/convert"
                    "application/json" $
                    "{ \"from\": \"markdown\", \"to\": \"epub\", \"content\": " ++ content ++ "}"
                resp <- getRes irr
                rspCode resp `shouldBe` (2, 0, 0)

            it "returns 200 upon a valid input json when converting to epub" $ do
                let content = "[{\"unMeta\":{}},[{\"t\":\"Plain\",\"c\":[{\"t\":\"Str\",\"c\":\"Hello\"}]}]]"
                irr <- simpleHTTP $ postRequestWithBody
                    "http://127.0.0.1:8081/convert"
                    "application/json" $
                    "{ \"from\": \"json\", \"to\": \"epub\", \"content\": " ++ content ++ "}"
                resp <- getRes irr
                rspCode resp `shouldBe` (2, 0, 0)

            it "returns 200 upon a valid input markdown including an external image when converting to pdf" $ do
                let content = "\"# Title\n![Image text](http://placehold.it/10x10)\n\""
                irr <- simpleHTTP $ postRequestWithBody
                    "http://127.0.0.1:8081/convert"
                    "application/json" $
                    "{ \"from\": \"markdown\", \"to\": \"epub\", \"content\": " ++ content ++ "}"
                resp <- getRes irr
                rspCode resp `shouldBe` (2, 0, 0)

getRes :: Result a -> IO a
getRes (Left err) = fail $ show err
getRes (Right a) = pure a

withService :: IO () -> IO ()
withService func = do
    tid <- forkIO runPandocService
    threadDelay $ 100 * 1000 -- Wait for it to start up
    func
    killThread tid
