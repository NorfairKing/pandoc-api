module Pandoc.ServiceSpec where

import Control.Concurrent
import Control.Monad
import Pandoc.Service
import System.Directory
import System.FilePath
import Test.Hspec

import Network.HTTP
import Network.Stream

spec :: Spec
spec = do
    around_ withService $
        describe "Integration tests" $ do
            it "returns 400 Bad Request upon an empty request" $ do
                irr <-
                    simpleHTTP $
                    postRequestWithBody
                        "http://127.0.0.1:8081/convert"
                        "application/json"
                        ""
                resp <- getRes irr
                rspCode resp `shouldBe` (4, 0, 0)
            it "returns 400 Bad Request upon an unknown from format" $ do
                irr <-
                    simpleHTTP $
                    postRequestWithBody
                        "http://127.0.0.1:8081/convert"
                        "application/json"
                        "{ \"from\": \"unknown format\", \"to\": \"pdf\", \"content\": \"A string\"}"
                resp <- getRes irr
                rspCode resp `shouldBe` (4, 0, 0)
            it "returns 200 upon a valid input markdown when converting to pdf" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $
                    "{ \"from\": \"markdown\", \"to\": \"pdf\", \"content\": " ++
                    content ++ "}"
            it "returns 200 upon a valid input json when converting to pdf" $ do
                let content =
                        "{\"meta\":{}, \"blocks\": [{\"t\":\"Plain\",\"c\":[{\"t\":\"Str\",\"c\":\"Hello\"}]}], \"pandoc-api-version\":[1,17,0,4] }"
                shouldJustWork $
                    "{ \"from\": \"json\", \"to\": \"pdf\", \"content\": " ++
                    content ++ "}"
            it "returns 200 upon a valid input markdown when converting to epub" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $
                    "{ \"from\": \"markdown\", \"to\": \"epub\", \"content\": " ++
                    content ++ "}"
            it "returns 200 upon a valid input markdown when converting to json" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $
                    "{ \"from\": \"markdown\", \"to\": \"json\", \"content\": " ++
                    content ++ "}"
            it "returns 200 upon a valid input json when converting to epub" $ do
                let content =
                        "{\"meta\":{}, \"blocks\": [{\"t\":\"Plain\",\"c\":[{\"t\":\"Str\",\"c\":\"Hello\"}]}], \"pandoc-api-version\":[1,17,0,4] }"
                shouldJustWork $
                    "{ \"from\": \"json\", \"to\": \"epub\", \"content\": " ++
                    content ++ "}"
            it
                "returns 200 upon a valid input markdown including an external image when converting to pdf" $ do
                let content =
                        "\"# Title\n![Image text](http://placehold.it/10x10)\n\""
                shouldJustWork $
                    "{ \"from\": \"markdown\", \"to\": \"epub\", \"content\": " ++
                    content ++ "}"
            it
                "returns 200 upon a valid input markdown when converting to pdf with empty options" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $
                    "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": {}, \"content\": " ++
                    content ++ "}"
            it
                "returns 200 upon a valid input markdown when converting to pdf with empty reader options" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $
                    "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"reader\": {} }, \"content\": " ++
                    content ++ "}"
            it
                "returns 200 upon a valid input markdown when converting to pdf with empty writer options" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $
                    "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"writer\": {} }, \"content\": " ++
                    content ++ "}"
            it
                "returns 200 upon a valid input markdown when converting to pdf with this example writer option" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $
                    "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"writer\": { \"dpi\": 500 } }, \"content\": " ++
                    content ++ "}"
            it
                "returns 200 upon a valid input markdown when converting to pdf with writer variables" $ do
                let content = "\"# Title\nContent\n\""
                shouldJustWork $
                    "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"writer\": { \"variables\": [ [ \"toc\", \"true\" ] ] } }, \"content\": " ++
                    content ++ "}"
            it "uses a named template if we tell it to" $ do
                let content = "\"# Title\nContent\n\""
                let templates =
                        [ ( "hitemplate"
                          , "\\documentclass{article}\n\\begin{document}\nhi\n\\end{document}\n")
                        ]
                shouldWorkWithTemplates templates $
                    "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"writer\": { \"named-template\":\"hitemplate\" } }, \"content\": " ++
                    content ++ "}"
            it "returns a 404 on named template not found" $ do
                irr <-
                    simpleHTTP $
                    postRequestWithBody
                        "http://127.0.0.1:8081/convert"
                        "application/json"
                        "{ \"from\": \"markdown\", \"to\": \"pdf\", \"options\": { \"writer\": { \"named-template\":\"empty-template\" } }, \"content\": \"# Title\nContent\n\"}"
                resp <- getRes irr
                rspCode resp `shouldBe` (4, 0, 4)

shouldWorkWithTemplates :: [(String, String)] -> String -> IO ()
shouldWorkWithTemplates templates request = do
    createDirectoryIfMissing True templateDir
    forM_ templates $ \(name, content) -> do
        let templateFile = templateDir </> name
        writeFile templateFile content
    shouldJustWork request

shouldJustWork :: String -> IO ()
shouldJustWork request = do
    irr <-
        simpleHTTP $
        postRequestWithBody
            "http://127.0.0.1:8081/convert"
            "application/json"
            request
    resp <- getRes irr
    case rspCode resp of
        (2, 0, 0) -> pure ()
        _ -> expectationFailure $ show (resp, rspBody resp)

getRes :: Result a -> IO a
getRes (Left err) = fail $ show err
getRes (Right a) = pure a

withService :: IO () -> IO ()
withService func = do
    (tid, var) <- forkThing runPandocService
    threadDelay $ 100 * 1000 -- Wait for it to start up
    func
    killThread tid
    void $ takeMVar var
    removePathForcibly templateDir

forkThing :: IO () -> IO (ThreadId, MVar ())
forkThing proc = do
    handle <- newEmptyMVar
    tid <- forkFinally proc (\_ -> putMVar handle ())
    return (tid, handle)
