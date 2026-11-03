{-# LANGUAGE OverloadedStrings #-}

-- | Main module for launching the web server that handles parsing and
-- solving MiniZinc-lite models. This server exposes an API that accepts a
-- model, parses and solves it, and returns the resulting assignment.
module Main (main) where

import Assignment (formatSolution, getTime, miniZincParser, solveProblem)
import Control.Arrow (second)
import Control.Concurrent.QSemN (newQSemN)
import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Key (fromString)
import Data.Text.Lazy (Text, pack, unpack)
import Instances (ParseResult (..), parse)
import Web.Scotty (ActionM, formParam, json, post, scotty)

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
    json $
        object
            [ fromString key .= (pack value :: Text) | (key, value) <- pairs
            ]

-- The three possible results of running a model. Both endpoints share
-- this, so a model is parsed and solved in exactly one place.
data Outcome
    = ParseFailure String
    | NoSolution
    | Solved [(String, Int)]

-- Parses and solves a model (pure: no IO happens here).
runModel :: String -> Outcome
runModel source = case parse miniZincParser source of
    Error err -> ParseFailure (show err)
    Result _ model -> maybe NoSolution Solved (solveProblem model)

-- Reads the "model" form field sent by the frontend.
modelParam :: ActionM String
modelParam = unpack <$> (formParam "model" :: ActionM Text)

-- Writes a solution to solution_<time>.out using the provided getTime,
-- returning the file name.
saveSolution :: [(String, Int)] -> IO FilePath
saveSolution bindings = do
    time <- getTime
    let file = "solution_" ++ time ++ ".out"
    writeFile file (formatSolution time bindings)
    pure file

-- --------------------------------------------------------------------------------
-- | Entry point – launches Scotty web server and defines API routes
main :: IO ()
main =
    newQSemN 1 >>= \_ -> scotty 3000 $ do
        post "/api/minizinc" $ do
            model <- modelParam
            liftIO $ putStrLn model
            case runModel model of
                ParseFailure err -> jsonResponse [("error", err)]
                NoSolution -> jsonResponse [("result", "No solution")]
                Solved env -> jsonResponse (map (second show) env)

        -- Saves a solution of the current model. The model is solved
        -- again here, so only a real solution can ever be written.
        post "/api/save" $ do
            model <- modelParam
            case runModel model of
                ParseFailure err -> jsonResponse [("error", err)]
                NoSolution ->
                    jsonResponse [("error", "No solution to save")]
                Solved env -> do
                    saved <- liftIO (try (saveSolution env))
                    case saved :: Either IOException FilePath of
                        Left err -> jsonResponse
                            [("error", "Save failed: " ++ show err)]
                        Right file -> jsonResponse [("saved", file)]
