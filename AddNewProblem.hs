#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

-- | Script to add boilerplate code to a new coding problem.
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Control.Foldl as F
import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Ord (comparing)
import System.Environment (getArgs)

solutionFileContents :: T.Text -> T.Text
solutionFileContents n =
    "module Day" <> n <> ".Day" <> n <> " where\n\n"

specFileContents :: T.Text -> T.Text
specFileContents n = T.unlines [
      "module Day" <> n <> "Spec ( spec ) where"
    , ""
    , "import Test.Hspec"
    , "import Test.Hspec.QuickCheck"
    , "import Day" <> n <> ".Day" <> n
    , ""
    , "spec :: Spec"
    , "spec = undefined"
    ]
problemFolder :: T.Text -> T.Text
problemFolder n = "src/Day" <> n

readmeContents :: T.Text -> T.Text
readmeContents n =
    "* [Day " <> n <> "](" <> problemFolder n <> ")"

getProblemNumber :: IO Int
getProblemNumber = do
    let namesShell = toText . filename <$> ls "src"
    names' <- fold namesShell F.list

    case sequence names' of
        Left _ -> return 1
        Right names -> do
            let numsStr = filter (not . T.null) (T.takeWhileEnd isDigit <$> names)
                solvedProblems = read . T.unpack <$> numsStr
                prevProblem = maximum solvedProblems
            return (prevProblem+1)

createSrcFiles :: T.Text -> IO ()
createSrcFiles n = do
    let folderPath = fromText (problemFolder n)
    let fileName = fromText ("Day" <> n <> ".hs")
    mkdir folderPath
    touch $ folderPath </> "README.md"
    touch $ folderPath </> fileName
    writeTextFile (folderPath </> fileName) (solutionFileContents n)

createTestFiles :: T.Text -> IO ()
createTestFiles n = do
    let testFileName = fromText ("test/Day" <> n <> "Spec.hs")
    touch testFileName
    writeTextFile testFileName (specFileContents n)

addLinkToReadme :: T.Text -> IO ()
addLinkToReadme n =
    append "README.md"
           (select $ textToLines $ readmeContents n)

buildProject :: IO ExitCode
buildProject = proc "stack" ["build", "--test", "--no-run-tests"] empty

main :: IO ExitCode
main = do
    n <- T.pack . show <$> getProblemNumber
    createSrcFiles n
    createTestFiles n
    addLinkToReadme n
    buildProject