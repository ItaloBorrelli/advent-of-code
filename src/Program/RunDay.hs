module Program.RunDay (runDay, Day, Verbosity (Quiet, Timings, Verbose)) where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import Data.Text.IO (readFile)
import Data.Time (diffUTCTime, getCurrentTime)
import Program.Color (withColor)
import System.Console.ANSI (Color (Blue, Red))
import System.Directory (doesFileExist)
import Text.Parsec (parse)
import Text.Parsec.Text (Parser)
import Text.Printf (printf)

data Verbosity = Quiet | Timings | Verbose deriving (Eq, Show, Ord)

type Day = Verbosity -> String -> IO (Maybe Double, Maybe Double)

runDay :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> Program.RunDay.Day
runDay inputParser partA partB verbosity inputFile = do
  input <- runExceptT $ do
    inputFileExists <- liftIO $ doesFileExist inputFile
    fileContents <-
      if inputFileExists
        then liftIO $ Data.Text.IO.readFile inputFile
        else
          throwError $
            unwords
              [ "I couldn't read the input!",
                "I was expecting it to be at",
                inputFile
              ]
    case parse inputParser inputFile fileContents of
      Left e -> throwError $ "Parser failed to read input. Error:\n" ++ show e
      Right i -> do
        when (verbosity == Verbose) $ do
          liftIO $ putStrLn "Parser output:"
          liftIO $ print i
        return i

  case input of
    Left x -> withColor Red (putStrLn x) >> return (Nothing, Nothing)
    Right i -> do
      withColor Blue $ putStrLn "Part A:"
      time1 <- getCurrentTime
      successA <- catch (print (partA i) $> True) $
        \(m :: SomeException) -> withColor Red $ do
          putStrLn "Couldn't run Part A!"
          when (verbosity == Verbose) $ print m
          return False
      time2 <- getCurrentTime

      let timeA = realToFrac $ diffUTCTime time2 time1
      when (verbosity >= Timings && successA) $ putStrLn $ printf "(%.5fs)" timeA

      withColor Blue $ putStrLn "Part B:"
      successB <- catch (print (partB i) $> True) $
        \(m :: SomeException) -> withColor Red $ do
          putStrLn "Couldn't run Part B!"
          when (verbosity == Verbose) $ print m
          return False
      time3 <- getCurrentTime

      let timeB = realToFrac $ diffUTCTime time3 time2
      when (verbosity >= Timings && successB) $ putStrLn $ printf "(%.5f)" timeB

      return $
        (,)
          (if successA then Just timeA else Nothing)
          (if successB then Just timeB else Nothing)
