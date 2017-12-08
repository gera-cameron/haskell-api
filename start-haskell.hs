module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (void)
import Numeric (showInt)
import System.Environment (getArgs)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR1)
import System.Process (ProcessHandle, callProcess, spawnProcess, terminateProcess, waitForProcess)

main :: IO ()
main = do
  currentPid <- getProcessID
  args <- getArgs
  restartChan <- newChan

  forkIO $ serverLoop Nothing args restartChan
  _ <- installHandler sigUSR1 (Catch $ triggerRestart restartChan) Nothing

  callProcess "stack" [ "build", "--fast", "--file-watch", "--exec", "kill -USR1 " ++ (showInt currentPid "")]

triggerRestart :: Chan () -> IO ()
triggerRestart restartChan = do
  putStrLn "Rebuilt. (Re)starting Server"
  writeChan restartChan ()

serverLoop :: Maybe ProcessHandle -> [String] -> Chan () -> IO ()
serverLoop oldServerHandle serverArgs restartChan = do
  readChan restartChan

  case oldServerHandle of
    Nothing -> pure ()
    Just handle -> do
      terminateProcess handle
      void $ waitForProcess handle

  newServerHandle <- spawnProcess "stack" ("exec":"api":"--":serverArgs)
  serverLoop (Just newServerHandle) serverArgs restartChan
