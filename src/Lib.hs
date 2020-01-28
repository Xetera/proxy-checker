{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( main
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Either
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Proxy
import System.Console.ANSI
import System.Environment

endpoint :: Request
endpoint = "http://ifconfig.me"

fromFile :: FilePath -> IO [Proxy]
fromFile path = do
  proxyStrings <- lines <$> readFile path
  return $ map toProxy proxyStrings
  where
    toProxy str =
      let [ip, port] = splitOn ":" str
       in Proxy (C8.pack ip) (read port :: Int)

isSuccess :: Response a -> Bool
isSuccess = (== 200) . statusCode . responseStatus

addProxy :: Proxy -> ManagerSettings -> ManagerSettings
addProxy proxy = managerSetProxy (useProxy proxy)

-- I realize finding the IP of the proxy is pretty stupid
-- since you already need to know the ip to connect in
-- the first place but whatever
findProxyIP :: Proxy -> ProxyResponse IP
findProxyIP proxy = do
  man <- liftIO . newManager $ addProxy proxy defaultManagerSettings
  resE <- liftIO . tryAny $ httpLbs endpoint man
  case resE of
    Left _ -> throwE RequestTimeout
    -- TODO: how do we pattern match errors with SomeException here?
    Right res ->
      if isSuccess res
        then pure $ IP (responseBody res)
        else throwE InvalidResponse

isProxyWorking :: Proxy -> IO (Async Bool)
isProxyWorking prox = async (isRight <$> runExceptT (findProxyIP prox))

printProxies :: [ProxyState] -> IO ()
printProxies proxies =
  clear >> printHeader >> mapM_ printLine (sortOn proxyS proxies)
  where
    staticLength = 22
    remaining = length $ filter ((== Checking) . stateS) proxies
    printHeader = do
      putStr "Checking proxies: "
      setSGR [SetColor Foreground Vivid Blue]
      putStr $ show remaining <> "/" <> show (length proxies)
      setSGR [Reset]
      putStrLn "\n"
    printLine proxy = do
      let status = stateS proxy
      let proxyState = proxyS proxy
      let ip = proxyHost proxyState
      let target = intercalate ":" [C8.unpack ip, show (proxyPort proxyState)]
      setSGR [SetColor Foreground Vivid White]
      putStr target
      setSGR [SetColor Foreground Vivid (statusColor status)]
      let replicateLength = staticLength - length target
      putStrLn $ concat (replicate replicateLength " ") <> show status
      setSGR [Reset]

clear :: IO ()
clear = clearScreen >> setCursorPosition 0 0

defaultState :: Proxy -> ProxyState
defaultState p = ProxyState {proxyS = p, stateS = Checking}

updateDisplay :: MVar Accumulator -> ProxyState -> IO ()
updateDisplay acc newState = do
  modifyMVar_ acc $ \oldState -> do
    let host = proxyHost $ proxyS newState
    let updated = M.insert host newState oldState
    printProxies $ M.elems updated
    return updated
  return ()

resultsChannel :: [Async (Proxy, Bool)] -> IO (Chan (Maybe ProxyState))
resultsChannel tasks = do
  chan <- newChan
  forkIO (insertResults chan tasks)
  pure chan
  where
    insertResults :: Chan (Maybe ProxyState) -> [Async (Proxy, Bool)] -> IO ()
    insertResults chan [] = writeChan chan Nothing
    insertResults chan tasks = do
      (a, (proxy, working)) <- waitAny tasks
      let state =
            if working
              then Valid
              else Invalid
      writeChan chan $ Just $ ProxyState {proxyS = proxy, stateS = state}
      insertResults chan (a `delete` tasks)

-- we need to retain the context of the proxy that's
-- being checked so we return a (Proxy, Bool) tuple
checkProxy :: Proxy -> IO (Async (Proxy, Bool))
checkProxy proxy = do
  working <- isProxyWorking proxy
  return $ (proxy, ) <$> working

run :: FilePath -> IO ()
run path = do
  proxyList <- fromFile path
  let defaultProxies = map defaultState proxyList
  printProxies defaultProxies
  -- using the ip as the key for the map
  let proxyState =
        M.fromList
          (map (\prox -> (proxyHost $ proxyS prox, prox)) defaultProxies)
  proxyM <- newMVar proxyState
  tasks <- traverse checkProxy proxyList
  chan <- resultsChannel tasks
  -- returning Nothing to signal the last elem being read
  let loop =
        readChan chan >>= \case
          Nothing -> return ()
          Just a -> updateDisplay proxyM a >> loop
   in loop

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn
           "You must enter a proxy file path as the argument\nFormat: ip:port"
    else run (head args)
