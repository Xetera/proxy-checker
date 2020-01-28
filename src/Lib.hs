{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( main
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
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
findProxyIP :: Proxy -> IO (Either HttpExceptionContent IP)
findProxyIP proxy = do
  man <- liftIO . newManager $ addProxy proxy defaultManagerSettings
  res <-
    tryAsync $ httpLbs endpoint man :: IO (Either HttpException (Response BL.ByteString))
  return $
    case res of
      Right a -> return $ IP $ responseBody a
      Left (HttpExceptionRequest _ b) -> Left b

extractError :: Either a b -> Maybe a
extractError e =
  case e of
    Left a -> Just a
    Right _ -> Nothing

isProxyWorking :: Proxy -> IO (Async (Maybe HttpExceptionContent))
isProxyWorking prox = async (extractError <$> findProxyIP prox)

printProxies :: [ProxyState] -> IO ()
printProxies proxies =
  clear >> printHeader >>
  mapM_ (uncurry printLine) (zip (sortOn proxyS proxies) [0 ..])
  where
    staticLength = 22
    remaining = length $ filter (isLeft . errorS) proxies
    printHeader = do
      putStr "Checking proxies: "
      setSGR [SetColor Foreground Vivid Blue]
      putStr $ show remaining <> "/" <> show (length proxies)
      setSGR [Reset]
      putStrLn "\n"
    printLine proxy num = do
      let status = errorS proxy
      let proxyState = proxyS proxy
      let ip = proxyHost proxyState
      let target = intercalate ":" [C8.unpack ip, show (proxyPort proxyState)]
      setSGR [SetColor Foreground Vivid Yellow]
      putStr $ show (num + 1) <> " "
      setSGR [SetColor Foreground Vivid White]
      putStr target
      setSGR [SetColor Foreground Vivid (statusColor status)]
      let replicateLength = staticLength - length target
      putStrLn $ concat (replicate replicateLength " ") <> statusStr status
      setSGR [Reset]

clear :: IO ()
clear = clearScreen >> setCursorPosition 0 0

defaultState :: Proxy -> ProxyState
defaultState p = ProxyState {proxyS = p, errorS = Left Checking}

updateDisplay :: MVar Accumulator -> ProxyState -> IO ()
updateDisplay acc newState = do
  modifyMVar_ acc $ \oldState -> do
    let host = proxyHost $ proxyS newState
    let updated = M.insert host newState oldState
    printProxies $ M.elems updated
    return updated
  return ()

resultsChannel :: [Async CheckResponse] -> IO (Chan (Maybe ProxyState))
resultsChannel tasks = do
  chan <- newChan
  forkIO (insertResults chan tasks)
  pure chan
  where
    insertResults :: Chan (Maybe ProxyState) -> [Async CheckResponse] -> IO ()
    insertResults chan [] = writeChan chan Nothing
    insertResults chan tasks = do
      (a, (proxyS, errorS)) <- waitAny tasks
      writeChan chan $ Just $ ProxyState {proxyS, errorS = Right errorS}
      insertResults chan (a `delete` tasks)

-- we need to retain the context of the proxy that's
-- being checked so we return a (Proxy, Bool) tuple
checkProxy :: Proxy -> IO (Async CheckResponse)
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
