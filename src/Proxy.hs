module Proxy
  ( ProxyFailures(..)
  , ProxyResponse
  , Accumulator
  , Checking(..)
  , IP(..)
  , ProxyState(..)
  , ProxyT
  , CheckResponse
  , statusColor
  , statusStr
  ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Network.HTTP.Client
import System.Console.ANSI

data ProxyFailures
  = RequestTimeout
  | InvalidResponse
  deriving (Show)

newtype IP =
  IP BL.ByteString
  deriving (Show)

type ProxyResponse = ExceptT HttpExceptionContent IO

data Checking =
  Checking
  deriving (Show)

statusColor :: Either Checking (Maybe HttpExceptionContent) -> Color
statusColor (Left Checking) = Yellow
statusColor (Right (Just _)) = Red
statusColor (Right Nothing) = Green

statusStr :: Either Checking (Maybe HttpExceptionContent) -> String
statusStr (Left Checking) = "⏳ Checking..."
statusStr (Right (Just e)) = "❌ Error: " <> show e
statusStr (Right Nothing) = "✅ Working!"

data ProxyState =
  ProxyState
    { proxyS :: Proxy
    , errorS :: Either Checking (Maybe HttpExceptionContent)
    } -- deriving (Show)

-- deriving instance Eq ProxyStatus
-- deriving instance Eq ProxyState
-- instance Ord ProxyState where
--   (ProxyState h1 _) `compare` (ProxyState h2 _) = h1 `compare` h2
type Accumulator = M.Map B.ByteString ProxyState

type ProxyT = ReaderT Proxy IO

type CheckResponse = (Proxy, Maybe HttpExceptionContent)
