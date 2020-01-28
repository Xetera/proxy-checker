module Proxy
  ( Accumulator
  , Checking(..)
  , IP(..)
  , ProxyState(..)
  , CheckResponse
  , statusColor
  , statusStr
  ) where

import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Network.HTTP.Client
import System.Console.ANSI

newtype IP =
  IP BL.ByteString
  deriving (Show)

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
    }

type Accumulator = M.Map B.ByteString ProxyState

type CheckResponse = (Proxy, Maybe HttpExceptionContent)
