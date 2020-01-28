{-# LANGUAGE StandaloneDeriving #-}

module Proxy
  ( ProxyFailures(..)
  , ProxyResponse
  , ProxyStatus(..)
  , Accumulator
  , IP(..)
  , ProxyState(..)
  , ProxyT
  , statusColor
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

type ProxyResponse = ExceptT ProxyFailures IO

data ProxyStatus
  = Checking
  | Invalid
  | Valid
  deriving (Show)

statusColor :: ProxyStatus -> Color
statusColor Checking = Yellow
statusColor Invalid = Red
statusColor Valid = Green

data ProxyState =
  ProxyState
    { proxyS :: Proxy
    , stateS :: ProxyStatus
    }
  deriving (Show)

deriving instance Eq ProxyStatus

deriving instance Eq ProxyState

instance Ord ProxyState where
  (ProxyState h1 _) `compare` (ProxyState h2 _) = h1 `compare` h2

type Accumulator = M.Map B.ByteString ProxyState

type ProxyT = ReaderT Proxy IO
