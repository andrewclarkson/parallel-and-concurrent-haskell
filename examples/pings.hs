{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure

import DistribUtils

import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Message = Ping ProcessId
             | Pong ProcessId deriving (Typeable, Generic)

instance Binary Message

pingServer :: Process ()
pingServer = do
    Ping to <- expect
    say "ping."
    from <- getSelfPid
    send to (Pong from)

remotable ['pingServer]

master :: Process ()
master = do
    node <- getSelfNode
  
    to <- spawn node $(mkStaticClosure 'pingServer)
    from <- getSelfPid
    send to (Ping from)
    
    Pong _ <- expect 
    say "pong."

    terminate


main :: IO ()
main = distribMain (\_ -> master) Main.__remoteTable
