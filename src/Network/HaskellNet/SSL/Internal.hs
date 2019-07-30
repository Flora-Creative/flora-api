{-

Copyright (c) 2013, Daniel P. Wright

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Daniel P. Wright nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module Network.HaskellNet.SSL.Internal
  ( connectSSL
  , connectPlain
  )
where

import           Network.Connection
import           Network.HaskellNet.SSL
import           Network.HaskellNet.BSStream

import qualified Data.ByteString.Char8         as B
import           Data.Default

import           Control.Monad                            ( (>=>) )

type STARTTLS = IO ()

connectionGetBytes :: Connection -> Int -> IO B.ByteString
connectionGetBytes = loop B.empty where
  loop buf _ 0 = return buf
  loop buf c l = connectionGet c l >>= nextIteration
    where nextIteration b = loop (buf `B.append` b) c $ l - B.length b

connectionToStream :: Connection -> Settings -> BSStream
connectionToStream c cfg = BSStream
  { bsGet          = connectionGetBytes c >=> withLog "RECV"
  , bsPut          = withLog "SEND" >=> connectionPut c
  , bsFlush        = return ()
  , bsClose        = connectionClose c
  , bsIsOpen       = return True
  , bsGetLine      = connectionGetLine maxl c >>= withLog "RECV"
  , bsWaitForInput = connectionWaitForInput c
  } where
  maxl    = sslMaxLineLength cfg
  withLog = if sslLogToConsole cfg then logToConsole else flip (const . return)

logToConsole :: String -> B.ByteString -> IO B.ByteString
logToConsole dir s = do
  putStrLn $ "HaskellNet-SSL " ++ dir ++ ": " ++ show s
  return s

connectSSL :: String -> Settings -> IO BSStream
connectSSL hostname cfg = do
  c <- initConnectionContext >>= flip connectTo params
  return $ connectionToStream c cfg
 where
  params = ConnectionParams hostname port (Just tlsCfg) Nothing
  port   = sslPort cfg
  tlsCfg = def
    { settingDisableCertificateValidation = sslDisableCertificateValidation cfg
    }

connectPlain :: String -> Settings -> IO (BSStream, STARTTLS)
connectPlain hostname cfg = do
  ctx <- initConnectionContext
  c   <- connectTo ctx params
  return (connectionToStream c cfg, connectionSetSecure ctx c tlsCfg)
 where
  params = ConnectionParams hostname port Nothing Nothing
  port   = sslPort cfg
  tlsCfg = def
    { settingDisableCertificateValidation = sslDisableCertificateValidation cfg
    }
