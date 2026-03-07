module Main (main) where

import System.IO (hIsTerminalDevice)
import System.Posix.Signals (installHandler, sigHUP, sigQUIT, Handler(..))

import LedOptions (opts)
import LedRun (run)
import Options.Applicative (execParser)

main :: IO ()
main =
  hIsTerminalDevice stdin >>= \interactive ->
  execParser opts >>= \options ->
  installHandler sigQUIT Ignore Nothing >>
  newIORef (pure () :: IO ()) >>= \hupRef ->
  installHandler sigHUP
    (Catch (readIORef hupRef >>= id >> exitFailure))
    Nothing >>
  run interactive hupRef options >>= \hadError ->
  if hadError then exitFailure else exitSuccess
