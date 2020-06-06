{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}


module Main (main) where

import Prelude ()
import Prelude.Compat

-- GetOpt:
import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Read (readMaybe)
import qualified Data.ConfigFile as CF
import Data.Either.Utils

import StStephens (runApi, ApiParameters (..))

defaultPort = 7001
defaultConfFile = "/Users/lhugo/.sisApi.conf"



data CmdLineArg = Port Int               -- -p
    | ConfigFile String           -- -c
    | Help                        -- --help
    deriving (Eq,Ord,Show)


cmdLineOptions :: [OptDescr CmdLineArg]
cmdLineOptions =
   [ Option ['p'] ["port"] (ReqArg parsePort "PORT")  $ "Sets the listening port (default: " ++ (show defaultPort) ++ ")."
     , Option ['c'] ["conf"] (ReqArg parseConfig "FILE") "Configuration file to use."
     , Option ['h'] ["help"] (NoArg Help) "Print this help message."
  ]

parsePort :: String -> CmdLineArg
parsePort mbString =
  case readMaybe mbString :: Maybe Int of
    Just anInt -> Port anInt
    Nothing -> Port defaultPort

parseConfig :: String -> CmdLineArg
parseConfig mbString =
   ConfigFile mbString


parseArgs :: [String] -> IO ([CmdLineArg], [String])
parseArgs argv= case getOpt Permute cmdLineOptions argv of

    (args,fs,[]) -> do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header cmdLineOptions)
                    exitWith ExitSuccess
            else return (nub (concatMap (\x -> [x]) args), files)

    (_,_,errs)      -> do
        hPutStrLn stderr (concat errs ++ usageInfo header cmdLineOptions)
        exitWith (ExitFailure 1)

    where header = "Usage: sisApi [-p/--port <port>] [-c/--conf <config file>]"


showDbgArguments :: [ CmdLineArg ] -> IO ()
showDbgArguments args = do
  putStrLn "-- dbg:start --"
  showDbgArguments' args
  putStrLn "-- dbg:end --"

showDbgArguments' [] = return ()
showDbgArguments' (car:cons) = showDbgArg car >> showDbgArguments' cons


showDbgArg :: CmdLineArg -> IO ()
showDbgArg anArg = do
  case anArg of
    Port aNumber -> putStrLn $ "Port: " ++ (show aNumber)
    ConfigFile aFile -> putStrLn $ "Conf: " ++ aFile
    Help -> putStrLn "help!"


loadConfig :: [ CmdLineArg ] -> IO ApiParameters
loadConfig cmdArgs = do
  let configs = ApiParams { port= 7001, db = "aDB", host= "aHost", user= "aUser", pswd="aPasswd" }
  let targetFile= findTargetFile cmdArgs
  val <- CF.readfile CF.emptyCP targetFile
  case val of
      Right configValues -> do
        o1 <- return $ setIntFromConfFile "port" (\v c -> c { port = v}) configValues configs
        o1 <- return $ setStringFromConfFile "db" (\v c -> c { db = v}) configValues o1
        o1 <- return $ setStringFromConfFile "host" (\v c -> c { host = v}) configValues o1
        o1 <- return $ setStringFromConfFile "user" (\v c -> c { user = v}) configValues o1
        o1 <- return $ setStringFromConfFile "pswd" (\v c -> c { pswd = v}) configValues o1
        return o1
      Left (err, msg) -> do
        putStrLn $ "Error on config file: " ++ msg
        return configs


setIntFromConfFile :: String -> (Int -> ApiParameters -> ApiParameters) -> CF.ConfigParser -> ApiParameters -> ApiParameters
setIntFromConfFile confName accessor fileValues configs =
  case CF.get fileValues "default" confName of
     Right value -> case readMaybe value of
          Just number ->  accessor number configs
          Nothing ->  configs
     Left e ->  configs

setStringFromConfFile :: String -> (String -> ApiParameters -> ApiParameters) -> CF.ConfigParser -> ApiParameters -> ApiParameters
setStringFromConfFile confName accessor fileValues configs =
  case CF.get fileValues "default" confName of
     Right value -> accessor value configs
     Left e ->  configs


findTargetFile :: [ CmdLineArg ] -> String
findTargetFile cmdArgs =
  defaultConfFile
--  foldl (\flag acc -> if flag == (ConfigFile x) then x else acc) defaultConfFile cmdArgs

-- DEBUG:
debugConfFile configValues = do
  putStrLn "Configs are:"
  putStrLn $ "Port: " ++ (forceEither $ CF.get configValues "default" "port")
  putStrLn $ "db: " ++ (forceEither $ CF.get configValues "default" "db")
  putStrLn $ "host: " ++ (forceEither $ CF.get configValues "default" "host")
  putStrLn $ "user: " ++ (forceEither $ CF.get configValues "default" "user")
  putStrLn $ "password: " ++ (forceEither $ CF.get configValues "default" "pswd")


main :: IO ()
main = do
    rawArgs <- getArgs
    (cmdArgs, fileArgs) <- parseArgs rawArgs
    showDbgArguments cmdArgs
    configs <- loadConfig cmdArgs
    runApi configs
    return ()
