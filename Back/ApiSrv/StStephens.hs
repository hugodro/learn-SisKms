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

module StStephens (runApi, ApiParameters (..)) where

import Prelude ()
import Prelude.Compat

import qualified Data.ByteString.Char8 as C8


-- Servant:
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.List (map)
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser

-- Hasql:
import Data.Int
import Data.Text
import qualified Data.Vector (map, mapM, toList)
import PostgreSQL.Binary.Data
import Data.Functor.Contravariant
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import qualified Hasql.TH as TH
import qualified Data.Map as DMap

-- API Model:

-- Starting the logic:
data ApiParameters = ApiParams {
      port :: Int
      , db :: String
      , host :: String
      , user :: String
      , pswd :: String
  }


type ItemsDict = DMap.Map String [AssetModel]

data ApiResult =
    Ok { result :: String, items :: ItemsDict }
  | Err { result :: String, reason :: String }
  deriving Generic

instance ToJSON ApiResult


data AssetModel = AssetModel (String, String, String)
   deriving Generic

instance ToJSON AssetModel


type CategAssetModel = (String, AssetModel)


type EndpointAPI = "apistub" :> QueryParam "nodeid" Integer :> Post '[JSON] ApiResult


webHandlers :: Connection.Connection -> Server EndpointAPI
webHandlers connection = assetLister
  where assetLister :: Maybe Integer -> Handler ApiResult
        assetLister anID = do
            liftIO $ putStrLn "@[assetLister] starting."
            dbResult <- liftIO $ getAllAssets connection anID
            case dbResult of
                Right itemsDict -> return (Ok "ok" itemsDict)
                Left errMsg -> return (Err "err" errMsg)


webApi :: Proxy EndpointAPI
webApi = Proxy


runApi :: ApiParameters -> IO ()
runApi configs = do
  putStrLn "@[runApi] starting."
  connection <- dbConnect (db configs) (host configs) (user configs) (pswd configs)
  run (port configs) (addAllOriginsMiddleware (serve webApi $ webHandlers connection))
  -- run 7001 (serve webApi $ webHandlers connection)


-- Support CORS authorisation:
addAllOriginsMiddleware :: Application -> Application
addAllOriginsMiddleware baseApp = \req responseFunc -> baseApp req (responseFunc . addOriginsAllowed)


addOriginsAllowed :: Response -> Response
addOriginsAllowed = mapResponseHeaders $
  (:) ("Access-Control-Allow-Origin", "*")


-- Hasql:
-- assetSelectStmt -> category, label, locator, doneOn.
type AssetDbRc = (Text, Text, Text, Text)

dbConnect :: String -> String -> String -> String -> IO Connection.Connection
dbConnect db host user pswd = do
   result <- Connection.acquire connectionSettings
   case result of
       Right connection -> return connection
       Left errorMsg -> error $ show errorMsg
  where
    connectionSettings = Connection.settings (C8.pack host) 5432 (C8.pack user) (C8.pack pswd) (C8.pack db)


getAllAssets :: Connection.Connection -> Maybe Integer -> IO (Either String ItemsDict)
getAllAssets dbConn anID = do
    dbRez <- Session.run fetchAssetsDB dbConn
    case mapAssetRz dbRez of
        Right categAssets ->
            return $ Right $ Prelude.Compat.foldr (\(k, v) m -> DMap.insertWith (++) k [v] m) (DMap.empty :: ItemsDict) categAssets
        Left err -> return $ Left $ (show err)


mapAssetRz :: Either Session.QueryError (Vector AssetDbRc) -> Either String [ CategAssetModel ]
mapAssetRz aValue =
  case aValue of
        Right result ->
          Right $ Data.Vector.toList $ Data.Vector.map mapRecordToModel result
          where
            mapRecordToModel :: AssetDbRc -> CategAssetModel
            mapRecordToModel (cate, label, locator, doneOn)=
               ((unpack cate), AssetModel ((unpack label), (unpack locator), (unpack doneOn)))
        Left err -> Left $ "Bad DB: " ++ (show err)


fetchAssetsDB :: Session (Vector AssetDbRc)
fetchAssetsDB = do
  Session.statement () assetSelectStmt

assetSelectStmt :: Statement () (Vector AssetDbRc)
assetSelectStmt = [TH.vectorStatement|
        select b.label::text, a.label::text, a.locator::text, a.doneOn::text from "simpleasset" a
            join "categories" b on b.id = a.categfk
      |]
