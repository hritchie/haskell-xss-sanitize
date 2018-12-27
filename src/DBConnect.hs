{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- New style of container connection to MySQL
-- We can read MYSQL_* env variables directly, instead of more convoluting shared
-- mounting of /app/config/database.yml

module DBConnect where
import           Prelude               (read)
import           Protolude

import           Control.Arrow
import           Control.Monad         (mzero)
import           Control.Monad.Catch   (Handler (..))
import           Control.Retry         (RetryPolicyM, RetryStatus,
                                        constantDelay, defaultLogMsg,
                                        limitRetries, logRetries, recovering)
import           Data.Aeson
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Pool             (Pool, createPool)
import           Data.Text             (Text, pack, unpack)
import           Data.Time.Clock       (getCurrentTime)
import           Database.MySQL.Base   (MySQLError, Option (..))
import           Database.MySQL.Simple
import           System.Environment

getConnectionFromEnv :: IO Connection
getConnectionFromEnv =
  getConnectInfoFromEnv >>= connect

getConnectInfoFromEnv :: IO ConnectInfo
getConnectInfoFromEnv = do
  ps <- getEnvironment
  let v1 :: Map Text Value
      v1 =  M.mapWithKey (\k v ->
              case (k, v) of
                ("MYSQL_PORT", String port) -> toJSON ((read . unpack $ port) :: Int)
                _ -> v
            ) $ M.fromList $ map (pack *** toJSON) ps

      v2 :: Value
      v2 = toJSON v1
      r = fromJSON v2
  case r of
    Success c -> pure c
    Error err -> die $ "Missing MySQL environment variables: " <> show err


-- FIXME orphaned instance
instance FromJSON ConnectInfo where
    parseJSON (Object v) =
        ConnectInfo <$> v .:? "MYSQL_HOST" .!= "localhost"
                    <*> v .:? "MYSQL_PORT" .!= 3306
                    <*> v .:? "MYSQL_USERNAME" .!= "root"
                    <*> v .:? "MYSQL_PASSWORD".!= ""
                    <*> v .: "MYSQL_DATABASE"
                    <*> pure [CharsetName "utf8"]
                    <*> v .:? "MYSQL_SOCKET" .!= "" -- "/tmp/mysql.sock"
                    <*> pure Nothing
    parseJSON _ = mzero


waitForConnection :: IO ()
waitForConnection = do
  putText "Waiting for MySQL connection"
  retry' 2000000 100 $ do
    c <- getConnectionFromEnv
    Only (_ :: Int):_ <- query_ c "select count(*) from users"
    return ()


-- 50 ms delay up to 3 times
retry :: IO a -> IO a
retry = retry' 50000 3

retry' :: Int -> Int -> IO a -> IO a
retry' delay limit action = do
    let f _ = action
    recovering myRetryPolicy [h1] f
  where myRetryPolicy :: RetryPolicyM IO
        myRetryPolicy = constantDelay delay <> limitRetries limit
        h1 :: RetryStatus -> Handler IO Bool
        h1 status = do
            let test _ = return True
            let reporter :: Bool -> MySQLError -> RetryStatus -> IO ()
                reporter shouldRetry err retryStatus = do
                  let msg = defaultLogMsg shouldRetry err retryStatus
                  t <- getCurrentTime
                  putText $ "Retrying failed MySQL query at: " <> show t
                  putText $ pack msg
                  return ()
            -- hPutStrLn stderr $ "retry status " ++ show status
            logRetries test reporter status

maxConnections :: Int
maxConnections = 4

getConnPool :: IO (Pool Connection)
getConnPool = do
  connInfo <- getConnectInfoFromEnv
  createPool (connect connInfo) close 1 1 maxConnections
