{-# LANGUAGE OverloadedStrings #-}
module MegaCodCoords where

-- import MegaCodCoords.Internal
import qualified Data.Aeson as JSON
import Control.Monad.Except

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

import Control.Concurrent (threadDelay)


-- import Data.Maybe (isNothing)
-- import qualified Geocoder as Geocoder
import Geography.Geocoding.Google (geoEncode)

import GHC.Float

import CodInfo

type App = ExceptT String IO

-- openCageKey :: T.Text
-- openCageKey = "92411b86788531bbb42df90a58c3d32b"

-- testGeoCoder :: Geocoder.TokenToGeocode -> IO (Maybe Geocoder.ResponseBody)
-- testGeoCoder s = Geocoder.getAPIResponse s openCageKey


parseFile :: String -> App ([CodInfo])
parseFile fn =
  do c <- liftIO $ BS.readFile fn
     case JSON.eitherDecode c of
       Right cods -> return cods
       Left err -> throwError $ "error parsing file: " ++ err


reportEmptyAddresses :: (MonadIO m) => [CodInfo] -> m ()
reportEmptyAddresses cis =
  do liftIO $ putStr "empty addresses count: "
     liftIO $ putStrLn . show . length $ filter isEmptyAddress cis
  -- where isEmptyAddress = isNothing . ciAddress


reportEmptyCoords :: (MonadIO m) => [CodInfo] -> m ()
reportEmptyCoords cis =
  do liftIO $ putStr "empty coords count: "
     liftIO $ putStrLn . show . length $ filter isEmptyCoords cis
  -- where isEmptyCoords = isNothing . ciCoords

withDelay :: (MonadIO m) => Int -> (a -> m a) -> a -> m a
withDelay secs f x = liftIO (threadDelay (secs * 1000000)) >> f x

fillCoords :: (MonadIO m) => CodInfo -> m CodInfo
fillCoords c = case (ciAddress c) of
                 Just a -> do
                   cc <- getCoords a
                   return $ c {ciCoords = cc }
                 Nothing -> return c
  where getCoords :: (MonadIO m) => TextField -> m (Maybe Coords)
        getCoords a =
          do r <- liftIO $ geoEncode (T.unpack a)
             case r of
               Right (lat, lon) -> return $ Just $ Coords (double2Float lat) (double2Float lon)
               Left _ -> return Nothing


addCodCoords :: String -> String -> IO ()
addCodCoords inP outP =
  do r <- runExceptT $ do
       cods <- parseFile inP >>= mapM (withDelay 1 fillCoords)
       reportEmptyAddresses cods
       reportEmptyCoords cods
       liftIO $ BS.writeFile outP . JSON.encode $ cods
     putStrLn . show $ r

-- fillCoords' :: (MonadIO m) => CodInfo -> m CodInfo
-- fillCoords' c = return c >>= maybe (const (return c))  (return id)
