{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad (forM_)
import Data.Aeson (encode)

import qualified Data.ByteString.Lazy.Char8 as C8

import Pushover.Client

main :: IO ()
main = do
    Options{..} <- parseOptions

    case oAction of
        Receive -> do
            (Credentials s d) <- readCredentials oCredentials
            ms@(Messages msgs) <- fromResponse <$> getMessages s d

            forM_ (safeMaximum $ map mId msgs) $ \maxId -> do
                rsp <- updateHighestMessage s d maxId

                -- Force this value to trigger any errors
                fromResponse rsp `seq` return ()

            C8.putStrLn $ encode ms

        Register e p dn -> do
            (Login s) <- fromResponse <$> login e p
            (Device d) <- fromResponse <$> registerDevice s dn
            writeCredentials oCredentials $ Credentials s d
            putStrLn $ "Device registered. Credentials saved in " ++ oCredentials ++ "."

  where
    safeMaximum :: Ord a => [a] -> Maybe a
    safeMaximum [] = Nothing
    safeMaximum xs = Just $ maximum xs
