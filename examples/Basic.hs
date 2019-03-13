#!/usr/bin/env stack
{- stack
   --resolver lts-13.12
   runghc
   --package unliftio
   --package warp
   --package webby
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text                as T
import qualified Network.Wai.Handler.Warp as W
import           UnliftIO                 (liftIO)
import qualified UnliftIO.Exception       as E

import           Webby

main :: IO ()
main = do
    let routes = [ get "/api/a" (text "a\n")
                 , get "/api/b" (text "b\n")
                 , post "/api/capture/:id"
                   (do idVal :: Int <- getCapture "id"
                       text $ (T.pack (show idVal) `T.append` "\n")
                   )
                 , get "/api/showEnv" (do env <- getAppEnv
                                          text env
                                      )
                 , get "/aaah" (liftIO $ E.throwString "oops!")
                 ]

    webbyApp <- mkWebbyApp ("MyEnv\n" :: T.Text) routes
    putStrLn "Starting webserver..."
    W.runEnv 7000 webbyApp
