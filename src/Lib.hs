{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( knock001
    , knock002
    , knock003
    , knock004
    ) where

import qualified Data.Text as T

-- stack build
-- stack ghc -- -isrc app/Main.hs
-- app/Main.exe


title :: String -> IO ()
title = putStrLn
p :: String -> IO ()
p text = putStrLn $ "  " ++ text

knock001 :: IO ()
knock001 = do
    title "001"
    p $ reverse "stressed"

knock002 :: IO ()
knock002 = do
    title "002"
    -- putStrLn "パタトクカシーー"
    let text = T.pack "パタトクカシーー"
    let t = T.unpack text
    p $ [t !! 1, t !! 3, t !! 5, t !! 7]
    -- putChar $ t !! 1

knock003 :: IO ()
knock003 = do
    title "003"
    let text1 = T.pack "パトカー"
    let text2 = T.pack "タクシー"
    let t1 = T.unpack text1
    let t2 = T.unpack text2
    let z = zip t1 t2
    p $ foldl (++) "" $ map (\(a,b)->[a,b]) z

knock004 :: IO ()
knock004 = do
    title "004"
    let text = T.pack "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
    -- putStrLn $ show $ T.length text
    -- let sep = T.pack " "
    let list = T.split (\c -> c == ' ') text
    let words0 = map (\t -> T.replace "," "" t) list
    let words = map (\t -> T.unpack $ T.replace "." "" t) words0
    mapM_ p words
    p $ concat $ map (show . length) words
