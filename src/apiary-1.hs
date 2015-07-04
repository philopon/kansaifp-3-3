{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiary
import Control.Monad.Apiary.Filter.Capture

import Data.Text(Text)
import Data.Proxy
import Data.Monoid

import Network.Wai.Handler.Warp

main :: IO ()
main = runApiary (run 3000) def $ do

    -- /hello/:name に対するルーテング
    path "hello" .
        fetch (Proxy :: Proxy ("name" := Text)) Nothing .
        action $ do
            -- nameキーが存在していないとエラー
            -- 加えてnameはもともとText型に変換済み
            name <- param (Proxy :: Proxy "name")
            text $ "Hello, " <> name

    -- 流石にダルいのでQQを使える
    [capture|/hello2/name::Text|] . action $ do
        name <- param [key|name|]
        text $ "Hello, " <> name

