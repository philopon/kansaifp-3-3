{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.IO.Class
import Options.Declarative

greet :: Flag "g" '["greet"] "STRING" "greeting message" (Def "Hello" String)
      -> Arg "NAME" String
      -> Cmd "Greeting command" ()
greet (get -> msg) (get -> name) =
    liftIO $ putStrLn $ msg ++ ", " ++ name ++ "!"

main :: IO ()
main = run_ greet
