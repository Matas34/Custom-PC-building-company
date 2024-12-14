{-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import Data.ByteString
import Network.Wreq
import Data.String.Conversions
import Control.Lens
import Control.Monad.Free

main :: IO ()
main = do
    command <- interpreter $ add "CPU AMD ryzen5 5GHz"
    let rawRequest = cs command :: ByteString
    resp <- post "http://localhost:3000" rawRequest
    putStrLn $ cs $ resp ^. responseBody

    command <- interpreter $ search "CPU AMD ryzen5 5GHz"
    let rawRequest = cs command :: ByteString
    resp <- post "http://localhost:3000" rawRequest
    putStrLn $ cs $ resp ^. responseBody

    command <- interpreter $ add "GPU nvidia 4090 24GB"
    let rawRequest = cs command :: ByteString
    resp <- post "http://localhost:3000" rawRequest
    putStrLn $ cs $ resp ^. responseBody

    command <- interpreter $ showBuild
    let rawRequest = cs command :: ByteString
    resp <- post "http://localhost:3000" rawRequest
    putStrLn $ cs $ resp ^. responseBody

    command <- interpreter $ save
    let rawRequest = cs command :: ByteString
    resp <- post "http://localhost:3000" rawRequest
    putStrLn $ cs $ resp ^. responseBody

            
data MyQuery next = Add String (String -> next)
                  | Remove String (String -> next)
                  | ShowBuild (String -> next)
                  | Search String (String -> next)
                  | Compare String String (String -> next)
                  | Save (String -> next)
                  | Load (String -> next)
                  deriving Functor

type Query = Free MyQuery

add :: String -> Query String
add c = liftF $ Add c id

remove :: String -> Query String
remove c = liftF $ Remove c id

showBuild :: Query String
showBuild = liftF $ ShowBuild id

search :: String -> Query String
search c = liftF $ Search c id

compare :: String -> String -> Query String
compare c1 c2 = liftF $ Main.Compare c1 c2 id

save :: Query String
save = liftF $ Save id

load :: Query String
load = liftF $ Load id

interpreter :: Query a -> IO a
interpreter (Pure a) = return a
interpreter (Free step) = do
    next <- runStep step
    interpreter next
    where
        runStep :: MyQuery a -> IO a
        runStep (Add c next) = return $ next $ "Add " ++ c
        runStep (Remove c next) = return $ next $ "Remove " ++ c
        runStep (ShowBuild next) = return $ next "ShowBuild"
        runStep (Search c next) = return $ next $ "Search " ++ c
        runStep (Compare c1 c2 next) = return $ next $ "Compare " ++ c1 ++ " " ++ c2
        runStep (Save next) = return $ next "Save"
        runStep (Load next) = return $ next "Load"