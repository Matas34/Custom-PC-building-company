{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class(liftIO)
import Data.String.Conversions
import Web.Scotty

import Control.Applicative
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans.State (State, runState, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Concurrent ( Chan, writeChan, readChan, newChan )
import Control.Concurrent (forkIO)
import Control.Concurrent.STM(TVar, newTVarIO, readTVarIO)
import qualified Data.List as L
import qualified Data.Char as C

import qualified Lib2
import qualified Lib3

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

type Parser a = ExceptT String (State String) a

main :: IO ()
main = do
    chan <- newChan :: IO (Chan Lib3.StorageOp)
    state <- newTVarIO Lib2.emptyState
    _ <- forkIO $ Lib3.storageOpLoop chan
    
    scotty 3000 $ do
        post "/" $ do
            b <- body
            case Lib3.parseCommand (cs b) of
                Left e -> text $ cs e
                Right (c, "") -> do
                    tr <- liftIO $ Lib3.stateTransition state c chan
                    case tr of
                        Left e2 -> text $ cs e2
                        Right m -> case m of
                            Nothing -> text "No result. (error?)"
                            Just result -> text $ cs result
                Right (_, r) -> text $ cs ("PARSE ERROR: string is not fully consumed - " ++ r)

data PC = PC Components deriving (Show, Eq)

data Components = Components [Component] deriving (Show, Eq)

data Component = CPUComponent CPU | GPUComponent GPU | MotherboardComponent Motherboard | RAMComponent RAM | StorageComponent Storage | PSUComponent PSU | CaseComponent Case | PCComponent PC deriving (Show, Eq)

data CPU = CPU Brand Model Speed deriving (Show, Eq)
data GPU = GPU Brand Model Vram deriving (Show, Eq)
data Motherboard = Motherboard Brand Socket deriving (Show, Eq)
data RAM = RAM Brand DDR Capacity Speed deriving (Show, Eq)
data Storage = Storage Brand StorageType Capacity deriving (Show, Eq)
data PSU = PSU Brand Wattage deriving (Show, Eq)
data Case = Case Brand FormFactor deriving (Show, Eq)

data Brand = Brand String deriving (Show, Eq)
data Model = Model String deriving (Show, Eq)
data Speed = Speed Double deriving (Show, Eq)
data Vram = Vram Integer deriving (Show, Eq)
data DDR = DDR Integer deriving (Show, Eq)
data Socket = Socket String deriving (Show, Eq)
data Capacity = Capacity Integer deriving (Show, Eq)
data StorageType = StorageType String deriving (Show, Eq)
data Wattage = Wattage Integer deriving (Show, Eq)
data FormFactor = FormFactor String deriving (Show, Eq)


parsePC :: Parser PC
parsePC = (\_ components _ -> PC (Components components)) <$> (parseChar '[') <*> parseComponents <*> (parseChar ']')

parseComponents :: Parser [Component]
parseComponents = (:) <$> parseComponent <*> some ((\_ b -> b) <$> (parseChar ' ') <*> parseComponent)

parseTwoComponents :: Parser (Component, Component)
parseTwoComponents = (\a _ b -> (a, b)) <$> parseComponent <*> (parseChar ' ') <*> parseComponent

parseComponent :: Parser Component
parseComponent = do
    input <- lift S.get
    let componentStr = L.takeWhile C.isLetter input
        rest = drop (length componentStr + 1) input
    lift (S.put rest)
    case componentStr of
        "CPU"      -> parseCPU
        "GPU"      -> parseGPU
        "MB"       -> parseMotherboard
        "RAM"      -> parseRAM
        "Storage"  -> parseStorage
        "PSU"      -> parsePSU
        "Case"     -> parseCase
        "PC"       -> parseInnerPC
        _          -> throwE "Incorrect component type."

parseInnerPC :: Parser Component
parseInnerPC = do
    pc <- parsePC
    return (PCComponent pc)

parseCPU :: Parser Component
parseCPU = (\brand model speed -> CPUComponent (CPU brand model speed)) <$> parseBrand <*> (parseChar ' ' *> parseModel) <*> (parseChar ' ' *> parseSpeed)

parseGPU :: Parser Component
parseGPU = (\brand model vram -> GPUComponent (GPU brand model vram)) <$> parseBrand <*> (parseChar ' ' *> parseModel) <*> (parseChar ' ' *> parseVram)

parseMotherboard :: Parser Component
parseMotherboard = (\brand _ socket -> MotherboardComponent (Motherboard brand socket)) <$> parseBrand <*> (parseChar ' ') <*> parseSocket

parseRAM :: Parser Component
parseRAM = (\brand ddr capacity speed -> RAMComponent (RAM brand ddr capacity speed)) <$> parseBrand <*> (parseChar ' ' *> parseDDR) <*> (parseChar ' ' *> parseCapacity) <*> (parseChar ' ' *> parseSpeed)

parseStorage :: Parser Component
parseStorage = (\brand storagetype capacity -> StorageComponent (Storage brand storagetype capacity)) <$> parseBrand <*> (parseChar ' ' *> parseStorageType) <*> (parseChar ' ' *> parseCapacity)

parsePSU :: Parser Component
parsePSU = (\brand _ wattage -> PSUComponent (PSU brand wattage)) <$> parseBrand <*> (parseChar ' ') <*> parseWattage

parseCase :: Parser Component
parseCase = (\brand _ formfactor -> CaseComponent (Case brand formfactor)) <$> parseBrand <*> (parseChar ' ') <*> parseFormFactor

parseStorageType :: Parser StorageType
parseStorageType = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse a storage type"
        _ -> do
            let storagetype = L.takeWhile C.isLetter input
                rest = drop (length storagetype) input
            if storagetype == "SSD" || storagetype == "HDD"
                then lift (S.put rest) >> return (StorageType storagetype)
                else throwE "Storage type must be 'SSD' or 'HDD'"

parseSocket :: Parser Socket
parseSocket = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse a socket"
        _ -> do
            let socket = L.takeWhile (\c -> C.isLetter c || C.isDigit c) input
                rest = drop (length socket) input
            if socket `elem` ["LGA1700", "LGA1200", "AM4", "AM5"]
                then lift (S.put rest) >> return (Socket socket)
                else throwE "Socket must be 'LGA1200' or 'LGA1700' or 'AM4' or 'AM5'"

parseFormFactor :: Parser FormFactor
parseFormFactor = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse a form factor"
        _ -> do
            let formfactor = L.takeWhile (\c -> C.isLetter c || c == '-') input
                rest = drop (length formfactor) input
            if formfactor `elem` ["ATX", "Micro-ATX"]
                then lift (S.put rest) >> return (FormFactor formfactor)
                else throwE "Form factor must be either 'ATX' or 'Micro-ATX'"

parseWattage :: Parser Wattage
parseWattage = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse wattage"
        _ -> do
            let wattageStr = L.takeWhile C.isDigit input
                rest = drop (length wattageStr) input
            if "W" `L.isPrefixOf` rest
                then lift (S.put (drop 1 rest)) >> return (Wattage (read wattageStr))
                else throwE "Wattage must end with 'W'"

parseCapacity :: Parser Capacity
parseCapacity = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse capacity"
        _ -> do
            let capacityStr = L.takeWhile C.isDigit input
                rest = drop (length capacityStr) input
            if "GB" `L.isPrefixOf` rest || "TB" `L.isPrefixOf` rest
                then lift (S.put (drop 2 rest)) >> return (Capacity (read capacityStr))
                else throwE "Capacity must end with 'GB' or 'TB'"

parseBrand :: Parser Brand
parseBrand = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse a brand"
        _ -> do
            let brand = L.takeWhile C.isLetter input
                rest = drop (length brand) input
            if not (null brand)
                then lift (S.put rest) >> return (Brand brand)
                else throwE "Not a brand"

parseModel :: Parser Model
parseModel = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse a model"
        _ -> do
            let model = L.takeWhile (\c -> C.isLetter c || C.isDigit c || c == '-') input
                rest = drop (length model) input
            if not (null model)
                then lift (S.put rest) >> return (Model model)
                else throwE "Not a model"

parseSpeed :: Parser Speed
parseSpeed = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse speed"
        _ -> do
            let speedStr = L.takeWhile (\c -> C.isDigit c || c == '.') input
                rest = drop (length speedStr) input
            if "GHz" `L.isPrefixOf` rest || "MHz" `L.isPrefixOf` rest
                then lift (S.put (drop 3 rest)) >> return (Speed (read speedStr))
                else throwE "Speed must end with 'GHz' or 'MHz'"

parseVram :: Parser Vram
parseVram = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse vram"
        _ -> do
            let vramStr = L.takeWhile C.isDigit input
                rest = drop (length vramStr) input
            if "GB" `L.isPrefixOf` rest
                then lift (S.put (drop 2 rest)) >> return (Vram (read vramStr))
                else throwE "Vram must end with 'GB'"


parseDDR :: Parser DDR
parseDDR = do
    input <- lift S.get
    case input of
        [] -> throwE "empty input, cannot parse DDR type"
        _ -> do
            let ddrStr = L.takeWhile C.isLetter input
            let rest = drop (length ddrStr) input
        
            if ddrStr /= "DDR" then throwE "missing DDR"
            else do
                if "4" `L.isPrefixOf` rest
                    then lift (S.put (drop 1 rest)) >> return (DDR 4)
                else if "5" `L.isPrefixOf` rest
                    then lift (S.put (drop 1 rest)) >> return (DDR 5)
                else if "6" `L.isPrefixOf` rest
                    then lift (S.put (drop 1 rest)) >> return (DDR 6)
                else throwE "DDR must end with '4', '5', or '6'"


parseChar :: Char -> Parser Char
parseChar a = do
    input <- lift S.get
    case input of
        [] -> throwE "Empty input"
        (x:xs) -> if x == a
            then lift $ S.put xs >> return x
            else
                throwE $ a:" is not found"