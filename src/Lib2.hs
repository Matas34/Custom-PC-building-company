{-# LANGUAGE InstanceSigs #-}

module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition,
      parseComponent,
      parseTwoComponents,
      Component(..),
      Components(..),
      PC(..),
      CPU(..),
      GPU(..),
      Motherboard(..),
      RAM(..),
      Storage(..),
      PSU(..),
      Case(..),
      Brand(..),
      Model(..),
      Speed(..),
      Vram(..),
      DDR(..),
      Socket(..),
      Capacity(..),
      StorageType(..),
      Wattage(..),
      FormFactor(..)
    ) where

import qualified Data.Char as C
import qualified Data.List as L

data Query = Add Component | Remove Component | ShowBuild | Search Component | Compare Component Component deriving (Show, Eq)

parseQuery :: String -> Either String Query
parseQuery [] = Left "Empty input, cannot parse an operation"
parseQuery str =
    let
        operation = L.takeWhile C.isLetter str
        rest = drop (length operation + 1) str
    in
        case operation of
            "Add" -> 
                case parseComponent rest of
                    Right (component, _) -> Right (Add component)
                    Left err -> Left ("Failed to parse component: " ++ err)
            "Remove" -> 
                case parseComponent rest of
                    Right (component, _) -> Right (Remove component)
                    Left err -> Left ("Failed to parse component: " ++ err)
            "ShowBuild" -> Right ShowBuild
            "Search" -> 
                case parseComponent rest of
                    Right (component, _) -> Right (Search component)
                    Left err -> Left ("Failed to parse component: " ++ err)
            "Compare" -> 
                case parseTwoComponents rest of
                    Right ((component1, component2), _) -> Right (Compare component1 component2)
                    Left err -> Left ("Failed to parse components for comparison: " ++ err)

            _ -> Left ("Unknown operation: " ++ operation)

data State = State PC deriving Show

emptyState :: State
emptyState = State (PC (Components []))


stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State (PC (Components components))) query =
  case query of
    Add component ->
        let newComponents = component : components
            newState = State (PC (Components newComponents))
        in Right (Just "Component added successfully.", newState)

    Remove component ->
      if component `notElem` components
        then Left "Component not found in the build."
        else 
          let newComponents = L.delete component components
              newState = State (PC (Components newComponents))
          in Right (Just "Component removed successfully.", newState)

    ShowBuild -> Right (Just (show (State (PC (Components components)))), State (PC (Components components)))

    Search component ->
        if component `elem` components
            then Right (Just "This component is in the build.", State (PC (Components components)))
        else Left("This components isn`t in the build.")

    Compare component1 component2 ->
        case (component1, component2) of
            (CPUComponent (CPU _ _ (Speed speed1)), CPUComponent (CPU _ _ (Speed speed2))) ->
                if speed1 == speed2
                    then Right(Just "CPUs are equal", State (PC (Components components)))
                else if speed1 > speed2
                    then Right(Just "The first CPU is better", State (PC (Components components)))
                else Right(Just "The second CPU is better", State (PC (Components components)))
            (GPUComponent (GPU _ _ (Vram vram1)), GPUComponent (GPU _ _ (Vram vram2))) ->
                    if vram1 == vram2
                    then Right(Just "GPUs are equal", State (PC (Components components)))
                else if vram1 > vram2
                    then Right(Just "The first GPU is better", State (PC (Components components)))
                else Right(Just "The second GPU is better", State (PC (Components components)))
            (RAMComponent (RAM _ (DDR ddr1) (Capacity capacity1) (Speed speed1)), RAMComponent (RAM _ (DDR ddr2) (Capacity capacity2) (Speed speed2))) ->
                if ddr1 > ddr2 then
                    Right (Just "The first RAM is better", State (PC (Components components)))
                else if ddr1 < ddr2 then
                    Right (Just "The second RAM is better", State (PC (Components components)))
                else
                    if capacity1 > capacity2 then
                        if speed1 > speed2 then
                            Right (Just "The first RAM is better", State (PC (Components components)))
                        else if speed1 == speed2 then
                            Right (Just "The first RAM is better", State (PC (Components components)))
                        else
                            Right (Just "The second RAM is better", State (PC (Components components)))
                    else if capacity1 < capacity2 then
                        if speed1 < speed2 then
                            Right (Just "The second RAM is better", State (PC (Components components)))
                        else if speed1 == speed1 then
                            Right (Just "The second RAM is better", State (PC (Components components)))
                        else
                            Right (Just "The first RAM is better", State (PC (Components components)))
                    else
                        if speed1 > speed2 then
                            Right (Just "The first RAM is better", State (PC (Components components)))
                        else if speed1 < speed2 then
                            Right (Just "The second RAM is better", State (PC (Components components)))
                        else
                            Right (Just "RAMs are equal", State (PC (Components components)))
            (StorageComponent (Storage _ (StorageType storagetype1) (Capacity cap1)), StorageComponent (Storage _ (StorageType storagetype2) (Capacity cap2))) ->
                if (storagetype1 /= storagetype2) then
                    if storagetype1 == "SSD" then Right(Just "The first storage device is better", State (PC (Components components)))
                    else Right(Just "The second storage device is better", State (PC (Components components)))
                else if (cap1 /= cap2) then
                    if cap1 > cap2 then Right(Just "The first storage device is better", State (PC (Components components)))
                    else Right(Just "The second storage device is better", State (PC (Components components)))
                else Right(Just "Both storage devices are equal", State (PC (Components components)))
            (PSUComponent (PSU _ (Wattage watt1)), PSUComponent (PSU _ (Wattage watt2))) ->
                    if watt1 > watt2 then
                        Right (Just "The first PSU is better", State (PC (Components components)))
                    else if watt1 < watt2 then
                        Right (Just "THe second PSU is better", State (PC (Components components)))
                    else
                        Right (Just "Both PSUs are equal", State (PC (Components components)))
            _ -> Left "Components aren`t comparable"

parseTwoComponents :: Parser (Component, Component)
parseTwoComponents = and3 (\a _ b -> (a, b)) parseComponent (parseChar ' ') parseComponent

type Parser a = String -> Either String (a, String)

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
parsePC = and3 (\_ components _ -> PC (Components components)) (parseChar '[') parseComponents (parseChar ']')

parseComponents :: Parser [Component]
parseComponents = and2 (:) parseComponent (many (and2 (\_ b -> b) (parseChar ' ') parseComponent))

parseComponent :: Parser Component
parseComponent str = 
    let 
        componentStr = L.takeWhile C.isLetter str
        rest = drop (length componentStr + 1) str
    in
        if componentStr == "CPU"
            then parseCPU rest
        else if componentStr == "GPU"
            then parseGPU rest
        else if componentStr == "MB"
            then parseMotherboard rest
        else if componentStr == "RAM"
            then parseRAM rest
        else if componentStr == "Storage"
            then parseStorage rest
        else if componentStr == "PSU"
            then parsePSU rest
        else if componentStr == "Case"
            then parseCase rest
        else if componentStr == "PC"
            then parseInnerPC rest
        else
            Left "Incorrect component type."



parseInnerPC :: Parser Component
parseInnerPC input = 
    case parsePC input of
        Right (pc, rest) -> Right (PCComponent pc, rest)
        Left err -> Left err


parseCPU :: Parser Component
parseCPU = and3
    (\brand model speed -> CPUComponent (CPU brand model speed))
    parseBrand
    (and2 (\_ b -> b) (parseChar ' ') parseModel)
    (and2 (\_ c -> c) (parseChar ' ') parseSpeed)

parseGPU :: Parser Component
parseGPU = and3
    (\brand model vram -> GPUComponent (GPU brand model vram))
    parseBrand
    (and2 (\_ b -> b) (parseChar ' ') parseModel)
    (and2 (\_ c -> c) (parseChar ' ') parseVram)

parseMotherboard :: Parser Component
parseMotherboard = and3 (\brand _ socket -> MotherboardComponent (Motherboard brand socket))
                parseBrand
                (parseChar ' ')
                parseSocket

parseRAM :: Parser Component
parseRAM = and4 (\brand ddr capacity speed -> RAMComponent (RAM brand ddr capacity speed))
                parseBrand
                (and2 (\_ ddr -> ddr) (parseChar ' ') parseDDR)
                (and2 (\_ capacity -> capacity) (parseChar ' ') parseCapacity)
                (and2 (\_ speed -> speed) (parseChar ' ') parseSpeed)

parseStorage :: Parser Component
parseStorage = and3 (\brand storagetype capacity -> StorageComponent (Storage brand storagetype capacity))
                parseBrand
                (and2 (\_ storagetype -> storagetype) (parseChar ' ') parseStorageType)
                (and2 (\_ capacity -> capacity) (parseChar ' ') parseCapacity)


parsePSU :: Parser Component
parsePSU = and3 (\brand _ wattage -> PSUComponent (PSU brand wattage))
                parseBrand
                (parseChar ' ')
                parseWattage

parseCase :: Parser Component
parseCase = and3 (\brand _ formfactor -> CaseComponent (Case brand formfactor))
                parseBrand
                (parseChar ' ')
                parseFormFactor

parseStorageType :: Parser StorageType
parseStorageType [] = Left "empty input, cannot parse a storage type"
parseStorageType str =
    let
        storagetype = L.takeWhile C.isLetter str
        rest = drop (length storagetype) str
    in
        case storagetype of
            _ -> 
                if storagetype == "SSD"
                then Right (StorageType storagetype, rest)
                else if storagetype == "HDD"
                then Right (StorageType storagetype, rest)
                else Left "Storage type must be 'SSD' or 'HDD'"

parseSocket :: Parser Socket
parseSocket [] = Left "empty input, cannot parse a socket"
parseSocket str =
    let
        socket = L.takeWhile (\c -> C.isLetter c || C.isDigit c) str
        rest = drop (length socket) str
    in
        case socket of
            [] -> Left "not a socket"
            _ -> 
                if socket == "LGA1700"
                then Right (Socket socket, rest)
                else if socket == "LGA1200"
                then Right (Socket socket, rest)
                else if socket == "AM4"
                then Right (Socket socket, rest)
                else if socket == "AM5"
                then Right (Socket socket, rest)
                else Left "Socket must be 'LGA1200' or 'LGA1200' or 'AM4' or 'AM5'"

parseFormFactor :: Parser FormFactor
parseFormFactor [] = Left "empty input, cannot parse a number"
parseFormFactor str =
    let
        formfactor = L.takeWhile (\c -> C.isLetter c || c == '-') str
        rest = drop (length formfactor) str
    in
        case formfactor of
            [] -> Left "not a brand"
            _ -> 
                if formfactor == "ATX"
                then Right (FormFactor formfactor, rest)
                else if formfactor == "Micro-ATX"
                then Right (FormFactor formfactor, rest)
                else Left "Form factor must be either 'ATX' or 'Micro-ATX'"

parseWattage :: Parser Wattage
parseWattage [] = Left "empty input, cannot parse wattage"
parseWattage str =
    let
        wattageStr = L.takeWhile C.isDigit str
        rest = drop (length wattageStr) str
    in
        case wattageStr of
            [] -> Left "not valid wattage"
            _ ->
                if "W" `L.isPrefixOf` rest
                then Right (Wattage (read wattageStr), drop 1 rest)
                else Left "Wattage must end with 'W'"

parseCapacity :: Parser Capacity
parseCapacity [] = Left "empty input, cannot parse capacity"
parseCapacity str =
    let
        capacityStr = L.takeWhile C.isDigit str
        rest = drop (length capacityStr) str
    in
        case capacityStr of
            [] -> Left "not valid capacity"
            _ ->
                if "GB" `L.isPrefixOf` rest
                then Right (Capacity (read capacityStr), drop 2 rest)
                else if "TB" `L.isPrefixOf` rest
                then Right (Capacity (read capacityStr), drop 2 rest)
                else Left "Capacity must end with 'GB' or 'TB'"

parseBrand :: Parser Brand
parseBrand [] = Left "empty input, cannot parse a brand"
parseBrand str =
    let
        brand = L.takeWhile C.isLetter str
        rest = drop (length brand) str
    in
        case brand of
            [] -> Left "not a brand"
            _ -> Right (Brand brand, rest)


parseModel :: Parser Model
parseModel [] = Left "empty input, cannot parse a model"
parseModel str =
    let
        model = L.takeWhile (\c -> C.isLetter c || C.isDigit c || c == '-') str
        rest = drop (length model) str
    in
        case model of
            [] -> Left "not a model"
            _ -> Right (Model model, rest)


parseSpeed :: Parser Speed
parseSpeed [] = Left "empty input, cannot parse speed"
parseSpeed str =
    let
        speedStr = L.takeWhile (\c -> C.isDigit c || c == '.') str
        rest = drop (length speedStr) str
    in
        case speedStr of
            [] -> Left "not a valid speed"
            _ ->
                if "GHz" `L.isPrefixOf` rest
                then Right (Speed (read speedStr), drop 3 rest)
                else if "MHz" `L.isPrefixOf` rest
                then Right (Speed (read speedStr), drop 3 rest)
                else Left "Speed must end with 'GHz' or 'MHz'"

parseVram :: Parser Vram
parseVram [] = Left "empty input, cannot parse vram"
parseVram str =
    let
        vramStr = L.takeWhile C.isDigit str
        rest = drop (length vramStr) str
    in
        case vramStr of
            [] -> Left "not a valid vram"
            _ ->
                if "GB" `L.isPrefixOf` rest
                then Right (Vram (read vramStr), drop 2 rest)
                else Left "Vram must end with 'GB'"

parseDDR :: Parser DDR
parseDDR [] = Left "empty input, cannot parse DDR type"
parseDDR str =
    let
        ddrStr = L.takeWhile C.isLetter str
        rest = drop (length ddrStr) str
    in
        if ddrStr /= "DDR"
        then Left "missing DDR"
        else
            if "4" `L.isPrefixOf` rest
            then Right (DDR 4, drop 1 rest)
            else if "5" `L.isPrefixOf` rest
            then Right (DDR 5, drop 1 rest)
            else if "6" `L.isPrefixOf` rest
            then Right (DDR 6, drop 1 rest)
            else Left "DDR must end with '4', '5', or '6'"

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find the character '" ++ [c] ++ "' in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 c a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (c v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f a b c = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right (f v1 v2 v3, r3)
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

and4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4 f a b c d = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) ->
                            case d r3 of
                                Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
                                Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

many :: Parser a -> Parser [a]
many p = many' p []
    where
        many' p' acc = \input ->
            case p' input of
                Left _ -> Right (acc, input)
                Right (v, r) -> many' p' (acc ++ [v]) r
