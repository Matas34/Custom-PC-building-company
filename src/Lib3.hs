{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements (..)
    ) where

import Control.Concurrent ( Chan, writeChan, readChan, newChan )
import Control.Concurrent.STM( STM, TVar, atomically, writeTVar, readTVar)
import qualified Lib2
import qualified Data.Char as C
import qualified Data.List as L

data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  
  case op of
    Save content savedChan -> do
      writeFile "config.txt" content
      writeChan savedChan ()
      
    Load resultChan -> do
      content <- readFile "config.txt"
      writeChan resultChan content

  storageOpLoop chan

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

parseCommand :: String -> Either String (Command, String)
parseCommand [] = Left "Empty input, cannot parse a command"
parseCommand str =
    let
        command = L.takeWhile C.isLetter str
        rest = drop (length command + 1) str
    in
        case command of
            "Load" -> Right (LoadCommand, rest)
            "Save" -> Right (SaveCommand, rest)
            "BEGIN" ->
              case parseStatements str of
                Left e -> Left e
                Right (batch, rst) -> Right (StatementCommand batch, rst)
            _ -> 
              case parseStatements str of
                Left e -> Left e
                Right (single, rst) -> Right (StatementCommand single, rst)

parseStatements :: String -> Either String (Statements, String)
parseStatements input =
    let
        statements = []
    in
        case words input of
            ("BEGIN" : xs) -> parseBatch xs statements
            _ -> parseSingle input
  where
    parseBatch :: [String] -> [Lib2.Query] -> Either String (Statements, String)
    parseBatch ("END" : xs) queries = Right (Batch queries, unwords xs)
    parseBatch input queries = 
        case parseStatements (unwords input) of
            Left err -> Left err
            Right (Single query, input) -> parseBatch (words input) (queries ++ [query])
    
    parseSingle :: String -> Either String (Statements, String)
    parseSingle str = 
      let
        operation = L.takeWhile C.isLetter str
        rest = drop (length operation + 1) str
      in
        case operation of
            "Add" -> 
                case Lib2.parseComponent rest of
                    Right (component, restt) -> Right (Single (Lib2.Add component), restt)
                    Left err -> Left ("Failed to parse component: " ++ err)
            "Remove" -> 
                case Lib2.parseComponent rest of
                    Right (component, restt) -> Right (Single (Lib2.Remove component), restt)
                    Left err -> Left ("Failed to parse component: " ++ err)
            "ShowBuild" -> Right (Single (Lib2.ShowBuild), rest)
            "Search" -> 
                case Lib2.parseComponent rest of
                    Right (component, restt) -> Right (Single (Lib2.Search component), restt)
                    Left err -> Left ("Failed to parse component: " ++ err)
            "Compare" -> 
                case Lib2.parseTwoComponents rest of
                    Right ((component1, component2), restt) -> Right (Single (Lib2.Compare component1 component2), restt)
                    Left err -> Left ("Failed to parse components for comparison: " ++ err)
 
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State (Lib2.PC (Lib2.Components components))) = 
  case convert components [] of
    Right statements -> Batch statements

renderStatements :: Statements -> String
renderStatements statements = 
  let
    s = "BEGIN\n"
  in
    case statements of
      Batch queries ->
        case rendering queries s of
          st -> st ++ "END"
      Single query ->
        case rendering [query] "" of 
          st -> init st

rendering :: [Lib2.Query] -> String -> String
rendering [] s = s
rendering (query:queries) s = 
    case query of
      Lib2.Add component -> rendering queries (s ++ ("Add " ++ componentToString component) ++ "\n")

      Lib2.Remove component -> rendering queries (s ++ ("Remove " ++ componentToString component) ++ "\n")

      Lib2.ShowBuild -> rendering queries (s ++ "ShowBuild\n")

      Lib2.Search component -> rendering queries (s ++ ("Search " ++ (componentToString component) ++ "\n"))

      Lib2.Compare component1 component2 -> rendering queries (s ++ ("Compare " ++ (componentToString component1) ++ " " ++ (componentToString component2) ++ "\n"))

componentToString :: Lib2.Component -> String
componentToString (Lib2.CPUComponent (Lib2.CPU (Lib2.Brand b) (Lib2.Model m) (Lib2.Speed sp))) = "CPU " ++ b ++ " " ++ m ++ " " ++ show sp ++ "GHz"
componentToString (Lib2.GPUComponent (Lib2.GPU (Lib2.Brand b) (Lib2.Model m) (Lib2.Vram v))) = "GPU " ++ b ++ " " ++ m ++ " " ++show v ++ "GB"
componentToString (Lib2.MotherboardComponent (Lib2.Motherboard (Lib2.Brand b) (Lib2.Socket so))) = "MB " ++ b ++ " " ++ so
componentToString (Lib2.RAMComponent (Lib2.RAM (Lib2.Brand b) (Lib2.DDR d) (Lib2.Capacity c) (Lib2.Speed sp))) = "RAM " ++ b ++ " DDR" ++ show d ++ " " ++ show c ++ "GB " ++ show sp ++ "MHz"
componentToString (Lib2.StorageComponent (Lib2.Storage (Lib2.Brand b) (Lib2.StorageType st) (Lib2.Capacity c))) = "Storage " ++ b ++ " " ++ st ++ " " ++ show c ++ "GB"
componentToString (Lib2.PSUComponent (Lib2.PSU (Lib2.Brand b) (Lib2.Wattage w))) = "PSU " ++ b ++ " " ++ show w ++ "W"
componentToString (Lib2.CaseComponent (Lib2.Case (Lib2.Brand b) (Lib2.FormFactor f))) = "Case " ++ b ++ " " ++ f
componentToString (Lib2.PCComponent (Lib2.PC (Lib2.Components components))) = "PC [" ++ (init (renderPCComponents components "")) ++ "]"

renderPCComponents :: [Lib2.Component] -> String -> String
renderPCComponents []  s = s
renderPCComponents (component:components) s = (s ++ componentToString component ++ " " ++ renderPCComponents components s)

stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition stateVar cmd ioChan = do
    case cmd of
        StatementCommand q -> 
          case q of
            Single query -> atomically $ processSingleTransaction stateVar query
            
            Batch queries -> atomically $ processBatchTransaction stateVar queries

        SaveCommand -> do
            state <- atomically $ readTVar stateVar
            savedChan <- newChan
            writeChan ioChan (Save (renderStatements (marshallState state)) savedChan)
            readChan savedChan
            return $ Right (Just "State saved successfully")

        LoadCommand -> do
            state <- atomically $ readTVar stateVar
            resultChan <- newChan
            writeChan ioChan (Load resultChan)
            content <- readChan resultChan
            case parseStatements content of
                Left err -> return $ Left ("Failed to parse loaded state: " ++ err)
                Right (Batch queries, _) -> atomically $ loadBatch stateVar queries
                Right (Single query, _) -> atomically $ loadSingle stateVar query

loadBatch :: TVar Lib2.State -> [Lib2.Query] -> STM (Either String (Maybe String))
loadBatch stateVar queries = do
    case processQueries Lib2.emptyState queries of
        Left err -> return $ Left err
        Right newState -> do
            writeTVar stateVar newState
            return $ Right (Just "State loaded and applied successfully")

processSingleTransaction :: TVar Lib2.State -> Lib2.Query -> STM (Either String (Maybe String))
processSingleTransaction stateVar query = do
    state <- readTVar stateVar
    case Lib2.stateTransition state query of
        Left err -> return $ Left err
        Right (output, newState) -> do
            writeTVar stateVar newState
            return $ Right output

processBatchTransaction :: TVar Lib2.State -> [Lib2.Query] -> STM (Either String (Maybe String))
processBatchTransaction stateVar queries = do
    state <- readTVar stateVar
    case processQueries state queries of
        Left err -> return $ Left err
        Right newState -> do
            writeTVar stateVar newState
            return $ Right (Just "Batch completed successfully")

loadSingle :: TVar Lib2.State -> Lib2.Query -> STM (Either String (Maybe String))
loadSingle stateVar query = do
    case Lib2.stateTransition Lib2.emptyState query of
        Left err -> return $ Left err
        Right (_, newState) -> do
          writeTVar stateVar newState
          return $ Right (Just "State loaded and applied successfully")

processQueries :: Lib2.State -> [Lib2.Query] -> Either String Lib2.State
processQueries state [] = Right state
processQueries state (query:queries) = 
    case Lib2.stateTransition state query of
        Left err -> Left err
        Right (_, newState) -> processQueries newState queries

convert :: [Lib2.Component] -> [Lib2.Query] -> Either String [Lib2.Query]
convert [] queries = Right queries
convert (component:components) queries = 
    case component of
        Lib2.CPUComponent cpu -> convert components (queries ++ [Lib2.Add (Lib2.CPUComponent cpu)])
        Lib2.GPUComponent gpu -> convert components (queries ++ [Lib2.Add (Lib2.GPUComponent gpu)])
        Lib2.MotherboardComponent motherboard -> convert components (queries ++ [Lib2.Add (Lib2.MotherboardComponent motherboard)])
        Lib2.RAMComponent ram -> convert components (queries ++ [Lib2.Add (Lib2.RAMComponent ram)])
        Lib2.StorageComponent storage -> convert components (queries ++ [Lib2.Add (Lib2.StorageComponent storage)])
        Lib2.PSUComponent psu -> convert components (queries ++ [Lib2.Add (Lib2.PSUComponent psu)])
        Lib2.CaseComponent c -> convert components (queries ++ [Lib2.Add (Lib2.CaseComponent c)])
        Lib2.PCComponent pc -> convert components (queries ++ [Lib2.Add (Lib2.PCComponent pc)])