{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Event sourcing framework for hackage-server.
--
-- 'makeAcidic' generates per-event beam table types and 'SaveEvent'
-- instances with typed beam INSERTs. All beam identifiers are resolved
-- hygienically here — calling modules do NOT need beam imports.
--
-- Calling modules need: @DerivingStrategies@, @DeriveGeneric@,
-- @DeriveAnyClass@, @TypeFamilies@, @FlexibleInstances@,
-- @MultiParamTypeClasses@, @OverloadedStrings@
module Distribution.Server.Framework.EventSourcing
  ( -- * Typeclasses
    QueryEvent(..)
  , UpdateEvent(..)
  , SaveEvent(..)
    -- * Query/Update monads
  , Query
  , Update
    -- * Template Haskell
  , makeAcidic
  ) where

import Control.Monad.Reader (Reader)
import qualified Control.Monad.Trans.Reader as Reader (ReaderT)
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.Trans.State.Lazy as State (StateT)
import Data.Functor.Identity (Identity)
import Data.List (isInfixOf)
import GHC.Generics (Generic)
import Language.Haskell.TH

import Distribution.Server.Framework.PgTx (PgTx, beamTx)

-- Beam imports — used by TH quotations ('', ') so they resolve
-- hygienically at this module, not at the splice site.
import Database.Beam
  ( Beamable, Table(..), C, Columnar
  , DatabaseSettings, Database, DatabaseEntity, TableEntity
  , runInsert, insert, insertValues
  , withDbModification, defaultDbSettings
  , modifyTableFields, tableModification, setEntityName
  )
import Database.Beam.Backend.SQL (HasSqlValueSyntax)
import Database.Beam.Postgres (Postgres, runBeamPostgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)

type Query st a = Reader st a
type Update st a = State st a

class QueryEvent ev where
  type QueryState ev :: *
  type QueryResult ev :: *
  runQueryEvent :: ev -> Query (QueryState ev) (QueryResult ev)

class UpdateEvent ev where
  type UpdateState ev :: *
  type UpdateResult ev :: *
  runUpdateEvent :: ev -> Update (UpdateState ev) (UpdateResult ev)

class UpdateEvent ev => SaveEvent ev where
  saveEvent :: ev -> PgTx ()
  saveEvent _ev = return ()

-- * Template Haskell

makeAcidic :: Name -> [Name] -> Q [Dec]
makeAcidic stateName fns = do
  let prefix = camelToSnake (nameBase stateName)
  concat <$> mapM (makeEvent prefix) fns

makeEvent :: String -> Name -> Q [Dec]
makeEvent prefix fnName = do
  info <- reify fnName
  case info of
    VarI _ ty _ -> makeEventFromType prefix fnName ty
    _ -> fail $ "makeAcidic: " ++ show fnName ++ " is not a function"

makeEventFromType :: String -> Name -> Type -> Q [Dec]
makeEventFromType prefix fnName ty = do
  let (argTypes, retType) = splitFnType ty
  case classifyReturn retType of
    Just (IsQuery stType resultType) ->
      makeQueryEvent fnName argTypes stType resultType
    Just (IsUpdate stType resultType) ->
      makeUpdateEvent prefix fnName argTypes stType resultType
    Nothing -> fail $ "makeAcidic: " ++ show fnName
                   ++ " does not return Query or Update. Got: " ++ show retType

data ReturnClass = IsQuery Type Type | IsUpdate Type Type

classifyReturn :: Type -> Maybe ReturnClass
classifyReturn (AppT (AppT (AppT (ConT rname) stType) (ConT iname)) resultType)
  | rname == ''Reader.ReaderT && iname == ''Identity = Just (IsQuery stType resultType)
  | rname == ''State.StateT && iname == ''Identity = Just (IsUpdate stType resultType)
classifyReturn (AppT (AppT (ConT qname) stType) resultType)
  | nameBase qname == "Query"  = Just (IsQuery stType resultType)
  | nameBase qname == "Update" = Just (IsUpdate stType resultType)
classifyReturn _ = Nothing

splitFnType :: Type -> ([Type], Type)
splitFnType (ForallT _ _ t) = splitFnType t
splitFnType (AppT (AppT ArrowT arg) rest) =
  let (args, ret) = splitFnType rest
  in (arg : args, ret)
splitFnType t = ([], t)

makeQueryEvent :: Name -> [Type] -> Type -> Type -> Q [Dec]
makeQueryEvent fnName argTypes stType resultType = do
  let eventName = toEventName fnName
  argNames <- mapM (\_ -> newName "x") argTypes
  let con = NormalC eventName (map (\t -> (Bang NoSourceUnpackedness NoSourceStrictness, t)) argTypes)
      dataDec = DataD [] eventName [] Nothing [con] []
      instDec = InstanceD Nothing []
        (AppT (ConT ''QueryEvent) (ConT eventName))
        [ TySynInstD (TySynEqn Nothing
            (AppT (ConT ''QueryState) (ConT eventName)) stType)
        , TySynInstD (TySynEqn Nothing
            (AppT (ConT ''QueryResult) (ConT eventName)) resultType)
        , FunD 'runQueryEvent
            [Clause [ConP eventName [] (map VarP argNames)]
                    (NormalB (foldl AppE (VarE fnName) (map VarE argNames)))
                    []]
        ]
  return [dataDec, instDec]

makeUpdateEvent :: String -> Name -> [Type] -> Type -> Type -> Q [Dec]
makeUpdateEvent prefix fnName argTypes stType resultType = do
  let eventBaseName = nameBase fnName
      eventName = toEventName fnName
      tableName = prefix ++ "__" ++ camelToSnake eventBaseName

      -- "replace" and "migrate" events are always checkpoint-only
      isNameBasedCheckpoint = take 7 eventBaseName == "replace"
                           || take 7 eventBaseName == "migrate"
                           || take 6 eventBaseName == "resign"
                           -- Core package events (complex state interactions)
                           || take 3 eventBaseName == "add" && "Package" `isInfixOf` eventBaseName
                           || eventBaseName `elem` [ "deletePackage", "setPackageUploader"
                             , "setPackageUploadTime", "addOtherIndexEntry"
                             , "updatePackageInfo", "updateSecurityState"
                             , "setRootMirrorsAndKeys", "setTarGzFileInfo"
                             , "updateCandidatePkgInfo", "deleteCandidate"
                             , "deleteCandidates", "setMigratedPkgTarball"
                             , "addCandidate"
                             ]

  -- Check if all arg types have HasSqlValueSyntax PgValueSyntax instances.
  -- If any arg lacks an instance, fall back to checkpoint-only.
  hasInstances <- if isNameBasedCheckpoint || null argTypes
    then return False
    else and <$> mapM hasBeamInstance argTypes

  let isCheckpointEvent = isNameBasedCheckpoint || (not (null argTypes) && not hasInstances)

  argNames <- mapM (\_ -> newName "x") argTypes

  -- Event data type
  let con = NormalC eventName (map (\t -> (Bang NoSourceUnpackedness NoSourceStrictness, t)) argTypes)
      dataDec = DataD [] eventName [] Nothing [con] []

  -- UpdateEvent instance
  let updateInstDec = InstanceD Nothing []
        (AppT (ConT ''UpdateEvent) (ConT eventName))
        [ TySynInstD (TySynEqn Nothing
            (AppT (ConT ''UpdateState) (ConT eventName)) stType)
        , TySynInstD (TySynEqn Nothing
            (AppT (ConT ''UpdateResult) (ConT eventName)) resultType)
        , FunD 'runUpdateEvent
            [Clause [ConP eventName [] (map VarP argNames)]
                    (NormalB (foldl AppE (VarE fnName) (map VarE argNames)))
                    []]
        ]

  if isCheckpointEvent || null argTypes
    then do
      -- No event logging for checkpoint events
      let saveInstDec = InstanceD Nothing []
            (AppT (ConT ''SaveEvent) (ConT eventName)) []
      return [dataDec, updateInstDec, saveInstDec]
    else do
      beamDecs <- makeBeamEventTable tableName eventName argTypes argNames
      return $ [dataDec, updateInstDec] ++ beamDecs

-- | Generate beam table, database, settings, and SaveEvent for one event.
makeBeamEventTable :: String -> Name -> [Type] -> [Name] -> Q [Dec]
makeBeamEventTable tableName eventName argTypes argNames = do
  let tableTypeName  = mkName (nameBase eventName ++ "EventT")
      rowConName     = mkName (nameBase eventName ++ "EventRow")
      dbTypeName     = mkName (nameBase eventName ++ "EventDb")
      dbConName      = dbTypeName
      dbSettingsName = mkName (lcFirst (nameBase eventName) ++ "EventDb")
      tableFieldName = mkName ("_" ++ lcFirst (nameBase eventName) ++ "EventTable")
      tableEntityName = mkName (lcFirst (nameBase eventName) ++ "EventTable")
      pkConName      = mkName (nameBase eventName ++ "EventPK")
      fieldPrefix    = "_" ++ lcFirst (nameBase eventName) ++ "E"

  fName <- newName "f"
  _connName <- newName "conn" -- unused, kept for future event table expansion

  let usedFieldNames = [ mkName (fieldPrefix ++ "Arg" ++ show i) | i <- [0 :: Int .. length argTypes - 1] ]
      colNames       = [ "arg" ++ show i | i <- [0 :: Int .. length argTypes - 1] ]

  -- 1. Table data type: data FooEventT f = FooEventRow { ... } deriving (Generic, Beamable)
  let recFields = [ (fn, noBang, AppT (AppT (ConT ''C) (VarT fName)) t)
                  | (fn, t) <- zip usedFieldNames argTypes ]
      tableDataDec = DataD [] tableTypeName [PlainTV fName BndrReq] Nothing
        [RecC rowConName recFields]
        [ DerivClause (Just StockStrategy) [ConT ''Generic]
        , DerivClause (Just AnyclassStrategy) [ConT ''Beamable]
        ]

  -- 2. Table instance with dummy PK (real PK is BIGSERIAL in schema.sql)
  let tableInstDec = InstanceD Nothing []
        (AppT (ConT ''Table) (ConT tableTypeName))
        [ DataInstD [] Nothing
            (AppT (AppT (ConT ''PrimaryKey) (ConT tableTypeName)) (VarT fName))
            Nothing
            [NormalC pkConName []]
            [ DerivClause (Just StockStrategy) [ConT ''Generic]
            , DerivClause (Just AnyclassStrategy) [ConT ''Beamable]
            ]
        , FunD 'primaryKey [Clause [WildP] (NormalB (ConE pkConName)) []]
        ]

  -- 3. Database type: data FooEventDb f = FooEventDb { _fooEventTable :: f (TableEntity FooEventT) }
  let dbField = (tableFieldName, noBang,
                 AppT (VarT fName) (AppT (ConT ''TableEntity) (ConT tableTypeName)))
      dbDataDec = DataD [] dbTypeName [PlainTV fName BndrReq] Nothing
        [RecC dbConName [dbField]]
        [ DerivClause (Just StockStrategy) [ConT ''Generic]
        , DerivClause (Just AnyclassStrategy) [AppT (ConT ''Database) (ConT ''Postgres)]
        ]

  -- 4. DatabaseSettings binding
  let entityModExpr = case zip usedFieldNames colNames of
        [] -> AppE (VarE 'setEntityName) (LitE (StringL tableName))
        fieldCols ->
          let tmod = RecUpdE (VarE 'tableModification)
                       [ (fn, LitE (StringL col)) | (fn, col) <- fieldCols ]
          in InfixE
               (Just (AppE (VarE 'setEntityName) (LitE (StringL tableName))))
               (VarE '(<>))
               (Just (AppE (VarE 'modifyTableFields) tmod))
      dbSettingsType = AppT (AppT (ConT ''DatabaseSettings) (ConT ''Postgres)) (ConT dbTypeName)
      dbSettingsBody = AppE
        (AppE (VarE 'withDbModification)
              (SigE (VarE 'defaultDbSettings) dbSettingsType))
        (AppE (ConE dbConName) entityModExpr)
      dbSettingsSig = SigD dbSettingsName dbSettingsType
      dbSettingsDec = ValD (VarP dbSettingsName) (NormalB dbSettingsBody) []

  -- 5. Table entity accessor
  let tableEntityType = AppT (AppT (AppT (ConT ''DatabaseEntity) (ConT ''Postgres))
                                   (ConT dbTypeName))
                             (AppT (ConT ''TableEntity) (ConT tableTypeName))
      tableEntitySig = SigD tableEntityName tableEntityType
      tableEntityDec = ValD (VarP tableEntityName)
        (NormalB (AppE (VarE tableFieldName) (VarE dbSettingsName))) []

  -- 6. SaveEvent instance using beamTx (transaction monad)
  let rowExpr = foldl AppE (ConE rowConName) (map VarE argNames)
      insertExpr = AppE
        (AppE (VarE 'insert) (VarE tableEntityName))
        (AppE (VarE 'insertValues) (ListE [rowExpr]))
      -- beamTx (runInsert insertExpr) :: PgTx ()
      -- but runInsert returns Int64, so: beamTx (void (runInsert insertExpr))
      saveBody = AppE (VarE 'beamTx)
        (AppE (AppE (VarE 'fmap) (LamE [WildP] (ConE '())))
              (AppE (VarE 'runInsert) insertExpr))
      saveInstDec = InstanceD Nothing []
        (AppT (ConT ''SaveEvent) (ConT eventName))
        [ FunD 'saveEvent
            [Clause [ConP eventName [] (map VarP argNames)]
                    (NormalB saveBody) []]
        ]

  return [ tableDataDec, tableInstDec
         , dbDataDec
         , dbSettingsSig, dbSettingsDec
         , tableEntitySig, tableEntityDec
         , saveInstDec
         ]

-- | Check if a type has a HasSqlValueSyntax PgValueSyntax instance.
-- Used at TH time to decide if an event arg can be a beam column.
hasBeamInstance :: Type -> Q Bool
hasBeamInstance ty = do
  let constraint = AppT (AppT (ConT ''HasSqlValueSyntax) (ConT ''PgValueSyntax)) ty
  instances <- reifyInstances ''HasSqlValueSyntax [ConT ''PgValueSyntax, ty]
  return (not (null instances))

-- * Helpers

noBang :: Bang
noBang = Bang NoSourceUnpackedness NoSourceStrictness

lcFirst :: String -> String
lcFirst [] = []
lcFirst (c:cs) = toLower c : cs
  where toLower x | x >= 'A' && x <= 'Z' = toEnum (fromEnum x + 32)
                  | otherwise = x

toEventName :: Name -> Name
toEventName n = mkName $ case nameBase n of
  (c:cs) -> toUpper c : cs
  []     -> []
  where toUpper c | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
                  | otherwise = c

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x:xs) = toLower x : go xs
  where
    go [] = []
    go (c:cs) | isUpper c = '_' : toLower c : go cs
              | otherwise = c : go cs
    isUpper c = c >= 'A' && c <= 'Z'
    toLower c | isUpper c = toEnum (fromEnum c + 32)
              | otherwise = c
