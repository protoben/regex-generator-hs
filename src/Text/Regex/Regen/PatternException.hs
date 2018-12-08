{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.Regex.Regen.PatternException where

import Control.Monad.Catch (MonadThrow(..))
import Data.Typeable (Typeable(..))
import qualified Control.Exception as X
import qualified Data.ByteString.Char8 as S

data PatternException
    = EParserError  String
    | EGenError     String
    | ENotSupported String
    | ENoSuchClass  String
    | ENoSuchVerb   String
    | EBadGroupName S.ByteString
    | EDupGroupName S.ByteString
    | ENoSuchIndex  (Either Int S.ByteString)
    | EClassOutOfOrder
    | EQuantOutOfOrder
    deriving (Eq, Typeable)
instance Show PatternException where
    show (EParserError  s) = "parser error: " ++ s
    show (EGenError     s) = "generator error: " ++ s
    show (ENotSupported s) = s ++ " not supported"
    show (ENoSuchClass  s) = "unknown POSIX class name: " ++ show s
    show (ENoSuchVerb   s) = "verb not recognized or malformed: " ++ s
    show (EBadGroupName s) = "bad sub-pattern name: " ++ show s
    show (EDupGroupName s) = "duplicate sub-pattern name: " ++ show s
    show (ENoSuchIndex  e) = "non-existent sub-pattern: " ++ either show show e
    show  EClassOutOfOrder = "out of order range in character class"
    show  EQuantOutOfOrder = "numbers out of order in quantifier"
instance X.Exception PatternException

eParserError, eGenError, eNotSupported, eNoSuchClass, eNoSuchVerb :: MonadThrow m => String -> m a
eParserError  = throwM . EParserError
eGenError     = throwM . EGenError
eNotSupported = throwM . ENotSupported
eNoSuchClass  = throwM . ENoSuchClass
eNoSuchVerb   = throwM . ENoSuchVerb

eBadGroupName, eDupGroupName :: MonadThrow m => S.ByteString -> m a
eBadGroupName = throwM . EBadGroupName
eDupGroupName = throwM . EDupGroupName

eNoSuchIndex :: MonadThrow m => Either Int S.ByteString -> m a
eNoSuchIndex = throwM . ENoSuchIndex

eClassOutOfOrder, eQuantOutOfOrder, eUnsatisfiable :: MonadThrow m => m a
eClassOutOfOrder = throwM EClassOutOfOrder
eQuantOutOfOrder = throwM EQuantOutOfOrder
eUnsatisfiable   = eGenError "pattern unsatisfiable"

isPatternException :: X.Exception e => e -> Bool
isPatternException e = case X.fromException (X.toException e) of
    Just (pe::PatternException) -> True
    Nothing                     -> False

isEParserError :: PatternException -> Bool
isEParserError (EParserError _) = True
isEParserError _                = False

isEGenError :: PatternException -> Bool
isEGenError (EGenError _) = True
isEGenError _             = False

isENotSupported :: PatternException -> Bool
isENotSupported (ENotSupported _) = True
isENotSupported _                 = False

isENoSuchClass :: PatternException -> Bool
isENoSuchClass (ENoSuchClass _) = True
isENoSuchClass _                = False

isEBadGroupName :: PatternException -> Bool
isEBadGroupName (EBadGroupName _) = True
isEBadGroupName _                 = False

isEDupGroupName :: PatternException -> Bool
isEDupGroupName (EDupGroupName _) = True
isEDupGroupName _                 = False

isENoSuchVerb :: PatternException -> Bool
isENoSuchVerb (ENoSuchVerb _) = True
isENoSuchVerb _               = False

isENoSuchIndex :: PatternException -> Bool
isENoSuchIndex (ENoSuchIndex _) = True
isENoSuchIndex _                = False

isEClassOutOfOrder :: PatternException -> Bool
isEClassOutOfOrder EClassOutOfOrder = True
isEClassOutOfOrder _                = False

isEQuantOutOfOrder :: PatternException -> Bool
isEQuantOutOfOrder EQuantOutOfOrder = True
isEQuantOutOfOrder _                = False

