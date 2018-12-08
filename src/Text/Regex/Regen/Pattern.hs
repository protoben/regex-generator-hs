{-# LANGUAGE RecordWildCards #-}
module Text.Regex.Regen.Pattern where

import Control.Monad.Catch (MonadThrow)
import Data.Map (Map)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as Map

import Text.Regex.Regen.PatternException

data Pattern = Pattern
    { pGroups     :: !(Map Int Group)
    , pGroupNames :: !(Map S.ByteString Int)
    , pGroupCount :: !Int
    , pOptions    :: !Options
    } deriving (Show, Eq)

emptyPattern :: Options -> Pattern
emptyPattern = Pattern Map.empty Map.empty 0

lookupGroupByNum :: Int -> Pattern -> Maybe Group
lookupGroupByNum n Pattern {..} = Map.lookup n pGroups

lookupGroupByName :: S.ByteString -> Pattern -> Maybe Group
lookupGroupByName bs Pattern {..} = do
    n <- Map.lookup bs pGroupNames
    Map.lookup n pGroups

data Part
    = Empty
    | Byte Char
    | Sequence [Part]
    | CClass Bool [Char]
    | Quantified Part Range Strategy
    | Alternative [Part]
    | Reference Int
    | Call (Either Int S.ByteString)
    | Anchored Part [Anchor] Part
    deriving (Show, Eq)
instance Monoid Part where
    mempty = Empty
    Empty        `mappend` p            = p
    p            `mappend` Empty        = p
    Sequence ps1 `mappend` Sequence ps2 = nonEmptySeq $ ps1 <> ps2
    Sequence ps  `mappend` p            = nonEmptySeq $ reverse (p : reverse ps)
    p            `mappend` Sequence ps  = nonEmptySeq $ p : ps
    p1           `mappend` p2           = Sequence [p1,p2]

data Anchor
    = WordBoundary
    | WordInternal
    | Start
    | End
    | EndBeforeNewline
    | BeforeNewline
    | AfterNewline
    | StartOfMatch
    deriving (Show, Eq)

data Group = Group
    { gIndex :: !Int
    , gParts :: !Part
    } deriving (Show, Eq)

data Range = Range
    { rMin :: !Int
    , rMax :: !(Maybe Int)
    } deriving (Show, Eq)

oLineEndChars :: Options -> [Char]
oLineEndChars o = case oLineEnds o of
    CR      -> ['\r']
    LF      -> ['\n']
    CRLF    -> []
    AnyCRLF -> ['\r','\n']
    Any     -> ['\r','\n']

nonEmptyAltM :: MonadThrow m => [Part] -> m Part
nonEmptyAltM [] = eUnsatisfiable
nonEmptyAltM ps = pure $ Alternative ps

nonEmptyCClassM :: MonadThrow m => (Char -> Bool) -> [Char] -> m Part
nonEmptyCClassM p cs = case filter p cs of
    []  -> eUnsatisfiable
    cs' -> pure $ CClass True cs'

nonEmptyAlt :: [Part] -> Part
nonEmptyAlt [] = Empty
nonEmptyAlt ps = Alternative ps

nonEmptySeq :: [Part] -> Part
nonEmptySeq [] = Empty
nonEmptySeq ps = Sequence ps

data Options = Options
    { oDefaultStrategy :: !Strategy
    , oMultiline       :: !Bool
    , oIgnoreCase      :: !Bool
    , oLineEnds        :: !LineEnds
    , oRecursion       :: !Bool
    , oDotAll          :: !Bool
    , oFreeSpacing     :: !Bool
    , oDupNames        :: !Bool
    } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options
    { oDefaultStrategy = Greedy
    , oMultiline       = False
    , oIgnoreCase      = False
    , oLineEnds        = AnyCRLF
    , oRecursion       = False
    , oDotAll          = False
    , oFreeSpacing     = False
    , oDupNames        = False
    }

data LineEnds
    = CR      -- \r
    | LF      -- \n
    | CRLF    -- \r\n
    | AnyCRLF -- (\r|\n|\r\n)
    | Any     -- Any unicode newline sequence
    deriving (Show, Eq)

data Strategy
    = Greedy
    | Lazy
    | Possessive
    deriving (Show, Eq)
