{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Regex.Regen.Gen where

import Control.Applicative (Alternative(..))
import Control.Exception (SomeException, mapException)
import Control.Monad (MonadPlus(..), guard, (<=<), when, void)
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.Catch (try, fromException, toException)
import Data.List ((\\), foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe, maybe, catMaybes)
import Data.Monoid ((<>))
import System.Random (Random, StdGen, random, randomR, newStdGen)
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as Map

import Text.Regex.Regen.Parse
import Text.Regex.Regen.Pattern
import Text.Regex.Regen.PatternException
import Text.Regex.Regen.Util

generate :: S.ByteString -> IO S.ByteString
generate = generateOpts defaultOptions

generateOpts :: Options -> S.ByteString -> IO S.ByteString
generateOpts o bs = parsePatternOpts o bs >>= generatePatternIO

generatePatternIO :: Pattern -> IO S.ByteString
generatePatternIO p = do
    (e,_) <- generatePattern p <$> newStdGen
    either throwM pure e

generatePattern :: Pattern -> StdGen -> (Either SomeException S.ByteString, StdGen)
generatePattern p g = let (s,e) = runGen genTop (makeGenState p g) in (e, gsGen s)
    where
    state  = makeGenState p g
    genTop = getGroup 0 >>= genGroup

data GenState = GenState
    { gsGen        :: !StdGen
    , gsOptions    :: !Options
    , gsGroups     :: !(Map Int Group)
    , gsGroupNames :: !(Map S.ByteString Int)
    , gsResolved   :: !(Map Int S.ByteString)
    } deriving Show

makeGenState :: Pattern -> StdGen -> GenState
makeGenState Pattern {..} g = GenState g pOptions pGroups pGroupNames Map.empty

newGenState :: Pattern -> IO GenState
newGenState p = makeGenState p <$> newStdGen

newtype Gen a = Gen { runGen :: GenState -> (GenState, Either SomeException a) }
instance Functor Gen where
    fmap f m = Gen $ \s -> let (s',e) = runGen m s in (s', fmap f e)
instance Applicative Gen where
    pure a = Gen $ \s -> (s, Right a)
    m <*> n = Gen $ \s ->
        let (s',f)  = runGen m s
            (s'',a) = runGen n s'
         in (s'', f <*> a)
instance Alternative Gen where
    empty = Gen $ \s -> (s, mapException fromPatEx eUnsatisfiable)
        where
        fromPatEx :: PatternException -> SomeException
        fromPatEx = toException
    m <|> n = Gen $ \s -> case runGen m s of
        r@(_, Right _) -> r
        (_, Left _)    -> runGen n s
instance Monad Gen where
    m >>= f = Gen $ \s -> case runGen m s of
        (s', Left e)  -> (s', Left e)
        (s', Right a) -> runGen (f a) s'
    fail = throwM . EGenError
instance Monoid m => Monoid (Gen m) where
    mempty = pure mempty
    mappend g1 g2 = g1 >>= \m1 -> g2 >>= \m2 -> pure $ m1 <> m2
instance MonadThrow Gen where
    throwM e = Gen $ \s -> (s, Left $ toException e)
instance MonadCatch Gen where
    catch m f = Gen $ \s -> case runGen m s of
        (s', Right a) -> (s', Right a)
        (s,  Left e)  -> case fromException e of
            Nothing -> (s, Left e)
            Just e' -> runGen (f e') s

evalGen :: Gen a -> GenState -> Either SomeException a
evalGen = snd .: runGen

getGenState :: Gen GenState
getGenState = Gen $ \s -> (s, Right s)

putGenState :: GenState -> Gen ()
putGenState s = Gen $ const (s, Right ())

modifyGenState' :: (GenState -> GenState) -> Gen ()
modifyGenState' f = Gen $ \s -> (f s, Right ())

modifyGenState :: (GenState -> (GenState, a)) -> Gen a
modifyGenState f = Gen $ \s -> let (s',a) = f s in (s', Right a)

getOption :: (Options -> a) -> Gen a
getOption f = f . gsOptions <$> getGenState

getNamedGroup' :: S.ByteString -> Gen (Maybe Group)
getNamedGroup' bs = lookupNamedGroup <$> getGenState
    where
    lookupNamedGroup :: GenState -> Maybe Group
    lookupNamedGroup GenState {..} =
        flip Map.lookup gsGroups =<< Map.lookup bs gsGroupNames

getNamedGroup :: S.ByteString -> Gen Group
getNamedGroup bs = getNamedGroup' bs >>=
    maybe (fail $ "nonexistent group name: " ++ show bs) pure

getGroup' :: Int -> Gen (Maybe Group)
getGroup' ix = Map.lookup ix . gsGroups <$> getGenState

getGroup :: Int -> Gen Group
getGroup ix = getGroup' ix >>=
    maybe (fail $ "nonexistent group index: " ++ show ix) pure

getResolved' :: Int -> Gen (Maybe S.ByteString)
getResolved' ix = Map.lookup ix . gsResolved <$> getGenState

getResolved :: Int -> Gen S.ByteString
getResolved ix = getResolved' ix >>=
    maybe (fail $ "group index not yet resolved: " ++ show ix) pure

resolveRef :: Int -> S.ByteString -> Gen S.ByteString
resolveRef ix bs = modifyGenState f
    where
    f s = (s { gsResolved = Map.insert ix bs (gsResolved s) }, bs)

getStdGen :: Gen StdGen
getStdGen = gsGen <$> getGenState

putStdGen :: StdGen -> Gen ()
putStdGen g = modifyGenState' (\s -> s { gsGen = g })

randomGen :: Random a => Gen a
randomGen = do
    g <- getStdGen
    let (a,g') = random g
    putStdGen g'
    pure a

randomRGen :: Random a => (a,a) -> Gen a
randomRGen r = do
    g <- getStdGen
    let (a,g') = randomR r g
    putStdGen g'
    pure a

randomElemGen :: [a] -> Gen a
randomElemGen l = do
    g <- getStdGen
    guard $ not (null l)
    let (n,g') = randomR (0, length l - 1) g
    putStdGen g'
    pure $ n `th` l
    where
    n `th` l = head $ drop n l

genGroup :: Group -> Gen S.ByteString
genGroup Group {..} = genPart gParts >>= resolveRef gIndex

genPart :: Part -> Gen S.ByteString
genPart Empty                = pure S.empty
genPart (Byte c)             = pure $ S.singleton c
genPart (Sequence    ps)     = genSequence ps
genPart (CClass      b cs)   = genCClass b cs
genPart (Quantified  p r t)  = genQuantified p r t
genPart (Alternative ps)     = genAlternative ps
genPart (Reference   ix)     = genReference ix
genPart (Call        _)      = eNotSupported "pattern calls"
genPart (Anchored    p as q) = genAnchored p as q

genSequence :: [Part] -> Gen S.ByteString
genSequence ps = mconcat $ genPart <$> ps

genCClass :: Bool -> [Char] -> Gen S.ByteString
genCClass b pos = S.singleton <$> randomElemGen cs
    where
    cs = if b then pos else ['\x00'..'\xff'] \\ pos

genQuantified :: Part -> Range -> Strategy -> Gen S.ByteString
genQuantified p (Range n m) _ = do
    -- TODO: Make default spread configurable? Maybe normally distributed?
    r <- randomRGen (n, fromMaybe (n + 10) m)
    genSequence $ replicate r p

genAlternative :: [Part] -> Gen S.ByteString
genAlternative = genPart <=< randomElemGen

genReference :: Int -> Gen S.ByteString
genReference ix = getResolved ix <|> do
    g <- getGroup ix
    genGroup g

genAnchored :: Part -> [Anchor] -> Part -> Gen S.ByteString
genAnchored p1 as p2 = do
    as' <- mapM (tryMaybe . resolve) as
    alt <- nonEmptyAltM $ catMaybes as'
    genPart alt
    where
    resolve StartOfMatch     = eGenError "start-of-match anchors unsupported"
    resolve WordBoundary     = eGenError "word-boundary anchors unsupported"
    resolve WordInternal     = eGenError "word-internal anchors unsupported"
    resolve Start            =                      nullify    p1 <> pure         p2
    resolve End              =                      pure       p1 <> nullify      p2
    resolve BeforeNewline    = getLEPred >>= \le -> pure       p1 <> startWith le p2
    resolve AfterNewline     = getLEPred >>= \le -> endWith le p1 <> pure         p2
    resolve EndBeforeNewline = getLEPred >>= \le -> pure       p1 <> comprise  le p2
    getLEPred = getOption oLineEndChars >>= \cs -> pure (`elem` cs)

nullify :: Part -> Gen Part
nullify part = case part of
    Empty -> mempty
    Byte _ -> eUnsatisfiable
    Sequence ps -> if null ps then mempty else eUnsatisfiable
    CClass _ _ -> eUnsatisfiable
    Quantified _ r _ -> if rMin r == 0 then mempty else eUnsatisfiable
    Alternative ps -> foldr (<|>) empty $ nullify <$> ps
    Reference ix -> do
        m <- getResolved' ix
        case m of
            Just bs -> if S.null bs then pure () else eUnsatisfiable
            Nothing -> do
                g <- getGroup ix
                p <- nullify $ gParts g
                void . genGroup $ Group ix p
        pure $ Reference ix
    Call _ -> eNotSupported "pattern calls"
    Anchored p1 as p2 -> Anchored <$> nullify p1 <*> pure as <*> nullify p2

comprise :: (Char -> Bool) -> Part -> Gen Part
comprise p part = case part of
    Empty -> mempty
    Byte c -> if p c then pure part else eUnsatisfiable
    Sequence ps -> Sequence <$> mapM (comprise p) ps
    CClass b cs -> nonEmptyCClassM p $ if b then cs else ['\x00'..'\xff'] \\ cs
    Quantified q r t -> Quantified <$> comprise p q <*> pure r <*> pure t
    Alternative ps -> nonEmptyAlt . catMaybes <$> mapM (tryMaybe . comprise p) ps
    Reference ix -> do
        m <- getResolved' ix
        case m of
            Just bs -> if S.all p bs then pure () else eUnsatisfiable
            Nothing -> do
                g <- getGroup ix
                p <- comprise p $ gParts g
                void . genGroup $ Group ix p
        pure $ Reference ix
    Call _ -> eNotSupported "pattern calls"
    Anchored p1 as p2 -> Anchored <$> comprise p p1 <*> pure as <*> comprise p p2

endWith :: (Char -> Bool) -> Part -> Gen Part
endWith p part = case part of
    Empty -> mempty
    Byte c -> if p c then pure part else eUnsatisfiable
    Sequence ps -> case reverse ps of
        []         -> eUnsatisfiable
        part : ps' -> nonEmptySeq . reverse .: (:) <$> endWith p part <*> pure ps'
    CClass b cs -> nonEmptyCClassM p $ if b then cs else ['\x00'..'\xff'] \\ cs
    Quantified q r t -> Quantified <$> endWith p q <*> pure r <*> pure t
    Alternative ps -> Alternative . catMaybes <$> mapM (tryMaybe . endWith p) ps
    Reference ix -> do
        m <- getResolved' ix
        case m of
            Just bs -> if S.null bs || p (S.last bs) then pure () else eUnsatisfiable
            Nothing -> do
                g <- getGroup ix
                p <- endWith p $ gParts g
                void . genGroup $ Group ix p
        pure $ Reference ix
    Call ix -> eNotSupported "pattern calls"
    Anchored p1 as p2 -> eGenError "<><><>"

startWith :: (Char -> Bool) -> Part -> Gen Part
startWith p part = case part of
    Empty -> mempty
    Byte c -> if p c then pure part else eUnsatisfiable
    Sequence ps -> case ps of
        []         -> eUnsatisfiable
        part : ps' -> nonEmptySeq .: (:) <$> startWith p part <*> pure ps'
    CClass b cs -> nonEmptyCClassM p $ if b then cs else ['\x00'..'\xff'] \\ cs
    Quantified q r t -> Quantified <$> startWith p q <*> pure r <*> pure t
    Alternative ps -> Alternative . catMaybes <$> mapM (tryMaybe . startWith p) ps
    Reference ix -> do
        m <- getResolved' ix
        case m of
            Just bs -> if S.null bs || p (S.head bs) then pure () else eUnsatisfiable
            Nothing -> do
                g <- getGroup ix
                p <- startWith p $ gParts g
                void . genGroup $ Group ix p
        pure $ Reference ix
    Call ix -> eNotSupported "pattern calls"
    Anchored p1 as p2 -> eGenError "<><><>"
