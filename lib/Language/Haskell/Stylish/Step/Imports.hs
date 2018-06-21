{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports
    ( Options (..)
    , defaultOptions
    , ImportAlign (..)
    , ListAlign (..)
    , LongListAlign (..)
    , EmptyListAlign (..)
    , ListPadding (..)
    , step
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Control.Monad                   (void)
import           Data.Bool
import           Data.Char                       (toLower)
import           Data.List                       (intercalate, sortBy)
import           Data.Maybe                      (isJust, maybeToList)
import           Data.Ord                        (comparing)
import qualified Language.Haskell.Exts           as H
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Types                as A


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util

--------------------------------------------------------------------------------
data Options = Options
    { importAlign    :: ImportAlign
    , listAlign      :: ListAlign
    , longListAlign  :: LongListAlign
    , emptyListAlign :: EmptyListAlign
    , listPadding    :: ListPadding
    , separateLists  :: Bool
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { importAlign    = Global
    , listAlign      = AfterAlias
    , longListAlign  = Inline
    , emptyListAlign = Inherit
    , listPadding    = LPConstant 4
    , separateLists  = True
    }

data ListPadding
    = LPConstant Int
    | LPModuleName
    deriving (Eq, Show)

data ImportAlign
    = Global
    | File
    | Group
    | Semicolons
    | SemicolonsNone
    | None
    deriving (Eq, Show)

data ListAlign
    = NewLine
    | WithAlias
    | AfterAlias
    deriving (Eq, Show)

data EmptyListAlign
    = Inherit
    | RightAfter
    deriving (Eq, Show)

data LongListAlign
    = Inline
    | InlineWithBreak
    | InlineToMultiline
    | Multiline
    deriving (Eq, Show)

data PaddingStyle
    = PadQualified
    | PadImport
    | PadNothing
    deriving (Eq, Show)

--------------------------------------------------------------------------------
imports :: H.Module l -> [H.ImportDecl l]
imports (H.Module _ _ _ is _) = is
imports _                     = []


--------------------------------------------------------------------------------
importName :: H.ImportDecl l -> String
importName i = let (H.ModuleName _ n) = H.importModule i in n


--------------------------------------------------------------------------------
longestImport :: [H.ImportDecl l] -> Int
longestImport = maximum . map (length . importName)

longestImportAndQualification :: [H.ImportDecl l] -> Int
longestImportAndQualification = maximum . map impLength
 where
    impLength imp = length (importName imp)
                    + if H.importQualified imp then 17 else 7

--------------------------------------------------------------------------------
-- | Compare imports for ordering
compareImports :: Bool -> H.ImportDecl l -> H.ImportDecl l -> Ordering
compareImports qualifiedFirst
    = comparing ( map toLower . importName
                  &&& bool id not qualifiedFirst . H.importQualified)


--------------------------------------------------------------------------------
-- | The implementation is a bit hacky to get proper sorting for input specs:
-- constructors first, followed by functions, and then operators.
compareImportSpecs :: H.ImportSpec l -> H.ImportSpec l -> Ordering
compareImportSpecs = comparing key
  where
    key :: H.ImportSpec l -> (Int, Bool, String)
    key (H.IVar _ x)         = (1, isOperator x, nameToString x)
    key (H.IAbs _ _ x)       = (0, False, nameToString x)
    key (H.IThingAll _ x)    = (0, False, nameToString x)
    key (H.IThingWith _ x _) = (0, False, nameToString x)


--------------------------------------------------------------------------------
-- | Sort the input spec list inside an 'H.ImportDecl'
sortImportSpecs :: H.ImportDecl l -> H.ImportDecl l
sortImportSpecs imp = imp {H.importSpecs = sort' <$> H.importSpecs imp}
  where
    sort' (H.ImportSpecList l h specs) = H.ImportSpecList l h $
        sortBy compareImportSpecs specs


--------------------------------------------------------------------------------
-- | Order of imports in sublist is:
-- Constructors, accessors/methods, operators.
compareImportSubSpecs :: H.CName l -> H.CName l -> Ordering
compareImportSubSpecs = comparing key
  where
    key :: H.CName l -> (Int, Bool, String)
    key (H.ConName _ x) = (0, False,        nameToString x)
    key (H.VarName _ x) = (1, isOperator x, nameToString x)


--------------------------------------------------------------------------------
-- | By default, haskell-src-exts pretty-prints
--
-- > import Foo (Bar(..))
--
-- but we want
--
-- > import Foo (Bar (..))
--
-- instead.
prettyImportSpec :: (Ord l) => Bool -> H.ImportSpec l -> String
prettyImportSpec separate = prettyImportSpec'
  where
    prettyImportSpec' (H.IThingAll  _ n)     = H.prettyPrint n ++ sep "(..)"
    prettyImportSpec' (H.IThingWith _ n cns) = H.prettyPrint n
        ++ sep "("
        ++ intercalate ", "
          (map H.prettyPrint $ sortBy compareImportSubSpecs cns)
        ++ ")"
    prettyImportSpec' x                      = H.prettyPrint x

    sep = if separate then (' ' :) else id


--------------------------------------------------------------------------------
prettyImport :: (Ord l, Show l) =>
    Int -> Options -> PaddingStyle -> Bool -> Int -> H.ImportDecl l -> [String]
prettyImport columns Options{..} padQualified padName longest imp
    | (void `fmap` H.importSpecs imp) == emptyImportSpec = emptyWrap
    | otherwise = case longListAlign of
        Inline            -> inlineWrap
        InlineWithBreak   -> longListWrapper inlineWrap inlineWithBreakWrap
        InlineToMultiline -> longListWrapper inlineWrap inlineToMultilineWrap
        Multiline         -> longListWrapper inlineWrap multilineWrap
  where
    emptyImportSpec = Just (H.ImportSpecList () False [])
    -- "import" + space + qualifiedLength has space in it.
    listPadding' = listPaddingValue (6 + 1 + qualifiedLength) listPadding
      where
        qualifiedLength =
            if null qualified then 0 else 1 + sum (map length qualified)

    longListWrapper shortWrap longWrap
        | listAlign == NewLine
        || length shortWrap > 1
        || length (head shortWrap) > columns
            = longWrap
        | otherwise = shortWrap

    emptyWrap = case emptyListAlign of
        Inherit -> inlineWrap
        RightAfter -> [paddedNoSpecBase ++ " ()"]

    inlineWrap = inlineWrapper
        $ mapSpecs
        $ withInit (++ ",")
        . withHead ("(" ++)
        . withLast (++ ")")

    inlineWrapper = case listAlign of
        NewLine    -> (paddedNoSpecBase :) . wrapRest columns listPadding'
        WithAlias  -> wrap columns paddedBase (inlineBaseLength + 1)
        -- Add 1 extra space to ensure same padding as in original code.
        AfterAlias -> withTail (' ' :)
            . wrap columns paddedBase (afterAliasBaseLength + 1)

    inlineWithBreakWrap = paddedNoSpecBase : wrapRest columns listPadding'
        ( mapSpecs
        $ withInit (++ ",")
        . withHead ("(" ++)
        . withLast (++ ")"))

    inlineToMultilineWrap
        | length inlineWithBreakWrap > 2
        || any ((> columns) . length) (tail inlineWithBreakWrap)
            = multilineWrap
        | otherwise = inlineWithBreakWrap

    -- 'wrapRest 0' ensures that every item of spec list is on new line.
    multilineWrap = paddedNoSpecBase : wrapRest 0 listPadding'
        ( mapSpecs
          ( withHead ("( " ++)
          . withTail (", " ++))
        ++ [")"])

    paddedBase = base padImport $ importName imp

    paddedNoSpecBase = base padImportNoSpec $ importName imp

    padImport = if hasExtras && padName
        then padRight longest
        else id

    padImportNoSpec = if (isJust (H.importAs imp) || hasHiding) && padName
        then padRight longest
        else id

    unwordsNonEmpties = unwords . concat . filter (not . null)

    base' pad baseName importAs hasHiding' = unwordsNonEmpties
        [ if importAlign `elem` [Semicolons, SemicolonsNone]
            then [ pad $ unwordsNonEmpties
                    [ _import
                    , source
                    , safe
                    , qualified
                    , show <$> maybeToList (H.importPkg imp)
                    , [baseName] ] ]
            else [ unwordsNonEmpties
                    [ _import
                    , source
                    , safe
                    , qualified
                    , show <$> maybeToList (H.importPkg imp)
                    , [pad baseName] ] ]
        , importAs
        , hasHiding'
        ]

    base pad baseName = base' pad baseName
        ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp]
        ["hiding" | hasHiding]

    inlineBaseLength = length $ base' padImport (importName imp) [] []

    afterAliasBaseLength = length $ base' padImport (importName imp)
        ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp] []

    (hasHiding, importSpecs) = case H.importSpecs imp of
        Just (H.ImportSpecList _ h l) -> (h, Just l)
        _                             -> (False, Nothing)

    hasExtras = isJust (H.importAs imp) || isJust (H.importSpecs imp)

    _import
        | H.importQualified imp     = ["import"]
        | padQualified /= PadImport = ["import"]
        | H.importSrc imp           = ["import"]
        | H.importSafe imp          = ["    ;import"]
        | otherwise                 = ["         ;import"]

    qualified
        | H.importQualified imp        = ["qualified"]
        | padQualified /= PadQualified = []
        | H.importSrc imp              = []
        | H.importSafe imp             = ["    "]
        | otherwise                    = ["         "]

    safe
        | H.importSafe imp = ["safe"]
        | otherwise        = []

    source
        | H.importSrc imp = ["{-# SOURCE #-}"]
        | otherwise       = []

    mapSpecs f = case importSpecs of
        Nothing -> []     -- Import everything
        Just [] -> ["()"] -- Instance only imports
        Just is -> f $ map (prettyImportSpec separateLists) is


--------------------------------------------------------------------------------
prettyImportGroup :: Int -> Options -> Bool -> Int
                  -> [H.ImportDecl LineBlock]
                  -> Lines
prettyImportGroup columns align fileAlign longest imps =
    foldr (\imp f qmod -> prettyImport columns align (padQual qmod imp) padName longest' imp
                          ++ f (if H.importQualified imp then importName imp
                                                         else qmod))
          (const [])
          (sortBy (compareImports (align' `elem` [Semicolons,SemicolonsNone])) imps)
          "" -- Last seen qualified import (qmod).
  where
    align' = importAlign align

    longest' = case align' of
        Semicolons -> longestImportAndQualification imps
        Group      -> longestImport imps
        _          -> longest

    padName = align' `notElem` [None, SemicolonsNone]

    padQual qmod imp = case align' of
        Global         -> PadQualified
        File           -> if fileAlign then PadQualified else PadNothing
        Group          -> if any H.importQualified imps then PadQualified else PadNothing
        Semicolons     -> if qmod == importName imp then PadImport else PadNothing
        SemicolonsNone -> if qmod == importName imp then PadImport else PadNothing
        None           -> PadNothing


--------------------------------------------------------------------------------
step :: Int -> Options -> Step
step columns = makeStep "Imports" . step' columns


--------------------------------------------------------------------------------
step' :: Int -> Options -> Lines -> Module -> Lines
step' columns align ls (module', _) = applyChanges
    [ change block $ const $
        prettyImportGroup columns align fileAlign longest importGroup
    | (block, importGroup) <- groups
    ]
    ls
  where
    imps    = map sortImportSpecs $ imports $ fmap linesFromSrcSpan module'
    longest = longestImport imps
    groups  = groupAdjacent [(H.ann i, i) | i <- imps]

    fileAlign = case importAlign align of
        File -> any H.importQualified imps
        _    -> False

--------------------------------------------------------------------------------
listPaddingValue :: Int -> ListPadding -> Int
listPaddingValue _ (LPConstant n) = n
listPaddingValue n LPModuleName   = n

--------------------------------------------------------------------------------

instance A.FromJSON ListPadding where
    parseJSON (A.String "module_name") = return LPModuleName
    parseJSON (A.Number n) | n' >= 1   = return $ LPConstant n'
      where
        n' = truncate n
    parseJSON v                        = A.typeMismatch "'module_name' or >=1 number" v
