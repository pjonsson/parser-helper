{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy            as B
import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (listToMaybe)
import           Data.Text                       (pack)
import           Language.Haskell.Exts.Annotated
import           System.Environment
import           System.Exit                     (exitFailure)
import           Text.Read                       (readMaybe)

-- | Program version. Important for API changes.
version :: String
version = "0.1.0.1"

$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SrcSpanInfo)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SrcSpan)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Module)
-- We need to manually derive this instance since one of its constructors, Con, clashes with Windows.
-- $(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Exp)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Promoted)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ImportDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ImportSpec)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ExportSpec)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''WarningText)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ModulePragma)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Tool)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CName)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''QName)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IPName)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Decl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RPat)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RPatOp)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Annotation)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''InstHead)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Activation)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''CallConv)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Safety)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Rhs)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Match)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Op)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''InstDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ClassDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DeclHead)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Assoc)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ConDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''BangType)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''PatField)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''GuardedAlts)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Kind)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Context)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Name)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''SpecialCon)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''XName)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''TyVarBind)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Asst)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Splice)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Bracket)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Literal)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''QOp)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Type)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''QualStmt)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FieldUpdate)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Stmt)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Pat)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Binds)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Boxed)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''RuleVar)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''DataOrNew)

-- FIXME: Waiting for https://github.com/bos/aeson/issues/206.
{-
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ModuleHead)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ModuleName)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Comment)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ImportSpecList)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''ExportSpecList)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Deriving)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IfAlt)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''GuardedAlt)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Alt)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''PXAttr)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''XAttr)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FunDep)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''GadtDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''FieldDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''QualConDecl)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''IPBind)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''GuardedRhs)
$(deriveToJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''Rule)
-}


-- l (ModuleName l) (Maybe (WarningText l)) (Maybe (ExportSpecList l))
instance ToJSON l => ToJSON (ModuleHead l) where
    toJSON (ModuleHead s n wt es)
        = object [pack "ModuleHead" .= [toJSON s, toJSON n, toJSON wt, toJSON es]]

instance ToJSON l => ToJSON (ModuleName l) where
    toJSON (ModuleName s name)
        = object [pack "ModuleName" .= [toJSON s, toJSON name]]

-- data Comment = Comment Bool SrcSpan String
instance ToJSON Comment where
    toJSON (Comment b s c)
        = object [pack "Comment" .= [toJSON b, toJSON s, toJSON c]]

instance ToJSON l => ToJSON (ImportSpecList l) where
    toJSON (ImportSpecList l b ls)
        = object [pack "ImportSpecList" .= [toJSON l, toJSON b, toJSON ls]]

instance ToJSON l => ToJSON (ExportSpecList l) where
    toJSON (ExportSpecList l ls)
        = object [pack "ExportSpecList" .= [toJSON l, toJSON ls]]

instance ToJSON l => ToJSON (Deriving l) where
    toJSON (Deriving l hs) = object [pack "Deriving" .= [toJSON l, toJSON hs]]

instance ToJSON l => ToJSON (IfAlt l) where
    toJSON (IfAlt l e1 e2)
        = object [pack "IfAlt" .= [toJSON l, toJSON e1, toJSON e2]]

instance ToJSON l => ToJSON (GuardedAlt l) where
    toJSON (GuardedAlt l s1 e2)
        = object [pack "GuardedAlt" .= [toJSON l, toJSON s1, toJSON e2]]

instance ToJSON l => ToJSON (Alt l) where
    toJSON (Alt l p alts bs)
        = object [pack "Alt" .= [toJSON l, toJSON p, toJSON alts, toJSON bs]]

instance ToJSON l => ToJSON (PXAttr l) where
    toJSON (PXAttr l x p)
        = object [pack "PXAttr" .= [toJSON l, toJSON x, toJSON p]]

instance ToJSON l => ToJSON (XAttr l) where
    toJSON (XAttr l x e)
        = object [pack "XAttr" .= [toJSON l, toJSON x, toJSON e]]

instance ToJSON l => ToJSON (FunDep l) where
    toJSON (FunDep l n1 n2)
        = object [pack "FunDep" .= [toJSON l, toJSON n1, toJSON n2]]

instance ToJSON l => ToJSON (GadtDecl l) where
    toJSON (GadtDecl l n1 t2)
        = object [pack "GadtDecl" .= [toJSON l, toJSON n1, toJSON t2]]

instance ToJSON l => ToJSON (FieldDecl l) where
    toJSON (FieldDecl l n1 b2)
        = object [pack "FieldDecl" .= [toJSON l, toJSON n1, toJSON b2]]

instance ToJSON l => ToJSON (QualConDecl l) where
    toJSON (QualConDecl l tys c1 c2)
        = object [pack "QualConDecl" .= [toJSON l, toJSON tys, toJSON c1, toJSON c2]]

instance ToJSON l => ToJSON (IPBind l) where
    toJSON (IPBind l ip n)
        = object [pack "IPBind" .= [toJSON l, toJSON ip, toJSON n]]

instance ToJSON l => ToJSON (GuardedRhs l) where
    toJSON (GuardedRhs l s e)
        = object [pack "GuardedRhs" .= [toJSON l, toJSON s, toJSON e]]

instance ToJSON l => ToJSON (Rule l) where
    toJSON (Rule l s a r e1 e2)
        = object [pack "Rule" .= [toJSON l, toJSON s, toJSON a, toJSON r, toJSON e1, toJSON e2]]

instance ToJSON l => ToJSON (Exp l) where
    toJSON (Var a b) = object [pack "Var" .= [toJSON a, toJSON b]]
    toJSON (IPVar a b) = object [pack "IPVar" .= [toJSON a, toJSON b]]
    toJSON (Con a b) = object [pack "Con_" .= [toJSON a, toJSON b]]
    toJSON (Lit a b) = object [pack "Lit" .= [toJSON a, toJSON b]]
    toJSON (InfixApp a b c d) = object [pack "InfixApp" .= [toJSON a, toJSON b, toJSON c, toJSON d]]
    toJSON (App a b c) = object [pack "App" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (NegApp a b) = object [pack "NegApp" .= [toJSON a, toJSON b]]
    toJSON (Lambda a b c) = object [pack "Lambda" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (Let a b c) = object [pack "Let" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (If a b c d) = object [pack "If" .= [toJSON a, toJSON b, toJSON c, toJSON d]]
    toJSON (MultiIf a b) = object [pack "MultiIf" .= [toJSON a, toJSON b]]
    toJSON (Case a b c) = object [pack "Case" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (Do a b) = object [pack "Do" .= [toJSON a, toJSON b]]
    toJSON (MDo a b) = object [pack "MDo" .= [toJSON a, toJSON b]]
    toJSON (Tuple a b c) = object [pack "Tuple" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (TupleSection a b c) = object [pack "TupleSection" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (List a b) = object [pack "List" .= [toJSON a, toJSON b]]
    toJSON (Paren a b) = object [pack "Paren" .= [toJSON a, toJSON b]]
    toJSON (LeftSection a b c) = object [pack "LeftSection" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (RightSection a b c) = object [pack "RightSection" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (RecConstr a b c) = object [pack "RecConstr" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (RecUpdate a b c) = object [pack "RecUpdate" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (EnumFrom a b) = object [pack "EnumFrom" .= [toJSON a, toJSON b]]
    toJSON (EnumFromTo a b c) = object [pack "EnumFromTo" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (EnumFromThen a b c) = object [pack "EnumFromThen" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (EnumFromThenTo a b c d) = object [pack "EnumFromThenTo" .= [toJSON a, toJSON b, toJSON c, toJSON d]]
    toJSON (ListComp a b c) = object [pack "ListComp" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (ParComp a b c) = object [pack "ParComp" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (ExpTypeSig a b c) = object [pack "ExpTypeSig" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (VarQuote a b) = object [pack "VarQuote" .= [toJSON a, toJSON b]]
    toJSON (TypQuote a b) = object [pack "TypQuote" .= [toJSON a, toJSON b]]
    toJSON (BracketExp a b) = object [pack "BracketExp" .= [toJSON a, toJSON b]]
    toJSON (SpliceExp a b) = object [pack "SpliceExp" .= [toJSON a, toJSON b]]
    toJSON (QuasiQuote a b c) = object [pack "QuasiQuote" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (XTag a b c d e) = object [pack "XTag" .= [toJSON a, toJSON b, toJSON c, toJSON d, toJSON e]]
    toJSON (XETag a b c d) = object [pack "XETag" .= [toJSON a, toJSON b, toJSON c, toJSON d]]
    toJSON (XPcdata a b) = object [pack "XPcdata" .= [toJSON a, toJSON b]]
    toJSON (XExpTag a b) = object [pack "XExpTag" .= [toJSON a, toJSON b]]
    toJSON (XChildTag a b) = object [pack "XChildTag" .= [toJSON a, toJSON b]]
    toJSON (CorePragma a b c) = object [pack "CorePragma" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (SCCPragma a b c) = object [pack "SCCPragma" .= [toJSON a, toJSON b, toJSON c]]
    toJSON (GenPragma a b c d e) = object [pack "GenPragma" .= [toJSON a, toJSON b, toJSON c, toJSON d, toJSON e]]
    toJSON (Proc a b c) = object [pack "Proc" .= [toJSON a, toJSON b, toJSON c]]

-- | Parse the first argument and serialize as JSON to stdout.
main :: IO ()
main = getArgs >>= handleArgs

-- | Handle possible arguments.
handleArgs []                    = printUsage >> exitFailure
handleArgs ["--numeric-version"] = putStrLn version
handleArgs ["--version"]         = putStrLn $ "parser-helper v" ++ version
handleArgs ["--help"]            = printUsage
handleArgs ["-h"]                = printUsage
handleArgs (fileName:args)       = doWork fileName args

-- | Print usage info.
printUsage :: IO ()
printUsage = putStrLn . unlines $
    [ "Usage: parser-helper [ file | --numeric-version | --version ] [-XExtension ...]"
    , ""
    , "To parse from stdin, use - instead of file: parser-helper -"
    ]

getExtensions :: [String] -> [Extension]
getExtensions = map parseExtension . filter ("-X" `isPrefixOf`)
  where
    parseExtension x = maybe (error $ "Invalid extension " ++ show x) EnableExtension . readMaybe . drop 2 $ x

-- | Parse, serialize and print to stdout.
doWork :: String -> [String] -> IO ()
doWork fileName args = do
    let extensions = getExtensions args
        parseOpts = defaultParseMode { parseFilename = "A.hs"
                                     , baseLanguage = Haskell2010
                                     , extensions = ghcDefault ++ extensions
                                     }
    p <- if fileName == "-" then
            parseFileContentsWithComments parseOpts <$> getContents
         else
            parseFileWithComments parseOpts fileName

    case p of
        ParseOk tree -> B.putStrLn (encode tree)
        ParseFailed loc msg -> putStrLn ("ERROR:" ++ msg) >> exitFailure
