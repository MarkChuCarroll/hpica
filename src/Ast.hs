module Ast (
  Sym (..),
  Ident (..),
  SType (..),
  UseDecl (..),
  TypedName (..),
  FlavorDef (..),
  QuarkDef (..),
  BosonDef (..),
  BosonOptionDef (..),
  ChannelDirection (..),
  ChannelDef (..),
  TypeParamSpec (..),
  BehaviorDef (..),
  Action (..),
  RecvClause (..),
  MessagePattern (..),
  Operator (..),
  Expr (..),
  PicaDef (..),
  HadronDef (..),
) where

newtype Sym = Sym String deriving (Eq)

instance Show Sym where
  show (Sym s) = s

newtype Ident = Ident [Sym] deriving (Eq)

instance Show Ident where
  show (Ident segs) = foldl1 (\l r -> l ++ "::" ++ r) (map show segs)

-- SType: syntactic types - the types as parsed from the source code.
data SType
  = NamedType Ident (Maybe [SType])
  | TypeVar Sym
  | Chan ChannelDirection SType
  deriving (Show, Eq)

data UseDecl
  = UseHadron Ident
  | UseNames Ident [Sym]
  deriving (Show, Eq)

data TypedName = TypedName Sym SType
  deriving (Show, Eq)

data FlavorDef
  = FlavorDef
  { fd_name :: Sym
  , fd_type_params :: Maybe [TypeParamSpec]
  , fd_composes :: Maybe [SType]
  , fd_channels :: [ChannelDef]
  }
  deriving (Show, Eq)

data QuarkDef
  = QuarkDef
  { qd_name :: Sym
  , qd_type_params :: Maybe [TypeParamSpec]
  , qd_value_params :: [TypedName]
  , qd_provides_channels :: [ChannelDef]
  , qd_slots :: [TypedName]
  , qh_behaviors :: [BehaviorDef]
  , qd_default :: (Sym, [Expr])
  }
  deriving (Show, Eq)

data BosonDef
  = BosonDef
  { boson_name :: Sym
  , boson_type_params :: Maybe [TypeParamSpec]
  , boson_options :: [BosonOptionDef]
  }
  deriving (Show, Eq)

data BosonOptionDef
  = BosonTupleOptionDef Sym [SType]
  | BosonStructOptionDef Sym [TypedName]
  deriving (Show, Eq)

data ChannelDirection = CIn | COut | CBoth
  deriving (Show, Eq)

data ChannelDef
  = ChannelDef
  { cd_name :: Sym
  , cd_type :: SType
  , cd_dir :: ChannelDirection
  }
  deriving (Show, Eq)

data TypeParamSpec
  = TypeParamSpec
  { tps_name :: Sym
  , tps_constraint :: Maybe SType
  }
  deriving (Show, Eq)

data BehaviorDef
  = BehaviorDef
  { bd_name :: Sym
  , bd_params :: [TypedName]
  , bd_body :: [Action]
  }
  deriving (Show, Eq)

data Action
  = Par [Action]
  | Seq [Action]
  | Select [Action]
  | Send
      { as_chan :: Expr
      , as_value :: Expr
      }
  | Recv
      { ar_chan :: Expr
      , ar_clauses :: [RecvClause]
      , ar_else :: [Action]
      }
  | Assign Sym Expr
  | LocalVar Sym SType Expr
  | Cond [(Expr, [Action])] [Action]
  | For Sym Expr [Action]
  | While Expr [Action]
  | Adopt Sym [Expr]
  | Exit
  deriving (Eq, Show)

data RecvClause
  = RecvClause
  { rc_pat :: MessagePattern
  , rc_action :: [Action]
  }
  deriving (Eq, Show)

data MessagePattern
  = TuplePattern Ident [Sym]
  | StructPattern Ident [(Sym, Sym)]
  deriving (Eq, Show)

data Operator
  = OpPlus
  | OpMinus
  | OpTimes
  | OpDiv
  | OpMod
  | OpPow
  | OpAnd
  | OpOr
  | OpNot
  | OpUminus
  | OpEq
  | OpNeq
  | OpGeq
  | OpLeq
  | OpGt
  | OpLt
  deriving (Eq, Show)

data Expr
  = CreateExpr SType [Expr]
  | BosonTupleExpr [Expr]
  | BosonStructExpr [(Sym, Expr)]
  | StrLitExpr String
  | IntLitExpr Int
  | FloatLitExpr Float
  | CharLitExpr Char
  | VarRefExpr Ident
  | OpExpr Operator [Expr]
  | ChannelExpr Expr Sym
  deriving (Eq, Show)

data PicaDef
  = Fl FlavorDef
  | Qu QuarkDef
  | Bo BosonDef
  deriving (Show)

data HadronDef = HadronDef [UseDecl] [PicaDef] deriving (Show)
