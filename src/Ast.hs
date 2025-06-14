module Ast where

-- A typeclass for the operation of replacing a type variable with
-- a type as a result of an instantiation.
class TypeSubstitutable t where
  substitute :: Sym -> SType -> t -> t
  open_typevars :: t -> [Sym]

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

instance TypeSubstitutable SType where
  open_typevars (NamedType _ Nothing) = []
  open_typevars (NamedType _ (Just types)) = types >>= open_typevars
  open_typevars (TypeVar sym) = [sym]
  open_typevars (Chan _ st) = open_typevars st

  substitute _ _ nt@(NamedType _ Nothing) = nt
  substitute old new (NamedType name (Just types)) =
    NamedType name (Just (fmap (substitute old new) types))
  substitute old new tv@(TypeVar sym) =
    if sym == old then new else tv
  substitute old new (Chan dir st) =
    Chan dir (substitute old new st)

data UseDecl
  = UseHadron Ident
  | UseNames Ident [Sym]
  deriving (Show, Eq)

data TypedName = TypedName Sym SType
  deriving (Show, Eq)

instance TypeSubstitutable TypedName where
  open_typevars (TypedName _ stype) = open_typevars stype

  substitute old new (TypedName n stype) = TypedName n (substitute old new stype)

data FlavorDef
  = FlavorDef
  { fd_name :: Sym
  , fd_type_params :: Maybe [TypeParamSpec]
  , fd_composes :: Maybe [SType]
  , fd_channels :: [ChannelDef]
  }
  deriving (Show, Eq)

instance TypeSubstitutable FlavorDef where
  open_typevars fd = open_in_type_params ++ open_in_composes ++ open_in_channels
   where
    open_in_type_params = case fd_type_params fd of
      Just tps -> tps >>= open_typevars
      Nothing -> []
    open_in_composes = case fd_composes fd of
      Just cs -> cs >>= open_typevars
      Nothing -> []
    open_in_channels = fd_channels fd >>= open_typevars

  substitute old new (FlavorDef name type_params composes chans) =
    FlavorDef name subst_type_params subst_composes subst_channels
   where
    subst_type_params = fmap (fmap (substitute old new)) type_params
    subst_composes = fmap (fmap (substitute old new)) composes
    subst_channels = fmap (substitute old new) chans

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

instance TypeSubstitutable QuarkDef where
  open_typevars (QuarkDef _ type_params val_params channels slots behs (_, beh_params)) =
    maybe [] (>>= open_typevars) type_params
      ++ (val_params >>= open_typevars)
      ++ (slots >>= open_typevars)
      ++ (channels >>= open_typevars)
      ++ (behs >>= open_typevars)
      ++ (beh_params >>= open_typevars)

  substitute old new (QuarkDef name type_params val_params channels slots behs (beh_name, beh_params)) =
    QuarkDef name subst_type_params subst_val_params subst_channels subst_slots subst_behs (beh_name, subst_beh_params)
   where
    subst_type_params = fmap (fmap (substitute old new)) type_params
    subst_val_params = fmap (substitute old new) val_params
    subst_channels = fmap (substitute old new) channels
    subst_slots = fmap (substitute old new) slots
    subst_behs = fmap (substitute old new) behs
    subst_beh_params = fmap (substitute old new) beh_params

data BosonDef
  = BosonDef
  { boson_name :: Sym
  , boson_type_params :: Maybe [TypeParamSpec]
  , boson_options :: [BosonOptionDef]
  }
  deriving (Show, Eq)

instance TypeSubstitutable BosonDef where
  open_typevars (BosonDef _ type_params options) =
    maybe [] (>>= open_typevars) type_params ++ (options >>= open_typevars)

  substitute old new (BosonDef name params options) =
    BosonDef name (fmap (map (substitute old new)) params) (map (substitute old new) options)

data BosonOptionDef
  = BosonTupleOptionDef Sym [SType]
  | BosonStructOptionDef Sym [TypedName]
  deriving (Show, Eq)

instance TypeSubstitutable BosonOptionDef where
  open_typevars (BosonTupleOptionDef _ types) = concatMap open_typevars types
  open_typevars (BosonStructOptionDef _ typed_names) = concatMap open_typevars typed_names

  substitute old new (BosonTupleOptionDef name types) =
    BosonTupleOptionDef name (fmap (substitute old new) types)
  substitute old new (BosonStructOptionDef name typed_names) =
    BosonStructOptionDef name (fmap (substitute old new) typed_names)

data ChannelDirection = CIn | COut | CBoth
  deriving (Show, Eq)

data ChannelDef
  = ChannelDef
  { cd_name :: Sym
  , cd_type :: SType
  , cd_dir :: ChannelDirection
  }
  deriving (Show, Eq)

instance TypeSubstitutable ChannelDef where
  open_typevars (ChannelDef _ stype _) =
    open_typevars stype

  substitute old new (ChannelDef name stype dir) =
    ChannelDef name (substitute old new stype) dir

data TypeParamSpec
  = TypeParamSpec
  { tps_name :: Sym
  , tps_constraint :: Maybe SType
  }
  deriving (Show, Eq)

instance TypeSubstitutable TypeParamSpec where
  open_typevars (TypeParamSpec _ cst) = maybe [] open_typevars cst

  substitute old new (TypeParamSpec n cst) = TypeParamSpec n (fmap (substitute old new) cst)

data BehaviorDef
  = BehaviorDef
  { bd_name :: Sym
  , bd_params :: [TypedName]
  , bd_body :: [Action]
  }
  deriving (Show, Eq)

instance TypeSubstitutable BehaviorDef where
  open_typevars (BehaviorDef _ params body) =
    (params >>= open_typevars) ++ (body >>= open_typevars)

  substitute old new (BehaviorDef name params body) =
    BehaviorDef name (fmap (substitute old new) params) (fmap (substitute old new) body)

data CondClause = CondClause
  { cc_cond :: Expr
  , cc_actions :: [Action]
  }
  deriving (Show, Eq)

instance TypeSubstitutable CondClause where
  open_typevars (CondClause cond actions) = open_typevars cond ++ (actions >>= open_typevars)
  substitute old new (CondClause cond actions) =
    CondClause (substitute old new cond) (map (substitute old new) actions)

data SendBody = SendBody {send_channel :: Expr, send_value :: Expr} deriving (Eq, Show)

instance TypeSubstitutable SendBody where
  open_typevars (SendBody chan val) = open_typevars chan ++ open_typevars val

  substitute old new (SendBody chan val) =
    SendBody (substitute old new chan) (substitute old new val)

data RecvBody = RecvBody
  { recv_chan :: Expr
  , recv_clauses :: [RecvClause]
  , recv_else :: Maybe [Action]
  }
  deriving (Eq, Show)

instance TypeSubstitutable RecvBody where
  open_typevars (RecvBody chan clauses elseClause) =
    open_typevars chan
      ++ (clauses >>= open_typevars)
      ++ maybe [] (>>= open_typevars) elseClause

  substitute old new (RecvBody chan clauses else_clause) =
    RecvBody (substitute old new chan) (map (substitute old new) clauses) (fmap (map (substitute old new)) else_clause)

data Action
  = Par [Action]
  | Seq [Action]
  | Select [Action]
  | Send SendBody
  | Recv RecvBody
  | Assign Sym Expr
  | LocalVar Sym SType Expr
  | Cond [CondClause] (Maybe [Action])
  | For Sym Expr [Action]
  | While Expr [Action]
  | Adopt Sym [Expr]
  | Exit
  deriving (Eq, Show)

instance TypeSubstitutable Action where
  open_typevars (Par actions) = actions >>= open_typevars
  open_typevars (Seq actions) = actions >>= open_typevars
  open_typevars (Select actions) = actions >>= open_typevars
  open_typevars (Send sb) = open_typevars sb
  open_typevars (Recv recv_body) = open_typevars recv_body
  open_typevars (Assign _ e) = open_typevars e
  open_typevars (LocalVar _ stype expr) = open_typevars stype ++ open_typevars expr
  open_typevars (Cond clauses elseClause) = concatMap open_typevars clauses ++ maybe [] (>>= open_typevars) elseClause
  open_typevars (For _ e as) = open_typevars e ++ (as >>= open_typevars)
  open_typevars (While e as) = open_typevars e ++ (as >>= open_typevars)
  open_typevars (Adopt _ es) = es >>= open_typevars
  open_typevars _ = []

  substitute old new (Par actions) = Par (map (substitute old new) actions)
  substitute old new (Seq actions) = Seq (map (substitute old new) actions)
  substitute old new (Select actions) = Select (map (substitute old new) actions)
  substitute old new (Send sb) = Send (substitute old new sb)
  substitute old new (Recv rb) = Recv (substitute old new rb)
  substitute old new (Assign s e) = Assign s (substitute old new e)
  substitute old new (LocalVar s t e) = LocalVar s (substitute old new t) (substitute old new e)
  substitute old new (Cond cls a) = Cond (map (substitute old new) cls) (fmap (map (substitute old new)) a)
  substitute old new (For s e as) = For s (substitute old new e) (map (substitute old new) as)
  substitute old new (While e as) = While (substitute old new e) (map (substitute old new) as)
  substitute old new (Adopt s es) = Adopt s (map (substitute old new) es)
  substitute _ _ Exit = Exit

data RecvClause
  = RecvClause
  { rc_pat :: MessagePattern
  , rc_action :: [Action]
  }
  deriving (Eq, Show)

instance TypeSubstitutable RecvClause where
  open_typevars (RecvClause _ actions) = actions >>= open_typevars

  substitute old new (RecvClause pat actions) = RecvClause pat (map (substitute old new) actions)

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
  | BosonTupleExpr Ident [Expr]
  | BosonStructExpr Ident [(Sym, Expr)]
  | StrLitExpr String
  | IntLitExpr Int
  | FloatLitExpr Float
  | CharLitExpr Char
  | VarRefExpr Sym
  | OpExpr Operator [Expr]
  deriving (Eq, Show)

instance TypeSubstitutable Expr where
  open_typevars (CreateExpr st exprs) = open_typevars st ++ (exprs >>= open_typevars)
  open_typevars (BosonTupleExpr _ es) = es >>= open_typevars
  open_typevars (BosonStructExpr _ bes) =
    let open_typevars_in_pair (_, e) = open_typevars e
     in bes >>= open_typevars_in_pair
  open_typevars (OpExpr _ es) = es >>= open_typevars
  open_typevars _ = []

  substitute old new (CreateExpr st exprs) = CreateExpr (substitute old new st) (map (substitute old new) exprs)
  substitute old new (BosonTupleExpr s exprs) = BosonTupleExpr s (map (substitute old new) exprs)
  substitute old new (BosonStructExpr s bes) = BosonStructExpr s (map subst_struct_value bes)
   where
    subst_struct_value (key, val) = (key, substitute old new val)
  substitute old new (OpExpr op exprs) = OpExpr op (map (substitute old new) exprs)
  substitute _ _ e = e

data PicaDef
  = Fl FlavorDef
  | Qu QuarkDef
  | Bo BosonDef
  deriving (Show)

instance TypeSubstitutable PicaDef where
  open_typevars (Fl fl) = open_typevars fl
  open_typevars (Qu qu) = open_typevars qu
  open_typevars (Bo bo) = open_typevars bo

  substitute old new (Fl fl) = Fl (substitute old new fl)
  substitute old new (Qu qu) = Qu (substitute old new qu)
  substitute old new (Bo bo) = Bo (substitute old new bo)

data HadronDef = HadronDef [UseDecl] [PicaDef] deriving (Show)
