{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grammar (parseHadron) where

import Ast
import Control.Applicative
import Data.Char
import qualified Data.Maybe
import Text.Earley

parseHadron :: forall r. Grammar r (Prod r String Char HadronDef)
parseHadron =
  mdo
    ws <- rule $ many $ satisfy isSpace

    let tok :: Prod r String Char a -> Prod r String Char a
        tok p = ws *> p

        kw_chars :: String -> String -> Prod r String Char Sym
        kw_chars word [c] = Sym word <$ satisfy (== c)
        kw_chars word (c : cs) = satisfy (== c) *> kw_chars word cs
        kw_chars word [] = pure (Sym word)

        kw word = ws *> kw_chars word word

        sym x = tok $ token x <?> [x]

        usym = tok $ (\f r -> Sym (f : r)) <$> satisfy isUpperCase <*> many (satisfy isAlphaNum) <?> "usym"

        lsym = tok $ (\f r -> Sym (f : r)) <$> satisfy isLowerCase <*> many (satisfy isAlphaNum) <?> "lsym"

        tvar = tok $ (\t f r -> Sym (t : (f : r))) <$> sym '`' <*> satisfy isUpperCase <*> many (satisfy isAlphaNum) <?> ""

        sign =
          ( \case
              Just _ -> "-"
              Nothing -> ""
          )
            <$> optional (satisfy (== '-'))

        dot = "." <$ satisfy (== '.')

        float_literal =
          tok $
            (\s digs _ fracs ex -> read (s ++ digs ++ "." ++ fracs ++ ex) :: Float) <$> sign <*> some (satisfy isDigit) <*> dot <*> some (satisfy isDigit) <*> opt_expo

        expo =
          (\_ exp_sign exp_digits -> "e" ++ exp_sign ++ exp_digits)
            <$> satisfy (== 'e')
            <*> sign
            <*> some (satisfy isDigit)

        opt_expo =
          ( \case
              Just e -> e
              Nothing -> ""
          )
            <$> optional expo

        int_literal = tok $ (\s d -> read (s ++ d) :: Int) <$> sign <*> some (satisfy isDigit)

    ident <-
      rule $
        Ident
          <$> colon_usyms

    coco <- rule $ (\_ _ -> "::") <$> sym ':' <*> sym ':'

    colon_usyms <-
      rule $
        (:) <$> usym <* coco <*> colon_usyms
          <|> (: []) <$> usym

    comma_usyms <-
      rule $
        (:) <$> usym <* sym ',' <*> comma_usyms
          <|> (: []) <$> usym

    usedecl <-
      rule $
        flip UseNames <$> (kw "use" *> usenames) <* kw "from" <*> ident
          <|> UseHadron <$> (kw "use" *> ident <?> "import entire hadron")

    usenames <- rule $ sym '{' *> comma_usyms <* sym '}'

    comma_types <-
      rule $
        (:) <$> stype <* sym ',' <*> comma_types
          <|> (: []) <$> stype

    stype <-
      rule $
        NamedType <$> ident <*> optional type_args
          <|> TypeVar <$> tvar

    stypes <-
      rule $
        (:) <$> stype <* sym ',' <*> stypes
          <|> (: []) <$> stype

    typed_name <-
      rule $
        TypedName <$> lsym <* sym ':' <*> stype

    typed_names <-
      rule $
        (:) <$> typed_name <* sym ',' <*> typed_names
          <|> (: []) <$> typed_name

    type_args <-
      rule $
        sym '[' *> stypes <* sym ']'

    type_param <-
      rule $
        TypeParamSpec <$> tvar <*> optional stype <?> "type parameter spec"

    type_params <-
      rule $
        (:) <$> type_param <* sym ',' <*> type_params
          <|> (: []) <$> type_param

    type_param_block <-
      rule $
        sym '[' *> type_params <* sym ']'

    def <-
      rule $
        Fl <$> flavor_def
          <|> Qu <$> quark_def
          <|> Bo <$> boson_def

    flavor_def <-
      rule $
        (\_ n tps cs _ chs _ _ -> FlavorDef n tps cs chs)
          <$> kw "flavor"
          <*> usym
          <*> optional type_param_block
          <*> optional composes
          <*> kw "is"
          <*> some channel
          <*> kw "end"
          <*> optional (kw "@flavor")

    composes <- rule $ kw "composes" *> stypes

    channel <- rule $ (\_ n _ d t -> ChannelDef n t d) <$> kw "chan" <*> usym <*> sym ':' <*> dir <*> stype <?> "channel def"

    dir <-
      rule $
        CIn <$ kw "in"
          <|> COut <$ kw "out"
          <|> CBoth <$ kw "both"

    boson_def <-
      rule $
        (\_ n tp _ os _ _ -> BosonDef n tp os)
          <$> kw "boson"
          <*> usym
          <*> optional type_param_block
          <*> kw "is"
          <*> some boson_option
          <*> kw "end"
          <*> optional (kw "@boson")

    boson_option <-
      rule $
        (\s _ ts _ -> BosonTupleOptionDef s ts) <$> usym <*> sym '(' <*> stypes <*> sym ')'
          <|> (\name _ vs _ -> BosonStructOptionDef name vs) <$> usym <*> sym '{' <*> typed_names <*> sym '}'

    quark_def <-
      rule $
        (\_ tps name vps provs _ slots behs _ initial_beh _ params _ _ _ -> QuarkDef name tps vps provs slots behs (initial_beh, params))
          <$> kw
            "quark"
          <*> optional type_param_block
          <*> usym
          <*> value_params
          <*> provides
          <*> kw "is"
          <*> some slot
          <*> some behavior
          <*> kw "adopt"
          <*> usym
          <*> sym '('
          <*> exprs
          <*> sym ')'
          <*> kw "end"
          <*> optional (kw "@quark")

    bindings <-
      rule $
        (:) <$> binding <* sym ',' <*> bindings
          <|> (: []) <$> binding

    binding <-
      rule $ (\field _ value -> (field, value)) <$> lsym <*> sym '=' <*> expr

    exprs <-
      rule $
        (:) <$> expr <* sym ',' <*> exprs
          <|> (: []) <$> expr

    expr <-
      rule $
        (\_ typespec _ params _ -> CreateExpr typespec params) <$> kw "create" <*> stype <*> sym '(' <*> exprs <*> sym ')'
          <|> logic_expr

    logic_op <-
      rule $
        OpAnd <$ kw "and"
          <|> OpOr <$ kw "or"

    logic_expr <-
      rule $
        (\l op r -> OpExpr op [l, r]) <$> logic_expr <*> logic_op <*> compare_expr
          <|> compare_expr

    compare_op <-
      rule $
        OpLeq <$ kw "<="
          <|> OpLt <$ sym '<'
          <|> OpGeq <$ kw ">="
          <|> OpGt <$ sym '>'
          <|> OpEq <$ kw "=="
          <|> OpNeq <$ kw "!="

    compare_expr <-
      rule $
        (\l op r -> OpExpr op [l, r]) <$> add_expr <*> compare_op <*> compare_expr
          <|> add_expr

    add_op <-
      rule $
        OpPlus <$ sym '+'
          <|> OpMinus <$ sym '-'

    add_expr <-
      rule $
        (\l op r -> OpExpr op [l, r]) <$> mult_expr <*> add_op <*> add_expr
          <|> mult_expr

    mult_op <-
      rule $
        OpTimes <$ sym '*'
          <|> OpDiv <$ sym '/'
          <|> OpMod <$ sym '%'

    mult_expr <-
      rule $
        (\l op r -> OpExpr op [l, r]) <$> pow_expr <*> mult_op <*> mult_expr
          <|> pow_expr

    pow_expr <-
      rule $
        (\l _ r -> OpExpr OpPow [l, r]) <$> unary_expr <*> sym '^' <*> pow_expr
          <|> unary_expr

    unary_op <-
      rule $
        OpUminus <$ sym '-'
          <|> OpNot <$ kw "not"

    unary_expr <-
      rule $
        ( \u r -> case u of
            Just op -> OpExpr op [r]
            Nothing -> r
        )
          <$> optional unary_op
          <*> primary_expr

    primary_expr <-
      rule $
        FloatLitExpr <$> float_literal
          <|> IntLitExpr <$> int_literal
          --          <|> string_literal
          --          <|> char_literal
          <|> VarRefExpr <$> lsym
          <|> (\name _ values _ -> BosonTupleExpr name values) <$> ident <*> sym '(' <*> exprs <*> sym ')'
          <|> (\name _ values _ -> BosonStructExpr name values) <$> ident <*> sym '{' <*> bindings <*> sym '}'
          <|> sym '(' *> expr <* sym ')'

    provides <- rule $ kw "provides" *> some channel

    slot <-
      rule $
        (\_ s _ t -> TypedName s t)
          <$> kw "slot"
          <*> lsym
          <*> sym ':'
          <*> stype

    behavior <-
      rule $
        (\_ bid vps _ acts _ _ -> BehaviorDef bid vps acts)
          <$> kw "behavior"
          <*> usym
          <*> value_params
          <*> kw "do"
          <*> some action
          <*> kw "end"
          <*> optional (kw "@behavior")

    value_params <- rule $ sym '(' *> typed_names <* sym ')'

    action <-
      rule $
        par_action
          <|> seq_action
          <|> select_action
          <|> send_action
          <|> recv_action
          <|> assign_action
          <|> local_action
          <|> cond_action
          <|> for_action
          <|> while_action
          <|> adopt_action
          <|> exit_action

    par_action <- rule $ (\_ _ body _ -> Par body) <$> kw "par" <*> sym '{' <*> some action <*> sym '}'

    seq_action <- rule $ (\_ _ body _ -> Seq body) <$> kw "seq" <*> sym '{' <*> some action <*> sym '}'

    select_action <- rule $ (\_ _ body _ -> Select body) <$> kw "select" <*> sym '{' <*> some action <*> sym '}'

    send_action <- rule $ (\_ ch _ value _ -> Send ch value) <$> kw "send" <*> expr <*> sym '(' <*> expr <*> sym ')'

    recv_action <-
      rule $
        (\_ chanExpr _ clauses ec _ _ -> Recv chanExpr clauses ec)
          <$> kw "recv"
          <*> expr
          <*> kw "do"
          <*> some recv_clause
          <*> optional recv_else_clause
          <*> kw "end"
          <*> optional (kw "@recv")

    recv_else_clause <-
      rule $ kw "else" *> some action

    recv_clause <-
      rule $
        (\_ pat _ acts _ -> RecvClause pat acts)
          <$> kw "on"
          <*> msg_pattern
          <*> kw "do"
          <*> some action
          <*> kw "end"

    msg_pattern <-
      rule $
        tuple_pattern
          <|> struct_pattern

    tuple_pattern <-
      rule $
        (\tname _ binds _ -> TuplePattern tname (Data.Maybe.fromMaybe [] binds))
          <$> ident
          <*> sym '('
          <*> optional tuple_pat_bindings
          <*> sym ')'

    tuple_pat_bindings <-
      rule $
        (:) <$> tuple_pat_binding <* sym ',' <*> tuple_pat_bindings
          <|> (: []) <$> tuple_pat_binding

    tuple_pat_binding <- rule lsym

    struct_pattern <-
      rule $
        (\bn _ str_fields _ -> StructPattern bn str_fields)
          <$> ident
          <*> sym '{'
          <*> struct_pat_bindings
          <*> sym '}'

    struct_pat_bindings <-
      rule $
        (:) <$> struct_pat_binding <* sym ',' <*> struct_pat_bindings
          <|> (: []) <$> struct_pat_binding

    struct_pat_binding <-
      rule $
        (,) <$> usym <* sym ';' <*> usym

    assign_action <-
      rule $
        Assign <$> lsym <* kw ":=" <*> expr

    local_action <-
      rule $
        (\_ n _ ty _ value -> LocalVar n ty value)
          <$> kw "local"
          <*> lsym
          <*> sym ':'
          <*> stype
          <*> sym '='
          <*> expr

    cond_action <-
      rule $
        (\_ cls ec _ _ -> Cond cls ec)
          <$> kw "cond"
          <*> some cond_clause
          <*> optional else_clause
          <*> kw "end"
          <*> optional (kw "@cond")

    cond_clause <-
      rule $
        (\cond _ acts _ -> (cond, acts))
          <$> expr
          <*> kw "then"
          <*> some action
          <*> kw "end"

    else_clause <-
      rule $
        kw "else" *> some action

    for_action <-
      rule $
        (\_ idx _ vs _ body _ _ -> For idx vs body)
          <$> kw "for"
          <*> lsym
          <*> kw "in"
          <*> expr
          <*> kw "do"
          <*> some action
          <*> kw "end"
          <*> optional (kw "@for")

    while_action <-
      rule $
        (\_ e _ b _ _ -> While e b)
          <$> kw "while"
          <*> expr
          <*> kw "do"
          <*> some action
          <*> kw "end"
          <*> optional (kw "@while")

    adopt_action <-
      rule $
        (\_ s _ params _ -> Adopt s params)
          <$> kw "adopt"
          <*> usym
          <*> sym '('
          <*> exprs
          <*> sym ')'

    exit_action <-
      rule $ Exit <$ kw "exit"

    hadron <- rule $ HadronDef <$> many usedecl <*> many def

    return hadron
