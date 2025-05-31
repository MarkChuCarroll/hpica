{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grammar (parseHadron) where

import Ast
import Control.Applicative
import Data.Char
import Text.Earley

parseHadron :: forall r. Grammar r (Prod r String String HadronDef)
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

    ident <-
      rule $
        Ident
          <$> colon_usyms

    hadron <- rule $ HadronDef <$> many usedecl <*> many def

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
        sym '['

    type_param <-
      rule $
        TypeParamSpec <$> tvar <*> optional stype <?> "type parameter spec"

    type_params <-
      rule $
        (:) <$> type_param <*> many (sym ',' *> type_params)
          <|> (: []) <$> type_param

    type_param_block <-
      rule $
        sym '[' *> type_params <* sym ']'

    def <-
      rule $
        flavor_def
          <|> quark_def
          <|> boson_def

    flavor_def <-
      rule $
        (\_ n tps cs _ chs _ _ -> FlavorDef n tps cs chs) <$> kw "flavor" <*> usym <*> optional type_params <*> optional composes <*> kw "is" <*> some channel <*> kw "end" <*> optional (kw "@flavor")

    composes <- rule $ kw "composes" *> stypes

    channel <- rule $ (\_ n _ d t -> ChannelDef n t d) <$> kw "chan" <*> usym <*> sym ':' <*> dir <*> stype <?> "channel def"

    dir <-
      rule $
        CIn <$ kw "in"
          <|> COut <$ kw "out"
          <|> CBoth <$ kw "both"

    boson_def <-
      rule $
        (\_ n tp _ os _ _ -> BosonDef n tp os) <$> kw "boson" <*> usym <*> optional type_param_block <*> kw "is" <*> some boson_option <*> kw "end" <*> optional (kw "@boson")

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
          <*> ')'
          <*> kw "end"
          <*> optional (kw "@quark")

    provides <- rule $ kw "provides" *> some channel

    value_params <- rule $ sym '(' *> typed_names <* sym ')'

    return hadron
