
module Twist where

data Twist =
    TObj String [Twist]
  | TAttr String String
  | TVal String Twist
  | TArray String [Twist]
  | TOpt (Maybe Twist)

join :: String -> [String] -> String
join sep ("":t) = join sep t
join _ (h:[]) = h
join _ [] = ""
join sep (h:t) = h ++ sep ++  (join sep t)

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

indent :: Int -> String
indent n = duplicate "   " n

twist_to_text :: Int -> Twist -> String
twist_to_text n (TObj lbl cs) =
  indent(n) ++ "{" ++ lbl ++ "}:\n" ++ (join "\n" (map (twist_to_text (n+1)) cs))
twist_to_text n (TAttr lbl v) = indent(n) ++ lbl ++ " = " ++ v
twist_to_text n (TVal lbl v) = indent(n) ++ lbl ++ ":\n" ++ (twist_to_text (n+1) v)
twist_to_text n (TArray lbl vs) = indent(n) ++ "[" ++ lbl ++ "]:\n" ++ (join "\n" (map (twist_to_text (n+1)) vs))
twist_to_text n (TOpt (Just t)) = twist_to_text n t
twist_to_text _ (TOpt Nothing) = ""

class Twistable a where
  twist :: a -> Twist
