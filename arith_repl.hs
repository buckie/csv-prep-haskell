module AExp where

data AExp n =  Lit n | BiOp String (AExp n) (AExp n) | UnOp String (AExp n)
  deriving (Eq,Show)

mult :: AExp n -> AExp n -> AExp n
mult = BiOp "MULT"

div :: AExp n -> AExp n -> AExp n
div = BiOp "DIV"

add :: AExp n -> AExp n -> AExp n
add = BiOp "ADD"

sub :: AExp n -> AExp n -> AExp n
sub = BiOp "Sub"

interp :: (Num a, Fractional a, Show a) => AExp a -> a
interp (BiOp opName a b) =  opChoice (interp a) (interp b)
  where
    opChoice = case opName of
                  "MULT" -> (*)
                  "DIV" -> (/)
                  "ADD" -> (+)
                  "SUB" -> (-)
                  what  -> error $ "you gave a bad op with the name: " ++ what
interp (Lit n) = n
interp x = error $  "we dont know how to run that yet" ++ (show x)

