import Data.List
import Text.ParserCombinators.Parsec

-- Declara termos
data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type
        deriving Show

-- Declara nomes
type Name = String

-- Declara unificadores
type Unifier = [(Name, Type)]

--
-- { X |-> Y }
--

-- Árvore de derivações: 
--
--                     parseType
--                    /         \
--               parseFun       parseAtom
--                /   \             /  |  \
--        parseAtom  parseType  pInt  pVar pParen
--       /   |  \      /   \        
--    ...  ...  ...  ...  ...

{-----------------------PARSERS-----------------}
-- int: "Int"
parseInt :: Parser Type
parseInt = do
        char 'I'
        char 'n'
        char 't'
        return TypeInt

-- var: lowercase+
parseVar :: Parser Type
parseVar = do
        variable <- many1 lower
        return (TypeVar variable)

-- paren: "(" type ")"
parseParen :: Parser Type 
parseParen = do
        char '('
        atom <- parseAtom
        char ')'
        return atom

-- atom: int | var | paren
parseAtom :: Parser Type
parseAtom = try
        parseInt <|> parseVar <|> parseParen

-- fun: atom "->" type
parseFun :: Parser Type
parseFun = do
        atom <- parseAtom
        whitespace
        char '-'
        char '>'
        whitespace
        TypeArrow atom <$> parseType

-- type: function | atom
parseType :: Parser Type
parseType = try
        parseFun <|> parseAtom


{---------------------LEITURA TIPOS-----------------}
-- unit: type eof
unit :: Parser Type
unit = do
        t <- parseType
        eof
        return t

--- espaço
whitespace :: Parser ()
whitespace = do
        many (char ' ')
        return ()


{--------------------UNIFICADORES-------------------}
unify :: Type -> Type -> Maybe Unifier

unify (TypeInt) (TypeInt) = 
      Just []

unify (TypeVar x) (TypeVar y) | x==y = 
      Just []

unify (TypeVar x) e | not (occursCheck x e) = 
      Just [(x , e)]

unify e (TypeVar x) | not (occursCheck x e) = 
      Just [(x , e)]




{--------------------AUXILIARES-------------------}

--- ocorrencia
occursCheck :: Name -> Type -> Bool
occursCheck x TypeInt = 
        False
occursCheck x (TypeVar y) =
         x == y
occursCheck x (TypeArrow y z) =  
        occursCheck x y || occursCheck x z



--compose :: Unifer -> Unifier -> Unifier
--compose xs ys = xs ++ ys
compose :: Unifier -> Unifier -> Unifier
compose xs ys =
  xs ++ fmap substElement ys
  where
    substElement :: (Name, Type) -> (Name, Type)
    substElement (x, e) =
      -- Essa função irá ser usada em cada elemento de s1...
      -- O nome da variável permanece o mesmo, porém, o
      -- elemento e é trocado pela substituição s2(e)!
      (x, subst xs e)



{------------------TESTE---------------------}

---substituiçao
subst :: Unifier -> Type -> Type
subst x TypeInt = 
      TypeInt
subst s (TypeVar x) = 
      case lookup x s of 
        Just e -> e 
        Nothing -> (TypeVar x)
-- subts {x |-> a} (x->y) = (a->y)
subst s (TypeArrow x y ) =
      subst s x 
    -- subst s y


{-------------------MAIN=-------------------}

main :: IO ()
main = do
    putStrLn "Digite um termo:"
    str <- getLine
    print $ parse unit "<stdin>" str
    putStrLn "Digite o outro termo:"
    str1 <- getLine
    print $ parse unit "<stdin>" str1
    

