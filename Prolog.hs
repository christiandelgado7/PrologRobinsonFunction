module Prolog where

-- DEFINICION DE TIPOS DE DATOS

data Value = I Int 
                | B Bool 
                | F Float 
                | C Char 
                deriving (Eq,Show)

data Term = Const Value
          | Var String
          | Fun String [Term]
            deriving (Eq,Show)

type Atom = (String,[Term])

type Tail = [Atom]

data Question = Q Tail 
                deriving (Eq,Show)

data Definite = Fact Atom
              | Rule Atom Tail
              deriving (Eq,Show)

data Program = Prolog [Definite] 
                deriving (Eq,Show)

data Subs = S [(Term,Term)] 
                | E String 
                deriving (Eq,Show)

-- FUNCIONES

-- Sustitucion

sustitucion :: Atom -> Subs -> Atom
sustitucion atomo (E _) = atomo
sustitucion (nom,ts) (S ss) = (nom,map (sustit (S ss)) ts)
 
-- función que resuelve a nivel de términos
 
sustit :: Subs -> Term -> Term
sustit (S []) y = Var "no"
sustit (S ((x,z):xs)) y = if (x==y) then z
                                    else sustit (S xs) y
-- Robinson

robinson :: Atom -> Atom -> Subs
robinson (nom,ts) (mon,st)
                        | (nom /= mon)              = E "predicados distintos"
                        | (length ts /= length st)  = E "aridades distintas"
                        | otherwise                 = alan (pares ts st) 

-- función que despliega los casos del algoritmo (Alan es el nombre de Robinson)
alan :: [(Term,Term)] -> Subs

alan (((Const x) , (Const y )):terms) 
                | x/=y                                          = E "constantes distintas"


alan (((Fun f xs),(Fun g ys)):terms)
        | (f/=g)                    = E "funciones distintas"
        | (length xs /= length ys)  = E "aridades de funciones distintas"
        | otherwise                 = alan ((pares xs ys) ++ terms)

alan (((Const x),(Fun g ys)):terms) = E "incompatiblidad"

alan (((Fun g ys),(Const x)):terms) = E "incompatiblidad"

alan (((Var x),(Fun g ys)):terms)
        | occurs_check (Var x) (Fun g ys)                    = E "funciones distintas"

alan (((Fun g ys),(Var x)):terms)
        | occurs_check (Fun g ys) (Var x)                    = E "funciones distintas"

alan (((Var x),(Const y)):terms)
    = catsub (S [((Var x),(Const y))]) (alan (map (sust_par (S [((Var x),(Const y))])) terms))

alan (((Var x),(Fun g ys)):terms)
    = catsub (S [((Var x),(Fun g ys))]) (alan (map (sust_par (S [((Var x),(Fun g ys))])) terms))

alan (((Const y),(Var x)):terms)
    = catsub (S [((Const y),(Var x))]) (alan (map (sust_par (S [((Var x),(Const y))])) terms))

alan (((Fun g ys),(Var y)):terms)
    = catsub (S [((Fun g ys),(Var y))]) (alan (map (sust_par (S [((Var y),(Fun g ys))])) terms))

alan (((Var x),(Var y)):terms)
    = catsub (S [((Var x),(Var y))]) (alan (map (sust_par (S [((Var x),(Var y))])) terms))

sust_par :: Subs -> (Term,Term) -> (Term,Term)
sust_par ss (t1,t2) = (sustit ss t1,sustit ss t2)


catsub :: Subs -> Subs -> Subs
catsub (E e1) (E e2) = E (e1 ++ " y " ++ e2)
catsub (E e) _  = (E e)
catsub _ (E e)  = (E e)
catsub (S ts) (S st) = S (ts ++ st)

occurs_check :: Term -> Term -> Bool
occurs_check (Var x) (Fun m rs) = pertenece (Var x) rs
occurs_check (Fun m rs) (Var x) = occurs_check (Var x) (Fun m rs)
occurs_check _ _                = False

pertenece :: Term -> [Term] -> Bool
pertenece t []                    = False
pertenece (Var x) ((Var y):rs)    = if x==y then True
                                    else False
pertenece (Var x) ((Fun f ts):rs) = (or (map (occurs_check (Var x)) ts))
--pertenece (Var x) (x:rs) = False
--pertenece _ _                     = False
 
--empilar
pares :: [Term] -> [Term] -> [(Term,Term)]
pares [] [] = []
pares (t:ts) (r:rs) = (t,r) : pares ts rs 

-- Resolución
--resolucion :: Question -> Program -> Subs

-- EJEMPLOS PARA CHEQUEAR LAS FUNCIONES
-- ejemplos de data

app :: Program
app = Prolog [Fact ("append",[Fun "lista" [],Var "L",Var "L"]),
              Rule ("append",[Fun "lista" [Var "A",Var"X"],
                          Var "Y",
                          Fun "lista" [Var "A",Var "Z"]])  [("append",[Var "X",Var "Y",Var "Z"])]]
                          
query :: Question
query = Q [("append", [Fun "lista" [Var "Head",Var "Tail1"],Var "Tail2",Fun "lista" [Const (C 'a'),Fun "list" []]])]

-- ejemplo para la funci?n sustituci?n

atomo :: Atom
atomo = ("append",[Fun "lista" [Var "A",Var"X"],Var "Y",Fun "lista" [Var "A",Var "Z"]])


sust_ejem :: Subs
sust_ejem = S [(Var "A",Const (C 'a')),(Var "X",Fun "lista" [])]

-- ejemplo para la funci?n robinson

atomo1 :: Atom
atomo1 = ("append",[Fun "lista" [Var "A",Var"X"],Var "Y",Fun "lista" [Var "A",Var "Z"]])

atomo2 :: Atom
atomo2 = ("append",[Fun "lista" [Var "Head",Var "Tail1"],Var "Tail2",Fun "lista" [Const (C 'a'),Fun "lista" []]])
