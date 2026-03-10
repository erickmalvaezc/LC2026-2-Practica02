module Practica02 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

--EJERCICIOS

--Ejercicio 1
variables :: Prop -> [String]
variables p = sinRepetidos (vars p)
  where
    vars (Var x) = [x]
    vars (Cons _) = []
    vars (Not x) = vars x
    vars (Or x y) = vars x ++ vars y
    vars (And x y) = vars x ++ vars y
    vars (Impl x y) = vars x ++ vars y
    vars (Syss x y) = vars x ++ vars y

    
elimina :: String -> [String] -> [String]
elimina _ [] = []
elimina x (y:ys)
    | x == y    = elimina x ys
    | otherwise = y : elimina x ys

sinRepetidos :: [String] -> [String]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (elimina x xs)


--Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons b) _ = b
interpretacion (Var x) e = pertenece x e
interpretacion (Not p) e = not (interpretacion p e)
interpretacion (And p q) e = interpretacion p e && interpretacion q e
interpretacion (Or p q) e = interpretacion p e || interpretacion q e
interpretacion (Impl p q) e = not (interpretacion p e) || interpretacion q e
interpretacion (Syss p q) e = interpretacion p e == interpretacion q e

pertenece :: String -> Estado -> Bool
pertenece _ [] = False
pertenece x (y:ys)
    | x == y = True
    | otherwise = pertenece x ys

--Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles x = conjPotencia(variables x)

--Ejercicio 4
modelos :: Prop -> [Estado]
modelos = undefined

--Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes = undefined

--Ejercicio 6 
tautologia :: Prop -> Bool
tautologia = undefined

--Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion = undefined

--Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica = undefined


--Funcion auxiliar
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs
