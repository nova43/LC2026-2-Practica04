module Practica04 where

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

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [Clausula])
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Show

{-
Coso random de la clase
esUnitaria :: Clausula -> Bool
esUnitaria [x] = True
esUnitaria xs = False

obtenerNombre :: Literal -> String
obtenerNombre (Var x) = x
obtenerNombre (Not (Var x)) = x

tieneInterpretacion :: String -> Interpretacion -> Bool
tieneInterpretacion _ [] = False
tieneInterpretacion x ((y,b):ys) = if x == y
                            then True
                            else tieneInterpretacion x ys
-}

-- =========================
-- AUXILIARES
-- =========================

esUnitaria :: Clausula -> Bool
esUnitaria [_] = True
esUnitaria _   = False

obtenerLiteral :: Clausula -> Literal
obtenerLiteral [x] = x
obtenerLiteral _   = error "No es clausula unitaria"

nombreLiteral :: Literal -> String
nombreLiteral (Var x)     = x
nombreLiteral (Not (Var x)) = x
nombreLiteral _ = error "Literal invalido"

valorLiteral :: Literal -> Bool
valorLiteral (Var _)       = True
valorLiteral (Not _)       = False
valorLiteral _             = error "Literal invalido"

tieneInterpretacion :: String -> Interpretacion -> Bool
tieneInterpretacion _ [] = False
tieneInterpretacion x ((y,_):ys)
    | x == y    = True
    | otherwise = tieneInterpretacion x ys

agregarInterpretacion :: Literal -> Interpretacion -> Interpretacion
agregarInterpretacion l i =
    (nombreLiteral l, valorLiteral l) : i

-- =========================
-- EJERCICIO 1
-- =========================

conflict :: Estado -> Bool
conflict (_, []) = False
conflict (_, c:cs)
    | c == []  = True
    | otherwise = conflict ([], cs)

-- =========================
-- EJERCICIO 2
-- =========================

success :: Estado -> Bool
success (_, []) = True
success _       = False

-- =========================
-- EJERCICIO 3 (UNIT)
-- =========================

unit :: Estado -> Estado
unit (i, []) = (i, [])
unit (i, c:cs)
    | esUnitaria c =
        let l = obtenerLiteral c
        in if tieneInterpretacion (nombreLiteral l) i
           then unit (i, cs)
           else unit (agregarInterpretacion l i, cs)
    | otherwise =
        let (i', cs') = unit (i, cs)
        in (i', c:cs')

-- =========================
-- EJERCICIO 4 (ELIM)
-- elimina clausulas satisfechas
-- =========================

satisfaceLiteral :: Interpretacion -> Literal -> Bool
satisfaceLiteral [] _ = False
satisfaceLiteral ((x,b):xs) l
    | nombreLiteral l == x = valorLiteral l == b
    | otherwise = satisfaceLiteral xs l

satisfaceClausula :: Interpretacion -> Clausula -> Bool
satisfaceClausula i = any (satisfaceLiteral i)

elim :: Estado -> Estado
elim (i, cs) = (i, filter (not . satisfaceClausula i) cs)

-- =========================
-- EJERCICIO 5 (RED)
-- elimina literales falsos dentro de clausulas
-- =========================

literalFalso :: Interpretacion -> Literal -> Bool
literalFalso [] _ = False
literalFalso ((x,b):xs) l
    | nombreLiteral l == x = valorLiteral l /= b
    | otherwise = literalFalso xs l

reducirClausula :: Interpretacion -> Clausula -> Clausula
reducirClausula i = filter (not . literalFalso i)

red :: Estado -> Estado
red (i, cs) = (i, map (reducirClausula i) cs)

-- =========================
-- EJERCICIO 6 (SEP)
-- =========================

negar :: Literal -> Literal
negar (Var x)     = Not (Var x)
negar (Not (Var x)) = Var x
negar _ = error "Literal invalido"

sep :: Literal -> Estado -> (Estado, Estado)
sep l (i, cs) =
    ( (agregarInterpretacion l i, cs)
    , (agregarInterpretacion (negar l) i, cs)
    )

--IMPLEMENTACION PARTE 2


--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral = undefined

--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll = undefined

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 = undefined