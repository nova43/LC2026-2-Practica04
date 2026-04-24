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
=========================
AUXILIARES
=========================
-}

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


{-
=========================
   EJERCICIO 1
=========================
-}

conflict :: Estado -> Bool
conflict (_, []) = False
conflict (_, x:xs) = if x == []
                    then True
                    else conflict (([], xs))

{-
=========================
   EJERCICIO 1
=========================
-}

success :: Estado -> Bool
success (_, []) = True
success _       = False

{-
=========================
  EJERCICIO 3 
=========================
-}

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

{-
=========================
  EJERCICIO 4 
elimina clausulas satisfechas
=========================
-}

satisfaceLiteral :: Interpretacion -> Literal -> Bool
satisfaceLiteral [] _ = False
satisfaceLiteral ((x,b):xs) l
    | nombreLiteral l == x = valorLiteral l == b
    | otherwise = satisfaceLiteral xs l

satisfaceClausula :: Interpretacion -> Clausula -> Bool
satisfaceClausula i = any (satisfaceLiteral i)

elim :: Estado -> Estado
elim (i, cs) = (i, filter (not . satisfaceClausula i) cs)

{-
=========================
  EJERCICIO 5 (RED)
elimina literales falsos dentro de clausulas
=========================
-}

literalFalso :: Interpretacion -> Literal -> Bool
literalFalso [] _ = False
literalFalso ((x,b):xs) l
    | nombreLiteral l == x = valorLiteral l /= b
    | otherwise = literalFalso xs l

reducirClausula :: Interpretacion -> Clausula -> Clausula
reducirClausula i = filter (not . literalFalso i)

red :: Estado -> Estado
red (i, cs) = (i, map (reducirClausula i) cs)

{-
=========================
   EJERCICIO 6 (SEP)
=========================
-}

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

{-
=========================
   EJERCICIO 1
=========================
-}

contarLiteral :: Literal -> Clausula -> Int
contarLiteral _ [] = 0
contarLiteral l (x:xs)
    | l == x    = 1 + contarLiteral l xs
    | otherwise = contarLiteral l xs

aparicionesLiteral :: Literal -> [Clausula] -> Int
aparicionesLiteral _ [] = 0
aparicionesLiteral l (c:cs) = contarLiteral l c + aparicionesLiteral l cs

obtenerLiterales :: [Clausula] -> [Literal]
obtenerLiterales [] = []
obtenerLiterales (c:cs) = c ++ obtenerLiterales cs

mejorLiteral :: [Clausula] -> Literal -> Literal -> Literal
mejorLiteral cs l1 l2 =
    if aparicionesLiteral l1 cs >= aparicionesLiteral l2 cs then l1 else l2

heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral cs = foldr1 (mejorLiteral cs) (quitarRepetidos (obtenerLiterales cs))

{-
=========================
   EJERCICIO 2
=========================
-}

probarCasos :: Literal -> Estado -> Interpretacion
probarCasos l (interp, claus) =
    let ((i1, c1), (i2, c2)) = sep l (interp, claus)
        res1 = dpllAux (red (elim (i1, c1)))
        in if not (null res1) then res1 else dpllAux (red (elim (i2, c2)))

dpllAux :: Estado -> Interpretacion
dpllAux (interp, clausulasPend)
    | conflict (interp, clausulasPend) = []
    | success (interp, clausulasPend)  = interp
    | otherwise =
        let (nuevaInterp, nuevasClausulasPend) = unit (interp, clausulasPend)
        in if (nuevaInterp, nuevasClausulasPend) /= (interp, clausulasPend)
           then dpllAux (red (elim (nuevaInterp, nuevasClausulasPend)))
           else probarCasos (heuristicsLiteral clausulasPend) (interp, clausulasPend)

dpll :: [Clausula] -> Interpretacion
dpll c = dpllAux ([], c)

{-
=========================
   EXTRA
=========================
-}

dpll2 :: Prop -> Interpretacion
dpll2 f = dpll (clausulas (fnc f))

--Codigo de la practica 3
fnn :: Prop -> Prop
fnn (Cons x) = Cons x
fnn (Var p1) = Var p1
fnn (Not (Not f1)) = fnn f1
fnn (Not (And f1 f2)) = fnn (Or (fnn (Not f1)) (fnn (Not f2)))
fnn (Not (Or f1 f2)) = fnn (And (fnn (Not f1)) (fnn (Not f2)))
fnn (Impl f1 f2) = fnn (Or (Not f1) f2)
fnn (Syss f1 f2) = fnn (And (Impl f1 f2) (Impl f2 f1))
fnn (And f1 f2) = And (fnn f1) (fnn f2)
fnn (Or f1 f2) = Or (fnn f1) (fnn f2)
fnn (Not f1) = Not (fnn f1)

fnc :: Prop -> Prop
fnc prop = fncAux (fnn prop)

fncAux :: Prop -> Prop
fncAux (And a b) = And (fncAux a) (fncAux b)
fncAux (Or a b) = distribuir (fncAux a) (fncAux b)
fncAux x = x

distribuir :: Prop -> Prop -> Prop
distribuir f1 (And f2 f3) = And (distribuir f1 f2) (distribuir f1 f3)
distribuir (And f1 f2) f3 = And (distribuir f1 f3) (distribuir f2 f3)
distribuir f1 f2 = Or f1 f2

clausulas :: Prop -> [Clausula]
clausulas (Cons x) = [[]]
clausulas (Var p) = [[Var p]]
clausulas (Not p) = [[Not p]]
clausulas (Or p q) = [clausulasAux (Or p q)]
clausulas (And p q) = clausulas p ++ clausulas q

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

clausulasAux :: Prop -> Clausula
clausulasAux (Cons _) = []
clausulasAux (Var p) = [Var p]
clausulasAux (Not p) = [Not p]
clausulasAux (Or f1 f2) = quitarRepetidos (clausulasAux(f1) ++ clausulasAux (f2))
clausulasAux x = []

quitarRepetidos :: Eq a => [a] -> [a]
quitarRepetidos [] = []
quitarRepetidos (x:xs)
    | pertenece x xs = quitarRepetidos xs
    | otherwise = x : quitarRepetidos xs