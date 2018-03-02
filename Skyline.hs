module Skyline where

-- Cabecera del programa Skyline.hs
-- Práctica de Teoría de los Lenguajes de Programación
-- Curso 2015-2016

type Edificio = (Int,Int,Int)
type Coordenada = (Int,Int)
type Skyline = [Coordenada]

--------------------------------------------------------------------------
--Funciones de la practica
--------------------------------------------------------------------------

resuelveSkyline::[Edificio]->Skyline
resuelveSkyline [] = []
resuelveSkyline [(r1,r2,r3)] = edificioAskyline (r1,r2,r3)
resuelveSkyline l = combina (resuelveSkyline(fst(divide l))) (resuelveSkyline(snd(divide l)))

edificioAskyline::Edificio->Skyline
edificioAskyline (x,y,z) = [(x,z),(y,0)]

divide::[Edificio]->([Edificio],[Edificio])
divide [] = ([],[])
divide [e] = ([e],[])
divide (x:y:z) = (x:xs,y:ys) where (xs,ys) = divide z

combina::Skyline->Skyline->Skyline
combina [] [] = []
combina (x1:x2) [] = (x1:x2)
combina [] (y1:y2) = (y1:y2)
combina z1 z2 = combinaAux z1 z2 0 0 0
    where 
        combinaAux ([]) (cd) _ _ _= cd
        combinaAux (ab) ([]) _ _ _ = ab
        combinaAux ((a,b):ab) ((c,d):cd) ux uy m 
            | (a == c)&&(max b d == m) = combinaAux (ab) (cd) b d m
            | (a == c)&&(max b d /= m) = (a, max b d) : combinaAux (ab) (cd) b d (max b d)
            | (a < c) &&(max b uy == m) = combinaAux (ab) ((c,d):cd) b uy m
            | (a < c) &&(max b uy /= m) = (a, max b uy) : combinaAux (ab) ((c,d):cd) b uy (max b uy)
            | (a > c) &&(max d ux == m) = combinaAux ((a,b):ab) (cd) ux d m
            | (a > c) &&(max d ux /= m) = (c, max d ux) : combinaAux ((a,b):ab) (cd) ux d (max d ux)
            | otherwise = [(0,0)]

-- Parte opcional

dibujaSkyline::Skyline->String
dibujaSkyline s = dibujaSkylineAux (listaAltura s) (maximum (listaAltura s)) (listaAltura s)
    where
        dibujaSkylineAux lista iy aux
            | (lista == []) && (iy /= 0) = "\n" ++ dibujaSkylineAux aux (iy-1) aux
            | (lista == []) && (iy == 0) = ""
            | ((head lista)>=iy) && (iy /=0)= "*" ++ dibujaSkylineAux (tail lista) iy aux
            | ((head lista)<iy) && (iy /=0) = " " ++ dibujaSkylineAux (tail lista) iy aux
            | otherwise = "-" ++ dibujaSkylineAux (tail lista) iy []

listaAltura::Skyline->[Int]
listaAltura lst = listaAlturaAux lst 0 0
    where 
        listaAlturaAux [] _ _ = []
        listaAlturaAux lst i alx
            | i<(fst(head lst)) = alx:listaAlturaAux lst (i+1) alx
            | i==(fst(head lst)) = (snd(head lst)):listaAlturaAux (tail lst) (i+1) (snd(head lst))
    
