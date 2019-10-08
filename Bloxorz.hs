{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}



data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}


data Cell = Cell Char | Switch Bool [Position]
    deriving (Eq,Ord)

instance Show Cell where
    show (Cell c) = (c:[]) 
    show (Switch _ _) = switch :[]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

-- block positon(may be two cells), map of cells, rows cols
data Level = Level [Position] (A.Array Position Cell) Int Int Bool
    deriving (Eq, Ord)

-- instance Ord Level where
--   (Level bl1 arr1 _ _) `compare` (Level bl2 arr2 _ _) = bl1 `compare` bl2
{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

instance Show Level where
    show lvl@(Level bl arr r c moved) = ['\n'] ++ (
        concat [(concat [show (
            if elem (i,j) bl then (Cell block) 
                else (arr A.! (i,j))) | j <- [0..c]]) ++ ['\n'] | 
                    i<-[0..r]]) ++ msg
                        where msg 
                                | wonLevel lvl = "Congrats! You won!\n"
                                | moved && lostLevel lvl = "Game Over\n"
                                | otherwise = []

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel end@(r,c) pos = Level 
    [pos] (A.array ((0,0), end)
            [((i,j), (Cell emptySpace))
                | i <- [0..r], j <- [0..c] ]) r c False
{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile ch pos (Level bl arr r c m) = 
    Level bl (arr A.// [(pos,(Cell tp))]) r c m
      where tp = case ch of
                'H' -> hardTile
                'S' -> softTile
                'W' -> winningTile
                _ -> emptySpace

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos list (Level bl arr r c m) = 
    Level bl (arr A.// [(pos,(Switch False list))]) r c m

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

modLevel :: Level -> [(Position, Cell)]-> Level 
modLevel (Level p arr r c m) modif= Level p 
    (arr A.// modif) r c m

wonLevel :: Level -> Bool
wonLevel lvl@(Level bl _ _ _ _) = case bl of
    [a] -> case (getCell a lvl) of
        (Cell tp) -> if (tp == winningTile) then True else False
        _ -> False
    _ -> False

getType :: Cell -> Char
getType (Cell tp) = tp 
getType (Switch _ _) = switch

lostLevel :: Level -> Bool
lostLevel lvl@(Level bl _ _ _ _) = case bl of
    [a] -> case (getCell a lvl) of
        (Cell tp) -> if (tp == softTile || tp == emptySpace)
                         then True else False
        _ -> False
    [a,b] -> if (getType ((getCell a lvl)) == emptySpace) ||
                (getType ((getCell b lvl)) == emptySpace)
            then True else False
    _ -> False
-- either off table or on soft cell

testSwitch :: Position -> Level -> Level
testSwitch a lvl = case (getCell a lvl) of
        (Switch False p) -> 
            modLevel lvl ((a, (Switch True p)):
                [(t, (Cell hardTile)) | t <- p]) -- do the change
        (Switch True p) -> 
            modLevel lvl ((a, (Switch False p)):
                [(t, (Cell emptySpace)) | t <- p])
        _ -> lvl

-- am modificat tipul argumentului deoarece nu era util sa primesca o Celula
activate :: Level -> Level
activate lvl@(Level bl _ _ _ _) = case bl of
    [a] -> testSwitch a lvl
    [a,b] -> testSwitch a (testSwitch b lvl)
    _ -> lvl

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

modBl :: Level -> [Position] -> Level
modBl (Level _ acc r c _) newBl = (Level newBl acc r c True) 

getCell :: Position -> Level -> Cell
getCell pos (Level _ arr _ _ _) = arr A.! pos

normalise :: [Position] -> (Position,Position) -> [Position]
normalise bl (f@(f1,f2),s@(s1,s2)) = case bl of
    [(a1, a2)]
        | a1 < f1 || a2 < f2 -> [s]
        | a1 > s1 || a2 > s2 -> [f]
        | otherwise -> bl
    [(a1, a2), (b1,b2)] 
        | a1 < f1 || a2 < f2 || b1 < f1 || b2 < f2-> [s]
        | a1 > s1 || a2 > s2 || b1 > s1 || b2 > s2-> [f]
        | otherwise -> bl
    _ -> bl 
 
twoCellMove :: [Position] -> Directions -> [Position]
twoCellMove [(x1,y1),(x2,y2)] dir
    | (x1 == x2) = case dir of -- horizontal
        North ->  [(x1-1,y1),(x2-1,y2)]
        South ->  [(x1+1,y1),(x2+1,y2)]
        West ->  [(x1,(min y1 y2)-1)]
        East ->  [(x1,(max y1 y2)+1)]
    | otherwise = case dir of --verical
        North ->  [((min x1 x2)-1,y1)]
        South ->  [((max x1 x2)+1,y1)]
        West ->  [(x1,y1-1),(x2,y2-1)]
        East ->  [(x1,y1+1),(x2,y2+1)]
twoCellMove bl _ = bl

move :: Directions -> Level -> Level
move dir lvl@(Level pos arr _ _ _) = activate (modBl lvl 
                                    (normalise newBl bnds))
    where 
        bnds = A.bounds arr
        newBl = case pos of
            [(x1,y1)] -> case dir of 
                North -> [(x1-1,y1),(x1-2,y1)]
                South -> [(x1+1,y1),(x1+2,y1)]
                West  -> [(x1,y1-1), (x1, y1-2)]
                East  -> [(x1,y1+1), (x1, y1+2)]
            _ -> twoCellMove pos dir

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame lvl = if wonLevel lvl || lostLevel lvl then False else True

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

distWinPos :: Level -> Int
distWinPos (Level bl arr _ _ _) = res
    where res = minimum (map (distance winPos) bl)
          winPos = (fst.head) $ filter (f.snd) (A.assocs arr) 
          f (Cell ch) = ch == winningTile
          f (Switch _ _) = False

distance :: Position -> Position -> Int
distance (x1,y1) (x2,y2) = (abs.sum) [x2-x1, y2-y1]


instance ProblemState Level Directions where
    successors lvl
        | wonLevel lvl = []
        | otherwise = filter (not . lostLevel . snd) res1
            where res1 = map (\x -> (x, move x lvl)) 
                    [North, South, West, East]

{- 
successors node@(N lvl _ children ad) = filter (not . lostLevel . nodeState . snd) res1
        where res1 = map (\x -> (x, (N (move x lvl) node [] (ad +1)))) 
                    [North, South, West, East]
-}

-- nu vom pastra succesorii unde se pierde
    isGoal = wonLevel

    -- Doar petru BONUS
    heuristic = distWinPos 
