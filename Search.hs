{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S

import qualified Data.List as L

import qualified Data.Maybe as M

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

-- starea(nivelul) crt, actiune, parinte, copii, adancime
data Node s a = N s a (Node s a) [Node s a] Int | Null

instance (Show s) => Show (Node s a) where
    show Null = "X"
    show (N lvl _ _ ch depth) = (show lvl)

instance (Eq s ) => Eq (Node s a) where
  (N lvl1 _ _ _ _) == (N lvl2 _ _ _ _) = lvl1 == lvl2 
  _ == Null = undefined
  Null == _  = undefined

instance (Ord s ) => Ord (Node s a) where
  (N lvl1 _ _ _ _) `compare` (N lvl2 _ _ _ _) = lvl1 `compare` lvl2
  compare Null _ = undefined
  compare _ Null = undefined

--  trebuie scrisa functia <

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState (N state _ _ _ _) = state
nodeState Null = undefined

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

notNull :: Node s a -> Bool
notNull Null = False
notNull _ = True

-- cat timp mai am succesori pt fiecare succesor fac chestia asta 
createStateSpace :: ((Ord s),(ProblemState s a)) => s -> Node s a
createStateSpace lvl = f (N lvl act Null [] 0) S.empty
    where 
      act = fst $ head (successors lvl)
      f Null _ = undefined
      f node@(N old a par _ depth) viz = N old a par newCh depth
        where
          newCh = case nodeSucc of
            [] -> [] 
            _ -> filter notNull $ map (`f` newViz) nodeSucc
          nodeSucc = filter rmDup listAllCh 
          rmDup (N l _ _ _ _) = (not . (S.member l)) viz 
          rmDup Null = False
          listAllCh = map createSucc (successors old)
          newViz = if (not . (S.member old)) viz 
            then S.insert old viz 
            else viz -- don't add
          createSucc (k, succs) = N succs k node [] (depth+1)

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace (N lvl a par ch depth) =
    case ch of
    [] -> (N lvl a par ch depth)
    _ -> (N lvl a par (map orderStateSpace newCh) depth)
    where sortingFunction (N lvl1 _ _ _ _) (N lvl2 _ _ _ _) = 
            compare (heuristic lvl1) (heuristic lvl2)
          sortingFunction _ _ = undefined
          newCh = (L.sortBy sortingFunction ch)
orderStateSpace Null = undefined
{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}
pop :: [Node s a] -> [Node s a] 
pop = tail
top :: [Node s a] -> Node s a 
top = head

limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs Null _ = []
limitedDfs node height = aux [node] S.empty
    where aux stack set
            | null stack = []
            | otherwise = crt : (aux newStack newSet)
                where 
                    crt@(N _ _ _ ch depth) = top stack
                    newSet = if (not . (S.member crt)) set
                        then S.insert crt set 
                        else set
                    newStack = newNodes ++ (pop stack)
                    newNodes 
                        | depth == height = []
                        | otherwise = filter (\x -> 
                            (not . (S.member x)) set) ch
                    -- pt fiecare din ch daca nu e in set il pun pe stiva
-- starea(nivelul) crt,actiune, parinte, copii, adancime
{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}


iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
   -- nodul are deja createStateSpace facut pe el >:(                     
iterativeDeepening node = (first, states)
    where
        hs = [0..]
        first@(N _ _ _ _ depth) = head $ head $ filter (not.null) (map f hs)
        f x = filter (isGoal.nodeState) (limitedDfs node x)
        states = sum $ map tr $ take (depth+1) $ (map g hs)
        tr (nr, fnd)
            | M.isJust fnd = M.fromJust fnd
            | otherwise = nr
        g x = (length path,L.elemIndex first path)
            where path = (limitedDfs node x)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath :: Node s a -> [(a, s)]
extractPath Null = []
extractPath node@(N _ _ _ _ depth) = reverse $ map modify list
        where list = take depth $ iterate f node
              f (N _ _ par _ _) = par
              f Null = undefined
              modify (N lvl a _ _ _) = (a, lvl) 
              modify Null = undefined

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve _ True = undefined
solve s False = extractPath solution
    where solution = fst (iterativeDeepening (createStateSpace s)) 

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))