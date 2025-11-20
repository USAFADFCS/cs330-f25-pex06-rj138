-- pex6.hs 
-- unKnot Haskell

-- name: RyalJames Weldon-Carroll

{- DOCUMENTATION:
I used this website to learn how to pull out the frist two things out of a list using pattern matching
https://en.wikibooks.org/wiki/Haskell/Pattern_matching
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "not a knot"
   -- call the typeIknot function to test for knots
   | null (typeIknot tripCode []) = "not a knot"
   | otherwise = "tangle - resulting trip code: " ++ (show (typeIknot tripCode []))


-- Called typeIknot but deals with all knot types
typeIknot :: [(Char, Char)] -> [(Char,Char)] -> [(Char,Char)]
-- creating a base
typeIknot [] lit= lit
-- second bse in case list is only 1 long
typeIknot [x] lit= lit ++ [x]
-- check if there is type 1 or type 2, if there is type 2 call helper func killer to remove any
-- secondary instances of that intersection
typeIknot (x:y:xs) lit = 
   if fst x == fst y || snd x == snd y
   then typeIknot (killer xs (fst x) (fst y) []) lit 
   else typeIknot (y:xs) (lit ++ [x])



-- helper function made to get ride of any typles that have the given variables in the x position
-- return list of tuples
killer :: [(Char, Char)] -> Char -> Char->  [(Char,Char)] ->  [(Char,Char)]
-- creating a base
killer [] a b lit = lit
-- general recursive case to check if the tuple matches the varailbe and react accordingly
killer (x:xs) y b lit = 
   if fst x == y || fst x == b
   then killer xs y b lit
   else killer xs y b (lit ++ [x])



-- typeIknot tripCode

main :: IO ()
main = do
   let t01 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:                                    " ++ unKnot t01)

