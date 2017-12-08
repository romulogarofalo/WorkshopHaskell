module Pizza where

data Pergunta = Sim | Nao

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0


parImpar :: Int -> String
parImpar x = if mod x 2 == 0 then "par" else "impar"

data OS = Windows | OSX  | Ubuntu deriving (Eq)


compara::OS -> OS -> Bool
compara x f = x == f

data Tree a = Null | Leaf a | Branch a (Tree a) (Tree a) deriving Show

-- E R D
emOrdem :: Tree a -> [a]
emOrdem Null = []
emOrdem (Leaf x) = [x]
emOrdem (Branch r e d) = emOrdem e ++ [r] ++ emOrdem d

foo:: Tree Int
foo = Branch 4 (Branch 2 (Leaf 1) (Leaf 2)) (Leaf 9)

inserir :: Ord a => Tree a -> a -> Tree a
inserir (Leaf x) c 
    | x > c = (Branch x (Leaf c) Null)
    | otherwise = (Branch x Null (Leaf c))
inserir Null c = (Leaf c)
inserir (Branch x l r) c
    | x > c = (Branch x (inserir l c) r)
    | otherwise = (Branch x l (inserir r c))

