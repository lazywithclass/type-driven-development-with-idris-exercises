module Main

-- Different types:

-- enumerated - defined giving the possible values directly
data Bool = False | True

-- union - enumerated that carry additional data with each values
data Color = Blue | White | Red

-- recursive -
data Nat = Z | S Main.Nat

-- generic - parametrized over some other types
-- dependant - computed from some other value


-- data Tree elem = Empty
--                | Node (Tree elem) elem (Tree elem)

data BSTree : Type -> Type where
     Empty : Ord elem => BSTree elem
     Node : Ord elem => (left : BSTree elem) -> (val : elem) ->
                        (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
-- orig is a named pattern
insert x orig@(Node left val right)
       = case compare x val of
              LT => Node (insert x left) val right
              EQ => orig
              GT => Node left val (insert x right)

listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Ord a => BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) =
           treeToList left ++ val :: treeToList right

-- evaluating treeToList (listToTree [4,1,8,7,2,3,9,5,6])
-- gives [1, 2, 3, 4, 5, 6, 7, 8, 9]

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

-- evaluate (Mult (Val 10) (Add (Val 6) (Val 3)))
-- gives 90

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = if x > y then Just x else Just y
-- maxMaybe (Just 4) (Just 5)
-- gives 5
-- maxMaybe (Just 4) Nothing
-- gives 4
