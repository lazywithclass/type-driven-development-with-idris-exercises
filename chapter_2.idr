module Main

-- (String, String, String)
-- ("A", "B", "C")

-- List String
-- ["A", "B", "C"]

-- ((Char, String), Char)
-- (('A', "B"), 'C')

palindrome : Nat -> String -> Bool
palindrome limit x = if (length x) > limit
                     then toLower x == reverse (toLower x)
                     else False

counts : String -> (Nat, Nat)
counts x = (length (words x), length x)

top_ten : Ord a => List a -> List a
top_ten xs = List.take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length threshold xs = List.length (filter (\x => length x > threshold) xs)

main : IO ()
main = repl "Enter a string: " (\x => show (palindrome 1 x) ++ "\n")

