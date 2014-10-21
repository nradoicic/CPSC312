-- Q1 A
-- myremoveduplicates
--
-- This function returns a list with the same
-- elements as the initial list without duplicate
-- elements.
--
-- Arguments:
-- -- lst: the input list
--
-- Returns: the new list

-- main = print $ myremoveduplicates [3,2,1,3,2,2,1,1]
myremoveduplicates :: Eq a => [a] -> [a]
myremoveduplicates lst
    | null lst = []
    | (head lst) `elem` (tail lst) = myremoveduplicates (tail lst)
    | otherwise = head lst : myremoveduplicates (tail lst)

-- Q1 B
-- main = print $ myremoveduplicates_pm [3,2,1,3,2,2,1,1]
myremoveduplicates_pm :: Eq a => [a] -> [a]
myremoveduplicates_pm [] = []
myremoveduplicates_pm (x:xs)
    | x `elem` xs = myremoveduplicates_pm xs
    | otherwise = x : myremoveduplicates_pm xs


-- Q2 A
-- myintersection
--
-- This function returns a list with all the elements
-- which are common to list one and list 2.  Duplicates
-- are allowed in the return list.
--
-- Arguments:
-- -- lst1, lst2: the input lists
--
-- Returns: the new list

-- main = print $ myintersection [3,4,2,1] [5,4,1,6,2]
-- main = print $ myintersection "abc" "bcd"
myintersection :: Eq a => [a] -> [a] -> [a]
myintersection lst1 lst2
    | null lst1 = []
    | (head lst1) `elem` lst2 = (head lst1) : myintersection (tail lst1) lst2
    | otherwise = myintersection (tail lst1) lst2

-- Q2 B
-- main = print $ myintersection_pm [3,4,2,1] [5,4,1,6,2]
myintersection_pm [] _ = []
myintersection_pm (x:xs) lst2
    | x `elem` lst2 = x : myintersection_pm xs lst2
    | otherwise = myintersection_pm xs lst2


-- Q3 A
-- mynthtail
--
-- This function returns all the elements of a given
-- list after a given index.
--
-- Arguments:
-- -- n : the index
-- -- lst: the input lists
--
-- Returns: the new list

-- main = print $ mynthtail 2 "abcd"
-- main = print $ mynthtail 0 "abcd"
-- main = print $ mynthtail 4 "abcd"
mynthtail :: Eq a => Int -> [a] -> [a]
mynthtail n lst
    | n == 0 = lst
    | otherwise = mynthtail (n-1) (tail lst)

-- Q3 B
-- main = print $ mynthtail_pm 2 "abcd"
-- main = print $ mynthtail_pm 0 "abcd"
-- main = print $ mynthtail_pm 4 "abcd"
mynthtail_pm :: Eq a => Int -> [a] -> [a]
mynthtail_pm 0 lst = lst
mynthtail_pm n (x:xs) = mynthtail_pm (n-1) xs


-- Q4 A
-- mylast
--
-- This function returns a list containing the
-- last element of a given list.
--
-- Arguments:
-- -- lst: the input lists
--
-- Returns: the new list with one element

-- main = print $ mylast "abcd"
-- main = print $ mylast [1,2,3,4]
mylast :: Eq a => [a] -> [a]
mylast lst
    | null lst = []
    | null (tail lst) = [(head lst)]
    | otherwise = mylast (tail lst)

-- Q4 B
-- main = print $ mylast_pm "abcd"
-- main = print $ mylast_pm [1,2,3,4]
mylast_pm :: Eq a => [a] -> [a]
mylast_pm [] = []
mylast_pm (x:xs)
    | null xs = [x]
    | otherwise = mylast_pm xs


-- Q5 A
-- myreverse
--
-- This function returns the same elements
-- of a given list in reverse order
--
-- Arguments:
-- -- lst: the input lists
--
-- Returns: the list in reverse order

-- main = print $ myreverse ""
-- main = print $ myreverse "abc"
-- main = print $ myreverse [1,2,3]
myreverse :: Eq a => [a] -> [a]
myreverse lst
    | null lst = []
    | otherwise = myreverse_helper lst []

myreverse_helper :: Eq a => [a] -> [a] -> [a]
myreverse_helper lst1 lst2
    | null (tail lst1) = (head lst1) : lst2
    | otherwise = myreverse_helper (tail lst1) ((head lst1) : lst2)

-- Q5 B
-- main = print $ myreverse_pm ""
-- main = print $ myreverse_pm "abc"
-- main = print $ myreverse_pm [1,2,3]
myreverse_pm :: Eq a => [a] -> [a]
myreverse_pm [] = []
myreverse_pm lst = myreverse_pm_helper lst []

myreverse_pm_helper :: Eq a => [a] -> [a] -> [a]
myreverse_pm_helper (x:xs) lst2
    | null xs = x : lst2
    | otherwise = myreverse_pm_helper xs (x : lst2)



-- Q6 A
-- myreplaceall
--
-- This function returns a list with the same elements
-- with all of one element (b) replaced with an element
-- (a).
--
-- Arguments:
-- -- a : The element to replace with
-- -- b : The element to replace
--
-- Returns: The new list with replaced elements

-- main = print $ myreplaceall 3 7 [7,0,7,1,7,2,7]
-- main = print $ myreplaceall 'x' 'a' ""
-- main = print $ myreplaceall 'x' 'a' "abacad"
myreplaceall :: Eq a => a -> a -> [a] -> [a]
myreplaceall a b lst
    | null lst = []
    | (head lst) == b = a : myreplaceall a b (tail lst)
    | otherwise = (head lst) : myreplaceall a b (tail lst)

-- Q6 B
-- main = print $ myreplaceall_pm 3 7 [7,0,7,1,7,2,7]
-- main = print $ myreplaceall_pm 'x' 'a' ""
-- main = print $ myreplaceall_pm 'x' 'a' "abacad"
myreplaceall_pm :: Eq a => a -> a -> [a] -> [a]
myreplaceall_pm _ _ [] = []
myreplaceall_pm a b (x:xs)
    | x == b = a : myreplaceall_pm a b xs
    | otherwise = x : myreplaceall_pm a b xs


-- Q7 A
-- myorder
--
-- This function returns True if the elements
-- are in order, and False if they are not
--
-- Arguments:
-- -- lst : The list
--
-- Returns: True or False

-- main = print $ myorder "abcdefg"
-- main = print $ myorder ""
myorder :: Ord a => [a] -> Bool
myorder lst
    | null lst = True
    | null (tail lst) = True
    | otherwise = (head lst) <= (head (tail lst)) && myorder (tail lst)

-- Q7 A (All Boolean)
-- main = print $ myorder_bool "abcdefg"
myorder_bool :: Ord a => [a] -> Bool
myorder_bool lst = null lst || null (tail lst) || ( (head lst) <= (head (tail lst)) && myorder (tail lst))

-- Q7 B
-- main = print $ myorder_pm "abcdefg"
-- main = print $ myorder_pm ""
myorder_pm :: Ord a => [a] -> Bool
myorder_pm [] = True
myorder_pm (x:xs)
    | null xs = True
    | otherwise = x <= (head xs) && myorder_pm xs

-- Q7 B (All Boolean)
-- main = print $ myorder_pm ["abcdefg"]
myorder_pm_bool :: Ord a => [a] -> Bool
myorder_pm_bool [] = True
myorder_pm_bool (x:xs) = null xs || (x <= (head xs) && myorder_pm xs)
