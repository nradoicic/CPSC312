-- Q1
-- main = print $ preparednessQuotient 1 2 1 1 5 6 7
preparednessQuotient :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
preparednessQuotient i s c pn pa d n = (8 * pa * (s + c)) / (3 * pn * (d + n + i))

-- Q2
-- main = print $ kungPaoFactor 1 2 1 1 5 6 300 8
-- main = print $ kungPaoFactor 1 1 1 1 1 1 1 1
kungPaoFactor :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
kungPaoFactor r dm ds n c ft ff s = (n/30 - ds/dm) + (10 * s ^ 2 * sqrt r) / (c * (ft - ff + 1))

-- Q3
-- main = print $ multiply 0 0
-- main = print $ multiply 5 3
multiply :: Int -> Int -> Int
multiply 0 _ = 0
multiply x y = y + multiply (x-1) y

-- Q4
-- main = print $ multiply_tr 3 4
multiply_tr :: Int -> Int -> Int
multiply_tr 0 _ = 0
multiply_tr x y = y + multiply_tr_inner (x-1) y 0

multiply_tr_inner :: Int -> Int -> Int -> Int
multiply_tr_inner 0 _ sum = sum
multiply_tr_inner x y sum = multiply_tr_inner (x-1) y (sum+y)

-- Q5
-- main = print $ power 3 4
power :: Int -> Int -> Int
power _ 0 = 1
power 0 _ = 0
power x y = x `multiply` power x (y-1)

-- Q6
-- main = print $ power_tr 2 10
power_tr :: Int -> Int -> Int
power_tr 0 _ = 0
power_tr x y = power_tr_inner x y 1

power_tr_inner :: Int -> Int -> Int -> Int
power_tr_inner _ 0 product = product
power_tr_inner x y product = power_tr_inner x (y-1) product `multiply_tr` x

-- Q7
-- main = print $ harmonic 4
harmonic :: Int -> Float
harmonic 1 = 1
harmonic n = 1 / fromIntegral n + harmonic (n-1)

-- Q8
-- main = print $ harmonic 4
harmonic_tr :: Int -> Float
harmonic_tr 1 = 1
harmonic_tr n = harmonic_tr_inner n 0

harmonic_tr_inner :: Int -> Float -> Float
harmonic_tr_inner 1 sum = sum
harmonic_tr_inner n sum = harmonic_tr_inner (n-1) ( 1 / fromIntegral n + sum)
