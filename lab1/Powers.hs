-- PART 0 (lecture implementation)
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- PART 1
{-
The above implementation uses n + 1 computing steps.
-}

-- PART 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product (replicate (fromInteger k) n)

-- PART 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n*n) (k `div` 2)
power2 n k | odd k = n * power2 (n*n) ((k-1) `div` 2)

-- PART 4
{-
Test cases: 
-- n = 3  k = 0     -- a valid input (base case)
-- n = 2  k = 4     -- a valid input with even k
-- n = 2  k = 3     -- a valid input with odd k
-}

prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k &&
                  power n k == power2 n k &&
                  power1 n k == power2 n k

test_powers = and [prop_powers 3 0, prop_powers 2 4, prop_powers 2 3]

prop_powers' :: Integer -> Integer -> Bool  -- passes QuickTest's tests
prop_powers' n k = prop_powers n (abs k)
