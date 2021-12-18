import Math.NumberTheory.Primes

noob b c = if b == c then 1 else (if isPrime b then 0 else 1) + (noob (b+17) c)

main =
    let b = (81 * 100) + 100000
        c = b + 17000
    in print $ noob b c -- length $ dropWhile (<b) $ takeWhile (<c) primes
