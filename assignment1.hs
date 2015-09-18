--Comp 3007 Assignment 1
--Rehnuma Tarannum 100838870

{-We encourage you to ask questions when you're struggling to understand a concept—you can even choose to remain anonymous to your fellow students-}

\x -> \y -> x >> y :: Monad m => m a -> m b -> m b


m n = if n > 100 then n-10 else m( m (n+11) )

--m(17) = 91, m(35) = 91 , m(88) = 91

pre n = n-1
suc n = n+1

add x y = pre x + 1 + suc y - 1

mult x y = (pre x + 1) * (suc y - 1)