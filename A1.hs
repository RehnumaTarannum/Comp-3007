-- Question 1
-- "We encourage you to ask questions when you're struggling to
-- understand a concept—you can even choose to remain anonymous to
-- your fellow students."


-- Question 2
-- \x-> \y -> x>>y :: Monad m => m a -> m b -> m b

-- Question 3
m n = if n>100 then n - 10 else m(m(n+11))


-- Question 4

pre n = n-1

suc n = n+1

add x y =
  if x==0
  then y
  else succ $ add (pred x) y

mult x y =
  if x==0
  then 0
  else if x==1
       then y
       else add y $ mult (pred x) y

