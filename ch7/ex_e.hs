{-
T(1) = O(1) -- actually theta
T(n) = T(n div 2) + T(n - n div 2) + O(n)
for 2 <= n.
Prove T(2^k) = O(k * 2^k)
i.e. T(n) = O(n log n)

I think this function describes build.
This solution was already given in the chapter, and now we're supposed to prove it's true. Hm.

Probably this needs some induction or something.
IB: k=1
T(2^k) = T(2^1) = T(2) = T(2 div 2) + T(2 - 2 div 2) + O(2) = T(1) + T(1) + O(2) = O(1) + O(1) + O(2)
for C1 = 1, C2 = 10, this yields
1 * (1 * 2^1) = 1 <= 4 <= 10 * (1 * 2^1) = 10

IS: k+1
T(2^(k+1)) = T(2^k * 2) = T(2^k) + T(2) + O(2^k)
{ IH }
= O(k * 2^k) + O(4) + O(2^k) = O((k+1) * 2^k) + O(4) 
^ this is off by constant factor 2, so ok.

Definition:
f in O(g) (theta, actually!) iff exist C1,C2>0, n0 : C1 * g(n) <= f(n) <= C2 * g(n)

the sample solution... not sure what it is doing and why t_t. it looks a bit confused ?_?
