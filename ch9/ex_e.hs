-- Prove: approx n xs <= xs for all n. (i.e. xs is an upper bound for approx n xs)
-- Let's try to prove this using induction on n.
-- IB: n = 0
-- { For any xs: }
-- approx 0 xs
-- { base case exhaustion }
-- = undefined.
-- Obviously, undefined <= xs, even if xs = undefined
--
-- IS: n -> n+1
-- For xs = []
-- approx (n+1) []
-- = []
-- Obviously, [] <= []
--
-- For xs = x:xs'
-- approx (n+1) (x:xs')
-- = x : approx n xs'
-- Since by induction, approx n xs' <= xs', also x : approx n xs' <= x : xs'
--
-- Also the special case for xs = undefined, xs'=undefined would have to be considered.

-- Proof 2:
-- If approx n xs <= ys for all n, then xs <= ys (i.e. xs is the LOWEST upper bound)
-- Assume this is not the case, i.e. xs !<= ys.
-- Then exists an x_m such that
-- x_m !<= y_m
-- This means that xs != undefined and some element in xs is more defined or uncomparable to the element at the same position in ys.
-- Then we can calculate approx m xs and obtain x1:x2: ... : xm : undefined
-- and since x_m !<= y_m, we immediately see that approx m xs !<= ys and we're done.
--
-- the sample solution uses induction here which seems unnecessarily complicated ?_?
