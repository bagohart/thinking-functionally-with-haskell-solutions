-- build []
-- ^ what does it do?
-- build []
-- = fst (build2 (length []) [])
-- = fst (build2 0 [])

-- Using the old definition of build2:
-- build2 0 []
-- = ... where (u,xs') = build2 0 []
-- so this runs into infinite recursion. uh oh.
--
-- Use the state-based (new) definition of build2:
-- build2 0 []
-- = do u <- build2 0
--      ...
--  = build2 0 >>= \u -> ...
--  this runs into infinite recursion, too.
--
-- Its value, therefore, is bottom.