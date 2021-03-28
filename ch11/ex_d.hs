-- Alternative definition for <|>s
-- p <|> q = parser (\s -> parse p s ++ parse q s)
-- (this seems broken, should be apply instead of parse... did the author even test any of the solutions for this chapter?)
-- Recall the old definition:
-- (Parser p) <|> (Parser q) = Parser $ \s -> let ps = p s in
--                                            if null ps then q s else ps

-- The old definiton meant that always only the result(s) of one parser would be used.
-- And the result of the first parser would be preferred and then the second one wouldn't even be checked.
-- The new definition always tries to use both parsers.
-- It still uses the first parser first, but that doesn't actually make any difference because both are started
-- on the same state (s).
--
-- Is that a reasonable alternative definition?
-- Sorta, it gets all possible parses, so this is just a different thing.
-- When is it deterministic?
-- deterministic means it gets 0 or 1 results always.
-- Obviously, both p and q have to be deterministic.
-- Also, there must not be any s, for which both p and q get one result.
-- So the "intersection" between the parsers must be empty.
--
-- ... According to the sample solution, p or q must be fail to make the whole thing deterministic.
-- That seems Just Wrong.
--
-- How can I define a function limit :: Parser a -> Parser a
-- s.t. limit (p <|> q) is a deterministic parser?
-- this seems obvious, just take the first element and don't botch the empty list case:
-- limit (Parser p) = Parser \s -> op (p s)
--                      where op [] = []
--                            op (x:xs) = [x]
--
-- the sample solution uses take 1. That... could have been obvious to me xD