-- the first parser parses leading whitespace twice if the thing doesn't start with a '-'
-- the second parser has exactly the same problem.
-- the third parses whitespace first and never has to backtrack.