done :: Monad m => m ()
done = pure ()

-- recall
putStr1 = foldr (>>) done . map putChar

-- Q: find the meaning of
what = foldl (>>) done . map putChar

-- Obviously,
(>>!) :: Monad m => m a -> m b -> m b
(>>!) ma mb = ma >>= \_ -> mb

-- And so
-- foldl (>>) done . map putChar
-- Here, putChar expects Char, so map putChar expects [Char], which yields [IO ()], and the rest operates on that
-- So we get
-- ((done >> putChar x1) >> putChar x2) >> ... putChar xn
-- So, the new thing does the same thing as the old thing.
-- What does that have to do with the monad laws?
-- done is just the left/right identity for m (), and >> is assocative, so they should be equal.
-- But the laws are in terms of >>= which is a bit different ?_?
-- Actually, I think I would need Applicative laws here. Hm.
--
-- *looking at sample solution*
-- ok, they want me to actually prove that (>>) is associative with identity `done`.
-- Assume the implementation
-- done = return ()
-- Then, for left identity
-- done >> m
-- = return () >> m
-- { where m :: IO () }
-- = return () >>= \_ -> m
-- { the right side ignores its element, so we can feed it () as well }
-- = return () >>= \x -> m
-- { Monad law }
-- = m
--
-- For right identity:
-- m >> done
-- = m >>= \x -> return ()
-- { handwaving, first Monad law, and note that () has only one type... }
-- = m
--
-- What about associativity?
-- (p >> f) >> g
-- { express >> as >>= }
-- = (p >>= \_ -> f) >>= \_ -> g
-- { Monad law }
-- = p >>= (\x -> ((\_ -> f) x >>= (\_ -> g)))
-- { apply inner function, x is thrown away }
-- = p >>= (\_ -> (f >>= (\_ -> g)))
-- { express >>= as >> backwards
-- = p >> (f >> g)
--
-- the sample solution expresses `\_ -> q` as `const q` which makes the proof a bit nicer
