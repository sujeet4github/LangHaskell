{-

MonadPlus = Monads that are also Monoids
i.e monads with a zero element and a plus operation

class (Monad m) => MonadPlus where
	mzero	:: m a
	mplus	:: m a -> m a -> m a

The following laws apply for mzero:
	m >>= \x -> mzero = mzero
	mzero >>= m = mzero

	m 'mplus' mzero = m
	mzero 'mplus' m = m
-}
