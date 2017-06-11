import Data.Foldable (fold)

-- A function for defining arbitrary forms of "recursive recursion" over some
-- context, according to the computational modes afforded in that context.
moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
moeb f x = go where go = f ($ go) x

-- The base case of the specialization of moeb, allowing the collapsing of
-- a function into a single value.
--
-- For reference, the type of id is:
--     id :: a -> a
fix :: (a -> a) -> a
fix = moeb id

-- The next case I've so far gotten to work (not sure if there's a constructive
-- procedure for successive specializations of `moeb`, but I think there might
-- be. If there is, there may be a case between `fix` and `loeb`.
--
-- For reference, the type of fmap is:
--     fmap :: Functor f => (a -> b) -> f a -> f b
loeb :: (Functor f) => f (f a -> a) -> f a
loeb = moeb fmap

-- This is probably the "next" specialization of `moeb` that is possible,
-- unless there's some way to get specialization on `<*>` to work, which
-- I doubt. Do note that `=<<` is used over `>>=` because we need the
-- function input to be the first input.
--
-- I am unsure if there's any additional "power" added by the transition
-- to a monadic context. So far, I've not worked out any processing I can
-- do with "monadic moeb" but not with "functorial moeb."
--
-- For reference, the type of `=<<` is:
--     =<< :: Monad f => (a -> f b) -> f a -> f b
noeb :: (Monad m) => m (m a -> m a) -> m a
noeb = moeb (=<<)

-- Not sure where this falls in the hierarchy. This seems to be the "next tier
-- up" from `loeb` and `noeb`, and I doubt that there exists a specialization
-- of `moeb` between `toeb` and `noeb`.
--
-- I have not yet played around with testing this function.
--
-- For reference, the type of `traverse` is:
--     traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
toeb :: (Applicative a, Traversable t) => t (a (t x) -> a x) -> a (t x)
toeb = moeb traverse

-- These two don't seem to work. Probably because of the outer context wrapping
-- in the first argument of their input function type.
--
-- poeb :: (f a -> a -> b) -> f a -> f b
-- poeb = moeb (<*>)
--
-- foeb :: (Monoid m, Foldable f) => f (f m -> m) -> f m
-- foeb = moeb fold

