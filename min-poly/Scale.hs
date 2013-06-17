{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Scale where

class Scale s a | a -> s where
  scale :: s -> a -> a

-- instance Num a => Scale a a where
--   scale = (*)


