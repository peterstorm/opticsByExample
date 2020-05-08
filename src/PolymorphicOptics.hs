{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, InstanceSigs, LambdaCase #-}

module PolymorphicOptics (main) where

import Control.Lens
import Control.Lens.Unsound (lensProduct)
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- 4.1 Introduction to polymorphic optics


main :: IO ()
main = do
  putStrLn "Hello from PolymorphicOptics.hs"

