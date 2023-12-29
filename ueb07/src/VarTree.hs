{-# LANGUAGE InstanceSigs #-}

module VarTree where

data VarTree a = VarTree a [VarTree a] deriving (Show, Eq)

instance Functor VarTree where
  fmap :: (a -> b) -> VarTree a -> VarTree b
  fmap f (VarTree a children) = VarTree (f a) (map (fmap f) children)

{-- Beispiel:
foldVarTree (λx ys → x + sum ys) (VarTree 1 [VarTree 2 [], VarTree 3 []]) ⇝ 6
--}
foldVarTree :: (a -> [b] -> b) -> VarTree a -> b
foldVarTree f (VarTree a children) = f a (map (foldVarTree f) children)

-- use https://graphviz.org/download/ to create a picture from this
toDot :: (Show a) => VarTree a -> String
toDot tree = "digraph {\n" ++ showConnections tree ++ "}"

showConnections :: (Show a) => VarTree a -> String
showConnections = concatMap showConnection . connections
  where
    showConnection (a, b) = show a ++ " -> " ++ show b ++ "\n"

connections :: VarTree a -> [(a, a)]
connections = snd . foldVarTree f
  where
    f :: a -> [(a, [(a, a)])] -> (a, [(a, a)])
    f node children = (node, concatMap (\(child, childconnections) -> (node, child) : childconnections) children)

writeDot :: (Show a) => FilePath -> VarTree a -> IO ()
writeDot path tree = writeFile path $ toDot tree
