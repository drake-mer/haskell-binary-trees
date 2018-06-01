module BinaryTree ( BinaryTree(Node, EmptyT), reverseTree, right, left ) where

data BinaryTree a = EmptyT | Node a (BinaryTree a) (BinaryTree a)
     deriving (Show, Eq)

type Ancestor a = BinaryTree a
type Current a = BinaryTree a
type Left a = BinaryTree a
type Right a = BinaryTree a
type BinGraph a = [(Current a,
                    Ancestor a,
                    Left a,
                    Right a)]

maybeVal :: BinaryTree a -> Maybe a
maybeVal EmptyT = Nothing
maybeVal (Node x left right) = Just x


left :: BinaryTree a -> BinaryTree a
left EmptyT = EmptyT
left (Node _ l _) = l

right :: BinaryTree a -> BinaryTree a
right EmptyT = EmptyT
right (Node _ _ r) = r

binToGraph :: BinaryTree a -> BinGraph a
binToGraph = binToGraph_ EmptyT


binToGraph_ :: BinaryTree a -> BinaryTree a -> BinGraph a
binToGraph_ ancestor node = curNode ++ leftNode ++ rightNode
    where leftNode = (binToGraph_ node (left node))
          rightNode = (binToGraph_ node (right node))
          curNode = [(node, ancestor, left node, right node)]

maxDepth :: BinaryTree a -> Int
maxDepth EmptyT = 0
maxDepth (Node _ left right) = 1 + (max (maxDepth left) (maxDepth right))


basicTree :: BinaryTree Int
basicTree = (Node 8
              (Node 5
                (Node 9
                  EmptyT
                  EmptyT)
                (Node 7
                  (Node 1 EmptyT EmptyT)
                  (Node 12
                    (Node 2 EmptyT EmptyT)
                    EmptyT)))
             (Node 4
               EmptyT
               (Node 11
                 (Node 3 EmptyT EmptyT)
                 EmptyT)))


reverseTree :: BinaryTree a -> BinaryTree a
reverseTree EmptyT = EmptyT
reverseTree (Node nodeValue leftTree rightTree) =
  (Node nodeValue (reverseTree rightTree) (reverseTree leftTree))
