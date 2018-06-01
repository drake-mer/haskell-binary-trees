module InOrder where
import BinaryTree(BinaryTree(EmptyT, Node))

inOrderTraversal t = aux t []
  where
    aux :: BinaryTree a -> [a] -> [a]
    aux EmptyT       rest = rest
    aux (Node x l r) rest = aux l (x : aux r rest)
