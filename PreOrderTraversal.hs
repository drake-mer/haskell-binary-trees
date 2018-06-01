import BinaryTree(BinaryTree(Node, EmptyT))

preOrderTraversal :: (Eq a) => BinaryTree a -> [BinaryTree a] -> [a]

-- end of traversal case
preOrderTraversal EmptyT [] = []

-- End of branch tree, continue from the stack
preOrderTraversal EmptyT ((Node value leftTree rightTree):xs) =
    (value:(preOrderTraversal leftTree newStack))
    where newStack = if (rightTree == EmptyT) then xs
                     else (rightTree:xs)

-- Continue on the left as a default
preOrderTraversal (Node value leftTree rightTree) stack =
    (value:(preOrderTraversal leftTree newStack)) where
    newStack = if (rightTree == EmptyT) then stack
               else (rightTree:stack)

