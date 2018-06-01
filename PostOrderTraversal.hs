import BinaryTree(BinaryTree(Node, EmptyT))

postOrderTraversal :: (Eq a) => BinaryTree a -> [BinaryTree a] -> [a]
postOrderTraversal (Node value EmptyT EmptyT) (headStack:tailStack) =
    (value:(postOrderTraversal headStack tailStack))

postOrderTraversal (Node value leftValue rightValue) stack
    | leftValue == EmptyT && rightValue /= EmptyT = postOrderTraversal rightValue ((Node value leftValue EmptyT):stack)
    | leftValue /= EmptyT && rightValue /= EmptyT = postOrderTraversal leftValue ((Node value EmptyT rightValue):stack)
    | leftValue /= EmptyT && rightValue == EmptyT = postOrderTraversal leftValue ((Node value EmptyT rightValue):stack)
    | leftValue == EmptyT && rightValue == EmptyT && null stack = [value]
    | otherwise = (value:(postOrderTraversal x xs))
    where  x = head stack
          xs = tail stack
