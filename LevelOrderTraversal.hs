import BinaryTree(BinaryTree(Node,EmptyT))

getValFromNodeList :: (Eq a) => [BinaryTree a] -> [a]
getValFromNodeList = (map (\(Node val _ _) -> val)).(filter ((/=) EmptyT))

getNodesFromNodeList :: (Eq a) => [BinaryTree a] -> [BinaryTree a]
getNodesFromNodeList = (filter ((/=) EmptyT)).concat.(map (\(Node _ l r) -> [l,r]))

levelOrderTraversal :: (Eq a) => [BinaryTree a] -> [a]
levelOrderTraversal [] = []
levelOrderTraversal nodeList = (getValFromNodeList nodeList)++(levelOrderTraversal under)
    where under = (getNodesFromNodeList nodeList)

