--module BinTree () where
--module BinTree (BinTree,emptyTree,treeMember,treeInsert,
--                        treeDelete, buildTree,inorder,treeSort) where

treeMember   :: (Ord a,Show a) => a -> BinTree a -> Bool
treeInsert   :: (Ord a,Show a) => a -> BinTree a -> BinTree a
treeDelete   :: (Ord a,Show a) => a -> BinTree a -> BinTree a
--buildTree    :: (Ord a,Show a) => [a] -> BinTree a
--inorder      :: (Ord a,Show a) => BinTree a -> [a]
--treeSort     :: (Ord a,Show a) => [a] -> [a]

data (Ord a) => BinTree a = EmptyBT
                          | NodeBT a (BinTree a) (BinTree a)
    deriving Show

emptyTree = EmptyBT
------------------------------- treeMember -------------------------------------
treeMember v' EmptyBT                = False
treeMember v' (NodeBT v lf rt) | v==v' = True  
                               | v'<v  = treeMember v' (lf)
                               | v'>v  = treeMember v' (rt)
							   
						
------------------------------- treeInsert -------------------------------------
treeInsert v' EmptyBT                      = NodeBT v' EmptyBT EmptyBT
treeInsert v' (NodeBT v lf rt) | v'==v     = NodeBT v lf rt
                               | v' < v    = NodeBT v lf (treeInsert v' rt)
                               | otherwise = NodeBT v rt (treeInsert v' lf)

							   
buildTree lf = foldr treeInsert EmptyBT (reverse lf)

-- value not found
--treeDelete v' EmptyBT                       = v' NodeBT
-- one descendant
treeDelete v' (NodeBT v lf EmptyBT) | v'==v = treeDelete v' (NodeBT v lf EmptyBT) 
treeDelete v' (NodeBT v EmptyBT rt) | v'==v = treeDelete v' (NodeBT v EmptyBT rt)
-- two descendants
--treeDelete v' (NodeBT v lf rt)
--    | v'<v  = ??
--    | v'>v  = ?? 
--    | v'==v = ??

--minTree 

--inorder 

--treeSort

t1:: BinTree Integer
t1= NodeBT 5 (NodeBT 2 EmptyBT EmptyBT) EmptyBT

t2:: BinTree Integer
t2= NodeBT 5 (NodeBT 2 EmptyBT (NodeBT 4 (NodeBT 3 EmptyBT EmptyBT) EmptyBT)) EmptyBT

t3::BinTree Integer
t3= NodeBT 5 (NodeBT 2 EmptyBT (NodeBT 4 (NodeBT 3 EmptyBT EmptyBT) EmptyBT)) (NodeBT 6 EmptyBT (NodeBT 7 EmptyBT (NodeBT 10 (NodeBT 9 EmptyBT EmptyBT) (NodeBT 11 EmptyBT EmptyBT))))



