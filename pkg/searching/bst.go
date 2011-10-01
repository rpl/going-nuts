package bst

/*import (
	"log"
)*/


type IKey interface {
	Compare(other IKey) int
}

type IValue interface{}

type NodeType struct {
	Key         IKey
	Value       IValue
	N           int
	Left, Right *NodeType
}

type BST struct {
	Root *NodeType
}

/* BST Size */

func (bst *BST) Size() int {
	return size(bst.Root)
}

func size(node *NodeType) int {
	if node != nil {
		return node.N
	}

	return 0
}

/* GET node */

func (bst *BST) Get(key IKey) IValue {
	return get(bst.Root, key)
}

func get(node *NodeType, key IKey) IValue {
	if node != nil {
		if res := key.Compare(node.Key); res < 0 {
			return get(node.Left, key)
		} else if res > 0 {
			return get(node.Right, key)
		} else {
			return node.Value
		}
	}

	return nil
}

/* PUT node */

func (bst *BST) Put(key IKey, value IValue) {
	bst.Root = put(bst.Root, key, value)
}

func put(node *NodeType, key IKey, value IValue) *NodeType {
	if node == nil {
		new_node := &NodeType{Key: key, Value: value, N: 1}
		return new_node
	}
	res := key.Compare(node.Key)
	if res < 0 {
		node.Left = put(node.Left, key, value)
	} else if res > 0 {
		node.Right = put(node.Right, key, value)
	} else {
		node.Value = value
	}

	node.N = size(node.Left) + size(node.Right) + 1

	return node
}

/* SELECT node */

func (bst *BST) Min() IKey {
	return min(bst.Root).Key
}

func min(node *NodeType) *NodeType {
	if node.Left == nil {
		return node
	}

	return min(node.Left)
}

func (bst *BST) Max() IKey {
	return max(bst.Root).Key
}

func max(node *NodeType) *NodeType {
	if node.Right == nil {
		return node
	}

	return max(node.Right)
}

func (bst *BST) Floor(key IKey) IKey {
	node := floor(bst.Root, key)
	if node == nil {
		return nil
	}

	return node.Key
}

func floor(node *NodeType, key IKey) *NodeType {
	if node == nil {
		return nil
	}

	if res := key.Compare(node.Key); res == 0 {
		return node
	} else if res < 0 {
		return floor(node.Left, key)
	}
	temp := floor(node.Right, key)
	if temp == nil {
		return node
	}
	return temp
}

func (bst *BST) Ceil(key IKey) IKey {
	node := ceil(bst.Root, key)
	if node == nil {
		return nil
	}

	return node.Key
}

func ceil(node *NodeType, key IKey) *NodeType {
	if node == nil {
		return nil
	}

	if res := key.Compare(node.Key); res == 0 {
		return node
	} else if res > 0 {
		return ceil(node.Right, key)
	}
	temp := ceil(node.Left, key)
	if temp == nil {
		return node
	}
	return temp
}

/* DELETE node */

func (bst *BST) DeleteMin() {
	bst.Root = deleteMin(bst.Root)
}

func deleteMin(node *NodeType) *NodeType {
	if node.Left == nil {
		return node.Right
	}

	node.Left = deleteMin(node.Left)
	node.N = size(node.Left) + size(node.Right) + 1

	return node
}

func (bst *BST) Delete(key IKey) {
	bst.Root = delete(bst.Root, key)
}

func delete(node *NodeType, key IKey) *NodeType {
	if node == nil {
		return nil
	}
	if res := key.Compare(node.Key); res < 0 {
		node.Left = delete(node.Left, key)
	} else if res > 0 {
		node.Right = delete(node.Right, key)
	} else {
		if node.Right == nil {
			return node.Left
		}
		if node.Left == nil {
			return node.Right
		}

		tmp := node
		node = min(tmp.Right)
		node.Right = deleteMin(tmp.Right)
		node.Left = tmp.Left
	}

	node.N = size(node.Left) + size(node.Right) + 1

	return node
}

func (bst *BST) DeleteMax() {
	bst.Root = deleteMax(bst.Root)
}

func deleteMax(node *NodeType) *NodeType {
	if node.Right == nil {
		return node.Left
	}

	node.Right = deleteMax(node.Right)
	node.N = size(node.Left) + size(node.Right) + 1
	return node
}
