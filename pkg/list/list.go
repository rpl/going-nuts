// this package implements a generic list data type and its methods  
package list

// ValueType represents a generic value
type ValueType interface {}

// NodeType represents a generic node
type NodeType struct {
	value ValueType
	next *NodeType
}

// ListType represents a generic list
type ListType struct {
	start *NodeType
	length int
}

// NewList create a new list
func NewList() *ListType {
	var new_list ListType
	new_list.length = 0

	return &new_list
}

// Length returns the length of the list
func (list *ListType) Length() int {
	return list.length
}

// Append add a value to the list
func (list *ListType) Append(value ValueType) {
	new_node := new(NodeType)
	new_node.value = value

	if list.length == 0 {
		list.start = new_node
	} else {
		node := node_at(list,list.length-1)
		node.next = new_node
	}

	list.length++
}

// At returns the node value at a given position
func (list *ListType) At(position int) ValueType {
	node := node_at(list, position)

	if node != nil {
		return node.value
	}

	return nil
}

// node_at is a private function that returns the node in a given list and position
func node_at(list *ListType, position int) *NodeType {
	if position < list.length {
		for i, node := 0, list.start; node != nil && i < list.length; i, node = i+1, node.next {
			if i == position {
				return node
			}
		}
	} 

	return nil
}

// delete_at is a private function that remove a node from a given list and position
func delete_at(list *ListType, position int) {
	node := node_at(list, position)

	if node != nil {
		if node.next != nil {
			node.value = node.next.value
			node.next = node.next.next
		}

	}
}

// Delete remove a node at a given position from the list
func (list *ListType) Delete(position int) {
	if position == 0 && list.length > 0 {
		list.start = list.start.next
		list.length--		
	} else if position == list.length-1 {		
		node := node_at(list, position-1)
		if node != nil {
			node.next = nil
			list.length--
		}
	} else if position < list.length -1 {
		delete_at(list,position)
		list.length--
	}
}