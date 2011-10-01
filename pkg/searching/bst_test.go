package bst

import(
	"testing"
	pt "prettytest"
)

type IntKey int

func (self IntKey) Compare(other IKey) int {
	var j IntKey = other.(IntKey)

	if self < j {
		return -1
	} else if self > j {
		return 1
	}

	return 0
}

type bstTestSuite struct {
	pt.Suite
}

func (s *bstTestSuite) TestBasicOperations() {
	bst1 := BST{}

	for _, v := range []int{10, 50, 80} {
		key := IntKey(v)
		value := v * 10
		bst1.Put(key, value)
	}

	bst1.Delete(IntKey(80))

	s.Equal(2,bst1.Size())
	s.Equal(100,bst1.Get(IntKey(10)))
	s.Equal(500,bst1.Get(IntKey(50)))
	s.Equal(nil,bst1.Get(IntKey(80)))
	s.Equal(nil,bst1.Get(IntKey(1000)))
}

func (s *bstTestSuite) TestIntCast() {
	bst1 := BST{}
	bst1.Put(IntKey(5),10)
	s.Equal(1,bst1.Size())
}

func (s *bstTestSuite) TestDeleMin() {
	bst1 := BST{}
	bst1.Put(IntKey(5),"node1")
	bst1.Put(IntKey(1),"node2")
	s.Equal(2,bst1.Size())
	bst1.DeleteMin()
	s.Equal(1,bst1.Size())
	s.Equal(nil,bst1.Get(IntKey(1)))
	s.Equal("node1",bst1.Get(IntKey(5)))
}

func (s *bstTestSuite) TestDeleMax() {
	bst1 := BST{}
	bst1.Put(IntKey(5),"node1")
	bst1.Put(IntKey(1),"node2")
	s.Equal(2,bst1.Size())
	bst1.DeleteMax()
	s.Equal(1,bst1.Size())
	s.Equal(nil,bst1.Get(IntKey(5)))
	s.Equal("node2",bst1.Get(IntKey(1)))
}

func (s *bstTestSuite) TestDelete() {
	bst1 := BST{}
	populateSmallBST(&bst1)
	s.Equal(3,bst1.Size())
	bst1.Delete(IntKey(1))
	s.Equal(2,bst1.Size())
	s.Equal(nil,bst1.Get(IntKey(1)))
	s.Equal("node1",bst1.Get(IntKey(5)))
	s.Equal("node3",bst1.Get(IntKey(10)))
}

func (s *bstTestSuite) TestFloorAndCeil() {
	bst1 := BST{}
	populateSmallBST(&bst1)
	s.Equal(IntKey(5),bst1.Floor(IntKey(6)))
	s.Equal(IntKey(10),bst1.Ceil(IntKey(6)))
}

func (s *bstTestSuite) TestMaxAndMin() {
	bst1 := BST{}
	populateSmallBST(&bst1)
	s.Equal(IntKey(10),bst1.Max())
	s.Equal(IntKey(1),bst1.Min())
}

func populateSmallBST(bst *BST) {	
	bst.Put(IntKey(5),"node1")
	bst.Put(IntKey(1),"node2")
	bst.Put(IntKey(10),"node3")
}

func TestBST(t *testing.T) {
	pt.Run(
		t,
		new(bstTestSuite),
		)
}