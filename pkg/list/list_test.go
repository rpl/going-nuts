package list

import (
	"testing"
	pt "prettytest"
	// "log"
)

type listTestSuite struct {
	pt.Suite
}

func TestList(t *testing.T) {
	pt.Run(
		t,
		new(listTestSuite),
		)
}

func (s *listTestSuite) TestAppendAndLength() {
	var list1 *ListType

	list1 = NewList()
	list1.Append(5)
	list1.Append(6)
	list1.Append(7)
	s.Equal(3,list1.Length())
}

func (s *listTestSuite) TestDeleteFirstElement() {
	var list1 *ListType

	list1 = NewList()
	list1.Append(5)
	list1.Append(6)
	list1.Append(7)
	list1.Delete(0)
	s.Equal(2,list1.Length())
	s.Equal(6,list1.At(0))
	s.Equal(7,list1.At(1))
}

func (s *listTestSuite) TestDeleteMiddleElement() {
	var list1 *ListType

	list1 = NewList()
	list1.Append(5)
	list1.Append(6)
	list1.Append(7)
	list1.Delete(1)
	s.Equal(2,list1.Length())
	s.Equal(5,list1.At(0))
	s.Equal(7,list1.At(1))
}

func (s *listTestSuite) TestDeleteLastElement() {
	var list1 *ListType

	list1 = NewList()
	list1.Append(5)
	list1.Append(6)
	list1.Append(7)
	list1.Delete(2)
	s.Equal(2,list1.Length())
	s.Equal(5,list1.At(0))
	s.Equal(6,list1.At(1))
}

func (s *listTestSuite) TestBasicOperations() {
	var list1 *ListType

	list1 = NewList()
	pippo := 5
	list1.Append(pippo)

	s.NotEqual(nil,list1)
	s.Equal(5,list1.At(0))
	list1.Delete(0)
	s.Equal(nil,list1.At(0))
}