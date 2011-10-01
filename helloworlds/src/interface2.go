package main

import ("log")

type Comparable interface {
	Compare(other Comparable) int
	
}

type IntComparable int

func (self IntComparable) Compare(other IntComparable) int {
	log.Println("COMPARE")

	if self < other {
		return -1
	} else if self > other {
		return 1
	}
	
	return 0
}

func main() {
	log.Println("START")
	var i,j IntComparable

	i = 5
	j = 6
	k := i.Compare(j)

	log.Println(i,j,k)
}