package main

import (
	"mergesort"
	"quicksort"
	"heapsort"
	"fmt"
)

func main() {
	data := []int{4,10,3,1,100,50,23,4,4,10,23,60,12,6,105,500,10,1,30,20}
	fmt.Printf("UNSORTED:\n%o\n",data)
	fmt.Printf("MERGESORT:\n%o\n",mergesort.Sort(data))

	data = []int{4,10,3,1,100,50,23,4,4,10,23,60,12,6,105,500,10,1,30,20}
	fmt.Printf("QUICKSORT:\n%o\n",quicksort.Sort(data))
	data = []int{4,10,3,1,100,50,23,4,4,10,23,60,12,6,105,500,10,1,30,20}
	fmt.Printf("HEAPSORT :\n%o\n",heapsort.Sort(data))
}

