package main

import "fmt"

func main() {
	test := [3]int{1,2,3}

	for i,v := range test {
		fmt.Printf("test[%d]:%d\n",i,v)
	}
}