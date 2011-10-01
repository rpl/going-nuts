package main

import "fmt"

func main() {
	test := map[string]int{"one":1,"two":2,"three":3}

	for k,v := range test {
		fmt.Printf("test[%s]:%d\n",k,v)
	}
}