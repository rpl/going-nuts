package main

import "fmt"

func fib_r(num int) int {
	if(num == 0) {
		return 0
	}
	if(num == 1) {
		return 1
	}
	
	return fib_r(num-1) + fib_r(num-2);
}

func fib_i(num uint64) uint64 {
	if(num == 0) {
		return 0
	}
	if(num == 1) {
		return 1
	}

	var result uint64 = 0
	var fib_l1 uint64 = 1
	var fib_l2 uint64 = 0

	var i uint64 = 2
	
	for ; i<=num && (max_uint64 - fib_l1 >= fib_l2) ; i++ {
		result = fib_l1 + fib_l2
		fib_l2 = fib_l1
		fib_l1 = result
	}

	if(i < num) {
		fmt.Printf("OVERFLOW\n")
	}
	
	return result
}

var max_uint64 uint64 = 1 << 64 - 1;

func main() {
	var i uint64;
	for i = 0; i<100; i++ {
		fmt.Printf("Fib(%d): %d\n", i, fib_i(i))
	}
}