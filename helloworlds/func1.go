package main

import "fmt"

func func1(name string) int {
	fmt.Printf("called func1("+name+")\n");
	
	return 1
}

func main() {
	fmt.Printf("Testing function declaration\n");
	fmt.Printf("%d\n", func1("test1"));
	fmt.Printf("%d\n", func1("test2"));
}