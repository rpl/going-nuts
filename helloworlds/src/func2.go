package main

import "fmt"

func func2(name string) int {
	fmt.Printf("called func1("+name+")\n");
	
	if(name=="test1") {
        	func2(name+"-2");	
        }
	return 1
}

func main() {
	fmt.Printf("Testing function recursion\n");
	fmt.Printf("%d\n", func2("test1"));
	fmt.Printf("%d\n", func2("test2"));
}