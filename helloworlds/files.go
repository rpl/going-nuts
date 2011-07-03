package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Printf("Writing files\n")

	out, err := os.Create("prova123.dat")

	if err != nil {
		fmt.Printf("ERROR - %s\n", err)
		return
	}

	out.Write([]byte("prova\n"))
	out.Close()
}