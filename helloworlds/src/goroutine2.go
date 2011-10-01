package main

import (
	"log"
	_ "time"
)

func goroutine1(ch chan string) {
	ch <- "test"
}

func main() {
	ch := make(chan string)
	go goroutine1(ch)
	msg := <- ch
	log.Print("MSG: ", msg)
}