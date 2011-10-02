package main

import (
	"log"
	"time"
)

func goroutine1() {
	for ;; {
		log.Print("GOROUTINE1")
		time.Sleep(10*1e8)
		panic("PANIC")
	}
}

func main() {
	go goroutine1()
	time.Sleep(10*1e9)
}