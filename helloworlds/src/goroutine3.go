package main

import (
	"log"
	"time"
)

func goroutine1(ch chan string) {
	// ch <- "test"
}

func gotimeout(timeout chan bool) {
  time.Sleep(2*1e9)
	timeout <- true
}

func main() {
	ch := make(chan string)
	timeout := make(chan bool)
	go gotimeout(timeout)
	go goroutine1(ch)
	select {
	case msg := <- ch:
    log.Print("MSG: ", msg)
  case _ = <- timeout:
    log.Print("TIMEOUT")
	}
}