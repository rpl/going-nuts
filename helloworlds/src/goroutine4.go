package main

import (
	"log"
	"time"
)

func goroutine1(ch chan interface{}) {
//	for ;; {
//		_ = <- ch
		ch <- "testreply1"
//	}
}

func send_wait_until(ch chan interface{}, outmsg interface{}, wait_time int64) (interface{}, bool) {
	timeout := make(chan bool)
	timeout_goroutine := func () {
		time.Sleep(wait_time*1e9)
		timeout <- true
	}

	go timeout_goroutine()

  select {
	case ch <- outmsg:
    log.Print("MESSAGE SENT")
  default: 
    log.Print("MESSAGE NOT SENT")
	}

	select {
	case msg := <- ch:
    return msg, true
  case _ = <- timeout:
    return nil, false
	}

	return nil, false
}

func main() {
	ch := make(chan interface{})
	go goroutine1(ch)

	reply, ok := send_wait_until(ch, "prova", 2)
	log.Print("REPLY: ",reply," - OK: ",ok)
}