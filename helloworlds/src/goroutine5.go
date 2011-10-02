package main

import "log"

type IntChannel chan int

func goroutine5(ch IntChannel) {
  for ; ; {
		msg := <- ch
		ch <- msg
	}
}

func Call(ch IntChannel, msg int) int {
	ch <- msg
	return <- ch
}

func main() {
	ch := make(IntChannel)

	go goroutine5(ch)

  log.Print("HELLO ", Call(ch, 5))
}