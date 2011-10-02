package main

import "log"

type Message interface {}
type Data interface {}

type CastMessage struct {
	Name string
	Args Data
}

type CallMessage struct {
  Name string
	Args Data
  ReplyChannel MessageChannel
}

type ReplyMessage struct {
	Ok bool
	Result Data
}

type MessageChannel chan Message

func goroutine6(ch <-chan Message) {
  for ; ; {
		msg := <- ch

		switch cmsg := msg.(type) {
		case CallMessage:
			log.Print("RECEIVED CALL",msg)
			cmsg.ReplyChannel <- ReplyMessage{Ok: true, Result: msg}
	  case CastMessage:
			log.Print("RECEIVED CAST",msg)
		}
	}
}

func Call(ch MessageChannel, name string, args Data) Message {
	reply_ch := make(MessageChannel)
	ch <- CallMessage{Name: name, Args: args, ReplyChannel: reply_ch}
	return <- reply_ch
}

func Cast(ch MessageChannel, name string, args Data) {
	ch <- CastMessage{Name: name, Args: args}
}

func main() {
	ch := make(MessageChannel)

	go goroutine6(ch)

  log.Print("GOT CALL REPLY ", Call(ch, "prova1", "WORLD"))

	Cast(ch, "p2", "1")
	Cast(ch, "p3", "2")
	Cast(ch, "p4", "3")
}