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
  ReplyChannel ReplyMessageChannel
}

type ReplyMessage struct {
	Ok bool
	Result Data
}

type MessageChannel chan Message
type ReplyMessageChannel chan ReplyMessage

type GenServer struct {
  ch chan Message
	impl IGenServerImpl
}

func (self *GenServer) loop() {
  for ; ; {
		msg := <- self.ch

		switch cmsg := msg.(type) {
		case CallMessage:
			log.Print("RECEIVED CALL",msg)
			self.impl.HandleCall(cmsg)
			cmsg.ReplyChannel <- ReplyMessage{Ok: true, Result: msg}
	  case CastMessage:
			log.Print("RECEIVED CAST",msg)
			self.impl.HandleCast(cmsg)
		}
	}
}

func (self *GenServer) Call(name string, args Data) ReplyMessage {
	reply_ch := make(ReplyMessageChannel)
	self.ch <- CallMessage{Name: name, Args: args, ReplyChannel: reply_ch}
	return <- reply_ch
}

func (self *GenServer) Cast(name string, args Data) {
	self.ch <- CastMessage{Name: name, Args: args}
}

func (self *GenServer) Start(impl IGenServerImpl) {
	ch := make(MessageChannel)
	self.ch = ch
	self.impl = impl
	go self.loop()  
}

type IGenServerImpl interface {
  Init(args Data) (bool, Data)
	HandleCast(msg CastMessage)
	HandleCall(msg CallMessage)
}

type MyServer struct {
  *GenServer
}

func (self *MyServer) Start() {
	gsrv := new(GenServer)
	self.GenServer = gsrv
	gsrv.Start(self)  
}

func (self *MyServer) Init(args Data) (bool, Data) {
	return true, nil
}

func (self *MyServer) HandleCast(msg CastMessage) {
	log.Print("HANDLE CAST ", msg)
}

func (self *MyServer) HandleCall(msg CallMessage) {
	log.Print("HANDLE CALL ", msg)
}

func (self *MyServer) CallTest1(src string) Data {
	reply := self.GenServer.Call("p1", src)
	return reply.Result
}

func (self *MyServer) CastTest2(src string) {
	self.GenServer.Cast("p2", src)
}


func main() {
	srv := new(MyServer)
	srv.Start()
  log.Print("GOT CALL REPLY ", srv.CallTest1("WORLD"))

	srv.CastTest2("1")
	srv.CastTest2("2")
	srv.CastTest2("3")
}