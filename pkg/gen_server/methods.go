// "Erlang OTP gen_server"-like Concurrency Pattern for Go
package gen_server

import (
	"log"
	"runtime"
	"time"
	"fmt"
)

// loop implements the internal GenServer goroutine loop
func (self *GenServer) loop() {
	defer self.recover()

  for ; ; {
		self.status = READY
		select {
		case msg := <- self.ch :
			self.status = BUSY
			switch cmsg := msg.(type) {
			case CallMessage:
				self.handle_call(&cmsg)
			case CastMessage:
				self.handle_cast(&cmsg)
			}
		case cmd := <- self.control_ch :
			self.status = BUSY
			switch ccmd := cmd.(type) {
			case initControlMessage:
				self.handle_init(&ccmd)
			case stopControlMessage:
				self.handle_stop(&ccmd)
			}
		}
	}
}

func (self *GenServer) recover() {
	if err := recover(); err != nil {
		self.status = CRASHED
		self.log("RECOVERED FROM PANIC:",err)
		for i := 2; ; i++ {
			_, file, line, ok := runtime.Caller(i);
			if ok {
				self.log(fmt.Sprintf("%s : %d\n", file, line),nil);
			} else {
				break;
			}
		} 
	}
}

// log will print log messages if self.debug is true
func (self *GenServer) log(log_msg string, log_data interface {}) {
	if self.debug {
		log.Print(log_msg,log_data)
	}
}

// SetDebug enable/disable debugging messages
func (self *GenServer) SetDebug(debug bool) {
	self.debug = debug
}

// handle_init will be called to handle incoming InitControlMessages
func (self *GenServer) handle_init(cmd *initControlMessage) {
  self.log("RECEIVED INIT ",cmd)
	self.status = READY
	cmd.ReplyChannel <- ReplyMessage{Ok: true}
}

// handle_stop will be called to handle incoming StopControlMessages
func (self *GenServer) handle_stop(cmd *stopControlMessage) {
	defer func() {
		// TODO: error handling
		self.status = STOPPED
		cmd.ReplyChannel <- ReplyMessage{Ok: true}
	}()
	self.log("RECEIVED STOP ",cmd)
	runtime.Goexit()
}

// handle_cast will be called to handle incoming CastMessages
func (self *GenServer) handle_cast(cast *CastMessage) {
	self.log("RECEIVED CAST ",cast)
	self.impl.HandleCast(cast)
}

// handle_call will be called to handle incoming CallMessages
func (self *GenServer) handle_call(call *CallMessage) {
	self.log("RECEIVED CALL ",call)
	self.impl.HandleCall(call)
}

// GetStatus returns the current GenServer Status
func (self *GenServer) GetStatus() int {
	return self.status
}

func CreateGenServer(impl IGenServerImpl) *GenServer {
	gensrv := new(GenServer)

	ch := make(MessageChannel)
	control_ch := make(controlChannel)
	gensrv.ch = ch
	gensrv.control_ch = control_ch
	gensrv.impl = impl

	return gensrv
}

func (self *GenServer) Start() {
	self.status = STARTING;
	go self.loop()  
	reply_ch := make(ReplyMessageChannel,1)
	self.control_ch <- initControlMessage{ReplyChannel: reply_ch}
	<- reply_ch
}

type callback_fn func() interface {}

func send_and_wait_until(where chan interface {}, what interface {},
	sent_cb callback_fn, timeout_cb callback_fn, timeout int64) (Data) {

	timeout_ch := make(chan bool,1)
	go func() {
		time.Sleep(timeout)
		timeout_ch <- true
	}()
	select {
	case where <- what: 
		return sent_cb()
  case <- timeout_ch:
		return timeout_cb()
	}

	return nil
}


func (self *GenServer) Stop() ReplyMessage {
	if self.status >= STOPPED {
		return ReplyMessage{Ok: false, Error: "GenServer Stopped or Crashed"}
	}
	timeout_ch := make(chan bool,1)
	reply_ch := make(ReplyMessageChannel,1)
	go func() {
		time.Sleep(1e9)
		timeout_ch <- true
	}()
	select {
	case self.control_ch <- stopControlMessage{ReplyChannel: reply_ch}:
		return <- reply_ch
	case <- timeout_ch:
		return ReplyMessage{Ok: false, Error: "GenServer Stop Timeout"}
	}
	return ReplyMessage{Ok: false, Error: "GenServer Error Unknown"}
}


func (self *GenServer) Cast(payload Data) {
	if self.status >= STOPPED {
		return // Error: "GenServer Stopped or Crashed"
	}

	// NOTE: This is blocking if receive is not ready
	send_and_wait_until(self.ch, CastMessage{Payload: payload},
		func() interface{} { log.Print("CAST SENT"); return nil},
		func() interface{} { log.Print("CAST TIMEOUT"); return nil},
		5e9)
	//self.ch <- CastMessage{Payload: payload}
}

func (self *GenServer) Call(payload Data) ReplyMessage {
	if self.status >= STOPPED {
		return ReplyMessage{Ok: false, Error: "GenServer Stopped or Crashed"}
	}

	timeout_ch := make(chan bool,1)
	reply_ch := make(ReplyMessageChannel,1)
	go func() {
		time.Sleep(1e9)
		timeout_ch <- true
	}()
	select {
	case self.ch <- CallMessage{Payload: payload, replyChannel: reply_ch}:
		return <- reply_ch
	case <- timeout_ch:
		return ReplyMessage{Ok: false, Error: "GenServer Stop Timeout"}
	}
	return ReplyMessage{Ok: false, Error: "GenServer Error Unknown"}
}

func (self *CallMessage) Reply(ok bool, result Data) {
	select {
	case self.replyChannel <- ReplyMessage{Ok: ok, Result: result}:
	default:
		// CLIENT GOROUTINE CRASHED?
	}
}

