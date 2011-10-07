package gen_server

import (
	"log"
	"runtime"
	"time"
	"fmt"
)

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

	return ReplyMessage{Ok: false, Error: "GenServer Error Unknown"}
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

func getopt_timeout(opts []interface{}, default_value int64) int64 {
	timeout := default_value
	if len(opts)>0 {
		switch value := opts[0].(type) {
		case int:
			timeout = int64(value)
		case int64:
			timeout = value
		case float64:
			timeout = int64(value)
		default:
			// ERROR "timeout option type unknown"
		}
	}

	return timeout
}

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

// handle_init will be called to handle incoming InitControlMessages
func (self *GenServer) handle_init(cmd *initControlMessage) {
  self.log("RECEIVED INIT ",cmd)
	ok, state := self.impl.Init(cmd.Args)
	if ok == true {
		self.state = state
		self.status = READY
		cmd.ReplyChannel <- ReplyMessage{Ok: true}
	} else {
		cmd.ReplyChannel <- ReplyMessage{Ok: false, Error: "GenServer Init failed"}
	}
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
	self.impl.HandleCast(cast,self.state)
}

// handle_call will be called to handle incoming CallMessages
func (self *GenServer) handle_call(call *CallMessage) {
	self.log("RECEIVED CALL ",call)
	self.impl.HandleCall(call,self.state)
}

