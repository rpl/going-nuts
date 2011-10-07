// "Erlang OTP gen_server"-like Concurrency Pattern for Go
package gen_server

// SetDebug enable/disable debugging messages
func (self *GenServer) SetDebug(debug bool) {
	self.debug = debug
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

func (self *GenServer) Start(init_args Data, opts ...interface{}) {
	self.status = STARTING;
	go self.loop()  

	timeout := getopt_timeout(opts, 5e9)
	reply_ch := make(ReplyMessageChannel,1)

  send_and_wait_until(self.control_ch, 
		initControlMessage{Args: init_args, ReplyChannel: reply_ch},
		func() interface{} { return <- reply_ch },
		func() interface{} { return ReplyMessage{Ok: false, Error: "GenServer Cast Timeout"} },
		timeout)
}

func (self *GenServer) Stop(opts ...interface{}) ReplyMessage {
	if self.status >= STOPPED {
		return ReplyMessage{Ok: false, Error: "GenServer Stopped or Crashed"}
	}

	timeout := getopt_timeout(opts, 5e9)
	reply_ch := make(ReplyMessageChannel,1)

	return send_and_wait_until(self.control_ch, stopControlMessage{ReplyChannel: reply_ch},
		func() interface{} { return <- reply_ch },
		func() interface{} { return ReplyMessage{Ok: false, Error: "GenServer Stop Timeout"} },
		timeout).(ReplyMessage)
}


func (self *GenServer) Cast(payload Data, opts ...interface{}) {
	if self.status >= STOPPED {
		return // Error: "GenServer Stopped or Crashed"
	}

	timeout := getopt_timeout(opts, 5e9)

	send_and_wait_until(self.ch, CastMessage{Payload: payload},
		func() interface{} { return ReplyMessage{Ok: true} },
		func() interface{} { return ReplyMessage{Ok: false, Error: "GenServer Cast Timeout"} },
		timeout)
}

func (self *GenServer) Call(payload Data, opts ...interface{}) ReplyMessage {
	if self.status >= STOPPED {
		return ReplyMessage{Ok: false, Error: "GenServer Stopped or Crashed"}
	}

	timeout := getopt_timeout(opts, 5e9)
	reply_ch := make(ReplyMessageChannel,1)

	return send_and_wait_until(self.ch, 
		CallMessage{Payload: payload, replyChannel: reply_ch},
		func() interface{} { return <- reply_ch },
		func() interface{} { return ReplyMessage{Ok: false, Error: "GenServer Stop Timeout"} },
		timeout).(ReplyMessage)
}

func (self *CallMessage) Reply(ok bool, result Data) {
	select {
	case self.replyChannel <- ReplyMessage{Ok: ok, Result: result}:
	default:
		// CLIENT GOROUTINE CRASHED?
	}
}

