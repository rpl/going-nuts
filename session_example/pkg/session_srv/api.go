package session_srv

import ("gen_server")

func CreateSessionSrv() *SessionSrv {
	srv := new(SessionSrv)
	srv.GenServer = gen_server.CreateGenServer(srv)
	srv.SetDebug(true)
	return srv
}

func (self *SessionSrv) Start() bool {
	self.GenServer.Start(nil)  
	return true
}

func (self *SessionSrv) CallTest(src string) *gen_server.ReplyMessage {
	reply := self.GenServer.Call(src)
	return &reply
}

func (self *SessionSrv) CastTest(src string) {
	self.GenServer.Cast(CastTestMessage{msg: src})
}

func (self *SessionSrv) CastCrashTest(src string) {
	self.GenServer.Cast(CastCrashTestMessage{msg: src})
}
