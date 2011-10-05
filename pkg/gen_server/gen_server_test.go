package gen_server

import (
	"testing"
	pt "prettytest"
	_ "log"
)

type gen_serverTestSuite struct {
	pt.Suite
}

func TestGenServer(t *testing.T) {
	pt.Run(t, new(gen_serverTestSuite))
}

// BEGIN TestServer Mockup 

type TestServer struct {
	*GenServer
	last_cast_received *CastMessage
}

func CreateTestServer() *TestServer {
	srv := new(TestServer)
	srv.GenServer = new(GenServer)
	//srv.GenServer.debug = true
	return srv
}

func (self *TestServer) Start() bool {
	self.GenServer.Start(self)  
	return true
}

func (self *TestServer) Init(args Data) (bool, Data) {
	return true, nil
}

type CastTestMessage struct {
	msg string
}

type CastCrashTestMessage struct {
	msg string
}

func (self *TestServer) HandleCast(cast *CastMessage) {
	self.log("HANDLE CAST ", cast)
	self.last_cast_received = cast
	switch payload := cast.Payload.(type) {
	case CastCrashTestMessage:
		panic("panic inside the gen_server impl")
	}
}

func (self *TestServer) HandleCall(call *CallMessage) {
	self.log("HANDLE CALL ", call)
	call.Reply(true, call.Payload)
	self.log("MESSAGE SENT ", call)
}

func (self *TestServer) CallTest(src string) *ReplyMessage {
	reply := self.GenServer.Call(src)
	return &reply
}

func (self *TestServer) CastTest(src string) {
	self.GenServer.Cast(CastTestMessage{msg: src})
}

func (self *TestServer) CastCrashTest(src string) {
	self.GenServer.Cast(CastCrashTestMessage{msg: src})
}

// END TestServer Mockup 

func (s *gen_serverTestSuite) TestGenServer01_StartStop() {
  srv := CreateTestServer()
	s.Equal(NEW, srv.GetStatus())
	ok := srv.Start()
	s.Equal(true, ok)
	s.Equal(READY, srv.GetStatus())
	srv.Stop()
	s.Equal(STOPPED, srv.GetStatus())
}

func (s *gen_serverTestSuite) TestGenServer02_CastMessages() {
  srv := CreateTestServer()
	ok := srv.Start()
	s.Equal(true, ok)
	s.Equal(READY, srv.GetStatus())

	srv.CastTest("hello")

	srv.Stop()
	s.Equal(STOPPED, srv.GetStatus())

	s.Equal("hello", srv.last_cast_received.Payload.(CastTestMessage).msg)
}

func (s *gen_serverTestSuite) TestGenServer02_CallMessages() {
  srv := CreateTestServer()
	ok := srv.Start()
	s.Equal(true, ok)
	s.Equal(READY, srv.GetStatus())

	reply := srv.CallTest("WORLD")

	s.Equal(true, reply.Ok)
	s.Equal("WORLD", reply.Result)	

	srv.Stop()
	s.Equal(STOPPED, srv.GetStatus())
}

func (s *gen_serverTestSuite) TestGenServer03_CrashOnCast() {
  srv := CreateTestServer()
	ok := srv.Start()
	s.Equal(true, ok)
	s.Equal(READY, srv.GetStatus())

	srv.CastCrashTest("hello")

	s.Equal(false, srv.Stop().Ok)
	s.Equal(CRASHED, srv.GetStatus())
}