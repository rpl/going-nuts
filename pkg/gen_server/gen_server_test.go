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

func (self *TestServer) HandleCast(msg *CastMessage) {
	self.log("HANDLE CAST ", msg)
	self.last_cast_received = msg
}

func (self *TestServer) HandleCall(msg *CallMessage) {
	self.log("HANDLE CALL ", msg)
	msg.Reply(true, "WORLD")
	self.log("MESSAGE SENT ", msg)
}

func (self *TestServer) CallTest1(src string) Data {
	reply := self.GenServer.Call("p1", src)
	return reply.Result
}

func (self *TestServer) CastTest2(src string) {
	self.GenServer.Cast("p2", src)
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

	srv.Cast("hello", "gen_server")

	srv.Stop()
	s.Equal(STOPPED, srv.GetStatus())

	s.Equal("hello", srv.last_cast_received.Name)
	s.Equal("gen_server", srv.last_cast_received.Args)
}

func (s *gen_serverTestSuite) TestGenServer02_CallMessages() {
  srv := CreateTestServer()
	ok := srv.Start()
	s.Equal(true, ok)
	s.Equal(READY, srv.GetStatus())

	reply := srv.Call("hello", "gen_server")

	s.Equal(true, reply.Ok)
	s.Equal("WORLD", reply.Result)	

	srv.Stop()
	s.Equal(STOPPED, srv.GetStatus())
}
