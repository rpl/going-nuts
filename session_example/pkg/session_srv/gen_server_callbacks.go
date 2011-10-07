package session_srv

import (
	"gen_server"
	"log"
	"container/vector"
)

func (self *SessionSrv) Init(args gen_server.Data) (bool, gen_server.State) {
	return true, &vector.IntVector{}
}

func (self *SessionSrv) HandleCast(cast *gen_server.CastMessage, state gen_server.State) {
	self.last_cast_received = cast
	switch payload := cast.Payload.(type) {
	case CastCrashTestMessage:
		panic("panic inside the gen_server impl")
	}
}

func (self *SessionSrv) HandleCall(call *gen_server.CallMessage, state gen_server.State) {
	log.Print("CURRENT STATE ",state)
	v1 := state.(*vector.IntVector)
	v1.Push(4)
	call.Reply(true, call.Payload)
}