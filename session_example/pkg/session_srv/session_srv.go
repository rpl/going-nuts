package session_srv

import ("gen_server")

type SessionSrv struct {
	*gen_server.GenServer
	last_cast_received *gen_server.CastMessage
}

type CastTestMessage struct {
	msg string
}

type CastCrashTestMessage struct {
	msg string
}

