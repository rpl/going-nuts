package main

import (
	"fmt"
	"session_srv"
)

func main() {
  fmt.Printf("hello, world\n")
  srv := session_srv.CreateSessionSrv()
	srv.Start()
	srv.CallTest("HELLO")
	srv.CallTest("WORLD")
	srv.CallTest("WORLD")
	srv.CallTest("WORLD")
	srv.Stop()
}
