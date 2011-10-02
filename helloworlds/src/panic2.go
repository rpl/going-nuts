package main

import (
	"log"
	"time"
	"runtime"
)

func goroutine1() {
	defer func() {
		if err := recover(); err != nil {
			log.Println("RECOVERED FROM PANIC:",err)
			for i := 2; ; i++ {
				_, file, line, ok := runtime.Caller(i);
				if ok {
					log.Printf("%s : %d\n", file, line);
				} else {
					break;
				}
			} 
		}
	}()

	for ;; {
		log.Print("GOROUTINE1")
		time.Sleep(10*1e8)
		unsafefunc1()
	}
}

func unsafefunc1() {
	panic("PANIC")
}

func main() {
	go goroutine1()
	time.Sleep(10*1e9)
}