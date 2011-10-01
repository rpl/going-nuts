package main

import (
	"log"
)

type Point struct {
	x int
	y int
}

func (p *Point) Abs() int {
	return p.x * p.y
}

func main() {
	p1 := Point{y:2,x:3}
	log.Println("Prova",p1,p1.Abs())
}