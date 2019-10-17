package main

import (
	"fmt"
	"os"
	"strconv"
	"time"
)

type chanT chan int

func main() {
	if len(os.Args) != 3 {
		fmt.Println("./ring N:<number of process> M:<trips>")
		os.Exit(1)
	}

	n, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Println("cannot parse number of process")
	}

	m, err := strconv.Atoi(os.Args[2])
	if err != nil {
		fmt.Println("cannot parse number of trips")
	}

	//fmt.Println("Creating Ring")
	t0 := time.Now()
	s, e := createRing(n)
	//t1 := time.Now()
	//fmt.Printf("Ring created in: %d ms\n", t1.Sub(t0).Milliseconds())

	for i := 0; i < m; i++ {
		s <- i
		_ = <-e
	}
	//t2 := time.Now()

	//fmt.Printf(
	//	"runned in: %d ms, tot: %dms\n",
	//	t2.Sub(t1).Milliseconds(),
	//	t2.Sub(t0).Milliseconds(),
	//)
	fmt.Printf("%d\n", time.Since(t0).Milliseconds())
}

func createRing(n int) (chanT, chanT) {
	startChannel := make(chanT)

	node := func(src, dst chanT) {
		for {
			msg := <-src
			dst <- msg
		}
	}

	src := startChannel
	var dst chanT
	for i := 0; i < n; i++ {
		dst = make(chanT)
		go node(src, dst)
		src = dst
	}

	return startChannel, dst
}
