package main

import (
	"fmt"
	"os"
	"strconv"
)

func main() {
	if len(os.Args) != 4 {
		fmt.Println("./ring <repetitions for every test> <range: number of nodes> <range: number of trips>\n\tRange format: start:stop:step")
		os.Exit(1)
	}

	numberOfTestRepetitions, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Println("cannot parse number of repetitions")
	}

	nodesRange, err := parseRange(os.Args[2])
	if err != nil {
		fmt.Println("cannot parse number of process")
	}

	tripsRange, err := parseRange(os.Args[3])
	if err != nil {
		fmt.Println("cannot parse number of trips")
	}

	nodesSteps := calculateSteps(nodesRange)
	tripsSteps := calculateSteps(tripsRange)

	var languages = map[string]string{
		"cpp":      "../cppring/",
		"go":       "../goring/",
		"erlang":   "../erlring/",
		"elixir":   "../exring/",
		"haskell":  "../haskring",
		"ponylang": "../ponyring",
	}

	for language, path := range languages {
		statisticalResults := make(map[int]map[int]StatisticTestResult)

		fmt.Printf("%s start\n", language)
		for _, n := range nodesSteps {
			statisticalResults[n] = make(map[int]StatisticTestResult)
			for _, s := range tripsSteps {
				fmt.Printf("\tnodes=%d & trips=%d\n", n, s)
				statisticalResult := executeStatisticTest(path, n, s, numberOfTestRepetitions)
				statisticalResults[n][s] = statisticalResult
			}
		}
		fmt.Printf("%s stop\n", language)
		writeOutput("./output", language, statisticalResults)
	}

}
