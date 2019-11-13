package main

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
)

// TestResult Result of a single test
type TestResult struct {
	timeToSetupRing            int
	timeFromFirstToLastMessage int
}

// StatisticTestResult Structure containing results calculated over multiple executions of a test
type StatisticTestResult struct {
	resultsRaw                               []TestResult
	timeToSetupRingMean                      float64
	timeToSetupRingRMS                       float64
	timeToSetupRingMedian                    float64
	timeToSetupRing98thPercentile            float64
	timeFromFirstToLastMessageMean           float64
	timeFromFirstToLastMessageRMS            float64
	timeFromFirstToLastMessageMedian         float64
	timeFromFirstToLastMessage98thPercentile float64
}

func executeStatisticTest(path string, nodes, trips, numberOfRepetitions int) StatisticTestResult {
	var testSetupTimes []int
	var testTimeToCompleteTimes []int
	var testResults []TestResult
	for i := 0; i < numberOfRepetitions; i++ {
		result := executeSingleTest(path, nodes, trips)
		testResults = append(testResults, result)
		testSetupTimes = append(testSetupTimes, result.timeToSetupRing)
		testTimeToCompleteTimes = append(testTimeToCompleteTimes, result.timeFromFirstToLastMessage)
	}
	return StatisticTestResult{
		resultsRaw:                               testResults,
		timeToSetupRingMean:                      calculateMean(testSetupTimes),
		timeToSetupRingRMS:                       calculateRMS(testSetupTimes),
		timeToSetupRingMedian:                    calculateMedian(testSetupTimes),
		timeToSetupRing98thPercentile:            calculatePercentile(testSetupTimes, 98),
		timeFromFirstToLastMessageMean:           calculateMean(testTimeToCompleteTimes),
		timeFromFirstToLastMessageRMS:            calculateRMS(testTimeToCompleteTimes),
		timeFromFirstToLastMessageMedian:         calculateMedian(testTimeToCompleteTimes),
		timeFromFirstToLastMessage98thPercentile: calculatePercentile(testTimeToCompleteTimes, 98),
	}
}

func executeSingleTest(path string, nodes, trips int) TestResult {
	cmd := exec.Command("./ring", strconv.Itoa(nodes), strconv.Itoa(trips))
	cmd.Dir = path
	rawOut, err := cmd.Output()

	if err != nil {
		fmt.Printf("Run error %s\n", err)
		os.Exit(1)
	}

	out := strings.TrimSuffix(string(rawOut), "\n")

	timeToSetupRing, err := strconv.Atoi(strings.Split(out, " ")[0])
	if err != nil {
		fmt.Printf("Error parsing result: %s\n", err)
		os.Exit(1)
	}

	timeFromFirstToLastMessage, err := strconv.Atoi(strings.Split(string(out), " ")[1])
	if err != nil {
		fmt.Printf("Error parsing result: %s\n", err)
		os.Exit(1)
	}

	return TestResult{
		timeToSetupRing:            timeToSetupRing,
		timeFromFirstToLastMessage: timeFromFirstToLastMessage,
	}
}

func writeOutput(outputPath, language string, results map[int]map[int]StatisticTestResult) {
	_ = os.Mkdir(outputPath, os.ModeDir|0777)
	f, err := os.Create(fmt.Sprintf("%s/%s.csv", outputPath, language))
	fRaw, rawErr := os.OpenFile(fmt.Sprintf("%s/raw.dat", outputPath), os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0660)

	if err != nil {
		panic(err)
	} else {
		defer f.Close()
	}

	if rawErr != nil {
		panic(err)
	} else {
		defer fRaw.Close()
	}

	fmt.Fprintln(f, "#nodes,trips,iterations,setup mean[ms],setup RMS,setup median[ms],setup 98th percentile[ms],time to finish mean[ms],time to finish RMS, time to finish median[ms], time to finish 98th percentile[ms]")
	for nodesValue, testsByNodes := range results {
		for tripsValue, tests := range testsByNodes {
			fmt.Fprintf(
				f,
				"%d,%d,%d,%f,%f,%f,%f,%f,%f,%f,%f\n",
				nodesValue,
				tripsValue,
				len(tests.resultsRaw),
				tests.timeToSetupRingMean,
				tests.timeToSetupRingRMS,
				tests.timeToSetupRingMedian,
				tests.timeToSetupRing98thPercentile,
				tests.timeFromFirstToLastMessageMean,
				tests.timeFromFirstToLastMessageRMS,
				tests.timeFromFirstToLastMessageMedian,
				tests.timeFromFirstToLastMessage98thPercentile,
			)

			for idx, testResult := range tests.resultsRaw {
				fmt.Fprintf(
					fRaw,
					"%s %d %d %d %d %d\n",
					language,
					nodesValue,
					tripsValue,
					idx,
					testResult.timeToSetupRing,
					testResult.timeFromFirstToLastMessage,
				)
			}
		}
	}

	f.Sync()
}
