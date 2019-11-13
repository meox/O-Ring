package main

import (
	"math"
	"sort"
)

func copyslice(input []int) []int {
	s := make([]int, len(input))
	copy(s, input)
	return s
}

func sortedCopy(input []int) (copy []int) {
	copy = copyslice(input)
	sort.Ints(copy)
	return
}

func calculateRMS(values []int) float64 {
	mean := calculateMean(values)

	var sum float64 = 0

	for i := 0; i < len(values); i++ {
		sum += math.Pow(float64(values[i])-mean, 2)
	}

	return math.Sqrt(sum / float64(len(values)))
}

func calculateMean(values []int) float64 {
	var avg, sum float64

	for i := 0; i < len(values); i++ {
		sum += float64(values[i])
	}

	avg = sum / float64(len(values))
	return avg
}

func calculateMedian(values []int) float64 {
	sort.Ints(values)

	if len(values)%2 == 0 {
		return float64(values[len(values)/2]+values[len(values)/2-1]) / 2
	}

	return float64(values[(len(values)-1)/2])
}

func calculatePercentile(values []int, percentage int) (percentile float64) {

	if len(values) == 0 {
		return math.NaN()
	}

	if len(values) == 1 {
		return float64(values[0])
	}

	if len(values) <= 0 || percentage > 100 {
		return math.NaN()
	}

	c := sortedCopy(values)

	// Multiply percent by length of input
	index := (float64(percentage) / 100) * float64(len(c))

	// Check if the index is a whole number
	if index == float64(int64(index)) {

		// Convert float to int
		i := int(index)

		// Find the value at the index
		percentile = float64(c[i-1])

	} else if index > 1 {

		// Convert float to int via truncation
		i := int(index)

		// Find the average of the index and following values
		percentile = calculateMean([]int{c[i-1], c[i]})

	} else {
		return math.NaN()
	}

	return percentile

}
