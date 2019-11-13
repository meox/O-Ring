package main

import (
	"errors"
	"strconv"
	"strings"
)

// Range A range of values splitted by a step
type Range struct {
	start int
	stop  int
	step  int
}

func parseRange(r string) (Range, error) {
	values := strings.Split(r, ":")

	var zero = Range{start: 0, stop: 0, step: 0}

	if len(values) != 3 {
		return zero, errors.New("No valid range")
	}

	start, err := strconv.Atoi(values[0])
	if err != nil {
		return zero, errors.New("cannot parse start value")
	}

	stop, err := strconv.Atoi(values[1])
	if err != nil {
		return zero, errors.New("cannot parse stop value")
	}

	step, err := strconv.Atoi(values[2])
	if err != nil {
		return zero, errors.New("cannot parse step value")
	}

	return Range{
		start: start,
		stop:  stop,
		step:  step,
	}, nil
}

func calculateSteps(r Range) []int {
	var values []int
	var i int

	for i = r.start; i <= r.stop; i += r.step {
		values = append(values, i)
	}

	if r.stop-values[len(values)-1] > 0 {
		values = append(values, r.stop-values[len(values)-1])
	}

	return values
}
