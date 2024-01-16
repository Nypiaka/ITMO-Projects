package fact

import (
	"bufio"
	"bytes"
	"fmt"
	"reflect"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"
)

func NewInputTest(NumsOfGoroutine int, Numbers []int) Input {
	return Input{
		NumsOfGoroutine: NumsOfGoroutine,
		Numbers:         Numbers,
	}
}

func strToInt(str string) (int, error) {
	return strconv.Atoi(strings.TrimSpace(str))
}

func delimiterStringsToSliceInt(delimiterStrings []string) ([]int, error) {
	delimiters := make([]int, 0, len(delimiterStrings))
	for _, dString := range delimiterStrings {
		num, err := strToInt(dString)
		if err != nil {
			return nil, err
		}
		delimiters = append(delimiters, num)
	}

	return delimiters, nil
}

func parseLine(line string) (int, int, []int, error) {
	lineParts := strings.Split(line, ",")
	reduceSpaces := regexp.MustCompile(`\s+`).ReplaceAllString(lineParts[0], " ")
	numLine, err := strToInt(strings.Split(reduceSpaces, " ")[1])
	if err != nil {
		return 0, 0, nil, fmt.Errorf("line =  %s, error: %w", line, err)
	}

	equationParts := strings.Split(lineParts[1], "=")
	if len(equationParts) != 2 {
		return 0, 0, nil, fmt.Errorf("there must be exatly one \"=\" symbol in  the line")
	}
	num, err := strToInt(equationParts[0])
	if err != nil {
		return 0, 0, nil, fmt.Errorf("line =  %s, error: %w", line, err)
	}

	delimiterStrings := strings.Split(equationParts[1], "*")
	delimiters, err := delimiterStringsToSliceInt(delimiterStrings)
	if err != nil {
		return 0, 0, nil, fmt.Errorf("line = \"%s\", error: %w", line, err)
	}

	if !checkDelimitersFormat(num, delimiters) {
		return 0, 0, nil, fmt.Errorf("wrong delimeters format")
	}

	return numLine, num, delimiters, nil
}

func checkDelimitersFormat(num int, delimiters []int) bool {
	if len(delimiters) < 1 {
		return false
	}

	if num == 1 && len(delimiters) != 1 && delimiters[0] != 1 {
		return false
	}

	if num == -1 && len(delimiters) != 2 && delimiters[0] != -1 && delimiters[1] != 1 {
		return false
	}

	if num < 0 && delimiters[0] != -1 || num > 1 && delimiters[0] < 2 {
		return false
	}

	if num < -1 || num > 1 {
		for i := 1; i < len(delimiters); i++ {
			if delimiters[i] < 2 || delimiters[i-1] > delimiters[i] {
				return false
			}
		}
	}

	return true
}

func checkFactorization(num int, delimiters []int) bool {
	if !sort.SliceIsSorted(delimiters, func(i, j int) bool { return delimiters[i] < delimiters[j] }) {
		return false
	}
	got := 1
	for _, d := range delimiters {
		got *= d
	}
	return num == got
}

// ================ factorizationCorrectness =========

type TestCase_factorizationCorrectness struct {
	numGoroutine int
	input        []int
}

func (tc TestCase_factorizationCorrectness) Run(t *testing.T) {
	factorization := NewFactorization()
	input := NewInputTest(tc.numGoroutine, tc.input)
	RWBuf := bytes.NewBufferString("")
	err := factorization.Work(input, RWBuf)
	if err != nil {
		t.Errorf("factorization.Work error: %v", err)
		return
	}
	scanner := bufio.NewScanner(RWBuf)
	for scanner.Scan() {
		line := scanner.Text()
		_, num, delimiters, err := parseLine(line)
		if err != nil {
			t.Errorf("parseLine error: %v", err)
			return
		}
		checkFactorization(num, delimiters)
	}

	if scanner.Err() != nil {
		t.Errorf("scanner error: %v", scanner.Err())
		return
	}
}

func Test_TestCase_factorizationCorrectness_NaturalNums(t *testing.T) {
	TestCase_factorizationCorrectness{
		numGoroutine: 5,
		input:        []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
	}.Run(t)
}

func Test_TestCase_factorizationCorrectness_ZNums(t *testing.T) {
	TestCase_factorizationCorrectness{
		numGoroutine: 5,
		input:        []int{-10, -225, -100, 250, 9, 7, 5, 3, -117, -1},
	}.Run(t)
}

// ================ lineNumberCorrectness =========

type TestCase_lineNumberCorrectness struct {
	numGoroutine int
	input        []int
}

func (tc TestCase_lineNumberCorrectness) Run(t *testing.T) {
	factorization := NewFactorization()
	input := NewInputTest(tc.numGoroutine, tc.input)
	RWBuf := bytes.NewBufferString("")
	err := factorization.Work(input, RWBuf)
	if err != nil {
		t.Errorf("factorization.Work error: %v", err)
		return
	}
	scanner := bufio.NewScanner(RWBuf)
	i := 1
	for scanner.Scan() {
		line := scanner.Text()
		lineNum, _, _, err := parseLine(line)
		if err != nil {
			t.Errorf("parseLine error: %v", err)
			return
		}
		if lineNum != i {
			t.Errorf("want lineNum = %d, but got %d", i, lineNum)
			return
		}
		i++
	}

	if scanner.Err() != nil {
		t.Errorf("scanner error: %v", scanner.Err())
	}
}

func Test_TestCase_lineNumberCorrectness_NaturalNums(t *testing.T) {
	TestCase_lineNumberCorrectness{
		numGoroutine: 5,
		input:        []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100},
	}.Run(t)
}

func Test_TestCase_lineNumberCorrectness_ZNums(t *testing.T) {
	TestCase_lineNumberCorrectness{
		numGoroutine: 5,
		input:        []int{-10, -225, -100, 250, 9, 7, 5, 3, -117, -1},
	}.Run(t)
}

// ================ setNumbersCorrectness =========

type TestCase_setNumbersCorrectness struct {
	numGoroutine int
	input        []int
}

func (tc TestCase_setNumbersCorrectness) Run(t *testing.T) {
	factorization := NewFactorization()
	input := NewInputTest(tc.numGoroutine, tc.input)
	RWBuf := bytes.NewBufferString("")
	err := factorization.Work(input, RWBuf)
	if err != nil {
		t.Errorf("factorization.Work error: %v", err)
		return
	}
	scanner := bufio.NewScanner(RWBuf)
	allNums := make([]int, 0, len(tc.input))
	for scanner.Scan() {
		line := scanner.Text()
		_, num, _, err := parseLine(line)
		if err != nil {
			t.Errorf("parseLine error: %v", err)
			return
		}
		allNums = append(allNums, num)
	}
	if scanner.Err() != nil {
		t.Errorf("scanner error: %v", scanner.Err())
		return
	}
	sort.Ints(allNums)
	sort.Ints(tc.input)
	if !reflect.DeepEqual(allNums, tc.input) {
		t.Errorf("want nums %v,\n got nums %v \n", tc.input, allNums)
	}
}

func Test_TestCase_setNumbersCorrectness_NaturalNums(t *testing.T) {
	TestCase_setNumbersCorrectness{
		numGoroutine: 5,
		input:        []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
	}.Run(t)
}

func Test_TestCase_setNumbersCorrectness_ZNums(t *testing.T) {
	TestCase_setNumbersCorrectness{
		numGoroutine: 5,
		input:        []int{-10, -225, -100, 250, 250, 9, 7, 5, 3, -117, -1},
	}.Run(t)
}

// ================ delimitersOrderCorrectness =========

type TestCase_delimitersOrderCorrectness struct {
	numGoroutine int
	input        []int
}

func (tc TestCase_delimitersOrderCorrectness) Run(t *testing.T) {
	factorization := NewFactorization()
	input := NewInputTest(tc.numGoroutine, tc.input)
	RWBuf := bytes.NewBufferString("")
	err := factorization.Work(input, RWBuf)
	if err != nil {
		t.Errorf("factorization.Work error: %v", err)
	}
	scanner := bufio.NewScanner(RWBuf)
	for scanner.Scan() {
		line := scanner.Text()
		_, num, delimiters, err := parseLine(line)
		if err != nil {
			t.Errorf("parseLine error: %v", err)
			return
		}

		if !sort.IntsAreSorted(delimiters) {
			t.Errorf("delimiters are not sorted: %v", delimiters)
			return
		}

		if num < 0 {
			if len(delimiters) < 2 {
				t.Errorf("too few delimiters: %v", delimiters)
				return
			}
			if delimiters[0] != -1 || delimiters[1] <= 0 {
				t.Errorf("first delimiter must be -1, from the second all must be positive, got: %v", delimiters)
				return
			}
			if len(delimiters) > 2 {
				if delimiters[1] == 1 {
					t.Errorf("1 is not a delimiter: %v", delimiters)
					return
				}
			}
			continue
		}

		if len(delimiters) > 1 && delimiters[0] == 1 {
			t.Errorf("1 is not a delimiter: %v", delimiters)
		}
	}
	if scanner.Err() != nil {
		t.Errorf("scanner error: %v", scanner.Err())
	}
}

func Test_TestCase_delimitersOrderCorrectness_NaturalNums(t *testing.T) {
	TestCase_delimitersOrderCorrectness{
		numGoroutine: 5,
		input:        []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
	}.Run(t)
}

func Test_TestCase_delimitersOrderCorrectness_ZNums(t *testing.T) {
	TestCase_delimitersOrderCorrectness{
		numGoroutine: 5,
		input:        []int{-10, -225, -100, 250, 250, 9, 7, 5, 3, -117, -1},
	}.Run(t)
}

// ================ concurrentOutput =========

func bigInput() []int {
	sizeN := 100_000_000
	slice := make([]int, sizeN)

	for i := range slice {
		slice[i] = i + 1
	}

	return slice
}

type TestCase_concurrentOutput struct {
	numGoroutine int
	input        []int
}

func (tc TestCase_concurrentOutput) Run(t *testing.T) {
	factorization := NewFactorization()
	input := NewInputTest(tc.numGoroutine, tc.input)
	RWBuf := bytes.NewBufferString("")
	go func() {
		err := factorization.Work(input, RWBuf)
		if err != nil {
			t.Errorf("factorization.Work error: %v", err)
		}
	}()
	time.Sleep(10 * time.Millisecond)
	scanner := bufio.NewScanner(RWBuf)
	if !scanner.Scan() {
		t.Errorf("writer is not concurrent")
	}
}

func Test_TestCase_concurrentOutput_NGoroutine(t *testing.T) {
	TestCase_concurrentOutput{
		numGoroutine: 5,
		input:        bigInput(),
	}.Run(t)
}

func Test_TestCase_concurrentOutput_OneGoroutine(t *testing.T) {
	TestCase_concurrentOutput{
		numGoroutine: 1,
		input:        bigInput(),
	}.Run(t)
}
