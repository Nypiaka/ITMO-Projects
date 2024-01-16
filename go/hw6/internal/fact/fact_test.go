package fact

import (
	"fmt"
	"regexp"
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"
)

type stolenWriterMock struct{}

var stolenWriterError = fmt.Errorf("stolen writer err")
var slice = []int{-321, 100, -99, 98, 97, 96, 95, 94, 32, 1024 * 1024 * 1024 * 1024 * 1024 * 1024}
var linesBank = [10]string{`line \d+, -99 = -1 \* 3 \* 3 \* 11`,
	`line \d+, 96 = 2 \* 2 \* 2 \* 2 \* 2 \* 3`,
	`line \d+, 32 = 2 \* 2 \* 2 \* 2 \* 2`,
	`line \d+, -321 = -1 \* 3 \* 107`,
	`line \d+, 98 = 2 \* 7 \* 7`,
	`line \d+, 95 = 5 \* 19`,
	`line \d+, 1152921504606846976 = 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2 \* 2`,
	`line \d+, 100 = 2 \* 2 \* 5 \* 5`,
	`line \d+, 97 = 97`,
	`line \d+, 94 = 2 \* 47`,
}

func (r *stolenWriterMock) Write([]byte) (n int, err error) {
	return 0, stolenWriterError
}

type customWriter struct {
	builder []string
}

func (r *customWriter) Write(b []byte) (n int, err error) {
	r.builder = append(r.builder, string(b))
	return len(b), nil
}

func Test_Simple_Error(t *testing.T) {
	slice := []int{-321, 100}
	err := NewFactorization().Work(Input{3, slice}, &stolenWriterMock{})
	assert.Equal(t, stolenWriterError, err)
}

func TestsTemplate(t *testing.T, numOfGorutines int, numbers []int, lines []string) {
	writer := &customWriter{
		builder: make([]string, 0),
	}
	err := NewFactorization().Work(Input{numOfGorutines, numbers}, writer)
	assert.Nil(t, err)
	m := make(map[string]bool)
	for _, s := range writer.builder {
		m[s] = false
	}
	for regex := range lines {
		for key := range m {
			matched, _ := regexp.MatchString(lines[regex], key)
			if matched && !m[key] {
				m[key] = true
			}
		}
	}

	for key := range m {
		assert.True(t, m[key])
	}

	for i := range writer.builder {
		assert.Contains(t, writer.builder[i], "line "+strconv.Itoa(i+1))
	}
}

func Test_Simple(t *testing.T) {
	TestsTemplate(t, 3, slice, linesBank[:])
}

func Test_One_Gorutine(t *testing.T) {
	TestsTemplate(t, 1, slice, linesBank[:])
}

func Test_Many_Gorutines(t *testing.T) {
	TestsTemplate(t, 1000, slice, linesBank[:])
}

func Test_No_numbers(t *testing.T) {
	TestsTemplate(t, 10, make([]int, 0), make([]string, 0))
}

func Test_Corner_Cases(t *testing.T) {
	lines := [3]string{`line \d+, -1 = -1`, `line \d+, 0 = 0`, `line \d+, 1 = 1`}
	TestsTemplate(t, 10, []int{-1, 0, 1}, lines[:])
}
