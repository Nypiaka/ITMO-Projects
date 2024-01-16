package mark

import (
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func mockReader(str string) io.Reader {
	return strings.NewReader(str)
}

var (
	readerError = fmt.Errorf("reader err")
	writerError = fmt.Errorf("writer err")
)

type mockReaderWithError struct{}

func (r *mockReaderWithError) Read(p []byte) (n int, err error) {
	return 0, readerError
}

type mockWriterWithError struct{}

func (r *mockWriterWithError) Write(p []byte) (n int, err error) {
	return 0, writerError
}

// ================ SummaryByStudent =========

type TestCase_summaryByStudent struct {
	input         string
	studentName   string
	isWantSummary bool
	wantSummary   int
}

func (tc TestCase_summaryByStudent) Run(t *testing.T) {
	statistics, err := ReadStudentsStatistic(mockReader(tc.input))
	if err != nil {
		t.Errorf("unexpected error")
	}
	gotSummary, isGotSummary := statistics.SummaryByStudent(tc.studentName)
	assert.Equal(t, tc.isWantSummary, isGotSummary)
	if tc.isWantSummary {
		assert.Equal(t, tc.wantSummary, gotSummary)
	}
}

func Test_summaryByStudent_Sum(t *testing.T) {
	TestCase_summaryByStudent{
		input: `Примеров Пример Примерович	5
Примеров Пример Примерович	6
Примеров Пример	7`,
		studentName:   "Примеров Пример Примерович",
		isWantSummary: true,
		wantSummary:   11,
	}.Run(t)
}

func Test_summaryByStudent_Sum_1(t *testing.T) {
	TestCase_summaryByStudent{
		input: `Примеров Пример Примерович1	9
Примеров Пример Примерович2	7
Примеров Пример Примерович1	5
Примеров Пример Примерович1	5
Примеров Пример Примерович1	6
Примеров Пример Примерович2	2`,
		studentName:   "Примеров Пример Примерович1",
		isWantSummary: true,
		wantSummary:   25,
	}.Run(t)
}

func Test_summaryByStudent_Sum_Arabian(t *testing.T) {
	TestCase_summaryByStudent{
		input: `عباس	5
عباس	6
Примеров Пример	7`,
		studentName:   "عباس",
		isWantSummary: true,
		wantSummary:   11,
	}.Run(t)
}

func Test_summaryByStudent_Sum_Skip(t *testing.T) {
	TestCase_summaryByStudent{
		input: `Примеров Пример Примерович1	9
Примеров Пример Примерович2	7
Примеров Пример Примерович1	5
Примеров Пример Примерович1	5
Примеров Пример Примерович1
Примеров Пример Примерович1	6
Примеров Пример Примерович2	2`,
		studentName:   "Примеров Пример Примерович1",
		isWantSummary: true,
		wantSummary:   25,
	}.Run(t)
}

func Test_summaryByStudent_NoStudent(t *testing.T) {
	TestCase_summaryByStudent{
		input:         `Примеров Пример Примерович1	9`,
		studentName:   "Примеров Пример Примерович2",
		isWantSummary: false,
		wantSummary:   0,
	}.Run(t)
}

// ================ AverageByStudent =========

type TestCase_averageByStudent struct {
	input         string
	studentName   string
	isWantAverage bool
	wantAverage   float32
}

func (tc TestCase_averageByStudent) Run(t *testing.T) {
	statistics, err := ReadStudentsStatistic(mockReader(tc.input))
	if err != nil {
		t.Errorf("unexpected error")
	}
	gotAverage, isGotAverage := statistics.AverageByStudent(tc.studentName)
	assert.Equal(t, tc.isWantAverage, isGotAverage)
	if tc.isWantAverage {
		assert.Equal(t, tc.wantAverage, gotAverage)
	}
}

func Test_AveragebyStudent_Sum(t *testing.T) {
	TestCase_averageByStudent{
		input: `Примеров Пример Примерович1	9
Примеров Пример Примерович2	7
Примеров Пример Примерович1	5
Примеров Пример Примерович1	6
Примеров Пример Примерович2	2`,
		studentName:   "Примеров Пример Примерович1",
		isWantAverage: true,
		wantAverage:   6.67,
	}.Run(t)
}

func Test_AveragebyStudent_Sum_Natural(t *testing.T) {
	TestCase_averageByStudent{
		input:         `Примеров Пример Примерович1	9`,
		studentName:   "Примеров Пример Примерович1",
		isWantAverage: true,
		wantAverage:   9,
	}.Run(t)
}

func Test_AveragebyStudent_NoStudent(t *testing.T) {
	TestCase_averageByStudent{
		input:         `Примеров Пример Примерович1	9`,
		studentName:   "Примеров Пример Примерович2",
		isWantAverage: false,
		wantAverage:   0,
	}.Run(t)
}

// ================ Students =================

type TestCase_students struct {
	input        string
	wantStudents []string
}

func (tc TestCase_students) Run(t *testing.T) {
	statistics, err := ReadStudentsStatistic(mockReader(tc.input))
	if err != nil {
		t.Errorf("unexpected error")
	}
	gotStudents := statistics.Students()
	assert.Equal(t, tc.wantStudents, gotStudents)
}

func Test_Students_OneStudent(t *testing.T) {
	TestCase_students{
		input:        `Примеров Пример Примерович1	9`,
		wantStudents: []string{"Примеров Пример Примерович1"},
	}.Run(t)
}

func Test_Students_SomeStudents(t *testing.T) {
	TestCase_students{
		input: `Примеров Пример Примерович1	9
Примеров Пример Примерович2	7
Примеров Пример Примерович1	5
Примеров Пример Примерович1	5
Примеров Пример Примерович3	1
Примеров Пример Примерович1	6
Примеров Пример Примерович2	2`,
		wantStudents: []string{
			"Примеров Пример Примерович1",
			"Примеров Пример Примерович2",
			"Примеров Пример Примерович3",
		},
	}.Run(t)
}

func Test_Students_IgnoreBadLines(t *testing.T) {
	TestCase_students{
		input: `Примеров Пример Примерович1	9
Примеров Пример Примерович2	7
Примеров Пример Примерович1	5
Примеров Пример Примерович1	5
Примеров Пример Примерович3	1
Примеров Пример Примерович4
Примеров Пример Примерович5	42
Примеров Пример Примерович1	6
Примеров Пример Примерович2	2`,
		wantStudents: []string{
			"Примеров Пример Примерович1",
			"Примеров Пример Примерович2",
			"Примеров Пример Примерович3",
		},
	}.Run(t)
}

// ================ Summary ==================

type TestCase_summary struct {
	input       string
	wantSummary int
}

func (tc TestCase_summary) Run(t *testing.T) {
	statistics, err := ReadStudentsStatistic(mockReader(tc.input))
	if err != nil {
		t.Errorf("unexpected error")
	}
	gotSummary := statistics.Summary()
	assert.Equal(t, tc.wantSummary, gotSummary)
}

func Test_Summary_OneStudent(t *testing.T) {
	TestCase_summary{
		input:       `Примеров Пример Примерович1	9`,
		wantSummary: 9,
	}.Run(t)
}

func Test_Summary_SomeStudents(t *testing.T) {
	TestCase_summary{
		input: `Примеров Пример Примерович1	9
Примеров Пример Примерович2	7
Примеров Пример Примерович1	5
Примеров Пример Примерович1	5
Примеров Пример Примерович3	1
Примеров Пример Примерович1	6
Примеров Пример Примерович2	2`,
		wantSummary: 35,
	}.Run(t)
}

// ================ Median ===================

type TestCase_median struct {
	input      string
	wantMedian int
}

func (tc TestCase_median) Run(t *testing.T) {
	statistics, err := ReadStudentsStatistic(mockReader(tc.input))
	if err != nil {
		t.Errorf("unexpected error")
	}
	gotMedian := statistics.Median()
	assert.Equal(t, tc.wantMedian, gotMedian)
}

func Test_Median_OneStudent(t *testing.T) {
	TestCase_median{
		input:      `Примеров Пример Примерович1	9`,
		wantMedian: 9,
	}.Run(t)
}

func Test_Median_SomeStudentsEven(t *testing.T) {
	TestCase_median{
		input: `Примеров Пример Примерович1	2
Примеров Пример Примерович2	1
Примеров Пример Примерович1	6
Примеров Пример Примерович1	4
Примеров Пример Примерович1	3
Примеров Пример Примерович3	5`,
		wantMedian: 4,
	}.Run(t)
}

func Test_Median_SomeStudentsOdd(t *testing.T) {
	TestCase_median{
		input: `Примеров Пример Примерович1	2
Примеров Пример Примерович2	1
Примеров Пример Примерович2	7
Примеров Пример Примерович1	6
Примеров Пример Примерович1	4
Примеров Пример Примерович1	3
Примеров Пример Примерович3	5`,
		wantMedian: 4,
	}.Run(t)
}

// ================ MostFrequent =============

type TestCase_mostFrequent struct {
	input            string
	wantMostFrequent int
}

func (tc TestCase_mostFrequent) Run(t *testing.T) {
	statistics, err := ReadStudentsStatistic(mockReader(tc.input))
	if err != nil {
		t.Errorf("unexpected error")
	}
	gotMostFrequent := statistics.MostFrequent()
	assert.Equal(t, tc.wantMostFrequent, gotMostFrequent)
}

func Test_MostFrequent_UniqMarks(t *testing.T) {
	TestCase_mostFrequent{
		input: `Примеров Пример Примерович1	2
Примеров Пример Примерович2	1
Примеров Пример Примерович2	7
Примеров Пример Примерович1	6
Примеров Пример Примерович1	4
Примеров Пример Примерович1	3
Примеров Пример Примерович3	5`,
		wantMostFrequent: 7,
	}.Run(t)
}

func Test_MostFrequent_RepeatedMarks(t *testing.T) {
	TestCase_mostFrequent{
		input: `Примеров Пример Примерович1	9
Примеров Пример Примерович2	5
Примеров Пример Примерович1	5
Примеров Пример Примерович1	5
Примеров Пример Примерович3	1
Примеров Пример Примерович1	1
Примеров Пример Примерович2	2`,
		wantMostFrequent: 5,
	}.Run(t)
}

func Test_MostFrequent_RepeatedSameTimesMarks(t *testing.T) {
	TestCase_mostFrequent{
		input: `Примеров Пример Примерович1	1
Примеров Пример Примерович3	5
Примеров Пример Примерович2	1
Примеров Пример Примерович1	5
Примеров Пример Примерович1	3
Примеров Пример Примерович1	3`,
		wantMostFrequent: 5,
	}.Run(t)
}

// ================ ReadStudentsStatistic ====

func Test_ReadStudentsStatistic_Err(t *testing.T) {
	errReader := &mockReaderWithError{}
	_, err := ReadStudentsStatistic(errReader)
	assert.Equal(t, readerError, err)
}

// ================ WriteStudentsStatistic ===

type TestCase_writeStudentsStatistic struct {
	input     string
	output    string
	writer    io.Writer
	wantError bool
	err       error
}

func (tc TestCase_writeStudentsStatistic) Run(t *testing.T) {
	statistics, err := ReadStudentsStatistic(mockReader(tc.input))
	if err != nil {
		t.Errorf("unexpected error")
	}
	err = WriteStudentsStatistic(tc.writer, statistics)
	if tc.wantError {
		assert.Equal(t, tc.err, err)
		return
	}
	stringer, ok := tc.writer.(fmt.Stringer)
	if !ok {
		t.Errorf("writer not a stringer")
	}
	assert.Equal(t, tc.output, stringer.String())
}

func Test_writeStudentsStatistic_Err(t *testing.T) {
	TestCase_writeStudentsStatistic{
		input:     `Примеров Пример Примерович1	2`,
		writer:    &mockWriterWithError{},
		wantError: true,
		err:       writerError,
	}.Run(t)
}

func Test_writeStudentsStatistic_output(t *testing.T) {
	TestCase_writeStudentsStatistic{
		input: `Примеров Пример Примерович	5
Примеров Пример Примерович	6
Примеров Пример	7`,
		output: `18	6	7
Примеров Пример Примерович	11	5.50
Примеров Пример	7	7`,
		writer: &strings.Builder{},
	}.Run(t)
}
