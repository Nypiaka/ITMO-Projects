package mark

import (
	"fmt"
	"github.com/stretchr/testify/assert"
	"strings"
	"testing"
)

var (
	stolenReaderError = fmt.Errorf("stolen reader err")
	stolenWriterError = fmt.Errorf("stolen writer err")
)

func SimpleTemplate(
	t *testing.T,
	readResource,
	name string,
	awaitSummaryByStudent,
	awaitSummary,
	awaitMedian,
	awaitMostFrequent int,
	awaitStudents []string,
	awaitAverageByStudent float32) {
	reader := strings.NewReader(readResource)
	a, readError := ReadStudentsStatistic(reader)
	summaryByStudent, _ := a.SummaryByStudent(name)
	averageByStudent, _ := a.AverageByStudent(name)
	assert.Equal(t, awaitSummaryByStudent, summaryByStudent, "wrong summary by student")
	assert.Equal(t, awaitSummary, a.Summary(), "wrong summary")
	assert.Equal(t, awaitMedian, a.Median(), "wrong median")
	assert.Equal(t, awaitMostFrequent, a.MostFrequent(), "wrong most frequent")
	assert.Equal(t, awaitStudents, a.Students(), "wrong list of students")
	assert.Equal(t, awaitAverageByStudent, averageByStudent, "wrong average by student")
	assert.Equal(t, nil, readError)
}

func EmptyTemplate(
	t *testing.T,
	readResource,
	name string) {
	reader := strings.NewReader(readResource)
	a, err := ReadStudentsStatistic(reader)
	summaryByStudent, successSumByStudent := a.SummaryByStudent(name)
	averageByStudent, successAvgByStudent := a.AverageByStudent(name)
	assert.Equal(t, 0, summaryByStudent, "wrong summary by student")
	assert.Equal(t, 0, a.Summary(), "wrong summary")
	assert.Equal(t, 0, a.Median(), "wrong median")
	assert.Equal(t, 0, a.MostFrequent(), "wrong most frequent")
	assert.Equal(t, []string{}, a.Students(), "wrong list of students")
	assert.Equal(t, float32(0), averageByStudent, "wrong average by student")
	assert.Equal(t, false, successSumByStudent)
	assert.Equal(t, false, successAvgByStudent)
	assert.Equal(t, nil, err)
}

func Test_Simple(t *testing.T) {
	SimpleTemplate(
		t,
		`Примеров Пример Примерович1	9
Примеров Пример Примерович2	7
Примеров Пример Примерович1	5
Примеров Пример Примерович1	5
Примеров Пример Примерович3	1
Примеров Пример Примерович4
Примеров Пример Примерович5	42
Примеров Пример Примерович1	6
Примеров Пример Примерович2	2`,
		"Примеров Пример Примерович1",
		25,
		35,
		5,
		5,
		[]string{"Примеров Пример Примерович1", "Примеров Пример Примерович2", "Примеров Пример Примерович3"},
		6.25,
	)
}

func Test_Alone(t *testing.T) {
	SimpleTemplate(
		t,
		`Примеров Пример Примерович1	9`,
		"Примеров Пример Примерович1",
		9,
		9,
		9,
		9,
		[]string{"Примеров Пример Примерович1"},
		9,
	)
}

func Test_Empty(t *testing.T) {
	EmptyTemplate(
		t,
		``,
		"Кто То Ктотович")
}

func Test_Incorrect(t *testing.T) {
	EmptyTemplate(
		t,
		"Это Неправильная Оценка\t123\n"+
			"Здесь Ничего Распознаться Не Должно\n"+
			"10 Хоть Оценка И Верная Но Она На Другой Строке\n"+
			"8 Как И В Этом Примере\n"+
			"Ошибок При Парсинге Кстати Быть Тоже Не Должно 123\n"+
			"Но Это Я Ещё Проверю\t50\n",
		"Это Неправильная Оценка")
}

type stolenReaderMock struct{}

func (r *stolenReaderMock) Read([]byte) (n int, err error) {
	return 0, stolenReaderError
}

type stolenWriterMock struct{}

func (r *stolenWriterMock) Write([]byte) (n int, err error) {
	return 0, stolenWriterError
}

func Test_ReaderError(t *testing.T) {
	_, err := ReadStudentsStatistic(&stolenReaderMock{})
	assert.Equal(t, stolenReaderError, err)
}

func Test_WriterError(t *testing.T) {
	err := WriteStudentsStatistic(&stolenWriterMock{}, &StudentsStatisticsImpl{
		marksNum:        0,
		sum:             0,
		mostPopularMark: ToIntPair[int]{},
		studentToSum:    nil,
		markToNum:       nil,
	})
	assert.Equal(t, stolenWriterError, err)
}

func Test_WriterSimple(t *testing.T) {
	a, b := ReadStudentsStatistic(strings.NewReader(`Примеров Пример Примерович1	9
Примеров Пример Примерович2	7
Примеров Пример Примерович1	5
Примеров Пример Примерович1	5
Примеров Пример Примерович3	1
Примеров Пример Примерович4
Примеров Пример Примерович5	42
Примеров Пример Примерович1	6
Примеров Пример Примерович2	2`))
	writer := &strings.Builder{}
	err := WriteStudentsStatistic(writer, a)
	assert.Equal(t, nil, err)
	assert.Equal(t, nil, b)
	assert.Equal(t, writer.String(), "35\t5\t5\nПримеров Пример Примерович1\t25\t6.25\nПримеров "+
		"Пример Примерович2\t9\t4.50\nПримеров Пример Примерович3\t1\t1")
}
