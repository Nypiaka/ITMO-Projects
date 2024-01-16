package mark

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"sort"
	"strconv"
	"strings"
)

type Student struct {
	Name string
	Mark int
}

type ToIntPair[T any] struct {
	Key   T
	Value int
}

type ByKey[T any] []ToIntPair[T]

func (s ByKey[T]) Len() int {
	return len(s)
}

func (s ByKey[T]) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func (s ByKey[T]) Less(i, j int) bool {
	return s[i].Value > s[j].Value
}

type StudentsStatistic interface {
	SummaryByStudent(student string) (int, bool)     // default_value, false - если студента нет
	AverageByStudent(student string) (float32, bool) // default_value, false - если студента нет
	Students() []string
	Summary() int
	Median() int
	MostFrequent() int
}

type StudentsStatisticsImpl struct {
	marksNum        int
	sum             int
	mostPopularMark ToIntPair[int]
	studentToSum    map[string]ToIntPair[int]
	markToNum       []int
}

func (s *StudentsStatisticsImpl) SummaryByStudent(student string) (int, bool) {
	pair, success := s.studentToSum[student]
	if success {
		return pair.Value, true
	}
	return 0, false
}

func (s *StudentsStatisticsImpl) AverageByStudent(student string) (float32, bool) {
	pair, success := s.studentToSum[student]
	if success {
		return float32(math.Round(float64(float32(pair.Value)/float32(pair.Key)*100)) / 100), true
	}
	return 0, false
}

func (s *StudentsStatisticsImpl) Students() []string {
	pairs := make([]ToIntPair[string], len(s.studentToSum))
	i := 0
	for k := range s.studentToSum {
		pairs[i] = ToIntPair[string]{k, s.studentToSum[k].Value}
		i++
	}
	sort.Sort(ByKey[string](pairs))
	names := make([]string, len(s.studentToSum))
	for j, k := range pairs {
		names[j] = k.Key
	}
	return names

}

func (s *StudentsStatisticsImpl) Summary() int {
	return s.sum
}

func (s *StudentsStatisticsImpl) Median() int {
	var median int
	isOdd := 0
	if s.marksNum%2 != 0 {
		isOdd = 1
	}
	median = s.marksNum/2 + isOdd
	sum := 0
	for j := range s.markToNum {
		if sum+s.markToNum[j]+isOdd > median {
			return j
		} else {
			sum += s.markToNum[j]
		}
	}
	return 0
}

func (s *StudentsStatisticsImpl) MostFrequent() int {
	return s.mostPopularMark.Key
}

func ReadStudentsStatistic(reader io.Reader) (StudentsStatistic, error) {
	sum := 0
	marksNum := 0
	mostPopularMark := ToIntPair[int]{0, 0}
	studentToMarks := map[string]ToIntPair[int]{}
	markToNum := []int{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

	scanner := bufio.NewScanner(reader)
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		line := scanner.Text()
		nameAndMark := strings.Split(line, "\t")
		if len(nameAndMark) >= 2 {
			name := nameAndMark[0]
			text := nameAndMark[1]
			currentMark, err := strconv.Atoi(text)
			if err == nil && currentMark > 0 && currentMark < 11 {
				marksNum++
				sumOfStudent, success := studentToMarks[name]
				if success {
					studentToMarks[name] = ToIntPair[int]{sumOfStudent.Key + 1, sumOfStudent.Value + currentMark}
				} else {
					studentToMarks[name] = ToIntPair[int]{1, currentMark}
				}
				markToNum[currentMark]++
				if markToNum[currentMark] > mostPopularMark.Value ||
					(markToNum[currentMark] == mostPopularMark.Value && currentMark > mostPopularMark.Key) {
					mostPopularMark.Key = currentMark
					mostPopularMark.Value = markToNum[currentMark]
				}
				sum += currentMark
			}
		}
	}
	if scanner.Err() != nil {
		return nil, scanner.Err()
	}
	return &StudentsStatisticsImpl{
		marksNum:        marksNum,
		sum:             sum,
		mostPopularMark: mostPopularMark,
		studentToSum:    studentToMarks,
		markToNum:       markToNum,
	}, nil
}

func WriteStudentsStatistic(writer io.Writer, statistic StudentsStatistic) error {
	newWriter := bufio.NewWriter(writer)
	_, firstLineError := newWriter.WriteString(fmt.Sprintf("%d\t%d\t%d\n", statistic.Summary(), statistic.Median(), statistic.MostFrequent()))
	if firstLineError != nil {
		return firstLineError
	}
	for i, name := range statistic.Students() {
		summary, _ := statistic.SummaryByStudent(name)
		avg, _ := statistic.AverageByStudent(name)
		format := "%s\t%d\t%0.2f"
		if math.Abs(float64(avg)-math.Round(float64(avg))) < 0.01 {
			format = "%s\t%d\t%g"
		}
		_, currentError := newWriter.WriteString(fmt.Sprintf(format, name, summary, avg))
		if i != len(statistic.Students())-1 {
			_, separatorWritingError := newWriter.WriteString("\n")
			if separatorWritingError != nil {
				return separatorWritingError
			}
		}
		if currentError != nil {
			return currentError
		}
	}
	flushError := newWriter.Flush()
	if flushError != nil {
		return flushError
	}
	return nil
}
