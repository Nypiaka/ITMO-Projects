package fact

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
	"sync"
)

type Input struct {
	NumsOfGoroutine int   // n - число горутин
	Numbers         []int // слайс чисел, которые необходимо факторизовать
}

type Factorization interface {
	Work(Input, io.Writer) error
}

type FactorizationImpl struct{}

func (f *FactorizationImpl) Work(input Input, writer io.Writer) error {
	newWriter := bufio.NewWriter(writer)
	var wg sync.WaitGroup
	threads := min(input.NumsOfGoroutine, len(input.Numbers))
	wg.Add(threads)
	ops := 1
	var l sync.Mutex
	var errors []error
	for i := 0; i < threads; i++ {
		go count(i, threads, &input.Numbers, &wg, newWriter, &ops, &l, &errors)
	}
	wg.Wait()
	if len(errors) > 0 {
		return errors[0]
	}
	return nil
}

func count(index, threads int, numbers *[]int, wg *sync.WaitGroup, writer *bufio.Writer, ops *int, l *sync.Mutex, errors *[]error) {
	for index < len(*numbers) {
		decomposed := decompose((*numbers)[index])
		l.Lock()
		_, werr := writer.WriteString("line " + strconv.Itoa(*ops) + ", " + strconv.Itoa((*numbers)[index]) + " = " +
			strings.Trim(strings.Join(strings.Fields(fmt.Sprint(decomposed)), " * "), "[]") + "\n")
		ferr := writer.Flush()
		*ops++
		if werr != nil {
			*errors = append(*errors, werr)
		}
		if ferr != nil {
			*errors = append(*errors, ferr)
		}

		l.Unlock()
		index += threads
	}
	wg.Done()
}

func decompose(num int) []int {
	if num == 0 || num == 1 {
		return []int{num}
	}

	if num == -1 {
		return []int{-1, 1}
	}

	var result []int
	if num < 0 {
		num = -num
		result = append(result, -1)
	}
	divider := 2
	for {
		if num%divider == 0 {
			result = append(result, divider)
			num /= divider
		} else {
			divider++
		}
		if num == 1 {
			break
		}
	}
	return result
}

func NewFactorization() Factorization {
	return &FactorizationImpl{}
}
