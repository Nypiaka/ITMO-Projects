package _defer

import (
	"fmt"
	"io"
)

type Counter struct {
	writer io.Writer
	value  int
}

func (c *Counter) Increment() {
	c.value++
}

func (c *Counter) Printer() func() {
	value := c.value
	printer := func() {
		_, _ = fmt.Fprint(c.writer, value)
	}
	return printer
}

func PrintSequence(writer io.Writer) {
	counter := Counter{writer, 0}
	counter.Printer()()
}
