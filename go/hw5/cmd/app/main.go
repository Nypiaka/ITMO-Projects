package main

import (
	m "hw3/internal/mark"
	"log"
	"os"
)

func main() {
	file, err := os.Open("./data/input_1.tsv")
	if err != nil {
		log.Fatal("error with reading input file: ", err)
	}
	studentsStatistic, err := m.ReadStudentsStatistic(file)
	if err != nil {
		log.Fatal("error with reading input file: ", err)
	}
	studentsStatistic.Students()
}
