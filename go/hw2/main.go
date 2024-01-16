package main

import (
	"math"
)

func getCharByIndex(str string, idx int) rune {
	return []rune(str)[idx]
}

func getStringBySliceOfIndexes(str string, indexes []int) string {
	if indexes == nil {
		return str
	}
	result := make([]rune, len(indexes))
	runeFromStr := []rune(str)
	for i, elem := range indexes {
		result[i] = runeFromStr[elem]
	}
	return string(result)
}

func addPointers(ptr1, ptr2 *int) *int {
	if ptr1 == nil || ptr2 == nil {
		return nil
	}
	sum := *ptr1 + *ptr2
	return &sum
}

func isComplexEqual(a, b complex128) bool {
	magicConst := 0.0001
	return math.Abs(real(a)-real(b)) < magicConst && math.Abs(imag(a)-imag(b)) < magicConst
}

func getRootsOfQuadraticEquation(a, b, c float64) (complex128, complex128) {
	if a == 0 && (b == 0 || (b != 0 && c == 0)) {
		return 0i, 0i
	}
	if a == 0 && c != 0 {
		return complex(-c/b, 0), complex(-c/b, 0)
	}
	discriminant := b*b - 4*a*c
	sqrt := math.Sqrt(math.Abs(discriminant))
	if discriminant > 0 {
		return complex((-b+sqrt)/(2*a), 0), complex((-b-sqrt)/(2*a), 0)
	} else {
		return complex((-b)/(2*a), sqrt/(2*a)), complex((-b)/(2*a), -sqrt/(2*a))
	}
}

func mergeSortRecursive(s []int) []int {
	if s == nil {
		return s
	}
	if len(s) > 1 {

		mid := len(s) / 2

		L := make([]int, mid)

		R := make([]int, len(s)-mid)

		copy(L, s[:mid])

		copy(R, s[mid:])

		L = mergeSortRecursive(L)

		R = mergeSortRecursive(R)

		i, j, k := 0, 0, 0

		for i < len(L) && j < len(R) {
			if L[i] <= R[j] {
				s[k] = L[i]
				i++
			} else {
				s[k] = R[j]
				j++
			}
			k += 1
		}

		for i < len(L) {
			s[k] = L[i]
			i++
			k++
		}
		for j < len(R) {
			s[k] = R[j]
			j += 1
			k += 1
		}
	}
	return s
}

func mergeSort(s []int) []int {
	return mergeSortRecursive(s)
}

func reverseSliceOne(s []int) {
	i := 0
	for i < len(s)/2 {
		s[i], s[len(s)-i-1] = s[len(s)-i-1], s[i]
		i++
	}
}

func reverseSliceTwo(s []int) []int {
	if s == nil {
		return nil
	}
	newS := make([]int, len(s))
	copy(newS, s)
	reverseSliceOne(newS)
	return newS
}

func swapPointers(a, b *int) {
	*a, *b = *b, *a
}

func isSliceEqual(a, b []int) bool {
	if a == nil && b == nil {
		return true
	}
	if a != nil && b != nil {
		if len(a) != len(b) {
			return false
		}
		for i := range a {
			if a[i] != b[i] {
				return false
			}
		}
		return true
	}
	return false
}

func deleteByIndex(s []int, idx int) []int {
	if s == nil || idx < 0 || len(s) < idx {
		return s
	}
	return append(s[:idx], s[idx+1:]...)
}
