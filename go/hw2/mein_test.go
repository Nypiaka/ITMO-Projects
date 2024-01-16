package main

import (
	"github.com/stretchr/testify/require"
	"math"
	"sort"
	"testing"
)

func mergeSortTemplateTest(s []int, t *testing.T) {
	result := mergeSort(s)
	sort.Ints(s)
	require.Equal(t, s, result)
}

func reverseSliceOneTemplateTest(s, wait []int, t *testing.T) {
	reverseSliceOne(s)
	require.Equal(t, wait, s)
}

func reverseSliceTwoTemplateTest(s, wait []int, t *testing.T) {
	if s == nil {
		require.Nil(t, reverseSliceTwo(nil))
		return
	}
	copyS := make([]int, len(s))
	copy(copyS, s)
	reversed := reverseSliceTwo(s)
	require.Equal(t, wait, reversed)
	require.Equal(t, copyS, s)
}

func swapPointersTemplateTest(a, b *int, t *testing.T) {
	prevA := *a
	prevB := *b
	swapPointers(a, b)
	require.Equal(t, prevB, *a)
	require.Equal(t, prevA, *b)
}

func wrapSolutionToSlice(a, b, c float64) []complex128 {
	solution1, solution2 := getRootsOfQuadraticEquation(a, b, c)
	return []complex128{solution1, solution2}
}

func Test_mergeSort(t *testing.T) {
	mergeSortTemplateTest([]int{219, 3893, 131, 133193, 130, 0, 0, 0, 130, -130}, t)
	mergeSortTemplateTest([]int{219}, t)
	mergeSortTemplateTest([]int{}, t)
	mergeSortTemplateTest([]int{219, 219, 219, 219, 219}, t)
}

func Test_reverseSliceOne(t *testing.T) {
	reverseSliceOneTemplateTest(nil, nil, t)
	reverseSliceOneTemplateTest(
		[]int{219, 3893, 131, 133193, 130, 0, 0, 0, 130, -130},
		[]int{-130, 130, 0, 0, 0, 130, 133193, 131, 3893, 219}, t)
	reverseSliceOneTemplateTest([]int{219}, []int{219}, t)
	reverseSliceOneTemplateTest([]int{}, []int{}, t)
}

func Test_reverseSliceTwo(t *testing.T) {
	reverseSliceTwoTemplateTest(nil, nil, t)
	reverseSliceTwoTemplateTest(
		[]int{219, 3893, 131, 133193, 130, 0, 0, 0, 130, -130},
		[]int{-130, 130, 0, 0, 0, 130, 133193, 131, 3893, 219}, t)
	reverseSliceTwoTemplateTest([]int{219}, []int{219}, t)
	reverseSliceTwoTemplateTest([]int{}, []int{}, t)
}

func Test_swapPointers(t *testing.T) {
	a, b := 1, 2
	swapPointersTemplateTest(&a, &b, t)
	a, b = 1, 1
	swapPointersTemplateTest(&a, &b, t)
}

func Test_isSliceEqual(t *testing.T) {
	require.True(t, isSliceEqual(nil, nil))
	require.False(t, isSliceEqual(nil, []int{1, 2, 3}))
	require.False(t, isSliceEqual([]int{1, 2, 3}, nil))
	require.False(t, isSliceEqual([]int{1, 2, 3, 4}, []int{1, 2, 3}))
	require.False(t, isSliceEqual([]int{1, 2, 3}, []int{1, 2, 3, 4}))
	require.True(t, isSliceEqual([]int{}, []int{}))
	require.True(t, isSliceEqual([]int{1, 2, 3, 4, 56, 789}, []int{1, 2, 3, 4, 56, 789}))
}

func Test_getRootsOfQuadraticEquation(t *testing.T) {
	require.Equal(t, []complex128{1, 1}, wrapSolutionToSlice(1, -2, 1))
	require.Equal(t, []complex128{2, 2}, wrapSolutionToSlice(0, 2, -4))
	require.Equal(t, []complex128{0, 0}, wrapSolutionToSlice(0, 0, -4))
	require.Equal(t, []complex128{0, 0}, wrapSolutionToSlice(0, 121, 0))
	require.Equal(t,
		[]complex128{0.3997399635857069, -102.14973996358572},
		wrapSolutionToSlice(12, 1221, -490))
	require.Equal(t,
		[]complex128{-0.08333333333333333 + 6.389553105569191i, -0.08333333333333333 - 6.389553105569191i},
		wrapSolutionToSlice(12, 2, 490))
}

func Test_deleteByIndex(t *testing.T) {
	require.Nil(t, deleteByIndex(nil, 1901))
	require.Equal(t, []int{}, deleteByIndex([]int{}, 1901))
	require.Equal(t, []int{}, deleteByIndex([]int{1}, 0))
	require.Equal(t, []int{1}, deleteByIndex([]int{1}, -190))
	require.Equal(t, []int{1}, deleteByIndex([]int{1}, 190))
	require.Equal(t, []int{1, 101}, deleteByIndex([]int{1, 101, 1001}, 2))
}

func Test_isComplexEqual(t *testing.T) {
	require.True(t, isComplexEqual(complex(12, math.E), complex(12, 2.7182818)))
	require.True(t, isComplexEqual(complex(12, math.Ln10), complex(12, 2.302585)))
	require.True(t, isComplexEqual(complex(2, 1), complex(2, 1)))
	require.False(t, isComplexEqual(complex(1, 2), complex(2, 1)))
	require.False(t, isComplexEqual(complex(1, 2), complex(1.0001, 2.0001)))
	require.True(t, isComplexEqual(complex(1, 2), complex(1.000000001, 2.000000001)))
}

func Test_addPointers(t *testing.T) {
	a, b := 10, 20
	require.Equal(t, 30, *addPointers(&a, &b))
	a, b = 0, 20
	require.Equal(t, 20, *addPointers(&a, &b))
	require.Nil(t, addPointers(nil, &b))
	require.Nil(t, addPointers(nil, nil))
}

func Test_getCharByIndex(t *testing.T) {
	require.Equal(t, "1", string(getCharByIndex("1", 0)))
	require.Equal(t, "r", string(getCharByIndex("doresh", 2)))
	require.Equal(t, "😑", string(getCharByIndex("a🙂摆得🌚😑不整重新🙃🌚😑😐z", 5)))
}

func Test_getStringBySliceOfIndexes(t *testing.T) {
	require.Equal(t, "11111", string(getStringBySliceOfIndexes("1", []int{0, 0, 0, 0, 0})))
	require.Equal(t, "oreh", getStringBySliceOfIndexes("doresh", []int{1, 2, 3, 5}))
	require.Equal(t, "a摆🙂🙂🌚摆不",
		getStringBySliceOfIndexes("a🙂摆得🌚😑不整重新🙃🌚😑😐z", []int{0, 2, 1, 1, 4, 2, 6}))
}
