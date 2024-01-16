package rangeI

import (
	"github.com/stretchr/testify/require"
	"testing"
)

func TestLength_my(t *testing.T) {
	require.Equal(t, 4, NewRangeInt(1, 4).Length())
	require.Equal(t, 0, NewRangeInt(1, 0).Length())
	require.Equal(t, 0, NewRangeInt(10, 4).Length())
	require.Equal(t, 1, NewRangeInt(11, 11).Length())
}

func IntersectAndReturn(r *RangeIntImpl, left, right int) RangeInt {
	r.Intersect(NewRangeInt(left, right))
	return r
}

func TestIntersect(t *testing.T) {
	require.Equal(t, NewRangeInt(2, 4), IntersectAndReturn(NewRangeInt(1, 4), 2, 5))
	require.Equal(t, NewRangeInt(2, 1), IntersectAndReturn(NewRangeInt(1, 4), 2, 1))
	require.Equal(t, NewRangeInt(2, 5), IntersectAndReturn(NewRangeInt(1, 6), 2, 5))
	require.Equal(t, NewRangeInt(4, 4), IntersectAndReturn(NewRangeInt(1, 4), 4, 5))
}

func UnionTestTemplate(
	firstLeft, firstRight, secondLeft, secondRight int,
	awaitUnionResult bool,
	awaitLeft, awaitRight int,
	t *testing.T,
) {
	firstRange := NewRangeInt(firstLeft, firstRight)
	successUnion := firstRange.Union(NewRangeInt(secondLeft, secondRight))
	require.Equal(t, awaitUnionResult, successUnion)
	require.Equal(t, firstRange.Left(), awaitLeft)
	require.Equal(t, firstRange.Right(), awaitRight)
}

func TestUnion(t *testing.T) {
	UnionTestTemplate(0, 10, 8, 17, true, 0, 17, t)
	UnionTestTemplate(0, 10, 11, 17, true, 0, 17, t)
	UnionTestTemplate(0, 10, 13, 17, false, 0, 17, t)
	UnionTestTemplate(0, -1, 8, 17, true, 8, 17, t)
	UnionTestTemplate(0, 10, 8, 7, true, 0, 10, t)
	UnionTestTemplate(0, -1, 8, 7, true, 0, -1, t)
}

func TestIsEmpty(t *testing.T) {
	require.Equal(t, true, NewRangeInt(-1, -10).IsEmpty())
	require.Equal(t, false, NewRangeInt(-10, -1).IsEmpty())
	require.Equal(t, false, NewRangeInt(1, 1).IsEmpty())
}

func TestContainsInt(t *testing.T) {
	require.Equal(t, true, NewRangeInt(0, 10).ContainsInt(4))
	require.Equal(t, true, NewRangeInt(4, 10).ContainsInt(4))
	require.Equal(t, true, NewRangeInt(4, 4).ContainsInt(4))
	require.Equal(t, false, NewRangeInt(10, 4).ContainsInt(4))
	require.Equal(t, false, NewRangeInt(5, 10).ContainsInt(4))
}

func TestContainsRange(t *testing.T) {
	require.Equal(t, true, NewRangeInt(0, 10).ContainsRange(NewRangeInt(1, 9)))
	require.Equal(t, true, NewRangeInt(0, 10).ContainsRange(NewRangeInt(0, 10)))
	require.Equal(t, true, NewRangeInt(0, 10).ContainsRange(NewRangeInt(2, 1)))
	require.Equal(t, false, NewRangeInt(0, 10).ContainsRange(NewRangeInt(-1, 11)))
	require.Equal(t, false, NewRangeInt(0, 10).ContainsRange(NewRangeInt(0, 11)))
	require.Equal(t, true, NewRangeInt(4, 4).ContainsRange(NewRangeInt(4, 4)))
	require.Equal(t, true, NewRangeInt(4, 4).ContainsRange(NewRangeInt(4, 3)))
	require.Equal(t, false, NewRangeInt(4, 3).ContainsRange(NewRangeInt(0, 10)))
	require.Equal(t, true, NewRangeInt(0, -1).ContainsRange(NewRangeInt(0, -1)))
}

func TestIsIntersect(t *testing.T) {
	require.Equal(t, true, NewRangeInt(1, 9).IsIntersect(NewRangeInt(0, 10)))
	require.Equal(t, true, NewRangeInt(0, 10).IsIntersect(NewRangeInt(0, 10)))
	require.Equal(t, false, NewRangeInt(2, 1).IsIntersect(NewRangeInt(0, 10)))
	require.Equal(t, true, NewRangeInt(-1, 11).IsIntersect(NewRangeInt(0, 10)))
	require.Equal(t, true, NewRangeInt(0, 11).IsIntersect(NewRangeInt(0, 10)))
	require.Equal(t, true, NewRangeInt(4, 4).IsIntersect(NewRangeInt(4, 4)))
	require.Equal(t, false, NewRangeInt(4, 3).IsIntersect(NewRangeInt(4, 4)))
	require.Equal(t, false, NewRangeInt(0, 10).IsIntersect(NewRangeInt(4, 3)))
	require.Equal(t, false, NewRangeInt(0, -1).IsIntersect(NewRangeInt(0, -1)))
	require.Equal(t, true, NewRangeInt(1, 8).IsIntersect(NewRangeInt(2, 9)))
}

func TestToSlice(t *testing.T) {
	require.Equal(t, []int{}, NewRangeInt(1, 0).ToSlice())
	require.Equal(t, []int{1}, NewRangeInt(1, 1).ToSlice())
	require.Equal(t, []int{}, NewRangeInt(1, -10).ToSlice())
	require.Equal(t, []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, NewRangeInt(1, 10).ToSlice())
}

func MaxOrMinTestTemplate(t *testing.T, awaitBool bool, awaitInt int, isMax bool, r *RangeIntImpl) {
	var realBool bool
	var realInt int
	if isMax {
		realInt, realBool = r.Maximum()
	} else {
		realInt, realBool = r.Minimum()
	}
	require.Equal(t, awaitBool, realBool)
	require.Equal(t, awaitInt, realInt)
}

func TestMaximum(t *testing.T) {
	MaxOrMinTestTemplate(t, true, 10, true, NewRangeInt(0, 10))
	MaxOrMinTestTemplate(t, true, 10, true, NewRangeInt(10, 10))
	MaxOrMinTestTemplate(t, false, -1, true, NewRangeInt(10, 1))
}

func TestMinimum(t *testing.T) {
	MaxOrMinTestTemplate(t, true, 10, false, NewRangeInt(10, 100))
	MaxOrMinTestTemplate(t, true, 10, false, NewRangeInt(10, 10))
	MaxOrMinTestTemplate(t, false, -1, false, NewRangeInt(10, 1))
}

func TestString(t *testing.T) {
	require.Equal(t, "", NewRangeInt(10, 9).String())
	require.Equal(t, "[10,10]", NewRangeInt(10, 10).String())
	require.Equal(t, "[1,10]", NewRangeInt(1, 10).String())

}
