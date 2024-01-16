package rangeI

import "fmt"

type RangeInt interface {
	Left() int
	Right() int
	Length() int
	Intersect(other RangeInt)
	Union(other RangeInt) bool
	IsEmpty() bool
	ContainsInt(i int) bool
	ContainsRange(other RangeInt) bool
	IsIntersect(other RangeInt) bool
	ToSlice() []int
	Minimum() (int, bool)
	Maximum() (int, bool)
	String() string
}

func (r *RangeIntImpl) Left() int {
	return r.left
}

func (r *RangeIntImpl) Right() int {
	return r.right
}

func NewRangeInt(a, b int) *RangeIntImpl {
	return &RangeIntImpl{
		left:  a,
		right: b,
	}
}

type RangeIntImpl struct {
	left, right int
}

func (r *RangeIntImpl) Length() int {
	if r.left <= r.right {
		return r.right - r.left + 1
	}
	return 0
}

func (r *RangeIntImpl) Intersect(other RangeInt) {
	if !r.IsEmpty() {
		newLeft := max(r.left, other.Left())
		newRight := min(r.right, other.Right())
		r.left = newLeft
		r.right = newRight
	}
}

func (r *RangeIntImpl) Union(other RangeInt) bool {
	if other.IsEmpty() {
		return true
	}
	if r.IsEmpty() {
		r.right = other.Right()
		r.left = other.Left()
		return true
	}
	prevSumLen := r.Length() + other.Length()
	newLeft := min(r.left, other.Left())
	newRight := max(r.right, other.Right())
	r.left = newLeft
	r.right = newRight
	return prevSumLen >= r.Length()
}

func (r *RangeIntImpl) IsEmpty() bool {
	return r.Length() == 0
}

func (r *RangeIntImpl) ContainsInt(i int) bool {
	if !r.IsEmpty() {
		return r.left <= i && r.right >= i
	}
	return false
}

func (r *RangeIntImpl) ContainsRange(other RangeInt) bool {
	return r.left <= other.Left() && r.right >= other.Right()
}

func (r *RangeIntImpl) IsIntersect(other RangeInt) bool {
	if r.IsEmpty() || other.IsEmpty() {
		return false
	}
	return max(r.left, other.Left()) <= min(r.right, other.Right())
}

func (r *RangeIntImpl) ToSlice() []int {
	if r.IsEmpty() {
		return []int{}
	}
	slice := make([]int, r.right-r.left+1)
	for i := range slice {
		slice[i] = r.left + i
	}
	return slice
}

func (r *RangeIntImpl) Minimum() (int, bool) {
	if !r.IsEmpty() {
		return r.left, true
	}
	return -1, false
}

func (r *RangeIntImpl) Maximum() (int, bool) {
	if !r.IsEmpty() {
		return r.right, true
	}
	return -1, false
}

func (r *RangeIntImpl) String() string {
	if r.IsEmpty() {
		return ""
	}
	return "[" + fmt.Sprint(r.left) + "," + fmt.Sprint(r.right) + "]"
}
