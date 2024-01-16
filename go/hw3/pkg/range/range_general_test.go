package rangeI

import (
    "github.com/stretchr/testify/assert"
    "testing"

    "github.com/stretchr/testify/require"
)

func TestLength(t *testing.T) {
    rangeInt := NewRangeInt(2, 5)
    want := 4
    // 2 3 4 5
    got := rangeInt.Length()
    if want != got {
        t.Errorf("wrong length: want %d, got %d", want, got)
    }
}

func Equals(a, b RangeInt) bool {
    if a.IsEmpty() && b.IsEmpty() {
        return true
    }

    if a.IsEmpty() || b.IsEmpty() {
        return false
    }
    minA, _ := a.Minimum()
    maxA, _ := a.Maximum()
    minB, _ := b.Minimum()
    maxB, _ := b.Maximum()

    return minA == minB && maxA == maxB
}

func NewEmptyRangeInt() RangeInt {
    return NewRangeInt(1, 0)
}

// ======================= Length ==========

type TestCase_length struct {
    gotRangeInt RangeInt
    wantLength  int
}

func (tc TestCase_length) Run(t *testing.T) {
    require.Equal(t, tc.wantLength, tc.gotRangeInt.Length())
}

func TestLengthInvalid(t *testing.T) {
    TestCase_length{
        gotRangeInt: NewRangeInt(0, -100),
        wantLength:  0,
    }.Run(t)
}

func Test_length_Empty(t *testing.T) {
    TestCase_length{
        gotRangeInt: NewEmptyRangeInt(),
        wantLength:  0,
    }.Run(t)
}

func Test_length_Point(t *testing.T) {
    TestCase_length{
        gotRangeInt: NewRangeInt(15, 15),
        wantLength:  1,
    }.Run(t)
}

func Test_length_Range(t *testing.T) {
    TestCase_length{
        gotRangeInt: NewRangeInt(10, 15),
        wantLength:  6,
    }.Run(t)
}

// ======================= Intersect =======

type TestCase_intersect struct {
    aRangeInt    RangeInt
    bRangeInt    RangeInt
    wantRangeInt RangeInt
}

func (tc TestCase_intersect) Run(t *testing.T) {
    tc.aRangeInt.Intersect(tc.bRangeInt)
    gotEquals := Equals(tc.wantRangeInt, tc.aRangeInt)
    assert.Equal(t, true, gotEquals)
}

func TestIntersectEmpty(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewRangeInt(-100, -20),
        bRangeInt:    NewRangeInt(-19, 100),
        wantRangeInt: NewEmptyRangeInt(),
    }.Run(t)
}

func TestIntersectToPoint(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewRangeInt(-100, -20),
        bRangeInt:    NewRangeInt(-20, 100),
        wantRangeInt: NewRangeInt(-20, -20),
    }.Run(t)
}

func Test_intersect_RangeAndRange(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewRangeInt(3, 7),
        bRangeInt:    NewRangeInt(1, 2),
        wantRangeInt: NewEmptyRangeInt(),
    }.Run(t)
}

func Test_intersect_RangeAndRange_1(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewRangeInt(3, 7),
        bRangeInt:    NewRangeInt(1, 5),
        wantRangeInt: NewRangeInt(3, 5),
    }.Run(t)
}

func Test_intersect_RangeAndRange_2(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewRangeInt(3, 7),
        bRangeInt:    NewRangeInt(4, 5),
        wantRangeInt: NewRangeInt(4, 5),
    }.Run(t)
}

func Test_intersect_RangeAndRange_3(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewRangeInt(3, 7),
        bRangeInt:    NewRangeInt(5, 10),
        wantRangeInt: NewRangeInt(5, 7),
    }.Run(t)
}

func Test_intersect_RangeAndRange_4(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewRangeInt(1, 5),
        bRangeInt:    NewRangeInt(5, 10),
        wantRangeInt: NewRangeInt(5, 5),
    }.Run(t)
}

func Test_intersect_EmptyAndEmpty(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewEmptyRangeInt(),
        bRangeInt:    NewEmptyRangeInt(),
        wantRangeInt: NewEmptyRangeInt(),
    }.Run(t)
}

func Test_intersect_EmptyAndRange(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewEmptyRangeInt(),
        bRangeInt:    NewRangeInt(3, 7),
        wantRangeInt: NewEmptyRangeInt(),
    }.Run(t)
}

func Test_intersect_RangeAndEmpty(t *testing.T) {
    TestCase_intersect{
        aRangeInt:    NewRangeInt(3, 7),
        bRangeInt:    NewEmptyRangeInt(),
        wantRangeInt: NewEmptyRangeInt(),
    }.Run(t)
}

// ======================= Union ===========

type TestCase_union struct {
    aRangeInt    RangeInt
    bRangeInt    RangeInt
    wantRangeInt RangeInt
    wantUnion    bool
}

func (tc TestCase_union) Run(t *testing.T) {
    gotUnion := tc.aRangeInt.Union(tc.bRangeInt)
    assert.Equal(t, tc.wantUnion, gotUnion)
    if tc.wantUnion {
        gotEquals := Equals(tc.wantRangeInt, tc.aRangeInt)
        assert.Equal(t, gotEquals, true)
    }
}

func Test_union_EmptyAndEmpty(t *testing.T) {
    TestCase_union{
        aRangeInt:    NewEmptyRangeInt(),
        bRangeInt:    NewEmptyRangeInt(),
        wantRangeInt: NewEmptyRangeInt(),
        wantUnion:    true,
    }.Run(t)
}

func Test_union_EmptyAndRange(t *testing.T) {
    TestCase_union{
        aRangeInt:    NewEmptyRangeInt(),
        bRangeInt:    NewRangeInt(3, 7),
        wantRangeInt: NewRangeInt(3, 7),
        wantUnion:    true,
    }.Run(t)
}

func Test_union_RangeAndRange(t *testing.T) {
    TestCase_union{
        aRangeInt:    NewRangeInt(1, 2),
        bRangeInt:    NewRangeInt(3, 7),
        wantRangeInt: NewRangeInt(1, 7),
        wantUnion:    true,
    }.Run(t)
}

func Test_union_RangeAndRange_1(t *testing.T) {
    TestCase_union{
        aRangeInt:    NewRangeInt(1, 5),
        bRangeInt:    NewRangeInt(3, 7),
        wantRangeInt: NewRangeInt(1, 7),
        wantUnion:    true,
    }.Run(t)
}

func Test_union_RangeAndRange_2(t *testing.T) {
    TestCase_union{
        aRangeInt:    NewRangeInt(5, 6),
        bRangeInt:    NewRangeInt(3, 7),
        wantRangeInt: NewRangeInt(3, 7),
        wantUnion:    true,
    }.Run(t)
}

func Test_union_RangeAndRange_3(t *testing.T) {
    TestCase_union{
        aRangeInt:    NewRangeInt(5, 10),
        bRangeInt:    NewRangeInt(3, 7),
        wantRangeInt: NewRangeInt(3, 10),
        wantUnion:    true,
    }.Run(t)
}

func TestUnionEdgePoint(t *testing.T) {
    TestCase_union{
        aRangeInt:    NewRangeInt(-100, -20),
        bRangeInt:    NewRangeInt(-19, 100),
        wantRangeInt: NewRangeInt(-100, 100),
        wantUnion:    true,
    }.Run(t)
}

func TestUnionUnionFalse(t *testing.T) {
    TestCase_union{
        aRangeInt:    NewRangeInt(-100, -20),
        bRangeInt:    NewRangeInt(-18, 100),
        wantRangeInt: NewRangeInt(-100, 100),
        wantUnion:    false,
    }.Run(t)
}

// ======================= Empty ===========

type TestCase_empty struct {
    gotRangeInt RangeInt
    wantEmpty   bool
}

func (tc TestCase_empty) Run(t *testing.T) {
    assert.Equal(t, tc.wantEmpty, tc.gotRangeInt.IsEmpty())
}

func Test_empty_NewEmpty(t *testing.T) {
    TestCase_empty{
        gotRangeInt: NewEmptyRangeInt(),
        wantEmpty:   true,
    }.Run(t)
}

func Test_empty_New(t *testing.T) {
    TestCase_empty{
        gotRangeInt: NewRangeInt(10, 5),
        wantEmpty:   true,
    }.Run(t)
}

func Test_empty_NotEmpty(t *testing.T) {
    TestCase_empty{
        gotRangeInt: NewRangeInt(1, 15),
        wantEmpty:   false,
    }.Run(t)
}

func Test_empty_NotEmptyPoint(t *testing.T) {
    TestCase_empty{
        gotRangeInt: NewRangeInt(1, 1),
        wantEmpty:   false,
    }.Run(t)
}

// ======================= ContainsInt =====

type TestCase_containsInt struct {
    rangeInt     RangeInt
    integer      int
    wantContains bool
}

func (tc TestCase_containsInt) Run(t *testing.T) {
    gotContains := tc.rangeInt.ContainsInt(tc.integer)
    assert.Equal(t, tc.wantContains, gotContains)
}

func Test_containsInt_Empty(t *testing.T) {
    TestCase_containsInt{
        rangeInt:     NewEmptyRangeInt(),
        integer:      0,
        wantContains: false,
    }.Run(t)
}

func Test_containsInt_InRange(t *testing.T) {
    TestCase_containsInt{
        rangeInt:     NewRangeInt(-5, 10),
        integer:      0,
        wantContains: true,
    }.Run(t)
}

func Test_containsInt_InRangeBorderRight(t *testing.T) {
    TestCase_containsInt{
        rangeInt:     NewRangeInt(-5, 10),
        integer:      10,
        wantContains: true,
    }.Run(t)
}

func Test_containsInt_InRangeBorderLeft(t *testing.T) {
    TestCase_containsInt{
        rangeInt:     NewRangeInt(-5, 10),
        integer:      -5,
        wantContains: true,
    }.Run(t)
}

func Test_containsInt_NotInRange(t *testing.T) {
    TestCase_containsInt{
        rangeInt:     NewRangeInt(5, 10),
        integer:      0,
        wantContains: false,
    }.Run(t)
}

// ======================= ContainsRange ===

type TestCase_containsRange struct {
    aRangeInt    RangeInt
    bRangeInt    RangeInt
    wantContains bool
}

func (tc TestCase_containsRange) Run(t *testing.T) {
    gotContains := tc.aRangeInt.ContainsRange(tc.bRangeInt)
    assert.Equal(t, tc.wantContains, gotContains)
}

func Test_containsRange_EmptyAndEmpty(t *testing.T) {
    TestCase_containsRange{
        aRangeInt:    NewEmptyRangeInt(),
        bRangeInt:    NewEmptyRangeInt(),
        wantContains: true,
    }.Run(t)
}

func Test_containsRange_EmptyAndRange(t *testing.T) {
    TestCase_containsRange{
        aRangeInt:    NewEmptyRangeInt(),
        bRangeInt:    NewRangeInt(1, 1),
        wantContains: false,
    }.Run(t)
}

func Test_containsRange_EmptyAndEmpty_1(t *testing.T) {
    TestCase_containsRange{
        aRangeInt:    NewEmptyRangeInt(),
        bRangeInt:    NewRangeInt(1, 10),
        wantContains: false,
    }.Run(t)
}

func Test_containsRange_RangeAndEmpty(t *testing.T) {
    TestCase_containsRange{
        aRangeInt:    NewRangeInt(1, 1),
        bRangeInt:    NewEmptyRangeInt(),
        wantContains: true,
    }.Run(t)
}

func Test_containsRange_RangeAndEmpty_1(t *testing.T) {
    TestCase_containsRange{
        aRangeInt:    NewRangeInt(1, 10),
        bRangeInt:    NewEmptyRangeInt(),
        wantContains: true,
    }.Run(t)
}

func Test_containsRange_RangeAndRange(t *testing.T) {
    TestCase_containsRange{
        aRangeInt:    NewRangeInt(1, 1),
        bRangeInt:    NewRangeInt(1, 1),
        wantContains: true,
    }.Run(t)
}

func Test_containsRange_RangeAndRange_1(t *testing.T) {
    TestCase_containsRange{
        aRangeInt:    NewRangeInt(1, 5),
        bRangeInt:    NewRangeInt(3, 4),
        wantContains: true,
    }.Run(t)
}

func Test_containsRange_RangeAndRange_2(t *testing.T) {
    TestCase_containsRange{
        aRangeInt:    NewRangeInt(1, 10),
        bRangeInt:    NewRangeInt(-1, 1),
        wantContains: false,
    }.Run(t)
}

func TestContainsEdgeRange(t *testing.T) {
    t.Run("contains №1", func(t *testing.T) {
        TestCase_containsRange{
            aRangeInt:    NewRangeInt(-100, -11),
            bRangeInt:    NewRangeInt(-100, -10),
            wantContains: false,
        }.Run(t)
    })

    t.Run("contains №2", func(t *testing.T) {
        TestCase_containsRange{
            aRangeInt:    NewRangeInt(-100, -10),
            bRangeInt:    NewRangeInt(-101, -10),
            wantContains: false,
        }.Run(t)
    })
}

// ======================= IsIntersect =====

type TestCase_isIntersect struct {
    aRangeInt       RangeInt
    bRangeInt       RangeInt
    wantIsIntersect bool
}

func (tc TestCase_isIntersect) Run(t *testing.T) {
    gotIsIntersect := tc.aRangeInt.IsIntersect(tc.bRangeInt)
    assert.Equal(t, tc.wantIsIntersect, gotIsIntersect)
}

func Test_isIntersect_EmptyAndEmpty(t *testing.T) {
    TestCase_isIntersect{
        aRangeInt:       NewEmptyRangeInt(),
        bRangeInt:       NewEmptyRangeInt(),
        wantIsIntersect: false,
    }.Run(t)
}

func Test_isIntersect_EmptyAndRange(t *testing.T) {
    TestCase_isIntersect{
        aRangeInt:       NewRangeInt(1, 3),
        bRangeInt:       NewEmptyRangeInt(),
        wantIsIntersect: false,
    }.Run(t)
}

func Test_isIntersect_RangeAndEmpty(t *testing.T) {
    TestCase_isIntersect{
        aRangeInt:       NewEmptyRangeInt(),
        bRangeInt:       NewRangeInt(1, 3),
        wantIsIntersect: false,
    }.Run(t)
}

func Test_isIntersect_RangeAndRange(t *testing.T) {
    TestCase_isIntersect{
        aRangeInt:       NewRangeInt(3, 10),
        bRangeInt:       NewRangeInt(1, 3),
        wantIsIntersect: true,
    }.Run(t)
}

func Test_isIntersect_RangeAndRange_1(t *testing.T) {
    TestCase_isIntersect{
        aRangeInt:       NewRangeInt(4, 10),
        bRangeInt:       NewRangeInt(1, 3),
        wantIsIntersect: false,
    }.Run(t)
}

func TestIsIntersectEdge(t *testing.T) {
    TestCase_isIntersect{
        aRangeInt:       NewRangeInt(-100, -21),
        bRangeInt:       NewRangeInt(-20, 100),
        wantIsIntersect: false,
    }.Run(t)
}

// ======================= ToSlice =========

type TestCase_toSlice struct {
    rangeInt  RangeInt
    wantSlice []int
}

func (tc TestCase_toSlice) Run(t *testing.T) {
    gotSlice := tc.rangeInt.ToSlice()
    assert.Equal(t, tc.wantSlice, gotSlice)
}

func Test_toSlice_Empty(t *testing.T) {
    TestCase_toSlice{
        rangeInt:  NewEmptyRangeInt(),
        wantSlice: []int{},
    }.Run(t)
}

func Test_toSlice_Point(t *testing.T) {
    TestCase_toSlice{
        rangeInt:  NewRangeInt(5, 5),
        wantSlice: []int{5},
    }.Run(t)
}

func Test_toSlice_Range(t *testing.T) {
    TestCase_toSlice{
        rangeInt:  NewRangeInt(5, 8),
        wantSlice: []int{5, 6, 7, 8},
    }.Run(t)
}

// ======================= Minimum =========

type TestCase_minimum struct {
    rangeInt  RangeInt
    isWantMin bool
    wantMin   int
}

func (tc TestCase_minimum) Run(t *testing.T) {
    gotMin, isGotMin := tc.rangeInt.Minimum()
    assert.Equal(t, tc.isWantMin, isGotMin)

    if isGotMin {
        assert.Equal(t, tc.wantMin, gotMin)
    }
}

func Test_minimum_Empty(t *testing.T) {
    TestCase_minimum{
        rangeInt:  NewEmptyRangeInt(),
        isWantMin: false,
        wantMin:   0,
    }.Run(t)
}

func Test_minimum_Point(t *testing.T) {
    TestCase_minimum{
        rangeInt:  NewRangeInt(0, 0),
        isWantMin: true,
        wantMin:   0,
    }.Run(t)
}

func Test_minimum_Range(t *testing.T) {
    TestCase_minimum{
        rangeInt:  NewRangeInt(5, 15),
        isWantMin: true,
        wantMin:   5,
    }.Run(t)
}

// ======================= Maximum =========

type TestCase_maximum struct {
    rangeInt  RangeInt
    isWantMax bool
    wantMax   int
}

func (tc TestCase_maximum) Run(t *testing.T) {
    gotMax, isGotMax := tc.rangeInt.Maximum()
    assert.Equal(t, tc.isWantMax, isGotMax)

    if isGotMax {
        assert.Equal(t, tc.wantMax, gotMax)
    }
}

func Test_maximum_Empty(t *testing.T) {
    TestCase_maximum{
        rangeInt:  NewEmptyRangeInt(),
        isWantMax: false,
        wantMax:   0,
    }.Run(t)
}

func Test_maximum_Point(t *testing.T) {
    TestCase_maximum{
        rangeInt:  NewRangeInt(0, 0),
        isWantMax: true,
        wantMax:   0,
    }.Run(t)
}

func Test_maximum_Range(t *testing.T) {
    TestCase_maximum{
        rangeInt:  NewRangeInt(2, 5),
        isWantMax: true,
        wantMax:   5,
    }.Run(t)
}

// ======================= ToString ========

type TestCase_toString struct {
    rangeInt   RangeInt
    wantString string
}

func (tc TestCase_toString) Run(t *testing.T) {
    gotString := tc.rangeInt.String()
    assert.Equal(t, tc.wantString, gotString)
}

func Test_toString_Empty(t *testing.T) {
    TestCase_toString{
        rangeInt:   NewEmptyRangeInt(),
        wantString: "",
    }.Run(t)
}

func Test_toString_Point(t *testing.T) {
    TestCase_toString{
        rangeInt:   NewRangeInt(0, 0),
        wantString: "[0,0]",
    }.Run(t)
}

func Test_toString_Range(t *testing.T) {
    TestCase_toString{
        rangeInt:   NewRangeInt(1, 3),
        wantString: "[1,3]",
    }.Run(t)
}
