package HW1

import "testing"

func Test_sum(t *testing.T) {
	a, b := 150, 150
	want := 300
	if want != sum(a, b) {
		t.Errorf(
			"expected %d + %d = %d, but found %d",
			a,
			b,
			want,
			sum(a, b))
	}
}
