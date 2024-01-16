package group

type TutorService struct{}

func (s *TutorService) TutorsID(subject string) []int64 {
	panic("don't call it")
}

func (s *TutorService) Subjects() []string {
	panic("don't call it")
}
