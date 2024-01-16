package learning

type TutorService interface {
	Subjects() []string
	TutorsID(subject string) []int64
}
