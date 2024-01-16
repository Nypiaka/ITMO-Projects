package learning

type Repository struct{}

func NewRepository() *Repository {
	return &Repository{}
}

func (r *Repository) GetStudentInfo(id int64) (*studentInfo, bool) {
	panic("don't call it")
}

func (r *Repository) GetAllSubjects() ([]string, bool) {
	panic("don't call it")
}

func (r *Repository) GetAllSubjectsInfo() ([]subjectInfo, bool) {
	panic("don't call it")
}
