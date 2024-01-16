package learning

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

var vanya = studentInfo{
	Name:    "Vanya",
	Age:     10,
	Subject: "math",
}
var petya = studentInfo{
	Name:    "Petya",
	Age:     10,
	Subject: "math",
}
var kolya = studentInfo{
	Name:    "Kolya",
	Age:     10,
	Subject: "phys",
}

func newTutorsIdTest(service Service, awaitTutorsId []int64, awaitTutorsIdSuccess bool, studentID int64) TutorsIdTest {
	return TutorsIdTest{
		service:              &service,
		awaitTutorsId:        awaitTutorsId,
		awaitTutorsIdSuccess: awaitTutorsIdSuccess,
		studentID:            studentID,
	}
}

func newTopSubjectsTest(
	service Service,
	awaitTopSubjects []string,
	awaitTopSubjectsSuccess bool,
	topN int,
) TopSubjectsTest {
	return TopSubjectsTest{
		service:                 &service,
		awaitTopSubjects:        awaitTopSubjects,
		awaitTopSubjectsSuccess: awaitTopSubjectsSuccess,
		topN:                    topN,
	}
}

type TopSubjectsTest struct {
	service                 *Service
	awaitTopSubjects        []string
	awaitTopSubjectsSuccess bool
	topN                    int
}

type TutorsIdTest struct {
	service              *Service
	awaitTutorsId        []int64
	awaitTutorsIdSuccess bool
	studentID            int64
}

func (mockedService *TutorsIdTest) test(t *testing.T) {
	tutors, success := mockedService.service.GetTutorsIDPreferIndividual(mockedService.studentID)
	t.Run("tutors id test", func(t *testing.T) {
		assert.Equal(t, mockedService.awaitTutorsIdSuccess, success)
		assert.Equal(t, mockedService.awaitTutorsId, tutors)
	})
}

func (mockedService *TopSubjectsTest) test(t *testing.T) {

	subjects, success := mockedService.service.GetTopSubjects(mockedService.topN)
	t.Run("top subjects test", func(t *testing.T) {
		assert.Equal(t, mockedService.awaitTopSubjectsSuccess, success)
		assert.Equal(t, mockedService.awaitTopSubjects, subjects)
	})

}

func initMocks(t *testing.T) (*TutorServiceMock, *TutorServiceMock, *RepositoryMock) {
	return NewTutorServiceMock(t), NewTutorServiceMock(t), NewRepositoryMock(t)
}

func TestTutorsIdEmpty(t *testing.T) {
	individual, group, repository := initMocks(t)
	service := NewService(individual, group, repository)
	repository.GetStudentInfoMock.Expect(101).Return(nil, false)
	test := newTutorsIdTest(*service, nil, false, 101)
	test.test(t)
}

func TestSubjectsEmpty(t *testing.T) {
	individual, group, repository := initMocks(t)
	service := NewService(individual, group, repository)
	repository.GetAllSubjectsInfoMock.Expect().Return(nil, false)
	test := newTopSubjectsTest(*service, nil, false, 101)
	test.test(t)
}

func TestTutorsSimple(t *testing.T) {
	individual, group, repository := initMocks(t)
	service := NewService(individual, group, repository)
	repository.GetStudentInfoMock.When(101).Then(&vanya, true)
	repository.GetStudentInfoMock.When(102).Then(&petya, true)
	repository.GetStudentInfoMock.When(103).Then(&kolya, true)
	individual.TutorsIDMock.When("math").Then([]int64{201})
	individual.TutorsIDMock.When("phys").Then([]int64{301, 302, 303})
	test := newTutorsIdTest(*service, []int64{301, 302, 303}, true, 103)
	test.test(t)
}

func TestSubjectsSimple(t *testing.T) {
	individual, group, repository := initMocks(t)
	service := NewService(individual, group, repository)
	repository.GetAllSubjectsInfoMock.Return([]subjectInfo{
		{name: "math", numberOfTutors: 1},
		{name: "phys", numberOfTutors: 3},
		{name: "chem", numberOfTutors: 10},
		{name: "bio", numberOfTutors: 2}},
		true)
	subjectsTest := newTopSubjectsTest(*service, []string{"math", "bio"}, true, 2)
	subjectsTest.test(t)
	test := newTopSubjectsTest(*service, nil, false, 120)
	test.test(t)
}

func TestSubjectsMoreThanPresent(t *testing.T) {
	individual, group, repository := initMocks(t)
	service := NewService(individual, group, repository)
	repository.GetAllSubjectsInfoMock.Return([]subjectInfo{
		{name: "math", numberOfTutors: 1},
		{name: "phys", numberOfTutors: 3},
		{name: "chem", numberOfTutors: 10},
		{name: "bio", numberOfTutors: 2}},
		true)
	subjectsTest := newTopSubjectsTest(*service, []string{"math", "bio"}, true, 2)
	subjectsTest.test(t)
	test := newTopSubjectsTest(*service, nil, false, 120)
	test.test(t)
}

func TestTutorsWithoutIndividualPractices(t *testing.T) {
	individual, group, repository := initMocks(t)
	service := NewService(individual, group, repository)
	repository.GetStudentInfoMock.When(101).Then(&vanya, true)
	repository.GetStudentInfoMock.When(102).Then(&petya, true)
	repository.GetStudentInfoMock.When(103).Then(&kolya, true)
	individual.TutorsIDMock.When("phys").Then(nil)
	individual.TutorsIDMock.When("math").Then(nil)
	group.TutorsIDMock.When("math").Then([]int64{201})
	group.TutorsIDMock.When("phys").Then([]int64{301, 302, 303})
	test := newTutorsIdTest(*service, []int64{301, 302, 303}, true, 103)
	test.test(t)
	test = newTutorsIdTest(*service, []int64{201}, true, 102)
	test.test(t)
}
