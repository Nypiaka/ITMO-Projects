# HW4 &ndash; Тестирование

## Дан фрагмент кода

```golang
package learning

import (
  "sort"

  "hw4/internal/group"
  "hw4/internal/individual"
)

type Service struct {
  individualTutorService *individual.TutorService
  groupTutorService      *group.TutorService
  repository             *Repository
}

func NewService(
  individualTutorService *individual.TutorService,
  groupTutorService *group.TutorService,
  repository *Repository,
) *Service {
  return &Service{
    individualTutorService: individualTutorService,
    groupTutorService:      groupTutorService,
    repository:             repository,
  }
}

func (s *Service) GetTutorsIDPreferIndividual(studentID int64) ([]int64, bool) {
  studentInfo, ok := s.repository.GetStudentInfo(studentID)
  if !ok {
    return nil, ok
  }

  tutorsID := s.individualTutorService.TutorsID(studentInfo.Subject)
  if len(tutorsID) == 0 {
    tutorsID = s.groupTutorService.TutorsID(studentInfo.Subject)
    if len(tutorsID) == 0 {
      return nil, false
    }
    return tutorsID, true
  }

  return tutorsID, true
}

func (s *Service) GetTopSubjects(topN int) ([]string, bool) {
  subjects, ok := s.repository.GetAllSubjectsInfo()
  if !ok {
    return nil, ok
  }
  if len(subjects) > topN {
    return nil, false
  }

  sort.SliceStable(
    subjects,
    func(i, j int) bool {
      return subjects[i].numberOfTutors < subjects[j].numberOfTutors
    },
  )

  return fromSubject(subjects[:topN]), true
}
```
## Задание

Реализовать mock-тесты для методов `GetTutorsIDPreferIndividual` и `GetTopSubjects`.

Краткое описание функционала.

* Метод `GetTutorsIDPreferIndividual` возвращает слайс ID преподавателей, в первую очередь для индивидульных занятий.
Если не нашлось, то возвращает ID преподавателей групповых занятий. В случае успеха вторым  параметром идет `true`,
иначе `false`;
* Метод `GetTopSubjects` возкращает топ N предметов, по количетву преподавателей. В случае успеха вторым  параметром идет `true`,
иначе `false`.
* Два сервиса `group` и `individual`, которые отвечают за индивидуальные и групповые занятия. Каждый сервис
может вернуть список преподавателей по названию предмета, а также список всех доступных предметов.
* Репозиторий является фейковым доступом в базу данных.

Запрещается менять код во всех пакетах кроме `learning`, в пакете `learning` нельзя менять методы сервиса
и файлы `repository.go`, `model.go`, `convertor.go`.

## Как сдавать:

* Проверить, что появился **ваш личный** репозиторий с `HW4`, сделать его клон,
личный репозиторий имеет вид `https://tinkoff-edu.gitlab.yandexcloud.net/itmo-course-autumn-2023/Students/<username>/Homeworks/HW4`
* Добавить ваше решение в ветку `hw`
* Добавить файл конфигурации `.gitlab-ci.yml` для запуска пайплайна с тестами
* Открыть _Merge request_ из ветки `hw` в ветку `master` **вашего репозитория** (не основного)
* Дождаться, когда пайплайн станет зелёным
* Если будут вопросы по времени сдачи дз &ndash; мы будем ориентироваться на время последнего вашего действия в _Merge request_.
