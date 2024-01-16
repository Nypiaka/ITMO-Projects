# HW3 &ndash; Коллекции

## Часть 1 &ndash; `RangeInt`

### Дан фрагмент кода

```golang
package rangeI

type RangeInt interface {
  Length() int
  Intersect(other RangeInt)
  Union(other RangeInt) bool
  IsEmpty() bool // Считаем пустым любой [a, b], где a > b
  ContainsInt(i int) bool
  ContainsRange(other RangeInt) bool
  IsIntersect(other RangeInt) bool
  ToSlice() []int // Отсортированные по возрастанию числа из интервала
  Minimum() (int, bool)
  Maximum() (int, bool)
  String() string // "" если пусто, иначе в формате "[4,7]"
}

// Также реализуйте конструктор
func NewRangeInt(a, b int) /*your type*/ {}
```

Запрещается в приведённом фрагменте кода:

* Менять сигнатуры;
* Менять модификаторы доступа;
* Менять пакеты;

### Задание

Реализуйте `RangeInt`, представляющий замкнутый целочисленный интервал.

Напишите тесты, покрывающие все методы.

## Часть 2 &ndash; `Bank`

### Дан следующий фрагмент кода

```golang
package bank

import (...)

const (
  TopUpOp OperationType = iota
  WithdrawOp
)

type OperationType int64

type Clock interface {
  Now() time.Time
}

type Operation struct {
  OpTime   time.Time
  OpType   OperationType
  OpAmount int
  Balance  int // Баланс на конец операции
}

func (o Operation) String() string {
  var format string
  if o.OpType == TopUpOp {
    format = `%s +%d %d`
  } else {
    format = `%s -%d %d`
  }
  return fmt.Sprintf(format, o.OpTime.String()[:19], o.OpAmount, o.Balance)
}

type Account interface {
  TopUp(amount int) bool //  Если amount <= 0, то false
  Withdraw(amount int) bool // Если amount <= 0 или невозможно столько снять, то false
  Operations() []Operation // Храним все "успешные" TopUp и Withdraw
  Statement() string // Для перевода в строку используем метод String()
  Balance() int
}

// Конструктор
func NewAccount(clock Clock) /*your type*/ {}
```

Запрещается в приведённом фрагменте кода:

* Менять сигнатуры;
* Менять модификаторы доступа;
* Менять пакеты;

### Задание

Реализуйте `Account`, представляющий модель банковского расчетного счета. Счет ведется в условных
единицах и не допускает достижения отрицательного баланса. Начальный баланс 0.

Пример формата выписки:

```text
2023-03-13 11:19:07 +100 100
2023-03-13 11:22:07 -50 50
```

Напишите тесты, покрывающие все методы.

---

### StyleCode

* Используйте стандартные пакеты

### Как сдавать:

* Проверить, что появился **ваш личный** репозиторий с `HW3`, сделать его клон,
личный репозиторий имеет вид `https://tinkoff-edu.gitlab.yandexcloud.net/itmo-course-autumn-2023/Students/<username>/Homeworks/HW3`
* Добавить ваше решение в ветку `hw`
* Добавить файл конфигурации `.gitlab-ci.yml` для запуска пайплайна с тестами
* Открыть _Merge request_ из ветки `hw` в ветку `master` **вашего репозитория** (не основного)
* Дождаться, когда пайплайн станет зелёным
* Если будут вопросы по времени сдачи дз &ndash; мы будем ориентироваться на время последнего вашего действия в _Merge request_
* В какой-то момент выйдут тесты курса, их нужно будет подтянуть в свой _Merge request_.