# HW2 &ndash; Строки, слайсы и указатели

### Даны сигнатуры функций

```go
func getCharByIndex(str string, idx int) rune { return rune(0) }

func getStringBySliceOfIndexes(str string, indexes []int) string { return "" }

func addPointers(ptr1, ptr2 *int) *int { return nil }

func isComplexEqual(a, b complex128) bool { return true }

func getRootsOfQuadraticEquation(a, b, c float64) (complex128, complex128) { return 0i, 0i }

func mergeSort(s []int) []int { return s }

func reverseSliceOne(s []int) {}

func reverseSliceTwo(s []int) []int { return s }

func swapPointers(a, b *int) {}

func isSliceEqual(a, b []int) bool { return true }

func deleteByIndex(s []int, idx int) []int { return []int{} }
```

### Задание

1. Добавьте имплементацию для следующих функций:
    1. `getCharByIndex` &ndash; вернуть i-й символ данной строки;
        * стоит исходить из того, что "символ" не всегда занимает ровно 1 байт
    2. `getStringBySliceOfIndexes` &ndash; вернуть строку, полученную из конкатенации заданных символов строки.
       Номера символов переданы слайсом;
        * стоит исходить из того, что "символ" не всегда занимает ровно 1 байт
    3. `addPointers` &ndash; сложить указатели;
    4. `isComplexEqual` &ndash; проверить, совпадают ли два комплексных числа. <br/>_Hint_: подумать над точностью;
    5. `getRootsOfQuadraticEquation` &ndash; вернуть два корня квадратного трехчлена (в любом порядке);
    6. `mergeSort` &ndash; отсортировать переданную строку алгоритмом сортировки слиянием;
    7. `reverseSliceOne` &ndash; развернуть порядок элементов в слайса;
    8. `reverseSliceTwo` &ndash; развернуть порядок элементов в слайсе. При этом модификация возвращённого слайса не
       должна менять исходный слайс;
    9. `swapPointers` &ndash; поменять местами значения указателей;
    10. `isSliceEqual` &ndash; проверить два слайса на равенство элементов;
    11. `deleteByIndex` &ndash; удалить i-й элемент из слайса;

---

2. Добавьте исчерпывающие тесты для перечисленных функций;

---

3. В корень добавьте файл `answer.txt` с описанием ответов на вопросы ниже.

    * Что выведет код ниже и почему? (постарайтесь ответить на вопрос, до того как запустите код)
       ```go
       func update(p *int) {
         b := 2
         p = &b
       }
       
       func main() {
         var (
           a = 1
           p = &a
         )
         fmt.Println(*p)
         update(p)
         fmt.Println(*p)
       }
       ```
    * Можно ли реализовать функцию из задания 1.11, которая бы обходилась только изменением исходного слайса
      и при этом ничего не возвращала?
    * Можно ли реализовать функцию из задания 1.9 так, чтобы следующий код
       ```go
       i, j := 1, 2
       a, b := &i, &j
       swapPointers(a, b)
       fmt.Println(i, j, *a, *b)
       ```
      выводил `1 2 2 1`, то есть чтобы функция меняла местами указатели, но не значения?
      А если разрешается изменить сигнатуру функции и ее аргументы при вызове?

### StyleCode:

* Использование сторонних библиотек запрещается;

### Как сдавать:

* Проверить, что появился **ваш личный** репозиторий с `HW2`, сделать его клон, 
личный репозиторий имеет вид `https://tinkoff-edu.gitlab.yandexcloud.net/itmo-course-autumn-2023/Students/<username>/Homeworks/HW2`
* Все функции реализовать в файле [`main.go`](main.go)
* Добавить ваше решение в ветку `hw`
* Добавить файл конфигурации `.gitlab-ci.yml` для запуска пайплайна с тестами
* Открыть _Merge request_ из ветки `hw` в ветку `master` **вашего репозитория** (не основного)
* Дождаться, когда пайплайн станет зелёным
* Если будут вопросы по времени сдачи дз &ndash; мы будем ориентироваться на время последнего вашего действия в _Merge request_
* В какой-то момент выйдут тесты курса, их нужно будет подтянуть в свой _Merge request_
