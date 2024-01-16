# HW7 &ndash; Concurrency patterns

---

## Обработка заказов

### Задача: Обработать заказы с помощью n воркеров с применением паттернов Pipeline, WorkerPool и Fan In/Out. Параметр n задается при инициализации вокерпула.

### Даны следующие модели

`internal/model/model.go`

```golang
package model

type OrderState string

const (
    Initialized                 OrderState = "order_initialized"
    ProcessStarted              OrderState = "order_process_started"
    FinishedExternalInteraction OrderState = "order_finished_external_interaction"
    ProcessFinished             OrderState = "order_process_finished"
)

type OrderActions struct {
    InitToStarted                                func()
    StartedToFinishedExternalInteraction         func()
    FinishedExternalInteractionToProcessFinished func()
}

type OrderInitialized struct {
    OrderID     int
    ProductID   int
    OrderStates []OrderState
    Error       error
}

type OrderProcessStarted struct {
    OrderInitialized OrderInitialized
    OrderStates      []OrderState
    Error            error
}

type OrderFinishedExternalInteraction struct {
    OrderProcessStarted OrderProcessStarted
    StorageID           int
    PickupPointID       int
    OrderStates         []OrderState
    Error               error
}

type OrderProcessFinished struct {
    OrderFinishedExternalInteraction OrderFinishedExternalInteraction
    OrderStates                      []OrderState
    Error                            error
}

type Order struct {
    OrderID       int
    ProductID     int
    StorageID     int
    PickupPointID int
    IsProcessed   bool
    OrderStates   []OrderState
}
```

### Вам необходимо реализовать workerpool

`internal/workerpool/workerpool.go`

```golang
type OrderWorkerPool interface {
    StartWorkerPool(ctx context.Context, orders <-chan model.OrderInitialized, additionalActions model.OrderActions, workersCount int) <-chan model.OrderProcessFinished
}

type OrderWorkerPoolImplementation struct{}

func NewOrderWorkerPoolImplementation() *OrderWorkerPoolImplementation {
    return &OrderWorkerPoolImplementation{}
}
```

### Вместе с паттерном workerpool должен быть реализован паттерн pipeline, соответствующий стадиям обработки заказа

```golang
type OrderPipeline interface {
    Start(ctx context.Context, actions model.OrderActions, orders <-chan model.OrderInitialized, processed chan<- model.OrderProcessFinished)
}

type OrderPipelineImplementation struct{}

func NewOrderPipelineImplementation() *OrderPipelineImplementation {
    return &OrderPipelineImplementation{}
}
```

#### 1. Initialized - > ProcessStarted
- Вызывается фукнция InitToStarted из additionalActions
- В массив состояний добавляется новое состояние "order_process_started"


#### 2. ProcessStarted -> FinishedExternalInteraction
-  Вызывается фукнция StartedToFinishedExternalInteraction из additionalActions
- Инициализируется склад для заказа - результат взятия ID товара по модулю 2 с добавлением единицы
-  Инициализируется пункт выдачи для заказа - результат взятия ID товара по модулю 3 с добавлением единицы
- В массив состояний добавляется новое состояние "order_finished_external_interaction"

#### 3. FinishedExternalInteraction -> ProcessFinished
-  Вызывается фукнция FinishedExternalInteractionToProcessFinished из additionalActions
-  В массив состояний добавляется новое состояние "order_process_finished"

### Примечания
-  Стадию ProcessStarted -> FinishedExternalInteraction необходимо реализовать с применением паттернов Fan In и Fan Out (k = 10)
-  При возникновении ошибки на какой-либо стадии, на следующих не должны выполняться никакие действия, связанные с обработкой заказа
-  При сомнениях в реализции стоит ориентироваться на авторские тесты

### Тестирование
 Помимо авторских тестов вам необходимо написать собственные. При написании тестов рекомендуется использовать паттерн generator

```golang
type OrderGenerator interface {
    GenerateOrdersStream(ctx context.Context, orders []model.OrderInitialized) <-chan model.OrderInitialized
}

type OrderGeneratorImplementation struct{}

func NewOrderGeneratorImplementation() *OrderGeneratorImplementation {
    return &OrderGeneratorImplementation{}
}

func (o *OrderGeneratorImplementation) GenerateOrdersStream(ctx context.Context, orders []model.OrderInitialized) <-chan model.OrderInitialized {
}
```

### Запрещается в задании:
* Менять тесты курса
* Пользоваться сторонними библиотеками в которых есть API, которое предоставляет использование конкурентных патернов для реализации задания
---

## Как сдавать:
* Проверить, что появился **ваш личный** репозиторий с `HW7`, сделать его клон,
  личный репозиторий имеет вид `https://tinkoff-edu.gitlab.yandexcloud.net/itmo-course-autumn-2023/Students/<username>/Homeworks/HW7`
* Добавить ваше решение в ветку `hw`
* Добавить файл конфигурации `.gitlab-ci.yml` для запуска пайплайна с тестами
* Открыть _Merge request_ из ветки `hw` в ветку `master` **вашего репозитория** (не основного)
* Дождаться, когда пайплайн станет зелёным
* Если будут вопросы по времени сдачи дз &ndash; мы будем ориентироваться на время последнего вашего действия в _Merge request_.