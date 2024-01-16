package model

import "errors"

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

type OrderStates interface {
	OrderInitialized | OrderProcessStarted | OrderFinishedExternalInteraction | OrderProcessFinished
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

func Init(order *OrderInitialized, actions *OrderActions) {
	defer func() {
		if err := recover(); err != nil {
			order.Error = errors.New("error")
		}
	}()
	actions.InitToStarted()
	order.OrderStates = append(order.OrderStates, ProcessStarted)
}

func Proc(order *OrderFinishedExternalInteraction, actions *OrderActions) {
	defer func() {
		if err := recover(); err != nil {
			order.Error = errors.New("error")
		}
	}()
	actions.StartedToFinishedExternalInteraction()
	order.StorageID = (order.OrderProcessStarted.OrderInitialized.ProductID % 2) + 1
	order.PickupPointID = (order.OrderProcessStarted.OrderInitialized.ProductID % 3) + 1
	order.OrderStates = append(order.OrderStates, FinishedExternalInteraction)
}

func Finish(order *OrderFinishedExternalInteraction, actions *OrderActions) {
	defer func() {
		if err := recover(); err != nil {
			order.Error = errors.New("error")
		}
	}()
	actions.FinishedExternalInteractionToProcessFinished()
	order.OrderStates = append(order.OrderStates, ProcessFinished)
}
