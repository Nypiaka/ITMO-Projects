package main

import (
	"context"
	"runtime"
	"slices"
	"testing"
	"time"

	"github.com/stretchr/testify/require"

	"hw7/internal/generator"
	"hw7/internal/model"
	"hw7/internal/workerpool"
)

// Показательный тест. Чтобы он заработал, реализуйте generator
func TestSuccessProcess(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	orders := []model.OrderInitialized{
		{
			OrderID:     13,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
	}

	actions, countChecker := getDefaultAdditionalActions()
	orderWorkerPool := workerpool.NewOrderWorkerPoolImplementation()
	orderGenerator := generator.NewOrderGeneratorImplementation()

	result := orderWorkerPool.StartWorkerPool(ctx, orderGenerator.GenerateOrdersStream(ctx, orders), actions, 2)
	processedOrders := make([]model.Order, 0)

	for order := range result {
		processedOrders = append(processedOrders, finishedStateToOrder(order))
	}

	slices.SortFunc(processedOrders, func(a, b model.Order) int {
		return a.OrderID - b.OrderID
	})

	processedOrder := processedOrders[0]

	require.Equal(t, 1, len(processedOrders))
	require.Equal(t, 13, processedOrder.OrderID)
	require.Equal(t, 10, processedOrder.ProductID)
	require.Equal(t, 1, processedOrder.StorageID)
	require.Equal(t, 2, processedOrder.PickupPointID)
	require.Equal(t, true, processedOrder.IsProcessed)
	require.Equal(t, getFullOrderStates(), processedOrder.OrderStates)

	require.Equal(t, 1, countChecker.initToStartedCounter)
	require.Equal(t, 1, countChecker.StartedToFinishedExternalInteractionCounter)
	require.Equal(t, 1, countChecker.FinishedExternalInteractionToProcessFinishedCounter)
}

func TestPanicInitToStartActionProcess(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	orders := []model.OrderInitialized{
		{
			OrderID:     13,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
	}

	actions, countChecker := getDefaultAdditionalActions()
	actions.InitToStarted = func() {
		countChecker.initToStartedCounter++
		panic("test")
	}

	orderWorkerPool := workerpool.NewOrderWorkerPoolImplementation()
	orderGenerator := generator.NewOrderGeneratorImplementation()

	result := orderWorkerPool.StartWorkerPool(ctx, orderGenerator.GenerateOrdersStream(ctx, orders), actions, 2)
	processedOrders := make([]model.Order, 0)

	for order := range result {
		processedOrders = append(processedOrders, finishedStateToOrder(order))
	}

	slices.SortFunc(processedOrders, func(a, b model.Order) int {
		return a.OrderID - b.OrderID
	})

	processedOrder := processedOrders[0]

	require.Equal(t, 1, len(processedOrders))
	require.Equal(t, 13, processedOrder.OrderID)
	require.Equal(t, 10, processedOrder.ProductID)
	require.Equal(t, 0, processedOrder.StorageID)
	require.Equal(t, 0, processedOrder.PickupPointID)
	require.Equal(t, false, processedOrder.IsProcessed)
	require.Equal(t, []model.OrderState{model.Initialized}, processedOrder.OrderStates)

	require.Equal(t, 1, countChecker.initToStartedCounter)
	require.Equal(t, 0, countChecker.StartedToFinishedExternalInteractionCounter)
	require.Equal(t, 0, countChecker.FinishedExternalInteractionToProcessFinishedCounter)
}

func TestPanicFinishedExternalInteractionToProcessFinished(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	orders := []model.OrderInitialized{
		{
			OrderID:     13,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
	}

	actions, countChecker := getDefaultAdditionalActions()
	actions.FinishedExternalInteractionToProcessFinished = func() {
		countChecker.FinishedExternalInteractionToProcessFinishedCounter++
		panic("test")
	}

	orderWorkerPool := workerpool.NewOrderWorkerPoolImplementation()
	orderGenerator := generator.NewOrderGeneratorImplementation()

	result := orderWorkerPool.StartWorkerPool(ctx, orderGenerator.GenerateOrdersStream(ctx, orders), actions, 2)
	processedOrders := make([]model.Order, 0)

	for order := range result {
		processedOrders = append(processedOrders, finishedStateToOrder(order))
	}

	slices.SortFunc(processedOrders, func(a, b model.Order) int {
		return a.OrderID - b.OrderID
	})

	processedOrder := processedOrders[0]

	require.Equal(t, 1, len(processedOrders))
	require.Equal(t, 13, processedOrder.OrderID)
	require.Equal(t, 10, processedOrder.ProductID)
	require.Equal(t, 1, processedOrder.StorageID)
	require.Equal(t, 2, processedOrder.PickupPointID)
	require.Equal(t, false, processedOrder.IsProcessed)
	require.Equal(t, []model.OrderState{model.Initialized, model.ProcessStarted, model.FinishedExternalInteraction},
		processedOrder.OrderStates)

	require.Equal(t, 1, countChecker.initToStartedCounter)
	require.Equal(t, 1, countChecker.StartedToFinishedExternalInteractionCounter)
	require.Equal(t, 1, countChecker.FinishedExternalInteractionToProcessFinishedCounter)
}

func TestFanPerformance(t *testing.T) {
	runtime.GOMAXPROCS(1)
	ctx := context.Background()

	orders := []model.OrderInitialized{
		{
			OrderID:     13,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     14,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     15,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     16,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     17,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     18,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     19,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
	}

	actions, countChecker := getDefaultAdditionalActions()

	actions.StartedToFinishedExternalInteraction = func() {
		countChecker.StartedToFinishedExternalInteractionCounter++
		time.Sleep(time.Second * 5)
	}

	orderWorkerPool := workerpool.NewOrderWorkerPoolImplementation()
	orderGenerator := generator.NewOrderGeneratorImplementation()

	result := orderWorkerPool.StartWorkerPool(ctx, orderGenerator.GenerateOrdersStream(ctx, orders), actions, 1)
	processedOrders := make([]model.Order, 0)

	ticker := time.NewTicker(time.Second * 10)

	func() {
		for {
			select {
			case o, ok := <-result:
				if !ok {
					return
				}

				processedOrders = append(processedOrders, finishedStateToOrder(o))
			case <-ticker.C:
				t.Fail()
			}
		}
	}()

	slices.SortFunc(processedOrders, func(a, b model.Order) int {
		return a.OrderID - b.OrderID
	})

	require.Equal(t, 7, len(processedOrders))

	require.Equal(t, 7, countChecker.initToStartedCounter)
	require.Equal(t, 7, countChecker.StartedToFinishedExternalInteractionCounter)
	require.Equal(t, 7, countChecker.FinishedExternalInteractionToProcessFinishedCounter)
}

func getFullOrderStates() []model.OrderState {
	return []model.OrderState{model.Initialized, model.ProcessStarted, model.FinishedExternalInteraction, model.ProcessFinished}
}

type CountChecker struct {
	initToStartedCounter                                int
	StartedToFinishedExternalInteractionCounter         int
	FinishedExternalInteractionToProcessFinishedCounter int
}

func getDefaultAdditionalActions() (model.OrderActions, *CountChecker) {
	countChecker := &CountChecker{}

	initToStarted := func() {
		countChecker.initToStartedCounter++
	}

	startedToFinishedExternalInteraction := func() {
		countChecker.StartedToFinishedExternalInteractionCounter++
	}

	finishedExternalInteractionToProcessFinished := func() {
		countChecker.FinishedExternalInteractionToProcessFinishedCounter++
	}

	orderActions := model.OrderActions{
		InitToStarted:                                initToStarted,
		StartedToFinishedExternalInteraction:         startedToFinishedExternalInteraction,
		FinishedExternalInteractionToProcessFinished: finishedExternalInteractionToProcessFinished,
	}

	return orderActions, countChecker
}

func finishedStateToOrder(finished model.OrderProcessFinished) model.Order {
	order := model.Order{
		OrderID:       finished.OrderFinishedExternalInteraction.OrderProcessStarted.OrderInitialized.OrderID,
		ProductID:     finished.OrderFinishedExternalInteraction.OrderProcessStarted.OrderInitialized.ProductID,
		StorageID:     finished.OrderFinishedExternalInteraction.StorageID,
		PickupPointID: finished.OrderFinishedExternalInteraction.PickupPointID,
		OrderStates:   finished.OrderStates,
	}

	if finished.Error == nil {
		order.IsProcessed = true
	}

	return order
}
