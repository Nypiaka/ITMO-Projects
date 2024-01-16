package main

import (
	"context"
	"slices"
	"time"

	"testing"

	"github.com/stretchr/testify/require"

	"hw7/internal/generator"
	"hw7/internal/model"
	"hw7/internal/workerpool"
)

func TestSimple(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	orders := []model.OrderInitialized{
		{
			OrderID:     1,
			ProductID:   2,
			OrderStates: []model.OrderState{model.Initialized},
		},
	}

	init := 0
	started := 0
	finished := 0

	acts := model.OrderActions{
		InitToStarted:                                func() { init++ },
		StartedToFinishedExternalInteraction:         func() { started++ },
		FinishedExternalInteractionToProcessFinished: func() { finished++ },
	}

	orderWorkerPool := workerpool.NewOrderWorkerPoolImplementation()
	orderGenerator := generator.NewOrderGeneratorImplementation()

	result := orderWorkerPool.StartWorkerPool(ctx, orderGenerator.GenerateOrdersStream(ctx, orders), acts, 2)
	processedOrders := make([]model.Order, 0)

	for order := range result {
		processedOrders = append(processedOrders, finishedStateToOrder(order))
	}

	processedOrder := processedOrders[0]

	require.Equal(t, 1, len(processedOrders))
	require.Equal(t, 1, processedOrder.OrderID)
	require.Equal(t, 2, processedOrder.ProductID)
	require.Equal(t, 1, processedOrder.StorageID)
	require.Equal(t, 3, processedOrder.PickupPointID)
	require.Equal(t, true, processedOrder.IsProcessed)
	require.Equal(t, getFullOrderStates(), processedOrder.OrderStates)

	require.Equal(t, 1, init)
	require.Equal(t, 1, started)
	require.Equal(t, 1, finished)
}

func TestMultiple(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	orders := []model.OrderInitialized{
		{
			OrderID:     1,
			ProductID:   2,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     3,
			ProductID:   4,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     5,
			ProductID:   6,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     7,
			ProductID:   8,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     9,
			ProductID:   10,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     11,
			ProductID:   12,
			OrderStates: []model.OrderState{model.Initialized},
		},
		{
			OrderID:     13,
			ProductID:   14,
			OrderStates: []model.OrderState{model.Initialized},
		},
	}

	init := 0
	started := 0
	finished := 0

	acts := model.OrderActions{
		InitToStarted: func() {
			time.Sleep(time.Second * 3)
			init++
		},
		StartedToFinishedExternalInteraction: func() {
			time.Sleep(time.Second * 3)
			started++
		},
		FinishedExternalInteractionToProcessFinished: func() {
			time.Sleep(time.Second * 3)
			finished++
		},
	}

	orderWorkerPool := workerpool.NewOrderWorkerPoolImplementation()
	orderGenerator := generator.NewOrderGeneratorImplementation()

	result := orderWorkerPool.StartWorkerPool(ctx, orderGenerator.GenerateOrdersStream(ctx, orders), acts, 10)
	processedOrders := make([]model.Order, 0)

	for order := range result {
		processedOrders = append(processedOrders, finishedStateToOrder(order))
	}

	slices.SortFunc(processedOrders, func(a, b model.Order) int {
		return a.OrderID - b.OrderID
	})

	processedOrder0 := processedOrders[0]
	processedOrder1 := processedOrders[1]
	processedOrder2 := processedOrders[2]
	processedOrder3 := processedOrders[3]
	processedOrder4 := processedOrders[4]
	processedOrder5 := processedOrders[5]
	processedOrder6 := processedOrders[6]

	require.Equal(t, 7, len(processedOrders))
	require.Equal(t, 1, processedOrder0.OrderID)
	require.Equal(t, 2, processedOrder0.ProductID)
	require.Equal(t, 1, processedOrder0.StorageID)
	require.Equal(t, 3, processedOrder0.PickupPointID)
	require.Equal(t, true, processedOrder0.IsProcessed)
	require.Equal(t, getFullOrderStates(), processedOrder0.OrderStates)

	require.Equal(t, 3, processedOrder1.OrderID)
	require.Equal(t, 4, processedOrder1.ProductID)
	require.Equal(t, 1, processedOrder1.StorageID)
	require.Equal(t, 2, processedOrder1.PickupPointID)
	require.Equal(t, true, processedOrder1.IsProcessed)
	require.Equal(t, getFullOrderStates(), processedOrder1.OrderStates)

	require.Equal(t, 5, processedOrder2.OrderID)
	require.Equal(t, 6, processedOrder2.ProductID)
	require.Equal(t, 1, processedOrder2.StorageID)
	require.Equal(t, 1, processedOrder2.PickupPointID)
	require.Equal(t, true, processedOrder2.IsProcessed)
	require.Equal(t, getFullOrderStates(), processedOrder2.OrderStates)

	require.Equal(t, 7, processedOrder3.OrderID)
	require.Equal(t, 8, processedOrder3.ProductID)
	require.Equal(t, 1, processedOrder3.StorageID)
	require.Equal(t, 3, processedOrder3.PickupPointID)
	require.Equal(t, true, processedOrder3.IsProcessed)
	require.Equal(t, getFullOrderStates(), processedOrder3.OrderStates)

	require.Equal(t, 9, processedOrder4.OrderID)
	require.Equal(t, 10, processedOrder4.ProductID)
	require.Equal(t, 1, processedOrder4.StorageID)
	require.Equal(t, 2, processedOrder4.PickupPointID)
	require.Equal(t, true, processedOrder4.IsProcessed)
	require.Equal(t, getFullOrderStates(), processedOrder4.OrderStates)

	require.Equal(t, 11, processedOrder5.OrderID)
	require.Equal(t, 12, processedOrder5.ProductID)
	require.Equal(t, 1, processedOrder5.StorageID)
	require.Equal(t, 1, processedOrder5.PickupPointID)
	require.Equal(t, true, processedOrder5.IsProcessed)
	require.Equal(t, getFullOrderStates(), processedOrder5.OrderStates)

	require.Equal(t, 13, processedOrder6.OrderID)
	require.Equal(t, 14, processedOrder6.ProductID)
	require.Equal(t, 1, processedOrder6.StorageID)
	require.Equal(t, 3, processedOrder6.PickupPointID)
	require.Equal(t, true, processedOrder6.IsProcessed)
	require.Equal(t, getFullOrderStates(), processedOrder6.OrderStates)

	require.Equal(t, 7, init)
	require.Equal(t, 7, started)
	require.Equal(t, 7, finished)
}

func TestWithError(t *testing.T) {
	t.Parallel()

	ctx := context.Background()

	orders := []model.OrderInitialized{
		{
			OrderID:     1,
			ProductID:   2,
			OrderStates: []model.OrderState{model.Initialized},
		},
	}

	init := 0
	started := 0
	finished := 0

	acts := model.OrderActions{
		InitToStarted: func() {
			init++
			panic("test")
		},
		StartedToFinishedExternalInteraction: func() {
			started++
		},
		FinishedExternalInteractionToProcessFinished: func() {
			finished++
		},
	}

	orderWorkerPool := workerpool.NewOrderWorkerPoolImplementation()
	orderGenerator := generator.NewOrderGeneratorImplementation()

	result := orderWorkerPool.StartWorkerPool(ctx, orderGenerator.GenerateOrdersStream(ctx, orders), acts, 2)
	processedOrders := make([]model.Order, 0)

	for order := range result {
		processedOrders = append(processedOrders, finishedStateToOrder(order))
	}

	slices.SortFunc(processedOrders, func(a, b model.Order) int {
		return a.OrderID - b.OrderID
	})

	processedOrder := processedOrders[0]

	require.Equal(t, 1, len(processedOrders))
	require.Equal(t, 1, processedOrder.OrderID)
	require.Equal(t, 2, processedOrder.ProductID)
	require.Equal(t, 0, processedOrder.StorageID)
	require.Equal(t, 0, processedOrder.PickupPointID)
	require.Equal(t, false, processedOrder.IsProcessed)
	require.Equal(t, []model.OrderState{model.Initialized}, processedOrder.OrderStates)

	require.Equal(t, 1, init)
	require.Equal(t, 0, started)
	require.Equal(t, 0, finished)
}
