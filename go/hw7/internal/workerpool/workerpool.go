package workerpool

import (
	"context"
	"sync"

	"hw7/internal/model"
	"hw7/internal/pipeline"
)

type OrderWorkerPool interface {
	StartWorkerPool(ctx context.Context, orders <-chan model.OrderInitialized, additionalActions model.OrderActions, workersCount int) <-chan model.OrderProcessFinished
}

type OrderWorkerPoolImplementation struct{}

func NewOrderWorkerPoolImplementation() *OrderWorkerPoolImplementation {
	return &OrderWorkerPoolImplementation{}
}

func (o *OrderWorkerPoolImplementation) StartWorkerPool(ctx context.Context, orders <-chan model.OrderInitialized, additionalActions model.OrderActions, workersCount int) <-chan model.OrderProcessFinished {
	resultChan := make(chan model.OrderProcessFinished, len(orders))
	var wg sync.WaitGroup
	worker := func() {
		pipeline.NewOrderPipelineImplementation().Start(
			ctx,
			additionalActions,
			orders,
			resultChan,
			&wg,
		)
	}

	for i := 0; i < workersCount; i++ {
		wg.Add(1)
		go worker()
	}

	go func() {
		wg.Wait()
		close(resultChan)
	}()

	return resultChan
}
