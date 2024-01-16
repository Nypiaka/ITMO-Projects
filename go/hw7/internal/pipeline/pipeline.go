package pipeline

import (
	"context"
	"sync"

	"hw7/internal/model"
)

const fanLimit = 10

type OrderPipeline interface {
	Start(ctx context.Context, actions model.OrderActions, orders <-chan model.OrderInitialized, processed chan<- model.OrderProcessFinished)
}

type OrderPipelineImplementation struct{}

func NewOrderPipelineImplementation() *OrderPipelineImplementation {
	return &OrderPipelineImplementation{}
}

func (o *OrderPipelineImplementation) Start(
	ctx context.Context,
	actions model.OrderActions,
	orders <-chan model.OrderInitialized,
	processed chan<- model.OrderProcessFinished,
	waitGroup *sync.WaitGroup,
) {
	firstChan := make(chan model.OrderProcessStarted)
	secondChan := make(chan model.OrderFinishedExternalInteraction)

	go func() {
		for order := range orders {
			model.Init(&order, &actions)
			firstChan <- model.OrderProcessStarted{
				OrderInitialized: order,
				OrderStates:      order.OrderStates,
				Error:            order.Error,
			}
		}
		close(firstChan)
	}()

	var wg sync.WaitGroup
	for i := 0; i < fanLimit; i++ {
		wg.Add(1)
		go func() {
			for order := range firstChan {
				if order.Error == nil {
					actions.StartedToFinishedExternalInteraction()
					secondChan <- model.OrderFinishedExternalInteraction{
						OrderProcessStarted: order,
						StorageID:           order.OrderInitialized.ProductID%2 + 1,
						PickupPointID:       order.OrderInitialized.ProductID%3 + 1,
						OrderStates:         append(order.OrderStates, model.FinishedExternalInteraction),
						Error:               order.Error,
					}
				} else {
					secondChan <- model.OrderFinishedExternalInteraction{
						OrderProcessStarted: order,
						StorageID:           0,
						PickupPointID:       0,
						OrderStates:         order.OrderStates,
						Error:               order.Error,
					}
				}
			}
			wg.Done()
		}()
	}

	var wg2 sync.WaitGroup
	wg2.Add(1)

	go func() {
		wg.Wait()
		close(secondChan)
		wg2.Wait()
		waitGroup.Done()

	}()

	go func() {
		for order := range secondChan {
			if order.Error == nil {
				model.Finish(&order, &actions)
			}
			processed <- model.OrderProcessFinished{OrderFinishedExternalInteraction: order, OrderStates: order.OrderStates, Error: order.Error}
		}
		wg2.Done()
	}()
}
