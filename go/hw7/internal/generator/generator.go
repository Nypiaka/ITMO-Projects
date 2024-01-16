package generator

import (
	"context"

	"hw7/internal/model"
)

type OrderGenerator interface {
	GenerateOrdersStream(ctx context.Context, orders []model.OrderInitialized) <-chan model.OrderInitialized
}

type OrderGeneratorImplementation struct{}

func NewOrderGeneratorImplementation() *OrderGeneratorImplementation {
	return &OrderGeneratorImplementation{}
}

func (o *OrderGeneratorImplementation) GenerateOrdersStream(ctx context.Context, orders []model.OrderInitialized) <-chan model.OrderInitialized {
	generatedOrders := make(chan model.OrderInitialized)

	go func() {
		defer close(generatedOrders)
		for _, order := range orders {
			select {
			case <-ctx.Done():
				return
			case generatedOrders <- order:
			}
		}
	}()

	return generatedOrders
}
