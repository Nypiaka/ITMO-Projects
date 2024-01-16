package bank

import (
	"fmt"
	"strings"
	"time"
)

const (
	TopUpOp OperationType = iota
	WithdrawOp
)

type OperationType int64

type Clock interface {
	Now() time.Time
}

func NewRealTime() *RealClock {
	return &RealClock{}
}

type RealClock struct{}

func (c *RealClock) Now() time.Time {
	return time.Now()
}

type Operation struct {
	OpTime   time.Time
	OpType   OperationType
	OpAmount int
	Balance  int
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
	TopUp(amount int) bool
	Withdraw(amount int) bool
	Operations() []Operation
	Statement() string
	Balance() int
}

type AccountImpl struct {
	balance    int
	operations []Operation
	clock      Clock
	builder    strings.Builder
}

func (a *AccountImpl) TopUp(amount int) bool {
	if amount <= 0 {
		return false
	}
	a.balance += amount
	a.log(amount, TopUpOp)
	return true
}

func (a *AccountImpl) Withdraw(amount int) bool {
	if amount <= 0 || amount > a.balance {
		return false
	}
	a.balance -= amount
	a.log(amount, WithdrawOp)
	return true
}

func (a *AccountImpl) log(amount int, operationType OperationType) {
	operation := Operation{a.clock.Now(), operationType, amount, a.balance}
	a.operations = append(a.operations, operation)
	a.builder.WriteString(operation.String() + "\n")
}

func (a *AccountImpl) Operations() []Operation {
	return a.operations
}

func (a *AccountImpl) Statement() string {
	if len(a.operations) == 0 {
		return ""
	}
	return a.builder.String()[:len(a.builder.String())-1]
}

func (a *AccountImpl) Balance() int {
	return a.balance
}

func NewAccount(clock Clock) *AccountImpl {
	return &AccountImpl{
		balance:    0,
		operations: make([]Operation, 0),
		builder:    strings.Builder{},
		clock:      clock,
	}
}
