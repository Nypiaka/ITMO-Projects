package bank

import (
	"github.com/stretchr/testify/require"
	"testing"
)

func BalanceTestTemplate(t *testing.T, account *AccountImpl, operation OperationType, amount int, awaitBalance int, awaitSuccess bool) {
	success := false
	switch operation {
	case TopUpOp:
		success = account.TopUp(amount)
	case WithdrawOp:
		success = account.Withdraw(amount)
	}
	require.Equal(t, awaitSuccess, success)
	require.Equal(t, awaitBalance, account.Balance())
}

func TestBalanceWithDraw(t *testing.T) {
	testingAccount := NewAccount(NewMockTime())
	testingAccount.TopUp(1000)
	BalanceTestTemplate(t, testingAccount, WithdrawOp, 100, 900, true)
	BalanceTestTemplate(t, testingAccount, WithdrawOp, 1000, 900, false)
	BalanceTestTemplate(t, testingAccount, WithdrawOp, 800, 100, true)
	BalanceTestTemplate(t, testingAccount, WithdrawOp, 100, 0, true)
	BalanceTestTemplate(t, testingAccount, WithdrawOp, 100, 0, false)
	BalanceTestTemplate(t, testingAccount, WithdrawOp, -100, 0, false)
}

func TestBalanceTopUp(t *testing.T) {
	testingAccount := NewAccount(NewMockTime())
	BalanceTestTemplate(t, testingAccount, TopUpOp, 100, 100, true)
	BalanceTestTemplate(t, testingAccount, TopUpOp, 1000, 1100, true)
	BalanceTestTemplate(t, testingAccount, TopUpOp, -1000, 1100, false)
}

func TestOperations_my(t *testing.T) {
	testingAccount := NewAccount(NewMockTime())
	testingAccount.TopUp(10)
	testingAccount.TopUp(-10)
	testingAccount.Withdraw(5)
	testingAccount.TopUp(1000)
	time := NewMockTime()
	operations := []Operation{
		{
			OpTime:   time.Now(),
			OpType:   TopUpOp,
			OpAmount: 10,
			Balance:  10,
		},
		{
			OpTime:   time.Now(),
			OpType:   WithdrawOp,
			OpAmount: 5,
			Balance:  5,
		},
		{
			OpTime:   time.Now(),
			OpType:   TopUpOp,
			OpAmount: 1000,
			Balance:  1005,
		},
	}
	require.Equal(t, operations, testingAccount.Operations())
}

func TestString(t *testing.T) {
	testingAccount := NewAccount(NewMockTime())
	require.Equal(t, "", testingAccount.Statement())
	testingAccount.TopUp(10)
	testingAccount.TopUp(-10)
	testingAccount.Withdraw(5)
	testingAccount.TopUp(1000)
	require.Equal(t, "2023-03-18 12:34:07 +10 10\n2023-03-18 12:34:37 -5 5\n2023-03-18 12:35:07 +1000 1005",
		testingAccount.Statement())
}
