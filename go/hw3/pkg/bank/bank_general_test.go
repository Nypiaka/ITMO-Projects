package bank

import (
    "github.com/stretchr/testify/assert"
    "testing"
    "time"
)

func NewMockTime() *MockClock {
    return &MockClock{
        t: time.Unix(1679142817, 0).UTC(),
    }
}

type MockClock struct {
    t time.Time
}

func (c *MockClock) Now() time.Time {
    c.t = c.t.Add(time.Second * 30)
    return c.t
}

func TestBalance(t *testing.T) {
    account := NewAccount(NewMockTime())
    want := 0
    got := account.Balance()

    if want != got {
        t.Errorf("wrong: want %d, got %d", want, got)
    }
}

// ======================= Balance ==========

type TestCase_balance struct {
    account     Account
    wantBalance int
}

func (tc TestCase_balance) Run(t *testing.T) {
    assert.Equal(t, tc.wantBalance, tc.account.Balance())
}

func Test_balance_NewAccount(t *testing.T) {
    TestCase_balance{
        account:     NewAccount(NewMockTime()),
        wantBalance: 0,
    }.Run(t)
}

// ======================= TopUp ============

type TestCase_topUp struct {
    account     Account
    amount      int
    wantIsTopUp bool
    wantBalance int
}

func (tc TestCase_topUp) Run(t *testing.T) {
    gotIsTopUp := tc.account.TopUp(tc.amount)
    assert.Equal(t, tc.wantIsTopUp, gotIsTopUp)
    assert.Equal(t, tc.wantBalance, tc.account.Balance())
}

func Test_topUp_NewAccount(t *testing.T) {
    TestCase_topUp{
        account:     NewAccount(NewMockTime()),
        amount:      100,
        wantIsTopUp: true,
        wantBalance: 100,
    }.Run(t)
}

func Test_topUp_100Account(t *testing.T) {
    acc := NewAccount(NewMockTime())
    acc.TopUp(100)
    TestCase_topUp{
        account:     acc,
        amount:      100,
        wantIsTopUp: true,
        wantBalance: 200,
    }.Run(t)
}

func Test_topUp_100Account_1(t *testing.T) {
    acc := NewAccount(NewMockTime())
    acc.TopUp(100)
    TestCase_topUp{
        account:     acc,
        amount:      -100,
        wantIsTopUp: false,
        wantBalance: 100,
    }.Run(t)
}

func Test_topUp_100Account_2(t *testing.T) {
    acc := NewAccount(NewMockTime())
    acc.TopUp(100)
    TestCase_topUp{
        account:     acc,
        amount:      0,
        wantIsTopUp: false,
        wantBalance: 100,
    }.Run(t)
}

// ======================= Withdraw =========

type TestCase_withdraw struct {
    account        Account
    amount         int
    wantIsWithdraw bool
    wantBalance    int
}

func (tc TestCase_withdraw) Run(t *testing.T) {
    gotIsTopUp := tc.account.Withdraw(tc.amount)
    assert.Equal(t, tc.wantIsWithdraw, gotIsTopUp)
    assert.Equal(t, tc.wantBalance, tc.account.Balance())
}

func Test_withdraw_NewAccount(t *testing.T) {
    TestCase_withdraw{
        account:        NewAccount(NewMockTime()),
        amount:         100,
        wantIsWithdraw: false,
        wantBalance:    0,
    }.Run(t)
}

func Test_withdraw_NewAccount_1(t *testing.T) {
    TestCase_withdraw{
        account:        NewAccount(NewMockTime()),
        amount:         -10,
        wantIsWithdraw: false,
        wantBalance:    0,
    }.Run(t)
}

func Test_withdraw_NewAccount_2(t *testing.T) {
    TestCase_withdraw{
        account:        NewAccount(NewMockTime()),
        amount:         10,
        wantIsWithdraw: false,
        wantBalance:    0,
    }.Run(t)
}

func Test_withdraw_100Account(t *testing.T) {
    acc := NewAccount(NewMockTime())
    acc.TopUp(100)
    TestCase_withdraw{
        account:        acc,
        amount:         100,
        wantIsWithdraw: true,
        wantBalance:    0,
    }.Run(t)
}

func Test_withdraw_100Account_1(t *testing.T) {
    acc := NewAccount(NewMockTime())
    acc.TopUp(100)
    TestCase_withdraw{
        account:        acc,
        amount:         101,
        wantIsWithdraw: false,
        wantBalance:    100,
    }.Run(t)
}

func Test_withdraw_100Account_2(t *testing.T) {
    acc := NewAccount(NewMockTime())
    acc.TopUp(100)
    TestCase_withdraw{
        account:        acc,
        amount:         -101,
        wantIsWithdraw: false,
        wantBalance:    100,
    }.Run(t)
}

func Test_withdraw_100Account_3(t *testing.T) {
    acc := NewAccount(NewMockTime())
    acc.TopUp(100)
    TestCase_withdraw{
        account:        acc,
        amount:         51,
        wantIsWithdraw: true,
        wantBalance:    49,
    }.Run(t)
}

//2023-03-18 12:34:07 +100 100
//2023-03-18 12:34:37 -50 50
//2023-03-18 12:35:07 +150 200

//clock := NewMockTime()
//times := []time.Time{clock.Now(), clock.Now(), clock.Now()}

// ======================= Operations =======

type TestOperations struct {
    opType OperationType
    amount int
}

type TestCase_operations struct {
    account        Account
    operations     []TestOperations
    wantOperations []Operation
}

func (tc TestCase_operations) Run(t *testing.T) {
    for _, op := range tc.operations {
        switch op.opType {
        case TopUpOp:
            {
                tc.account.TopUp(op.amount)
            }
        case WithdrawOp:
            {
                tc.account.Withdraw(op.amount)
            }
        }
    }
    assert.Equal(t, tc.wantOperations, tc.account.Operations())
}

func Test_operations_NoOperations(t *testing.T) {
    TestCase_operations{
        account:        NewAccount(NewMockTime()),
        operations:     []TestOperations{},
        wantOperations: []Operation{},
    }.Run(t)
}

func Test_operations_ManyOperations(t *testing.T) {
    clock := NewMockTime()
    TestCase_operations{
        account: NewAccount(NewMockTime()),
        operations: []TestOperations{
            {
                opType: TopUpOp,
                amount: 100,
            },
            {
                opType: WithdrawOp,
                amount: 101,
            },
            {
                opType: WithdrawOp,
                amount: 50,
            },
        },
        wantOperations: []Operation{
            {
                OpTime:   clock.Now(),
                OpType:   TopUpOp,
                OpAmount: 100,
                Balance:  100,
            },
            {
                OpTime:   clock.Now(),
                OpType:   WithdrawOp,
                OpAmount: 50,
                Balance:  50,
            },
        },
    }.Run(t)
}

func Test_operations_WrongOperations(t *testing.T) {
    TestCase_operations{
        account: NewAccount(NewMockTime()),
        operations: []TestOperations{
            {
                opType: TopUpOp,
                amount: -100,
            },
            {
                opType: WithdrawOp,
                amount: 101,
            },
            {
                opType: WithdrawOp,
                amount: -50,
            },
            {
                opType: TopUpOp,
                amount: 0,
            },
            {
                opType: WithdrawOp,
                amount: 0,
            },
        },
        wantOperations: []Operation{},
    }.Run(t)
}

// ======================= Statement ========

type TestCase_statement struct {
    account       Account
    operations    []TestOperations
    wantStatement string
}

func (tc TestCase_statement) Run(t *testing.T) {
    for _, op := range tc.operations {
        switch op.opType {
        case TopUpOp:
            {
                tc.account.TopUp(op.amount)
            }
        case WithdrawOp:
            {
                tc.account.Withdraw(op.amount)
            }
        }
    }
    assert.Equal(t, tc.wantStatement, tc.account.Statement())
}

func Test_statement_NoOperations(t *testing.T) {
    TestCase_statement{
        account:       NewAccount(NewMockTime()),
        operations:    []TestOperations{},
        wantStatement: "",
    }.Run(t)
}

func Test_statement_ManyOperations(t *testing.T) {
    TestCase_statement{
        account: NewAccount(NewMockTime()),
        operations: []TestOperations{
            {
                opType: TopUpOp,
                amount: 100,
            },
            {
                opType: WithdrawOp,
                amount: 101,
            },
            {
                opType: WithdrawOp,
                amount: 50,
            },
        },
        wantStatement: `2023-03-18 12:34:07 +100 100
2023-03-18 12:34:37 -50 50`,
    }.Run(t)
}

func Test_statement_WrongOperations(t *testing.T) {
    TestCase_statement{
        account: NewAccount(NewMockTime()),
        operations: []TestOperations{
            {
                opType: TopUpOp,
                amount: -100,
            },
            {
                opType: WithdrawOp,
                amount: 101,
            },
            {
                opType: WithdrawOp,
                amount: -50,
            },
            {
                opType: TopUpOp,
                amount: 0,
            },
            {
                opType: WithdrawOp,
                amount: 0,
            },
        },
        wantStatement: "",
    }.Run(t)
}
