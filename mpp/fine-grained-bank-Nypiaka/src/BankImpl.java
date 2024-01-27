import java.util.concurrent.locks.ReentrantLock;

/**
 * Bank implementation.
 *
 * <p>
 * :TODO: This implementation has to be made thread-safe.
 *
 * @author KHairullin Alexander
 */
public class BankImpl implements Bank {
    /**
     * An array of accounts by index.
     */
    private final Account[] accounts;

    /**
     * Creates new bank instance.
     * 
     * @param n the number of accounts (numbered from 0 to n-1).
     */
    public BankImpl(int n) {
        accounts = new Account[n];
        for (int i = 0; i < n; i++) {
            accounts[i] = new Account();
        }
    }

    @Override
    public int getNumberOfAccounts() {
        return accounts.length;
    }

    /**
     * <p>
     * :TODO: This method has to be made thread-safe.
     */
    @Override
    public long getAmount(int index) {
        synchronized (accounts) {
            var res = accounts[index].amount;
            accounts.notify();
            return res;
        }
    }

    /**
     * <p>
     * :TODO: This method has to be made thread-safe.
     */
    @Override
    public long getTotalAmount() {
        long sum = 0;
        synchronized (accounts) {
            for (Account account : accounts) {
                sum += account.amount;
            }
            accounts.notify();
        }
        return sum;
    }

    /**
     * <p>
     * :TODO: This method has to be made thread-safe.
     */
    @Override
    public long deposit(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        synchronized (accounts) {
            Account account = accounts[index];
            if (amount > MAX_AMOUNT || account.amount + amount > MAX_AMOUNT)
                throw new IllegalStateException("Overflow");
            account.add(amount);
            var res = account.amount;
            accounts.notify();
            return res;
        }
    }

    /**
     * <p>
     * :TODO: This method has to be made thread-safe.
     */
    @Override
    public long withdraw(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        synchronized (accounts) {
            Account account = accounts[index];
            if (account.amount - amount < 0)
                throw new IllegalStateException("Underflow");
            account.add(-amount);
            var res = account.amount;
            accounts.notify();
            return res;
        }
    }

    /**
     * <p>
     * :TODO: This method has to be made thread-safe.
     */
    @Override
    public void transfer(int fromIndex, int toIndex, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        if (fromIndex == toIndex)
            throw new IllegalArgumentException("fromIndex == toIndex");
        Account from;
        Account to;
        synchronized (accounts) {
            from = accounts[fromIndex];
            to = accounts[toIndex];
            if (amount > from.amount)
                throw new IllegalStateException("Underflow");
            else if (amount > MAX_AMOUNT || to.amount + amount > MAX_AMOUNT)
                throw new IllegalStateException("Overflow");
            from.add(-amount);
            to.add(+amount);
            accounts.notify();
        }
    }

    /**
     * Private account data structure.
     */
    static class Account {
        /**
         * Amount of funds in this account.
         */
        ReentrantLock lock = new ReentrantLock();
        volatile long amount;

        void add(long amount) {
            lock.lock();
            try {
                this.amount += amount;
            } finally {
                lock.unlock();
            }
        }
    }
}
