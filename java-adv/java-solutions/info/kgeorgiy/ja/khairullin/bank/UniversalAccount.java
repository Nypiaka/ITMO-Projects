package info.kgeorgiy.ja.khairullin.bank;

import java.rmi.RemoteException;

public class UniversalAccount implements Account {
    private final String id;
    private long amount;

    public UniversalAccount(final String id) {
        this.id = id;
        this.amount = 0;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public synchronized long getAmount() {
        System.out.println("Getting amount of money for account " + id + ". sum:" + this.amount);
        return this.amount;
    }

    @Override
    public synchronized void setAmount(final long amount) {
        System.out.println("Setting amount of money for account " + this.id + ". sum:" + this.amount);
        this.amount = amount;
    }

    @Override
    public synchronized Account copyAccount() throws RemoteException {
        Account accountToReturn = new UniversalAccount(this.id);
        accountToReturn.setAmount(this.getAmount());
        return accountToReturn;
    }
}
