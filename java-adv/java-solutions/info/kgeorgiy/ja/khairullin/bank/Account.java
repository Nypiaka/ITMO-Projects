package info.kgeorgiy.ja.khairullin.bank;

import java.io.Serializable;
import java.rmi.*;

public interface Account extends Remote, Serializable {
    /**
     * Returns account identifier.
     */
    String getId() throws RemoteException;

    /**
     * Returns amount of money in the account.
     */
    long getAmount() throws RemoteException;

    /**
     * Sets amount of money in the account.
     */
    void setAmount(long amount) throws RemoteException;

    Account copyAccount() throws RemoteException;
}
