package info.kgeorgiy.ja.khairullin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {
    /**
     * Creates a new account with specified identifier if it does not already exist.
     *
     * @param id account id
     * @return created or existing account.
     */
    Account createAnonymousAccount(String id) throws RemoteException;

    /**
     * Returns account by identifier.
     *
     * @param id account id
     * @return account with specified identifier or {@code null} if such account does not exist.
     */
    Account getAnonymousAccount(String id) throws RemoteException;

    Account createNamedAccount(String id, String passportID) throws RemoteException;

    Account getNamedAccount(String namedAccountID) throws RemoteException;

    Person createPerson(String name, String surname, String passportID) throws RemoteException;

    Person getPerson(String passportID, boolean shouldReturnLocal) throws RemoteException;
}
