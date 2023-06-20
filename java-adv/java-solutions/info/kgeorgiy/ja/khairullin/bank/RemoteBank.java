package info.kgeorgiy.ja.khairullin.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class RemoteBank implements Bank {
    private final int port;
    private final Map<String, Account> anonymousAccounts = new ConcurrentHashMap<>();

    private final Map<String, Map<String, Account>> namedAccounts = new ConcurrentHashMap<>();

    private final Map<String, RemotePerson> remotePersons = new ConcurrentHashMap<>();

    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Account createAnonymousAccount(final String id) throws RemoteException {
        System.out.println("Creating account " + id);
        final Account account = new UniversalAccount(id);
        if (anonymousAccounts.putIfAbsent(id, account) == null) {
            UnicastRemoteObject.exportObject(account, port);
            return account;
        } else {
            return getAnonymousAccount(id);
        }
    }

    @Override
    public Account getAnonymousAccount(final String id) {
        System.out.println("Retrieving account " + id);
        return anonymousAccounts.get(id);
    }

    @Override
    public Account createNamedAccount(String id, String passportID) throws RemoteException {
        System.out.println("Creating new account for person");
        Person currentPerson = remotePersons.get(passportID);
        if (currentPerson == null) {
            System.out.println("Person not found");
            return null;
        }
        String subId = id == null ? String.valueOf(currentPerson.getAccounts().size()) : id;
        String currentAccountID = passportID + ":" + subId;
        Account maybeReal = namedAccounts.get(passportID).get(subId);
        if (maybeReal != null) return maybeReal;
        Account accountToAdd = new UniversalAccount(currentAccountID);
        UnicastRemoteObject.exportObject(accountToAdd, port);
        namedAccounts.get(passportID).put(subId, accountToAdd);
        return accountToAdd;
    }

    @Override
    public Account getNamedAccount(String namedAccountID) throws RemoteException {
        System.out.println("Retrieving named account " + namedAccountID);
        String[] passportAndId = namedAccountID.split(":", 2);
        // :NOTE: asser two parts
        Map<String, Account> currentMap = namedAccounts.get(passportAndId[0]);
        if (currentMap == null) {
            return null;
        }
        return currentMap.get(passportAndId[1]);
    }

    @Override
    public Person createPerson(String name, String surname, String passportID) throws RemoteException {
        // :NOTE: printf
        System.out.println("Creating person. Name: " + name + ". Surname: " + surname + ". passportID: " + passportID);
        Person remotePerson = getPerson(passportID, false);
        if (remotePerson != null) {
            return remotePerson;
        }
        Map<String, Account> commonMap = new ConcurrentHashMap<>();
        namedAccounts.put(passportID, commonMap);
        remotePersons.put(passportID, new RemotePerson(name, surname, passportID, commonMap));
        return remotePersons.get(passportID);
    }

    @Override
    public Person getPerson(final String passportID, boolean shouldReturnLocal) {
        Person returnPerson = remotePersons.get(passportID);
        if (returnPerson == null) return null;
        return shouldReturnLocal ? new LocalPerson(returnPerson) : returnPerson;
    }
}
