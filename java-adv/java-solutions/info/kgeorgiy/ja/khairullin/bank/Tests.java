package info.kgeorgiy.ja.khairullin.bank;


import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Phaser;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertThrows;


@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class Tests {
    private static Bank bank;

    private final List<String> names = List.of(
            "Alex",
            "Sentiy",
            "Vadim",
            "Ervin",
            "Kostya",
            "Gosha",
            "Gera",
            "Vanya");
    private final List<String> surnames = List.of(
            "Ivanov",
            "Petrov",
            "Sidorov",
            "Мяумяу",
            "普京",
            "پریگوژین",
            "ווילם השלישי",
            "Свинінін");
    private final List<String> passportIDs = List.of(
            "1111111111",
            "2222222222",
            "3333333333",
            "4444444444",
            "5555555555",
            "6666666666",
            "7777777777",
            "8888888888");

    private final String HELPFUL_PART = ":0";

    @BeforeClass
    public static void beforeClass() throws RemoteException, NotBoundException {
        final Registry registry = LocateRegistry.createRegistry(8888);
        registry.rebind("//localhost/bank", new RemoteBank(8181));
        bank = (Bank) registry.lookup("//localhost/bank");
        UnicastRemoteObject.exportObject(bank, 8888);
    }

    private void createAccountsUsingMultiThread(int numOfAccounts, int numOfPerson, int numOfThreads) {
        Phaser phaser = new Phaser(1);
        try (ExecutorService executorService = Executors.newFixedThreadPool(numOfThreads)) {
            IntStream.range(0, numOfPerson).forEach(i -> {
                phaser.register();
                executorService.execute(() -> {
                            try {
                                bank.createPerson(names.get(i), surnames.get(i), passportIDs.get(i));
                                IntStream.range(0, numOfAccounts).forEach(j -> {
                                    try {
                                        bank.createNamedAccount(String.valueOf(j), passportIDs.get(i));
                                    } catch (RemoteException e) {
                                        throw new RuntimeException(e);
                                    }
                                });
                            } catch (RemoteException e) {
                                throw new RuntimeException(e);
                            } finally {
                                phaser.arrive();
                            }
                        }
                );
            });
        }
        phaser.arriveAndAwaitAdvance();
        IntStream.range(0, numOfPerson).forEach(i -> {
            try {
                Assert.assertEquals(numOfAccounts, bank.getPerson(passportIDs.get(i), false).getAccounts().size());
            } catch (RemoteException e) {
                throw new RuntimeException(e);
            }
        });
    }

    private String generateNameForTest(int numOfTest) {
        return "name_for_test" + numOfTest;
    }

    private String generateSurnameForTest(int numOfTest) {
        return "surname_for_test" + numOfTest;
    }

    private String generatePassportIDForTest(int numOfTest) {
        StringBuilder result = new StringBuilder();
        IntStream.range(0, 5).forEach(i -> result.append(0).append(numOfTest));
        return result.toString();
    }

    @Test
    public void test01_multiThreadsCreatingTest() throws NotBoundException, RemoteException {
        createAccountsUsingMultiThread(80, 8, 20);
    }

    @Test
    public void test02_checkingRemoteAndLocal() throws RemoteException {
        String passportID = generatePassportIDForTest(3);
        String name = generateNameForTest(3);
        String surname = generateSurnameForTest(3);
        bank.createPerson(name, surname, passportID);
        Person remoteFirst = bank.getPerson(passportID, false);
        Person remoteSecond = bank.getPerson(passportID, false);
        Person localFirst = bank.getPerson(passportID, true);
        IntStream.range(0, 100).forEach(i -> {
            try {
                bank.createNamedAccount(String.valueOf(i), passportID);
            } catch (RemoteException e) {
                throw new AssertionError(e);
            }
        });
        Account firstAccount = bank.getNamedAccount(passportID + HELPFUL_PART);
        Assert.assertEquals(100, remoteSecond.getAccounts().size());
        Assert.assertEquals(100, remoteFirst.getAccounts().size());
        Assert.assertEquals(0, localFirst.getAccounts().size());
        firstAccount.setAmount(1000);
        Assert.assertEquals(1000, remoteFirst.getAccounts().get("0").getAmount());
    }

    @Test
    public void test03_validatePerson() throws RemoteException {
        String passportID = generatePassportIDForTest(4);
        assertThrows(IllegalArgumentException.class, () -> bank.createPerson("   ", "ivanov", passportID));
        assertThrows(IllegalArgumentException.class, () -> bank.createPerson("petr", "", passportID));
        assertThrows(IllegalArgumentException.class, () -> bank.createPerson("petr", "ivanov", "aboba"));
    }

    @Test
    public void test04_validateClient() throws RemoteException {
        assertThrows(IllegalArgumentException.class, () -> Client.main("name", "surname", "meowmeow", "aboba", null, null));
    }

    @Test
    public void test05_successClient() throws RemoteException {
        String passportID = generatePassportIDForTest(6);
        String name = generateNameForTest(6);
        String surname = generateSurnameForTest(6);
        bank.createPerson(name, surname, passportID);
        bank.createNamedAccount("0", passportID);
        Client.main(name, surname, passportID, "0", "1000");
        Assert.assertEquals(1000, bank.getNamedAccount(passportID + HELPFUL_PART).getAmount());
    }

    @Test
    public void test06_createAccountWithoutPerson() throws RemoteException {
        String passportID = generatePassportIDForTest(7);
        bank.createNamedAccount("0", passportID);
        Person resultPerson = bank.getPerson(passportID, false);
        Account resultAccount = bank.getNamedAccount(passportID + HELPFUL_PART);
        Assert.assertNull(resultAccount);
        Assert.assertNull(resultPerson);
    }
}
