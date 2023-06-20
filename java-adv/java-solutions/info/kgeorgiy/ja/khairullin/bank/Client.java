package info.kgeorgiy.ja.khairullin.bank;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.IntStream;

public final class Client {
    /**
     * Utility class.
     */
    private Client() {
    }

    public static void main(final String... args) throws RemoteException {
        final Bank bank;
        try {
            bank = (Bank) LocateRegistry.getRegistry(8888).lookup("//localhost/bank");
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
            return;
        }
        validateArgs(args);
        String name = args[0];
        String surname = args[1];
        String passportID = args[2];
        String accountNumber = args[3];
        int accountDelta = Integer.parseInt(args[4]);
        Person person = bank.getPerson(passportID, false);
        person = person == null ? bank.createPerson(name, surname, passportID) : person;
        Account account = bank.getNamedAccount(passportID + ":" + accountNumber);
        account = account == null ? bank.createNamedAccount(accountNumber, passportID) : account;
        account.setAmount(account.getAmount() + accountDelta);
        System.out.println(account.getAmount());
    }

    private static void validateArgs(String[] args) {
        if (args == null || args.length != 5 || Arrays.stream(args).anyMatch(Objects::isNull) ||
                IntStream.of(2, 3, 4).anyMatch(i -> args[i].chars().anyMatch(elem -> !Character.isDigit((char) elem))))
            throw new IllegalArgumentException("Args should be in format: " +
                    "Name (string), Surname (string), Passport id (digit sequence), Account number (digit sequence), amount delta (int)");
    }
}
