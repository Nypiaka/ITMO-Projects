package info.kgeorgiy.ja.khairullin.bank;

import java.rmi.RemoteException;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public abstract class AbstractPerson implements Person {
    private final String name;
    private final String surname;
    private final String passportID;
    private final int ID_LENGTH = 10;

    protected Map<String, Account> accounts = new ConcurrentHashMap<>();

    private void validateNameAndSurname(String name) {
        if (name.isEmpty() || name.isBlank()) {
            throw new IllegalArgumentException("Name shouldn't be empty or blank");
        }
    }

    private void validateID(String passportID) {
        if (passportID.length() != ID_LENGTH || passportID.chars().anyMatch(ch -> !Character.isDigit((char) ch))) {
            throw new IllegalArgumentException("Passport ID should be sequence of digits with length");
        }
    }

    public AbstractPerson(String name, String surname, String passportID) {
        validateNameAndSurname(name);
        validateNameAndSurname(surname);
        validateID(passportID);
        this.name = name;
        this.surname = surname;
        this.passportID = passportID;
    }

    @Override
    public String getName() {
        return this.name;
    }

    @Override
    public String getSurname() {
        return this.surname;
    }

    @Override
    public String getPassportID() {
        return this.passportID;
    }

    @Override
    public synchronized Map<String, Account> getAccounts() {
        return Collections.unmodifiableMap(this.accounts);
    }

    protected void copyAccounts(Person person) {
        person.getAccounts().forEach((id, account) -> {
            try {
                accounts.put(id, account.copyAccount());
            } catch (RemoteException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
