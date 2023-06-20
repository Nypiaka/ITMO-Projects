package info.kgeorgiy.ja.khairullin.bank;

import java.rmi.Remote;
import java.util.Map;

public class RemotePerson extends AbstractPerson implements Remote {
    public RemotePerson(String name, String surname, String passportID, Map<String, Account> accountMap) {
        super(name, surname, passportID);
        this.accounts = accountMap;
    }
}
