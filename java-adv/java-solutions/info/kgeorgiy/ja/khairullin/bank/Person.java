package info.kgeorgiy.ja.khairullin.bank;

import java.io.Serializable;
import java.util.Map;

public interface Person extends Serializable {
    String getPassportID();

    String getName();

    String getSurname();

    Map<String, Account> getAccounts();
}
