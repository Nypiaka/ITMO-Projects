package info.kgeorgiy.ja.khairullin.bank;

public class LocalPerson extends AbstractPerson {

    public LocalPerson(Person person) {
        super(person.getName(), person.getSurname(), person.getPassportID());
        this.copyAccounts(person);
    }

}
