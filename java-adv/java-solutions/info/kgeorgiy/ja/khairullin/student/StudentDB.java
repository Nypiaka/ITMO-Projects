package info.kgeorgiy.ja.khairullin.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class StudentDB implements StudentQuery {
    private final Comparator<? super Student> COMPARING_BY_NAME = Comparator
            .comparing(Student::getLastName)
            .thenComparing(Student::getFirstName)
            .reversed()
            .thenComparing(Student::getId);
    private final Comparator<? super Student> COMPARING_BY_FIRST_NAME = Comparator
            .comparing(Student::getFirstName)
            .thenComparing(Student::getId);

    private <E> List<E> getUniversal(List<Student> students, Function<? super Student, E> mapper) {
        return students.stream().map(mapper).toList();

    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return getUniversal(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return getUniversal(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return getUniversal(students, Student::getGroup);

    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return getUniversal(students, student -> String.join(" ", student.getFirstName(), student.getLastName()));
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return students.stream().sorted(COMPARING_BY_FIRST_NAME).map(Student::getFirstName).collect(Collectors.toSet());
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream().max(Student::compareTo).map(Student::getFirstName).orElse("");
    }

    private List<Student> sortStudentsUniversal(Collection<Student> students, Comparator<? super Student> comparator) {
        return students.stream().sorted(comparator).collect(Collectors.toList());
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortStudentsUniversal(students, Student::compareTo);
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortStudentsUniversal(students, COMPARING_BY_NAME);
    }

    private List<Student> findStudentsUniversal(Collection<Student> students, Predicate<? super Student> predicate) {
        return students.stream().filter(predicate).sorted(COMPARING_BY_NAME).collect(Collectors.toList());
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findStudentsUniversal(students, student -> student.getFirstName().equals(name));
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudentsUniversal(students, student -> student.getLastName().equals(name));
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findStudentsUniversal(students, student -> student.getGroup().equals(group));
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return students.stream().filter(student -> student.getGroup().equals(group)).
                collect(Collectors.toMap(Student::getLastName, Student::getFirstName, (ex, cur) -> ex.compareTo(cur) < 0 ? ex : cur));
    }
}