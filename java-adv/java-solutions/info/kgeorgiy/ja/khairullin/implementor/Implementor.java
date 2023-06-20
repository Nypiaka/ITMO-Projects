package info.kgeorgiy.ja.khairullin.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;
import info.kgeorgiy.java.advanced.implementor.Impler;


import javax.tools.*;
import java.io.*;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;

/**
 * This class can create realisations of interfaces and .jar versions of this realisations
 * <p>
 * Implementing {@link Impler} and {@link JarImpler}
 *
 * @author Nypiaka
 */
public class Implementor implements JarImpler {

    /**
     * String, which contains {@code System} separator.
     */
    private final String SEPARATOR = System.lineSeparator();
    /**
     * String, which whitespace.
     */
    private final String WHITESPACE = " ";
    /**
     * String, which tab.
     */
    private final String TAB = (WHITESPACE.repeat(4));
    /**
     * String, which contains left curly bracket.
     */
    private final String LEFT_CURLY_BRACKET = "{";
    /**
     * String, which contains right curly bracket.
     */
    private final String RIGHT_CURLY_BRACKET = "}";
    /**
     * String, which contains public modifier.
     */
    private final String PUBLIC = "public ";
    /**
     * String, which contains class name addition.
     */
    private final String CLASS_NAME_ADDITION = "Impl";
    /**
     * String, which contains empty string.
     */
    private final String EMPTY = "";
    /**
     * String, which contains {@code System} file separator.
     */
    private final char FILES_SEPARATOR = File.separatorChar;
    /**
     * String, which contains empty comma.
     */
    private final String COMMA = ",";

    /**
     * Implements interface in a class whose name is obtained by adding the postfix Impl to the name
     * of the interface and puts it in the root;
     *
     * @param token interface to be implemented;
     * @param root  directory where the {@link Class token} implementation will be;
     * @throws ImplerException if {@link Class token} is not an interface, or is a primitive, or is private
     *                         or {@link Path root} is not correct or problems with writing implemented methods
     *                         in the file;
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        validateToken(token);
        genCode(root, token);
    }

    /**
     * Starts the implementation or creation of a jar file. Calls the {@link #implement(Class, Path)} or
     * {@link #implementJar(Class, Path)} function depending on the args. Arguments in callable function signatures
     * are converted as needed;
     *
     * @param args Array containing {@code [token, root]} or {@code [-jar, token, root]}
     *             <p>
     *             {@code token} - interface that will be implemented;
     *             <p>
     *             {@code root} - path of root or .jar directory.
     *             in the first case of a set of arguments, {@link #implement(Class, Path)} is called,
     *             in the second - {@link #implementJar(Class, Path)};
     */
    public static void main(final String[] args) {
        if (!acceptInput(args)) {
            return;
        }
        try {
            if (args.length == 2) {
                new Implementor().implement(Class.forName(args[0]), Path.of(args[1]));
            } else {
                new Implementor().implementJar(Class.forName(args[1]), Path.of(args[2]));
            }
        } catch (ImplerException e) {
            System.out.println("problems with generating class for interface: " + args[args.length == 2 ? 0 : 1]);
        } catch (ClassNotFoundException e) {
            System.out.println("problems with input class: " + args[args.length == 2 ? 0 : 1]);
        }
    }

    //For work with code lines {

    /**
     * Tabulates a string. Returns a string preceded by the required number of tabs;
     *
     * @param line      any string;
     * @param numOfTabs an integer number of tabs to appear before the line;
     * @return {@link String} string preceded by the required number of tabs;
     */
    private String tabulate(final String line, int numOfTabs) {
        return TAB.repeat(numOfTabs) + line;
    }

    /**
     * Separates a string. Separates string by spaces from the number of sides depending on the input
     * parameters;
     *
     * @param line any string;
     * @param mode an integer number of tabs to appear before the line;
     *             if mode less than 2 separates only left side;
     *             if mode is 2 separates left and right sides;
     *             if mode more than 3 separates only right side;
     * @return {@link String} separated by spaces from the number of sides depending on the input parameters;
     */
    private String getSeparatedLine(final String line, int mode) {
        return (mode < 3 ? WHITESPACE : EMPTY) + line + (mode > 1 ? WHITESPACE : EMPTY);
    }

    /**
     * Selects a line. Wraps line in parentheses;
     *
     * @param line any string;
     * @return {@link String} wrapped in parentheses;
     */
    private String getInBrackets(final String line) {
        return "(" + line + ")";
    }

    /**
     * Code generates a string. Wraps a string in curly braces with the required number of tabs;
     *
     * @param line any string;
     * @return {@link String} wrapped in curly braces with the required number of tabs;
     */
    private String getCodeBlock(final String line) {
        return tabulate(LEFT_CURLY_BRACKET + SEPARATOR, 0) +
                line +
                tabulate(RIGHT_CURLY_BRACKET + SEPARATOR, 1);
    }

    /**
     * Code generates a string. Adds a semicolon as needed and a newline to the end of the line;
     *
     * @param line          any string;
     * @param needSemicolon is a parameter, depending on which the semicolon is added;
     *                      if needSemicolon = true semicolon will be added;
     *                      if needSemicolon = false semicolon will not be added;
     * @return {@link String} marked with a newline and a semi-column depending on the parameters;
     */
    private String getCodeLine(final String line, boolean needSemicolon) {
        return line + (needSemicolon ? ";" : EMPTY) + SEPARATOR;
    }

    /**
     * Generates a path. Replaces all dots in the path with file separator characters;
     *
     * @param currentPath is a string description of the path;
     * @return correct string path description, if currentPath is null, returns
     * empty string;
     */
    private String getStringPathWithCorrectSeparator(final String currentPath, boolean mode) {
        char charToReplace = (mode ? FILES_SEPARATOR : '/');
        return (currentPath == null ? EMPTY : currentPath.replace('.', charToReplace));
    }
    //}

    //For work with implement() input {

    /**
     * Validates class. Checks that the supplied interface is not a primitive, not private, and is an interface;
     *
     * @param token interface for validate;
     * @throws ImplerException if the token fails the tests;
     */
    private void validateToken(final Class<?> token) throws ImplerException {
        if (!token.isInterface() || Modifier.isPrivate(token.getModifiers()) || token.isPrimitive()) {
            throw new ImplerException("can't implement interface: " + token.getSimpleName());
        }
    }

    /**
     * Generates implementation. Writes to a file whose name is the name of the supplied interface with the Impl
     * postfix, an implementation of this interface with default return values;
     *
     * @param path  path to the directory where the interface implementation will be located;
     * @param token interface to be implemented
     * @throws ImplerException if there were errors while implementing the code
     */
    private void genCode(final Path path, final Class<?> token) throws ImplerException {
        try (BufferedWriter writer = Files.newBufferedWriter(createTruePath(path, token))) {
            genHead(token, writer);
            genMethods(token.getMethods(), writer);
        } catch (IOException | SecurityException e) {
            throw new ImplerException("can't write in file");
        }
    }

    /**
     * Validates an array of input parameters. Checks it against certain patterns that are allowed for the
     * {@link #implement(Class, Path)} or {@link #implementJar(Class, Path)} to work correctly;
     *
     * @param args an array with the arguments necessary for the implementor to work;
     * @return true, if args accepted tests and false in other cases;
     */
    private static boolean acceptInput(final String[] args) {
        return !(args == null || args.length == 0 || args.length == 1 || args[0] == null || args[1] == null || (args.length > 2 && (!args[0].equals("-jar")) || args[2] == null));
    }

    /**
     * Creates a path to the given implementation. If the current directory does not exist and its parent exists, it
     * also creates the current directory;
     *
     * @param path  the path where the implementation should be;
     * @param token the class to be implemented;
     * @return {@link Path} extended for class implementation;
     * @throws ImplerException if there are problems with expanding the path;
     */
    private Path createTruePath(Path path, final Class<?> token) throws ImplerException {
        try {
            path = path.resolve(getStringPathWithCorrectSeparator(token.getPackageName(), true));
            path = path.resolve(token.getSimpleName() + CLASS_NAME_ADDITION + ".java");
            if (!path.toFile().exists() && path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
        } catch (IOException e) {
            throw new ImplerException("can't create this directory: " + path);
        }
        return path;
    }

    /**
     * Returns the class description (top row);
     *
     * @param token whose top row is to be generated;
     * @return {@link String} first line of generated class;
     */
    private String getClassDescription(final Class<?> token) {
        return getCodeLine(PUBLIC + "class " + token.getSimpleName() + CLASS_NAME_ADDITION +
                getSeparatedLine("implements " + token.getCanonicalName(), 2), false);
    }

    /**
     * Returns the class package description;
     *
     * @param token whose package description is to be generated;
     * @return {@link String} package line of generated class;
     */
    private String getPackageName(final Class<?> token) {
        String currentInterfacePackage = token.getPackageName();
        return currentInterfacePackage.isEmpty() ? EMPTY :
                getCodeLine("package" + WHITESPACE + currentInterfacePackage, true);
    }

    /**
     * Generates the first two lines of the implemented class. Generates its description and package and writes it in
     * the result file;
     *
     * @param token  is interface, whose package and description will be generated and written to the final file;
     * @param writer this is the writer to the final file;
     * @throws IOException in case there are problems with writing to a file;
     */
    private void genHead(final Class<?> token, final BufferedWriter writer) throws IOException {
        writer.write(getPackageName(token) + getClassDescription(token));
    }

    /**
     * Generates methods with default value of the implemented class. Generates its description and package and writes
     * it in the result file;
     *
     * @param methods is methods from original interface, which will be implemented in such a way as to return
     *                default values;
     * @param writer  this is the writer to the final file;
     * @throws IOException in case there are problems with writing to a file;
     */
    private void genMethods(final Method[] methods, final BufferedWriter writer) throws IOException {
        writer.write(LEFT_CURLY_BRACKET + SEPARATOR);
        for (Method method : methods) {
            writer.write(getCodeLine(method.isDefault() ? EMPTY : getCodeLine(getOverride(method) +
                    getMethodDescription(method) + getMethodBody(method), false), false));
        }
        writer.write(RIGHT_CURLY_BRACKET);
    }
    //}

    //For work with methods {

    /**
     * Returns a string description of the errors thrown by the method;
     *
     * @param method whose errors description is to be generated;
     * @return {@link String} line description of the errors thrown by the method through throws, which will be
     * inserted into the final code;
     */
    private String getExceptions(final Method method) {
        StringBuilder thrownExceptions = new StringBuilder();
        Class<?>[] exceptions = method.getExceptionTypes();
        if (exceptions.length > 0) {
            thrownExceptions.append(" throws ");
        }
        for (int i = 0; i < exceptions.length; i++) {
            thrownExceptions.append(exceptions[i].getCanonicalName());
            if (i != exceptions.length - 1) {
                thrownExceptions.append(COMMA);
            }
            thrownExceptions.append(" ");
        }
        return thrownExceptions.toString();
    }

    /**
     * Returns a string description of the default class value;
     *
     * @param token whose default return value is to be generated;
     * @return {@link String} line description of the default return value of given class or primitive;
     */
    private String getReturnValue(final Class<?> token) {
        if (token.isPrimitive()) {
            if (token == boolean.class) {
                return "false";
            } else if (token == void.class) {
                return EMPTY;
            }
            return "0";
        }
        return "null";
    }

    /**
     * Returns a string description of the method signature;
     *
     * @param method is a method, a string representation of the signature of which will be generated;
     * @return {@link String} a string representation of the signature of given method;
     */
    private String getMethodSignature(final Method method) {
        StringBuilder signatureToString = new StringBuilder();
        Class<?>[] vars = method.getParameterTypes();
        for (int i = 0; i < vars.length; i++) {
            signatureToString.append(vars[i].getCanonicalName())
                    .append(" var")
                    .append(i)
                    .append(i == vars.length - 1 ? EMPTY : getSeparatedLine(COMMA, 3));
        }
        return getInBrackets(signatureToString.toString());
    }

    /**
     * Returns "@Override" or empty string for given method;
     *
     * @param method is a method, string representation of the override of which will be generated;
     * @return {@link String} a string representation of the override method;
     */
    private String getOverride(final Method method) {
        return tabulate(!Modifier.isStatic(method.getModifiers()) ?
                getCodeLine("@Override", false) : EMPTY, 1);
    }

    /**
     * Returns modifier of given method. If method is public, returns "public", else returns empty string;
     *
     * @param method is a method, a string representation of the override of which will be generated;
     * @return {@link String} a string representation of the override method;
     */
    private String getMethodModifier(final Method method) {
        return (Modifier.isPublic(method.getModifiers()) ? PUBLIC : EMPTY);
    }

    /**
     * Returns the method description (top row);
     *
     * @param method whose top row is to be generated;
     * @return {@link String} first line of generated method;
     */
    private String getMethodDescription(final Method method) {
        return tabulate(getMethodModifier(method) + method.getReturnType().getCanonicalName() +
                getSeparatedLine(method.getName() + getMethodSignature(method) + getExceptions(method), 1), 1);
    }

    /**
     * Returns a string description of the method return statement;
     *
     * @param method whose string description of the method return statement is to be generated;
     * @return {@link String} string description of the method return statement;
     */
    private String getMethodBody(final Method method) {
        return getCodeBlock(getCodeLine(tabulate("return" +
                getSeparatedLine(getReturnValue(method.getReturnType()), 1), 2), true));

    }

    //}

    //For work with jar generate {

    /**
     * Generates a .jar file at the given address from the implementation of the given interface;
     *
     * @param token   interface to be implemented;
     * @param jarFile directory where the token implementation, generated to .jar will be;
     * @throws ImplerException if token is not an interface, or is a primitive, or is private or jarFile is not
     *                         correct or problems with generating .jar file from implemented class in the file;
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        try {
            final Path tmp = Files.createTempDirectory(jarFile.toAbsolutePath().getParent(), "jarGeneration_");
            implement(token, jarFile.toAbsolutePath().getParent());
            genClass(token, jarFile.toAbsolutePath().getParent(), createTruePath(jarFile.toAbsolutePath().getParent(), token));
            genJar(token, jarFile.toAbsolutePath().getParent(), jarFile);
            tmp.toFile().deleteOnExit();
        } catch (IOException | SecurityException e) {
            throw new ImplerException("can't create temp dir");
        }
    }

    /**
     * Generates a .jar file from the given path;
     *
     * @param token   interface, the .jar file from the implementation of which will be generated;
     * @param tmpPath directory with all the temporary files we need to generate the .jar file;
     * @param jarFile the full name of the .jar file in which to generate the implementation of the interface;
     * @throws ImplerException if an {@link IOException} was caught in the case of working with files
     */
    private void genJar(final Class<?> token, final Path tmpPath, final Path jarFile) throws ImplerException {
        try (JarOutputStream jarOutputStream = new JarOutputStream(Files.newOutputStream(jarFile))) {
            String realName = getStringPathWithCorrectSeparator(token.getPackageName(), false) +
                    (token.getPackageName().isEmpty() ? EMPTY : '/') +
                    token.getSimpleName() + CLASS_NAME_ADDITION + ".class";
            jarOutputStream.putNextEntry(new ZipEntry(realName));
            Files.copy(tmpPath.resolve(realName), jarOutputStream);
        } catch (IOException e) {
            throw new ImplerException("problems with .jar generating");
        }
    }

    /**
     * Returns source-dir of token;
     *
     * @param token the class whose sources you want to get;
     * @return string representation of token's sources;
     */
    private Path getSource(final Class<?> token) throws ImplerException {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI());
        } catch (URISyntaxException e) {
            throw new ImplerException("can't generate uri: " + token);
        }
    }

    /**
     * Compiles a class that is an interface implementation;
     *
     * @param token    initial interface
     * @param temp     current temporary directory
     * @param truePath path to the given interface
     * @throws ImplerException if there were compilation errors (exit code of compilation != 0) or no JVM
     */
    private void genClass(final Class<?> token, final Path temp, final Path truePath) throws ImplerException {
        JavaCompiler java = ToolProvider.getSystemJavaCompiler();
        if (java == null) {
            throw new ImplerException("no java found  on machine");
        }
        String source = (temp.resolve(getSource(token)).toString());
        int result = ToolProvider.getSystemJavaCompiler().run(null, null, null, "-encoding", "UTF-8",
                "-classpath", source, truePath.toString());
        if (result != 0) {
            throw new ImplerException("compiler failed with code: " + result);
        }
    }
}
