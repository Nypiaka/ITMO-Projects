javac -classpath ../../java-advanced-2023/artifacts/info.kgeorgiy.java.advanced.implementor.jar ../java-solutions/info/kgeorgiy/ja/khairullin/implementor/Implementor.java
cd ../java-solutions || exit
jar cfm ../scripts/Implementor.jar ../scripts/MANIFEST.MF info/kgeorgiy/ja/khairullin/implementor/*.class
rm ../java-solutions/info/kgeorgiy/ja/khairullin/implementor/Implementor.class