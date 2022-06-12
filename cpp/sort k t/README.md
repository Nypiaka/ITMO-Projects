# sort command line utility

sort -- sort or merge records (lines) of text and binary files

The sort utility sorts text and binary files by lines.  A line is a record separated from the subsequent record by a newline.
A record can contain any printable or unprintable characters.  Comparisons are based on one or more sort
keys extracted from each line of input, and are performed lexicographically, according to the default (C) locale's collating rules and the
specified command-line options that can tune the actual sorting behavior.  By default, if keys are not given, sort uses entire lines for
comparison.

If no input file is specified or `-` is given instead of a file name, lines are read from standard input.

```bash
sort [OPTIONS] [FILE]
```

options:
* `-k, --key=field1[,field2]` - sort via a key; define a restricted sort key that has the starting position field1, and optional ending position field2 of a key field.
* `-t, --field-separator=SEP` - use SEP instead of non-blank to blank transition

### Example
```bash
$ cat e.txt
Yuri		Gagarin		1934-1968
Gherman		Titov		1935-2000
Valentina	Tereshkova	1937
Vladimir	Komarov		1927-1967
$ sort e.txt -k 2 # Sort by second column: by second name
Yuri		Gagarin		1934-1968
Vladimir	Komarov		1927-1967
Valentina	Tereshkova	1937
Gherman		Titov		1935-2000
$ sort e.txt -k 2 -t - # Sort by second column, but column separator is "-".
Valentina	Tereshkova	1937
Vladimir	Komarov		1927-1967
Yuri		Gagarin		1934-1968
Gherman		Titov		1935-2000
```
