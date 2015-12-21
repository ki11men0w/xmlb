# xmlb
###XML beautifier.
Indent nested tags in XML making it human readable.
You can also perform the opposite effect: to remove all non-essential whitespace from XML.

```
xmlb [OPTIONS] [XMLFILE1 [XMLFILE2 ...]]

Common flags:
  -b    --backup              Backup original files
  -o -e --encoding=ENC        Encoding for OUTPUT documents (default is
                              encoding of an input document)
  -i    --input-encoding=ENC  Encoding for INPUT documents. If not specified
                              then the content of an input XML-document will be
                              used to realize its encoding. If all failed then
                              UTF-8 will be used
        --spaces[=INT]        Use this number of spaces instead of tabs for
                              identation (default is 3)
        --strip               Strip all insignificant spaces instead of
                              making XML human readable
        --legacy              Legacy mode
  -q    --quiet               Be quiet. Do not print warnings
  -?    --help                Display help message
  -V    --version             Print version information

Format xml file(s). If input data is/are common file(s) and no redirection of
STDOUT is specified than input files will be changed inplace. If STDIN is not a
terminal (e.g. because of redirection) then result will be printed to STDOUT.
usage: xmlb OPTIONS XMLFILE1 [XMLFILE2 ...]
       xmlb OPTIONS < somefile.xml > somefile.xml
```
