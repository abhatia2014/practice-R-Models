getwd()
library(stringr)
files=list.files()
files
head(files)
# we use a string function to extract all files having 'practice' word

grep("learn",x=files,value=TRUE)
grep("learn",files,value=FALSE)
sum(grepl("practice",x = files))

#string functions related to regular expressions
#1. identify match to a pattern
#use grep, grepl, str_detect
str_detect(files,"practice")
str_extract(files,"learn")
#str_detect or str_extract does not extract the vectors that contain the string

str_extract_all(files,"learn",simplify = TRUE)
grep("practice",files,files, value=TRUE)
grep("practice",files,files, value=FALSE)

# clearly grep wins here over str_detect and str_extract

#locate pattern with in a string, gives the start position of matched position
#regexpr, gregexpr, str_locate, str_locate_all
regexpr("learn",files)
str_locate(files,"learn")
#most certainly, str_locate is better suited regexpr
gregexpr("learn",files)
str_locate_all(files,"learn")
#str_locate_all is definitely better than gregexpr

#replace a pattern 
#using sub, gsub,str_replace, str_replace_all

sub("learn","must_learn",files)
str_replace(files,"must_learn","learn")
#sub and str_replace are equal in functionality

gsub("practice","must_practice",files)
str_replace_all(files,"must_practice","practice")
string=c("There are some special characters in R that cannot be directly coded in a string. For example, let’s say you specify your pattern with single quotes and you want to find countries with the single quote '. You would have to “escape” the single quote in the pattern, by preceding it with ")
sub("you","GOD",string)
gsub("you","GOD",string)
str_replace(string,"GOD","you")
string
str_replace(string,"you","GOD")
str_replace_all(string=string,"you","GOD")
#clearly, sub and gsub work much better for string replacement than str_replace, and string_replace_all

#splitting a string using a pattern

#comparing strsplit or str_split
string1=string
rm(string)
strsplit(string1," ")
str_split(string1," ")

#clearly strsplit (base) is better and str_split introduces some additional characters

# winners are

# for identifying/matching a pattern- grep, grepl
# for locating the start and end digits in a pattern- str_locate
# for replacing a pattern- sub, gsub
# for splitting a string- strsplit

#regular expression syntax

# for finding patterns with single quote ' we have to use the escape character before \
# the search will be grep('\'', x)
grep('\'',string1, value=FALSE)
str_locate(string1,'\'')
regexpr('\'',string1)

str_extract_all(string1split,"you")
lapply(string1split,function(x) grep("you",x,value = TRUE))
lapply(string1split,function(x) regexpr("you",x))

#which all characters need to be escaped
#\' single quote
#\" double quote
#\n newline
#\r carriage return
#\t tab character

#example

lapply(string1split,function(x) grep('\'',x,value = TRUE))
cat("a\nb")
#prints it in two separate lines

#how to check for repititions

# * matches 0 times
# + matches atleast 1 times
# ? matches at most 1 times
# {n} matches exactly n times
# {n,} matches atleast n times
# {n,m} matches between n, m times

#examples
stringtry=c("a","ab","acb","accb","acccb","accccb")
stringtry
grep('ac+b',stringtry,value=TRUE)
grep('ac?b',stringtry,value=TRUE)
grep('ac*b',stringtry,value=TRUE)
grep('ac{2}b',stringtry,value=TRUE)
grep('ac{2,}b',stringtry,value=TRUE)
grep('ac{2,3}b',stringtry,value=TRUE)
files
#example to find pattern of t repeating more than two times in the files vector

grep('t{2,}',files,value=TRUE)


#position of pattern within a string


# ^ matches the start of the string
# $ matches the end of the string
# \b matches the empty string at the either edge of the word
# \B matches the empty string provided it's not the edge of the word

stringposition=c("abcd","cdab","cabd","c abd")

grep("^ab",stringposition,value=TRUE)
grep("^c",stringposition,value=TRUE)
grep("ab$",stringposition,value=TRUE)
grep("\\bab",stringposition,value=TRUE)
grep("\\Bab",stringposition,value=TRUE)

#find all .R files in the files

grep('.R',files,value = TRUE)
#find all files with underscore _
#changing
grep('_',files, value = TRUE)

#find all files starting with the letter p
grep('^p',files,value = TRUE,ignore.case = TRUE)
#find all files with two oo 
grep('o{2,}',files,value = TRUE)

#Remove the .R from all files, and then find the files ending with 'ng'

grep('ng$',sub(".R","",files),value = TRUE)

#find files that have R in between
grep('R',sub(".R","",files),value = TRUE)


#matching the operators

# . matches any single character 
# [...] matches any characters inside the square brackets
# [^...] matches any characters except those inside the square brackets
# \ suppresses special meaning of a meta character in regular expression like 
# $ * + ? {} [] ^ | () \ 
# since \ itself needs to be supressed, it is preceeded by a double \\

# | is an OR opearator, matches patterns on either side of |

# (...) a part of the pattern can be matched and can be retained while rest can be replaced

stringoperators=c('^ab','ab','abc','abd','abe','ab 12')

grep("ab.", stringoperators,value=TRUE)

grep("ab[c-e]", stringoperators,value=TRUE)

grep("ab[^c]", stringoperators,value=TRUE)

grep("^ab", stringoperators,value=TRUE)

grep("\\^ab", stringoperators,value=TRUE)

grep("abc|abd", stringoperators,value=TRUE)

gsub('(ab) 12',"\\1 34",stringoperators) # '\\1 means retain the first part of the match ie. within (..) and replace the other

# Character classes

# [:digit:] or \d which is equivalent to [0-9]

# \D non digits, which is equivalent to [^ 0-9]
# [:lower:] lower case letters equivalent to [a-z]
# [:upper:] upper case letters equivalent to [A-Z]

# [:alpha:] upper and lower case equivalent to [[:upper:][:lower:]]
# [:alnum:] all alphanumeric equivalent to [[:alpha:][:digit:]]
# \w word characters equivalent to [:alnum:]
# \W not word characters equivalent to [^A-z0-9_]
# [:blank:] blank characters i.e space and tab
# [:space:] space characters tab, newline, vertical tab, carriage return, space
# \s space
# \S not space
# [:punct:] punctuation characters 
# [:graph:] graphical characters [[:alnum:][:punct:]]
# [:print:] printable characters equivalent to [[:alnum:][:punct:]\\s]
# [:cntrl:] control characters

#fixed only regular expression

stringfixed=c("Axbc","A.bc")
pattern="A.b"
grep(pattern,stringfixed,value = TRUE)
grep(pattern, stringfixed,value=TRUE,fixed=TRUE)

grep('[:upper:]',files,value=TRUE)

#examples

strings <- c(
  "apple", 
  "219 733 8965", 
  "329-293-8753", 
  "Work: 579-499-7527; Home: 543.355.3679")
strings
phonepattern="([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
grepl(pattern = phonepattern,strings)
grep(phonepattern,strings,value=TRUE)
str_subset(strings,phonepattern)
str_extract(strings,phonepattern)
str_extract_all(strings,phonepattern)
str_extract_all(strings,phonepattern,simplify = TRUE)
str_match(strings,phonepattern)
