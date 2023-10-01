peek is very simple tool to facilitate working with logs, it is very basic but still useful in analyzing especially long logs.

usage:
1. Copy log you want to analyze to file log.txt

2. Issue ./peek

3. After program starts you will be prompted to enter filter by line "provide your filters:" you have couple of options:

3.1 Leave empty and press enter
This will list log file as is without substantial alteration. whatever result you see in console will be also stored into file out.txt. This will be done no matter if you enter filter or not.

3.2 provide filter
3.2.1 Inclusive and exclusive filters, they will be explained in example.

Let say log.txt contains following:

aaaaaa
bbbb
cccccc
dddddd

Inclusive filters:
If one issues filter "a ddd" this will preserve the lines which contains those strings (As mentioned the result will be stored also in out.txt).

example:
provide your filters: 
a ddd


aaaaaa
dddddd



Exclusive filters:
If one issues the same filter but with tokens prefixed with @ the filter became exclusive, so "@a @ddd" will exclude the lines which contains those strings.

example:
provide your filters: 
@a @ddd


bbbb
cccccc



Filters could be combined.

example:
provide your filters: 
aaa bbb ccc @c


aaaaaa
bbbb


3.2.2 Region filter
It is possible to restrict displayed content using region filter, it is useful if one wants to focus on certain sections only.
This stage of processing is the last one after  3.2.1 filters are applied. It will be ilustrated by example:

let say there is log.txt:

aaaaaa
bbbb
ffff
cccccc
dddddd
aaaaaa
bbbb
cccccc
dddddd
fffff
aaaaaa
bbbb
cccccc
dddddd

There is of special interest to inspect neighborhood of line consisting "f" sign, then following is possible

example:

provide your filters:
f^2



bbbb
ffff
cccccc
dddddd
----------------------------

dddddd
fffff
aaaaaa
bbbb
----------------------------

Where:
f  <- string to match
^  <- special sign
2  <- how many lines to preserve before (2 lines - this includes matching line itself) and after (2 lines)
As one can see only relevant sections are displyed.
It is possible to combine also those filters.



