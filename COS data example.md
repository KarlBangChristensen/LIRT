## COS data example

read data using

```
filename cos URL 'https://raw.githubusercontent.com/KarlBangChristensen/LIRT/master/cosdata.csv';

PROC IMPORT OUT= WORK.sleep1_2 
            DATAFILE= cos
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
```

include macros

```
%let url=https://raw.githubusercontent.com/KarlBangChristensen/LIRT/master;

filename all url "&url/LIRT_include.sas"; 
%include all;
```
