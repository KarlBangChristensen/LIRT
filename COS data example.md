COS data example


```
filename cos URL 'https://raw.githubusercontent.com/KarlBangChristensen/LIRT/master/cosdata.csv';

PROC IMPORT OUT= WORK.sleep1_2 
            DATAFILE= cos
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
```
