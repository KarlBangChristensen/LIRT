/*************************************************************************************

Macro simulating from the two-dimensional dichotomous Rasch model where item parameters are drawn from a uniform distribution and 
person parameters from a normal distribution. It is possible to include item specific response dependence drawn from a 
normal distribution. 
Marginal maximum likelihood estimation is used for estimation of item parameters and Warms Weighted likelihood estimation
for estimation of person parameters. 

NAMES : data set with columns name1 $, name2 $, score, ipar (not threshold!), disc (1 => Rasch item),
			 ld_item $ (only for time 2 items. Time 1 item on which the time 2 item depend), 
			 ld_group (the response group for which item parameter applies)

			 Example: 

			NAME1        NAME2       SCORE  IPAR   DISC  LD_GROUP  LD_ITEM

			item1_1      item2_1       1      -2     2     .          
 
			item1_1      item2_1       2      -2.5   2     .         
	
			item1_2                    1      -1     1 	   .

			             item2_2       1      -0.5   1     .

			item1_3                    1      1      1 	   .

					     item2_3_0     1      1.5    1     0 	   item1_3  
			
						 item2_3_1     1      0.5    1     1	   item1_3  

	
(item1_1 / item2_1 2PL items with no item drift, 
 item1_2 / item2_2 1PL items with item drift, 
 item1_3 / item2_3_0 item2_3_1 1PL items with local dependence across time points)

NDATA: number of datasets generated

NPERSONS : number of persons ( Default=0 -> a dataset with if the PPAR option is used)

PDATA : if NPERSONS=0 then PDATA should contain the individual theta estimates. More specifically the variables 

		(dim=1)

		THETA
		ID
		
		(dim=2)
	
		THETA1
		THETA2
		ID
		
		if NPERSONS^=0 then PDATA should be a data set with population distribution as outputted from %LIRT_MML. Data set with columns 
 
		PARAMETER (dim=1: mu sigma, dim=2: mu1 mu2 sigma1 sigma2 rho) 
		ESTIMATE (the estimate)

OUT  : prefix for output data set with simulated responses (and other variables)

OBS! If the item scores in &NAMES goes from 1 to some MAX it is assumed that the items are scored from 1,...,MAX 
and that the ipar and disc corresponding to a score of 0 is 0 and 1	resp. 
This corresponds to using the output &OUT_NAMES from %LIRT_MML as input to 

If the item scores goes from 0 to MAX then it is assumed that all the scores are given in the data set. 

/*************************************************************************************/


%macro LIRT_SIMU(	NAMES,
			DIM,
			NDATA, 
			NPERSONS,
			PDATA, 
			OUT,
			DELETE=Y);

options nomprint nonotes;
ods exclude all;

/*********** START OF 1-DIMENSIONAL CASE ***********/

%if &dim=1 %then %do;

%if &npersons.^=0 %then %do;
/* Simulate thetas given the parameters in PDATA */

options mprint;

proc sql noprint;
select estimate
into :pmean
from &pdata.
where parameter='mu';
quit;
%put pmean er &pmean.;
%let pmean=&pmean.;
%put pmean er &pmean.;

proc sql noprint;
select estimate**2
into :pvar
from &pdata.
where parameter='sigma';
quit;
%put pvar er &pvar.;
%let pvar=&pvar.;
%put pvar er &pvar.;

options nomprint;

%end;

proc sql noprint;
select count(distinct(name))
into :_nitems
from &names.;
quit;

%let _nitems=&_nitems.;

proc sql noprint;
select distinct(name), max(score)
into :i1-:i&_nitems., :max1-:max&_nitems.
from &names.
group by name;
quit;

%do s=1 %to &ndata.;

%if &npersons^=0 %then %do;
* simulate correlated thetas;						
data _theta; 
do id=1 to &npersons; 
x=rannor(0);
theta=&pmean.+sqrt(&pvar.)*x; 
join=1;
output;
end; 
run;
%end;
%else %do; 
* use thetas from PDATA;
data _theta; 
set &pdata.; 
join=1;
run;
%end;

/* Time 1 */

data _names;
set &names;
join=1;
run;

proc sql;
create table _items1 as select a.*,
b.id,
b.theta
/* Dette datasæt eksisterer ikke! */
from _names a left join _theta b 
on a.join=b.join;
quit;

proc sql;
create table _prob1 as select *,
exp(disc*(theta*score+ipar))/(1+sum(exp(disc*(theta*score+ipar)))) as prob
from _items1
group by name, id;
quit;

proc sort data=_prob1;
by id name;
run;

proc transpose data=_prob1 out=_prob1_t;
by id name theta;
id score;
var prob;
run;

data _resp1;
set _prob1_t;
%do i=1 %to &_nitems.;
if name="&&i&i" then do;
_0=1 %do h=1 %to &&max&i; -_&h. %end;;
resp=rand('table' %do score=0 %to &&max&i; ,_&score. %end;)-1;
end;
%end;
run;

/* Data set with simulated responses */	

proc transpose data=_resp1 out=&out._simu&s. (drop=_name_);
by id theta;
id name;
var resp;
run;

/* Data set with item_names that can be used as input for %LIRT_MML (true model as far as discrimination goes) */

/* Create variable 'max' from scores for &out._names */

proc sql;
create table _names as select *,
max(score) as max
from &names.
group by name;
quit;

proc sql;
create table &out._names as select 
distinct(name),
max, 
case disc when 1 then 'N' else 'Y' end as disc_yn
from _names;
quit; 

%end;
%end;

/*********** END OF 1-DIMENSIONAL CASE ***********/


/*********** START OF 2-DIMENSIONAL CASE ***********/

%else %if &dim=2 %then %do;
%if &npersons.^=0 %then %do;

proc sql noprint;
select estimate
into :pmean1
from &pdata.
where parameter='mu1';
quit;

proc sql noprint;
select estimate
into :pmean2
from &pdata.
where parameter='mu2';
quit;

proc sql noprint;
select estimate**2
into :pvar1
from &pdata.
where parameter='sigma1';
quit;

proc sql noprint;
select estimate**2
into :pvar2
from &pdata.
where parameter='sigma2';
quit;

proc sql noprint;
select estimate
into :rho
from &pdata.
where parameter='rho';
quit;

%end;

/* Items time 1 (anc and unanc, no LD) */

data _items1 _items2;
set &names. (where=(ld_group=.));
join=1;
if name1^='' then output _items1;
if name2^='' then output _items2;
run;

proc sql noprint;
select count(distinct(name1))
into :_nitems1
from _items1
where name1^='';
quit;

%let _nitems1=&_nitems1.;

proc sql noprint;
select distinct(name1), max(score)
into :i1_1-:i1_&_nitems1., :max1_1-:max1_&_nitems1.
from _items1
group by name1;
quit;

/* Items time 2 (no LD)*/

proc sql noprint;
select count(*)
into :_nobs2
from _items2;
quit;

%if &_nobs2.^=0 %then %do;

proc sql noprint;
select count(distinct(name2))
into :_nitems2
from _items2;
quit;

%let _nitems2=&_nitems2.;

proc sql noprint;
select distinct(name2), max(score)
into :i2_1-:i2_&_nitems2., :max2_1-:max2_&_nitems2.
from _items2
group by name2;
quit;

%end;

data _ld_items;
set &names. (where=(ld_group^=.));
join=1;
run;

proc sql noprint;
select count(*)
into :_nobs_ld
from _ld_items;
quit;

%if &_nobs_ld.^=0 %then %do;

proc sql noprint;
select count(distinct(name2))
into :_nitems2_ld
from _ld_items;
quit;

%let _nitems2_ld=&_nitems2_ld.;

proc sql noprint; 
select distinct(name2), max(score), ld_item, ld_group 
into :i2_ld_1-:i2_ld_&_nitems2_ld., 
:max2_ld_1-:max2_ld_&_nitems2_ld., 
:i1_ld_1-:i1_ld_&_nitems2_ld., 
:ld_group1-:ld_group&_nitems2_ld.
from _ld_items;
quit;

%end;
%do s=1 %to &ndata.;

%if &npersons^=0 %then %do;

/* simulate correlated thetas */

data _theta; 
do id=1 to &npersons; 
/* Simulate from two independent normal distributions */
x1=rannor(0);
x2=rannor(0);
/* Construct theta1 and theta2 with the desired distributions */
theta1=&pmean1.+sqrt(&pvar1.)*x1; 
theta2=&pmean2.+&rho.*sqrt(&pvar2.)*x1+sqrt(1-&rho.**2)*sqrt(&pvar2.)*x2; 
join=1;
output;
end; 
run;

%end;
%else %do; 
/* Use thetas from PDATA */

data _theta; 
set &pdata.; 
join=1;
run;

%end;

/* Calculate response probabilities for items with no LD */

/* Time 1 */

proc sql;
create table _items11 as select a.*,
b.id,
b.theta1
from _items1 a left join _theta b 
on a.join=b.join;
quit;

proc sql;
create table _prob1 as select *,
exp(disc*(theta1*score+ipar))/(1+sum(exp(disc*(theta1*score+ipar)))) as prob
from _items11
group by name1, id;
quit;

proc sort data=_prob1;
by id name1;
run;

proc transpose data=_prob1 out=_prob1_t;
by id name1 theta1;
id score;
var prob;
run;

/* Time 2 */

%if &_nobs2.^=0 %then %do;

proc sql;
create table _items22 as select a.*,
b.theta2,
b.id
from _items2 a left join _theta b 
on a.join=b.join;
quit;

proc sql;
create table _prob2 as select *,
exp(disc*(theta2*score+ipar))/(1+sum(exp(disc*(theta2*score+ipar)))) as prob
from _items22
group by name2, id;
quit;

proc sort data=_prob2;
by id name2;
run;

proc transpose data=_prob2 out=_prob2_t;
by id name2 theta2;
id score;
var prob;
run;

%end;

/* Simulate responses for items without LD */

/* Time 1 */

data _resp1;   
set _prob1_t;
%do i=1 %to &_nitems1.;
if name1="&&i1_&i" then do;
_0=1 %do h=1 %to &&max1_&i; -_&h. %end;;
resp=rand('table' %do score=0 %to &&max1_&i; ,_&score. %end;)-1; 				
end;
%end;
run;

proc transpose data=_resp1 out=_resp1_t (drop=_name_);
by id theta1;
id name1;
var resp;
run;

/* Time 2, no LD */

%if &_nobs2.^=0 %then %do;

data _resp2;
set _prob2_t;
%do i=1 %to &_nitems2.;
if name2="&&i2_&i" then do;
	_0=1 %do h=1 %to &&max2_&i; -_&h. %end;;
	resp=rand('table' %do score=0 %to &&max2_&i; ,_&score. %end;)-1;
end;
%end;
run;

proc transpose data=_resp2 out=_resp2_t (drop=_name_);
by id theta2;
id name2;
var resp;
run;

proc sql;
create table _responses as select a.*
%do i=1 %to &_nitems2.; , b.&&i2_&i %end; , theta2
from _resp1_t a left join _resp2_t b
on a.id=b.id;
quit; 

%end;
%if &_nobs_ld.^=0 %then %do;

/* Calculate response probabilities for items LD */

proc sql;
create table _ld_items1 as select a.*,
b.id,
b.theta2
from _ld_items a left join _theta b 
on a.join=b.join;
quit;

proc sql;
create table _prob_ld as select *,
exp(disc*(theta2*score+ipar))/(1+sum(exp(disc*(theta2*score+ipar)))) as prob
from _ld_items1
group by name2, id;
quit;

proc sort data=_prob_ld;
by id name2;
run;

proc transpose data=_prob_ld out=_prob_ld_t;
by id name2 theta2;
id score;
var prob;
run;

/* Simulate responses for items with LD */

data _resp_ld;
set _prob_ld_t;
%do i=1 %to &_nitems2_ld.;
if name2="&&i2_ld_&i" then do;
	_0=1 %do h=1 %to &&max2_ld_&i; -_&h. %end;;
	resp=rand('table' %do score=0 %to &&max2_ld_&i; ,_&score. %end;)-1;
end;
%end;
run;

proc transpose data=_resp_ld out=_resp_ld_t (drop=_name_);
by id theta2;
id name2;
var resp;
run; 

%end;
%if &_nobs2.^=0 %then %do;
%if &_nobs_ld.^=0 %then %do;

proc sql;
create table _responses_all as select a.*
%do i=1 %to &_nitems2_ld.; , b.&&i2_ld_&i %end;
from _responses a left join _resp_ld_t b
on a.id=b.id;
quit; 

%end;
/* Hvis der kun er tid 2 items uden LD */
/* Her ekisterer datasættet _resp_ld_t ikke (så tjek om der overhovedet er noget at merge på), kun _responses */
/* Skal _responses bare set'es?? */
%else %do;

data _responses_all;
set _responses;
run;

%end;
%end;
/* Hvis der kun er tid 2 items med LD */
%else %do;

proc sql;
create table _responses_all as select a.*, b.theta2
%do i=1 %to &_nitems2_ld.; , b.&&i2_ld_&i %end;
from _resp1_t a left join _resp_ld_t b
on a.id=b.id;
quit; 

%end;

/* Data set with simulated responses */

data _responses_all2;
set _responses_all;
%if &_nobs_ld.^=0 %then %do;
%do i=1 %to &_nitems2_ld.;
if &&i1_ld_&i.^=&&ld_group&i then &&i2_ld_&i=.;
%end;
%end;
run;

proc sql;
create table &out._simu&s. as select
id,
theta1,
theta2
%do _k=1 %to &_nitems1.; , &&i1_&_k %end; 
%if &_nobs2^=0 %then %do;
%do _k=1 %to &_nitems2.; , &&i2_&_k %end; 
%end;
%if &_nobs_ld^=0 %then %do;
%do _k=1 %to &_nitems2_ld.; , &&i2_ld_&_k %end; 
%end;
from _responses_all2;
quit;

%end;

* Data set with item_names that can be used as input for the macro %LIRT_MML.sas;
data _names1;
set &names.;
item=strip(name1)||strip(name2);
run;

/* Create variable 'max' from scores for &out._names */

/*
proc sql;
create table _names2 as select *,
max(score) as max
from _names1
group by item;
quit;
*/

proc sql;
create table &out._names (drop=item) as select 
distinct(item), 
name1, 
name2, 
max, 
ld_group,
ld_item,
case disc when 1 then 'N' else 'Y' end as disc_yn
from _names1/*2*/;
quit; 

%end;

/***** END OF 2-DIMENSIONAL CASE *****/	

%if %upcase(&delete.)=Y %then %do;

ods output Datasets.Members=_datasets;

proc datasets nodetails;
run; 
quit;

proc sql noprint;
select count(distinct(name))
into :_ndata
from _datasets
where substr(strip(name),1,1)='_';
quit;

%let _ndata=&_ndata.;

proc sql noprint;
select name
into :_data1-:_data&_ndata.
from _datasets
where substr(name,1,1)='_';
quit;
proc datasets nodetails; 
delete %do k=1 %to &_ndata.; &&_data&k %end; _datasets;
run;
quit;
%end;
options notes; 
ods exclude none;
%mend LIRT_SIMU;
