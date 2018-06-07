/************************************************************************************************ 
SAS macro that can create a data set with item names to use for %LIRT_MML.sas. Call the macro
using

%LIRT_NAMES_MML(DATA, OUTDATA, ANC1, ANC2, UNANC1=none, UNANC2=none, BIRN=none);

where

DATA: 	data set with items (necessary in order to find the maximum value)
OUTDATA:the resulting data set with item names
ANC1: 	anchor items at time 1 separated by blanks (default all)
ANC2: 	anchor items at time 2 separated by blanks (default all)
UNANC1: unanchored items at time 1 separated by blanks (default none)
UNANC2: unanchored items at time 2 separated by blanks (default none)
BIRN: 	if rasch then the variable DISC_YN='N', if ^=rasch (e.g. birn) then DISC_YN='Y' in the 
	output data set.

at least one item should be anchored. The lists ANC1 and ANC2 should have the same length. It 
is assumed that the first item in the sequences ANC1 and ANC2 have the same item parameters and 
so on: If "ANC1=v1 v2 item23" and "ANC=v1_2 item4 Y" then item parameters for v1 and v1_2 are 
restricted to be equal, item parameters for v2 and item4 are restricted to be equal, and item 
parameters for item23 and Y are restricted to be.

************************************************************************************************/

%macro LIRT_NAMES_MML(	DATA, 
			OUTDATA, 
			ANC1, 
			ANC2, 
			UNANC1=none, 
			UNANC2=none, 
			BIRN=none);

options nomprint nonotes;
ods exclude all;

/* Save anchored items as macro variables */

%let _anc1_1=%scan(&anc1.,1,' ');
%let _anc2_1=%scan(&anc2.,1,' ');
%let _nanc=1;

%if %scan(&anc1.,1,' ')^=%scan(&anc1.,-1,' ') %then %do;

	%let _i=1;
	%do %until (%scan(&anc1.,%eval(&_i.),' ')=%scan(&anc1.,-1,' '));
		%let _anc1_%eval(&_i.+1)=%scan(&anc1.,%eval(&_i.+1),' ');
		%let _anc2_%eval(&_i.+1)=%scan(&anc2.,%eval(&_i.+1),' ');
		%let _i=%eval(&_i.+1);
	%end;
	%let _nanc=%eval(&_i.);

%end;

/* Save maximum response for each of the items */

proc sql noprint;
select max(&_anc1_1) %do _i=2 %to &_nanc.; , max(&&_anc1_&_i) %end;
into :_max1	%do _i=2 %to &_nanc.; , :_max&_i. %end;
from &data.;
quit;
	
%if %upcase(&unanc1.)^=NONE %then %do;

	/* Save unanchored items as macro variables */
	
	%let _unanc1_1=%scan(&unanc1.,1,' ');
	%let _nunanc1=1;

	%if %scan(&unanc1.,1,' ')^=%scan(&unanc1.,-1,' ') %then %do;
		%let _i=1;
		%do %until (%scan(&unanc1.,%eval(&_i.),' ')=%scan(&unanc1.,-1,' '));
			%let _unanc1_%eval(&_i.+1)=%scan(&unanc1.,%eval(&_i.+1),' ');
			%let _i=%eval(&_i.+1);
		%end;
		%let _nunanc1=%eval(&_i.);
	%end;

	/* Save maximum response for each of the items */

	proc sql noprint;
	select max(&_unanc1_1) %do _i=2 %to &_nunanc1.; , max(&&_unanc1_&_i) %end;
	into :_max1_1 %do _i=2 %to &_nunanc1.; , :_max1_&_i. %end;
	from &data.;
	quit;	

%end;
%if %upcase(&unanc2.)^=NONE %then %do;

	/* Save unanchored items as macro variables */

	%let _unanc2_1=%scan(&unanc2.,1,' ');
	%let _nunanc2=1;

	%if %scan(&unanc2.,1,' ')^=%scan(&unanc2.,-1,' ') %then %do;
		%let _i=1;
		%do %until (%scan(&unanc2.,%eval(&_i.),' ')=%scan(&unanc2.,-1,' '));
			%let _unanc2_%eval(&_i.+1)=%scan(&unanc2.,%eval(&_i.+1),' ');
			%let _i=%eval(&_i.+1);
		%end;
		%let _nunanc2=%eval(&_i.);
	%end;

	/* Save maximum response for each of the items */

	proc sql noprint;
	select max(&_unanc2_1) %do _i=2 %to &_nunanc2.; , max(&&_unanc2_&_i) %end;
	into :_max2_1 %do _i=2 %to &_nunanc2.; , :_max2_&_i. %end;
	from &data.;
	quit;
	
%end;
%if %upcase(&birn.)^=NONE %then %do;

	/* Save unanchored items as macro variables */

	%let _birn1=%scan(&birn.,1,' ');
	%let _nbirn=1;

	%if %scan(&birn.,1,' ')^=%scan(&birn.,-1,' ') %then %do;

		%let _i=1;
		%do %until (%scan(&birn.,%eval(&_i.),' ')=%scan(&birn.,-1,' '));
			%let _birn%eval(&_i.+1)=%scan(&birn.,%eval(&_i.+1),' ');
			%let _i=%eval(&_i.+1);
		%end;
		%let _nbirn=%eval(&_i.);

	%end;
%end;

data &outdata.;
format name1 name2 $20. max 1. disc_yn $1.;
%do _i=1 %to &_nanc.;
	name1="&&_anc1_&_i";
	name2="&&_anc2_&_i";
	max=&&_max&_i;
	output;
%end;
%if %upcase(&unanc1.)^=NONE %then %do;
	%do _i=1 %to &_nunanc1.;
		name1="&&_unanc1_&_i";
		name2="";
		max=&&_max1_&_i;
		output;
	%end;
%end;
%if %upcase(&unanc2.)^=NONE %then %do;
	%do _i=1 %to &_nunanc2.;
		name1="";
		name2="&&_unanc2_&_i";
		/* Lav dette om når %LIRT_MML_SMART bliver rettet så der kun er en max-variabel */
		max=&&_max2_&_i;
		output;
	%end;
%end;
run;

/* Lav istedet noget hvor man skriver følge af items der er 2PL */

%if %upcase(&birn.)^=NONE %then %do;

	data &outdata.;
	set &outdata.;
	%do _i=1 %to &_nbirn.;
		if name1="&&_birn&_i" then do;
			disc_yn='Y';
		end;
		if name2="&&_birn&_i" then do;
			disc_yn='Y';
		end;
	%end;
	run;

%end;

options notes;
ods exclude none;
%mend LIRT_NAMES_MML;
