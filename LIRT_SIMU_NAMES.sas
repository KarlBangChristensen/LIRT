/**************************************************************************

SAS macro that creates a data set with item names to use for %LIRT_MML.sas

call the macro using


OUTDATA:the resulting data set with item names
PARDATA:data set with item names and corresponding scores and parameters 
	(columns: name, score, ipar, disc)
ANC1: 	anchor items at time 1 separated by blanks 
ANC2: 	anchor items at time 2 separated by blanks 
UNANC1: unanchored items at time 1 separated by blanks (default none)
UNANC2: unanchored items at time 2 separated by blanks (default none)

(first item in the sequence ANC1 and ANC2 are assumed to have the same item parameters and so on)

/* If the result of a split item, item2_0 and item2_1, in UNANC2 are (was) locally dependent on one of the items, item1 in UNANC1*/
/* Then it should be written as follows UNANC2=... item2_0 (item1=0) item2_1 (item1=1) */


%macro LIRT_NAMES_SIMU(	PARDATA,
			OUTDATA, 
		   	ANC1, 
		   	ANC2, 
		   	UNANC1=none, 
		   	UNANC2=none);

	options nomprint nonotes;
	ods exclude all;

	/* Save anchored items as macro variables */

	%let _anc1_1=%scan(&anc1.,1," ");
	%let _anc2_1=%scan(&anc2.,1," ");
	%let _nanc=1;

	%if %scan(&anc1.,1," ")^=%scan(&anc1.,-1," ") %then %do;

		%let _i=1;
		%do %until (%scan(&anc1.,%eval(&_i.)," ")=%scan(&anc1.,-1," "));
			%let _anc1_%eval(&_i.+1)=%scan(&anc1.,%eval(&_i.+1)," ");
			%let _anc2_%eval(&_i.+1)=%scan(&anc2.,%eval(&_i.+1)," ");
			%let _i=%eval(&_i.+1);
		%end;
		%let _nanc=%eval(&_i.);

	%end;
	%if %upcase(&unanc1.)^=NONE %then %do;

		/* Save unanchored items as macro variables */

		%let _unanc1_1=%scan(&unanc1.,1," ");
		%let _nunanc1=1;

		%if %scan(&unanc1.,1," ")^=%scan(&unanc1.,-1," ") %then %do;
			%let _i=1;
			%do %until (%scan(&unanc1.,%eval(&_i.)," ")=%scan(&unanc1.,-1," "));
				%let _unanc1_%eval(&_i.+1)=%scan(&unanc1.,%eval(&_i.+1)," ");
				%let _i=%eval(&_i.+1);
			%end;
			%let _nunanc1=%eval(&_i.);
		%end;
	%end;
	%if %upcase(%scan(&unanc2.,1," "))^=NONE %then %do;

		%let _n=1;

		%do %until ("%scan(&unanc2.,&_n.,' ')"="%scan(&unanc2.,-1,' ')");
			
			%let _var&_n.=%scan(&unanc2.,&_n.,' ');
			%let _n=%eval(&_n.+1);

		%end;	

		%let _var&_n.=%scan(&unanc2.,&_n.,' ');
		
		data _data1;
		format item1 $20.;
		%do _i=1 %to &_n.;
			item1="&&_var&_i";
			output;
		%end;
		run;

		data _data2;
		set _data1;
		item2=lag1(item1);
		obs=1;
		run;

		data _data3 (drop=obs);
		set _data2;
		by obs;
		if first.obs then delete;
		else output;
		if last.obs then do;
			item2=item1;
			item1=' ';
			output;
		end;
		run;
		
		data _data4;
		set _data3;
		if substr(item2,1,1)='(' then delete;
		if substr(item1,1,1)='(' then do;
			ld=1;
			ld_item=scan(scan(item1,1,"("),1,"=");
			ld_group=scan(scan(item1,1,")"),2,"=")*1;
		end;
		else ld=0;
		run;

		proc sql noprint;
		select count(*)
		into :_nunanc2
		from _data4;
		quit;

		%let _nunanc2=&_nunanc2.;

		proc sql noprint;
		select item2, ld_item, ld_group, ld
		into :_unanc2_1-:_unanc2_&_nunanc2., 
			 :_ld_item1-:_ld_item&_nunanc2., 
		     :_ld_group1-:_ld_group&_nunanc2., 
			 :_ld1-:_ld&_nunanc2.
		from _data4;
		quit;

	%end;

	data _outdata1;
	format name1 name2 $20. /*disc 6.4*/;
	%do _i=1 %to &_nanc.;
		name1="&&_anc1_&_i";
		name2="&&_anc2_&_i";
		ld_item='';
		ld_group=.;
		output;
	%end;
	%if %upcase(&unanc1.)^=NONE %then %do;
		%do _i=1 %to &_nunanc1.;
			name1="&&_unanc1_&_i";
			name2=" ";
			ld_item='';
			ld_group=.;
			output;
		%end;
	%end;
	%if %upcase(%scan(&unanc2.,1," "))^=NONE %then %do;
		%do _i=1 %to &_nunanc2.;
			name1=" ";
			name2="&&_unanc2_&_i";
			%if &&_ld&_i=1 %then %do;
				ld_item="&&_ld_item&_i.";
				ld_group=&&_ld_group&_i.;
			%end;
			%else %do;
				ld_item='';
				ld_group=.;
			%end;
			output;
		%end;
	%end;
	run;

	/* Merge items at time 1 with parameter values */

	proc sql;
	create table _outdata2 as select a.*,
	b.score as score1,
	b.ipar as ipar1,
	b.disc as disc1
	from _outdata1 a left join &pardata. b
	on a.name1=b.name;
	quit;
	
	/* Merge items at time 2 with parameter values */

	proc sql;
	create table _outdata3 as select a.*,
	b.score as score2,
	b.ipar as ipar2,
	b.disc as disc2
	from _outdata2 a left join &pardata. b
	on a.name2=b.name;
	quit;

	/* _anc (data set with anchored items), _outdata3*/

	/*
	data _anc _outdata3;
	set _outdata2;
	if name1^='' and name2^='' then output _anc;
	else output _outdata3;
	run;

	
	data _outdata5;
	set _anc _outdata4;
	run;
	*/

	data _outdata4 (drop=score1 score2 ipar1 ipar2 disc1 disc2);
	set _outdata3;
	score=max(score1,score2); 
	ipar=max(ipar1,ipar2);
	disc=max(disc1,disc2); 
	run;

	proc sql;
	create table &outdata. as select  
	name1,        
	name2,       
	score, 
	ipar,  
	disc,
	ld_group,
    ld_item  
	from _outdata4;
	quit;

	ods output Datasets.Members=_datasets;

	proc datasets;
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

	proc datasets; 
	delete %do k=1 %to &_ndata.; &&_data&k %end;;
	run;
	quit;

	options notes;
	ods exclude none;

%mend LIRT_NAMES_SIMU;
