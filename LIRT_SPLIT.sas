

/*Macro splitting for local dependence*/

/* DATA:  Data on the form required for use of %MML2D with one variable for each item */
/* NAMES: Data set with decription of the items (name1 name2 max) */
/* DIM:   Dimension (1 or 2) of the model. */
/* INDEP: Items to split DEP according to */
/* DEP:   Items to be split separated by blanks 
	   	  (first item in the sequence is split according to the value of first item in INDEP sequence) */

/* OUTPUT */

/* &DATA._SPLIT: Dataset with original and split items */
/* &names._SPLIT: Original names data with split item added */


%macro LIRT_SPLIT(data, names, dim, indep, dep, delete=Y);

options nomprint nonotes;

%if &dim.=1 %then %do;

	%let _indep1=%scan(&indep.,1,' ');
	%let _nindep=1;

	/* If more than one item with response dependence */
		
	%if %scan(&indep.,1,' ')^=%scan(&indep.,-1,' ') %then %do;
		%let _i=1;
		%do %until (%scan(&indep.,%eval(&_i.),' ')=%scan(&indep.,-1,' '));
			%let _indep%eval(&_i.+1)=%scan(&indep.,%eval(&_i.+1),' ');
			%let _i=%eval(&_i.+1);
		%end;
		%let _nindep=%eval(&_i.);
	%end;

	%let _dep1=%scan(&dep.,1,' ');
	%let _ndep=1;

	/* If more than one item with response dependence */
		
	%if %scan(&dep.,1,' ')^=%scan(&dep.,-1,' ') %then %do;
	%let _i=1;
		%do %until (%scan(&dep.,%eval(&_i.),' ')=%scan(&dep.,-1,' '));
			%let _dep%eval(&_i.+1)=%scan(&dep.,%eval(&_i.+1),' ');
			%let _i=%eval(&_i.+1);
		%end;
		%let _ndep=%eval(&_i.);
	%end;

	data _names0;
	set &names.;
	order=_n_;
	run;

	proc sql noprint;
	select max, disc_yn, order
	into :_m1 %if &_ndep.>1 %then %do; -:_m&_ndep. %end; 
		,:_d1 %if &_ndep.>1 %then %do; -:_d&_ndep. %end;
		,:_ord1 %if &_ndep.>1 %then %do; -:_ord&_ndep. %end;
	from _names0
	where name in (%do _i=1 %to &_ndep.; "&&_dep&_i" %end;);
	quit;

	%do _i=1 %to &_ndep.;
		%let _m&_i=&&_m&_i;
	%end;

	data &data._split;
	set &data.;
	%do _i=1 %to &_ndep.;
		%do _h=0 %to &&_m&_i;
			if &&_indep&_i=&_h. then cat&_h._&&_dep&_i=&&_dep&_i;
			else cat&_h._&&_dep&_i=.;
		%end;
	%end;	 
	run;

	data _names1;
	format name $20.;
	set _names0;
	if name in (%do _i=1 %to &_ndep.; "&&_dep&_i" %end;) then delete;
	run;

	data _names2;
	format name $20.;
	%do _i=1 %to &_ndep.;
		%do _h=0 %to &&_m&_i;
			 name="cat&_h._&&_dep&_i";
			 max=&&_m&_i;
			 disc_yn="&&_d&_i";
			 order=(&&_ord&_i)*1;
			 output;
		%end;
	%end;
	run;

	data _names3;
	set _names1 _names2;
	run;

	proc sort data=_names3 out=&names._split (drop=order);
	by order name;
	run;

%end;
%else %if &dim.=2 %then %do;

	%let _indep1=%scan(&indep.,1,' ');
	%let _nindep=1;

	/* If more than one item with response dependence */
		
	%if %scan(&indep.,1,' ')^=%scan(&indep.,-1,' ') %then %do;
		%let _i=1;
		%do %until (%scan(&indep.,%eval(&_i.),' ')=%scan(&indep.,-1,' '));
			%let _indep%eval(&_i.+1)=%scan(&indep.,%eval(&_i.+1),' ');
			%let _i=%eval(&_i.+1);
		%end;
		%let _nindep=%eval(&_i.);
	%end;

	%let _dep1=%scan(&dep.,1,' ');
	%let _ndep=1;

	/* If more than one item with response dependence */
		
	%if %scan(&dep.,1,' ')^=%scan(&dep.,-1,' ') %then %do;
	%let _i=1;
		%do %until (%scan(&dep.,%eval(&_i.),' ')=%scan(&dep.,-1,' '));
			%let _dep%eval(&_i.+1)=%scan(&dep.,%eval(&_i.+1),' ');
			%let _i=%eval(&_i.+1);
		%end;
		%let _ndep=%eval(&_i.);
	%end;

	proc sql noprint;
	select max
	into :_m1 %if &_ndep.>1 %then %do; -:_m&_ndep. %end;
	from &names.
	where name2 in (%do _i=1 %to &_ndep.; "&&_dep&_i" %end;);
	quit;

	%do _i=1 %to &_ndep.;
		%let _m&_i=&&_m&_i;
	%end;

	data &data._split;
	set &data.;
	%do _i=1 %to &_ndep.;
		%do _h=0 %to &&_m&_i;
			if &&_indep&_i=&_h. then cat&_h._&&_dep&_i=&&_dep&_i;
			else cat&_h._&&_dep&_i=.;
		%end;
	%end;	 
	run;

	data _names;
	set &names.;
	format name2 name2_temp $25.;
	output;
	%do _i=1 %to &_ndep.;
		%do _h=0 %to &&_m&_i;
			if name2="&&_dep&_i" then do;
				name2_temp="cat&_h._&&_dep&_i";
				output;
			end;
		%end;
	%end;
	run;

	data _names (drop=name2);
	set _names;
	format name3 $25.;
	name3=name2;
	run;

	data &names._split (drop=name2_temp rename=(name3=name2));
	set _names;
	if name3 in (%do _i=1 %to &_ndep.; "&&_dep&_i" %end;) and name2_temp='' then do;
		if name1='' then delete;
		else name3='';
	end;
	if name2_temp^='' then do; 
		name3=name2_temp;
		name1='';
	end;
	run; 

%end;
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

%mend LIRT_SPLIT;
