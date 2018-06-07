/************************************************************************************************
Call macro using

%LIRT_PPAR(data, names, dim, id, out, delete=Y);

where

DATA: 	data set with items 
NAMES: 	data set with item information and estimates (&out_names from %lirt_mml.sas) 
DIM: 	dimension of latent variable (1 or 2 time points)
ID: 	variable in DATA holding a unique person id 
OUT: 	prefix for output data sets
DELETE: Y/N indicating whether all temporary data sets beginning with an _ should be deleted.*/
************************************************************************************************/

%macro LIRT_PPAR(data, 
		 names, 
		 dim, 
		 id, 
		 out,  
		 delete=Y);

options nomprint nonotes;
ods exclude all;
%if &dim.=1 %then %do;

	proc sql noprint;
	select count(distinct(name))
	into :_nitems
	from &names.;
	quit;

	%let _nitems=&_nitems.;

	proc sql noprint;
	select distinct(name), max
	into :item1-:item&_nitems., 
		 :max1-:max&_nitems.
	from &names.;
	quit;

	data _items;
	set &names.;
	%do _i=1 %to &_nitems.;
		%do _h=0 %to &&max&_i;
			if name="&&item&_i" and score=&_h. then do; 
				call symput("ipar&_i._&_h.",ipar);
				call symput("disc&_i.",disc);
			end; 
		%end;	
	%end;
	run;

	data _scores;
	set &data.;
	s=sum(&item1. %do _i=2 %to &_nitems.; , &&item&_i %end;);
	m=sum((&item1.^=.)*&max1. %do _i=2 %to &_nitems.; ,(&&item&_i^=.)*&&max&_i %end;);
	run;

	data _new; 
	format value 3. item $20.;
	set _scores; 
	%do _i=1 %to &_nitems.; 
		item="&&item&_i"; 
		value=&&item&_i; 
		person=_N_; 
		output;
	%end;
	run;

	/*******************************************/
	/* start of item parameter estimation part */
	/*******************************************/

	proc sql noprint;
	select count(distinct(person))
	into :_npersons
	from _new
	where m-s>0 and s>0;
	quit;
	
	%let _npersons=&_npersons.;

	proc sql noprint;
	select distinct(person)
	into :_pers1-:_pers&_npersons.
	from _new
	where m-s>0 and s>0;
	quit;

	/* numerical maximization using PROC NLMIXED */

	%do _n=1 %to &_npersons.;	
			
		* direct output to files;
		ods output nlmixed.Fitstatistics =_logl&&_pers&_n;
		ods output nlmixed.ConvergenceStatus=_conv&&_pers&_n;
		ods output nlmixed.ParameterEstimates=_ppar&&_pers&_n;	
		
		data _new_;
		set _new (where=(person=&&_pers&_n));
		run;

		proc nlmixed data=_new_;
		parms theta&&_pers&_n=0; 
		%do _i=1 %to &_nitems.;
			_denom=1 %do _k=1 %to &&max&_i; 
				+exp(&&disc&_i.*(&_k.*theta&&_pers&_n+&&ipar&_i._&_k)) 
			%end;;
			if item="&&item&_i" and value=0 then ll=-log(_denom);
			%do _h=1 %to &&max&_i;
				if item="&&item&_i" and value=&_h then ll=&&disc&_i.*(&_h.*theta&&_pers&_n+&&ipar&_i._&_h)-log(_denom);
			%end;
		%end;
		model value~general(ll);
		run;

	%end;

	data &out._conv;
	set _conv&&_pers&_npersons. %do _n=1 %to %eval(&_npersons.-1); _conv&&_pers&_n %end;;
	person=scan(parameter,1,'theta')*1;
	run;

	%do _n=1 %to &_npersons.;

		data _out_logl&&_pers&_n;
		set	_logl&&_pers&_n (where=(Descr='-2 Log Likelihood'));
		person=&&_pers&_n;
		run;

	%end;

	data &out._logl;
	set _out_logl&&_pers&_npersons. %do _n=1 %to %eval(&_npersons.-1); _out_logl&&_pers&_n %end;;
	run;

	data _outdata1;
	set _ppar&&_pers&_npersons. %do _n=1 %to %eval(&_npersons.-1); _ppar&&_pers&_n %end;;
	person=scan(parameter,1,'theta')*1;
	run;
	
	proc sql;
	create table _id as	select distinct(&id.), person
	from _new;
	quit;

	proc sql;
	create table &out._ppar as select
	b.&id.,
	a.estimate as theta label='Theta',
	a.StandardError as se_theta label='SE(Theta)'
	from _outdata1 a left join _id b
	on a.person=b.person;
	quit;

	/*
	proc sql;
	create table &out._&data. as select
	a.*,
	b.theta,
	b.se_theta
	from &data. a left join _outdata2 b
	on a.&id.=b.&id.;
	quit;
	*/

%end;
%else %if &dim.=2 %then %do;

	data _items_temp _items1_temp _items2_temp;
	set &names.;
	if name1^=' ' and  name2^=' ' then output _items_temp;
	else if name1^=' ' then output _items1_temp;
	else if name2^=' ' then output _items2_temp;
	run;

	proc sql;
	create table _items as select distinct(name1), name2, max
	from _items_temp;
	quit; 
	
	proc sql;
	create table _items1 as select distinct(name1), max
	from _items1_temp;
	quit; 

	proc sql;
	create table _items2 as select distinct(name2), max
	from _items2_temp;
	quit; 

	/* rec1 and rec2 are variables indicating whether unachored items are present or not. */
	/* Kunne have nøjedes med at gøre det til tid 1 f.eks. */

	proc sql noprint;
	select count(*)
	into :rec1
	from _items1;
	quit;

	proc sql noprint;
	select count(*)
	into :rec2
	from _items2;
	quit;

	proc sql noprint;
	select count(distinct(name1))
	into :_nitems
	from _items;
	quit;

	%let _nitems=&_nitems.;
	
	%if &rec1>0 %then %do;

		proc sql noprint;
		select count(distinct(name1))
		into :_nitems1
		from _items1;
		quit;
		
		%let _nitems1=&_nitems1.;

	%end;
	%if &rec2>0 %then %do;

		proc sql noprint;
		select count(distinct(name2))
		into :_nitems2
		from _items2;
		quit;

		%let _nitems2=&_nitems2.;

	%end;

	/* Gem eventuelt også item_text for at kunne matche estimerede parametre med items */

	proc sql noprint;
	select distinct(name1), name2, max 
	into :item1_1-:item1_&_nitems., 
		 :item2_1-:item2_&_nitems.,  
		 :max1-:max&_nitems.
	from _items;
	quit;

	/* Save item estimated item parameters */

	proc sort data=_items_temp;
	by name1 score;
	run;

	data _items_temp2;
	set _items_temp;
	by name1;
	%do _i=1 %to &_nitems.;
		if name1="&&item1_&_i" and first.name1 then call symput("disc&_i.",disc);
		%do _h=0 %to &&max&_i;
			if name1="&&item1_&_i" and score=&_h. then call symput("ipar&_i._&_h.",ipar);	
		%end;	
	%end;
	run;

	%if &rec1>0 %then %do;

		proc sql noprint;
		select distinct(name1), max
		into :item1_%eval(&_nitems.+1)-:item1_%eval(&_nitems.+&_nitems1.), 
			 :max%eval(&_nitems.+1)-:max%eval(&_nitems.+&_nitems1.)
		from _items1;
		quit;

		proc sort data=_items1_temp;
		by name1 score;
		run;
	
		data _items1_temp2;
		set _items1_temp;
		by name1;
		/* Løkken skal vist bare køres fra 1 og frem */
		%do _i=%eval(&_nitems.+1) %to %eval(&_nitems.+&_nitems1.);
			if name1="&&item1_&_i" and first.name1 then call symput("disc&_i.",disc);
			%do _h=0 %to &&max&_i;
				if name1="&&item1_&_i" and score=&_h. then call symput("ipar&_i._&_h.",ipar);
			%end;	
		%end;
		run;
		
	%end;
	%if &rec2>0 %then %do;

		proc sql noprint;
		select distinct(name2), max
		into :item2_%eval(&_nitems.+&_nitems1.+1)-:item2_%eval(&_nitems.+&_nitems1.+&_nitems2.),
			 :max%eval(&_nitems.+&_nitems1.+1)-:max%eval(&_nitems.+&_nitems1.+&_nitems2.)
		from _items2;
		quit;

		proc sort data=_items2_temp;
		by name2 score;
		run;

		data _items2_temp2;
		set _items2_temp;
		by name2;
		%do _i=%eval(&_nitems.+&_nitems1.+1) %to %eval(&_nitems.+&_nitems1.+&_nitems2.);
			if name2="&&item2_&_i" and first.name2 then call symput("disc&_i.",disc);
			%do _h=0 %to &&max&_i;
				if name2="&&item2_&_i" and score=&_h then call symput("ipar&_i._&_h.",ipar);
			%end;	
		%end;
		run; 

	%end;
	
	data _scores;
	set &data.;
	s1=sum(&item1_1 %do _i=2 %to &_nitems.; , &&item1_&_i %end; 
		   %if &rec1>0 %then %do; 
		   	    %do _i=%eval(&_nitems.+1) %to %eval(&_nitems.+&_nitems1.); 
					, &&item1_&_i
		   	    %end;
		   %end;);
	s2=sum(&item2_1 %do _i=2 %to &_nitems.; , &&item2_&_i %end; 
		   %if &rec2>0 %then %do; 
		   	    %do _i=%eval(&_nitems.+&_nitems1.+1) %to %eval(&_nitems.+&_nitems1.+&_nitems2.); 
					, &&item2_&_i
		   	    %end;
		   %end;);
	m1=sum((&item1_1^=.)*&max1. %do _i=2 %to &_nitems.; , (&&item1_&_i^=.)*&&max&_i %end; 
		   %if &rec1>0 %then %do; 
		   	    %do _i=%eval(&_nitems.+1) %to %eval(&_nitems.+&_nitems1.); 
					, (&&item1_&_i^=.)*&&max&_i
		   	    %end;
		   %end;);
	m2=sum((&item2_1^=.)*&max1. %do _i=2 %to &_nitems.; , (&&item2_&_i^=.)*&&max&_i %end; 
		   %if &rec2>0 %then %do; 
		   	    %do _i=%eval(&_nitems.+&_nitems1.+1) %to %eval(&_nitems.+&_nitems1.+&_nitems2.); 
					, (&&item2_&_i^=.)*&&max&_i
		   	    %end;
		   %end;);
	run;
	
	data _new1; 
	format value 3. item $20.;
	set _scores; 
	%do _i=1 %to &_nitems.; 
		item="&&item1_&_i"; 
		value=&&item1_&_i; 
		person=_N_; 
		output;
		item="&&item2_&_i"; 
		value=&&item2_&_i; 
		person=_N_; 
		output;
	%end;
	%if &rec1>0 %then %do;
		%do _i=%eval(&_nitems.+1) %to %eval(&_nitems.+&_nitems1.); 
			item="&&item1_&_i"; 
			value=&&item1_&_i; 
			person=_N_; 
			output;
		%end;
	%end;
	%if &rec2>0 %then %do;
		%do _i=%eval(&_nitems.+&_nitems1.+1) %to %eval(&_nitems.+&_nitems1.+&_nitems2.); 
			item="&&item2_&_i"; 
			value=&&item2_&_i; 
			person=_N_; 
			output;
		%end;
	%end;
	run;
	
	proc sql noprint;
	select count(distinct(person))
	into :_npersons
	from _new1;	
	quit;
	
	%let _npersons=&_npersons.;

	/* numerical maximization using PROC NLMIXED */

	%do _n=1 %to &_npersons.;	
			
		data _new2;
		set _new1 (where=(person=&_n.));
		run;

		/* direct output to files */

		ods output nlmixed.ConvergenceStatus=_conv&_n;
		ods output nlmixed.Fitstatistics =_logl&_n;
		ods output nlmixed.ParameterEstimates=_ppar&_n;	
		ods output nlmixed.additionalestimates=_change&_n;  
		
		proc nlmixed data=_new2;
		parms theta1_&_n=0, theta2_&_n=0; 
		%do _i=1 %to &_nitems.;
			_denom=1 %do _k=1 %to &&max&_i; 
				+exp(&&disc&_i.*(&_k.*theta1_&_n.+&&ipar&_i._&_k)) 
			%end;;
			if item="&&item1_&_i" and value=0 then ll=-log(_denom);
			%do _h=1 %to &&max&_i;
				if item="&&item1_&_i" and value=&_h then ll=&&disc&_i.*(&_h.*theta1_&_n.+&&ipar&_i._&_h)-log(_denom);
			%end;
		%end;
		%do _i=1 %to &_nitems.;  
			_denom=1 %do _k=1 %to &&max&_i; 
				+exp(&&disc&_i.*(&_k.*theta2_&_n.+&&ipar&_i._&_k)) 
			%end;;
			if item="&&item2_&_i" and value=0 then ll=-log(_denom);
			%do _h=1 %to &&max&_i;
				if item="&&item2_&_i" and value=&_h then ll=&&disc&_i.*(&_h.*theta2_&_n.+&&ipar&_i._&_h)-log(_denom);
			%end;; 
		%end;
		%if &rec1>0 %then %do;		
			%do _i=%eval(&_nitems.+1) %to %eval(&_nitems.+&_nitems1.); 	
				_denom=1 %do _k=1 %to &&max&_i; 
					+exp(&&disc&_i.*(&_k.*theta1_&_n.+&&ipar&_i._&_k)) 
				%end;;
				if item="&&item1_&_i" and value=0 then ll=-log(_denom);
				%do _h=1 %to &&max&_i;
					if item="&&item1_&_i" and value=&_h. then ll=&&disc&_i.*(&_h.*theta1_&_n.+&&ipar&_i._&_h)-log(_denom);
				%end; 
			%end;
		%end;
		%if &rec2>0 %then %do;		
			%do _i=%eval(&_nitems.+&_nitems1.+1) %to %eval(&_nitems.+&_nitems1.+&_nitems2.); 
				_denom=1 %do _k=1 %to &&max&_i; 
					+exp(&&disc&_i.*(&_k.*theta2_&_n.+&&ipar&_i._&_k)) 
				%end;;
				if item="&&item2_&_i" and value=0 then ll=-log(_denom);
				%do _h=1 %to &&max&_i;
					if item="&&item2_&_i" and value=&_h. then ll=&&disc&_i.*(&_h.*theta2_&_n.+&&ipar&_i._&_h)-log(_denom);
				%end;
			%end;
		%end;
		model value~general(ll);
		estimate "delta&_n" theta2_&_n.-theta1_&_n.;
		run;

	%end;

	data &out._conv;
	set _conv&_npersons. %do _n=1 %to %eval(&_npersons.-1); _conv&_n %end;;
	run;

	data _latent_temp;
	set _ppar&_npersons. %do _n=1 %to %eval(&_npersons.-1); _ppar&_n %end;;
	run;

	data _change_temp;
	set _change&_npersons. %do _n=1 %to %eval(&_npersons.-1); _change&_n %end;;
	run;

	proc sql;
	create table _id as select distinct(&id.), person, s1, s2, m1, m2
	from _new1;
	quit;

	data _latent1 _latent2;
	set _latent_temp;
	person=scan(parameter,-1,'_')*1;
	if scan(scan(parameter,1,'theta'),1,'_')=1 then output _latent1;
	if scan(scan(parameter, 1, 'theta'),1,'_')=2 then output _latent2;
	run;

	proc sql;
	create table _outdata1 as select 
	a.*,
	b.estimate as theta1 label='Theta1',
	b.StandardError as se_theta1 label='SE(Theta1)'
	/*,b.lower as lower1 label='Lower(Theta1)'
	  ,b.upper as upper1 label='Upper(Theta1)'*/
	from _id a left join _latent1 b
	on a.person=b.person;
	quit;

	proc sql;
	create table _outdata2 as select 
	a.*,
	b.estimate as theta2 label='Theta2',
	b.StandardError as se_theta2 label='SE(Theta2)'
	/*,b.lower as lower2 label='Lower(Theta2)'
	  ,b.upper as upper2 label='Upper(Theta2)'*/
	from _outdata1 a left join _latent2 b
	on a.person=b.person;
	quit;

	/* Output data with latent variables */

	data _change;
	set _change_temp;
	person=scan(label,1,'delta')*1;
	run;

	proc sql;
	create table _outdata3 (drop=person) as select 
	a.*,
	b.estimate as delta label='Delta',
	b.StandardError as se_delta label='SE(Delta)'
	/*,b.lower as lower2 label='Lower(Theta2)'
	  ,b.upper as upper2 label='Upper(Theta2)'*/
	from _outdata2 a left join _change b
	on a.person=b.person;
	quit;
	
	data &out._ppar (drop=s1 s2 m1 m2);
	set _outdata3;
	if m1-s1=0 or s1<=0 then do;
		theta1=.;
		se_theta1=.;
	end;
	if m2-s2=0 or s2<=0 then do;
		theta2=.;
		se_theta2=.;
	end;
	if theta1=. or theta2=. then do;
		delta=.;
		se_delta=.;
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

	options notes;

%end;
ods exclude none;
%mend LIRT_PPAR;
