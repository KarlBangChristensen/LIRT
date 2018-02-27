

/* Macro plotting ICC curves for 2 dimensional IRT models (1PL and 2PL)*/

/* 

Names: data set with information on items. The following variables 
	
			name1
			name2
			max
			score
			ipar
			disc
			thres	

NOTE: If name1 and name2 are both non-empty that item is anchored. Otherwise it is not.

*/

%macro LIRT_ICC(names, 
				dim, 
				out,
				delete=Y);

options nomprint nonotes;

%if &dim.=1 %then %do;

	data _ipar1;
	set &names.;
	order0=_n_;
	title=name;
	run;

	proc sort data=_ipar1;
	by title;
	run;

	data _ipar2;
	set _ipar1;
	retain order;
	by title;
	if first.title then order=_n_;
	run;

	/* Selecting proper range for theta's */

	proc sql noprint;
	select round(min(thres)-1), round(max(thres)+1)
	into :xmin, :xmax
	from &names.;
	quit;

	%let xmin=&xmin.;
	%let xmax=&xmax.;
			
	/* Calculate 1PL/2PL probabilities for a range of theta's */

	data _icc1; 
	set _ipar2;
	do _theta=%eval(&xmin.) to %eval(&xmax.) by 0.01; 
		output; 
	end;
	run;

	/* Datasæt med (x,y) for ICC-kurver*/

	proc sql;
	create table _icc2 as select *,
	exp(disc*(score*_theta+ipar))/(sum(exp(disc*(score*_theta+ipar)))) as _prob
	from _icc1
	group by name, _theta
	order by order, score, _theta;
	quit;

	/* Save item_name and max for each item in the in the input data set &ipar (based on the variable item_no) */

	/* Gøres i to steps og med order fordi man ellers får gemt items i alfabetisk rækkefølge istedet for input orden */

	proc sql;
	create table _icc3 as select distinct(title), max, order 
	from _icc2;
	quit;

	proc sql noprint;
	select count(distinct(title))
	into :_ntitle
	from _icc3;
	quit;

	%let _ntitle=&_ntitle.;

	proc sql noprint;
	select title, max
	into :_title1-:_title&_ntitle., :_max1-:_max&_ntitle.
	from _icc3
	order by order;
	quit;
			
	/* Plotting ICC's */

	ods listing;
		
	axis1 order=(%eval(&xmin.) to %eval(&xmax.) by 1) length=15 cm value=(H=2) minor=NONE label=(H=2 'Latent variable');
	axis2 order=(0 to 1 by 0.1) label=(H=2 A=90 'Probability') length=10 cm value=(H=2) minor=NONE;

	%do _i=1 %to &_ntitle.;

		symbol1 v=none i=join l=1 w=1 c=black w=3 r=%eval(&&_max&_i+1);

		title "&&_title&_i";	

		proc gplot data=_icc2 (where=(title="&&_title&_i"));
		plot _prob*_theta=score / haxis=axis1 vaxis=axis2 nolegend;
		run; 
		quit;

	%end;

	ods listing close;

	proc sql;
	create table &out._plot as select
	name,
	score,
	ipar,
	thres, /* Drop evt. threshold */
	disc,
	_theta as theta,
	_prob  as Probability
	from _icc2
	order by order0, theta;
	quit;

%end;
%else %if &dim.=2 %then %do;

	data _ipar1;
	set &names.;
	order0=_n_;
	if name1^='' and name2^='' then title=strip(name1)||' and '||strip(name2);
	else title=strip(name1)||strip(name2);
	run;

	proc sort data=_ipar1;
	by title;
	run;

	data _ipar1;
	set _ipar1;
	retain order;
	by title;
	if first.title then order=_n_;
	run;


	/* Selecting proper range for theta's */

	proc sql noprint;
	select round(min(thres)-1), round(max(thres)+1)
	into :xmin, :xmax
	from &names.;
	quit;

	%let xmin=&xmin.;
	%let xmax=&xmax.;
			
	/* Calculate 1PL/2PL probabilities for a range of theta's */

	data _icc1; 
	set _ipar1;
	do _theta=%eval(&xmin.) to %eval(&xmax.) by 0.01; 
		output; 
	end;
	run;

	/* Datasæt med (x,y) for ICC-kurver*/

	proc sql;
	create table _icc2 as select *,
	exp(disc*(score*_theta+ipar))/(sum(exp(disc*(score*_theta+ipar)))) as _prob
	from _icc1
	group by order, _theta
	order by order, score, _theta;
	quit;

	/* Save item_name and max for each item in the in the input data set &ipar (based on the variable item_no) */

	/* Gøres i to steps og med order fordi man ellers får gemt items i alfabetisk rækkefølge istedet for input orden */

	proc sql;
	create table _icc3 as select distinct(title), max, order
	from _icc2;
	quit;

	proc sql noprint;
	select count(distinct(title))
	into :_ntitle
	from _icc3;
	quit;

	%let _ntitle=&_ntitle.;

	proc sql noprint;
	select distinct(order), title, max
	into :_order1-:_order&_ntitle., :_title1-:_title&_ntitle., :_max1-:_max&_ntitle.
	from _icc3
	order by order;
	quit;
			
	/* Plotting ICC's */

	ods listing;
		
	axis1 order=(%eval(&xmin.) to %eval(&xmax.) by 1) length=15 cm value=(H=2) minor=NONE label=(H=2 'Latent variable');
	axis2 order=(0 to 1 by 0.1) label=(H=2 A=90 'Probability') length=10 cm value=(H=2) minor=NONE;

	%do _i=1 %to &_ntitle.;

		symbol1 v=none i=join l=1 w=1 c=black w=3 r=%eval(&&_max&_i+1);

		title "&&_title&_i";	

		proc gplot data=_icc2 (where=(title="&&_title&_i"));
		plot _prob*_theta=score / haxis=axis1 vaxis=axis2 nolegend;
		run; 
		quit;

	%end;

	ods listing close;

	proc sql;
	create table &out._plot as select
	name1,
	name2,
	score,
	ipar,
	thres, /* Drop evt. threshold */
	disc,
	_theta as theta,
	_prob  as Probability
	from _icc2
	order by order0, theta;
	quit;

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

%mend LIRT_ICC;

