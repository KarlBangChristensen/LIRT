/**********************************************************

Goodness of fit macro (create data set that can be used for simulation based graphical evaluation of fit).

	ITEM: the item for which the fit should be checked

	DATA: data set with items

	NAMES: names data set (containing names and item information of all items in the scale). 
		   OUT_names from lirt_mml macro can be used

	DIM: dimension of latent variable, DIM=1 (2 is not yet implemented)

	NSIMU: number of simulated data sets for fitplot or fittest (default value 100)

	CLASS_SIZE: minimum size of score groups in the collapsed score (default value 20)

	OUT: the prefix for the output data set

**********************************************************/
%macro LIRT_GOF(item, 
				data, 
				names, 
				dim,
				nsimu=100, 
				CLASS_SIZE=20, 
				out=GOF);

%let out=%trim(&out);
ods exclude all;
goptions reset=all;
options nonotes /*no*/mprint;

* Include %LIRT_SIMU;
%let git=https://raw.githubusercontent.com/KarlBangChristensen/LIRT/master;
filename simu url "&git/LIRT_SIMU.sas";
%include simu;

* count number of records in data set;
data _null_; 
	set &data end=final; 
	if final then call symput('N',trim(left(_N_))); 
run;

%put;
%put ****************************************;
%put fitplot data set &data (N=&N);
%put ****************************************;
%put ;

/**** DIM=1 ****/
%if &dim=1 %then %do;

	data _pdata;
		PARAMETER='sigma'; ESTIMATE=1; output;
		PARAMETER='mu'; ESTIMATE=0; output;
	run;

	* read item names and maximum item scores from names data set;
	proc sql noprint;
		select count(unique(name)) into :_nitems from &names;
	quit;
	%let _nitems=&_nitems;
	proc sql noprint;
		select unique(name) into :_item1-:_item&_nitems from &names;
	quit;
	proc sql noprint;
		select max into :_max1-:_max&_nitems from &names where score=0;
	quit;
	proc sql noprint;
		select sum(max) into :_maxscore from &names where score=0;
	quit;
	%do _i=1 %to &_nitems; 
		%let _item&_i=&&_item&_i; 
		%let _max&_i=&&_max&_i; 
	%end;
	%put total score (range 0-%eval(&_maxscore)) will be collapsed (class size=%eval(&CLASS_SIZE));
	%put ****************************************;
	* simulate data sets;
	%lirt_simu(NAMES=&names, DIM=1, NDATA=&nsimu, NPERSONS=&N, PDATA=_pdata, OUT=s);
	ods exclude all;
	options nonotes /*no*/mprint;
	* combine observed and simulated data sets - compute score (item mean rescaled);
	data _s0 /*(rename=(&id.=id))*/; set &data; dataset=0; run;
	%do _simu=1 %to &nsimu;
		data _s&_simu; set s_simu&_simu; dataset=&_simu; run;
	%end;
	data _gof; 
		set _s0-_s&nsimu; 
		%do _i=1 %to &_nitems;
			__&_i=&&_item&_i/&&_max&_i ;
		%end;
		score=&_maxscore*mean(of __1-__&&_nitems); 
	run;	
	* collaps score into score groups;
	proc freq data=_gof; ods output Freq.Table1.OneWayFreqs=_margdist; table score/nocum; run;
	data _collaps;
		set _margdist end=last;
		retain _sum1(0);
		retain interval(0);
		if _sum1 ge %eval(&NSIMU*&CLASS_SIZE) then do; 
			interval=interval+1; 
			_sum1=0;  
		end;
		_sum1=_sum1+frequency;
		if last then do;
			call symput('_maxint',interval);
			call symput('_maxsum',_sum1);
		end;
	run;
	* is number in last score group big enough ?;
	%if &_maxsum < &CLASS_SIZE %then %do;
		data _collaps; set _collaps; if interval=&_maxint then interval=&_maxint-1; run;
	%end;

	proc sort data=_gof; by score; run;
	proc sort data=_collaps; by score; run;
	data _gof2; merge _gof _collaps; by score; run;

	proc means data=_gof2;
		var &item;
		class interval dataset;
		output out=_means mean=&item._mean;
	run;
	data _gofplot;
		set _means;
		drop _TYPE_ _FREQ_;
		if (interval=.) or (dataset=.) then delete;
	run;
	proc univariate data=_gofplot noprint;
		where dataset ne 0;
		by interval NOTSORTED;
		var &item._mean;
		output out=Pctls pctlpts = 1 2.5 97.5 99 pctlpre = &item._ pctlname = pct1 pct5 pct95 pct99;
	run;
	data &out.gofplot;
		set _gofplot(where=(dataset=0)) pctls;
	run;
%end;

/******************************************************************************/
/************ deleting temporary data sets generated by the macro *************/
/******************************************************************************/

ods output Datasets.Members=_datasets;

proc datasets nodetails n;
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

proc datasets nodetails nolist; 
	delete %do k=1 %to &_ndata.; &&_data&k %end; _datasets;
run;
quit;


proc datasets nodetails nolist; 
	delete s_simu1-s_simu&NSIMU;
run;
quit;
ods exclude none;
proc sgplot data=&out.gofplot;
	band x=interval lower=&item._pct1 upper=&item._pct99 / fillattrs=(color=lightgrey);
	band x=interval lower=&item._pct5 upper=&item._pct95 / fillattrs=(color=darkgrey);
	series y=&item._mean x=interval / lineattrs=(thickness=3 pattern=1 color=black);
run;

options notes;
%mend LIRT_GOF;
