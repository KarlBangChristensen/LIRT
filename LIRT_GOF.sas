/**********************************************************

Goodness of fit macro (plots and simulation based tests).

	ITEM: the item for which the fit should be checked

	DATA: data set with items

	NAMES: names data set (containing names and item information of all items in the scale). 
		   OUT_names from lirt_mml macro can be used

	DIM: dimension of latent variable (1 or 2)

	(ID: name of variable in DATA holding unique person ID)

	POPPAR: data set with parameters of the latent normal distribution. Should be different from the default (POPPAR=none) 
			when DIM=2 (in this case the output data set OUT_poppar from %LIRT_MML can be used).

	FITPLOT: variable indicating whether a fitplot should be made (Y) or not (N)

	FITTEST: variable indicating whether a fittest should be made (Y) or not (N)

	NSIMU: number of simulated data sets for fitplot or fittest (default value 30)

	CLASS_SIZE: minimum size of score groups in the collapsed score

	OUT: the prefix for the output data set

**********************************************************/
%macro LIRT_GOF(item, 
				data, 
				names, 
				dim,
				/*id=id,*/ 
				poppar=none, 
				fitplot=Y, 
				fittest=N, 
				nsimu=30, 
				CLASS_SIZE=20, 
				out=GOF);

%let out=%trim(&out);
ods listing close;
ods html close;
goptions reset=all;
options nonotes nomprint;

options mprint;

* Include %LIRT_SIMU;
* filename simu URL 'http://192.38.117.59/~mola/lirt_simu.sas'; 
* %include simu;

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



/****** FITPLOT ******/

%if %upcase(&fitplot.)=Y %then %do;
	
	/***************/
	/**** DIM=1 ****/
	/***************/

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
			select max into :_max from &names where score=0 and name="&item";
		quit;

		%put _max er &_max;
		proc sql noprint;
			select max into :_max1-:_max&_nitems from &names where score=0;
		quit;
		proc sql noprint;
			select sum(max) into :_maxscore from &names where score=0;
		quit;

		%put item &item (range 0-%eval(&_max));
		%put;

		%do _i=1 %to &_nitems; %let _item&_i=&&_item&_i; %end;
	
		%let error=1;
		%do _i=1 %to &_nitems;
			%if &item=&&_item&_i %then %do; %let error=0; %end;
			%put item&_i = &&_item&_i /*error=&error */ (range 0-&&_max&_i);
		%end;
		%put;
		%put total score (range 0-%eval(&_maxscore)) will be collapsed (class size=%eval(&CLASS_SIZE));
		%put ****************************************;

		%if &error=1 %then %do;
			%put ERROR: &item is not in &names data set;
			%put ****************************************;
			%goto quit; 
		%end;

		* simulate data sets;
		%lirt_simu(NAMES=&names, DIM=1, NDATA=&nsimu, NPERSONS=&N, PDATA=_pdata, OUT=s);
		options nonotes;
		ods listing close;

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
			output out=means mean=mean;
		run;

		axis1 order=0 to &_maxint by 1 value=(H=2) minor=NONE label=(H=2 'total score group');
		axis2 value=(H=2) order=0 to &_max by 1 minor=NONE label=(H=2 A=90 'mean item score');
		
		ods html;
		data _plot; set means(where=(dataset ne .)); if dataset=0 then dataset=&nsimu+1; run;
		proc gplot data=_plot;
			title "Mean scores for item &item.";
			plot mean*interval = dataset / haxis=axis1 vaxis=axis2 nolegend;
			symbol1 v=none i=join w=3 l=33 color=grey r=&nsimu;
			symbol2 v=none i=join w=6 l=1 color=black;
		run;
		quit;
		ods html close;

		data &out._GOFPLOT;
			set _plot;
		run;

	%end;

	/***************/
	/**** DIM=2 ****/
	/***************/

	%if &dim=2 %then %do;

		/* read item names from names data set */
		
		proc sql noprint;
			select count(distinct(name1)) 
			into :_nitems1
			from &names
			where name1^='';
		quit;
		
		proc sql noprint;
			select count(distinct(name2)) 
			into :_nitems2
			from &names
			where name2^='';
		quit;

		%let _nitems1=&_nitems1;
		%let _nitems2=&_nitems2;

		proc sql noprint;
			select distinct(name1) 
			into :_item1_1-:_item1_&_nitems1. 
			from &names;
		quit;
		
		proc sql noprint;
			select distinct(name2) 
			into :_item2_1-:_item2_&_nitems2. 
			from &names;
		quit;

		%do _i=1 %to &_nitems1;
			%let _item1_&_i=&&_item1_&_i;
		%end;
		
		%do _i=1 %to &_nitems2;
			%let _item2_&_i=&&_item2_&_i;
		%end;

		/* Find out whether ITEM (the item to be plotted) is a time 1 or time 2 item */

		data _names;
		set &names.;
		if name1="&item" then time=1;
		if name2="&item" then time=2;
		run;

		proc sql noprint;
		select max(time)
		into :time
		from _names;
		quit;

		%if &time.=. %then %do;
			%put ERROR: &item is not in &names data set;
			%goto quit; 
		%end;
		
		data _pdata2;
		set &poppar.;
		format parameter $10.;
		if parameter='mu' then parameter='mu2';
		if parameter='sigma' then parameter='sigma2';
		run;

		data _pdata1;
		format parameter $10.;
		parameter='mu1';
		estimate=0;
		output;
		parameter='sigma1';
		estimate=1;
		output;
		run;

		data _pdata;
		set _pdata1 _pdata2;
		run;

		/* Simulate from fitted model */

		%lirt_simu(NAMES=&names., DIM=2, NDATA=&nsimu, NPERSONS=&n, PDATA=_pdata, OUT=s, delete=N);
		options nonotes;
		ods listing close;

		data _s0 /*(rename=(&id.=id))*/; set &data; dataset=0; run;

		%do _simu=1 %to &nsimu;
			data _s&_simu; set s_simu&_simu; dataset=&_simu; run;
		%end;

		data _gof; 
		set _s0-_s&nsimu; 
		%if &time.=1 %then %do;
			score=sum(of %do _i=1 %to &_nitems1; &&_item1_&_i %end;); 
		%end;
		%else %if &time.=2 %then %do;
			score=sum(of %do _i=1 %to &_nitems2; &&_item2_&_i %end;); 
		%end;
		run;

		proc means data=_gof;
			var &item.;
			class score dataset;
			output out=means mean=mean;
		run;

		axis1 order=0 to 12 by 1 value=(H=2) minor=NONE label=(H=2);
		axis2 value=(H=2) minor=NONE label=(H=2 A=90);

		title "&item.";

		proc gplot data=means (where=(dataset ne .));
			plot mean*score = dataset / haxis=axis1 vaxis=axis2 nolegend;
			symbol1 v=none i=join w=5 l=1 color=black;
			symbol2 v=none i=join w=3 l=33 color=grey r=&nsimu;
		run;
	
	%end;
	
	goptions reset=all;

	%quit:

%end;
%if %upcase(&fittest.)=Y %then %do;

	
	/***************/
	/**** DIM=1 ****/
	/***************/

	%if &dim=1 %then %do;

		data _pdata;
			PARAMETER='sigma'; ESTIMATE=1; output;
			PARAMETER='mu'; ESTIMATE=0; output;
		run;

		/* read item names from names data set */

		proc sql noprint;
			select count(unique(name)) into :_nitems from &names;
		quit;

		%let _nitems=&_nitems;

		proc sql noprint;
			select unique(name) into :_item1-:_item&_nitems from &names;
		quit;

		%do _i=1 %to &_nitems;
			%let _item&_i=&&_item&_i;
		%end;
	
		%let error=1;
		%do _i=1 %to &_nitems;
			%if &item=&&_item&_i %then %do; %let error=0; %end;
			%put item = &item -item&_i = &&_item&_i error=&error;
		%end;
		%if &error=1 %then %do;
			%put ERROR: &item is not in &names data set;
			%goto quit; 
		%end;

		%lirt_simu(NAMES=&names, DIM=1, NDATA=&nsimu, NPERSONS=&N, PDATA=_pdata, OUT=s);
		options nonotes;
		ods listing close;

		data _s0 /*(rename=(&id.=id))*/; set &data; dataset=0; run;

		%do _simu=1 %to &nsimu;
			data _s&_simu; set s_simu&_simu; dataset=&_simu; run;
		%end;

		data _gof; 
			set _s0-_s&nsimu; 
			score=sum(of %do _i=1 %to &_nitems; &&_item&_i %end;)-&item;
		run;
		
		proc corr data=_gof outp=_corr;
			var score &item.;
			by dataset;
		run;

		data _corr (rename=(&item.=correlation));
			set _corr (where=(_name_='score'));
		run;

		data corr_&item. (keep=dataset correlation);
			label dataset='Data set' correlation='Correlation';
			set _corr;
		run;
		proc sql;
			select correlation into :_obscorr from corr_&item where dataset=0;
		quit;
		data ztest;
			set corr_&item;
			where dataset>0;
			z=&_obscorr-correlation;
		run;
		proc means data=ztest n p5 p95;
			var correlation;
			ods output summary=_tab;
		run;
		proc means data=ztest probt;
			var z;
			ods output summary=_pval;
		run;
		data &out._GOF;
			merge _tab _pval;
			obscorr=&_obscorr;
		run;
	%end;

	/***************/
	/**** DIM=2 ****/
	/***************/

	%if &dim=2 %then %do;

		/* read item names from names data set */
		
		proc sql noprint;
			select count(distinct(name1)) 
			into :_nitems1
			from &names
			where name1^='';
		quit;
		
		proc sql noprint;
			select count(distinct(name2)) 
			into :_nitems2
			from &names
			where name2^='';
		quit;

		%let _nitems1=&_nitems1;
		%let _nitems2=&_nitems2;

		proc sql noprint;
			select distinct(name1) 
			into :_item1_1-:_item1_&_nitems1. 
			from &names;
		quit;
		
		proc sql noprint;
			select distinct(name2) 
			into :_item2_1-:_item2_&_nitems2. 
			from &names;
		quit;

		%do _i=1 %to &_nitems1;
			%let _item1_&_i=&&_item1_&_i;
		%end;
		
		%do _i=1 %to &_nitems2;
			%let _item2_&_i=&&_item2_&_i;
		%end;

		/* Find out whether ITEM (the item to be plotted) is a time 1 or time 2 item */

		data _names;
		set &names.;
		if name1="&item" then time=1;
		if name2="&item" then time=2;
		run;

		proc sql noprint;
		select max(time)
		into :time
		from _names;
		quit;

		%if &time.=. %then %do;
			%put ERROR: &item is not in &names data set;
			%goto quit; 
		%end;
		
		data _pdata2;
		set &poppar.;
		format parameter $10.;
		if parameter='mu' then parameter='mu2';
		if parameter='sigma' then parameter='sigma2';
		run;

		data _pdata1;
		format parameter $10.;
		parameter='mu1';
		estimate=0;
		output;
		parameter='sigma1';
		estimate=1;
		output;
		run;

		data _pdata;
		set _pdata1 _pdata2;
		run;

		/* Simulate from fitted model */

		%lirt_simu(NAMES=&names., DIM=2, NDATA=&nsimu, NPERSONS=&n, PDATA=_pdata, OUT=s);
		options nonotes;
		ods listing close;

		data _s0 /*(rename=(&id.=id))*/; set &data; dataset=0; run;

		%do _simu=1 %to &nsimu;
			data _s&_simu; set s_simu&_simu; dataset=&_simu; run;
		%end;

		data _gof; 
		set _s0-_s&nsimu; 
		%if &time.=1 %then %do;
			score=sum(of %do _i=1 %to &_nitems1; &&_item1_&_i %end;); 
		%end;
		%else %if &time.=2 %then %do;
			score=sum(of %do _i=1 %to &_nitems2; &&_item2_&_i %end;); 
		%end;
		run;

		proc corr data=_gof outp=_corr noprint;
			var score &item.;
			by dataset;
		run;
		
		data _corr (rename=(&item.=correlation));
			set _corr (where=(_name_='score'));
		run;

		data corr_&item. (keep=dataset correlation);
			label dataset='Data set' correlation='Correlation';
			set _corr;
		run;
	%end;
%end;

/******************************************************************************/
/************ deleting temporary data sets generated by the macro *************/
/******************************************************************************/

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

proc datasets nodetails nolist; 
	delete %do k=1 %to &_ndata.; &&_data&k %end; _datasets;
run;
quit;


proc datasets nodetails nolist; 
	delete s_simu1-s_simu&NSIMU;
run;
quit;
ods listing;
ods html;

%if %upcase(&fittest.)=Y %then %do;
	
	data &out._GOF; 
		set &out._GOF; 
		drop z_Probt;
	run
	title "&out: data set &data";
	proc print data=&out._GOF noobs; 
	run;
%end;
%mend LIRT_GOF;
