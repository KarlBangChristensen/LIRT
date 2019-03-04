/****************************************************************************

LIRT_MML: a SAS macro that can be used to fit longitudinal polytomous IRT 
models (1 or 2 time points) using marginal maximum likelihood estimation. It 
can accommodate both item drift and local dependence across time points. Call
macro using

%LIRT_MML(data, names, dim, out, delete=Y);

where

DATA: is the data set with each item represented containing the items (scored 0,1, 
.. ,'max'), where the number of response categories 'max' can differ between items 
(within time point).  
	  
NAMES:  is a data set that contains information about the items. This data set 
should contain the variables 

	   (when dim=1)

	   name: names of items (should correspond to variable names in DATA)
	   max: maximum score on items 
 
	   (when dim=2)

	   name1: item names af time 1 items (should correspond to variable names in DATA)
	   name2: item names af time 2 items (should correspond to variable names in DATA)
	   max: maximum score on items
	   disc_yn: variable taking on value N if item is 1PL and Y if item is 2PL. 

DIM: is the dimension of latent variable (number of time points).

OUT: prefix of output data sets

	data set OUT_names 
	data set OUT_disc
	data set OUT_disc_std 
	data set OUT_ipar
	data set OUT_thres
	data set OUT_poppar
	data set OUT_logl
	data set OUT_conv

DELETE: indicator telling macro to delete temporary data sets
**************************************************************************/

%macro LIRT_MML2(data, 
		names, 
		dim, 
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
	select distinct(name), max, disc_yn
	into :item1-:item&_nitems., 
		 :max1-:max&_nitems., 
		 :d1-:d&_nitems.
	from &names.;
	quit;
	
	* only used for initial values in parms statement;
	proc sql noprint;
	select sum((disc_yn^='Y'))
	into :disc
	from &names.;
	quit;

	/*******************************************/
	/* start of item parameter estimation part */
	/*******************************************/
	proc IRT data=&data.;
		ods output IRT.Optimization.ConvergenceStatus=&out._conv;
		ods output IRT.FitStatistics.FitStatistics=&out._logl;
		ods output IRT.EstimationResults.ParameterEstimates=_item_parameters;
		var %do _i=1 %to &_nitems.; &&item&_i %end;;
		model %do _i=1 %to &_nitems.; &&item&_i %end;/resfunc=gpc;
	run;
	data &out._disc &out._thres;  
		set _item_parameters(drop=ProbT);
		score=substr(Parameter,6,1)*1;
		lower=estimate-1.96*stderr;
		upper=estimate+1.96*stderr;
		if parameter='Slope' then output &out._disc; 
		else output &out._thres;
	run;
	%do _i=1 %to &_nitems;
		proc sql;
			select estimate into :it&_i._thres1-:it&_i._thres&&max&_i
			from &out._thres
			where item="&&item&_i";
		quit;
	%end;
	%do _i=1 %to &_nitems;
		%put it&_i._thres1 er 999;
		%put it&_i._thres&&max&_i er 999;
	%end;
	data &out._ipar;
		set &out._thres;
		%do _i=1 %to &_nitems;
			if item="&&item&_i" then do;
				%do _h=1 %to &&max&_i;
					if score=&_h then ipar=0 %do _k=1 %to &_h; -&&it&_i._thres&_h. %end;;
				%end;
			end;
		%end;
		drop estimate stderr lower upper;
	run;
	data _names;
		set &names.;
		label score='Score';
		order=_n_;
		do score=0 to max;
			output;
		end;
	run;
	proc sql;
		create table _ipar1 as select a.*,
		b.ipar as ipar label='Item parameter'
		from _names a left join &out._ipar b
		on a.name=b.item and a.score=b.score;
	quit; 
	proc sql;
		create table _ipar2 as select a.*,
		b.estimate as thres label='Item threshold'
		from _ipar1 a left join &out._thres b
		on a.name=b.item and a.score=b.score;
	quit;
	proc sql;
		create table _ipar3 as select a.*,
		b.estimate as disc label='Item discrimination'
		from _ipar2 a left join &out._disc b
		on a.name=b.item;
	quit;
	data _ipar3;
		set _ipar3;
		if score=0 then do;
			ipar=0;
			thres=0;
		end;
	run;
	proc sort data=_ipar3 out=&out._names (drop=order disc_yn);
		by order score;
	run;
%end;
%else %if &dim.=2 %then %do;
	data _items _items1 _items2;
	set &names.;
	if name1^=' ' and  name2^=' ' then output _items;
	else if name1^=' ' then output _items1;
	else if name2^=' ' then output _items2;
	run;

	proc sql noprint;
	select sum((disc_yn^='Y'))
	into :disc
	from &names.;
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
	select distinct(name1), name2, max, disc_yn
	into :item1_1-:item1_&_nitems., 
		 :item2_1-:item2_&_nitems.,  
		 :max1-:max&_nitems., 
		 :d1-:d&_nitems.
	from _items;
	quit;

	%if &rec1>0 %then %do;

		proc sql noprint;
		select distinct(name1), max, disc_yn
		into :item1_%eval(&_nitems.+1)-:item1_%eval(&_nitems.+&_nitems1.), 
			 :max%eval(&_nitems.+1)-:max%eval(&_nitems.+&_nitems1.),	 
			 :d%eval(&_nitems.+1)-:d%eval(&_nitems.+&_nitems1.)
		from _items1;
		quit;

	%end;
	%if &rec2>0 %then %do;

		proc sql noprint;
		select distinct(name2), max, disc_yn
		into :item2_%eval(&_nitems.+&_nitems1.+1)-:item2_%eval(&_nitems.+&_nitems1.+&_nitems2.),
			 :max%eval(&_nitems.+&_nitems1.+1)-:max%eval(&_nitems.+&_nitems1.+&_nitems2.),
			 :d%eval(&_nitems.+&_nitems1.+1)-:d%eval(&_nitems.+&_nitems1.+&_nitems2.)
		from _items2;
		quit;

	%end;

	data _new; 
	format value 3. item $20.;
	set &data.; 
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

	/*******************************************/
	/* start of item parameter estimation part */
	/*******************************************/

	/* direct output to files */
	ods output nlmixed.ConvergenceStatus=&out._conv;
	ods output nlmixed.parameterestimates=_item_parameters1;
	ods output nlmixed.additionalestimates=_item_parameters2;
	ods output nlmixed.fitstatistics=_logl;

	/* numerical maximization using PROC NLMIXED */
	proc nlmixed data=_new;
	parms 
	mu=0,
	rho=.5,
	sigma=1
	/* Items with no item drift */
	%do _i=1 %to &_nitems.; 
		%if &&d&_i=Y %then %do;
			%do _h=1 %to &&max&_i; 
				,eta1_&_i._&_h.=0 	
			%end; 
		%end;
		%else %if &&d&_i^=Y %then %do;
			%do _h=1 %to &&max&_i; 
				,eta1_&_i._&_h.=0 
			%end; 
		%end;
	%end;
	%if &rec1>0 %then %do;
		/* Time 1, item drift */
		%do _i=%eval(&_nitems.+1) %to %eval(&_nitems.+&_nitems1.);
			%if &&d&_i=Y %then %do;
				,alpha1_&_i.=1 	
				%do _h=1 %to &&max&_i; 
					,eta1_&_i._&_h.=0
				%end; 
			%end;
			%else %if &&d&_i^=Y %then %do;
				%do _h=1 %to &&max&_i; 
					,eta1_&_i._&_h.=0 
				%end; 
			%end;
		%end;
	%end;
	%if &rec2>0 %then %do;
		/* Time 2, item drift */
		%do _i=%eval(&_nitems.+&_nitems1.+1) %to %eval(&_nitems.+&_nitems1.+&_nitems2.);
			%if &&d&_i=Y %then %do;
				,alpha2_&_i.=1 	
				%do _h=1 %to &&max&_i; 
					,eta2_&_i._&_h.=0 	
				%end;
			%end;
			%else %if &&d&_i^=Y %then %do;
				%do _h=1 %to &&max&_i; 
					,eta2_&_i._&_h.=0 
				%end;
			%end; 
		%end;
	%end;
	%if &disc.>0 %then %do;
		,alpha=1
	%end;; 
	%do _i=1 %to &_nitems.; 
		%if &&d&_i=Y %then %do;		
			_denom=1 %do _k=1 %to &&max&_i; 
				+exp(alpha1_&_i.*(&_k*_theta1+eta1_&_i._&_k.)) 
			%end;;
			if item="&&item1_&_i" and value=0 then ll=-log(_denom);
			%do _h=1 %to &&max&_i;
				if item="&&item1_&_i" and value=&_h then ll=alpha1_&_i.*(&_h*_theta1+eta1_&_i._&_h.)-log(_denom);
			%end; 
		%end;
		%else %if &&d&_i^=Y %then %do;		
			_denom=1 %do _k=1 %to &&max&_i; 
				+exp(alpha*(&_k*_theta1+eta1_&_i._&_k.)) 
			%end;;
			if item="&&item1_&_i" and value=0 then ll=-log(_denom);
			%do _h=1 %to &&max&_i;
				if item="&&item1_&_i" and value=&_h then ll=alpha*(&_h*_theta1+eta1_&_i._&_h.)-log(_denom);
			%end; 
		%end;
	%end;
	%do _i=1 %to &_nitems.;  
		%if &&d&_i=Y %then %do;		
			_denom=1 %do _k=1 %to &&max&_i; 
				+exp(alpha1_&_i.*(&_k*_theta2+eta1_&_i._&_k.)) 
			%end;;
			if item="&&item2_&_i" and value=0 then ll=-log(_denom);
			%do _h=1 %to &&max&_i;
				if item="&&item2_&_i" and value=&_h then ll=alpha1_&_i.*(&_h*_theta2+eta1_&_i._&_h.)-log(_denom);
			%end; 
		%end;
		%else %if &&d&_i^=Y %then %do;		
			_denom=1 %do _k=1 %to &&max&_i; 
				+exp(alpha*(&_k*_theta2+eta1_&_i._&_k.)) 
			%end;;
			if item="&&item2_&_i" and value=0 then ll=-log(_denom);
			%do _h=1 %to &&max&_i;
				if item="&&item2_&_i" and value=&_h then ll=alpha*(&_h*_theta2+eta1_&_i._&_h.)-log(_denom);
			%end; 
		%end;
	%end;
	%if &rec1>0 %then %do;		
		%do _i=%eval(&_nitems.+1) %to %eval(&_nitems.+&_nitems1.); 
			%if &&d&_i=Y %then %do;	
				_denom=1 %do _k=1 %to &&max&_i; 
					+exp(alpha1_&_i.*(&_k*_theta1+eta1_&_i._&_k.)) 
				%end;;
				if item="&&item1_&_i" and value=0 then ll=-log(_denom);
				%do _h=1 %to &&max&_i;
					if item="&&item1_&_i" and value=&_h then ll=alpha1_&_i.*(&_h*_theta1+eta1_&_i._&_h.)-log(_denom);
				%end; 
			%end;
			%if &&d&_i^=Y %then %do;	
				_denom=1 %do _k=1 %to &&max&_i; 
					+exp(alpha*(&_k*_theta1+eta1_&_i._&_k.)) 
				%end;;
				if item="&&item1_&_i" and value=0 then ll=-log(_denom);
				%do _h=1 %to &&max&_i;
					if item="&&item1_&_i" and value=&_h then ll=alpha*(&_h*_theta1+eta1_&_i._&_h.)-log(_denom);
				%end; 
			%end;
		%end; 
	%end;
	%if &rec2>0 %then %do;		
		%do _i=%eval(&_nitems.+&_nitems1.+1) %to %eval(&_nitems.+&_nitems1.+&_nitems2.); 
			%if &&d&_i=Y %then %do;	
				_denom=1 %do _k=1 %to &&max&_i; 
					+exp(alpha2_&_i.*(&_k*_theta2+eta2_&_i._&_k.)) 
				%end;;
				if item="&&item2_&_i" and value=0 then ll=-log(_denom);
				%do _h=1 %to &&max&_i;
					if item="&&item2_&_i" and value=&_h then ll=alpha2_&_i.*(&_h*_theta2+eta2_&_i._&_h.)-log(_denom);
				%end; 
			%end;
			%if &&d&_i^=Y %then %do;	
				_denom=1 %do _k=1 %to &&max&_i; 
					+exp(alpha*(&_k*_theta2+eta2_&_i._&_k.)) 
				%end;;
				if item="&&item2_&_i" and value=0 then ll=-log(_denom);
				%do _h=1 %to &&max&_i;
					if item="&&item2_&_i" and value=&_h then ll=alpha*(&_h*_theta2+eta2_&_i._&_h.)-log(_denom);
				%end; 
			%end;
		%end;
	%end;
	model value~general(ll);
	random _theta1 _theta2 ~ normal([0,mu],[1,rho*sigma,sigma*sigma]) subject=person;	
	%do _i=1 %to &_nitems.; 
		%if &&d&_i=Y %then %do;
			estimate "&&item1_&_i. (discrimination)" alpha1_&_i.;
			estimate "&&item2_&_i. (discrimination)" alpha1_&_i.;
		%end;  
		%if &&d&_i^=Y %then %do;
			estimate "&&item1_&_i. (discrimination)" alpha;
			estimate "&&item2_&_i. (discrimination)" alpha;
		%end; 
		%do _h=1 %to &&max&_i; 
			estimate "&&item1_&_i.|&_h. (threshold)" -eta1_&_i._&_h. %if &_h.>1 %then %do; +eta1_&_i._%eval(&_h.-1) %end;; 
			estimate "&&item2_&_i.|&_h. (threshold)" -eta1_&_i._&_h. %if &_h.>1 %then %do; +eta1_&_i._%eval(&_h.-1) %end;; 
			estimate "&&item1_&_i.|&_h." eta1_&_i._&_h.; 
			estimate "&&item2_&_i.|&_h." eta1_&_i._&_h.;
		%end; 
	%end;
	%if &rec1>0 %then %do;
		%do _i=%eval(&_nitems.+1) %to %eval(&_nitems.+&_nitems1.); 
			%if &&d&_i=Y %then %do;
				estimate "&&item1_&_i. (discrimination)" alpha1_&_i.;
			%end; 
			%if &&d&_i^=Y %then %do;
				estimate "&&item1_&_i. (discrimination)" alpha;
			%end; 
			%do _h=1 %to &&max&_i; 
				estimate "&&item1_&_i.|&_h. (threshold)" -eta1_&_i._&_h. %if &_h.>1 %then %do; +eta1_&_i._%eval(&_h.-1) %end;;
				estimate "&&item1_&_i.|&_h. " eta1_&_i._&_h.;
			%end; 
		%end;
	%end;
	%if &rec2>0 %then %do;
		%do _i=%eval(&_nitems.+&_nitems1.+1) %to %eval(&_nitems.+&_nitems1.+&_nitems2.); 
			%if &&d&_i=Y %then %do;
				estimate "&&item2_&_i.|&_h. (discrimination)" alpha2_&_i.;
			%end; 
			%if &&d&_i^=Y %then %do;
				estimate "&&item2_&_i.|&_h. (discrimination)" alpha;
			%end; 
			%do _h=1 %to &&max&_i; 
				estimate "&&item2_&_i.|&_h. (threshold)" -eta2_&_i._&_h. %if &_h.>1 %then %do; +eta2_&_i._%eval(&_h.-1) %end;;
				estimate "&&item2_&_i.|&_h." eta2_&_i._&_h.;
			%end; 
		%end;
	%end;
	run;

	data &out._logl;
	set _logl;
	where Descr='-2 Log Likelihood';
	run;

	* data set where parameters are linked to 'name';
	proc sql;
	create table &out._poppar as select
	parameter label='Parameter',
	estimate,
	StandardError,
	Lower,
	Upper
	from _item_parameters1
	where parameter in ('mu' 'rho' 'sigma');
	quit;

	data _item_thresholds (drop=label) _item_discriminations (drop=label score) _item_parameters (drop=label);  
	set _item_parameters2;
	parameter=scan(label,1,'(');
	item=scan(parameter,1,'|');
	if scan(label,-1,'(')='threshold)' then do;
		score=scan(parameter,-1,'|')*1; 
		output _item_thresholds; 
	end;
	else if scan(label,-1,'(')='discrimination)' then output _item_discriminations; 
	else do;
		score=scan(parameter,-1,'|')*1; 
		output _item_parameters;
	end;
	run;
	
	data _names_temp;
	set &names.;
	format name $30.;
	order=_n_;
	if name1^='' and name2='' then do;
		name=name1;
		orderx=1;
		output;
	end;
	if name2^='' and name1='' then do;
		name=name2;
		orderx=1;
		output;
	end;
	if name1^='' and name2^='' then do;
		name=name1;
		orderx=1;
		output;
		name=name2;
		orderx=2;
		output;
	end;
	run;

	proc sql;
	create table _thres_temp1 as select 
	a.*
	from _item_thresholds a left join _names_temp b
	on a.item=b.name
	order by order, orderx, score;
	quit; 

	proc sql;
	create table &out._thres as select 
	Parameter label='Parameter',
	Estimate,
	StandardError,
	Lower,
	Upper
	from _thres_temp1;	
	quit;

	proc sql;
	create table _ipar_temp1 as select 
	a.*
	from _item_parameters a left join _names_temp b
	on a.item=b.name
	order by order, orderx, score;
	quit; 

	proc sql;
	create table &out._ipar as select 
	Parameter label='Parameter',
	Estimate,
	StandardError,
	Lower,
	Upper
	from _ipar_temp1;
	quit;
	
	proc sql;
	create table &out._disc as select 
	Parameter label='Parameter',
	Estimate,
	StandardError,
	Lower,
	Upper
	from _item_discriminations
	order by item;
	quit;
	
	data _names;
	set &names.;
	order=_n_;
	do score=0 to max;
		output;
	end;
	run;

	proc sql;
	create table _ipar1 as select a.*,
	b.estimate as ipar1
	from _names a left join _item_parameters b
	on a.name1=b.item and a.score=b.score;
	quit;
	
	proc sql;
	create table _ipar2 as select a.*,
	b.estimate as ipar2
	from _ipar1 a left join _item_parameters b
	on a.name2=b.item and a.score=b.score;
	quit;

	proc sql;
	create table _ipar3 as select a.*,
	b.estimate as threshold1
	from _ipar2 a left join _item_thresholds b 
	on a.name1=b.item and a.score=b.score;
	quit;
		
	proc sql;
	create table _ipar4 as select a.*,
	b.estimate as threshold2
	from _ipar3 a left join _item_thresholds b 
	on a.name2=b.item and a.score=b.score;
	quit;

	proc sql;
	create table _ipar5 as select a.*,
	b.estimate as discrimination1
	from _ipar4 a left join _item_discriminations b 
	on a.name1=b.item;
	quit;
		
	proc sql;
	create table _ipar6 as select a.*,
	b.estimate as discrimination2
	from _ipar5 a left join _item_discriminations b 
	on a.name2=b.item;
	quit;
	
	data _names1 (drop=ipar1 ipar2 discrimination1 discrimination2 threshold1 threshold2 disc_yn);
	set _ipar6;
	label ipar='Item parameter' thres='Item threshold' disc='Item discrimination';
	ipar=max(ipar1,ipar2);
	thres=max(threshold1, threshold2);
	disc=max(discrimination1, discrimination2);
	if score=0 then do;
		ipar=0;
		thres=0;
	end;
	run;
	
	proc sort data=_names1 out=&out._names (drop=order);
	by order score;
	run;

	data _names_t1 (rename=(name1=name) drop=name2);
	set &out._names (where=(name1^=''));
	run;
	
	proc sql;
	create table _out1_t1 as select 
	distinct(name),
	disc as estimate label='Estimate',
	strip(name)||' (discrimination)' as parameter
	from _names_t1;
	quit;
	
	proc sql noprint;
	select count(distinct(name))
	into :_nitems_t1
	from _names_t1;
	quit;
	
	proc sql;
	create table _out2_t1 as select *,
	estimate/(exp(sum(log(estimate))))**(1/&_nitems_t1.) as estimate_std label='Standardized estimate',
	(exp(sum(log(estimate))))**(1/&_nitems_t1.) as weight
	from _out1_t1;
	quit;
	proc sql noprint;
	select distinct(weight)
	into :sigma1
	from _out2_t1;
	quit;

	data _sigma_t1;
	parameter='Sigma1';
	estimate=1;
	estimate_std=&sigma1.;
	run;

	data _disc_std_t1;
	format estimate estimate_std 7.4;
	set _out2_t1 (drop=weight) _sigma_t1;
	run; 
	
	proc sql;
	create table &out._disc_std as select 
	parameter label='Parameter',
	estimate,
	estimate_std
	from _disc_std_t1;
	quit;
	
	data _pardata1 (rename=(name1=name) drop=name2) _pardata2 (rename=(name2=name) drop=name1);
	set &out._names;
	if name1^='' then output _pardata1;
	if name2^='' then output _pardata2;
	run;

	data &out._pardata;
	set _pardata1 _pardata2;
	run;

%end;
* delete temporary data sets;
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
%mend LIRT_MML2;
