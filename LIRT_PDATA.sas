*;
%macro LIRT_PDATA(OUTDATA, DIM, MEAN1, MEAN2, VAR1, VAR2, RHO);

options nomprint nonotes;

%if &dim.=1 %then %do;

	data &outdata.;
	format parameter $6. estimate 7.4;
	parameter='mu'; estimate=&mean1.; output;
	parameter='sigma'; estimate=&var1.; output;
	run;

%end;
%else %if &dim.=2 %then %do;

	data &outdata.;
	format parameter $6. estimate 7.4;
	parameter='mu1'; estimate=&mean1.; output;
	parameter='mu2'; estimate=&mean2.; output;
	parameter='sigma1'; estimate=&var1.; output;
	parameter='sigma2'; estimate=&var2.; output;
	parameter='rho'; estimate=&rho.; output;
	run;

%end;

options notes;

%mend;
