* read data;
filename cos URL 'https://raw.githubusercontent.com/KarlBangChristensen/LIRT/master/cosdata.csv';

PROC IMPORT OUT= WORK.sleep1_2 
            DATAFILE= cos
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/* Include macros */

filename lirt_m url 'http://192.38.117.59/~mola/lirt_mml.sas'; 
%include lirt_m;

filename lirt_p url 'http://192.38.117.59/~mola/lirt_ppar.sas'; 
%include lirt_p;

filename lirt_i url 'http://192.38.117.59/~mola/lirt_icc.sas'; 
%include lirt_i;

filename lirt_s url 'http://192.38.117.59/~mola/lirt_simu.sas'; 
%include lirt_s;

filename lirt_s url 'http://192.38.117.59/~mola/lirt_split.sas'; 
%include lirt_s;


/******************************************************************/
/******  Section 5.1: Separate analyses for each time point  ******/
/******************************************************************/


data names1;
input name $ max disc_yn $;
datalines;
awake1   3  Y
fallasl1 3  Y
sleepba1 3  Y
wokenup1 3  Y
;
run;

%lirt_mml(  DATA=sleep1_2, 
            NAMES=names1, 
            DIM=1, 
            OUT=gpcm1);

data names2;
	set names1;
	disc_yn='N';
run;

%lirt_mml(  DATA=sleep1_2, 
            NAMES=names2, 
            DIM=1, 
            OUT=pcm1);

proc sql;
	select Value into :_ll_pcm from pcm1_logl;
	select Value into :_ll_gpcm from gpcm1_logl;
quit;

data _lrt;
	lrt=&_ll_pcm-&_ll_gpcm; 
	df=3; 
	p=1-cdf('chisquared',lrt,df);
run;

proc print data=_lrt round noobs; 
run;

%lirt_icc(  NAMES=gpcm1_names, 
            DIM=1, 
            OUT=icc1);

%lirt_ppar( DATA=sleep1_2 (where=(idnr<110011)), 
            NAMES=gpcm1_names, 
            ID=idnr,
            DIM=1, 
            OUT=pp1);


/**************** Tests of fit *****************/

%let it1=awake1 fallasl1 sleepba1 wokenup1;

data sleep1_2;
	set sleep1_2;
	score1=sum(of &it1);
run;

proc means data=sleep1_2;
	var awake1;
	class score1;
	output out=means mean=mean;
run;

goptions reset=all;

axis1 order=0 to 12 by 1 value=(H=2) minor=NONE label=(H=2);
axis2 value=(H=2) minor=NONE label=(H=2 A=90);

proc gplot data=means;
	plot mean*score1 / haxis=axis1 vaxis=axis2;
	symbol v=none i=join w=3 l=1 color=black;
run;

ods listing close; 

goptions reset=all;

data pdata;
	input PARAMETER $ ESTIMATE;
	datalines;
	mu 0
	sigma 1
	;
run;

%lirt_simu( NAMES=gpcm1_names, 
            DIM=1, 
            NDATA=4, 
            NPERSONS=1289, 
            PDATA=pdata, 
            OUT=s);

data gof1;
	set s_simu1;
	score1=sum(of &it1);
run;
 
proc means data=gof1;
	var awake1;
	class score1;
	output out=means mean=mean;
run;

ods listing;
goptions reset=all;

axis1 order=0 to 12 by 1 value=(H=2) minor=NONE label=(H=2);
axis2 value=(H=2) minor=NONE label=(H=2 A=90);

proc gplot data=means;
	plot mean*score1 / haxis=axis1 vaxis=axis2;
	symbol v=none i=join w=3 l=33 color=grey;
run;

ods listing close;
goptions reset=all;


data s0; set sleep1_2; dataset=0; run;
data s1; set s_simu1; dataset=1; score1=sum(of &it1); run;
data s2; set s_simu2; dataset=2; score1=sum(of &it1); run;
data s3; set s_simu3; dataset=3; score1=sum(of &it1); run;
data s4; set s_simu4; dataset=4; score1=sum(of &it1); run;
data gof; set s0-s4; run;

proc means data=gof;
	var awake1;
	class score1 dataset;
	output out=means mean=mean;
run;

ods listing;
goptions reset=all;

axis1 order=0 to 12 by 1 value=(H=2) minor=NONE label=(H=2);
axis2 value=(H=2) minor=NONE label=(H=2 A=90);

proc gplot data=means(where=(dataset ne .));
	plot mean*score1 = dataset / haxis=axis1 vaxis=axis2;
	symbol1 v=none i=join w=3 l=1 color=black;
	symbol2 v=none i=join w=3 l=33 color=grey r=4;
run;

ods listing close;
goptions reset=all;


/******************************************************************/
/***************  Section 5.2: Longitudinal analysis  *************/
/******************************************************************/

data names3;
	input name1 $ name2 $ max disc_yn $;
	datalines;
	awake1   awake2   3 Y
	fallasl1 fallasl2 3 Y
	sleepba1 sleepba2 3 Y
	wokenup1 wokenup2 3 Y
	;
run;

%lirt_mml(  DATA=sleep1_2, 
            NAMES=names3, 
            DIM=2, 
            OUT=lgpcm );

%lirt_ppar( DATA=sleep1_2 (where=(idnr<110011)), 
            NAMES=lgpcm_names, 
            DIM=2, 
            ID=idnr,
            OUT=pp );

%lirt_icc(  NAMES=lgpcm_names, 
            DIM=2, 
            OUT=icc);


/*****************************************************************/
/****  Section 5.3: Models allowing for item parameter drift  ****/
/*****************************************************************/
	
data names4;
	input name1 $ name2 $ max disc_yn $;
	datalines;
	awake1   .        3 Y
	.        awake2   3 Y
	fallasl1 fallasl2 3 Y
	sleepba1 sleepba2 3 Y
	wokenup1 wokenup2 3 Y
	;
run;

%lirt_mml(  DATA=sleep1_2, 
            NAMES=names4, 
            DIM=2, 
            OUT=drift);


/********************************************************************************/
/****  Section 5.4: Models allowing for local dependence across time points  ****/
/********************************************************************************/

data names3;
	set names3;
	if name2='awake2' then name2='aw2';
run;

data sleep1_2;
	set sleep1_2;
	aw2=awake2;
run;

%lirt_split( DATA=sleep1_2, 
             NAMES=names3, 
             DIM=2,
             INDEP=awake1, 
             DEP=aw2);

data names5;
	input name1 $ name2 $ max disc_yn $;
	datalines;
	awake1      .         3  Y
	.           cat0_aw2  3  Y
	.           cat1_aw2  3  Y
	.           cat2_aw2  3  Y
	.           cat3_aw2  3  Y
	fallasl1    fallasl2  3  Y
	sleepba1    sleepba2  3  Y
	wokenup1    wokenup2  3  Y
	;
run;

%lirt_mml(  DATA=sleep1_2_split, 
            NAMES=names5, 
            DIM=2, 
            OUT=split);


/*********************************************************************************************************/
/****  Section 5.5: Models allowing for item parameter drift and local dependence across time points  ****/
/*********************************************************************************************************/

data names6;
	input name1 $ name2 $ max disc_yn $;
	datalines;
	awake1   .        3 Y
	.        cat0_aw2 3 Y
	.        cat1_aw2 3 Y
	.        cat2_aw2 3 Y
	.        cat3_aw2 3 Y
	fallasl1 fallasl2 3 Y
	sleepba1 .        3 Y
	.        sleepba2 3 Y
	wokenup1 wokenup2 3 Y
	;
run;


/*************************************************/
/**********  Section 5.6: Tests of fit  **********/
/*************************************************/

data lgpcm_names;
	set lgpcm_names;
	LD_GROUP=.;
	LD_ITEM='';
run;

data pdata;
	input PARAMETER $ ESTIMATE;
	datalines;
	mu1 0
	mu2 -0.3428
	sigma1 1
	sigma2 1.1219
	rho 0.8279
	;
run;

%lirt_simu( NAMES=lgpcm_names, 
            DIM=2, 
            NDATA=4, 
            NPERSONS=1289, 
            PDATA=pdata, 
            OUT=s);


