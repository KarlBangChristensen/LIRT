* SAS statements that include the LIRT-macros from GitHub;
%let url=https://raw.githubusercontent.com/KarlBangChristensen/LIRT/master;

filename gof url "&url/ LIRT_GOF.sas"; %include gof;
filename icc url "&url/LIRT_ICC.sas"; %include icc;
filename mml url "&url/LIRT_MML.sas"; %include mml;
filename name url "&url/LIRT_MML_NAMES.sas"; %include Name;
filename pdat url "&url/LIRT_PDATA.sas"; %include pdat;
filename ppar url "&url/LIRT_PPAR.sas"; %include ppar;
filename simu url "&url/LIRT_SIMU.sas"; %include simu;
filename s_na url "&url/LIRT_SIMU_NAMES.sas"; %include s_na; 
filename spl url "&url/LIRT_SPLIT.sas"; %include spl; 
