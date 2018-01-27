




/*this function is for character variables*/

%macro write_to_one_wb(f_v,remain_v);


%if (&f_v.=&remain_v.) or (&f_v.^= and &remain_v.=) %then %do;

%local table_name;
%let table_name=%scan(&in_data.,2,%str(.));

ods listing close;
ods tagsets.excelxp file="&outpath.\&table_name._DS.xls" style=Printer options(SHEET_NAME="&f_v." sheet_interval='none' 
absolute_column_width='12, 12, 12'
EMBEDDED_TITLES="yes" EMBEDDED_FOOTNOTES="yes");


title "Table Name: &table_name. / Variable Name: &f_v. --  General Statistics";

proc sql;
select Statistics, value as Value, percentage as Percentage from desc_stats_&f_v.;quit;



/*%if %sysfunc(open(otr_misdet_&f_v.,is))^=0 %then %do;*/
%let ds_flag=%sysfunc(open(otr_misdet_&f_v.,is));

%if &ds_flag.^=0 %then %do;

title "Supplementary table for other types of missing values that are found";

proc sql;
select &f_v. as Value, frequency as Count, percentage as Percentage from otr_misdet_&f_v.;quit;

%let ds_close = %sysfunc(close(&ds_flag.));  

proc datasets library=work nolist nodetails;
delete otr_misdet_&f_v.;
run;quit;

%end;

title "Table Name: &table_name. / Variable Name: &f_v.  --  Top ten most frequent values: ";
proc sql;
select &f_v. as Value, Frequency as Freq_Of_Occurrence from top10_&f_v.;quit;

title "Table Name: &table_name. / Variable Name: &f_v.  --  Top ten least frequent values: ";
proc sql;
select &f_v. as Value, Frequency as Freq_Of_Occurrence from bottom_&f_v.; quit;
title "";

ods tagsets.excelxp close;
ods listing;

%end;

%else %if &f_v.^=&remain_v. %then %do;

%local table_name;
%let table_name=%scan(&in_data.,2,%str(.));

/*start of first worksheet*/
ods listing close;
ods tagsets.excelxp file="&outpath.\&table_name._DS.xls" style=Printer options(SHEET_NAME="&f_v." sheet_interval='none' 
absolute_column_width='12, 12, 12'
EMBEDDED_TITLES="yes" EMBEDDED_FOOTNOTES="yes");


title "Table Name: &table_name. / Variable Name: &f_v. --  General Statistics";

proc sql;
select Statistics, value as Value, percentage as Percentage from desc_stats_&f_v.;quit;

/*%if %sysfunc(open(otr_misdet_&f_v.,is))^=0 %then %do;*/
%let ds_flag=%sysfunc(open(otr_misdet_&f_v.,is));

%if &ds_flag.^=0 %then %do;

title "Supplementary table for other types of missing values that are found";

proc sql;
select &f_v. as Value, frequency as Count, percentage as Percentage from otr_misdet_&f_v.;quit;

%let ds_close = %sysfunc(close(&ds_flag.));  

proc datasets library=work nolist nodetails;
delete otr_misdet_&f_v.;
run;quit;

%end;


title "Table Name: &table_name. / Variable Name: &f_v.  --  Top ten most frequent values: ";
proc sql;
select &f_v. as Value, Frequency as Freq_Of_Occurrence from top10_&f_v.;quit;

title "Table Name: &table_name. / Variable Name: &f_v.  --  Top ten least frequent values: ";
proc sql;
select &f_v. as Value, Frequency as Freq_Of_Occurrence from bottom_&f_v.; quit;
title "";
/*end of frist worksheet*/

/*continue next couple of worksheets*/

    %let _i=1;
    %do %while(not(%qscan(&remain_v.,&_i,%str(, ))=));

	%let temp_v=%scan(&remain_v.,&_i,%str(, ));


	ods tagsets.excelxp options(sheet_interval='none' sheet_name="&temp_v.");

	title "Table Name: &table_name. / Variable Name: &temp_v. --  General Statistics";

	proc sql;
	select Statistics, value as Value, percentage as Percentage from desc_stats_&temp_v.;quit;

	/*%if %sysfunc(open(otr_misdet_&f_v.,is))^=0 %then %do;*/
	%let ds_flag=%sysfunc(open(otr_misdet_&temp_v.,is));

	%if &ds_flag.^=0 %then %do;

	title "Supplementary table for other types of missing values that are found";

	proc sql;
	select &temp_v. as Value, frequency as Count, percentage as Percentage from otr_misdet_&temp_v.;quit;

	%let ds_close = %sysfunc(close(&ds_flag.));  

	proc datasets library=work nolist nodetails;
	delete otr_misdet_&temp_v.;
	run;quit;

	%end;

	title "Table Name: &table_name. / Variable Name: &temp_v.  --  Top ten most frequent values: ";
	proc sql;
	select &temp_v. as Value, Frequency as Freq_Of_Occurrence from top10_&temp_v.;quit;

	title "Table Name: &table_name. / Variable Name: &temp_v.  --  Top ten least frequent values: ";
	proc sql;
	select &temp_v. as Value, Frequency as Freq_Of_Occurrence from bottom_&temp_v.; quit;
	title "";


	%let _i=%eval(&_i+1);
    %end;

	ods tagsets.excelxp close;
	ods listing;

%end;


%mend write_to_one_wb;


/*%macro process_char(in_data, cv, missing_value, is_id, is_time, is_convert);*/
%macro process_char(in_data, cv, missing_value, cv_type);

/*
cv_type:
1. CHAR -- not convertible to numbers;
2. DATE -- needs to be converted to dates; 
3. NUM  -- convertible to numbers;
*/

%local table_name;
%let table_name=%scan(&in_data.,2,%str(.));


/*===========General Statistics===========*/

proc sql;
select count(*) into :total_rec from  &in_data.;

%if &missing_value.^= %then %do;
select count(*) into :total_available from &in_data. where lowcase(&cv.) not in &missing_value. and &cv.^="";
%end;

%else %do;
select count(*) into :total_available from &in_data. where &cv.^="";
%end;

select count(*) into :total_missing from &in_data. where &cv.="" ;

%if &missing_value.^= %then %do;
select count(*) into : total_other_missing from &in_data. where lowcase(&cv.) in &missing_value.;
%end;

%if %upcase(&cv_type.) ^= DATE %then %do;
	select count(distinct &cv.) into :total_distinct from &in_data. where lowcase(&cv.) not in &missing_value. and &cv.^="";
	quit;
%end;


 
%if %upcase(&cv_type.)=CHAR %then %do;
		data desc_stats_&cv.;
		length Statistics $100 value $100 percentage 6.2; 
		format percentage percent8.2;


		Statistics = "Available";
		value=left(put(&total_available, 24.));
		percentage=%sysevalf(&total_available./&total_rec.) ;
		output;

		Statistics ="Missing Value";
		value="Blank";
		percentage=.;
		output;

		Statistics="Missing";
		value=left(put(&total_missing., 24.));
		percentage=%sysevalf(&total_missing./&total_rec.);
		output;

		%if &missing_value.^= %then %do;
		Statistics="All other types of Missing values";
		%if &total_other_missing.>0 %then %do;
 		value="See more details in the table below";
		%end;
		%else %do;
		value="No other missing values are found";
		%end;
		percentage=.;
		output;

		Statistics="Missing";
		value=left(put(&total_other_missing., 24.));
		percentage=%sysevalf(&total_other_missing./&total_rec.);
		output;
		%end;

		Statistics="Mean";
		value="-";
		percentage=.;
		output;

		Statistics="Median";
		value="-";
		percentage=.;
		output;

		Statistics="Range";
		value="-";
		percentage=.;
		output;

		Statistics="Distinct (Missing values not counted in)";
		value=left(put(&total_distinct.,24.));
		percentage=.;
		output;

		run;


		%if &missing_value.^= and &total_other_missing.>0 %then %do;

		

		proc sql;
		create table otr_misdet_&cv. as 
		select &cv., count(*) as frequency, count(*)/&total_rec. as percentage
		from  &in_data.
		where lowcase(&cv.) in &missing_value.
		group by &cv.;quit;

		proc sort data= otr_misdet_&cv.;
		by descending frequency;
		run;

		data otr_misdet_&cv.;
		set otr_misdet_&cv.;
		format percentage percent8.2; 
		run;


		%end;


%end;

%else %if %upcase(&cv_type.)=DATE %then %do;

/*only for time variables with specific format such as 2014-10-30 13:45:35*/
		data temp_timevar;
		set &in_data.;

		time_var=input(substr(left(&cv.), 1, 10),yymmdd10.);
		format time_var date9.;

		keep time_var;
		rename time_var=&cv.;

		run;
		
		proc sql;
		select min(&cv.) into :beginning_date from temp_timevar; 
		select max(&cv.) into :end_date from temp_timevar;
		select count(distinct &cv.) into :total_distinct from temp_timevar;
		quit;
	


		data desc_stats_&cv.;
		length Statistics $100 value $100 percentage 6.2; 
		format percentage percent8.2;

		Statistics = "Available";
		value=left(put(&total_available, 24.));
		percentage=%sysevalf(&total_available./&total_rec.) ;
		output;

		Statistics ="Missing Value";
		value="Blank";
		percentage=.;
		output;

		Statistics="Missing";
		value=left(put(&total_missing., 24.));
		percentage=%sysevalf(&total_missing./&total_rec.);
		output;

		%if &missing_value.^= %then %do;
		Statistics="All other types of Missing values";
		%if &total_other_missing.>0 %then %do;
 		value="See more details in the table below";
		%end;
		%else %do;
		value="No other missing values are found";
		%end;
		percentage=.;
		output;

		Statistics="Missing";
		value=left(put(&total_other_missing., 24.));
		percentage=%sysevalf(&total_other_missing./&total_rec.);
		output;
		%end;
 

		Statistics="Mean";
		value="-";
		percentage=.;
		output;

		Statistics="Median";
		value="-";
		percentage=.;
		output;

		Statistics="Beginning Date";
		value=put(&beginning_date.,date9.);
		percentage=.;
		output;

		Statistics="End Date";
		value=put(&end_date.,date9.);
		percentage=.;
		output;

		Statistics="Range";
		value= put(&beginning_date.,date9.) !! "--" !! put(&end_date.,date9.);
		percentage=.;
		output;

		Statistics="Distinct (Missing values not counted in)";
		value=left(put(&total_distinct.,24.));
		percentage=.;
		output;

		run;

		
/*!!!no need to report missing value on date value!!!*/


%end;
 
%else %if %upcase(&cv_type.)=NUM %then %do;

data temp_num;
set &in_data.;

length num_var 8;

if lowcase(&cv.) in &missing_value.  then &cv.="";


if &cv.^="" then do;
 
	num_var=&cv.-0;

end;
else if &cv.="" then num_var=.;

keep num_var;
rename num_var=&cv.;

run;



proc means data=temp_num noprint;
var &cv.;
output out=stats_num mean=num_var_mean median=num_var_median min=num_var_min max=num_var_max ;
run;

data _NULL_;
set stats_num;
call symput('num_var_mean',num_var_mean);
call symput('num_var_median',num_var_median);
call symput('num_var_min',num_var_min);
call symput('num_var_max',num_var_max);
run;

		data desc_stats_&cv.;
		length Statistics $100 value $100 percentage 6.2; 
		format percentage percent8.2;

		Statistics = "Available";
		value=left(put(&total_available, 24.));
		percentage=%sysevalf(&total_available./&total_rec.) ;
		output;

		Statistics ="Missing Value";
		value="Blank";
		percentage=.;
		output;

		Statistics="Missing";
		value=left(put(&total_missing., 24.));
		percentage=%sysevalf(&total_missing./&total_rec.);
		output;

		%if &missing_value.^= %then %do;
		Statistics="Other Missing Value";
		%if &total_other_missing.>0 %then %do;
 		value="See more details in the table below";
		%end;
		%else %do;
		value="No other missing values are found";
		%end;
		percentage=.;
		output;

		Statistics="Missing";
		value=left(put(&total_other_missing., 24.));
		percentage=%sysevalf(&total_other_missing./&total_rec.);
		output;
		%end;
 

		Statistics="Mean";
		value="&num_var_mean.";
		percentage=.;
		output;

		Statistics="Median";
		value="&num_var_median.";
		percentage=.;
		output;

		Statistics="Range";
		value= "&num_var_min. -- &num_var_max.";
		percentage=.;
		output;

		Statistics="Distinct (Missing values not counted in)";
		value=left(put(&total_distinct.,24.));
		percentage=.;
		output;

		run;

		%if &missing_value.^= and &total_other_missing.>0 %then %do;
	

		proc sql;
		create table otr_misdet_&cv. as 
		select &cv., count(*) as frequency, count(*)/&total_rec. as percentage
		from  &in_data.
		where lowcase(&cv.) in &missing_value.
		group by &cv.;quit;

		proc sort data= otr_misdet_&cv.;
		by descending frequency;
		run;

		data otr_misdet_&cv.;
		set otr_misdet_&cv.;
		format percentage percent8.2; 
		run;


		%end;

%end;



/*===========frequency table ========*/

%if %upcase(&cv_type.)=DATE %then %do;
 
		proc sql;
		create table Freq_tb_&cv. as
		select &cv., count(*) as Frequency from temp_timevar group by &cv.;quit;

%end;
%else %do;

		proc sql;
		create table Freq_tb_&cv. as
		select &cv., count(*) as Frequency from &in_data. group by &cv.;quit;

%end;
/*&total_missing.*/


proc sort data=Freq_tb_&cv. out=Desc_&cv._freq(keep=&cv. Frequency);
by descending Frequency ;
run;
proc sort data=Freq_tb_&cv. out=Asc_&cv._freq(keep=&cv. Frequency);
by Frequency ;
run;
data top10_&cv.;
set Desc_&cv._freq;
if _N_<=10;
run;
data bottom_&cv.;
set Asc_&cv._freq;
if _N_<=10;
run;
 



/*excel file*/
/*ods listing close;*/
/*ods html file="&outpath.\&table_name._&cv..xls";*/
/**/
/*title "Table Name: &table_name. / Variable Name: &cv. --  General Statistics";*/
/**/
/*proc sql;*/
/*select Statistics, value as Value, percentage as Percentage from desc_stats_&cv.;quit;*/
/**/
/*title "Table Name: &table_name. / Variable Name: &cv.  --  Top ten most frequent values: ";*/
/*proc sql;*/
/*select &cv. as Value, Frequency as Freq_Of_Occurrence from top10_&cv.;quit;*/
/**/
/*title "Table Name: &table_name. / Variable Name: &cv.  --  Top ten least frequent values: ";*/
/*proc sql;*/
/*select &cv. as Value, Frequency as Freq_Of_Occurrence from bottom_&cv.; quit;*/
/*title "";*/
/*ods html close;*/
/*ods listing;*/




%mend process_char;



%let missing_value=%nrstr(("not recorded","ni","unable to obtain","refused","unable","unble"
				,"not specified","unspecified","n/a","ni","n\a","n i", "unknown", "unsure of usage"
				,"0x","1x",".","'","none","u", "unclassified"));

libname lunc "F:\yxue\Luncentis\2015Q1";
%let outpath=D:\Jie\BE\result;
%include "F:\yxue\Luncentis\ForEach.sas";
options mprint;

/*=======Diagnosis table==========*/
%let in_data=Lunc.Diagnosis;
%let var_list=  encounter_id eye  icdcode  cleansed_icdcode ;
%let var_type=CHAR;

%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

/*out to one workbook*/
%let first_var=encounter_id;
%let remaining_vars= eye  icdcode  cleansed_icdcode;

%write_to_one_wb(&first_var., &remaining_vars.);

/*======!!!end of Diagnosis table=====*/



/*=======Encounter table =========*/
%let in_data=Lunc.Encounter;
%let var_list=  encounter_id  patient_id  physician_id  location_id ;
%let var_type=CHAR;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

%let var_list = encounter_date;
%let var_type=DATE;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

/*out to one workbook*/
%let first_var=encounter_id;
%let remaining_vars= encounter_date patient_id  physician_id  location_id  ;

%write_to_one_wb(&first_var., &remaining_vars.);


/*========!!!End of Encounter table*/


/*========Findings table========*/
%let in_data=lunc.findings2;
%let var_list= encounter_id segment element eye sf_name  sf_result ;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

/*out to one workbook*/
%let first_var=encounter_id;
%let remaining_vars= segment element eye sf_name  sf_result ;

%write_to_one_wb(&first_var., &remaining_vars.);

/*=======!!! End of Findings table=====*/


/*=========Iopmethods=============*/
%let in_data=lunc.Iopmethods;
%let var_list = encounter_id  method  odValue  osValue;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

/*out to one workbook*/
%let first_var=encounter_id;
%let remaining_vars= method  odValue  osValue;

%write_to_one_wb(&first_var., &remaining_vars.);



/*=======!!! End of Iopmethods========*/




/*========Location table=======*/
%let in_data=lunc.Location;
%let var_list = location_id  zip2;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));


/*out to one workbook*/
%let first_var=location_id;
%let remaining_vars= zip2;

%write_to_one_wb(&first_var., &remaining_vars.);


/*=======End of Location table=========*/



/*=========Ocular_meds table=========*/
%let in_data=lunc.Ocular_meds;
%let var_list = encounter_id medication  dosage  dose_unit administration ;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));


%let first_var=encounter_id;
%let remaining_vars=medication  dosage  dose_unit administration;
%write_to_one_wb(&first_var., &remaining_vars.);

/*===========End of Ocular_meds table=========*/


/*======Patient table ========*/
%let in_data=lunc.patient ;
%let var_list = patient_id  primary_insurance  secondary_insurance  age_range  gender;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));


%let first_var=patient_id;
%let remaining_vars=primary_insurance  secondary_insurance  age_range  gender;
%write_to_one_wb(&first_var., &remaining_vars.);

/*======End of patient table=====*/



/*======Physician table ========*/

/*physician table*/
proc sql;
create table check_practice_id as
select distinct practice_id from lunc.physician;quit;
data lunc.physician2;
set lunc.physician;

new_parctice_id=left(put(practice_id,11.));
drop practice_id;
rename new_parctice_id=practice_id;

run;

%let in_data=lunc.physician2 ;
%let var_list = physician_id practice_id;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));


%let first_var=physician_id;
%let remaining_vars=practice_id;
%write_to_one_wb(&first_var., &remaining_vars.);
/*===!!!End of Physician table===*/

/*============Procedures Table =======*/

data lunc.Procedures2;
set lunc.Procedures;
rename vestrum_procedure_name=vestr_proc_name;
run;

%let in_data= lunc.Procedures2;
%let var_list = encounter_id  procedure_text  eye  vestr_proc_name;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

%let first_var=encounter_id;
%let remaining_vars=procedure_text  eye  vestr_proc_name;
%write_to_one_wb(&first_var., &remaining_vars.);
/*===!!!End of Procedures table===*/


/*============Procedure_med table=============*/
%let in_data= lunc.Procedure_med;
%let var_list = encounter_id  eye  vestrum_drug_name;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

%let first_var=encounter_id;
%let remaining_vars=eye  vestrum_drug_name;
%write_to_one_wb(&first_var., &remaining_vars.);
/*===!!!End of Procedure_med table===*/

/*====Va_logmar table====*/
%let in_data=Lunc.Va_logmar;
%let var_list=  encounter_id  eye  element  value;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

%let var_list=  logMAR;
%let var_type=NUM;

%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

%let first_var=encounter_id;
%let remaining_vars= eye  element  value  logMAR;
%write_to_one_wb(&first_var., &remaining_vars.);
/*====!!! End of Va_logmar table====*/

/*Visual_acuity table */
%let in_data=lunc.Visual_acuity;
%let var_list=encounter_id eye element value;
%let var_type=char;
%foreach(v, &var_list. , %nrstr(%process_char(&in_data., &v., &missing_value.,&var_type.)));

%let first_var=encounter_id;
%let remaining_vars= eye element value;
%write_to_one_wb(&first_var., &remaining_vars.);
