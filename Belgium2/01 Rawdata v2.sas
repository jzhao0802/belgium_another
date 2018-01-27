


/*signoff;*/
/*directory with example data*/
libname indata "D:\Project Files\Belgium Predictive Modeling\Belgium2\Raw Data";

/*start connection*/
options remote=psc02 comamid=tcp;
filename rlink "D:\yxue\Unix SAS\Script\psc02_yan.txt";    
signon psc02;

rsubmit;
options mprint nosymbolgen mlogic obs = max;
libname BE '/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data';
endrsubmit;

/*define work*/
rsubmit;
libname tempwork "/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/tempwork";
options user=tempwork;
endrsubmit;


libname ube slibref = BE server = psc02;
libname Uwork slibref = tempwork server = psc02; 

/*library with additional data*/
rsubmit;
libname gy "/fs219.1/Sales & Account Management/2015/Belgium_PA/gyhu/01 Data/tmp/";
endrsubmit;

libname ugy slibref=gy server=psc02;

/*
main data to start with is -- master_cohort_pats_all (after deleting some patients) 
*/




/*statins molecule and packsize on index date*/


*rsubmit;
proc sql;
create table pre_table_01 as
select p_id, trans_date, index_date, molecule,  sum(units * pack_size ) as days_supply_on_index
from master_cohort_pats_all
where trans_date = index_date and market =1
group by p_id, trans_date, index_date, molecule
order by p_id, trans_date, index_date, molecule;
quit;

/**/
*rsubmit;
proc sql;
select count(*) from master_cohort_pats_all where  trans_date = index_date and market =1 and molecule="";
select count(*) from master_cohort_pats_all where  trans_date = index_date and market =1 and units=.;
quit;
/**rsubmit;*/
/*proc sql;*/
/*select count(distinct p_id) from pre_table_01 where days_supply_on_index>100; quit;   *;*/
/*quit;*/
/**rsubmit;*/
/*proc sql;*/
/**you may want to check these  patients;*/
/*quit;*/
/**rsubmit;*/
/*proc means data = pre_table_01 min p5 p10 p25 p50 p75 p90 p95 p99 max;*/
/*var days_supply_on_index;run;*/
/**rsubmit;*/
/*ods listing close;*/
/*ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/check_statins_days_supply_outlier.xls";*/
/*proc print data= pre_table_01; where days_supply_on_index>100; run;*/
/*ods html close;*/
/*ods listing;*/


/*dedup by statins molecule*/
*rsubmit;
proc sort data = pre_table_01 ;
by p_id trans_date descending days_supply_on_index;
run;

data    pre_table_unique_mol  pre_table_02  pre_table_03;
set pre_table_01;
by p_id trans_date descending days_supply_on_index;

if first.trans_date and last.trans_date then output pre_table_unique_mol;
else do;
	if molecule ^= "EZETIMIBE" then output pre_table_02;
	else output pre_table_03;
end;

run;


/*some qc*/
proc sql;
select count(distinct p_id) as pats_with_unique from pre_table_unique_mol; *55980;
select count(distinct p_id) as pats_with_multiple from pre_table_02;   *224;
select count(distinct p_id) as pats_with_multi_eze from pre_table_03;     *182 from the 224 patients;
quit;

*rsubmit;
proc sql;
select count(distinct p_id) from pre_table_03 where p_id not in (select distinct p_id from pre_table_02);
select count(distinct p_id) from pre_table_02 where p_id in (select distinct p_id from pre_table_unique_mol);
quit;

*rsubmit;
/*proc sql;*/
/*create table check_unequal_pats as*/
/*select * from pre_table_01*/
/*where p_id in (1452211, 3414455, 4390467, 5417003, 5525658, 6035108, 9324630, 9342185, 9343290, 9494238, 10994164);*/
/*quit;*/
*rsubmit;
/*proc sql;*/
/*select count(distinct p_id) from check_unequal_pats;*/
/*quit;*/
*rsubmit;
/*ods listing close;*/
/*ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/11_patients.xls";*/
/*proc print data= check_unequal_pats; run;*/
/*ods html close;*/
/*ods listing;*/
/**rsubmit;*/
/*proc sql;*/
/*select * from pre_table_02 where p_id = 10994164;*/
/*quit;*/




/*process those with more than one molecule ("EZETIMIBE" already excluded )*/
*rsubmit;
data pre_table_02b;
set pre_table_02;
by p_id trans_date descending days_supply_on_index;

if first.trans_date;

run;
*rsubmit;
proc sql;
select count(distinct p_id) from pre_table_02b; *224 patients;
select count(*) from pre_table_02b; *224 records;
quit;  

*rsubmit;
proc sql;
create table cohort_index_mole_supply as
select p_id,  index_date, molecule as molecule_on_index,  days_supply_on_index,
case when days_supply_on_index>80 then 1 else 0 end as usable
from
(select p_id, index_date, molecule, days_supply_on_index from pre_table_unique_mol
union 
select p_id, index_date, molecule, days_supply_on_index from pre_table_02b)
order by p_id
;
quit;  *56204 unique patients;

*rsubmit;
proc freq data= cohort_index_mole_supply; table usable/missing;run;   *3871  not usable (days supply is no larger than 80);




/*QC*/


*rsubmit;
proc sql;
create table cohort_index_mole_supply2 as
select a.*, b.Capped_Days_treatment
from cohort_index_mole_supply a left join BE.Statins_tom_50_buff_raw_pers b
on a.p_id = b.p_id;
quit;
*rsubmit;
proc sql;
select count(distinct p_id) from cohort_index_mole_supply2 where Capped_Days_treatment=.;
quit;



*rsubmit;
%macro cal_distribution(Capped_Days);

proc sql noprint; 
select count(distinct p_id) into: count_00 from cohort_index_mole_supply where days_supply_on_index<=80;
select count(distinct p_id) into: count_01 from cohort_index_mole_supply where days_supply_on_index>80 and days_supply_on_index<=100;
select count(distinct p_id) into: count_02 from cohort_index_mole_supply where days_supply_on_index>100 and days_supply_on_index<=180;
select count(distinct p_id) into: count_03 from cohort_index_mole_supply where days_supply_on_index>180 and days_supply_on_index<=200;
select count(distinct p_id) into: count_04 from cohort_index_mole_supply where days_supply_on_index>200;
quit;


proc sql noprint;
select count(distinct p_id) into: count_00b from cohort_index_mole_supply2 where  days_supply_on_index<=80 and Capped_Days_treatment>= &Capped_Days.;
select count(distinct p_id) into: count_01b from cohort_index_mole_supply2 where  days_supply_on_index>80 and days_supply_on_index<=100 and Capped_Days_treatment>= &Capped_Days.;
select count(distinct p_id) into: count_02b from cohort_index_mole_supply2 where days_supply_on_index>100 and days_supply_on_index<=180 and Capped_Days_treatment>= &Capped_Days.;
select count(distinct p_id) into: count_03b from cohort_index_mole_supply2 where days_supply_on_index>180 and days_supply_on_index<=200 and Capped_Days_treatment>= &Capped_Days.;
select count(distinct p_id) into: count_04b from cohort_index_mole_supply2 where days_supply_on_index>200 and Capped_Days_treatment>=  &Capped_Days.;
quit;

data cal_distribution_&Capped_Days.days;
format days_supply_range $20.;
days_supply_range="(0, 80]";
n_persist_patiens = &count_00b.;
n_total_patients= &count_00.;
percentage_persist=n_persist_patiens/n_total_patients;
format percentage_persist percent8.3;
output;

days_supply_range="(80, 100]";
n_persist_patiens = &count_01b.;
n_total_patients= &count_01.;
percentage_persist=n_persist_patiens/n_total_patients;

output;



days_supply_range="(100, 180]";
n_persist_patiens = &count_02b.;
n_total_patients= &count_02.;
percentage_persist=n_persist_patiens/n_total_patients;

output;


days_supply_range="(180, 200]";
n_persist_patiens = &count_03b.;
n_total_patients= &count_03.;
percentage_persist=n_persist_patiens/n_total_patients;

output;


days_supply_range="(200,  infinite]";
n_persist_patiens = &count_04b.;
n_total_patients= &count_04.;
percentage_persist=n_persist_patiens/n_total_patients;

output;


run;

%mend cal_distribution;

*rsubmit;
%cal_distribution(90);
%cal_distribution(180);
%cal_distribution(270);



*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/Cal_distribution_180days_final.xls";
proc print data= Cal_distribution_180days; run;
ods html close;
ods listing;


*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/Cal_distribution_90days_final.xls";
proc print data= Cal_distribution_90days; run;
ods html close;
ods listing;
*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/Cal_distribution_270days_final.xls";
proc print data= Cal_distribution_270days; run;
ods html close;
ods listing;




/*QC--compare with Guangyu's result table*/
*rsubmit;
proc sql;
create table temp_QC as
select a.p_id, a.index_date, a.index_statins_molecule,  a.days_supply_on_index,
b.index_date as index_date2, b.molecule_on_index as index_statins_molecule2, b.days_supply_on_index as days_supply_on_index2, 
(a.days_supply_on_index-b.days_supply_on_index) as days_supply_diff
from gy.pre_table_02 a left join Cohort_index_mole_supply2 b
on a.p_id = b.p_id;
quit;
*rsubmit;
proc sql;
select count(distinct p_id) from temp_QC where days_supply_diff^=0;
quit;

*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/check_days_supply_diff.xls";
proc print data= temp_QC; where days_supply_diff^=0;run;
ods html close;
ods listing;

*rsubmit;
proc sql;
select count(distinct p_id) from temp_QC where index_date^=index_date2;
select count(distinct p_id) from temp_QC where index_statins_molecule^=index_statins_molecule2;
quit;

*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/check_molecule_diff.xls";
proc print data= temp_QC; where index_statins_molecule^=index_statins_molecule2;run;
ods html close;
ods listing;









/*>>>>>>>>>>>>>>>>>>>> Start >>>>>>>>>>>>>>>>>>>>*/

*this is for a memo
when checking top 20 ATC3, just base on the 56204 patients after deleting those questionable patients. 
when calculating average duration, also base on the 56204 patients. time period is from Jul01, 2011  to  Jun30, 2015


the filtering based on statins days supply on index date is only for creating cohort patients for modeling.
other measures are for individal patients, so it is fine to do it before or after filtering patients by statins days supply on index date
;



/*>>>>>>>>>>>>>>>>>>>> End  >>>>>>>>>>>>>>>>>>>>*/


/*>>>>>>>>>>>>>>>>>>>> Re-check ATC3 codes >>>>>>>>>>>>>>>>>>>>*/


*rsubmit;
%let atc_num=3;
*rsubmit;
proc sql;
create table summary03_by_atc&atc_num. as
select  ATC&atc_num. , count(*) as transaction_count, count(distinct p_id) as num_patients,  count(*)/count(distinct p_id)  as mean_trans_per_patient
from master_cohort_pats_all
where trans_date < index_date and trans_date>= index_date-360
group by ATC&atc_num.;
quit;
proc sort data=summary03_by_atc&atc_num.; by descending num_patients;run;

*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/summary03_by_atc&atc_num._recheck.xls";
proc print data= summary03_by_atc&atc_num. ; run;
ods html close;
ods listing;


/*top 20 ATC3 codes for chronic diseases
A02B
C07A
B01C
N05C
N06A
M02A
C03A
C08A
C09A
A10J
R06A
R03F
H03A
R03G
C09B
C09C
N05A
C07B
N03A
C09D
analysis is only for the 68k cohort statins patients
*/

*!!! this top 20 list is still unchanged.;






/*>>>>>>>>>>>>>>>>>>>> End  >>>>>>>>>>>>>>>>>>>>*/
