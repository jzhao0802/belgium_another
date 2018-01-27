


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


*rsubmit;
proc sql;
create table p_id_maintable0 as
select distinct p_id from gy.statins_pre_index;
quit;  *49400;

/**rsubmit;*/
/*proc sql;*/
/*create table p_id_maintable as*/
/*select p_id from Cohort_index_mole_supply2*/
/*where days_supply_on_index > 80  and days_supply_on_index < 180;*/
/*quit;  *51635;*/
/**rsubmit;*/
/*proc sql;*/
/*select count(distinct p_id) from p_id_maintable;*/
/*quit;*/

/*master_cohort_pats_all*/
*rsubmit;
proc sql;
create table p_id_maintable as
select distinct p_id from master_cohort_pats_all
where trans_date < index_date and trans_date>= index_date-360
and market ^=1;  *53826;
quit;

*rsubmit;
proc sql;
create table p_id_maintable2 as
select distinct p_id from Cohort_index_mole_supply2
where days_supply_on_index > 80  and days_supply_on_index < 180
and p_id in (select p_id from p_id_maintable);
quit;  *49405;

/*compare gy's patient list and my list*/
*rsubmit;
proc sql;
select p_id from p_id_maintable0 where p_id not in (select p_id from p_id_maintable2);

select p_id from p_id_maintable2 where p_id not in (select p_id from   p_id_maintable0);
quit;

/*
                                                     p_id
                                              -----------
                                                   700803
                                                   916876
                                                  1063730
                                                  1789856
                                                  2101981

*/

/*overwrite p_id_maintable*/
*rsubmit;
proc sql;
create table p_id_maintable as
select * from p_id_maintable2;
quit;


/*QC*/
*rsubmit;
proc sql;
select count(distinct a.p_id) 
from p_id_maintable a left join Cohort_index_mole_supply2 b
on a.p_id = b.p_id
where b.days_supply_on_index<=80 or b.days_supply_on_index>=180
;
quit;

*rsubmit;
proc sql;
select count(distinct a.p_id)
from p_id_maintable a left join Cohort_index_mole_supply2 b
on a.p_id = b.p_id
where b.molecule_on_index is missing  ;
quit;

/*1. check intersection of  p_id_maintable and step4_final_outcome*/
*rsubmit;
proc sql;
create table intersect_main_and_step4 as
select distinct p_id from p_id_maintable
intersect
select distinct p_id from step4_final_outcome;

select count(distinct p_id) as p_count from intersect_main_and_step4;
quit;   *37693;

/*2. merge with step3 outcome table -- step3_final_outcome*/
*rsubmit;
proc sql;
create table intermediate_table_01 as
select a.p_id, b.GP_vol_portion, b.NonGP_vol_portion, b.Unknown_vol_portion, b.GP_vol, b.NonGP_vol, b.Unknown_vol
from intersect_main_and_step4 a left join step3_final_outcome b
on a.p_id=b.p_id;
quit;
*rsubmit;
proc sql;
select count(distinct p_id) from intermediate_table_01
where GP_vol_portion is missing 
or NonGP_vol_portion is missing
or Unknown_vol_portion is missing;

quit;




/*3. merge in proportion days covered -- there are 20 ATC codes*/
*rsubmit;
proc sort data= BE.Pro_days_covered_atc3_60days out=temp_01 (keep=p_id ATC3 pro_days_covered  ); by p_id ATC3; run;
proc transpose data= temp_01 out= temp_02(drop=_NAME_) prefix=pro_days_;
by p_id;
var pro_days_covered ;
id ATC3;
run; 

*rsubmit;
proc sort data= BE.Pro_days_covered_all_60days out= temp_03 (keep = p_id ATC3 pro_days_covered); by p_id; run;
proc transpose data= temp_03 out = temp_04(drop=_NAME_) prefix= pro_days_;
by p_id;
var pro_days_covered;
id ATC3;
run; 

*rsubmit;
proc sql;
create table intermediate_table_02 as
select a.*

, b.pro_days_A02B 
, b.pro_days_B01C
, b.pro_days_C07A
, b.pro_days_C03A
, b.pro_days_C08A
, b.pro_days_N05A
, b.pro_days_N05C
, b.pro_days_N06A
, b.pro_days_C09D
, b.pro_days_R03F
, b.pro_days_R06A
, b.pro_days_C09C
, b.pro_days_N03A
, b.pro_days_M02A
, b.pro_days_C09A
, b.pro_days_H03A
, b.pro_days_C09B
, b.pro_days_R03G
, b.pro_days_C07B
, b.pro_days_A10J

, c.pro_days_20_ATC3
from intermediate_table_01 a left join temp_02 b
on a.p_id = b.p_id
left join temp_04 c
on a.p_id=c.p_id
;quit;

/*4. merge in the column statins_2nd_script_compliance */
*rsubmit;
proc sql;
create table intermediate_table_03 as
select a.*, b.statins_2nd_script_compliance
from intermediate_table_02 a left join step4_final_outcome b
on a.p_id = b.p_id;

select nmiss(statins_2nd_script_compliance) as missing_compliance from intermediate_table_03;

quit;

/*QC how many missing*/


*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/check_n_missing_v2.xls";
proc sql;
select 
count(*) as total_patients
, nmiss(pro_days_A02B)/count(*) as percent_missing_A02B format percent8.2
, nmiss(pro_days_B01C)/count(*) as percent_missing_B01C format percent8.2
, nmiss(pro_days_C07A)/count(*) as percent_missing_C07A format percent8.2
, nmiss(pro_days_C03A)/count(*) as percent_missing_C03A format percent8.2
, nmiss(pro_days_C08A)/count(*) as percent_missing_C08A format percent8.2
, nmiss(pro_days_N05A)/count(*) as percent_missing_N05A format percent8.2
, nmiss(pro_days_N05C)/count(*) as percent_missing_N05C format percent8.2
, nmiss(pro_days_N06A)/count(*) as percent_missing_N06A format percent8.2
, nmiss(pro_days_C09D)/count(*) as percent_missing_C09D format percent8.2
, nmiss(pro_days_R03F)/count(*) as percent_missing_R03F format percent8.2
, nmiss(pro_days_R06A)/count(*) as percent_missing_R06A format percent8.2
, nmiss(pro_days_C09C)/count(*) as percent_missing_C09C format percent8.2
, nmiss(pro_days_N03A)/count(*) as percent_missing_N03A format percent8.2
, nmiss(pro_days_M02A)/count(*) as percent_missing_M02A format percent8.2
, nmiss(pro_days_C09A)/count(*) as percent_missing_C09A format percent8.2
, nmiss(pro_days_H03A)/count(*) as percent_missing_H03A format percent8.2
, nmiss(pro_days_C09B)/count(*) as percent_missing_C09B format percent8.2
, nmiss(pro_days_R03G)/count(*) as percent_missing_R03G format percent8.2
, nmiss(pro_days_C07B)/count(*) as percent_missing_C07B format percent8.2
, nmiss(pro_days_A10J)/count(*) as percent_missing_A10J format percent8.2
, nmiss(pro_days_20_ATC3)/count(*) as percent_missing_allATC3 format percent8.2
from intermediate_table_03;
quit;
ods html close;
ods listing;



/*further check on persistence data in step2*/
*rsubmit;
proc sql;
create table QC_01 as
select a.*,
case when b.p_id ^= . then 1 else 0 end as in_flag
from temp_02 a left join p_id_maintable2 b
on a.p_id = b.p_id
;
quit;


*rsubmit;
proc sql;
select ATC3 , count(distinct p_id) as p_count from BE.Pro_days_covered_atc3_60days
group by ATC3;
quit;


*rsubmit;
proc sql;
select count(*) - nmiss(pro_days_A02B) as A02B, 
 count(*) - nmiss(pro_days_B01C) as B01C,
 count(*) - nmiss(pro_days_C07A) as C07A
from temp_02;quit;
