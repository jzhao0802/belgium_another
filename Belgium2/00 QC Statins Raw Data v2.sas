

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
/*keep all variables from the raw data, but only keep cohort patients */

*rsubmit;
/*proc datasets library=tempwork KILL;run;quit;*/

*rsubmit;
proc sql;
create table raw_data_cohort_pats as
select a.*, b.Capped_Days_treatment
from BE.Statins_history_sql_nomol a inner join BE.Statins_tom_50_buff_raw_pers b
on a.p_id = b.P_ID
where a.p_id not in (select distinct p_id from gy.Pat_delete_statins)
and a.transactiondate >= 20110701 and a.transactiondate <= 20150630
;
quit;
*rsubmit;
proc sql;
select count(distinct p_id) from raw_data_cohort_pats;quit;  *67911;

*rsubmit;
data raw_data_cohort_pats;
set raw_data_cohort_pats;

trans_date= input(put(transactiondate, 8.), yymmdd8.);
format trans_date date9.;


run;

/*cross check*/
*rsubmit;
/*proc compare base=be.Statins_tom_50_buff_raw_pers compare=gy.Cal_persistence_statins;run;  *all euqal;*/



*rsubmit;
/*ods listing close;*/
/*ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/units_freq_cohort_pats.xls";*/
/*proc sql;*/
/*select units, count(*) as transaction_count, count(distinct p_id) as n_patients from raw_data_cohort_pats group by units;*/
/*quit;*/
/*ods html close;*/
/*ods listing;*/

*rsubmit;
/*proc sql;*/
/*select count(distinct p_id) from raw_data_cohort_pats where units>12 ; */
/*quit;*/



*rsubmit;
proc sql;
create table raw_data_cohort_pats_final as
select a.*, b.pack_size,
case when b.pack_size =. then 0 else 1 end as market
from  raw_data_cohort_pats a left join gy.Dictionary_statins b
on a.fcc=b.fcc;
quit;
*rsubmit;
proc freq data= raw_data_cohort_pats_final; tables market/missing;run;


/*index date*/
*rsubmit;
proc sql;
create table cohort_pats_index as
select p_id, min(trans_date) as index_date format date9.
from raw_data_cohort_pats_final
where market=1 
and trans_date>="01Jul2012"d
and trans_date<= "30Jun2013"d
group by p_id
;
quit;



/* first delete some patients*/
*1. delete those patients with extremely large rx_id or extremely large units (if >12);
*rsubmit;
proc sql;
create table rxid_units_pat_to_delete as
select distinct p_id from raw_data_cohort_pats
where rx_id >12 or units>12 ;
quit; *1493;

/*---further check on data---*/
*rsubmit;
proc sql;
create table counting_units_byprod as
select p_id, trans_date , prod, sum(units) as total_units
from  raw_data_cohort_pats
group by p_id, trans_date, prod
having sum(units)>12
order by p_id, trans_date, prod
;
quit;

*rsubmit;
proc sql;
create table counting_units_byfcc as
select p_id, trans_date, fcc, sum(units) as total_units
from raw_data_cohort_pats
group by p_id, trans_date, fcc
having sum(units)>12
order by p_id, trans_date, fcc;
quit;
*rsubmit;
proc sql;
select count(distinct p_id) as pat_count01 from counting_units_byprod;
select count(distinct p_id) as pat_count02 from counting_units_byfcc;
quit;

*rsubmit;
proc sql;
select count(distinct p_id) as count01 from counting_units_byfcc
where p_id not in (select distinct p_id from counting_units_byprod);

select count(distinct p_id) as count02 from counting_units_byprod
where p_id not in (select distinct p_id from counting_units_byfcc);
quit;

/*will go with the first patient list*/
*rsubmit;
proc sql;
create table additiona_pats_to_delete as
select distinct p_id from counting_units_byprod;   *1014 patients selected;   
quit;

/*---note: now add back this list of patients---*/

*rsubmit;
proc sql;
select count(p_id) as p_count01 from rxid_units_pat_to_delete where p_id not in (select p_id from additiona_pats_to_delete) ;
select count(p_id) as p_count02 from additiona_pats_to_delete where p_id not in (select p_id from rxid_units_pat_to_delete);
quit;
/*now union the two patient lists*/
*rsubmit;
proc sql;
create table all_pats_to_delete as
select p_id from
(
select p_id from rxid_units_pat_to_delete
union
select p_Id from additiona_pats_to_delete
);
quit;



/*check units count*/
*rsubmit;
proc sql;
create table counting_all_units as 
select p_id, trans_date, sum(units) as total_units
from raw_data_cohort_pats
group by p_id, trans_date
having sum(units)>12
order by p_id, trans_date;
quit;
proc sql;
select count(distinct p_id) from counting_all_units;quit;   *10964 patients in all;


/*check*/
*rsubmit;
proc sql;
/*select count(distinct p_id) from all_pats_to_delete where p_id not in (select distinct p_id from counting_all_units);*/
/*select count(distinct p_id) from additiona_pats_to_delete where p_id not in (select distinct p_id from counting_all_units);*/
select count(distinct p_id) from rxid_units_pat_to_delete where p_id not in (select distinct p_id from counting_all_units);  *743 not contained in ;
quit;

*rsubmit;
proc sql;
create table all_pats_to_delete_final as
select distinct p_id from counting_all_units
union
select distinct p_id from all_pats_to_delete;
quit;  *10964 + 743 = 11707 patients;



*rsubmit;
proc sql;
delete from cohort_pats_index 
where p_id in (select p_id from all_pats_to_delete_final);
quit;
*rsubmit;
proc sql;
select count(distinct p_id) from cohort_pats_index;   *56204 patients left after deleting  from the original 67911;
quit;


*rsubmit;
proc sql;
delete from raw_data_cohort_pats_final 
where (p_id in (select p_id from all_pats_to_delete_final)) 
or (trans_date < "01Jul2012"d and market = 1);

select count(distinct p_id) from raw_data_cohort_pats_final;  *56204 patients left;

select count(*) from raw_data_cohort_pats_final where trans_date < "01Jul2012"d and market = 1;  *no record;

quit;


*rsubmit;
proc sql;
create table master_cohort_pats_all as
select a.*, b.index_date
from raw_data_cohort_pats_final a left join cohort_pats_index b
on a.p_id = b.p_id;
quit;
proc sql;
select count( * ) from master_cohort_pats_all where index_date=.;quit;

/*append ATC2 and ATC3 codes */
*rsubmit;
data master_cohort_pats_all;
set master_cohort_pats_all;

	ATC2 = substr(left(atc),1,3);
	ATC3= substr(left(atc),1,4);

format molecule $255.;
molecule= mol1;
*redefine the molecule for INEGY;
if prod="INEGY" then molecule="EZETIMIBE/SIMVASTATIN";

if prod="LIPITOR" and molecule="" then molecule="ATORVASTATIN";

if prod="PRAVAFENIX" then molecule="FENOFIBRATE/PRAVASTATIN";


run;

/*QC*/
*rsubmit;
proc sql;
select distinct prod, molecule from master_cohort_pats_all where prod in ("EZETROL","INEGY");
quit;


*rsubmit;
proc sql;
create table QC_refreshed_molecule as
select distinct fcc, prod, molecule from master_cohort_pats_all where market = 1;
quit;

*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/QC_refreshed_molecule.xls";
proc print data=QC_refreshed_molecule ; run;
ods html close;
ods listing;









*rsubmit;
proc sql;
create table  cohort_pats_index_statins  as 
select p_id, trans_date, prod, pack_size, fcc,  index_date
from master_cohort_pats_all
where trans_date = index_date and market = 1
order by p_id, trans_date, prod;quit;
proc sql;
select count(distinct p_id) from cohort_pats_index_statins;quit;  * 56204;



/*this is for QC only*/
*rsubmit;
proc sql;
create table multi_trans_index_statins as
select * from cohort_pats_index_statins
where p_id in 
(select p_id from cohort_pats_index_statins group by p_id, trans_date having count(*)>1 );quit;
 
*rsubmit;
proc sql;
create table index_statins_diffprods as
select * from multi_trans_index_statins
where p_id in 
(select p_id from multi_trans_index_statins group by p_id, trans_date having count(distinct prod) >1 );
quit;
*rsubmit;
proc sql;
select count(distinct p_id) from index_statins_diffprods;quit;  *265 patiens with different statin products on index date , the number has been reduced after deleting more than 10k patients;

*rsubmit;
/*ods listing close;*/
/*ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/index_statins_diffprods_v2.xls";*/
/*proc print data= index_statins_diffprods; run;*/
/*ods html close;*/
/*ods listing;*/
/*end of this QC*/

/*delete large data: raw_data_cohort_pats , raw_data_cohort_pats_final if no longer needed.*/
*rsubmit;
/*proc datasets library=tempwork;*/
/*delete raw_data_cohort_pats*/
/*raw_data_cohort_pats_final*/
/*;run;quit;*/












