


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


/*prepare data*/
*rsubmit;
proc sql;
select count(distinct p_id) as cohort_pat_count from master_cohort_pats_all; *56204:
quit;

proc sql;
create table step4_2nd_statinsdate as
select p_id, min(trans_date) as second_statins_date format date9.
from master_cohort_pats_all
where trans_date > index_date and market =1
group by p_id;

select count(distinct p_id) as pats_with_2n_statinsdate from step4_2nd_statinsdate;  *42663;

quit;

*rsubmit;
proc sql;
select count(distinct p_id) from Cohort_index_mole_supply2;  *56204;
quit;


*rsubmit;
proc sql;
create table step4_pre_data as
select a.*, b.second_statins_date 
from Cohort_index_mole_supply2 a inner join step4_2nd_statinsdate b
on a.p_id = b.p_id;

select count(distinct p_id) from step4_pre_data; *42663 patients matched ;

quit;

*rsubmit;
proc sql;
select count(distinct p_id) from step4_pre_data where second_statins_date <= index_date;
quit;

*rsubmit;
data step4_final_outcome;
set step4_pre_data;

rxend_of_index = index_date + days_supply_on_index - 1;

gap =  second_statins_date - rxend_of_index - 1;

if gap > 0.1 * days_supply_on_index then statins_2nd_script_compliance=0;
else statins_2nd_script_compliance=1;

run;
proc freq data= step4_final_outcome ;
tables statins_2nd_script_compliance;
run;
proc freq data= step4_final_outcome ;
tables statins_2nd_script_compliance;
where days_supply_on_index>80;
run;


*rsubmit;
proc sql;

select statins_2nd_script_compliance, sum(persistence_180) as persist_pats, count(*) as total_pats, sum(persistence_180)/count(*) as proportion_persist format percent8.2
from 
(
select p_id,
statins_2nd_script_compliance,
case when Capped_Days_treatment>=180 then 1 else 0 end as persistence_180
from step4_final_outcome
where days_supply_on_index>80 and days_supply_on_index<180

)

group by statins_2nd_script_compliance

;
quit;














