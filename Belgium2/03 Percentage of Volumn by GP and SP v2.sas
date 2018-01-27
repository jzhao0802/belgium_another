



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

*rsubmit;
proc sql;
create table step3_pre_data as
select a.p_id
, a.trans_date format date9.
, a.index_date format date9.
, a.shortcode

, a.ATC3
, a.atc
, a.fcc
, a.prod
, a.units
, a.pack
,a.market

, b.DOC_SPECIALITY
, b.doc_gender
, b.activity
, b.region_doc
, case when b.DOC_SPECIALITY in ("11", "34") then "GP"
when b.DOC_SPECIALITY in ("","60","90") then "Unknown"
else "Non-GP" end as spec_group
 
from master_cohort_pats_all a left join BE.doc_details b
on a.shortcode = b.shortcode
where a.trans_date < a.index_date and  a.trans_date >= (a.index_date - 360)
;
quit;
*rsubmit;
proc sql;
select count(distinct p_id) from step3_pre_data where market=1;
quit;

*rsubmit;
proc freq data= step3_pre_data;tables spec_group/missing;run;

*rsubmit;
proc sql;

create table step3_pre_data2 as
select p_id, spec_group, count(*) as trans_count, sum(units) as units_sum 
from step3_pre_data
group by p_id, spec_group;

select count(distinct p_id) from step3_pre_data2;   *53, 826;

quit;

*rsubmit;
proc sql;
create table step3_pre_data3 as
select p_id, spec_group, trans_count, sum(trans_count) as total_trans_count, trans_count/sum(trans_count) as trans_portion
from step3_pre_data2
group by p_id
order by p_id;
quit;

*rsubmit;
proc sql;
create table step3_final_outcome as
select p_id, 
sum(GP_vol_portion) as GP_vol_portion format percent8.2,
sum(NonGP_vol_portion) as NonGP_vol_portion format percent8.2,
sum(Unknown_vol_portion) as Unknown_vol_portion format percent8.2,
sum(GP_vol) as GP_vol,
sum(NonGP_vol) as NonGP_vol,
sum(Unknown_vol) as Unknown_vol
from 
(
select p_id, 

case when spec_group="GP" then trans_portion else 0 end as GP_vol_portion,
case when spec_group="Non-GP" then trans_portion else 0 end as NonGP_vol_portion,
case when spec_group="Unknown" then trans_portion else 0 end as Unknown_vol_portion,

case when spec_group="GP" then trans_count else 0 end as GP_vol,
case when spec_group="Non-GP" then trans_count else 0 end as NonGP_vol,
case when spec_group="Unknown" then trans_count else 0 end as Unknown_vol

from step3_pre_data3
)
group by p_id
order by p_id;

quit;

*rsubmit;
proc sql;
select count(distinct p_id) from step3_final_outcome;
quit;  *53826;


/*End*/








