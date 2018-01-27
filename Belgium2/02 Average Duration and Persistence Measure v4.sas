



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
%macro write_atc3_codes(input_list);

%let _j=1;
%do %while(not(%qscan(&input_list.,&_j, %str( ))=));
	
	%let atc_value = %scan(&input_list.,&_j, %str( ));
	%if &_j = 1 %then  %do;
		data out_data;
		atc_id = &_j;
		atc3_code = "&atc_value.";
		run;
	%end;
	%else %do;
	data temp_data;
	atc_id = &_j;
	atc3_code = "&atc_value.";
	run;
	
	data out_data;
	set out_data temp_data;
	run;
	%end;

	%let _j=%eval(&_j+1);

%end;

%mend write_atc3_codes;
%let atc_series = A02B  C07A  B01C  N05C  N06A  C03A M02A  C08A  C09A  A10J  R06A  R03F  H03A  R03G  N05A  C09B  N03A  C07B  C09C  C09D;
%write_atc3_codes(&atc_series.);







/*average duration
it is based on SAS Dataset -- master_cohort_pats_all (include all statins days supply, time frame 01Jul2011 to 30Jun2015, over 10k patients deleted)
*/

/*need to dedup by prod, fcc, p_id, trans_date*/
*rsubmit;
proc sql;
create table maintable_avg_duration as
select distinct prod, fcc, p_id, trans_date, ATC3
from master_cohort_pats_all
where ATC3 in (select atc3_code from out_data)
order by prod, fcc, p_id, trans_date
;
quit;
/*
if no deduplication done, then 1,887,332 records selected from master_cohort_pats_all;
if deduplication done, then 1,796,560 records selected from master_cohort_pats_all;

*/

*rsubmit;
proc sql;
select count(distinct p_id) as p_count01 from  master_cohort_pats_all ;  *56204;
select count(distinct p_id) as p_count02 from  maintable_avg_duration;  *54469;
quit;



*rsubmit;
/*proc sql;*/
/*select distinct ATC3 from maintable_avg_duration;*/
/*quit;*/


*rsubmit;
/*proc sql;*/
/*select count(distinct p_id) from maintable_avg_duration*/
/*where p_id in (select distinct p_id from maintable_avg_duration group by p_id, trans_date, prod having count(*)>1);*/
/**only 1 product -- 54458 pats;*/
/**2 products --4830 pats;*/
/**3 products -- 51 pats;*/



*rsubmit;
/*just append columns to this data set*/
/*data maintable_avg_duration;*/
/*set maintable_avg_duration;*/
/*by prod fcc p_id trans_date;*/
/**/
/*last_trans_date=lag(trans_date);*/
/*format last_trans_date date9.;*/
/*if first.p_id then do;*/
/*	last_trans_date=.;*/
/*end;*/
/**/
/*gap_between=trans_date - last_trans_date;*/
/**/
/*run;*/

*rsubmit;
%macro prod_level_duration(dedup_prod=no);


/*Step 1 -- Identify and delete those products used by no more than 5 unique patients*/
*rsubmit;
proc sql;
create table maintable_avg_duration as
select distinct prod, fcc, p_id, trans_date, ATC3
from master_cohort_pats_all
where ATC3 in (select atc3_code from out_data)
order by prod, fcc, p_id, trans_date
;
quit;

/*check */
*rsubmit;
proc sql;
create table prods_to_delete as
select prod, count(distinct p_id) as p_count 
from  maintable_avg_duration
group by prod
having count(distinct p_id)<=5
;   *108;

select count(distinct prod) as n_prod_delete from prods_to_delete;

quit;
/*End of step 1*/


/*Step 2 -- delete those products and generate product level duration*/

*rsubmit;
proc sql;
create table maintable_avg_duration2 as
select prod, p_id, trans_date, fcc, ATC3
from maintable_avg_duration
where prod not in (select prod from prods_to_delete)
order by prod, p_id, trans_date;
quit;

*rsubmit;
proc sql;
select count(distinct p_id) as p_count from maintable_avg_duration2;  *54468;
quit;


data maintable_avg_duration2;
set maintable_avg_duration2;
by prod p_id trans_date;

/*this is for calculating product level duration*/
last_trans_date=lag(trans_date);
format last_trans_date date9.;
if first.p_id then do;
	last_trans_date=.;
end;

gap_between=trans_date - last_trans_date;

/*Nov24: this is for fcc level duration; the reason is that the current gap_between should be matched to last fcc*/
fcc_matched=lag(fcc);
if first.p_id then do;
	fcc_matched=. ;  *note: fcc is in numeric format;
end;


run;
*rsubmit;
proc means data=maintable_avg_duration2 noprint; 
var gap_between;
by prod;
where gap_between ^= . ;
output out=prod_median_duration(drop=_:) median=median_duration;
run;
*check if there is any product with 0 duration;
*rsubmit;
proc print data=prod_median_duration;
where median_duration=0;run; 
/* there is no products with 0 duration;
I will just leave it as it is.
*/

*rsubmit;
proc sql;
select count(distinct prod) as prod_count from prod_median_duration;quit;
quit;  *659;

*rsubmit;
proc means data=prod_median_duration min p5 p10 p25 median p75 p90 p95  max;
var median_duration;
output out=prod_mduration_p5_p95(drop=_:) p5=duration_p5 p95=duration_p95;
run;

*capped at p5 and p95;
*rsubmit;
data prod_median_duration;
set prod_median_duration;
if _N_ =1 then set prod_mduration_p5_p95;
if median_duration < duration_p5  then median_duration = duration_p5;
if median_duration > duration_p95 then median_duration = duration_p95;
drop duration_p5  duration_p95;

run;
*QC;
proc means data=prod_median_duration min p5 p10 p25 median p75 p90 p95  max;
var median_duration;run;

/*Step 3 -- calculate fcc level duration*/
*rsubmit;
proc sort data= maintable_avg_duration2 out=temp_sorted_01;
by fcc_matched prod;
where fcc_matched ^=.;run;

proc means data= temp_sorted_01 noprint;
var gap_between;
by fcc_matched prod;
where gap_between ^= . ;
output out=fcc_median_duration(drop=_:) median=median_duration;
run; 

/**rsubmit;*/
/*proc sql;*/
/*select count(distinct fcc_matched) from fcc_median_duration;*/
/*select count(*) from fcc_median_duration;*/
/*quit;*/


*rsubmit;
proc print data=fcc_median_duration;
where median_duration=0;run;
*rsubmit;
proc sql;
select count(distinct fcc_matched) as fcc_count from fcc_median_duration;   /*2334*/
select count(distinct prod) as prod_count from fcc_median_duration;   /*659*/
quit;
*rsubmit;
proc means data=fcc_median_duration  min p5 p10 p25 p50 p75 p90 p95 max;
var median_duration;
output out=fcc_mduration_p5_p95(drop=_:)  p5=duration_p5 p95=duration_p95; 
run;

*rsubmit;
data fcc_median_duration_cor;
set fcc_median_duration;
if _N_=1 then set fcc_mduration_p5_p95;

if median_duration<duration_p5 then median_duration=duration_p5;

if median_duration>duration_p95 then median_duration=duration_p95;

run;
proc means data= fcc_median_duration_cor min p5 p10 p25 p50 p75 p90 p95 max;
var median_duration;run;


*This is to dedup at product level before calculating product level duration;

%if %upcase(&dedup_prod.) = YES %then %do;
*rsubmit;
proc sql;
create table maintable_avg_duration2b as
select distinct prod, p_id, trans_date, ATC3
from maintable_avg_duration
where prod not in (select prod from prods_to_delete)
order by prod, p_id, trans_date;
quit;


*rsubmit;
proc sql;
select count(distinct p_id) from maintable_avg_duration2b;
quit;  *54468;


data maintable_avg_duration2b;
set maintable_avg_duration2b;
by prod p_id trans_date;

/*this is for calculating product level duration*/
last_trans_date=lag(trans_date);
format last_trans_date date9.;
if first.p_id then do;
	last_trans_date=.;
end;

gap_between=trans_date - last_trans_date;

run;

*rsubmit;
proc means data=maintable_avg_duration2b noprint; 
var gap_between;
by prod;
where gap_between ^= . ;
output out=prod_median_duration_cor(drop=_:) median=median_duration;
run;
*check if there is any product with 0 duration;
*rsubmit;
proc print data=prod_median_duration_cor;
where median_duration=0;run; 
/* there is no products with 0 duration;
I will just leave it as it is.
*/

*rsubmit;
proc sql;
select count(distinct prod) as prod_count from prod_median_duration_cor;quit;
quit;  *659;

*rsubmit;
proc means data=prod_median_duration_cor min p5 p10 p25 median p75 p90 p95  max;
var median_duration;
output out=prod_mduration_p5_p95_cor(drop=_:) p5=duration_p5 p95=duration_p95;
run;


*capped at p5 and p95;
*rsubmit;
data prod_median_duration_cor;
set prod_median_duration_cor;
if _N_ =1 then set prod_mduration_p5_p95_cor;
if median_duration < duration_p5  then median_duration = duration_p5;
if median_duration > duration_p95 then median_duration = duration_p95;
drop duration_p5  duration_p95;

run;
*QC;
proc means data=prod_median_duration_cor min p5 p10 p25 median p75 p90 p95  max;
var median_duration;run;
%end;


/*step 4 -- prepare 12 months pre-index data for proportino days covered*/
*rsubmit;
proc sql;
create table pre_pdc_data00 as
select * 
from master_cohort_pats_all
where trans_date < index_date  and  trans_date>= (index_date-360)
and ATC3 in (select atc3_code from out_data)
and prod not in (select prod from prods_to_delete);
quit;
proc sql;
select count(distinct p_id) as patient_count from pre_pdc_data00;   /*43543 if not including index date; 49894 if including index date */
select count(distinct prod) as product_count from pre_pdc_data00;  /* 611 if not including index date; 616 if including index date */
select count(distinct fcc) as fcc_count from pre_pdc_data00;   /* 2024 if not including index date;  2067 if not including index date*/
quit;

/*merge with median duration by fcc*/
/* and merge with median duration by prod in order to fill in for those fcc with missing duration*/
*rsubmit;
proc sql;
create table pre_pdc_data01 as
select a.*, b.median_duration as ff_median_duration, c.median_duration as prod_median_duration
from pre_pdc_data00 a left join fcc_median_duration_cor b
on a.fcc=b.fcc_matched

%if %upcase(&dedup_prod.) =YES %then %do;
left join prod_median_duration_cor c
%end;
%else %if %upcase(&dedup_prod.) =NO %then %do;
left join prod_median_duration c
%end;
on a.prod = c.prod
;
quit;


/*proc datasets library=tempwork;*/
/*delete maintable_avg_duration2*/
/*prod_mduration_p5_p95*/
/*;run;quit;*/

%mend prod_level_duration;

*rsubmit;
%prod_level_duration(dedup_prod=yes);

*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/prod_level_duration.xls";
proc means data=prod_median_duration min p5 p10 p25 median p75 p90 p95  max;
var median_duration;run;
proc means data=prod_median_duration_cor min p5 p10 p25 median p75 p90 p95  max;
var median_duration;run;
ods html close;
ods listing;


/*some QC*/
*rsubmit;
proc sql;
create table QC_situation_1 as
select fcc, prod, count(distinct p_id) as p_count, count(*) as trans_count from pre_pdc_data01
where ff_median_duration=. and prod_median_duration=.
group by fcc, prod
order by fcc, prod; *not very impactful as only at most 4 patients involved for any of the three products;
* 
1.  223510  "KIRA"
2.  244079  "NEBU-TROP"
3.  250364  "OMEPRACURE MYLAN"
;
/*create table QC_situation_2 as */
/*select distinct fcc, prod from pre_pdc_data01*/
/*where ff_median_duration^=. and prod_median_duration=.*/
/*order by fcc, prod;*/

create table QC_situation_1b as
select fcc, prod, p_id, trans_date from pre_pdc_data01
where prod in ("KIRA","NEBU-TROP","OMEPRACURE MYLAN");

create table QC_situation_1c as
select fcc, prod, p_id, trans_date from pre_pdc_data01
where fcc in (223510, 244079, 250364);

quit;
*rsubmit;
proc sql;
select distinct prod from pre_pdc_data01
where prod not in (select distinct prod from fcc_median_duration_cor)
;
select distinct prod from pre_pdc_data01
where prod not in (select distinct prod from prod_median_duration_cor)
;
quit;
*rsubmit;
proc sql;
create table QC_fcc_nonexist as
select * from maintable_avg_duration2
where fcc in (223510, 244079, 250364)
;
create table QC_prod_nonexist as 
select * from maintable_avg_duration2b
where prod in ("KIRA","NEBU-TROP","OMEPRACURE MYLAN");

quit;


/*further delete those with missing duration */
*rsubmit;
data pre_pdc_data01;
set pre_pdc_data01;

if prod not in ("KIRA", "NEBU-TROP","OMEPRACURE MYLAN");
median_duration= ff_median_duration;
if median_duration=. then median_duration=prod_median_duration;


run;
*rsubmit;
proc sql;
select count(*) from pre_pdc_data01 where median_duration=.;quit;

*rsubmit;
proc means data=pre_pdc_data01 min p5 median p95 max;
var median_duration;run;
*rsubmit;
proc means data=pre_pdc_data01 min p5 median p95 max;
var ff_median_duration prod_median_duration;run;


/*proportion days covered*/
*rsubmit;
options mprint;
%macro proportion_days_by_ATC3(input_data,n_days_preindex, clean_tempdata=yes);

*sum up duration for products bought on the same date, note: need to multiply median_duration with units;
proc sql;
create table temp_pre_portion_01 as
select ATC3, p_id, index_date, trans_date , sum(median_duration * units) as actual_duration
from &input_data.
group by ATC3, p_id, index_date, trans_date 
order by ATC3, p_id, index_date, trans_date ;
quit;

*rsubmit;
proc sql;
select count(distinct p_id) as n_pats_before_selection from temp_pre_portion_01;
quit;  *43543;

data temp_20ATC3_1stday(keep=ATC3  p_id  first_rx_date total_n_days);
set temp_pre_portion_01;
by ATC3  p_id  trans_date;

if first.p_id then do;
	total_n_days= index_date - trans_date;   
	if total_n_days >= &n_days_preindex. then do;   *use >=;
		rename trans_date=first_rx_date;
		output temp_20ATC3_1stday;
	end;
end;

run;

*rsubmit;
proc sql;
select count(distinct p_id) as p_count from temp_20ATC3_1stday;
quit;  
*37248 patients if 60 days preindex;
*40344 patients if 30 days preindex;
*35064 patients if 90 days preindex;

*rsubmit;
proc sql;
create table temp_pre_portion_02 as
select a.*, b.first_rx_date, b.total_n_days
from temp_pre_portion_01 a inner join temp_20ATC3_1stday b
on a.p_id=b.p_id and a.ATC3=b.ATC3
order by a.ATC3, a.p_id, a.trans_date
;
quit; 
*rsubmit;
proc sql;
select count(distinct p_id) from temp_pre_portion_02;quit;
quit;



*calculate rxend date;
data temp_pre_portion_02(rename=(trans_date=rxdate));
set temp_pre_portion_02;
rxend = trans_date + actual_duration - 1 ;
format rxend date9.;
run;

proc sort data= temp_pre_portion_02 ; by ATC3 p_id DESCENDING rxdate; run;

*this is for getting next rxdate;
data temp_pre_portion_02b;
set temp_pre_portion_02;
by ATC3 p_id DESCENDING rxdate;

next_rxdate = lag(rxdate);
format next_rxdate date9.;

if first.p_id then next_rxdate=.;

run;

proc sort data= temp_pre_portion_02b; by ATC3 p_id rxdate; run;

/*define episode begin and end*/
data temp_pre_portion_02c;
set temp_pre_portion_02b ;
by ATC3  p_id  rxdate;  *trans_date has been renamed to rxdate;
retain episnum  cum_days_covered;

if first.p_id then do;
	episnum=0;
	cum_days_covered=0;
end;

episnum = episnum+1;
episbeg=rxdate;
format episbeg date9.;

if last.p_id ^=1 then do;
	episend =min(rxend, next_rxdate-1 );
end;
else if last.p_id then do;
	episend=min(rxend, index_date-1);
end;
format episend date9.;

episode_days_covered= episend - episbeg + 1;

cum_days_covered = cum_days_covered + episode_days_covered;

run;

*rsubmit;
proc sql;
create table pro_days_covered_by_ATC3 as

select ATC3, p_id, sum(episode_days_covered) as days_covered, 
avg(total_n_days) as total_n_days, 
sum(episode_days_covered)/avg(total_n_days) as pro_days_covered format percent8.2

from temp_pre_portion_02c
group by ATC3, p_id
order by ATC3, p_id;

quit;

*rsubmit;
ods listing close;
ods html body="/fs219.1/Sales & Account Management/2015/Belgium_PA/01 Data/excel_out/pro_days_covered_by_ATC3_&n_days_preindex.days.xls";
proc means data = pro_days_covered_by_ATC3 min p5 p10 p25 median p75 p90 p95  max;
by ATC3;
var pro_days_covered;
run;
ods html close;
ods listing;

%if %upcase(&clean_tempdata.) = YES %then %do;

proc datasets LIBRARY=tempwork;
delete
temp_pre_portion_01
temp_20ATC3_1stday
temp_pre_portion_02
temp_pre_portion_02b
temp_pre_portion_02c
run;quit;

%end;



%mend proportion_days_by_ATC3;

*rsubmit;
%proportion_days_by_ATC3(pre_pdc_data01, 60, clean_tempdata=yes);
*save to permanent data -- go with 60 days options;
data BE.pro_days_covered_ATC3_60days;
set pro_days_covered_by_ATC3;
run;

*rsubmit;
proc sql;
select ATC3, count(distinct p_id) from as p_count BE.pro_days_covered_ATC3_60days group by ATC3
;quit;  


*other options;
%proportion_days_by_ATC3(pre_pdc_data01, 30, clean_tempdata=yes);
%proportion_days_by_ATC3(pre_pdc_data01, 90, clean_tempdata=yes);



/*Now put together all 20 ATC3*/
*rsubmit;
data pre_pdc_data01_mod;
set pre_pdc_data01;
ATC3="20 ATC3";
run;
*rsubmit;
%proportion_days_by_ATC3(pre_pdc_data01_mod, 60, clean_tempdata=no);
*save to permanent data -- go with 60 days options;
*rsubmit;
data BE.pro_days_covered_ALL_60days;
set pro_days_covered_by_ATC3;
run;


*rsubmit;
%proportion_days_by_ATC3(pre_pdc_data01_mod, 30, clean_tempdata=no);
*rsubmit;
%proportion_days_by_ATC3(pre_pdc_data01_mod, 90, clean_tempdata=no);





