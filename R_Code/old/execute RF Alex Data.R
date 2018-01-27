



#functionalized random forest model
#10 simulation -- 
#20% training samples  -- a flexible parameter 
#20% validation samples  -- a flexible parameters
#60% test samples
#training + validation -->to build final model with optimal parameters for random forests
#apply the model to 70% test samples 


#for the type of Continue
rm(list=ls(all=TRUE))

#initial R programs for random forest model 
library("randomForest")
library("glmnet")
library("doParallel")
library(xlsx)
source("D:\\jzhao\\Belgium\\R_Code\\BE Project Functions.R")    #!!! please change to your address




#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<01. General paramters<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulations <- 3       #changeable
training_portion<- 0.2   #changeable
validation_portion<- 0.2 #changeable

sampling_vector<- c(training_portion, validation_portion)

model_output_dir<- "D:\\jzhao\\Belgium\\Model_Output"       #changeable
rawdata_path<- "D:\\jzhao\\Belgium\\Model_Data"#changeable
rawdata_file<- "statins_pat_v2.csv"                                        #changeable
response_varname<- "persistence_6m"                                            #changeable

#sampling functions
SeedV<-c(8790, 625699, 561123, 4377881, 1564, 7790325, 98974, 37536615,643, 29117)    #changeable

setwd(model_output_dir)
raw_data <- read.csv(paste(rawdata_path,"/",rawdata_file,sep=""),header=TRUE)
#get the response variable when modeling
resp_var <- 'persistence_6m'
raw_data$response<- raw_data[, resp_var]

dim(raw_data)


#random forest model parameters
# par_all_scenarios<- par_all_scenarios<- expand.grid(ntree=c(500,1000,2000),mtry=c(3,5,10),nodesize=c(1,5,10))
par_all_scenarios<- par_all_scenarios<- expand.grid(ntree=c(800,1500,2500),mtry=c(10, 20, 30),nodesize=c(10,20,30))        #changeable
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<End of defined general paramters<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<02. Pre-processing Data<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#pre-process variables --
variable_screen_list<- read.xlsx(paste(rawdata_path, "Covariate_list_addDesc.xlsx", sep='\\'), sheetName='Covar_list', rowIndex=NULL,
                                 startRow=1, endRow=450,
                                 as.data.frame=TRUE, header=TRUE, colClasses=NA,
                                 keepFormulas=FALSE)
head(variable_screen_list)
names(variable_screen_list)

drop_var_list<- variable_screen_list[variable_screen_list$Input1==0,1]
#get the initial input variable list
var_list <- setdiff(colnames(raw_data), drop_var_list)

#check the constant variable and non-ordinal variable and then drop

levels <- unlist(lapply(var_list, function(v){
    var <- raw_data[, v]
    return(length(levels(as.factor(as.character(var)))))
}))
cons_var <- var_list[levels ==1]
#[1] "atc3_A10X_Max" "atc3_J01X_Max"

non_ordinal_list <- var_list[levels > 10]
#[1] "persistence"               "cnt_tx_12m"                "cnt_tx_days_12m"           "cnt_docs_12m"              "cnt_specialties_12m"       "cnt_pharmacies_12m"       
#[7] "cnt_fcc_12m"               "cnt_atc_12m"               "cnt_otc_12m"               "mean_price_12m"            "sum_spending_12m"          "mean_tx_type_12m"         
#[13] "mean_dcivos_12m"           "mean_units_12m"            "mean_doc_gender_12m"       "pre_OAP_cost"              "pre_ace_inhibitors_cost"   "pre_beta_blockers_cost"   
#[19] "pre_diabetes_insulin_cost" "pre_diabetes_other_cost"   "pre_other_drugs_cost"      "pre_OAP_Trx"               "pre_ace_inhibitors_Trx"    "pre_beta_blockers_Trx"    
#[25] "pre_diabetes_insulin_Trx"  "pre_diabetes_other_Trx"    "pre_other_drugs_Trx"  


conti_var_flag <- unlist(lapply(var_list, function(v){
    var <- raw_data[, v]
    return(!is.integer(var))
}))
conti_var_list <- var_list[conti_var_flag]

# further_to_deleted<- variable_screen_list[!is.na(variable_screen_list[,"X..of.positive"]) & variable_screen_list[,"X..of.positive"]<100,1]


#drop the other persistence variables
persis_vars <- grep("persistence", var_list, value=T)
#get the clean data to move to the modeling part
raw_data_clean<- raw_data[,setdiff(var_list,c(persis_vars, cons_var, non_ordinal_list))]
dim(raw_data_clean)#[1] 65087   321
names(raw_data_clean)


#persistence_3m <- ifelse((raw_data$persistence)/30>=3,1,0)
#persistence_6m <- ifelse((raw_data$persistence)/30>=6,1,0)
#persistence_9m <- ifelse((raw_data$persistence)/30>=9,1,0)
# table(persistence_3m)/length(persistence_3m)
# table(persistence_6m)/length(persistence_6m)
# table(persistence_9m)/length(persistence_9m)

#choose one of them as reponse variable






#covert nominal variables to factors 

#changeable
# non_ordinal_factors<- c("P_gender", "index_doc_spec", "index_statin_molecule","pre_OAP", "pre_ace_inhibitors","pre_beta_blockers", "pre_diabetes_insulin",
 #                        "pre_diabetes_other","pre_other_drugs")
#non_ordinal_factors %in% names(raw_data_clean)
#non_ordinal_factors<- variable_screen_list[variable_screen_list[,2]==1 & !is.na(variable_screen_list[,5]),1]
#non_ordinal_factors <- variable_screen_list[variable_screen_list[,2]==1 & length(levels(raw_data[, variable_screen_list]))>2]

for(vname in names(raw_data_clean)){
  
  raw_data_clean[,vname]<- factor(raw_data_clean[,vname], ordered=FALSE)
  
}
#create response variable, default name for response variable is "response"

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<End of Pre-processing Data<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

head(raw_data_clean)
dim(raw_data_clean)
#[1] 65087   307

#>>>>>>>>>>>>>>>>>>>>>>>>>>>03. Sampling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

all_samples_forsim<- BE_Sampling(raw_data_clean, SeedV, sampling_vector)
#QC
pt_num_list <- lapply(all_samples_forsim, founction(X){
    X1 <- X
    temp_dim <- unlist(lapply(X1, function(dt){
        return(nrow(dt))
    }))
    return(temp_dim)
})
library(plyr)
ldply(pt_num_list, quickdf)

dim_check <- numeric()
for(i in all_samples_forsim){
    temp_dim <- unlist(lapply(i, function(dt){
        return(nrow(dt))
    }))
    dim_check <- rbind(dim_check, temp_dim)
}
dim_check
#QC the response rate in each dataset
resp_rate_check <- numeric()
for(i in all_samples_forsim){
    temp_resp_rate <- unlist(lapply(i, function(dt){
        rate <- sum(as.numeric(as.vector(dt$response)))/nrow(dt)
        return(rate)
    }))
    resp_rate_check <- rbind(resp_rate_check, temp_resp_rate)
}
resp_rate_check
#>>>>>>>>>>>>>>>>>>>>>>>>>>>End of sampling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>>>>>>>>04. Modeling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#all_samples_forsim, par_all_scenarios, simulations,out_path
#out_path <- model_output_dir
Model_Outcome<- Run_BE_RF_Model(all_samples_forsim, par_all_scenarios, simulations, model_output_dir)

#show how much time the RF models have taken...
Model_Outcome[[2]]

#>>>>>>>>>>>>>>>>>>>>>>>>>>>End of Modeling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Model_Outcome_LR <- Run_BE_LR_Model(all_samples_forsim, simulations, model_output_dir, cutoff_score, Trail)
#show how much time the LR models have taken...
Model_Outcome[[2]]

Model_Outcome_Stepwise <- Run_BE_StepWise_Model(all_samples_forsim, simulations, model_output_dir, cutoff_score, Trail)
#show how much time the stepwise models have taken...
Model_Outcome[[2]]

