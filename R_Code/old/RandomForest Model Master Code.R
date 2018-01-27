

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
source("D:/Project Files/Belgium Predictive Modeling/R functions/BE Project Functions.R")    #!!! please change to your address


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<01. General paramters<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulations <- 10       #changeable
training_portion<- 0.2   #changeable
validation_portion<- 0.2 #changeable

sampling_vector<- c(training_portion, validation_portion)

work_dir<- "D:/Project Files/Belgium Predictive Modeling/newdata Aug28"       #changeable
rawdata_path<- "D:/Project Files/Belgium Predictive Modeling/newdata Aug28"   #changeable
rawdata_file<- "statins_pre_model.csv"                                        #changeable
response_varname<- "persistence_6m"                                            #changeable

#sampling functions
SeedV<-c(8790, 625699, 561123, 4377881, 1564, 7790325, 98974, 37536615,643, 29117)    #changeable

setwd(work_dir)
raw_data <- read.csv(paste(rawdata_path,"/",rawdata_file,sep=""),header=TRUE)
# head(raw_data)
# length(unique(raw_data$p_id))


#random forest model parameters
# par_all_scenarios<- par_all_scenarios<- expand.grid(ntree=c(500,1000,2000),mtry=c(3,5,10),nodesize=c(1,5,10))
par_all_scenarios<- par_all_scenarios<- expand.grid(ntree=500,mtry=c(3,5),nodesize= 20 )         #changeable
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<End of defined general paramters<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<02. Pre-processing Data<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#pre-process variables --
#covert nominal variables to factors 

var_to_exclude<-c("p_id", "persistence_3m", "persistence_6m", "persistence_9m")         #changeable

#changeable
non_ordinal_factors<- c("P_gender", "index_doc_spec", "index_statin_molecule","pre_OAP", "pre_ace_inhibitors","pre_beta_blockers", "pre_diabetes_insulin",
                        "pre_diabetes_other","pre_other_drugs")

for(vname in non_ordinal_factors){
  
  raw_data[,vname]<- factor(raw_data[,vname], ordered=FALSE)
  
}
#create response variable, default name for response variable is "response"
raw_data$response<- raw_data[,response_varname]

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<End of Pre-processing Data<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<





#>>>>>>>>>>>>>>>>>>>>>>>>>>>03. Sampling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

all_samples_forsim<- BE_Sampling(raw_data, SeedV, sampling_vector)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>End of sampling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>>>>>>>>04. Modeling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


Model_Outcome<- Run_BE_RF_Model(all_samples_forsim, var_to_exclude, par_all_scenarios, simulations, work_dir)

#show how much time the RF models have taken...
Model_Outcome[[2]]

#>>>>>>>>>>>>>>>>>>>>>>>>>>>End of Modeling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



