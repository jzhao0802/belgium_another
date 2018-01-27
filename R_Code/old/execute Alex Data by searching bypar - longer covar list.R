




#for the type of Continue
rm(list=ls(all=TRUE))

#initial R programs for random forest model 
library("randomForest")
library("glmnet")
library("doParallel")


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<01. General paramters<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulations <- 5       #changeable
training_portion<- 0.2   #changeable
validation_portion<- 0.2 #changeable
source("D:/yxue/BE/BE Project Functions v2.R")
sampling_vector<- c(training_portion, validation_portion)

work_dir<- "D:/yxue/BE"       #changeable
rawdata_path<- "D:/yxue/BE"   #changeable
rawdata_file<- "statins_pat.csv"                                        #changeable
response_varname<- "persistence_6m"                                            #changeable

#sampling functions
SeedV<-c(8790, 625699, 561123, 4377881, 1564, 7790325, 98974, 37536615,643, 29117)    #changeable

setwd(work_dir)
raw_data <- read.csv(paste(rawdata_path,"/",rawdata_file,sep=""),header=TRUE)

dim(raw_data)


#random forest model parameters

design_file_path <- "D:/yxue/BE/RF"   #changeable
design_file_name <- "RF_Design_5_5_5.csv"                     #changeable
#changeable
ntree<-c(800, 1000,1500,2000, 2500)
mtry <-c(10, 20, 30,50, 100)
nodesize<-c(10,20,30, 50, 100)
n.levels<- 5


level_define<- data.frame(Levels=c(1:n.levels),ntree=ntree, mtry = mtry, nodesize=nodesize)

#this part is automated
orig_design_file <-read.csv(paste(design_file_path,"/",design_file_name,sep=""),header=TRUE)
par_all_scenarios<- matrix(NA,nrow=nrow(orig_design_file),ncol=ncol(orig_design_file))
par_all_scenarios<- as.data.frame(par_all_scenarios)
colnames(par_all_scenarios) <- colnames(level_define)[-1]
for(i in 1:ncol(orig_design_file)){  
  par_all_scenarios[,i]<- level_define[match(orig_design_file[,i],level_define$Levels),i+1]  
  }


#


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<02. Pre-processing Data<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#pre-process variables --
variable_screen_list<- read.csv("Covar_list_forChecking (Load in longer list).csv",header=TRUE, stringsAsFactors =FALSE)
head(variable_screen_list)
names(variable_screen_list)
drop_var_list<- variable_screen_list[variable_screen_list$Inclusion==0,1]

further_to_deleted<- variable_screen_list[!is.na(variable_screen_list[,"X..of.positive"]) & variable_screen_list[,"X..of.positive"]<10,1]

# which(!(names(raw_data) %in% variable_screen_list[,1]))
#which(!(variable_screen_list[,1]) %in% names(raw_data))

raw_data<- raw_data[,setdiff(names(raw_data),c(drop_var_list,further_to_deleted))]
dim(raw_data)
names(raw_data)
head(raw_data)

persistence_3m <- ifelse((raw_data$persistence)/30>=3,1,0)
persistence_6m <- ifelse((raw_data$persistence)/30>=6,1,0)
persistence_9m <- ifelse((raw_data$persistence)/30>=9,1,0)
# table(persistence_3m)/length(persistence_3m)
# table(persistence_6m)/length(persistence_6m)
# table(persistence_9m)/length(persistence_9m)

#choose one of them as reponse variable
raw_data$response<- persistence_6m





#covert nominal variables to factors 

var_to_exclude<-c("p_id", "persistence")         #changeable

#changeable
# non_ordinal_factors<- c("P_gender", "index_doc_spec", "index_statin_molecule","pre_OAP", "pre_ace_inhibitors","pre_beta_blockers", "pre_diabetes_insulin",
#                         "pre_diabetes_other","pre_other_drugs")

non_ordinal_factors<- variable_screen_list[variable_screen_list[,2]==1 & !is.na(variable_screen_list[,5]),1]
non_ordinal_factors_to_do<-setdiff(non_ordinal_factors, c(drop_var_list,further_to_deleted))
for(vname in non_ordinal_factors_to_do){
  
  raw_data[,vname]<- factor(raw_data[,vname], ordered=FALSE)
  
}


#delete na
rows_to_keep<- apply(raw_data, 1, function(x){!any(is.na(x))})
length(rows_to_keep)
nrow(raw_data)
sum(rows_to_keep)

raw_data<- raw_data[rows_to_keep,]

#create response variable, default name for response variable is "response"

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<End of Pre-processing Data<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# head(raw_data)
# dim(raw_data)
# names(raw_data)
# 
# "p_id" %in% names(raw_data)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>03. Sampling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

all_samples_forsim<- BE_Sampling(raw_data, SeedV, sampling_vector)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>End of sampling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



#>>>>>>>>>>>>>>>>>>>>>>>>>>>04. Modeling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


Model_Outcome<- Run_BE_RF_Model_byparm(all_samples_forsim, var_to_exclude, par_all_scenarios, simulations, work_dir)

#show how much time the RF models have taken...
Model_Outcome[[2]]

#>>>>>>>>>>>>>>>>>>>>>>>>>>>End of Modeling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

