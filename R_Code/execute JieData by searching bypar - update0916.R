




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
library(snow)
library(xlsx)
source("D:\\jzhao\\Belgium\\R_Code\\BE Project Functions v2 update.R")    #!!! please change to your address




#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<01. General paramters<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
simulations <- 3       #changeable
training_portion<- 0.2   #changeable
validation_portion<- 0.2 #changeable

sampling_vector<- c(training_portion, validation_portion)
sampling_vector_forLR <- c(0.3, 0)

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

design_file_path <- "D:/yxue/BE/RF"   #changeable
design_file_name <- "RF_Design_5_5_5.csv"                     #changeable
#changeable
#ntree<-c(800, 1000,1500,2000, 2500)
#mtry <-c(10, 20, 30,50, 100)
#nodesize<-c(10,20,30, 50, 100)
ntree <- c(300, 500, 800, 1000, 1500)
mtry <- c(10, 20, 30, 50, 80)
nodesize <- c(5, 10, 20, 30, 50)
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

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<02. Pre-processing Data<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#pre-process variables --
covar_list <- as.vector(covar_df[covar_df[, 2]==1, 1])

simulations <- 4       #changeable
training_portion<- 0.2   #changeable
validation_portion<- 0.2 #changeable

sampling_vector<- c(training_portion, validation_portion)
sampling_vector_forLR <- c(0.3, 0)

model_output_dir<- "D:\\jzhao\\Belgium\\Model_Output"       #changeable
rawdata_path<- "D:\\jzhao\\Belgium\\Model_Data"#changeable
rawdata_file<- "statins_pat_v2.csv"                                        #changeable
SeedV<-c(8790, 625699, 561123, 4377881, 1564, 7790325, 98974, 37536615,643, 29117)    #changeable
setwd(model_output_dir)
raw_data <- read.csv(paste(rawdata_path,"/",rawdata_file,sep=""),header=TRUE)
raw_data_yan <- raw_data[, match(covar_list, names(raw_data))] #[1] 65087   391


#check the missing data and delete the corresponding rows
missNum_byRow <- apply(apply(raw_data_yan, 1, is.na), 2, sum) 
sum(missNum_byRow > 0 )#52 rows
raw_data_yan_2 <- raw_data_yan[missNum_byRow==0,] #[1] 65035   391
#check the missing data
missNum <- apply(apply(raw_data_yan_2, 1, is.na), 2, sum) 
sum(missNum > 0)


#check the binary variables with too small positive case
levels <- unlist(lapply(as.vector(covar_list), function(v){
    var <- raw_data_yan_2[, v]
    return(length(levels(as.factor(as.character(var)))))
}))
cons_var <- covar_list[levels ==1]
#[1] "atc3_A10X_Max" "atc3_J01X_Max"

binary_list <- covar_list[levels ==2] # are to be transformed as factor 
pos_num <- unlist(lapply(binary_list, function(v){
    pos_num <- sum(raw_data_yan_2[,v])
    return(pos_num)
}))
#delete the covariates whose pos case < 10
covar_del_1 <- binary_list[pos_num<100]
covar_del <- c(covar_del_1, cons_var)
#create the clean data 1
raw_data_yan_3 <- raw_data_yan_2[, match(setdiff(covar_list, covar_del), covar_list)]#[1] 65035   358
#raw_data_yan_3 <- subset(raw_data_yan_2, select=-(covar_del))

#transform the binray var into factor
temp=lapply(setdiff(binary_list, covar_del), function(v){
    raw_data_yan_3[, v] <- factor(raw_data_yan_3[, v])
})
for(v in setdiff(binary_list, covar_del)){
    raw_data_yan_3[, v] <- factor(raw_data_yan_3[, v])
    
}

resp_var <- 'persistence_6m'
raw_data_yan_3$response <- raw_data[missNum_byRow == 0 , resp_var] #[1] 65035   320
important_post_var <- c('market_doc_gender1_Max',
                        'market_doc_gender2_Max',
                        'market_spec11_Max',
                        'market_spec22_Max',
                        'market_spec90_Max')
var_list_all <- names(raw_data)
post_doc_var <- grep('^market_doc|market_spec',var_list_all, value=T) #28
post_mol_doc_var <- grep('^market|^mol_\\w+_Max$', var_list_all, value=T) #33
raw_data_yan_4 <- cbind(raw_data_yan_3, apply(raw_data[missNum_byRow == 0 , post_doc_var], 2, as.factor)) #[1] 65035   353

check_miss_value <- apply(apply(raw_data_yan_4[, post_doc_var], 1, is.na), 2, sum)
sum(check_miss_value>0)


#all_samples_forsim<- BE_Sampling(raw_data_yan_4, SeedV, sampling_vector)    
#s=1

#training_data<- all_samples_forsim[[s]][[1]]
#validation_data<- all_samples_forsim[[s]][[2]]
#test_data <- all_samples_forsim[[s]][[3]]

#full_training_data<- rbind(training_data, validation_data)


#add the 5 important post_index covariates

#logF <- paste(model_output_dir, "\\Log_RF", "_covar_v2_addDocMolPost",".csv", sep='')
#>>>>>>>>>>>>>>>>>>>>>>>>>>>03. Sampling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Model <- "RF"

if (Model == "RF"){
    all_samples_forsim<- BE_Sampling(raw_data_clean, SeedV, sampling_vector)    
}else{
    all_samples_forsim<- BE_Sampling(raw_data_clean, SeedV, sampling_vector_forLR)    
    
}

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
Trail <- 2
logF <- paste(model_output_dir, "\\Log_", Model, "_", Trail,".csv", sep='')

if(file.exists(logF)){
    file.remove(logF)
}


get_model_result <- function(all_samples_forsim, par_all_scenarios, simulations, model_output_dir, cutoff_score, Trail, Model){
    
    if (Model == "RF"){
        #all_samples_forsim, par_all_scenarios, simulations,out_path
        #out_path <- model_output_dir
        
        Model_Outcome<- Run_BE_RF_Model_byparm(all_samples_forsim, par_all_scenarios, simulations, model_output_dir, cutoff_score, Trail, logF)
        
    }else if(Model == "LR"){
        
        Model_Outcome <- Run_BE_LR_Model(all_samples_forsim, simulations, model_output_dir, cutoff_score, Trail, logF)
        
    }else if(Model == "Stw"){
        
        Model_Outcome <- Run_BE_StepWise_Model(all_samples_forsim, simulations, model_output_dir, cutoff_score, Trail, logF)
        
    }else{
        stop("Please confirm which model to run!\n")
    }
    return(Model_Outcome)
    
}
#test
#par_all_scenarios <- par_all_scenarios[1,]
#raw_data_clean_test <- raw_data_clean[1:1000,]
get_model_result(all_samples_forsim, par_all_scenarios, simulations, model_output_dir, 0.5, 2, "RF")

#profiling

