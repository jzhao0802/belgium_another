
#==========================================================================================
#	Project: Predicting patient persistence for Belgium 

#	Part II: Covariates preparation(selection and transformation) and Modeling using machine learning 

#	Develop time: 09/01/2015 - .

#	Developer: Yan Xue & Jie Zhao
#==========================================================================================

#functionalized random forest model
#10 simulation -- 
#20% training samples  -- a flexible parameter 
#20% validation samples  -- a flexible parameters
#60% test samples
#training + validation -->to build final model with optimal parameters for random forests
#apply the model to 70% test samples 


#remove all the objects in the currently active environment
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
sampling_vector_forLR <- c(0.4, 0)

model_output_dir<- "D:\\jzhao\\Belgium\\Model_Output"       #changeable -- directory where you plan to save your model output
rawdata_path<- "D:\\jzhao\\Belgium\\Model_Data"#changeable  --directory where you save your raw data in
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
covar_df<- read.xlsx('D:\\jzhao\\Belgium\\Model_Data\\Covar_list_v2.xlsx', sheetName='Covar_list', rowIndex=NULL,
                     startRow=1, endRow=438,
                     as.data.frame=TRUE, header=TRUE, colClasses=NA,
                     keepFormulas=FALSE)
covar_list <- as.vector(covar_df[covar_df[, 2]==1, 1]) #391

head(variable_screen_list)
names(variable_screen_list)
covar_list <- as.vector(covar_df[covar_df[, 2]==1, 1])

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

#delete the covariates whose pos case < 10, 100 if run the logisctic regression
covar_del_1 <- binary_list[pos_num<10]
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


#>>>>>>>>>>>>>>>>>>>>>>>>>>>03. Sampling>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Model <- "RF" # which model would you run? 'RF'-random forest, 'LR'-logistic regression, 'Stw'-stepwise regression

if (Model == "RF"){
    all_samples_forsim<- BE_Sampling(raw_data_clean, SeedV, sampling_vector)    
}else{
    all_samples_forsim<- BE_Sampling(raw_data_clean, SeedV, sampling_vector_forLR)    
    
}

#QC for the sampling
#QC for the sample number in each dataset
dim_check <- numeric()
for(i in all_samples_forsim){
    temp_dim <- unlist(lapply(i, function(dt){
        return(nrow(dt))
    }))
    dim_check <- rbind(dim_check, temp_dim)
}
dim_check

#QC whether the response rate in each dataset are the same.
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
Trail <- 2  #model trial number
logF <- paste(model_output_dir, "\\Log_", Model, "_", Trail,".csv", sep='')  #the model log file

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
model_output <- get_model_result(all_samples_forsim, par_all_scenarios, simulations, model_output_dir, 0.5, 2, "RF")


