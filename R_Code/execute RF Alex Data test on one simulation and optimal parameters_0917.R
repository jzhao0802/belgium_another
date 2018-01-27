

#using Yan's covariate list


rm(list=ls(all=TRUE))

#initial R programs for random forest model 
library("randomForest")
library("glmnet")
library("doParallel")
library(xlsx)
source("D:\\jzhao\\Belgium\\R_Code\\BE Project Functions v2 update.R")    #!!! please change to your address


#covar_list <- as.vector(read.csv('D:\\jzhao\\Belgium\\Model_Data\\Yan_list.csv', head=T)[, 1])
covar_df <- as.vector(read.csv('D:\\jzhao\\Belgium\\Model_Data\\Covar_list_v2.csv', head=T)) #465 7
covar_df<- read.xlsx('D:\\jzhao\\Belgium\\Model_Data\\Covar_list_v2.xlsx', sheetName='Covar_list', rowIndex=NULL,
                     startRow=1, endRow=438,
                     as.data.frame=TRUE, header=TRUE, colClasses=NA,
                     keepFormulas=FALSE)
covar_list <- as.vector(covar_df[covar_df[, 2]==1, 1]) #391

simulations <- 4       #changeable
training_portion<- 0.2   #changeable
validation_portion<- 0.2 #changeable

sampling_vector<- c(training_portion, validation_portion)
sampling_vector_forLR <- c(0.3, 0)

model_output_dir<- "D:\\jzhao\\Belgium\\Model_Output"       #changeable
rawdata_path<- "D:\\jzhao\\Belgium\\Model_Data"#changeable
rawdata_file<- "statins_pat_v3.csv"                                        #changeable
SeedV<-c(8790, 625699, 561123, 4377881, 1564, 7790325, 98974, 37536615,643, 29117)    #changeable
setwd(model_output_dir)
raw_data <- read.csv(paste(rawdata_path,"/",rawdata_file,sep=""),header=TRUE)
raw_data_yan <- raw_data[, match(covar_list, names(raw_data))] #[1] 65087   419

#check the missing data and delete the corresponding rows
# 1.
missNum_byRow <- apply(apply(raw_data_yan, 1, is.na), 2, sum) 
sum(missNum_byRow > 0 )#52 rows
raw_data_yan_2 <- raw_data_yan[missNum_byRow==0,] #[1] 65035   419
missRows <- raw_data_yan[missNum_byRow>0,]
#check the missing data
missNum <- apply(apply(raw_data_yan_2, 1, is.na), 2, sum) 
sum(missNum > 0)
# 2.delete the patients whose pat_gender_0_Max=999 or pat_gender_0_Max+pat_gender_1_Max=2
# and remove the pat_gender_1_Max just keep pat_gender_0_Max
#raw_data_yan_2_2 <- raw_data_yan_2[(raw_data_yan_2$pat_gender_0_Max+raw_data_yan_2$pat_gender_1_Max)==1 , -match("pat_gender_1_Max", covar_list)] #61668 418
#covar_list <- setdiff(covar_list, 'pat_gender_1_Max')


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
covar_del_1 <- binary_list[pos_num<10]
covar_del <- c(covar_del_1, cons_var) #33
#create the clean data 1
raw_data_yan_3 <- raw_data_yan_2[, match(setdiff(covar_list, covar_del), covar_list)]#[1] [1] 65035   358
#raw_data_yan_3 <- subset(raw_data_yan_2_2, select=-(covar_del))

#transform the binray var into factor
temp=lapply(setdiff(binary_list, covar_del), function(v){
    raw_data_yan_3[, v] <- factor(raw_data_yan_3[, v])
})
for(v in setdiff(binary_list, covar_del)){
    raw_data_yan_3[, v] <- factor(raw_data_yan_3[, v])
    
}

resp_var <- 'persistence_6m'
#raw_data_yan_3$response <- raw_data[missNum_byRow == 0 & (raw_data$pat_gender_0_Max+raw_data$pat_gender_1_Max)==1 , resp_var] #[1] 65035   320
raw_data_yan_3$response <- raw_data[missNum_byRow == 0, resp_var]
important_post_var <- c('market_doc_gender1_Max',
                        'market_doc_gender2_Max',
                        'market_spec11_Max',
                        'market_spec22_Max',
                        'market_spec90_Max')
var_list_all <- names(raw_data)
post_doc_var <- grep('^market_doc|market_spec',var_list_all, value=T) #28
post_mol_doc_var <- grep('^market|^mol_\\w+_Max$', var_list_all, value=T) #33
raw_data_yan_4 <- cbind(raw_data_yan_3, apply(raw_data[missNum_byRow == 0 , post_doc_var], 2, as.factor)) #[1] 65035   387

#index_var <- grep('^doc_specia|pat_gender', names(raw_data_yan_3), value=T)
#raw_data_yan_3_delIndxDate <- raw_data_yan_3[, -match(index_var, names(raw_data_yan_3))]

check_miss_value <- apply(apply(raw_data_yan_4[,], 1, is.na), 2, sum)
sum(check_miss_value>0)

#check the frequency table for  all the 7 packsiz
freq_1 <- unlist(lapply(grep('packsiz', names(raw_data), value=T), function(v)sum(raw_data[, v]==1)))
names(freq_1) <- grep('packsiz', names(raw_data), value=T)
perc_1 <- freq_1/nrow(raw_data)
freq_2 <- unlist(lapply(grep('packsiz', names(raw_data_yan_2), value=T), function(v)sum(raw_data_yan_2[, v]==1)))
names(freq_2) <- grep('packsiz', names(raw_data_yan_2), value=T)
perc_2 <- freq_2/nrow(raw_data_yan_2)
freq <- rbind(withMiss = freq_1, withoutMiss = freq_2, withMiss_perc = perc_1, withoutMiss_perc=perc_2)
write.csv(t(freq), 'Frequency_table_for_packsize_v2.csv', row.names=T)
#NOV13 output the packsize patient rows
packsize_data <- raw_data[, c('p_id', grep('packsiz', names(raw_data), value=T))]
var_psize <- grep('packsiz', names(raw_data), value=T)
psize <- apply(packsize_data,1, function(r){
            return(var_psize[which(r[-1]==1)])
        })
packsize_data <- data.frame(p_id=raw_data$p_id, packsize_start=gsub('(^\\w+_\\w+_)(\\d+$)', '\\2', psize, perl=T))
write.csv(packsize_data, 'Packsize_data_Nov13.csv', row.names=F)

#check the packsize
raw_data[raw_data$p_id==15163, grep('packsiz', names(raw_data), value=T)]
packsize_data[packsize_data$p_id==15163, 2]
simulation=1
all_samples_forsim<- BE_Sampling(raw_data_yan_4, SeedV[simulation], sampling_vector)    
s=1

training_data<- all_samples_forsim[[s]][[1]]
validation_data<- all_samples_forsim[[s]][[2]]
test_data <- all_samples_forsim[[s]][[3]]

full_training_data<- rbind(training_data, validation_data)


#add the 5 important post_index covariates

logF <- paste(model_output_dir, "\\Log_RF", "_Yan'slist_test_covar_v2_addPostDoc_gender_spec",".csv", sep='')

timeStart <- proc.time()

full_tr_response <- as.factor(full_training_data[,"response"])
full_RF_fit <- randomForest(x=full_training_data[,setdiff(names(full_training_data),c("response"))],y=full_tr_response, 
                            ntree=300,
                            mtry=50,
                            nodesize=10, 
                            importance=T)

imp <- importance(full_RF_fit)
write.csv(imp, 'covar_importance_covar_v2_addPostDoc_gender_spec.csv', row.names=T)
cat(file=logF, append=TRUE, 'Simulation ', 'apply the optimal parameter in (training+Validation) to get the final model end\n') 

full_tr_pred<- predict(full_RF_fit, full_training_data, type='prob')
full_tr_auc<- auc(full_training_data$response, full_tr_pred[,2])

test_pred <- predict(full_RF_fit, test_data,  type='prob')
test_auc <- auc(test_data$response, test_pred[,2])
cat(file=logF, append=TRUE, 'Simulation ', 'apply the final model into test data end and start to get the test performance measure \n') 


#calculate true positive, false positve, true negative and false negative -- using the 0.5 as a threshold

test_perf<-Cal_Perf_Measure(test_pred[,2],0.5,test_data)


cat(file=logF, append=TRUE, 'Simulation ', 'get the test performance measure end\n') 
saved_sim_outcome<-c(full_tr_auc, test_auc, test_perf) 
timeEnd <- proc.time()
execution_time<- (timeEnd-timeStart)[3]/60
execution_time
write.csv(saved_sim_outcome, 'model_result_for_covar_v2_addPostDoc_gender_spec.csv', row.names=F)

#profiling
#test_data

#add pat_gender_1_Max
bucket <- cut(test_pred[, 2], breaks=4, include.lowest=T,right=F, labels=1:4)
test_data_1 <- apply(test_data[,], 2, function(i)as.numeric(as.vector(i)))
profile <- t(aggregate(cbind(prediction_value=test_pred[, 2], test_data_1), by=list(bucket), mean))
write.csv(profile, 'Profile_allVar_withoutPat_gender.csv', row.names=T)



#add pat_gender_1_Max when profiling the patient
pat_gender <- raw_data$pat_gender_1_Max[missNum_byRow==0]
raw_data_yan_4$pat_gender_1_Max <- as.factor(ifelse(pat_gender==999, NA, pat_gender))
simulation=1
all_samples_forsim_1<- BE_Sampling(raw_data_yan_4, SeedV[simulation], sampling_vector)    
s=1

training_data_1<- all_samples_forsim_1[[s]][[1]]
validation_data_1<- all_samples_forsim_1[[s]][[2]]
test_data_1 <- all_samples_forsim_1[[s]][[3]]

full_training_data<- rbind(training_data_1, validation_data_1)

bucket_3 <- cut(test_pred[, 2], breaks=3, include.lowest=T,right=F, labels=1:3)
bucket_2 <- cut(test_pred[, 2], breaks=2, include.lowest=T,right=F, labels=1:2)
test_data_1 <- apply(test_data_1[,], 2, function(i)as.numeric(as.vector(i)))
profile_1 <- t(aggregate(cbind(prediction_value=test_pred[, 2], test_data_1), by=list(bucket_2), function(x){mean(x, na.rm=T)}))
write.csv(profile_1, 'Profile_allVar_withPat_gender_predictionScore_bucket2.csv', row.names=T)


or <- unlist(lapply(binary_list, function(v){
    tb <- table(full_training_data[, v], full_training_data$response)
    
    covar1_response1 <- tb[2,2]
    covar0_response1 <- tb[1,2]
    covar1_response0 <- tb[2,1]
    covar0_response0 <- tb[1,1]
    contigency_table<- matrix(c(covar1_response1, covar0_response1, covar1_response0, covar0_response0), nc=2, byrow=T)
    association_test<- fisher.test(contigency_table , alternative = "two.sided")
    odds_ratio<- as.vector(association_test$estimate)
    
}))
var_del <- binary_list[is.infinite(or)]

    