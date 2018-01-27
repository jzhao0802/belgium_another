

#using Yan's covariate list


rm(list=ls(all=TRUE))

#initial R programs for random forest model 
library("randomForest")
library("glmnet")
library("doParallel")
library(xlsx)
source("D:\\jzhao\\Belgium\\R_Code\\BE Project Functions v2 update snowfalls0921.R")    #!!! please change to your address


#covar_list <- as.vector(read.csv('D:\\jzhao\\Belgium\\Model_Data\\Yan_list.csv', head=T)[, 1])
covar_df <- as.vector(read.csv('D:\\jzhao\\Belgium\\Model_Data\\Covar_list_v2.csv', head=T)) #465 7
covar_df<- read.xlsx('D:\\jzhao\\Belgium\\Model_Data\\Covar_list_v2.xlsx', sheetName='Covar_list', rowIndex=NULL,
                     startRow=1, endRow=438,
                     as.data.frame=TRUE, header=TRUE, colClasses=NA,
                     keepFormulas=FALSE)
covar_list <- as.vector(covar_df[covar_df[, 2]==1, 1]) #391

simulations <- 1       #changeable
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

simulation=1
all_samples_forsim<- BE_Sampling(raw_data_yan_4, SeedV[simulation], sampling_vector)    
s=1

training_data<- all_samples_forsim[[s]][[1]]
validation_data<- all_samples_forsim[[s]][[2]]
test_data <- all_samples_forsim[[s]][[3]]

full_training_data<- rbind(training_data, validation_data)

miss_check_tr <- apply(apply(data, 2, is.na), 2, sum)



#logistic regression

#check the odds ration Inf
var_list_1 <- names(full_training_data)
levels <- unlist(lapply(var_list_1, function(v){
    return(is.factor(full_training_data[, v]))
}))
levels_forDel <- unlist(lapply(var_list_1, function(v){
    length(levels(full_training_data[, v]))
}))
cont_var_fullTrain <- var_list_1[levels_forDel==0]
binary_list <- var_list_1[levels]
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


#check the correlation between covariates
subFolder <- 'Sep23'
data <- as.data.frame(apply(full_training_data, 2, function(x){as.numeric(as.vector(x))}))
get_corr <- function(data, thresh){
    corr_matrix <- cor(data)
    #write.xlsx(corr_matrix, 'Correlation Result.xlsx', sheetName='matrix', row.names=T, append=T, showNA=T)
    write.csv(corr_matrix, paste(subFolder, '\\Correlation Result full for LR.csv', sep=''), row.names=T)
    
    pdf(paste(subFolder, '\\Correlation_matrix_for_LR.pdf', sep=''), height=6, width=8, pointsize=12)
    #heatmap(corr_matrix)
    dev.off()
    
    add_r <- numeric()
    high_corr <- numeric()
    for(i in 1:(ncol(data)-1)){
        for(j in (i+1):ncol(data)){
            if(!is.na(corr_matrix[i, j]) & abs(corr_matrix[i, j]) > thresh){
                add_r <- c(add_r, add=T)
                high_corr <- rbind(high_corr, c(Var1=names(data)[i], Var2=names(data)[j], Corr=corr_matrix[i, j]))
            }
        }
    }
    high_corr <- as.data.frame(high_corr)
    #write.xlsx(high_corr, 'Correlation Result for LR 0917.xlsx', sheetName='high_corr', row.names=T, append=T, showNA=T)
    write.csv(high_corr, paste(subFolder, '\\High Correlation Result for LR.csv', sep=''), row.names=T)
    return(high_corr)
}
high_corr_result <- get_corr(data, 0.5)

var_del2 <- as.vector(high_corr_result[as.numeric(as.vector(high_corr_result[, 3]))==1, 2])
var_del_all<- unique(c(var_del, var_del2) )
var_del_all <- c(var_del, var_del2, del_fromFit)

logF <- paste(model_output_dir, '\\', subFolder, "\\Log_RF", "_Yan'slist_test_covar_v2_addDocPost_LG",".csv", sep='')
full_training_data2 <- full_training_data[, -match(var_del_all, names(full_training_data))]
test_data2 <- test_data[, -match(var_del_all, names(test_data))]

timeStart <- proc.time()
response<-as.factor(full_training_data2[,"response"])

var_del_test <- unlist(lapply(var_del_all, function(v){
    return(grep(v, names(full_training_data2), value=T))
}))


fit<- glm(response~., data=full_training_data2, family=binomial)
cat(file=logF, append=TRUE, 'Simulation ',  i, "training model in training data end!\n") #added by Jie


LR_fit<-glm(response~.,data=full_training_data2,family="binomial")
LR_coef<-as.data.frame(coef(summary(fit)))
LR_out<-data.frame(rownames(LR_coef),LR_coef, exp(LR_coef[1]))

#get training auc
training_pred<- predict(fit, full_training_data2, type="response")
training_auc<- auc(as.numeric(as.vector(full_training_data2$response)) , training_pred)
#get test auc
test_pred <- predict(fit, test_data, type='response')
test_auc<- auc(as.numeric(as.vector(test_data$response)) , test_pred)
test_perf<-Cal_Perf_Measure(test_pred,0.5,test_data)
saved_sim_outcome <- c(training_auc, test_auc, test_perf, execution_time)
timeEnd <- proc.time()
execution_time<- (timeEnd-timeStart)[3]/60
execution_time
write.csv(saved_sim_outcome, paste(subFolder, 'model_result_for_covar_v2_addDocPost_LR.csv', sep=''), row.names=F)


bucket <- cut(x, breaks=4, include.lowest=T,right=F, labels=1:4)
raw_data_yan_3 <- apply(raw_data_yan_2[,], 2, function(i)as.numeric(as.vector(i)))
t(aggregate(raw_data_yan_3, by=list(bucket), mean))
tapply()