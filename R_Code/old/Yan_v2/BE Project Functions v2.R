

#>>>>>>>>>>>>>>>>Function for sampling>>>>>>>>>>>>>>>>>>>>>>>>>>>
BE_Sampling <-function(raw_data, seeds, sampling_vector){
  
  if(length(seeds)>0){
    
    #split raw data by response
    temp_raw_response_data<-raw_data[raw_data$response==1,]
    temp_raw_nonresponse_data<- raw_data[raw_data$response==0,]
    
    out_list<- vector("list",length=length(seeds))
    for(i in 1:length(seeds)){
      
      temp_seed<-seeds[i]
      
      total_num_response<- nrow(temp_raw_response_data)
      temp_tr_num_response<- round(total_num_response * sampling_vector[1])  #training response count
      temp_vl_num_response<- round(total_num_response * sampling_vector[2])  #validation response count
      #temp_ts_num_response <- total_num_response - temp_tr_num_response - temp_vl_num_response #remaining for validation
      
      total_num_nonresponse<- nrow(temp_raw_nonresponse_data)
      temp_tr_num_nonresponse<- round(total_num_nonresponse * sampling_vector[1]) #training non-response count
      temp_vl_num_nonresponse<- round(total_num_nonresponse * sampling_vector[2]) #validation response count
      #temp_ts_num_nonresponse <- total_num_nonresponse -  temp_tr_num_nonresponse - temp_vl_num_nonresponse # remaining for validation
      
      #start sampling on response data
      set.seed(temp_seed)
      Index<- sample(1:total_num_response)
      
      set.seed(temp_seed)
      TrRespIndex<- sample(Index,temp_tr_num_response)
      OtRespIndex<-setdiff(Index,TrRespIndex)
      
      set.seed(temp_seed)
      VlRespIndex<- sample(OtRespIndex,temp_vl_num_response)
      TsRespIndex<- setdiff(OtRespIndex, VlRespIndex)
      
      #start sampling on nonresponse data
      set.seed(temp_seed)
      Index2<- sample(1:total_num_nonresponse)
      
      set.seed(temp_seed)
      TrNonRespIndex<-sample(Index2,temp_tr_num_nonresponse)
      OtNonRespIndex<-setdiff(Index2, TrNonRespIndex)
      
      set.seed(temp_seed)
      VlNonRespIndex<-sample(OtNonRespIndex,temp_vl_num_nonresponse)
      TsNonRespIndex <- setdiff(OtNonRespIndex, VlNonRespIndex)
      
      temp_training<- rbind(temp_raw_response_data[TrRespIndex,],temp_raw_nonresponse_data[TrNonRespIndex,])
      temp_validation<- rbind(temp_raw_response_data[VlRespIndex,],temp_raw_nonresponse_data[VlNonRespIndex,])
      temp_test<- rbind(temp_raw_response_data[TsRespIndex,],temp_raw_nonresponse_data[TsNonRespIndex,])
      out_list[[i]] <- list(temp_training,temp_validation,temp_test)
      
      
    }
    
    return(out_list)
    
  }
  
  
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>End>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>>>>>>Function for calculating performance>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>this function is different from earlier version 
Cal_Perf_Measure <- function(Pred, probs_threshold, Mod_Data){
  
  FP<-sum(Pred > probs_threshold & Mod_Data$response==0)
  TP<-sum(Pred > probs_threshold & Mod_Data$response==1)
  PPV<- TP/(TP+FP)
  FN<- sum(Pred <= probs_threshold & Mod_Data$response==1)
  TN<- sum(Pred <= probs_threshold & Mod_Data$response==0)
  Recall<- TP/(TP+FN)
  #accuracy
  ACC<- (TP +TN)/nrow(Mod_Data)
  return(c(TP,FP,TN,FN,ACC,PPV,Recall))
  
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>End>>>>>>>>>>>>>>>>>>>>>>>>>>>>>





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>function for Random Forest Model>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Run_BE_RF_Model_byparm<- function(all_samples_forsim, var_to_exclude, par_all_scenarios, simulations,out_path){
  
  #start recording execution time
  timeStart <- proc.time()
  
  #run thru simulation loops
  registerDoParallel(cores = Sys.getenv('NUMBER_OF_PROCESSORS'))
  
  for(i in 1:simulations){
    
    #initialize
    
    if(i==1){
      
      saved_sim_outcome<- matrix(NA,nrow=simulations, ncol=1+ncol(par_all_scenarios)+9)
      
      saved_sim_outcome<- as.data.frame(saved_sim_outcome)
      colnames(saved_sim_outcome)<- c("Simulation", colnames(par_all_scenarios),"training_auc","test_auc","test TP", "test FP", "test TN", "test FN","test Accuracy", "test PPV","test Recall")
      
    }
    
    temp_auc_vector<-vector("numeric", length=nrow(par_all_scenarios))
    
    
    training_data<- all_samples_forsim[[i]][[1]]
    validation_data<-all_samples_forsim[[i]][[2]]
    test_data<-all_samples_forsim[[i]][[3]]
    
    
    
    #loop thru random forest paramters combo 
    temp_out<-foreach(scen = 1:nrow(par_all_scenarios), .packages=c('randomForest','glmnet'), .combine='c') %dopar% {
      
      tr_response<-as.factor(training_data[,"response"])
      RF_fit<-randomForest(x=training_data[,setdiff(names(training_data),c(var_to_exclude,"response"))],y=tr_response, 
                           ntree=par_all_scenarios[scen,"ntree"],
                           mtry=par_all_scenarios[scen,"mtry"],
                           nodesize=par_all_scenarios[scen,"nodesize"]
      ) 
      
      
      vl_pred<-predict(RF_fit,validation_data, type='prob')
      vl_auc<-auc(validation_data$response,vl_pred[,2])
      return(vl_auc)
      
      
    }
    
    #initialize
    opti_par_vector<- numeric(ncol(par_all_scenarios))
    names(opti_par_vector) <- colnames(par_all_scenarios)
    #loop thru each parameter
    for(par_id in 1:ncol(par_all_scenarios)){    
      
    temp_ave_auc <-tapply(temp_out,par_all_scenarios[,par_id],mean,simplify = TRUE)
        
    temp_max_auc_index<- which(max(temp_ave_auc)==temp_ave_auc)
    
    if(temp_max_auc_index==1){
      opti_par_vector[par_id]<- as.numeric(names(temp_ave_auc[temp_max_auc_index]))
    }else if(temp_max_auc_index>1){
      
      #random sampling
      sampled_auc_index<- sample(temp_max_auc_index,1)
      opti_par_vector[par_id]<- as.numeric(names(temp_ave_auc[sampled_auc_index]))
      
    }
  
    }
    
    
    #run RF with optimal parameter
    
    full_training_data<- rbind(training_data, validation_data)
    
    full_tr_response <- as.factor(full_training_data[,"response"])
    full_RF_fit <- randomForest(x=full_training_data[,setdiff(names(full_training_data),c(var_to_exclude,"response"))],y=full_tr_response, 
                                ntree=opti_par_vector["ntree"],
                                mtry=opti_par_vector["mtry"],
                                nodesize=opti_par_vector["nodesize"])
    
    full_tr_pred<- predict(full_RF_fit, full_training_data, type='prob')
    full_tr_auc<- auc(full_training_data$response, full_tr_pred[,2])
    
    test_pred <- predict(full_RF_fit, test_data,  type='prob')
    test_auc <- auc(test_data$response, test_pred[,2])
    
    
    #calculate true positive, false positve, true negative and false negative -- using the 0.5 as a threshold
    
    test_perf<-Cal_Perf_Measure(test_pred[,2],0.5,test_data)
    
    
    
    saved_sim_outcome[i,]<-c(i, opti_par_vector["ntree"], opti_par_vector["mtry"], opti_par_vector["nodesize"],full_tr_auc, test_auc, test_perf) 
    
  }
  
  write.csv(saved_sim_outcome, paste(out_path,"/", "saved_sim_outcome.csv",sep=""))
  
  timeEnd <- proc.time()
  
  execution_time<- (timeEnd-timeStart)[3]/60
  a_string<- paste("RF models take ", execution_time, " minutes.",sep="")
  print(a_string)
  return(list(saved_sim_outcome,a_string))
  
  
}
