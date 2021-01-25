

##============================================================================================================##
#Made for Group P11's Spring 2018 IE 483/583 Project (TAlkingData)
#Made by: Xiaoshi Guo, Jinchi Li, and Shawn Thompson
#Made 4.27.18
##============================================================================================================##





##============================================================================================================##
##============================================================================================================##
##============================================================================================================##
##RF R Code Start

#Install all used packages (and some not used in the final project)
  packages <- c(
    "knitr", 
    "tidyverse", 
    "highcharter",
    "data.table", 
    "lubridate",
    "pROC", 
    "DescTools",
    "data.table", 
    "caret",
    "e1071",
    "ranger",
    "Boruta",
    "Matrix"
  )
  
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  
  ipak(packages)
#End package install





set.seed(84)               



#day 1 (11/6/17) goes to 9308569 index
#day 2 goes to 68941879 index
#day 3 goes to 131886954 index; 131886954-68941879
#day 4 goes to 184903890 index


#Import day 3 values 
#Day 3 was chosen as it includes all 24 hour points, is the largest sample (of all the days), and has the closest 
##yes/no ratio to that of the entire data set

  
  day<-fread("C:/Users/shawnt/train.csv",skip=68941879,nrow=131886954-68941879)
  #day<-fread("F:/IE 483/train.csv",skip=68941879,nrow=131886954-68941879)
  
  
  
  #Re-Name columns/remove attributed_time
  names(day)[1]<-"ip"
  names(day)[2]<-"app"
  names(day)[3]<-"device"
  names(day)[4]<-"os"
  names(day)[5]<-"channel"
  names(day)[6]<-"click_time"
  names(day)[7]<-"attributed_time"
  names(day)[8]<-"is_attributed"
  day<-day[,-"attributed_time"]
  
  
  
  
  #Subsample from day 3
  day_sample<-sample_n(day,size=1333333,replace=TRUE)
  
  day_sample$ip<-as.factor(day_sample$ip)
  day_sample$app<-as.factor(day_sample$app)
  day_sample$device<-as.factor(day_sample$device)
  day_sample$os<-as.factor(day_sample$os)
  day_sample$channel<-as.factor(day_sample$channel)
  
  
  
  #Input Engineering: Extracting the click_time hour; remove the defualt click_time attribute
  #Re-Name as column "hour"
  day_sample$click_time<-Hour(day_sample$click_time)
  names(day_sample)[6] <-"hour"
  
  #Change attributed to character; all test data would need this to be done (if making predictions)
  day_sample$is_attributed<-ifelse(day_sample$is_attributed ==0, "No", "Yes")
  day_sample$is_attributed<-as.factor(day_sample$is_attributed)
  
  #Makes a row index column (otherwise anti_join removes duplicate rows from day_sample)
  day_sample[,"row_count"]=row(day_sample)
  day_sample_test<-sample_n(day_sample,size=333333, replace=FALSE)
  day_sample<-anti_join(day_sample,day_sample_test)
  
  #Remove Dummy row index column
  day_sample$row_count<-NULL
  day_sample_test$row_count<-NULL
  
  ##day_sample is to be the training data##
  ##day_sample_test is to be the independent test data set for error estimation##

#END DATA IMPORT/SAMPLING

  
  
  
#MODEL BUILDING

  #Day removed to avoid memory issues
  day<-NULL
  
  
  #storage list is created
  bestAccuracy<-NULL
  bestAccuracy<-matrix(nrow=9,ncol=4)
  colnames(bestAccuracy)<- c("Number of Trees", "Mtry","Threshold","Accuracy")
  x=0
  
  #loop for adjusting num.trees
  for(n in seq(from=250, to= 750, by=250)){
    
  
    #loop for adjusting mtry
    for(m in 3:5){
    #created a rf which returns proabilities instead of direct results for the training data
    model_rf <-ranger(is_attributed~., data=day_sample, probability=TRUE,num.trees = n, mtry= m)
    x=x+1
    bestAccuracy[x,1]<-n
    bestAccuracy[x,2]<-m
    bestAccuracy[x,3]<-0
    bestAccuracy[x,4]<-0
      
      #loop for determening the best threshold value for the built model
      ##best is determined by best training accuracy which meets min 85% correct prediction of download
      for(i in 1:1000){
  
        #pulls prediction probabilities from themodel
        newPred<-NULL
        newPred$pred<-ifelse(model_rf$predictions[,2] > (i*.0001), "Yes", "No")
        newPred$pred<-as.factor(newPred$pred)
        
        #creates a confusion matrix for this
        confM<-confusionMatrix(newPred$pred,day_sample$is_attributed)
        
        #stores combinations which meet requirements
        if((confM$table[2,2]/(confM$table[1,2]+confM$table[2,2]))>.85){
          if(confM$overall["Accuracy"]>bestAccuracy[x,4]){
            bestAccuracy[x,3]<-i*.0001
            bestAccuracy[x,4]<-confM$overall["Accuracy"]
          }
        }
          
        
      }
  
    }
  }
  

  #Final Model Building (Based off optimized threshold value and tuning parameters)
  model_rf <-ranger(is_attributed~., data=day_sample, probability=TRUE,num.trees = 750, mtry= 3)
  prediction<-predict(model_rf,day_sample_test)
  prediction2=NULL
  prediction2$pred<-ifelse(prediction$predictions[,2] > (.0048), "Yes", "No")
  prediction2$pred=as.factor(prediction2$pred)
  confusionMatrix(prediction2$pred,day_sample_test$is_attributed)
  #End Final Model

#END MODEL BUILDING




  
  
  
  
#USED AT ONE TIME (for data analysis) BUT NOT DIRECTLY NEEDED NOW#
  #USED TO FIND INDEX POINTS
  day<-day[!(Weekday(click_time)==4)]
  #END INDEX POINTS
  
  #IP Graphing
  ipRead<-fread("E:/IE 483/train.csv/mnt/ssd/kaggle-talkingdata2/competition_files/train.csv",select=1)
  
  
  ipRead<-ipRead %>% group_by(ip) %>% summarise(count = n()) %>% arrange(desc(-ip))
  ipRead<-ipRead[! (ipRead$count <100000),]
  
  h1 <- ipRead %>% mutate(ip = as.character(ip)) %>%
    hchart("bar", hcaes(y = count, x = ip, color =-count)) %>%
    hc_add_theme(hc_theme_ffx()) %>% hc_title(text = "Top IP for All")
  
  h1
  
  #END IP GRAPHING
  
  ## Categorical features GRAPHING
  
  ### Visualizaing most frequents values in training data; the text for this was adjusted for each day (all examined)
  
  
  h1 <- day %>% group_by(app) %>% summarise(count = n()) %>% 
    arrange(desc(count)) %>% head(15) %>% mutate(app = as.character(app)) %>%
    hchart("bar", hcaes(x = app, y = count, color =-count)) %>%
    hc_add_theme(hc_theme_ffx()) %>% hc_title(text = "Top Apps for 11/9/17")
  
  
  h1
  
  h2 <- day %>% group_by(os) %>% summarise(count = n()) %>% 
    arrange(desc(count)) %>% head(15) %>% mutate(os = as.character(os)) %>%
    hchart("bar", hcaes(x = os, y = count, color =-count)) %>%
    hc_add_theme(hc_theme_ffx()) %>% hc_title(text = "Top OS for 11/9/17")
  
  h2
  
  h3 <- day %>% group_by(channel) %>% summarise(count = n()) %>% 
    arrange(desc(count)) %>% head(15) %>% mutate(channel = as.character(channel)) %>%
    hchart("bar", hcaes(x = channel, y = count, color =-count)) %>%
    hc_add_theme(hc_theme_ffx()) %>% hc_title(text = "Top Channels for 11/9/17")
  
  h3
  
  h4 <- day %>% group_by(device) %>% summarise(count = n()) %>% 
    arrange(desc(count)) %>% head(15) %>% mutate(device = as.character(device)) %>%
    hchart("bar", hcaes(x = device, y = count, color =-count)) %>%
    hc_add_theme(hc_theme_ffx()) %>% hc_title(text = "Top Devices for 11/9/17")
  
  h4
  
  h5 <- day %>% group_by(hour) %>% summarise(count = n()) %>%
    arrange(desc(count)) %>% head(15) %>% mutate(hour = as.character(hour)) %>%
    hchart("bar", hcaes(x = hour, y = count, color =-count)) %>%
    hc_add_theme(hc_theme_ffx()) %>% hc_title(text = "Top Hours for 11/9/17")
  
  h5
  
  
  #END CATEGORICAL FEATURES GRAPHING
  
  #OVERALL COUNT CHECK
  count<- day %>% group_by(is_attributed) %>% summarise(count = n()) 
  percentDownload[2]<-count[1,2]/(count[1,2]+count[2,2])
  #END OVERALL COUNT

#END USED at ONE TIME BUT NOT NEEDED NOW STUFF
  

  ##RF R Code End  
  ##============================================================================================================##
  ##============================================================================================================##
  ##============================================================================================================##

  
  
  
  
  
  
  
  
  
  ##============================================================================================================##
  ##============================================================================================================##
  ##============================================================================================================##
  ##XGB R Code Start
  
  
  library(caret)
  library(RWeka) # For J48 
  library(e1071) # For NB
  library(Boruta)
  library(DMwR)
  library(data.table)
  library(dplyr)
  library(xgboost)
  library(Matrix)
  library(zoo)
  
  ##read data and make subset
  set.seed(123456)
  train_day_3 <- fread("D:/DataMining/competition/mnt/ssd/kaggle-talkingdata2/competition_files/train.csv",col.names = c("ip", "app", "device", "os", "channel", "click_time","attribute_time", "is_attributed"),showProgress=T,skip=68941879 ,nrows=131886954-68941879)
  train_sub=sample_n(tbl =train_day_3,size = 6*10^6,replace = F)
  write.csv(x = train_sub,file = 'train_sub.csv')
  train_sub=fread("train_sub.csv",select =c("ip", "app", "device", "os", "channel", "click_time", "is_attributed"),showProgress=T,colClasses=c("ip"="factor","app"="factor","device"="factor","os"="factor","channel"="factor","click_time"="character","is_attributed"="numeric"))
  set.seed(123456)
  ##
  inTraining_id <- createDataPartition(train_sub$is_attributed, p = .677,list = FALSE,times =1) #sampleing without replacement
  
  train_sub$hour=hour(train_sub$click_time)
  train_sub_n=train_sub[,-6]
  train_sub_lab=train_sub$is_attributed
  
  ##separate them
  trainingData <- train_sub_n[inTraining_id,]
  trainingLab <- train_sub_lab[inTraining_id]
  #trainingLab <- as.integer(train_sub_lab[inTraining_id])-1
  testData<-train_sub_n[-inTraining_id,]
  testLab <- train_sub_lab[-inTraining_id]
  
  ##============================================================================================================##
  ##I origninally read labels as factors
  ## I just leave those unfunctional codes for reference purpose
  ##============================================================================================================##
  #testLab <- as.integer(train_sub_lab[-inTraining_id])-1
  
  #train_m=sparse.model.matrix(is_attributed ~ .-1,data=trainingData[])
  #test_m=sparse.model.matrix(is_attributed~ .-1,data=testData[])
  
  #train_matrix=xgb.DMatrix(data=train_m,label=trainingLab)
  #test_matrix=xgb.DMatrix(data=test_m,label=testLab)
  #watchlist<-list(train=train_matrix, test=test_matrix)
  
  ##
  
  ##Parameter
  
  # bstSparse <- xgboost(data = train_m, label = trainingLab, max.depth = 2, eta = 1, nthread = 2, nround = 100, objective = "binary:logistic")
  # 
  # #confusionMatrix(predict(bstSparse,test_m),testLab)
  # 
  # confusionMatrix(as.numeric(predict(bstSparse,test_m)>0.5),testLab)
  # 
  # ##down sample
  # #train_down=downSample(y=trainingData$is_attributed,x=trainingData)
  # #train_down_Lab=train_down$is_attributed
  # 
  # ##multi:softprob
  # nc=2
  # xgb_params<-list("objective"="multi:softprob",
  #                  "eval_metric"="mlogloss",
  #                  "num_class"=nc
  # )
  # bst_model <- xgb.train(params = xgb_params,
  #                        data = train_matrix,
  #                        nrounds = 100,
  #                        eta = 0.001,
  #                        max.depth = 3,
  #                        watchlist=watchlist,
  #                        gamma = 0,
  #                        subsample = 1,
  #                        colsample_bytree = 1,
  #                        missing = NA,
  #                        seed = 123456)
  # ##binary:logistic
  # summary(testLab)
  # xgb_b_l_params<-list("objective"="binary:logistic","eval_metric"="rmse","scale_pos_weight"=400,"max_depth"=11,"max_delta_step"=9)
  # b_l_model<-xgb.train(params = xgb_b_l_params,
  #                      data = train_matrix,
  #                      nrounds = 200,
  #                      eta = (1:5)*0.1,
  #                      watchlist=watchlist,
  #                      gamma = c(1:4),
  #                      subsample = 1,
  #                      colsample_bytree = 1,
  #                      missing = NA,
  #                      seed = 123456)
  # 
  # confusionMatrix(as.numeric(predict(b_l_model,test_m)>0.0057),testLab)
  # ##select the best threshold value
  # 
  # Pred=predict(b_l_model,train_m)
  # bestAccuracy<-matrix(nrow=1,ncol=2)
  # bestAccuracy[1,1]<-0
  # bestAccuracy[1,2]<-0
  # 
  # for(i in 1:1000){
  #   newPred<-NULL
  #   newPred$pred<-ifelse(Pred > (i*.0001), 1, 0)
  #   #newPred$pred<-as.factor(newPred$pred)
  #   
  #   confM<-confusionMatrix(newPred$pred,trainingLab)
  #   
  #   if((confM$table[2,2]/(confM$table[1,2]+confM$table[2,2]))>.85){
  #     if(confM$overall["Accuracy"]>bestAccuracy[1,2]){
  #       bestAccuracy[1,1]<-i*.0001
  #       bestAccuracy[1,2]<-confM$overall["Accuracy"]
  #     }
  #   }
  #   
  # }
  # ##use AUC curve
  # xgb_b_auc_params<-list("objective"="binary:logistic","eval_metric"="auc" ,"max_depth"=7)
  #  b_l_model_auc<-xgb.train(params = xgb_b_auc_params,
  #                         data = train_matrix,
  #                         nrounds = 200,
  #                         min_child_weight=c(1:20),
  #                         eta = (1:5)*0.06,
  #                         watchlist=watchlist,
  #                        gamma = c(1:4),
  #                        subsample = 0.8,
  #                       colsample_bytree = 0.8,
  #                       missing = NA,
  #                     seed = 123456,print_every_n = 50, early_stopping_rounds = 150)
  #  
  # 
  # 
  #  confusionMatrix(as.numeric(predict(b_l_model_auc,test_m)>0.472),testLab)
  ##seems that need some regulization for preventing overfitting
  
  
  ##=======================================================================================##
  ##factor dataset approach
  ##=======================================================================================##
  
  apps_dummy<-Matrix::sparse.model.matrix(~0+trainingData$app)
  devices_dummy<-Matrix::sparse.model.matrix(~0+trainingData$device)
  oss_dummy<-Matrix::sparse.model.matrix(~0+trainingData$os)
  channels_dummy<-Matrix::sparse.model.matrix(~0+trainingData$channel)
  hour<-Matrix::sparse.model.matrix(~0+trainingData$hour)
  allData_dummified =cbind(apps_dummy,devices_dummy,oss_dummy,channels_dummy,hour)
  
  testData<-train_sub_n[-inTraining_id,]
  testLab <- train_sub_lab[-inTraining_id]
  
  t_apps_dummy<-Matrix::sparse.model.matrix(~0+testData$app)
  t_devices_dummy<-Matrix::sparse.model.matrix(~0+testData$device)
  t_oss_dummy<-Matrix::sparse.model.matrix(~0+testData$os)
  t_channels_dummy<-Matrix::sparse.model.matrix(~0+testData$channel)
  t_hour<-Matrix::sparse.model.matrix(~0+testData$hour)
  t_allData_dummified =cbind(t_apps_dummy,t_devices_dummy,t_oss_dummy,t_channels_dummy,t_hour)
  
  trainData_s=xgb.DMatrix(data=allData_dummified,label=trainingLab)
  #testData_s=xgb.DMatrix(data=t_allData_dummified,label=as.numeric(testLab)-1)
  testData_s=xgb.DMatrix(data=t_allData_dummified,label=testLab)
  
  watchlist_f<-list(train=trainData_s, test=testData_s)
  #model <- xgboost(trainData_s, nrounds = 2000, eta = 0.07, gamma = 4,lambda = 5,scale_pos_weight = 100,objective = "binary:logistic",eval_metric="auc")
  
  start_time5=Sys.time()
  p5 <- list(objective = "binary:logistic",
             booster = "gbtree",
             eval_metric = "auc",
             nthread = 4,
             eta = 0.03,
             max_depth = 4,
             subsample = 0.7,
             min_child_weight = 20,
             gamma = 4,
             lambda = 5,
             max_delta_step = 4,
             scale_pos_weight = 100,
             nrounds = 1500)
  
  m5_xgb <- xgb.train(p5, trainData_s, p5$nrounds,watchlist=watchlist_f, print_every_n = 20, early_stopping_rounds = 150)
  
  end_time5=Sys.time()
  r_time_5=end_time5-start_time5
  
  ##it seems that the overfitting is a crucial problem with our model training 
  ##=======================================================================================##
  ##sparse with larger regulizer and nother parameter to prevent overfitting
  ##=======================================================================================##
  
  xgb_b_auc_params_r<-list("objective"="binary:logistic","eval_metric"="auc" ,"max_depth"=11)
  b_l_model_auc_r<-xgb.train(params = xgb_b_auc_params_r,
                             data = trainData_s,
                             nrounds = 500,
                             min_child_weight=40,
                             eta = 0.06,
                             watchlist=watchlist_f,
                             gamma = 10,
                             lambda = 10,
                             subsample = 0.6,
                             colsample_bytree = 0.8,
                             scale_pos_weight = 100,
                             seed = 123456)
  
  
  
  #calculation for accuracy
  confusionMatrix(as.numeric(predict(b_l_model_auc_r,t_allData_dummified)>0.5),as.numeric(testLab)-1)
  confusionMatrix(as.numeric(predict(b_l_model_auc_r,allData_dummified)>0.5),trainingLab)
  ##It helps a little bit. 
  
  ##=======================================================================================##
  ##Try numeric data matrix (without one hot encoding)
  ##=======================================================================================##
  
  train_sub_num=fread("train_sub.csv",select =c("ip", "app", "device", "os", "channel", "click_time", "is_attributed"),showProgress=T)
  
  trainingLab_num=train_sub_num[inTraining_id,7]
  testLab_num=train_sub_num[-inTraining_id,7]
  #train_sub_num$hour=hour(train_sub_num$click_time)
  
  # train_sub_num[, hour := hour(click_time)
  #       ][, click_time := NULL
  #         ][, ip_f := .N, by = "ip"
  #           ][, app_f := .N, by = "app"
  #             ][, channel_f := .N, by = "channel"
  #               ][, device_f := .N, by = "device"
  #                 ][, os_f := .N, by = "os"
  #                   ][, app_f := .N, by = "app"
  #                     ][, ip_app_f := .N, by = "ip,app"
  #                       ][, ip_dev_f := .N, by = "ip,device"
  #                         ][, ip_os_f := .N, by = "ip,os"
  #                           ][, ip_chan_f := .N, by = "ip,channel"
  #                             ][, c("ip", "is_attributed") := NULL]
  
  trainingData_num=train_sub[inTraining_id,-c(6,7)]
  trainingData_num[]=lapply(trainingData_num,as.numeric)
  testData_num=train_sub[-inTraining_id,-c(6,7)]
  testData_num[]=lapply(testData_num,as.numeric) 
  
  
  
  dtrain <- xgb.DMatrix(data = data.matrix(trainingData_num),label=data.matrix(trainingLab_num))
  dtest <- xgb.DMatrix(data = data.matrix(testData_num), label = data.matrix(testLab_num))
  watchlist<-list(train=dtrain, test=dtest)
  ##============================================================================================================##
  ##parameter1     (we use part of the parameters from kaggle kernel, but delete those parts we feel incorrect)
  ##============================================================================================================##
  
  p <- list(objective = "binary:logistic",
            booster = "gbtree",
            eval_metric = "auc",
            nthread = 4,
            eta = 0.07,
            max_depth = 4,
            min_child_weight = 24,
            gamma = 36.7126,
            subsample = 0.9821,
            colsample_bytree = 0.3929,
            colsample_bylevel = 0.6818,
            alpha = 72.7519,
            lambda = 5.4826,
            max_delta_step = 5.7713,
            scale_pos_weight = 94,
            nrounds = 2000)
  
  m_xgb <- xgb.train(p, dtrain, p$nrounds,watchlist=watchlist, print_every_n = 50, early_stopping_rounds = 150)
  
  confusionMatrix(as.numeric(predict(m_xgb,dtest)>0.5),testLab_num$is_attributed)
  
  ##it seems that numerical model have a close performance
  ##============================================================================================================##
  ###parameter2  try out with more general case with higher eta value to reduce the convergence time
  ##============================================================================================================##
  p2 <- list(objective = "binary:logistic",
             booster = "gbtree",
             eval_metric = "auc",
             nthread = 8,
             eta = 0.09,
             max_depth = 3,
             min_child_weight = 24,
             gamma = 4,
             subsample = 0.8,
             lambda = 5,
             max_delta_step = 6,
             scale_pos_weight = 100,
             nrounds = 1500)
  
  m2_xgb <- xgb.train(p2, dtrain, p$nrounds,watchlist=watchlist, print_every_n = 20, early_stopping_rounds = 150)
  
  confusionMatrix(as.numeric(predict(m2_xgb,dtest)>0.5),testLab_num$is_attributed)
  ##it seems that we can achieve close performance in this setting as well.
  
  ##============================================================================================================##
  ## parameter3   test out the performance og larger scale_pos_weight to see if we can achieve higher accuracy in minority class
  ##============================================================================================================##
  p3 <- list(objective = "binary:logistic",
             booster = "gbtree",
             eval_metric = "auc",
             nthread = 4,
             eta = 0.06,
             max_depth = 3,
             subsample = 0.8,
             min_child_weight = 24,
             gamma = 4,
             lambda = 5,
             max_delta_step = 6,
             scale_pos_weight = 200,
             nrounds = 1500)
  
  m3_xgb <- xgb.train(p3, dtrain, p$nrounds,watchlist=watchlist, print_every_n = 20, early_stopping_rounds = 150)
  
  confusionMatrix(as.numeric(predict(m3_xgb,dtest)>0.5),testLab_num$is_attributed)
  ##the influence isn't that significant
  
  ##============================================================================================================##
  ##parameter4  try slightly differnt parameters and recorde trainning time
  ##============================================================================================================##
  start_time4=Sys.time()
  p4 <- list(objective = "binary:logistic",
             booster = "gbtree",
             eval_metric = "auc",
             nthread = 4,
             eta = 0.03,
             max_depth = 4,
             subsample = 0.7,
             min_child_weight = 20,
             gamma = 4,
             lambda = 5,
             max_delta_step = 4,
             scale_pos_weight = 100,
             nrounds = 1500)
  
  m4_xgb <- xgb.train(p4, dtrain, p$nrounds,watchlist=watchlist, print_every_n = 20, early_stopping_rounds = 150)
  
  confusionMatrix(as.numeric(predict(m4_xgb,dtest)>0.5),testLab_num$is_attributed)
  end_time4=Sys.time()
  r_time_4=end_time4-start_time4
  
  ##sampling
  #train_down=downSample(y=trainingData$is_attributed_f,x=trainingData)
  
  #downsampling
  # trainingData$is_attributed_f=as.factor(trainingData$is_attributed)
  # trainingData$hour=hour(trainingData$click_time)
  # train_down=downSample(y=trainingData$is_attributed_f,x=trainingData)
  # train_down_r=train_down[,-c(1,7,8,10,12)]
  # table(train_down$is_attributed)
  # #train_down_r$hour=hour(train_down_r$click_time)
  # train_down_r[]= lapply(train_down_r[],factor)
  # testData <- train_sub[-inTraining_id,]
  
  
  ##XGB R Code End  
  ##============================================================================================================##
  ##============================================================================================================##
  ##============================================================================================================##
