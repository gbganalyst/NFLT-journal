
# R Packages --------------------------------------

data_wrangling_packages <- c("dplyr", "ggplot2", "openxlsx", "tidyr", "tibble", "ggrepel")

machine_learning_packages <- c("caret","MASS", "car", "kernlab","rpart","randomForest","class","ada", "rda","e1071", "nnet","ipred", "dbarts", "klaR", "glmnet", "earth")

if(!require(install.load)){
  install.packages("install.load")
}

install.load::install_load(c(data_wrangling_packages, machine_learning_packages))


# Binary Classification on prostate data -------------------------

dataset=read.xlsx('Large p small n dataset/Binary class/prostate-cancer-1.xlsx',sheet = 1)

# Data wrangling and preprocessing

dataset <- dataset %>% mutate(y=ifelse(y==1, "success", "failure"))

factor_variable_position <- ncol(dataset)

prostate_dataset<- dataset %>% mutate_at(.,vars (factor_variable_position),~as.factor(.))

# Scaling the continous variables

preProcess_scale_model <- preProcess(prostate_dataset, method=c('center','scale'))

prostate_dataset<- predict(preProcess_scale_model, newdata = prostate_dataset)



#  Set the total number of replications 

R <- 100

#  Initialize the test error vector

err <- matrix(0, ncol=13, nrow=R)

for(r in 1:R){
  
  # Create the training and test datasets for prostate_dataset
  
  # Step 1: Get row numbers for the training data
  trainRowNumbers <- createDataPartition(prostate_dataset$y, p=0.8,  list=FALSE)
  
  # Step 2: Create the training  dataset
  train_prostateData <- prostate_dataset[trainRowNumbers,]
  
  # Step 3: Create the test dataset
  test_prostateData <- prostate_dataset[-trainRowNumbers,]
  
  # Store X and Y for later use.
  xtrain <-  train_prostateData[-ncol(train_prostateData)]
  ytrain <-  train_prostateData$y
  xtest <- test_prostateData[-ncol(test_prostateData)]
  ytest <- test_prostateData$y
  ntr <- nrow(train_prostateData)
  nte <- nrow(test_prostateData)
  
  
  lda.model<- train(y~., data=train_prostateData, method = "lda2", trControl = trainControl(method = "cv", number = 3, returnResamp = "all", classProbs = TRUE))
  yhat.lda <- predict(lda.model, xtest)
  err.lda <- 1-sum(diag(table(ytest, yhat.lda)))/nte
  
  err[r,1] <- err.lda
  
  svm.model <- ksvm(y~., data=train_prostateData)
  yhat.svm <- predict(svm.model, xtest)
  err.svm <- 1-sum(diag(table(ytest, yhat.svm)))/nte
  
  err[r,2] <- err.svm
  
  cart.model <- rpart(y~., data=train_prostateData)
  yhat.cart <- predict(cart.model, xtest, type='class')
  err.cart <- 1-sum(diag(table(ytest, yhat.cart)))/nte
  
  err[r,3] <- err.cart
  
  rf.model <- train(y ~ ., data=train_prostateData,  method = "rf", trControl = trainControl(method = "cv", number = 3, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary), metric = "ROC", ntree = 20, importance = TRUE)
  yhat.rf <- predict(rf.model, xtest)
  err.rf <- 1-sum(diag(table(ytest, yhat.rf)))/nte
  
  err[r,4] <- err.rf
  
  gausspr.model <- gausspr(y~., data=train_prostateData)
  yhat.gausspr <- predict(gausspr.model, xtest, type='response')
  err.gausspr <- 1-sum(diag(table(ytest, yhat.gausspr)))/nte
  
  err[r,5] <- err.gausspr
  
  knn.model <- train(y ~., data=train_prostateData, method = "knn", trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3), tuneLength = 10)
  yhat.knn <- predict(knn.model, newdata = xtest)
  err.knn <- 1-sum(diag(table(ytest, yhat.knn)))/nte
  
  err[r,6] <- err.knn
  
  boost.model <- ada(y~., data=train_prostateData)
  yhat.boost <- predict(boost.model, xtest)
  err.boost <- 1-sum(diag(table(ytest, yhat.boost)))/nte
  
  err[r,7] <- err.boost 
  
  #  nnet.model <- nnet(y~., data=train_prostateData, trace=F, size = 4,   rang = 0.1, decay = 5e-4, maxit = 200)
  #  yhat.nnet <- predict(nnet.model, xtest, type="class")
  #  err.nnet <- 1-sum(diag(table(ytest, yhat.nnet)))/nte
  
  #  err[r,8] <- err.nnet 
  
  logit.model <- glm(y~., data=train_prostateData, family=binomial(link='logit'))
  yhat.logit <- ifelse(predict(logit.model, xtest,type='response')>0.5, "success", "failure")
  err.logit <- 1-sum(diag(table(ytest, yhat.logit)))/nte
  
  err[r,8] <- err.logit
  
  bagging.model <- bagging(y~., data=train_prostateData)
  yhat.bagging <- predict(bagging.model, xtest)
  err.bagging <- 1-sum(diag(table(ytest, yhat.bagging)))/nte
  
  err[r,9] <- err.bagging
  
  naiveBayes.model <- naiveBayes(y~., data=train_prostateData)
  yhat.naiveBayes <- predict(naiveBayes.model, xtest)
  err.naiveBayes <- 1-sum(diag(table(ytest, yhat.naiveBayes)))/nte
  
  err[r,10] <- err.naiveBayes
  
  mars.model<- train(y ~ ., data=train_prostateData, method = "earth", trControl =  trainControl(method = "cv", number = 3, returnResamp = "all", classProbs = TRUE), tuneGrid = data.frame(degree = 1, nprune = (2:4)*2))
  yhat.mars  <- predict(mars.model, xtest)
  err.mars <- 1-sum(diag(table(ytest, yhat.mars)))/nte
  
  err[r,11] <- err.mars
  
  #  qda.model <- train(y ~ ., data=train_prostateData, method = "qda", trControl = trainControl(method = "cv", number = 3, returnResamp = "all",classProbs = TRUE, summaryFunction = twoClassSummary), metric = "ROC")
  #  yhat.qda <- predict(qda.model,xtest)
  #  err.qda <- 1-sum(diag(table(ytest, yhat.qda)))/nte
  
  #  err[r,13] <- err.qda
  
  rda.model <- rda(y~.,data=train_prostateData)
  yhat.rda <- predict(rda.model, newdata = xtest)$class
  err.rda <- 1-sum(diag(table(ytest, yhat.rda)))/nte
  
  err[r,12] <- err.rda
  
  glmnet.model <- train(y ~ ., data=train_prostateData,method = "glmnet", trControl = trainControl(method = "cv", number = 3, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary), metric = "ROC", tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15), .lambda = c((1:5)/10)))
  yhat.glmnet  <- predict(glmnet.model, xtest)
  err.glmnet <- 1-sum(diag(table(ytest, yhat.glmnet)))/nte
  
  err[r,13] <- err.glmnet
  
  if (r%%25==0) {
    cat('\n', round(100*r/R,0),'completed\n')
  } 
}



# Boxplot for AVTE --------------------------------

colnames(err)=c('LDA','SVM','CART','rForest','Gauss', 'kNN','Boost','Logit', 'Bagging', 'naiveBayes', 'MARS', 'RDA', 'glmnet')

err=as_tibble(err)

err %>% gather(1:13,key = 'Model',value = 'AVTE') %>% ggplot(.,aes(reorder(x=Model,AVTE, median), y=AVTE, fill=Model))+geom_boxplot(show.legend = F)+theme_bw()+labs(y=' Average test error', x='Method (Classifier)', title = 'Average test error of each classifier',caption = "Source: prostate-cancer-1 dataset")+theme(axis.title.x = element_text(face = 'bold',size = 12),axis.title.y = element_text(face = 'bold',size = 12),axis.text.x = element_text(angle = 50, vjust = 0.5))->AVTE_prostate_cancer_1                                           

ggsave("Large p small n script/Images of average test errors/Binary class/AVTE_prostate_cancer_1.png",width = 6.74, height = 4.54)


# Average prediction error --------------------------------

avg.err=apply(err, 2, mean)

data.frame(SN=1:13, AVTE=avg.err) %>%
  rownames_to_column(., var="Model") %>% ggplot(., aes(x=SN, y=AVTE))+geom_point()+
  ggrepel::geom_label_repel(aes(label=Model))+theme_bw()+
  labs(y="Average prediction error", x="Method (Classifier)", title = "Average prediction error of each classifier", caption = "Source: prostate-cancer-1 dataset")+ theme(axis.title.x = element_text(face = "bold",size = 12),axis.title.y = element_text(face = "bold",size = 12))->AVPE_prostate_cancer_1

ggsave("Large p small n script/Images of average test errors/Binary class/AVPE_prostate_cancer_1.png",width = 6.74, height = 4.54)

# prostate dataset for mean, median, standard deviation -----------

prostate_table <- err %>% gather(1:13, key = "Model",value = "Value") %>% group_by(Model) %>% summarise(avg.err=round(mean(Value),4),med.err=round(median(Value),4), std.err=round(sd(Value),4))


# prostate dataset for rank ---------------------------------------

prostate_table.rk <- prostate_table %>% mutate(avg.err.rk=rank(avg.err,ties.method = "average"), med.err.rk=rank(med.err,ties.method = "average"), std.err.rk=rank(std.err,ties.method = "average"))

write.table(prostate_table.rk, "Prostate", sep="\t", row.names = F)

#End