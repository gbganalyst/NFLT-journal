
# R Packages ----------------------------------------

data_wrangling_packages <- c("dplyr", "ggplot2","openxlsx", "tibble", "ggrepel", "tidyr")

machine_learning_packages <- c("caret","MASS", "car", "kernlab","rpart","randomForest","class","ada", "rda","e1071", "nnet","ipred", "dbarts", "klaR", "glmnet", "earth")

if(!require("xfun")){
  install.packages("xfun")
}

xfun::pkg_attach(c(data_wrangling_packages, machine_learning_packages),install = T)

# Multi Classification on Lymphoma dataset -----------------------------

dataset=read.xlsx('Large p small n dataset/Multi class/lymphoma-data.xlsx',sheet = 1)

# Data wrangling and preprocessing

factor_variable_position <- ncol(dataset)


lymphoma_dataset <- dataset %>% mutate_at(.,vars (factor_variable_position),~as.factor(.))

# Scaling the continous variables

preProcess_scale_model <- preProcess(lymphoma_dataset, method=c('center', 'scale'))

lymphoma_dataset <- predict(preProcess_scale_model, newdata = lymphoma_dataset)



#  Set the total number of replications 

R <- 25

#  Initialize the test error vector

err <- matrix(0, ncol=11, nrow=R)

for(r in 1:R){
  
  # Create the training and test datasets for lymphoma_dataset
  
  # Step 1: Get row numbers for the training data
  trainRowNumbers <- createDataPartition(lymphoma_dataset$y, p=0.8,   list=FALSE)
  
  # Step 2: Create the training  dataset
  train_lymphomaData <- lymphoma_dataset[trainRowNumbers,]
  
  # Step 3: Create the test dataset
  test_lymphomaData <- lymphoma_dataset[-trainRowNumbers,]
  
  # Store X and Y for later use.
  xtrain <-  train_lymphomaData[-ncol(train_lymphomaData)]
  ytrain <-  train_lymphomaData$y
  xtest <- test_lymphomaData[-ncol(test_lymphomaData)]
  ytest <- test_lymphomaData$y
  ntr <- nrow(train_lymphomaData)
  nte <- nrow(test_lymphomaData)
  
  
  lda.model <- lda(y~., data=train_lymphomaData)  
  yhat.lda <- predict(lda.model, xtest)$class
  err.lda <- 1-sum(diag(table(ytest, yhat.lda)))/nte
  
  err[r,1] <- err.lda
  
  svm.model <- ksvm(y~., data=train_lymphomaData)
  yhat.svm <- predict(svm.model, xtest)
  err.svm <- 1-sum(diag(table(ytest, yhat.svm)))/nte
  
  err[r,2] <- err.svm
  
  
  cart.model <- rpart(y~., data=train_lymphomaData)
  yhat.cart <- predict(cart.model, xtest, type='class')
  err.cart <- 1-sum(diag(table(ytest, yhat.cart)))/nte
  
  err[r,3] <- err.cart
  
  
  forest.model <- randomForest(y~., data=train_lymphomaData)
  yhat.forest <- predict(forest.model, xtest, type='class')
  err.forest <- 1-sum(diag(table(ytest, yhat.forest)))/nte
  
  err[r,4] <- err.forest
  
  gausspr.model <- gausspr(y~., data=train_lymphomaData,kernel='rbfdot')
  yhat.gausspr <- predict(gausspr.model, xtest, type='response')
  err.gausspr <- 1-sum(diag(table(ytest, yhat.gausspr)))/nte
  
  err[r,5] <- err.gausspr
  
  knn.model <- train(y ~., data=train_lymphomaData, method = "knn", trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3), tuneLength = 10)
  yhat.knn <- predict(knn.model, newdata = xtest)
  err.knn <- 1-sum(diag(table(ytest, yhat.knn)))/nte
  
  err[r,6] <- err.knn
  
  
 # mda.model <- train(y~., data=train_lymphomaData, method='mda')
 # yhat.mda  <- predict(mda.model, xtest)
 # err.mda <- 1-sum(diag(table(ytest, yhat.mda)))/nte
  
 # err[r,7] <- err.mda
  
  
  #nnet.model <- nnet(y~., data=train_lymphomaData, trace=F, size = 4,   rang = 0.1, decay = 5e-4, maxit = 200)
 # yhat.nnet <- predict(nnet.model, xtest, type="class")
 # err.nnet <- 1-sum(diag(table(ytest, yhat.nnet)))/nte
  
 # err[r,8] <- err.nnet 
  
  
  #multinom.model <- multinom(y ~., data=train_lymphomaData)
  #yhat.multinom<- predict(multinom.model, newdata = xtest,type = "class") 
  #err.multinom <- 1-sum(diag(table(ytest, yhat.multinom)))/nte
  
  #err[r,9] <- err.multinom
  
  
  bagging.model <- bagging(y~., data=train_lymphomaData)
  yhat.bagging <- predict(bagging.model, xtest)
  err.bagging <- 1-sum(diag(table(ytest, yhat.bagging)))/nte
  
  err[r,7] <- err.bagging
  
  naiveBayes.model <- naiveBayes(y~., data=train_lymphomaData)
  yhat.naiveBayes <- predict(naiveBayes.model, xtest)
  err.naiveBayes <- 1-sum(diag(table(ytest, yhat.naiveBayes)))/nte
  
  err[r,8] <- err.naiveBayes
  
  
  mars.model <- train(y~., data=train_lymphomaData, method='earth')
  yhat.mars  <- predict(mars.model, xtest)
  err.mars <- 1-sum(diag(table(ytest, yhat.mars)))/nte
  
  err[r,9] <- err.mars
  
  
  #qda.model <- qda(y ~., data=train_lymphomaData)
  #yhat.qda <- predict(qda.model,xtest)$class
  #err.qda <- 1-sum(diag(table(ytest, yhat.qda)))/nte
  
  #err[r,13] <- err.qda
  
  
  rda.model <- train(y~., data=train_lymphomaData, method='rda')
  yhat.rda  <- predict(rda.model, xtest)
  err.rda <- 1-sum(diag(table(ytest, yhat.rda)))/nte
  
  err[r,10] <- err.rda
  
  
  glmnet.model <- train(y~., data=train_lymphomaData, method='glmnet')
  yhat.glmnet  <- predict(glmnet.model, xtest)
  err.glmnet <- 1-sum(diag(table(ytest, yhat.glmnet)))/nte
  
  err[r,11] <- err.glmnet
  
  if (r%%25==0)  cat('\n', round(100*r/R,0),'completed\n')
}



# Boxplot for AVTE ------------------------------------

colnames(err)=c('LDA','SVM','CART','rForest','Gauss', 'kNN', 'Bagging', 'naiveBayes', 'MARS','RDA', 'glmnet')

err=as_tibble(err)

err %>% gather(1:11,key = 'Model',value = 'AVTE') %>% ggplot(.,aes(reorder(x=Model,AVTE, median), y=AVTE, fill=Model))+geom_boxplot(show.legend = F)+theme_bw()+labs(y=' Average test error', x='Method (Classifier)', title = 'Average test error of each classifier',caption = "Source: Lymphoma dataset")+theme(axis.title.x = element_text(face = 'bold',size = 12),axis.title.y = element_text(face = 'bold',size = 12),axis.text.x = element_text(angle = 50, vjust = 0.5))->AVTE_lymphoma_dataset                                           

ggsave("Large p small n script/Images of average test errors/Multi class/AVTE_lymphoma_dataset.png", width = 6.74, height = 4.54)


# Average prediction error --------------------------------

avg.err=apply(err, 2, mean)

data.frame(SN=1:11, AVTE=avg.err) %>%
  rownames_to_column(., var="Model") %>% ggplot(., aes(x=SN, y=AVTE))+geom_point()+
  ggrepel::geom_label_repel(aes(label=Model))+theme_bw()+
  labs(y="Average prediction error", x="Method (Classifier)", 
       title = "Average prediction error of each classifier", caption = "Source: Lymphoma dataset")+
  theme(axis.title.x = element_text(face = "bold",size = 12),axis.title.y = element_text(face = "bold",size = 12))->AVPE_lymphoma_dataset


ggsave("Large p small n script/Images of average test errors/Multi class/AVPE_lymphoma_dataset.png",width = 6.74, height = 4.54)

# Lymphoma dataset for mean, median, standard deviation -----------

lymphoma_table<- err %>% gather(1:11, key = "Model",value = "Value") %>% group_by(Model) %>% summarise(avg.err=round(mean(Value),4),med.err=round(median(Value),4), std.err=round(sd(Value),4))


# Lymphoma dataset for rank ----------------------------

lymphoma_table.rk <- lymphoma_table %>% mutate(avg.err.rk=rank(avg.err,ties.method = "average"), med.err.rk=rank(med.err,ties.method = "average"), std.err.rk=rank(std.err,ties.method = "average"))

write.table(lymphoma_table.rk, "lymphoma_table", sep="\t", row.names = F)

#End