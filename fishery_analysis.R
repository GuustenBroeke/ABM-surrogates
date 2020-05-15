## SVM analysis of fishery model in R
## This code demonstrates sensitivity analysis using SVM and SVR metamodels for a fishery ABM

#################
### Libraries ###
#################

library("e1071", lib.loc="C:/Program Files/R/R-3.5.0/library")
library("lhs", lib.loc="C:/ProgramData/R/win-library/3.5")
library("entropy", lib.loc="C:/ProgramData/R/win-library/3.5")
library("gtools", lib.loc="C:/Program Files/R/R-3.5.0/library")
source("fishery_fxns.R")

#################
### Constants ###
#################

n_p <- 27             ## Number of parameters
n_samples <- 3000     ## Total number of sample points
n_test <- 1000        ## Number of test sample points
size_cube <- 100      ## Size of Latin hypercubes (for replicated Latin hypercube design)
dx <- 0.05            ## Step size for parameters

#####################
## Read input data ##
#####################

X <- read.csv("indata_fishery.csv")[,2:28]       ## Parameter settings
profit <- read.csv("profit_fishery.csv")[,2:51]  ## Outputs (profit, stock, and number of vessels)
stock <- read.csv("stock_fishery.csv")[,2:51]
vessels <- read.csv("vessels_fishery.csv")[,2:51] 
data <- transform_parameters(X)                   ## For the SVM/SVR training, the input data is transformed to range between 0 and 1

##################################################################################
### SVM classification using random sampling (including sensitivity estimation)###
##################################################################################

data$extinction <- comp_extinction(vessels[,50])                                                                           ## Compute whether or not the fishery remains
data_test <- data[1:n_test,]                                                                                               ## Assign test data
data_train <- data[(n_test+1):dim(data)[1],]                                                                               ## Assign training data
temp_list = crossvalidate_svm(data_train)                                                                                  ## Perform cross-validation to determine meta-parameter settings
C_opt <- temp_list[[1]];gamma_opt <- temp_list[[2]]                                                                        ## Assign meta-parameter values
temp_list <- determine_classification_accuracy(data_train,data_test,C_opt,gamma_opt)                                       ## Determine F1 value of resulting SVM
acc_test_svm <- temp_list[[1]];acc_train_rand <- temp_list[[2]]                                                            ## Store computed F1 value
clf_svm <- svm(extinction ~ ., data = data_train, cost=C_opt,kernel='radial',gamma=gamma_opt, type = "C-classification")   ## Store SVM surrogate model
templist <- compute_S_class(clf_svm)                                                                                       ## Compute sensitivity indices based on SVM
S_range <- templist[[1]]                                                                                                   ## Store computed sensitivity indices 

########################################################################
### SVR using random sampling for positive samples (stock as output) ###
########################################################################

data$extinction <- NULL                                    ## For SVR we use the same parameter settings as for SVM, but we remove the SVM output
data$y <- rowMeans(stock[,20:dim(stock)[2]])               ## As output we use fish stock, averaged over time 

data_test <- data[1:n_test,]                               ## Assign test data
indpos <- which(vessels[1:n_test,50] > 0)                  ## Determine indices of test samples where the fishery remains positive
data_test <- data_test[indpos,]                            ## Discard from the test set sample points where the system went extinct

data_train <- data[(n_test+1):dim(data)[1],]               ## Assign training data
indpos <- which(vessels[(n_test+1):dim(data)[1],50] > 0)   ## Determine indices of training samples where the fishery remains positive                                       ## Determine indices of training samples where the fishery remains
data_train <- data_train[indpos,]                          ## Discard from the test set sample points where the system went extinct

temp_list = crossvalidate_svr(data_train)                                                          ## Perform cross-validation to determine meta-parameter settings
C_opt <- temp_list[[1]];gamma_opt <- temp_list[[2]];epsilon_opt <- temp_list[[3]]                  ## assign meta-parameter values
acc_test_stock_rand <- determine_regression_cop(data_train,data_test,C_opt,gamma_opt,epsilon_opt)  ## determine coefficient of prognosis
clf_svr_stock_rand <- svm(y ~ ., data = data_train, cost=C_opt,kernel='radial',gamma=gamma_opt, epsilon=epsilon_opt,type = "eps-regression")  ## Store SVR surrogate model
temp_list <- compute_S_entropy_pos(clf_svm, clf_svr_stock_rand,c(1:27),20,2000,10)                 ## Compute entropy-based sensitivity indices
S_ent_stock <- temp_list[[1]]; S_ent_tot_stock <- temp_list[[2]]                                   ## Store entropy-based sensitivity indices
S_grad_stock <- compute_S_meangrad_pos(clf_svm, clf_svr_stock_rand,c(1:27),20,2000,10)             ## Compute gradient-based sensitivity indices
S_ent_fullregion_stock <- compute_S_entropy(clf_svr_stock_rand,c(1:27),20,2000,10)                 ## Compute entropy-based indices over entire region (1-st order)
S_ent_fullregion_tot_stock <- compute_S_entropy_tot(clf_svr_stock_rand,c(1:27),20,1000,1000)       ## Compute entropy-based indices over entire region (total-order)

########################################################################
### SVR using random sampling for positive samples (profit as output) ###
########################################################################
indpos <- which(vessels[1:n_test,50] > 0)                                                       ## Determine indices of test samples where the fishery remains
data_test$y <- rowMeans(profit[indpos,21:dim(profit)[2]])                                       ## To remove random fluctuations, the output (profit) is averaged over a period of time

indpos <- which(vessels[(n_test+1):dim(data)[1],50] > 0)                                        ## Determine indices of training samples where the fishery remains
data_train$y <- rowMeans(profit[(n_test+1):dim(data)[1],][indpos,21:dim(profit)[2]])            ## To remove random fluctuations, the output (profit) is averaged over a period of time

temp_list = crossvalidate_svr(data_train)                                                          ## Perform cross-validation to determine meta-parameter settings
C_opt <- temp_list[[1]];gamma_opt <- temp_list[[2]];epsilon_opt <- temp_list[[3]]                  ## assign meta-parameter values
acc_test_profit_rand <- determine_regression_cop(data_train,data_test,C_opt,gamma_opt,epsilon_opt) ## determine coefficient of prognosis

## We repeat the SVR training with a subset of ABM parameters. This turns out to yield an improved performance. 
data_train_sel <- data_train[,c(1,3,4,7,21,22,24,26,28)]                                       ## Filter out parameters
temp_list = crossvalidate_svr(data_train_sel)                                                  ## Perform cross-validation to determine meta-parameter settings
C_opt <- temp_list[[1]];gamma_opt <- temp_list[[2]];epsilon_opt <- temp_list[[3]]              ## assign meta-parameter values
acc_test_profit_rand_sel <- determine_regression_cop(data_train_sel,data_test,C_opt,gamma_opt,epsilon_opt)   ## determine coefficient of prognosis

clf_svr_profit_rand_sel <- svm(y ~ ., data = data_train_sel, cost=C_opt,kernel='radial',gamma=gamma_opt, epsilon=epsilon_opt,type = "eps-regression")     ## Store SVR model

pars <- c(1,3,4,7,21,22,24,26)            ## List of included parameters
temp_list <- compute_S_entropy_pos(clf_svm, clf_svr_profit_rand_sel,pars,20,2000,10)   ## Compute entropy-based sensitivity indices 
S_ent <- temp_list[[1]]; S_ent_tot <- temp_list[[2]]                                   ## Store entropy-based sensitivity indices
S_grad <- compute_S_meangrad_pos(clf_svm, clf_svr_stock_rand,c(1:27),20,2000,10)         ## Compute gradient-based sensitivity indices
S_ent_fullregion <- compute_S_entropy(clf_svr_profit_rand_sel,pars,20,2000,10)           ## Compute entropy-based sensitivity indices over entire parameter space (first-order)
S_ent_fullregion_tot<- compute_S_entropy_tot(clf_svr_profit_rand_sel,pars,20,1000,1000)  ## Compute entropy-based sensitivity indices over entire parameter space (total-order)


plot(data_test$y,y_pred,ylab = "SVM prediction", xlab="ABM output",main="Profit")  ## Plot SVR predictions against test data
abline(a=0,b=1,col='red')                                                        
