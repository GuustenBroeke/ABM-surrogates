library(lhs)
library("e1071")  
library("Rfast")    
library("entropy")  
setwd("C:/user files/programming projects/surrogates/RC")       
source("bb_fxns.R")

n_test <- 1000       ## Number of samples in test set
n_train <- 1000      ## Number of samples in training set 
size_cube <- 100     ## Size of single Latin hypercube in replicated Latin hypercube design
dx <- 0.05           ## Step size in parameter value (for computing sensitivity indices)
n_p <- 3             ## Number of parameters included in sensitivity analysis
n_samples <- n_test + n_train   ## Total number of samples
n_t <- 5000                     ## Total number of time-steps
tt <- seq(0, n_t, length.out = n_t)   ## Vector of time-points
cube <- randomLHS(n_samples, n_p)     ## Random sample of parameter settings

bb_dde <- function(t, y, p) {    ## Bazykin-Berezovskaya model
  zeta     <- p[1]               ## Assign parameter values
  kappa    <- p[2]
  h        <- p[3]
  gamma    <- p[4]
  y1 <- y[[1L]]                  ## Assign initial condition values
  y2 <- y[[2L]]
  c(y1 * (y1 - zeta) * (kappa - y1) - y1*y2, gamma * (y1 - h) * y2)   ## Model equations
}

p <- vector(length=4)               ## Vector that will contain model parameters
y0 <- vector(length=2)              ## Vector that will contain initial conditions
yy <- vector(length=n_t)            ## Vector that will contain model output
yy_mean <- vector(length=n_samples) ## Vector that will contain model output averaged over time
for(i in 1:n_samples){
  p[1]  <- cube[i,1]                ## Assign parameter values
  p[2] <- 1
  p[3] <- cube[i,2]
  p[4] <- 1
  y0[1] <- 0.9                      ## Assign initial condition values
  y0[2] <- cube[i,3]
  run <- dde::dopri(y0, tt, bb_dde, p)  ## Perform model run
  yy[i] <- run[n_t,3]                   ## Store model output at final time-step
  yy_mean[i] <- mean(run[(4000:5000),3])  ## Store time-averaged model output
}

################################################
### SVM classification using random sampling ###
################################################

extinction <- yy                     ## Vector that will contain output of whether or not runs go extinct
extinction[extinction < 1.e-5] <- 0  ## Runs that went extinct
extinction[extinction > 0] <- 1      ## Runs with positive population
extinction <- as.integer(extinction) ## 

data_test_svm        <- data.frame(cube[1:n_test,])   ## Parameter values of svm test data
data_test_svm[,4]    <- extinction[1:n_test]          ## Output of svm test data
colnames(data_test_svm) <- c("zeta","h","y0","extinction")   

data_train_svm       <- data.frame(cube[(n_test+1):(n_test+n_train),])  ## Parameter values of svm training data
data_train_svm[,4]   <- extinction[(n_test+1):(n_test+n_train)]         ## Output of svm training data
colnames(data_train_svm) <- c("zeta","h","y0","extinction")         

temp_list = crossvalidate_svm(data_train_svm)                                                                ## determine optimal SVM meta-parameters through cross-validation
C_opt <- temp_list[[1]];gamma_opt <- temp_list[[2]]                                                          ## store optimal meta-parameter values
F1_test_svm <- determine_classification_accuracy(data_train_svm[1:1000,],data_test_svm,C_opt,gamma_opt)      ## Determine F1 scores and classificationn accuracy
clf_svm <- svm(extinction ~ ., data = data_train_svm, cost=C_opt,kernel='radial',gamma=gamma_opt, type = "C-classification")      ## Store SVM surrogate model

png(file="bb_decision_boundary.png", width=400, height=400)    ## Print figure of decision boundary
plot(clf_svm, data_test_slice_svm, h ~ zeta, slice = list(y0 = .1),dataSymbol = " ", svSymbol= "",xlabel= "")
dev.off()

S_r <- compute_S_class(clf_svm,n_p)                                  ## Compute sensitivity indices for classification

#########################################################################################
### Use SVR to predict the number of agents, for settings with a positive population  ###
#########################################################################################

#######################
### Random sampling ###
#######################

data_test_svr <- data.frame(cube[1:n_test,])            ## SVR test data (note: these are the same parameter settings as for SVM training data, but with population size as output)
data_test_svr[,4]   <- yy_mean[1:n_test]
data_test_svr <- data_test_svr[extinction[1:n_test] > 0,]      ## discard runs that led to extinction
colnames(data_test_svr) <- c("zeta","h","y0","n")

data_train_svr <- data.frame(cube[(n_test+1):(n_test+n_train),])            ## SVR training data (note: these are the same parameter settings as for SVM training data, but with population size as output)
data_train_svr[,4]   <- yy_mean[(n_test+1):(n_test+n_train)]
data_train_svr <- data_train_svr[extinction[(n_test+1):(n_test+n_train)] > 0,]      ## discard runs that led to extinction
colnames(data_train_svr) <- c("zeta","h","y0","n")

templist <- crossvalidate_svr(data_train_svr)                                                    ## use cross-validation to determine optimal meta-parameter values
C_opt <- templist[[1]] ; gamma_opt <- templist[[2]] ; epsilon_opt <- templist[[3]]              ## Store meta-parameter values
acc_test_svr_rand <- determine_regression_rsqr(data_train_svr,data_test_svr,C_opt,gamma_opt,epsilon_opt)  ## compute performance measures for SVR
clf_svr_rand <- svm(n ~ ., data = data_train_svr, cost=C_opt,kernel='radial',gamma=gamma_opt, epsilon=epsilon_opt,type = "eps-regression")   ## Store SVR surrogate model
pred = predict(clf_svr_rand, newdata= data_test_svr)                                            ## Compute SVR predictions on test set
plot(pred,data_test_svr[,4])

png(file="bb_svr_pred.png", width=400, height=400)           ## Plot of SVR prediction vs test data output
plot(pred,data_test_svr$n,ylim=c(0,0.25),xlim=c(0,0.25),ylab="SVM prediction",xlab="ABM output",main="Population density")
abline(0,1,col='red')
dev.off()

s_ent_pos  <- compute_S_entropy_pos(clf_svm,clf_svr_rand,c(1:3),c(1:3),20,10000,10000)                   ## Compute sensitivity indices based on entropy (positive region only)
S_ent_pos_tot_def <- compute_S_entropy_pos_tot(clf_svm,clf_svr_rand,c(1:3),c(1:3),20,1000,1000)          ## Compute total-order sensitivity indices based on entropy (positive region only)
s_ent  <- compute_S_entropy(clf_svr_rand,c(1:3),20,10000,10000)                                          ## Compute first-order sensitivity indices based on entropy (total region)
S_ent_tot <- compute_S_entropy_tot(clf_svr_rand,c(1:3),20,1000,1000)                                     ## Compute total-order sensitivity indices based on entropy (total region)
S_grad_pos <- compute_S_meangrad_pos(clf_svm,clf_svr_rand,20,100,100)                                    ## Computed sensitivity indices based on gradient

