#############################
### Constants and imports ###
#############################

n_p <- 15             ## Number of ABM parameters
n_test <- 1000        ## Number of samples in test set
size_cube <- 100      ## Size of single Latin hypercube in replicated Latin hypercube design
dx <- 0.05            ## Step size in parameter value (for computing sensitivity indices)

library("e1071", lib.loc="C:/Program Files/R/R-3.5.0/library")  ##
library("lhs", lib.loc="C:/ProgramData/R/win-library/3.5")      ##
library("Rfast", lib.loc="C:/ProgramData/R/win-library/3.5")    ##
library("entropy", lib.loc="C:/ProgramData/R/win-library/3.5")  ##
library(RNetLogo)                                               ##  For connecting R to NetLogo s
NLStart(nl.path = "C:/Program Files/NetLogo 6.0.4/app", gui = FALSE, nl.jarname = "netlogo-6.0.4.jar")
NLLoadModel("C:/user files/programming projects/surrogates/RC/RC_ABM.nlogo")
setwd("C:/user files/programming projects/surrogates/RC")             ##
source("rc_fxns.R")
setwd("C:/user files/programming projects/surrogates/RC")             ##

################################################
### SVM classification using random sampling ###
################################################
data_test_svm  <- read.csv('data_test_tot_svm2.csv')        ## Import test data for training of svm  
data_train_svm <- read.csv('data_train_svm_random2.csv')    ## Import training data for training of svm

temp_list = crossvalidate_svm(data_train_svm)               ## determine optimal SVM meta-parameters through cross-validation
C_opt <- temp_list[[1]];gamma_opt <- temp_list[[2]];        ## store optimal meta-parameter values
F1_test_svm <- determine_classification_accuracy(data_train_svm[1:1000,],data_test_svm,C_opt,gamma_opt)      ## Determine F1 scores and classificationn accuracy
clf_svm <- svm(extinction ~ ., data = data_train_svm, cost=C_opt,kernel='radial',gamma=gamma_opt, type = "C-classification")      ## Store SVM surrogate model

S_r <- compute_S_class(clf_svm,n_p)                            ## Compute sensitivity indices
plot(clf_svm, data_train_svm, c ~ R_max,slice = list(n_0 = 0.5, R_0 = 0.5, r = 0.5, D = 0.5, E_b = 0.5, R_unc = 0.5, E_h = 0.5, v_b = 0.5, E_m = 0.5, v_d = 0.5, E_move =0.5, z = 0.5, K = 0.5),dataSymbol = " ", svSymbol= "",xlabel= "")   ## Plot boundary between regions in parameter space

#########################################################################################
### Use SVR to predict the number of agents, for settings with a positive population  ###
#########################################################################################

#######################
### Random sampling ###
#######################

data_train_svr <- read.csv('data_train_svr_rand.csv')     ## Import SVR training data
data_train_svr <- data_train_svr[data_train_svr$n > 0,]   ## discard runs that led to extinction (with SVR we try to predict only the population size, for runs with a positive population)
data_test_svr <- read.csv('data_test_svr2.csv')            ## Import SVR test data (note: these are the same parameter settings as for SVM training data, but with population size as output)
data_test_svr <- data_test_svr[data_test_svr$n > 0,]      ## discard runs that led to extinction

templist = crossvalidate_svr(data_train_svr)                                                    ## use cross-validation to determine optimal meta-parameter values
C_opt <- templist[[1]] ; gamma_opt <- templist[[2]] ; epsilon_opt <- templist[[3]]              ## Store meta-parameter values
acc_test_svr_rand <- determine_regression_rsqr(data_train_svr,data_test_svr,C_opt,gamma_opt,epsilon_opt)  ## compute performance measures for SVR
clf_svr_rand <- svm(n ~ ., data = data_train_svr, cost=C_opt,kernel='radial',gamma=gamma_opt, epsilon=epsilon_opt,type = "eps-regression")   ## Store SVR surrogate model
pred = predict(clf_svr_rand, newdata= data_test_svr)                                            ## Compute SVR predictions on test set

n_s = dim(data_train_svr)[1]                          ## Total number of samples in training set 
n_iterations = floor(n_s/100)                         ## Number of iterations in loop below
acc_test_svr_rand =vector(length=n_iterations)        ## Vector will store SVR performance measure for different training set sizes
for(i in 1:n_iterations){                             ## For loop in which SVR performace measure is computed for different training set sizes 
  n_train = (i) * 100                                 ## Number of training set samples
  training_indices = sample(1:n_s,n_train,replace=F)  ## Randomly draw training samples
  data_train <- data_train_svr[training_indices,]     ## assing data to training set
  templist = crossvalidate_svr(data_train)            ## use cross-validation to determine optimal meta-parameter values
  C_opt <- templist[[1]] ; gamma_opt <- templist[[2]] ; epsilon_opt <- templist[[3]]              ## Store meta-parameter values
  acc_test_svr_rand[i] <- determine_regression_rsqr(data_train,data_test_svr,C_opt,gamma_opt,epsilon_opt)  ## perform cross-validation to find optimal meta-parameter values
}

#############################
#### Sequential Sampling ####
#############################

data_train_svr_seq <- read.csv('data_train_svr_rand.csv')             ## Import SVR training data
data_train_svr_seq <- data_train_svr_seq[data_train_svr_seq$n > 0,]   ## keep only samples with positive population
data_train_svr_seq <- data_train_svr_seq[1:100,]                      ## keep only the first 100 samples (to illustrate expanding training set through sequential sampling)
data_test_svr <- read.csv('data_test_svr2.csv')                       ## Import SVR test data
data_test_svr <- data_test_svr[data_test_svr$n > 0,]                  ## keep only samples with positive population

temp_list <- crossvalidate_svr(data_train_svr_seq)                                                    ## determine optimal meta-parameters using cross-validation 
C_opt = temp_list[1];gamma_opt = temp_list[2];epsilon_opt = temp_list[3]                              ## store optimal meta-parameter values
acc_test <- determine_regression_rsqr(data_train_svr_seq,data_test_svr,C_opt,gamma_opt,epsilon_opt)   ## compute performance measure for SVR based on initial training set

pars <- c(1:15)       ## Parameters to be included in analysis (all parameters)
n_draw = 20           ## Number of samples added per iteration of adaptive sampling 
max_iterations = 200  ## Maximum number of iterations of adaptive sampling
iteration_index = 0   ## index that will keep trach of adaptive sampling iterations
acc_aim = 0.9         ## Aimed value for performance measure
n_samples = dim(data_train_svr_seq)[1]                                                ## Number of training samples 
while(acc_test < acc_aim & iteration_index < max_iterations){                         ## Loop over adaptive sampling iterations (note: computational times can be long, depending on the chosen number of iterations)
  print(iteration_index)                                                              ## Print iteration number 
  print(acc_test)                                                                     ## Print perofrmance on test set
  n_pool <- 100*dim(data_train_svr_seq)[1]                                            ## Number of samples in sample pool, from which new samples will be drawn
  samplepool <- generate_samplepool(n_pool,n_p,pars)                                  ## Draw sample pool 
  samplepool_t <- transform_parameters(samplepool,pars)                               ## Transform sample pool to match parameter ranges of ABM
  svm_pred_samplepool <- predict(clf_svm, newdata=samplepool_t[,pars])                ## Compute SVM predictions for sample pool
  samplepool <- samplepool[as.integer(as.character(svm_pred_samplepool))==1,]         ## Keep only samples with positive SBM predictions in sample pool
  samplepool_t <- transform_parameters(samplepool,pars)                               ## Transform the reduced sample pool to match parameter ranges of ABM
  n_pool <- dim(samplepool)[1]                                                        ## Number of remaining samples in sample pool
  templist <-  assign_voronoi_regions(samplepool_t,data_train_svr_seq[1:n_p],n_pool)  ## Determine Voronoi regions based on existing training set and sample pool
  voronoi <- templist[1]; voronoi_dist<- templist[2]                                  ## store above results about Voronoi regions
  sv_acc <- determine_sv_accuracy(data_train_svr_seq,C_opt,gamma_opt,epsilon_opt)     ## Determine accuracy of surrogate model for existing samples 
  samples_new <- draw_new_samples_svr(sv_acc,n_draw,samplepool,voronoi,voronoi_dist)  ## Draw new samples to be added to training set
  outdata_new <- perform_runs(samples_new,dim(samples_new)[1])                        ## Run the ABM for the newly added training samples
  samples_new_t <- cbind(transform_parameters(samples_new,pars),outdata_new)          ## Add obtained ABM output to training samples 
  colnames(samples_new_t) <- colnames(data_train_svr_seq)                             ## set column names for new samples
  data_train_svr_seq <- rbind(data_train_svr_seq,samples_new_t)                       ## Add new samples to existing ones
  acc_test <-  determine_regression_rsqr(data_train_svr_seq,data_test_svr,C_opt,gamma_opt,epsilon_opt)  ## Assess surrogate model performance
  iteration_index = iteration_index + 1                                               ## Update index keeping track of adaptive sampling iterations
}
write.csv(data_train_svr_seq,"data_train_svr_seq_final.csv")                                  ## Store training data as csv file 

#########################################
### Analysis of seq. sampling results ###
#########################################

data_train_svr_seq <- read.csv("data_train_svr_seq_final.csv")                                       ## Read training data obtained through adaptive sampling
data_train_svr_seq  <- data_train_svr_seq[data_train_svr_seq$n > 0,]                                 ## Select only sample points with a positive population

temp_list <- crossvalidate_svr(data_train_svr_seq)                                                   ## Perform cross-validation to determine optimal meta-parameter values for SVR
C_opt = temp_list[1];gamma_opt = temp_list[2];epsilon_opt = temp_list[3]                             ## Store optimal meta-parameter values
acc_test <- determine_regression_rsqr(data_train_svr_seq,data_test_svr,C_opt,gamma_opt,epsilon_opt)  ## Compute performance measures for resulting SVR

clf_svr_seq <- svm(n ~., data=data_train_svr_seq,kernel='radial',C=C_opt,gamma=gamma_opt,epsilon=epsilon_opt, type='eps-regression')   ## Store the SVR
pred <- predict(clf_svr_seq, newdata=data_test_svr)                                                  ## Compute SVR predictions for test data

png(file="SVR_pred_rc.png", width=400, height=400)
plot(data_test_svr[,16],pred,ylim=c(0,2000),xlim=c(0,2000),ylab='SVR prediction',xlab='ABM output')  ## Plot SVR predictions against test data
abline(a=0,b=1,col='red')                                                                            ## add line to plot representing values for which the test data and predictions would be equal
dev.off()

s_ent_pos  <- compute_S_entropy_pos(clf_svm,clf_svr_seq,c(1:15),c(1:15),10,100,200)                  ## Compute sensitivity indices based on entropy (positive region only)
S_ent_pos_tot_def <- compute_S_entropy_pos_tot(clf_svm,clf_svr_seq,c(1:15),c(1:15),20,1000,1000)     ## Compute total-order sensitivity indices based on entropy (positive region only)
s_ent  <- compute_S_entropy(clf_svr_seq,c(1:15),10,100,200)                                          ## Compute first-order sensitivity indices based on entropy (total region)
S_ent_tot <- compute_S_entropy_pos_tot(clf_svm,clf_svr_seq,c(1:15),c(1:15),20,1000,1000)             ## Compute total-order sensitivity indices based on entropy (total region)
S_grad_pos <- compute_S_meangrad_pos(clf_svm,clf_svr_seq,20,100,100)                                 ## Computed sensitivity indices based on gradient

#####################################################
### Compare seq. sampling against random sampling ###
#####################################################

data_train_svr_seq  <- read.csv("data_train_svr_seq_final.csv")        ## Read training data obtained through adaptive sampling
pos_indices_seq <- which(data_train_svr_seq$n > 0)                     ## Store which samples have positive output
data_train_svr_rand <- read.csv('data_train_svr_rand.csv')             ## Import SVR training data from random sampling
pos_indices_rand <- which(data_train_svr_rand$n > 0)                   ## Store which samples have positive output
pos_indices_seq_eff <- pos_indices_seq                                 ## This is used to determine the number of used sample points (this differs from the drawn sample points, because only positive samples are used)
pos_indices_seq_eff[1:100] <- pos_indices_rand[1:100]                  ## This is used to determine the number of used sample points (this differs from the drawn sample points, because only positive samples are used)
pos_indices_seq_eff[101:length(pos_indices_seq)] <- pos_indices_seq[101:length(pos_indices_seq)] - 100 +pos_indices_seq[100]
data_test_svr <- read.csv('data_test_svr2.csv')                         ## Import SVR test data (note: these are the same parameter settings as for SVM training data, but with population size as output)
data_test_svr <- data_test_svr[1:1000,]                                ## 
data_test_svr <- data_test_svr[data_test_svr$n > 0,]                   ## discard runs that led to extinction

n_iterations <- min(floor(max(pos_indices_seq)/100),floor(max(pos_indices_rand)/100))  ## Number of different sample sizes for which both methods are compared
n_samples_rand <- vector(length=n_iterations)                          ## Number of samples for random sampling
n_samples_seq <- vector(length=n_iterations)                           ## Number of samples for sequential sampling
acc_test_svr_rand <- vector(length=n_iterations)                       ## Vector that will contain performance measure for different sample sizes
acc_test_svr_seq <- vector(length=n_iterations)                        ## Vector that will contain performance measure for different sample sizes
for(i in 1:n_iterations){                                              ## For loop in which SVR performace measure is computed for different training set sizes 
  n_drawn <- (i) * 100                                                 ## Number of used samples
  n_train_seq <- length(which(pos_indices_seq_eff <= n_drawn))         ## Number of training set samples (adaptive sampling) (note that this is number of used samples is smaller than the number of samples drawn to obtain it) 
  n_train_rand <- length(which(pos_indices_rand <= n_drawn))           ## Number of training set samples (random sampling) (note that this is number of used samples is smaller than the number of samples drawn to obtain it)
  training_indices_seq  <- pos_indices_seq[1:n_train_seq]              ## Assign training sample (sequential sampling)
  training_indices_rand <- pos_indices_rand[1:n_train_rand]            ## Assign training sample (random sampling)
  data_train_seq    <- data_train_svr_seq[training_indices_seq,]       ## adding data to training set
  data_train_rand   <- data_train_svr_rand[training_indices_rand,]     ## adding data to training set
  acc_test_svr_rand[i] <- determine_regression_rsqr(data_train_rand,data_test_svr,C_opt,gamma_opt,epsilon_opt)  ## Store performance measure (random sampling)
  acc_test_svr_seq[i]  <- determine_regression_rsqr(data_train_seq,data_test_svr,C_opt,gamma_opt,epsilon_opt)   ## Store performance measure (adaptive sampling)
}
png(file="sampling_comparison2.png", width=400, height=400)            
plot(100*c(1:n_iterations),acc_test_svr_rand,ylim=c(0.4,0.85),xlim=c(0,2000),ylab="Coefficient of prognosis",xlab = "Number of drawn samples")
points(100*c(1:n_iterations),acc_test_svr_seq,col='red',pch=0)         
dev.off()                                                              
