## Functions needed to replicate results in manuscript "The use of surrogate models to analyse agent-based models"
crossvalidate_svm <- function(data_train){    ## Function to find optimal SVM meta-parameter values through cross-validation
  C_values     = c(0.1,1,10,100)              ## regularisation meta-parameter
  gamma_values = c(1,0.1,0.01,0.001,0.00001)  ## meta-parameter in Gaussian kernel
  ind1 = 1                                    ## index to keep track of C meta-parameter
  acc = matrix(nrow=4,ncol=5)                 ## array that will contain classification accuracy for various metaparameter combinations
  ss <- sample(rep(1:5, diff(floor(nrow(data_train) * c(0, 0.2,0.4,0.6, 0.8, 1)))),replace = FALSE)  ## Divide training set into 5 parts for fivefold cross-validation  
  for(i in C_values){                         ## Loop over C values
    ind2 = 1                                  ## Index that will keep track of gamma parameter
    for(j in gamma_values){                   ## Loop over gamma values
      score = vector(length=5)                ## Vector to store F1 performance measure 
      for(k in 1:5){                          ## Loop over five parts of cross-validation set
        datatr <- data_train[ss != k,]        ## four out of five parts are used to train the svm
        datacc <- data_train[ss == k,]        ## the remaining part is used to test svm performance
        s = svm(extinction ~ ., data = datatr, cost =i, gamma = j, kernel = "radial", type = "C-classification")     ## Train SVM 
        pred <- predict(s, newdata = datacc)                                                                         ## Obtain SVM predictions for test part of data
        TP <- length(which(as.numeric(as.character(pred)) == 1 & datacc$extinction == 1))                            ## Number of true positive for occurrence of extinction
        FN <- length(which(as.numeric(as.character(pred)) == 0 & datacc$extinction == 1))                            ## Number of false negatives
        FP <- length(which(as.numeric(as.character(pred)) == 1 & datacc$extinction == 0))                            ## Number of false positives
        P <- TP/(TP+FP)                       ## Used to compute F1 score                                                                       ##
        R <- TP/(TP+FN)                       ## Used to compute F1 score                                                                       ##
        score[k] <- 2*P*R/(P+R)               ## Computation of F1 score                                                                        ##
      }
      acc[ind1,ind2] = mean(score)           ## Store obtained accuracy
      ind2 = ind2 + 1                        ## Increase gamma index
    }
    ind1 = ind1 + 1                          ## Increase c index
  }
  maxind <- which(acc == max(acc,na.rm=TRUE), arr.ind = TRUE)    ## Indices for which the SVM performance is best
  C_opt <- C_values[maxind[1]]                                   ## Store optimal value of C
  gamma_opt <- gamma_values[maxind[2]]                           ## Store optimal value of gamma
  return(list(C_opt,gamma_opt)) 
}
crossvalidate_svr <- function(data_train){       ## Function to find optimal SVM meta-parameter values through cross-validation
  C_values       = c(1,10,100,1000)              ## regularisation meta parameter
  gamma_values   = c(0.1,0.01,0.001)             ## meta-parameter in Gaussian kernel]
  epsilon_values = c(0.1,0.01,0.001)             ## insensitive region meta-parameter 
  ind1 = 1                                       ## index to keep track of C parameter
  neg_mae = array(dim=c(length(C_values),length(gamma_values),length(epsilon_values)))              ## array that will contain classification accuracy for various metaparameter combinations
  ss <- sample(rep(1:5, diff(floor(nrow(data_train) * c(0, 0.2,0.4,0.6, 0.8, 1)))),replace = FALSE) ## Divide training set into 5 parts for fivefold cross-validation 
  for(i in C_values){                     ## Loop over C values
    ind2 = 1                              ## Index that will keep track of gamma parameter
    for(j in gamma_values){               ## Loop over gamma values
      ind3 = 1                            ## Index to keep track of gamma meta-parameter
      for(k in epsilon_values){           ## Loop over epsilon values
        score = vector(length=5)          ## Vector to store performance measure  
        for(l in 1:5){                    ## Loop over 5 test sets for crossvalidation
          datatr <- data_train[ss != l,]  ## assign data to training set
          datacc <- data_train[ss == l,]  ## assign data to test set
          s <- svm(n ~ ., data = datatr, cost =i, gamma = j, epsilon = k, kernel = "radial", type = "eps-regression") ## Train SVR     
          pred <- predict(s, newdata = datacc)    ## obtain svr predictions
          score[l] <- -mean(abs(pred-datacc$n))   ## Store performance measure (based on mean absolute deviation)
        }
        neg_mae[ind1,ind2,ind3] = mean(score)     ## Store mean performance measure over 5 test sets
        ind3 = ind3 + 1                           ## Increase epsilon index
      }
      ind2 = ind2 + 1                             ## Increase gamma index
    }
    ind1 = ind1 + 1                               ## increase C index
  }
  best_ind = which(neg_mae == max(neg_mae), arr.ind = TRUE)       ## Store which index is best
  C_opt = C_values[best_ind[1]]              ## Store optimal value of C
  gamma_opt = gamma_values[best_ind[2]]      ## Store optimal value of gamma
  epsilon_opt = epsilon_values[best_ind[3]]  ## Store optimal value of epsilon
  return(list(C_opt,gamma_opt,epsilon_opt))  
}
determine_classification_accuracy <- function(data_train,data_test,C_opt,gamma_opt){    ## Function to compute F1 score of svm 
  s <- svm(extinction ~ ., data = data_train, cost=C_opt,kernel='radial',gamma=gamma_opt, type = "C-classification") ## train svm
  y_pred = predict(s,newdata=data_test)                           ## Obtain SVM predictions for test data
  TP <- length(which(as.numeric(as.character(y_pred)) == 1 & data_test$extinction == 1))  ## Number of true positives
  FN <- length(which(as.numeric(as.character(y_pred)) == 0 & data_test$extinction == 1))  ## number of false negatives
  FP <- length(which(as.numeric(as.character(y_pred)) == 1 & data_test$extinction == 0))  ## number of false positives
  P <- TP/(TP+FP)   ## Used to compute F1 score
  R <- TP/(TP+FN)   ## Used to compute F1 score
  F1 = 2*P*R/(P+R)  ## Compute F1 score. 
  return(F1)
}
determine_regression_rsqr <- function(data_train,data_test,C_opt,gamma_opt,epsilon_opt){
  clf <- svm(n ~., data=data_train,kernel='radial',C=C_opt,gamma=gamma_opt,epsilon=epsilon_opt,type='eps-regression')    ## Train SVM for optimal metaparameter values
  y_pred = predict(clf,newdata=data_test)              ## Obtain SVM predictions for test data
  acc = cor(y_pred,data_test$n)                        ## Store accuracy of SVM predictions for test data (after crossvalidation)
  return(acc)
}
compute_S_class <- function(clf_svm,n_p){
  c = dx*c(0:(1+as.integer(1/dx)))     ## Step in parameter values
  n_cubes    = length(c)*n_p           ## Number of drawn Latin hypercubes
  size_cubes = 1000                    ## Size of each Latin hypercube
  par_values = matrix(nrow=size_cubes*n_cubes,ncol=n_p)     ## Matrix that will contain drawn samples 
  for(index in 0:(n_cubes-1)){                              ## Loop over hypercubes
    cube <- randomLHS(n= size_cubes,k=n_p)                  ## Draw hypercube
    cube[,index%/%length(c) + 1] <- c[index%%length(c) + 1] ## For individual parameters, the values are replaced by fixed values. These are later used to compute the sensitivity for that parameter 
    par_values[(1+(index)*size_cubes):((index+1)*size_cubes),] <- cube  ## Write drawn parameter values to matrix
  }
  y_pred = predict(clf_svm,newdata=par_values)              ## Obtain SVM predictions for test data
  
  cond_mean_class = matrix(nrow=length(c),ncol=n_p)              ## Matrix that will contain conditional means (means with one parameter fixed)
  svm_data = cbind(par_values,as.numeric(as.character(y_pred)))  ## store drawn parameter values and corresponding svm predictions in single matrix
  for(p in 1:n_p){                                                                            ## Loop over model parameters
    par_data <- svm_data[(1+(p-1)*size_cubes*length(c)):(p*size_cubes*length(c)),c(p,n_p+1)]  ## Select data for different fixed values of current parameter
    cond_mean_class[,p] <- aggregate(par_data, by = list(par_data[,1]),mean)[,3]              ## Compute the conditional mean output for each fixed value (corresponding to classification probability)
  }
  S_range <- apply(cond_mean_class,2,function(x) {max(x)-min(x)})   ## Compute sensitivity index (Eq. 4 of manuscript)
  return(S_range)
}
compute_S_meangrad_pos <- function(clf_svm,clf_svr,n_bins,size_cubes,n_cubes){ ## function to compute entropy-based sensitivity indices
  par_values = matrix(nrow=size_cubes*n_cubes,ncol=n_p)                ## Matrix that will contain parameter values 
  for(index in 0:(n_cubes-1)){                                         ## Loop over cubes
    cube <- randomLHS(n= size_cubes,k=n_p)                             ## Draw parameter values for single cube 
    par_values[(1+(index)*size_cubes):((index+1)*size_cubes),] <- cube ## Write drawn parameter values to matrix
  }
  values_list <- (c(1:n_bins)-0.5)/n_bins                          ## list of fixed parameter values for computation of gradients
  mean_grad_pos <- matrix(nrow=size_cubes*n_cubes,ncol=n_p)        ## matrix that will contain means of computed gradients
  y_pred_svm <- as.integer(as.character(predict(clf_svm,newdata=par_values)))  ## Obtain SVM predictions
  y_pred_svr <- predict(clf_svr,newdata=par_values)                ## Obtain SVR predictions
  y_pred <- y_pred_svm*y_pred_svr                                  ## Obtain predictions by combining SVM and SVR
  mintot <- min(y_pred)                                            ## Maximum of predicted values
  maxtot <- max(y_pred)                                            ## Minimum of predicted values
  for(i in 1:n_p){                                                 ## Loop over parameters to compute gradient-based indices
    y_pred_tot <- matrix(nrow=size_cubes*n_cubes,ncol=n_bins)      ## matrix that will contain surrogate model predictions
    for(j in 1:n_bins){                                            ## Loop over parameter value bins
      par_values2 <- par_values                                    ## Copy old parameter values, to change parameters for gradient computation
      par_values2[,i] <- values_list[j]                            ## Change value of one parameter
      y_pred_svm <- as.integer(as.character(predict(clf_svm,newdata=par_values2)))  ## Obtain SVM predictions
      y_pred_svr <- predict(clf_svr,newdata=par_values2)                     ## Obtain SVR predictions
      y_pred <- y_pred_svm*y_pred_svr                                               ## Obtain predictions by combining SVM and SVR
      indpos <- which(y_pred != 0)                                                  ## Indices with nonzero output       
      y_pred_tot[,j] <- y_pred                                                      ## Compute mean of values keeping one parameter fixed 
      non_null <- list()                                                            ## list that will contain all indices for which the surrogate model predictions are nonzero
      y_pred_pos <- list()                                                          ## list that will contain all nonzero 
    }
    for(k in 1:(size_cubes*n_cubes)){                      ## list over all sample points
      non_null <- which(y_pred_tot[k,] != 0)               ## store indices with nonzero predictions
      y_pred_pos <- y_pred_tot[k,][y_pred_tot[k,] != 0]    ## store nonzero predictions
      difference <- abs(diff(y_pred_pos))                  ## store difference between consecutive parameter values
      gradient <- difference / (diff(non_null)/n_bins)     ## compute gradient, based on difference
      if(length(gradient)>0){                              ## if any gradients were computed (if all outputs are zero, no gradient is computed)
        mean_grad_pos[k,i] <- mean(gradient)               ## store the mean of the computed gradients
      }
      else{                                                
        mean_grad_pos[k,i] <- NA                           ## if no gradients were computed, store NA value instead
      }
    }
  }
  return(colMeans(mean_grad_pos,na.rm=TRUE) / (maxtot - mintot))   ## Normalize the computed gradients using the maximum and minimum predicted output value
}
compute_S_entropy_pos <- function(clf_svm,clf_svr,pars_svm,pars_svr,nbins,size_cubes,eta_tot_samples){
  n_p <- 3                               ## Number of parameters
  n_cubes    <- 100                       ## Number of Latin hypercubes drawn for sampling
  par_values <- matrix(nrow=size_cubes*n_cubes,ncol=n_p)    ## Matrix that will contain all parameter values
  for(index in 0:(n_cubes-1)){                              ## Loop over number of drawn hypercubes
    par_values[(1+(index)*size_cubes):((index+1)*size_cubes),] <- randomLHS(n= size_cubes,k=n_p)      ## Draw on hypercube containing parameter values      
  }
  y_pred_svm <- as.integer(as.character(predict(clf_svm,newdata=par_values[,pars_svm])))  ## Obtain SVM predictions for drawn samples
  par_values_pos <- par_values[y_pred_svm != 0,]      ## Discard runs that went to extinction 
  y_pred_pos <- predict(clf_svr,newdata=par_values_pos[,pars_svr])                            ## Obtain SVR predictions for drawn samples
  r1 <- c(min(y_pred_pos),max(y_pred_pos))                   ## Store range of model outcomes
  eta <- vector(length=n_p)                          ## Vector that will contain entropy-based sensitivity indices (first-order)
  eta_tot <- vector(length=n_p)                      ## Vector that will contain entropy-based sensitivity indices (total-order)
  H <- entropy.empirical(discretize(y_pred_pos,nbins))  ## Compute entropy of model output
  for(i in 1:n_p){                                      ## Loop over parameters 
    eta[i] <- mi.empirical(discretize2d(y_pred_pos,par_values_pos[,i],nbins,nbins))/H  ## Compute first-order sensitivity index                                   
  }
  return(eta)
}
compute_S_entropy <- function(clf_svr,pars_svr,nbins,size_cubes,eta_tot_samples){
  n_p <- 3                               ## Number of parameters
  n_cubes    <- 100                       ## Number of Latin hypercubes drawn for sampling
  par_values <- matrix(nrow=size_cubes*n_cubes,ncol=n_p)    ## Matrix that will contain all parameter values
  for(index in 0:(n_cubes-1)){                              ## Loop over number of drawn hypercubes
    par_values[(1+(index)*size_cubes):((index+1)*size_cubes),] <- randomLHS(n= size_cubes,k=n_p)      ## Draw on hypercube containing parameter values      
  }
  y_pred <- predict(clf_svr,newdata=par_values[,pars_svr])                            ## Obtain SVR predictions for drawn samples
  r1 <- c(min(y_pred),max(y_pred))                   ## Store range of model outcomes
  eta <- vector(length=n_p)                          ## Vector that will contain entropy-based sensitivity indices (first-order)
  eta_tot <- vector(length=n_p)                      ## Vector that will contain entropy-based sensitivity indices (total-order)
  H <- entropy.empirical(discretize(y_pred,nbins))  ## Compute entropy of model output
  for(i in 1:n_p){                                      ## Loop over parameters 
    eta[i] <- mi.empirical(discretize2d(y_pred,par_values[,i],nbins,nbins))/H  ## Compute first-order sensitivity index                                   
  }
  return(eta)
}
compute_S_entropy_pos_tot <- function(clf_svm,clf_svr,pars_svm,pars_svr,nbins,size_cubes,eta_tot_samples){
  n_p <- 3                                         ## Number of parameters
  par_values <- randomLHS(n= size_cubes,k=n_p)      ## Draw on hypercube containing parameter values      
  y_pred_svm <- as.integer(as.character(predict(clf_svm,newdata=par_values[,pars_svm])))  ## Obtain SVM predictions for drawn samples
  par_values_pos <- par_values[y_pred_svm != 0,]      ## Discard runs that went to extinction 
  y_pred_pos <- predict(clf_svr,newdata=par_values_pos[,pars_svr])                            ## Obtain SVR predictions for drawn samples
  r1 <- c(min(y_pred_pos),max(y_pred_pos))
  H <- entropy.empirical(discretize(y_pred_pos,nbins))                        ## Compute entropy of model output
  mean_eta_tot <- vector(length=n_p)
  for(i in 1:n_p){                                    ## Loop over model parameters
    print(i)
    eta_tot <- vector(length=dim(par_values_pos)[1])                            ## Vector that will contain entropy-based sensitivity indices (total-order)
    for(j in 1:dim(par_values_pos)[1]){                                             ## Loop over parameter settings
      par_values_cond <- matrix(ncol=n_p +1,nrow=eta_tot_samples)               ## Matrix that will contain parameter values parameter values 
      par_values_cond[,1] <- j                                                  ## index to keep track of sample number 
      par_values_cond[1:eta_tot_samples,2:(n_p+1)] <- rep(par_values_pos[j,],each = eta_tot_samples)   ## write values of all but one parameter
      par_values_cond[,(i+1)] <- runif(eta_tot_samples,0,1)                     ## Generate random values for remaining parameter
      y_pred_svm <- as.integer(as.character(predict(clf_svm,newdata=par_values_cond[,pars_svm+1])))  ## Obtain SVM predictions for drawn samples
      par_values_cond <- par_values_cond[which(y_pred_svm != 0),]                                    ## Discard parameter settings leading to extinction
      y_pred <- predict(clf_svr,newdata=par_values_cond[,pars_svr+1])                                ## Obtain SVR predictions for remaining samples
      H_cond <- aggregate(y_pred,by=list(par_values_cond[,1]),function(x) {entropy.empirical(discretize(x,nbins,r=r1))})[,2]   ## Compute mutual information
      eta_tot[j] <- mean(H_cond) / H                                                ## Divide by total entropy
    }
    mean_eta_tot[i] <- mean(na.omit(eta_tot))     ## average over sample points
    print(mean_eta_tot[i])               ## 
  }
  return(mean_eta_tot)
}
compute_S_entropy_tot <- function(clf_svr,pars_svr,nbins,size_cubes,eta_tot_samples){
  n_p <- 3                                         ## Number of parameters
  par_values <- randomLHS(n= size_cubes,k=n_p)      ## Draw on hypercube containing parameter values      
  y_pred <- predict(clf_svr,newdata=par_values[,pars_svr])                            ## Obtain SVR predictions for drawn samples
  r1 <- c(min(y_pred),max(y_pred))
  H <- entropy.empirical(discretize(y_pred,nbins))                        ## Compute entropy of model output
  mean_eta_tot <- vector(length=n_p)
  for(i in 1:n_p){                                    ## Loop over model parameters
    print(i)
    eta_tot <- vector(length=dim(par_values)[1])                            ## Vector that will contain entropy-based sensitivity indices (total-order)
    for(j in 1:dim(par_values)[1]){                                             ## Loop over parameter settings
      par_values_cond <- matrix(ncol=n_p +1,nrow=eta_tot_samples)               ## Matrix that will contain parameter values parameter values 
      par_values_cond[,1] <- j                                                  ## index to keep track of sample number 
      par_values_cond[1:eta_tot_samples,2:(n_p+1)] <- rep(par_values[j,],each = eta_tot_samples)   ## write values of all but one parameter
      par_values_cond[,(i+1)] <- runif(eta_tot_samples,0,1)                     ## Generate random values for remaining parameter
      y_pred <- predict(clf_svr,newdata=par_values_cond[,pars_svr+1])                                ## Obtain SVR predictions for remaining samples
      H_cond <- aggregate(y_pred,by=list(par_values_cond[,1]),function(x) {entropy.empirical(discretize(x,nbins,r=r1))})[,2]   ## Compute mutual information
      eta_tot[j] <- H_cond / H
    }
    mean_eta_tot[i] <- mean(na.omit(eta_tot))     ## 
    print(mean_eta_tot[i])               ## 
  }
  return(mean_eta_tot)
}