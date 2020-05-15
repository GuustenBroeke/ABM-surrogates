comp_extinction <- function(y){   ## Function to compute for what output values the fishery remains/disappears
  y[which(y > 0)] <- 1     ## runs with positive number of vessels
  y[which(y == 0)] <- 0    ## runs with 0 vessels
  y <- as.integer(y)
  return(y)
}
transform_parameters <- function(X){   ## Function to transform parameters to lie between 0 and 1, for SVM/SVR training
  p_range = c(49,44,0.99,0.0015,1,1,1,1,1,1,1,1,1,1,1,1,1,1800000,3000000,100000,4.5,2400,0.001,187500,1,0.0085,420)  ##Range of ABm parameters
  p_lower = c(0,0,0.01,0.001,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.5,400,-0.001,12500,0,0.0005,0)          ## Minimum value of ABM parameters
  n_p = length(p_range)                                    ## Number of parameters
  X_t = as.data.frame(matrix(nrow=dim(X)[1],ncol=n_p))     ## dataframe will contain transformed parameter values
  for(par_index in 1:n_p){                                 ## Loop over parameters
    X_t[,par_index] = (X[,par_index] - p_lower[par_index])/p_range[par_index]  ## Transform parameter values
  }
  return(X_t)                                              ## Return transformed parameter values
}
crossvalidate_svm <- function(data_train){      ## Function to determine which meta-parameters yield the best SVM performance. 
  C_values     = c(0.1,1,10,100)                ## regularisation meta parameter
  gamma_values = c(1,0.1,0.01,0.001,0.00001)    ## meta-parameter in Gaussian kernel
  ind1 = 1                                      ## index to keep track of C parameter
  acc = matrix(nrow=4,ncol=5)                   ## array that will contain the mean F1 score for various metaparameter combinations
  ss <- sample(rep(1:5, diff(floor(nrow(data_train) * c(0,0.2,0.4,0.6,0.8,1)))),replace = FALSE)  ##  
  for(i in C_values){                    ## Loop over C values
    ind2 = 1                             ## Index that will keep track of gamma parameter
    for(j in gamma_values){              ## Loop over gamma values
      score = vector(length=5)           ## vector that will store the F1 score for fivefold crossvalidation
      for(k in 1:5){                     ## Loop for fivefold crossvalidation
        datatr <- data_train[ss != k,]   ## data for training within crossvalidation
        datacc <- data_train[ss == k,]   ## data for testing within crossvalidation
        s = svm(extinction ~ ., data = datatr, cost =i, gamma = j, kernel = "radial", type = "C-classification")  ## training of svm    
        pred <- predict(s, newdata = datacc)                                                  ## svm predictions
        TP <- length(which(as.numeric(as.character(pred)) == 1 & datacc$extinction == 1))     ## true positives
        FN <- length(which(as.numeric(as.character(pred)) == 0 & datacc$extinction == 1))     ## false negatives
        FP <- length(which(as.numeric(as.character(pred)) == 1 & datacc$extinction == 0))     ## false positives
        P <- TP/(TP+FP)                  ## precision (for computation of F1 score)
        R <- TP/(TP+FN)                  ## recall (for computation of F1 score)
        score[k] <- 2*P*R/(P+R)          ## F1 score
      }
      acc[ind1,ind2] = mean(score)       ## Mean F1 score over fivefold crossvalidation
      ind2 = ind2 + 1                    ## Increase gamma index
    }
    ind1 = ind1 + 1                      ## Increase c index
  }
  minind <- which(acc == max(acc), arr.ind = TRUE)  ## Store indices that yield the best F1 score
  C_opt = C_values[minind[1]]              ## Store optimal value of C
  gamma_opt = gamma_values[minind[2]]      ## Store optimal value of gamma
  return(list(C_opt,gamma_opt)) 
}
determine_classification_accuracy <- function(data_train,data_test,C_opt,gamma_opt){  ## Function to determine F1 score for SVM surrogate model
  s <- svm(extinction ~ ., data = data_train, cost=C_opt,kernel='radial',gamma=gamma_opt, type = "C-classification")   ## Train SVM
  y_pred = predict(s,newdata=data_test)                           ## Obtain SVM predictions for test data
  TP <- length(which(as.numeric(as.character(y_pred)) == 1 & data_test$extinction == 1))  ## True positives
  FN <- length(which(as.numeric(as.character(y_pred)) == 0 & data_test$extinction == 1))  ## False negatives
  FP <- length(which(as.numeric(as.character(y_pred)) == 1 & data_test$extinction == 0))  ## False positives 
  P <- TP/(TP+FP)    ## Precision
  R <- TP/(TP+FN)    ## Recall 
  acc <- 2*P*R/(P+R) ## F1 score  (test set)
  y_pred_train = predict(s,newdata=data_train)
  TP <- length(which(as.numeric(as.character(y_pred_train)) == 1 & data_train$extinction == 1))  ## True positives
  FN <- length(which(as.numeric(as.character(y_pred_train)) == 0 & data_train$extinction == 1))  ## False negatives
  FP <- length(which(as.numeric(as.character(y_pred_train)) == 1 & data_train$extinction == 0))  ## False positives 
  P <- TP/(TP+FP)    ## Precision
  R <- TP/(TP+FN)    ## Recall 
  acc_train <- 2*P*R/(P+R) ## F1 score (training set)
  return(list(acc,acc_train))
}
crossvalidate_svr <- function(data_train){      ## Function to determine optimal values of meta-parameters of SVR surrogate model (using cross-validation) 
  C_values       = c(0.1,1,10,100)              ## regularisation meta parameter
  gamma_values   = c(1,0.1,0.01,0.001,0.00001)  ## meta-parameter in Gaussian kernel
  epsilon_values = c(1,0.1,0.01,0.001,0.0001)   ## insensitive region meta-parameter
  ind1 = 1                 ## index to keep track of C 
  cop = array(dim=c(length(C_values),length(gamma_values),length(epsilon_values)))    ## array that will contain coefficient of prognosis for various metaparameter combinations
  ss <- sample(rep(1:5, diff(floor(nrow(data_train) * c(0, 0.2,0.4,0.6, 0.8, 1)))),replace = FALSE)
  for(i in C_values){       ## Loop over C values
    ind2 = 1                 ## Index that will keep track of gamma
    for(j in gamma_values){   ## Loop over gamma values
      ind3 = 1                  ## index that will keep track of epsilon 
      for(k in epsilon_values){  ## Loop over epsilon values
        score = vector(length=5)  ## vector that will store the coefficient of prognosis for fivefold cross-validation
        for(l in 1:5){
          datatr <- data_train[ss != l,]  ## Training data for within cross-validation
          datacc <- data_train[ss == l,]  ## Test data for within cross-validation
          s = svm(y ~ ., data = datatr, cost =i, gamma = j, epsilon = k, kernel = "radial", type = "eps-regression")  ## Train SVR      
          pred <- predict(s, newdata = datacc)   ## SVR predictions
          score[l] <- (cor(pred,datacc$y))^2     ## Coefficient of prognosis 
        }
        cop[ind1,ind2,ind3] = mean(score)    ## Mean coefficient of prognosis
        ind3 = ind3 + 1  
      }
      ind2 = ind2 + 1
    }
    ind1 = ind1 + 1
  }
  best_ind = which(cop == max(cop), arr.ind = TRUE)       ## Store which index is best
  C_opt = C_values[best_ind[1]]              ## Store optimal value of C
  gamma_opt = gamma_values[best_ind[2]]      ## Store optimal value of gamma
  epsilon_opt = epsilon_values[best_ind[3]]  ## Store optimal value of epsilon
  return(list(C_opt,gamma_opt,epsilon_opt))
}
determine_regression_cop <- function(data_train,data_test,C_opt,gamma_opt,epsilon_opt){                                  ## Function to determine coefficient of prognosis 
  clf <- svm(y ~., data=data_train,kernel='radial',C=C_opt,gamma=gamma_opt,epsilon=epsilon_opt,type='eps-regression')    ## Train SVM for optimal metaparameter values
  y_pred = predict(clf,newdata=data_test)              ## Obtain SVM predictions for test data
  y_pred_train = predict(clf,newdata=data_train)       ## SVM predictions for training data
  CoP <- sum((data_test$y - mean(data_test$y))*(y_pred-mean(y_pred))/((length(y_pred)-1)*sd(y_pred)*sd(data_test$y)))^2  ## Compute coefficient of prognosis
  return(CoP)
}
compute_S_class <- function(clf_svm){  ## Function to compute sensitivity indices for classification
  n_p = 27                             ## Number of ABM parameters
  c = dx*c(0:(1+as.integer(1/dx)))     ## Step in parameter values
  n_cubes    = length(c)*n_p           ## Number of cubes for replicated latin hypercube sampling
  size_cubes = 1000                    ## Number of sample points per cube
  par_values = matrix(nrow=size_cubes*n_cubes,ncol=n_p)      ## Matrix that will contain sampled parameter values
  for(index in 0:(n_cubes-1)){                               ## Loop to draw latin hypercubes
    cube <- randomLHS(n= size_cubes,k=n_p)                   ## Draw Latin hypercube
    cube[,index%/%length(c) + 1] <- c[index%%length(c) + 1]  ## One of the parameters (for which the sensitivity index is being computed) is given a fixed values
    par_values[(1+(index)*size_cubes):((index+1)*size_cubes),] <- cube   ## Write the drawn cube to the matrix containing sampled parameter values
  }
  y_pred = predict(clf_svm,newdata=par_values)                           ## Obtain SVM predictions for test data
  
  cond_mean_class = matrix(nrow=length(c),ncol=n_p)                  ## vector that will contain conditional means (mean output with one parameter fixed)
  svm_data = cbind(par_values,as.numeric(as.character(y_pred)))      ## combine parameter values and corresponding SVM predictions in a single matrix
  for(p in 1:n_p){                                                   ## loop over parameters
    par_data <- svm_data[(1+(p-1)*size_cubes*length(c)):(p*size_cubes*length(c)),c(p,n_p+1)]   ## data for the computation of the sensitivity index for one parameter
    cond_mean_class[,p] <- aggregate(par_data, by = list(par_data[,1]),mean)[,3]               ## conditional means for this parameter
  }
  S_range <- apply(cond_mean_class,2,function(x) {max(x)-min(x)})    ## sensitivity index
  return(list(S_range))
}
compute_S_entropy_pos <- function(clf_svm,clf_svr,pars,nbins,size_cubes,eta_tot_samples){ ## function to compute entropy-based sensitivity indices
  n_cubes    = 100    ## number of cubes for replicated latin hypercube sampling
  par_values = matrix(nrow=size_cubes*n_cubes,ncol=n_p)  ## Matrix that will contain parameter values 
  for(index in 0:(n_cubes-1)){                          
    cube <- randomLHS(n= size_cubes,k=n_p)                 ## Draw Latin hypercube
    par_values[(1+(index)*size_cubes):((index+1)*size_cubes),] <- cube  ## Add Latin hypercube to sampled parameter settings
  }
  for(ind in 1:(dim(par_values)[1])){
    par_values[ind,c(8:12)]  <- rdirichlet(1,c(0.45,0.25,0.1,0.1,0.1))    ## For a few combinations of parameters, we use a direchlet distribution. This is to ensure that the sum of these parameters equals one, as is required for the ABM 
    par_values[ind,c(13,14)] <- rdirichlet(1,c(0.7,0.3))                  
    par_values[ind,c(15:17)] <- rdirichlet(1,c(0.2,0.6,0.2))               
  }
  y_pred_svm = as.integer(as.character(predict(clf_svm,newdata=par_values)))  ## Obtain SVM predictions
  y_pred_svr = predict(clf_svr,newdata=par_values[,pars])                     ## Obtain SVR predictions
  y_pred <- y_pred_svm*y_pred_svr                               ## Obtain predictions by combining SVM and SVR
  indpos <- which(y_pred != 0)           ## Indices with nonzero output
  y_pred_pos <- y_pred[y_pred != 0]      ## nonzero predictions    
  par_values_pos <- par_values[indpos,]  ## corresponding parameter values
  r1 <- c(min(y_pred),max(y_pred))       ## range of predcitions
  eta <- vector(length=n_p)              ## vector that will contain first-order entropy-based indices
  eta_tot <- vector(length=n_p)          ## vector that will contain total-order entropy-based indices
  d1 <- discretize(y_pred_pos,nbins)     ## discretise outcomes for entropy computation
  H <- entropy.empirical(d1)             ## total entropy of outcomes
  for(i in 1:n_p){                       ## Loop to compute first-order indices
    d12 <- discretize2d(y_pred_pos,par_values_pos[,i],nbins,nbins)  ## discretise output
    I <- mi.empirical(d12)               ## Compute mutual information
    eta[i] <- I/H                        ## compute first-order index
  }
  dp <- 1/eta_tot_samples               ## step size in parameter values
  parvalues <- dp/2 + c(0:(eta_tot_samples-1))*dp   ## sampled parameter values
  for(i in 1:n_p){                      ## Loop to compute total-order indices
    par_values_cond <- matrix(ncol=n_p +1,nrow=eta_tot_samples*dim(par_values_pos)[1])  ## Matrix that will contain parameter values
    for(j in 1:dim(par_values_pos)[1]){                                                 ##
      par_values_cond[(eta_tot_samples*(j-1)+1):(eta_tot_samples*j),1] <- j             ##
      par_values_cond[(eta_tot_samples*(j-1)+1):(eta_tot_samples*j),2:(n_p+1)] <- rep(par_values_pos[j,],each = eta_tot_samples)
      par_values_cond[(eta_tot_samples*(j-1)+1):(eta_tot_samples*j),(i+1)] <- parvalues ##
    }
    if(i > 7 & i < 13){                                                ## if loop to ensure that a few ABM parameters sum to one (required by ABM structure)
      for(j in 1:dim(par_values_cond)[1]){                             ## 
        parsum <- sum(par_values_cond[j,8:12])                         ## 
        par_values_cond[j,8:12] <- par_values_cond[j,8:12]/parsum      ## 
      }
    }
    if(i > 12 & i < 15){                                               ## if loop to ensure that a few ABM parameters sum to one (required by ABM structure)
      for(j in 1:dim(par_values_cond)[1]){                             ## 
        parsum <- sum(par_values_cond[j,13:14])                        ## 
        par_values_cond[j,13:14] <- par_values_cond[j,13:14]/parsum    ## 
      }
    }
    if(i > 14 & i < 18){                                               ## if loop to ensure that a few ABM parameters sum to one (required by ABM structure)
      for(j in 1:dim(par_values_cond)[1]){                             ##  
        parsum <- sum(par_values_cond[j,15:17])                        ##
        par_values_cond[j,15:17] <- par_values_cond[j,15:17]/parsum    ##
      }
    }
    y_pred_svm = as.integer(as.character(predict(clf_svm,newdata=par_values_cond[,2:(n_p+1)])))  ## SVM predictions  
    y_pred_svr = predict(clf_svr,newdata=par_values_cond[,pars+1])                               ## SVR predictions      
    y_pred <- y_pred_svm*y_pred_svr                                                              ## Combined predictions
    indpos <- which(y_pred != 0)                                                                 ## indices with nonzero predictions  
    y_pred <- y_pred[indpos]                                                                     ## nonzero predictions   
    par_values_cond <- par_values_cond[indpos,]                                                  ## corresponding paramter values
    H_cond <- aggregate(y_pred,by=list(par_values_cond[,1]),function(x) {entropy.empirical(discretize(x,nbins,r=r1))})[,2]      ## Conditional entropy
    H_cond_tot <- mean(H_cond)                                                                   ## mean of conditional entropies (computed over parameter values)
    eta_tot[i] <- H_cond_tot / H                                                                 ## entropy-based index
  }
  return(list(eta,eta_tot))
}
compute_S_meangrad_pos <- function(clf_svm,clf_svr,pars,nbins,size_cubes,eta_tot_samples){ ## function to compute entropy-based sensitivity indices
  n_cubes <- 100    ## number of cubes for replicated latin hypercube sampling
  n_bins  <- 10     ## Number of bins for parameter values in sensitivity computation
  par_values = matrix(nrow=size_cubes*n_cubes,ncol=n_p)                ## Matrix that will contain parameter values 
  for(index in 0:(n_cubes-1)){                                         ## Loop over cubes
    cube <- randomLHS(n= size_cubes,k=n_p)                             ## Draw parameter values for single cube 
    par_values[(1+(index)*size_cubes):((index+1)*size_cubes),] <- cube ## Write drawn parameter values to cube
  }
  for(ind in 1:(dim(par_values)[1])){
    par_values[ind,c(8:12)]  <- rdirichlet(1,c(0.45,0.25,0.1,0.1,0.1)) ## For a few combinations of parameters, we use a dirichlet distribution. This is to ensure that the sum of these parameters equals one, as is required for the ABM 
    par_values[ind,c(13,14)] <- rdirichlet(1,c(0.7,0.3))               ## 
    par_values[ind,c(15:17)] <- rdirichlet(1,c(0.2,0.6,0.2))           ## 
  }
  values_list <- (c(1:n_bins)-0.5)/n_bins                              ## list of set parameter values used for computation of gradient-based sensitivity indices
  mean_grad_pos <- matrix(nrow=size_cubes*n_cubes,ncol=n_p)            ## matrix that will contain mean gradients
  y_pred_svm <- as.integer(as.character(predict(clf_svm,newdata=par_values)))  ## Obtain SVM predictions
  y_pred_svr <- predict(clf_svr,newdata=par_values[,pars])                     ## Obtain SVR predictions
  y_pred <- y_pred_svm*y_pred_svr                   ## Obtain predictions by combining SVM and SVR
  mintot <- min(y_pred)                 ## Minimum of surrogate model predictions
  maxtot <- max(y_pred)                 ## Maximum of surrogate model predictions
  for(i in 1:n_p){                        ## Loop over parameters to compute gradient-based indices
    y_pred_tot <- matrix(nrow=size_cubes*n_cubes,ncol=n_bins)
    for(j in 1:n_bins){                   ## Loop over parameter value bins
      par_values2 <- par_values           ## Copy old parameter values, to change parameters for gradient computation
      par_values2[,i] <- values_list[j]   ## Change values of one parameter for computation of gradient
      if(i >= 8 & i <= 12){                ## For certain parameter combinations, it is ensured below that the sum equals one, following the restrictions of the ABM
        ind <- 8:12
        sumweights <- colSums(par_values2[,ind[ind!=i]])
        par_values2[,ind[ind!=i]] <- par_values2[,ind[ind!=i]]/sumweights*(1-par_values2[,i]) 
      }
      if(i == 13){
        par_values2[,14] <- 1 - par_values2[,13]
      }
      if(i == 14){
        par_values2[,13] <- 1 - par_values2[,14]
      }
      if(i >= 15 & i <= 17){
        ind <- 15:17
        sumweights <- colSums(par_values2[,ind[ind!=i]])
        par_values2[,ind[ind!=i]] <- par_values2[,ind[ind!=i]]/sumweights*(1-par_values2[,i])
      }
      y_pred_svm <- as.integer(as.character(predict(clf_svm,newdata=par_values2)))  ## Obtain SVM predictions
      y_pred_svr <- predict(clf_svr,newdata=par_values2[,pars])                     ## Obtain SVR predictions
      y_pred <- y_pred_svm*y_pred_svr                   ## Obtain predictions by combining SVM and SVR
      indpos <- which(y_pred != 0)                      ## Indices with nonzero output       
      y_pred_tot[,j] <- y_pred                          ## Compute mean of values keeping one parameter fixed 
      non_null <- list()   
      y_pred_pos <- list()
    }
    for(k in 1:(size_cubes*n_cubes)){                   ## Loop over sample points
      non_null <- which(y_pred_tot[k,] != 0)            ## indices of samples with positive output  
      y_pred_pos <- y_pred_tot[k,][y_pred_tot[k,] != 0] ## output of samples with positive output
      difference <- abs(diff(y_pred_pos))               ## difference between sample points (to estimate gradient)
      gradient <- difference / (diff(non_null)/n_bins)  ## estimate gradient (over nonzero output)
      if(length(gradient)>0){                           ## if any gradients were computed (no gradient is computed if all samples are zero)
        mean_grad_pos[k,i] <- mean(gradient)            ## compute the mean of computed gradients
      }
      else{
        mean_grad_pos[k,i] <- NA                        ## 
      }
    }
  }
  return(colMeans(mean_grad_pos,na.rm=TRUE) / (maxtot - mintot))
}
compute_S_entropy <- function(clf_svr,pars_svr,nbins,size_cubes,eta_tot_samples){   ## Function to compute entropy-based sensitivity indices
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
compute_S_entropy_tot <- function(clf_svr,pars_svr,nbins,size_cubes,eta_tot_samples){
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
      eta_tot[j] <- mean(H_cond) / H
    }
    mean_eta_tot[i] <- mean(na.omit(eta_tot))     ## 
    print(mean_eta_tot[i])               ## 
  }
  return(mean_eta_tot)
}