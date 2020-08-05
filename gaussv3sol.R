train_data = read.table("E:/MS DS/SMMDS/assignment 3/assign3GaussTrain.txt", quote="\"", comment.char="")
#View(train_data)

train_data = data.matrix(train_data)

test_data = read.table("E:/MS DS/SMMDS/assignment 3/v2/assign3GaussTest.txt", quote="\"", comment.char="")

test_data = data.matrix(test_data)

labels = train_data[,ncol(train_data)]

trainX = train_data[,-ncol(train_data)]

oneClass = labels==1			#this will give you all indices of class = 1

zeroClass = labels==0		#this will give you all indices of class = 0

oneDat = trainX[oneClass,]		#this will give you the data matrix for class = 1

zeroDat = trainX[zeroClass,]		#this will give you the data matrix for class = 0

oneMean = colMeans(oneDat)	#this will give the mean of class = 1

oneCov    = cov(oneDat)		#this will give the covariance matrix of class = 1

zeroMean = colMeans(zeroDat)	#this will give the mean of class = 0

zeroCov    = cov(zeroDat)		#this will give the covariance matrix of class = 0

gauss  <- function(x, meanVe, covM){ 
  
  n = 2
  
  part1 = (2 * pi)^ (n/2) * sqrt(det(covM))
  
  part2 = exp(-0.5 * t(x-meanVe) %*% solve(covM) %*% (x-meanVe))
  
  return (part2/part1)
  
  }

prior_one = nrow(oneDat)/nrow(trainX)

prior_zero = nrow(zeroDat)/nrow(trainX)


bayes  <- function(r, data, covM_one, covM_zero,plot){ 
  
  predict <- c()
  
  for(i in 1:r){
    
    point = data[i,] 
    
    liklihood_one = gauss(point, oneMean, covM_one)
    
    liklihood_zero = gauss(point, zeroMean, covM_zero)
    
    evidence = (liklihood_one * prior_one) + (liklihood_zero*prior_zero)
    
    postirior_num = liklihood_one * prior_one
    
    posterior = postirior_num/evidence
    
    posterior_num2 = (liklihood_zero * prior_zero)
    
    posterior2 = posterior_num2/evidence
    
    cat("Row No.",i,"\n","MAP = P(c=0|x) = ",posterior2,"\n","MAP = P(c=1|x) = ",posterior,"\n")
    if(posterior < posterior2){
      cat("Predicted Label = 0","\n")
      predict[i] <- 0
     
    }else{
      cat("Predicted Label = 1","\n")
      predict[i] <- 1
    }
  }
    if(plot){
      test_data_label <- cbind(test_data,predict)
      
      
      oneClasstest = predict==1			#this will give you all indices of class = 1
      
      zeroClasstest = predict==0		#this will give you all indices of class = 0
      
      oneDattest = test_data[oneClasstest,]		#this will give you the data matrix for class = 1
      
      zeroDattest = test_data[zeroClasstest,]
      
      x1 = oneDattest[,1]
      x2 = oneDattest[,2]
      plot(x1,x2,col='red')  	#this will plot data points for class one in yellow color
      
      points(zeroDattest[,1],zeroDattest[,2],col='blue') #this will plot data points for class zero in green color
      #on the same graph
      
    } else{
      x1 = oneDat[,1]
      x2 = oneDat[,2]
      plot(x1,x2,col='yellow')  	#this will plot data points for class one in yellow color
      
      points(zeroDat[,1],zeroDat[,2],col='green') #this will plot data points for class zero in green color
      #on the same graph
      
      
      mistake = predict!=labels			     #here suppose your predictions are stored in predict
      
      points(data[mistake,1],data[mistake,2],col='red')  	#this will plot all the mistakes in red color
      
    } 
   

   

   }

#to execute the first case run the following functions
cat("For Case 1:")
bayes(nrow(trainX),trainX,diag(2),diag(2),FALSE)
bayes(nrow(test_data),test_data,diag(2),diag(2),TRUE)
#to execute the second case run the following functions
cat("For Case 2:")
bayes(nrow(trainX),trainX,cov(trainX),cov(trainX),FALSE)
bayes(nrow(test_data),test_data,cov(trainX),cov(trainX),TRUE)
#to execute the third case run the following functions
cat("For Case 3:")
bayes(nrow(trainX),trainX,oneCov,zeroCov,FALSE)
bayes(nrow(test_data),test_data,oneCov,zeroCov,TRUE)




