# Accuracy code, set labels and prediction as input, complicated method
acc <- function(x, y) {
  accu = 0
  for(i in 1:length(x))
  {
    if( x[i] == y[i] )
    {
      accu <- accu + 1;
    }
  }
  return(100*accu/length(y))
}
#1.4.4
# Smoothing example code, this can be included in your code and will change the code so remember to 
# load the images again, ( A nice feature of R )
smoothImage <- function(grayImg){ # This function is run in loadSinglePersonsData check the code
  kernel <- makeBrush(9, shape='Gaussian', step=TRUE, sigma=0.8) # There exist a number of different functions
  #print(kernel) # just to show what we have made
  smoothed <- filter2(grayImg, kernel) # filter the image using the kernel
  return(smoothed)
}


# read image and perform knn
#folder <- "/home/frederik/workspace/svn/trunk/2018/group"
folder <- "C:/Users/DinoPC/Desktop/2018/2018/group"

id <- loadSinglePersonsData(100,7,0, folder)
id <- data.frame(id)
id$X1 <- factor(id$X1)

set.seed(423)
shuffledData <- id[sample(nrow(id)),]

test_pred <- knn(train = shuffledData[1:2000, -1], test = shuffledData[2000:4000, -1], cl = shuffledData[1:2000,1], k=1)

resultMatrix = CrossTable(x = shuffledData[2000:4000,1], y = test_pred, prop.chisq=FALSE)

sum(diag(resultMatrix$t))/sum(resultMatrix$t)




folds <- createFolds(id$X1, k = 10)
listOfFolders <- c(1:10)

for(i in 1:10)
{
  id_train <- id[-folds[[i]],-1]
  id_test <- id[folds[[i]],-1]
  
  id_train_labels <- id[-folds[[i]],1]
  id_test_labels <- id[folds[[i]],1]
  
  id_test_pred <- knn(train = id_train, test = id_test, cl = id_train_labels, k=1)
  
  listOfFolders[i] <- acc(id_test_pred,id_test_labels)
}

print(listOfFolders)
boxplot(listOfFolders,data=listOfFolders, main="10 Cross Test", xlab="", ylab="Recognition") 

# Example code for reading all images into a list, DPI 100
getAllData <- function(dataList){
  id <- data.frame()
  idList <- list()
  for(i in 1:length(dataList))
  {
    if( length(dataList[[i]]) > 0  ){
      for(j in 1:length(dataList[[i]])){
        idTemp <- loadSinglePersonsData(100,i - 1,j,folder)
        idList <- append(idList, list(idTemp))
      }
    }
  }
  return(idList)
}
# Set a "list of list" for each group and member and run
folder <- "C:/Users/DinoPC/Desktop/2018/2018/group"

dataList <- list( list(1,2,3), list(1,2,3), list(1,2,3), list(1,2), list(1,2,3), list(1,2,3), list(1), list(1), list(1), list(), list(1,2), list(1,2,3), list(), list(1)  )
idList <- getAllData(dataList)

# You can now iterate trough the list
for(i in 1:length(idList)){
  idTemp <- idList[i]
  idTemp <- data.frame(idTemp)
}
# you can combine the data frames using "id <- rbind(id, idTemp)"