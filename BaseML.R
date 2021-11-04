#Load required libraries
library(Matrix)
library(xgboost)
library(data.table)
library(zoo)

#Set working directory
setwd("~/Proyecto CIAT")

datatable <- read.csv("Variables_API1.1 - Tiempos.csv",header=T)

diseaseMLmodel <- function(datatable, twindow=1) {
  
  #List of possible answers for the desired disease
  disNames <- c("Tizón", "Tizón tardío", "Phytophthora", "Phytophthora infestans")
  
  #Obtain the relevant variables and prepare the table
  extrVar <- c("UP1","UA.u.2","UA.u.3","P.n.s.1","P.n.s.2","P.n.apl.2","P.n.oe.1","P.n.oe.2","P.n.oe.9")
  varPos <- lapply(extrVar,FUN=function(x) { which(datatable[,1] == x) })
  varPos <- lapply(varPos, function(x) { x[1] })
  if(length(is.na(varPos)) == 0) {
    varPos <- varPos[-which(is.na(varPos))]
  }
  tableMLt <- datatable[unlist(varPos),]
  rownames(tableMLt) <- tableMLt[,1]
  tableMLt[,1] <- NULL
  tableML <- as.data.frame(t(as.matrix(tableMLt)))
  tableML$time <- index(rownames(tableML))
  tableML$density <- tableML$P.n.s.1/tableML$UA.u.3
  
  #Keep only the data of disease appearances of the disease names in disNames
  tableML$P.n.oe.1 <- tableML$P.n.oe.1*(tableML$P.n.oe.9 %in% disNames)
  tableML$P.n.oe.2 <- tableML$P.n.oe.2*(tableML$P.n.oe.9 %in% disNames)
  
  #Set the time order of the table
  setorder(tableML, "time")
  
  #Generate the training table considering the effects of the variables in n-2 -> output in n-1
  train.1 <- tableML[(1+twindow):(length(tableML[,1])-twindow),]
  
  #Calculate time since last control product application
  tempApl <- vector()
  for(i in 2:length(tableML[,1])) {
    tempApl <- c(tempApl,(tableML$P.n.apl.2[i]-tableML$P.n.apl.2[(i-1)]))
  }
  
  #Prepare the output variable and the training dataset
  train.output <- tableML$P.n.oe.1[(2+twindow):(length(tableML$P.n.oe.1))]
  predData <- last(tableML)
  train.1 <- subset(train.1, select=-c(UP1,UA.u.3,P.n.s.1,P.n.apl.2,time))
  predData <- subset(predData, select=-c(UP1,UA.u.3,P.n.s.1,P.n.apl.2,time))
  train.1$lastApl <- tempApl[1:(length(tempApl)-1)]
  predData$lastApl <- last(tempApl)
  train.1$diseaseN <- train.output
  predData$diseaseN <- 0
  train.1 <- rbind(train.1,predData)
  
  #Create the training sparse matrix
  sparse_matrix <- sparse.model.matrix(diseaseN~.-1,data=train.1)
  #Separate the prediction data of t
  predData <- last(sparse_matrix)
  sparse_matrix <- sparse_matrix[-dim(sparse_matrix)[1],]
  
  #Train the model
  if(file.exists("DiseaseModel.model")) {
    disModel <- xgboost(data=sparse_matrix, label=train.output,eta=0.2,nrounds = 1000,max_depth=20,nthread=4,xgb_model = "DiseaseModel.model")
  }
  else {
    disModel <- xgboost(data=sparse_matrix, label=train.output,eta=0.2,nrounds = 1000,max_depth=20,nthread=4, early_stopping_rounds = 100)
  }
  xgb.save(disModel,"DiseaseModel.model")
  
  prediction <- predict(disModel,predData)
  
  return(prediction)
}
