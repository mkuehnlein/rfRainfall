##################################################
#### random forest classification/regression #####
##################################################

# Clear workspace
rm(list=ls(all=TRUE))

## SET WORKING DIRECTORY
path.wd <- ("/home/mkuehnlein/randomForest/rf_rain/")
setwd(path.wd)

path.in <- "/home/mkuehnlein/casestudies/rf_input/vp03/day/om/"
path.out <- "/home/mkuehnlein/randomForest/rf_rain/results/vp03/rprocess2/day/om/"


## LOAD LIBRARY
library(latticeExtra)
library(randomForest)
# Libraries for parallelization
library(foreach)
library(doSNOW)
library(parallel)





for (i in 1:30){

  # name version
  if(i<10){
    v <- paste("v0",i,sep="");
  } else if(i>=10) {
    v <- paste("v",i,sep="");
  } else {
        print("FATAL: missing Arguments");
  }
  print(v);

  path.out.akt <- paste(path.out,v,sep="/")
  dir.create(path.out.akt, recursive = TRUE, showWarnings = FALSE)

  # READ TRAIN DATA
  filename <- paste(paste(paste(path.in,"rfInput_day_om_vp03_train_",sep=""),v,sep=""),".dat",sep="")
  readData1 <- read.table(filename, 
                   header=T, 
                   row.names=NULL, 
                   na.strings="-99.000000")
  # DATA -> convective and stratiform data sets
  readDataS <- subset(readData1, readData1$RProcess2 == "stratiform")
  readDataC <- subset(readData1, readData1$RProcess2 == "convective")
  rm(readData1)

  attach(readDataS) 
  trainDataS <- data.frame(chDate,
                    x,
                    y,
		    B01_ca02,
		    B02_ca02,
		    B03_ca02,
		    #B01_ca01,
		    #B02_ca01,
		    #B03_ca01,
                    B04,
                    B05,
                    B06,
                    B07,
                    B08,
                    B09,
                    B10,
                    B11,
                    Tau,
                    Aef,
		    CWP,
		    B0103,
                    #B0409,
                    #B0406,
                    B0709,
                    B0910,
                    B0509,
                    B0610,
		    #SZen,
                    #RInfo
		    #RProcess1,
		    #RProcess2,
		    #RProcess3,
		    Rain
		    )
  detach(readDataS)
  rm(readDataS)

  attach(readDataC) 
  trainDataC <- data.frame(chDate,
                    x,
                    y,
		    B01_ca02,
		    B02_ca02,
		    B03_ca02,
		    #B01_ca01,
		    #B02_ca01,
		    #B03_ca01,
                    B04,
                    B05,
                    B06,
                    B07,
                    B08,
                    B09,
                    B10,
                    B11,
                    Tau,
                    Aef,
		    CWP,
		    B0103,
                    #B0409,
                    #B0406,
                    B0709,
                    B0910,
                    B0509,
                    B0610,
		    #SZen,
                    #RInfo
		    #RProcess1,
		    #RProcess2,
		    #RProcess3,
		    Rain
		    )
  detach(readDataC)
  rm(readDataC)

  names(trainDataS)
  names(trainDataC)
  unique(trainDataS$chDate)
  unique(trainDataC$chDate)
  nrow(trainDataC)
  nrow(trainDataS)



  # READ TEST DATA
  filename <- paste(paste(paste(path.in,"rfInput_night_om_vp03_test_",sep=""),v,sep=""),".dat",sep="")
  readData1 <- read.table(filename, 
                   header=T, 
                   row.names=NULL, 
                   na.strings="-99.000000")
  # DATA -> convective and stratiform data sets
  readDataS <- subset(readData1, readData1$RProcess2 == "stratiform")
  readDataC <- subset(readData1, readData1$RProcess2 == "convective")
  rm(readData1)

  attach(readDataS) 
  testDataS <- data.frame(chDate,
                    x,
                    y,
		    B01_ca02,
		    B02_ca02,
		    B03_ca02,
		    #B01_ca01,
		    #B02_ca01,
		    #B03_ca01,
                    B04,
                    B05,
                    B06,
                    B07,
                    B08,
                    B09,
                    B10,
                    B11,
                    Tau,
                    Aef,
		    CWP,
		    B0103,
                    #B0409,
                    #B0406,
                    B0709,
                    B0910,
                    B0509,
                    B0610,
		    #SZen,
                    #RInfo
		    #RProcess1,
		    #RProcess2,
		    #RProcess3,
		    Rain
		    )
  detach(readDataS)
  rm(readDataS)

  attach(readDataC) 
  testDataC <- data.frame(chDate,
                    x,
                    y,
		    B01_ca02,
		    B02_ca02,
		    B03_ca02,
		    #B01_ca01,
		    #B02_ca01,
		    #B03_ca01,
                    B04,
                    B05,
                    B06,
                    B07,
                    B08,
                    B09,
                    B10,
                    B11,
                    Tau,
                    Aef,
		    CWP,
		    B0103,
                    #B0409,
                    #B0406,
                    B0709,
                    B0910,
                    B0509,
                    B0610,
		    #SZen,
                    #RInfo
		    #RProcess1,
		    #RProcess2,
		    #RProcess3,
		    Rain
		    )
  detach(readDataC)
  rm(readDataC)

  names(testDataC)
  names(testDataC)
  unique(testDataC$chDate)
  unique(testDataC$chDate)
  nrow(testDataC)
  nrow(testDataS)


  ## PREDICTION ##
  ## stratiform ##
  ## training
  set.seed(41)
  ## Parallel execution of randomForest via package 'foreach'
 
  ## Number of cores
  n.cores <- detectCores() 
 
  ## Register SNOW parallel backend with 'foreach' package
  registerDoSNOW(makeCluster(n.cores, type="SOCK"))
 
  ## Define desired parameters stratiform
  n.tree <- 500
  m.try <- 8
 
  ## Parallel execution of randomForest stratiform
  system.time(train.rfS <- foreach(ntree=rep(ceiling(n.tree/n.cores),n.cores), .combine=combine, .packages="randomForest") %dopar%
   randomForest(trainDataS[,5:ncol(trainDataS)-1], 
 	       trainDataS[ , names(trainDataS) %in% c("Rain")], 
 	       ntree=ntree, 
 	       mtry=m.try, 
 	       importance=TRUE, 
 	       do.trace = FALSE))

  ## predict rainfall rate for test data set
  test.predict <- predict(train.rfS, testDataS[,1:ncol(testDataS)])

  ## writing prediction 
  ## convert factor to array 
  Rain.predict <- test.predict
  result <- cbind(testDataS,Rain.predict)
  names(result)

  ## WRITE DATA
  write.table(cbind(result$chDate,result$x,result$y,result$Rain,result$Rain.predict), 
  file=paste(path.out.akt,"prediction_stratiform.dat", sep = "/"),
  row.names = FALSE,
  col.names = FALSE,
  append = FALSE)



  ## convective ##
  ## training
  set.seed(41)
  ## Parallel execution of randomForest via package 'foreach'
 
  ## Number of cores
  n.cores <- detectCores() 
 
  ## Register SNOW parallel backend with 'foreach' package
  registerDoSNOW(makeCluster(n.cores, type="SOCK"))
 
  ## Define desired parameters convective
  n.tree <- 500
  m.try <- 7
 
  ## Parallel execution of randomForest convective
  system.time(train.rfC <- foreach(ntree=rep(ceiling(n.tree/n.cores),n.cores), .combine=combine, .packages="randomForest") %dopar%
   randomForest(trainDataC[,5:ncol(trainDataC)-1], 
 	       trainDataC[ , names(trainDataC) %in% c("Rain")], 
 	       ntree=ntree, 
 	       mtry=m.try, 
 	       importance=TRUE, 
 	       do.trace = FALSE))

  ## predict rainfall rate for test data set
  test.predict <- predict(train.rfC, testDataC[,1:ncol(testDataC)])

  ## writing prediction 
  ## convert factor to array 
  Rain.predict <- test.predict
  result <- cbind(testDataC,Rain.predict)
  names(result)

  ## WRITE DATA
  write.table(cbind(result$chDate,result$x,result$y,result$Rain,result$Rain.predict), 
  file=paste(path.out.akt,"prediction_convective.dat", sep = "/"),
  row.names = FALSE,
  col.names = FALSE,
  append = FALSE)


file.copy("rf_rainDay_om_prediction.Rout", path.out.akt, overwrite = TRUE, copy.mode = TRUE)

}


## copy Rout #####################
file.copy("rf_rainDay_om_prediction.Rout", path.out, overwrite = TRUE, copy.mode = TRUE)
file.remove("rf_rainDay_om_prediction.Rout")



