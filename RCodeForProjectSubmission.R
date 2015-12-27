#load the library
library(caret)

#read the training data
datatrain = read.csv("pml-training.csv")

#explore the data and identify the NA, missing data in the features
summary(datatrain)

#partition the 75% of data for training
#only use the full records for training, ignore the features with missing/NA
#prepare 25% of data for cross-validation
inTrain<-createDataPartition(y=datatrain$classe,p=0.75,list=FALSE)
datatrain_clean<-datatrain[inTrain,8:160]
datacv_clean<-datatrain[-inTrain,8:160]

#random Forrest
require(useful)
require(randomForest)
set.seed(9747)

##training data
rfdata<-classe~roll_belt+pitch_belt+yaw_belt+total_accel_belt+gyros_belt_x+gyros_belt_y+gyros_belt_z+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+pitch_arm+yaw_arm+total_accel_arm+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell+gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z+accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm+gyros_forearm_x+gyros_forearm_y+gyros_forearm_z+accel_forearm_x+accel_forearm_y+accel_forearm_z+magnet_forearm_x+magnet_forearm_y+magnet_forearm_z
xrf<-build.x(rfdata,data=datatrain_clean)
yrf<-build.y(rfdata,data=datatrain_clean)

##cross validation data
rfdatapred<-classe~roll_belt+pitch_belt+yaw_belt+total_accel_belt+gyros_belt_x+gyros_belt_y+gyros_belt_z+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+pitch_arm+yaw_arm+total_accel_arm+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell+gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z+accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm+gyros_forearm_x+gyros_forearm_y+gyros_forearm_z+accel_forearm_x+accel_forearm_y+accel_forearm_z+magnet_forearm_x+magnet_forearm_y+magnet_forearm_z
xrfpred<-build.x(rfdatapred,data=datacv_clean)
yrfpred<-build.y(rfdatapred,data=datacv_clean)

rfrtrain<-randomForest(x=xrf,y=yrf,xtest = xrfpred,ytest=yrfpred)

#read the test data
datatest = read.csv("pml-testing.csv")
datatest_clean<-datatest[,8:159]
datatest_clean[,"classe"]<-factor(c("A","B","C","D","E"))
names(datatest_clean)
datatest_clean

rfdatatest<-classe~roll_belt+pitch_belt+yaw_belt+total_accel_belt+gyros_belt_x+gyros_belt_y+gyros_belt_z+accel_belt_x+accel_belt_y+accel_belt_z+magnet_belt_x+magnet_belt_y+magnet_belt_z+roll_arm+pitch_arm+yaw_arm+total_accel_arm+gyros_arm_x+gyros_arm_y+gyros_arm_z+accel_arm_x+accel_arm_y+accel_arm_z+magnet_arm_x+magnet_arm_y+magnet_arm_z+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell+gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z+accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z+magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm+gyros_forearm_x+gyros_forearm_y+gyros_forearm_z+accel_forearm_x+accel_forearm_y+accel_forearm_z+magnet_forearm_x+magnet_forearm_y+magnet_forearm_z
xrftest<-build.x(rfdatatest,data=datatest_clean)
yrftest<-build.y(rfdatatest,data=datatest_clean)

rfresult<-randomForest(x=xrf,y=yrf,xtest = xrftest,ytest=yrftest)
rfresult
rfpred<-rfresult["test"][[1]][1]
rfpred

#### END ####

