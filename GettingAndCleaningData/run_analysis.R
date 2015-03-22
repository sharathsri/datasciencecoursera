library(dplyr)
run_analysis<-function()
{
   x_test<-read.table(".//test//X_test.txt", colClasses="numeric")
   x_test<-tbl_df(x_test)
   x_test<-readData(x_test,".//test//subject_test.txt",".//test//y_test.txt")
   x_train<-read.table(".//train//X_train.txt", colClasses="numeric")
   x_train<-tbl_df(x_train)
   x_train<-readData(x_train,".//train//subject_train.txt",".//train//y_train.txt")
   tData<-bind_rows(x_test,x_train)
   tData<-arrange(tData, Activity_Type, Activity_Name, Subject_ID)
   tData<-group_by(tData, Activity_Type, Activity_Name, Subject_ID)
   max_cols<-ncol(tData)
   tidy_cols<-paste("Mean_of",colnames(tData)[4:max_cols],sep="_")
   tData<-summarise_each(tData,funs(mean))
   colnames(tData)[4:max_cols]<-tidy_cols
   write.table(tData,file=".\\Step5_Tidy_Data.txt",row.name=FALSE)
   tData

}
# Extract the required features from the features.txt file.
getReqColsIdx<-function()
{
 reqFeatures<-read.table(".//features.txt", colClasses="character")
 sort(c(grep("-mean",reqFeatures[ ,2]),grep("-std",reqFeatures[ ,2])))
}
getReqColsNames<-function(){
 reqColNames<-read.table(".//features.txt", colClasses="character")
 reqColNames[getReqColsIdx(),2]
}

readData<-function(superset,subPath,typePath){
	sel_cols<-select(superset,getReqColsIdx())
	colNames<-gsub("-", "_", getReqColsNames())
	colNames<-gsub("mean\\(\\)", "Avg", colNames)
	colNames<-gsub("std\\(\\)", "Std_Deviation_From_Mean", colNames)
	colnames(sel_cols)<-colNames
	subs<-read.table(subPath, colClasses="integer")
	colnames(subs)<-c("Subject_ID")
	type<-read.table(typePath, colClasses="integer")
	colnames(type)<-c("Activity_Type")
	type_name<-data.frame(type)
	colnames(type_name)<-c("Activity_Name")
	data<-bind_cols(type,type_name,subs,sel_cols)
	data[data$Activity_Name == 1, 2]<-"WALKING"
	data[data$Activity_Name == 2, 2]<-"WALKING_UPSTAIRS"
	data[data$Activity_Name == 3, 2]<-"WALKING_DOWNSTAIRS"
	data[data$Activity_Name == 4, 2]<-"SITTING"
	data[data$Activity_Name == 5, 2]<-"STANDING"
	data[data$Activity_Name == 6, 2]<-"LAYING"
	data<-tbl_df(data)
	data
	
}