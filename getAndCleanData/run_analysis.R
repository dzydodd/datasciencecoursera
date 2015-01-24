run_analysis<-function(){
        #combine train data and test data
        train<-read.table("train/X_train.txt")
        test<-read.table("test/X_test.txt")
        total<-rbind(train,test)
        
        #select all features that contains std() or mean()
        features<-read.table("features.txt")
        z<-grepl("(std\\()|(mean\\()",features[,2])
        
        #extract all record meets specified features
        extracted<-total[,features[z,1]]
        
        #set extracted data frame column names
        names(extracted)<-as.vector(factor(features[z,2]))
        
        #subject and activity
        subject_train<-read.table("train/subject_train.txt")
        subject_test<-read.table("test/subject_test.txt")
        subject_total<-rbind(subject_train,subject_test)
        
        activity_train<-read.table("train/y_train.txt")
        activity_test<-read.table("test/y_test.txt")
        activity_total<-rbind(activity_train,activity_test)
        
        #convert activities of record from interge to meaningful string
        factorActivityTotal<-as.factor(activity_total[,1])
        activity_lables<-read.table("activity_labels.txt")
        levels(factorActivityTotal)<-as.vector(activity_lables[,2])
        
        #merge extracted data with subject and activity
        x<-cbind(subject_total,as.data.frame(factorActivityTotal),extracted)
        names(x)[c(1,2)]=c("subject","activity")
        
        #iterate to calculate average of features
        temp<-NULL
        for (i in c(1:30)){
                for (j in as.vector(activity_lables[,2]))
                {
                        spec<-sapply(x[x$subject==i&x$activity==j,c(3:dim(x)[2])],mean)
                        spec<-c(subject=i,activity=j,spec)
                        temp<-rbind(temp,spec)
                }
        }
        
        #final data
        newdf<-as.data.frame(temp)
        rownames(newdf)<-NULL
        #write to local file
        write.table(newdf,file="newfile.txt",row.name=FALSE)
}
