# This function reads all the .csv file from a directory into a list of data frames.
readAllCsv<-function(dir) {
    fileNames<-list.files(dir,pattern=".csv")
    fullFileNames<-paste(dir,fileNames,sep="/")
    #names(fullFileNames)<-fileNames
    outputfiles<-lapply(fullFileNames, read.csv)
}

# The function calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors.
pollutantmean<-function(dir, pollutant, id=1:332) {
    files<-readAllCsv(dir)
    count<-0
    sum<-0
    for (i in id){
        vec<-files[[i]][,pollutant]
        vec<-vec[!is.na(vec)]
        count<-count+length(vec)
        sum<-sum+sum(vec)
    }
    sum/count
    
}

#Function that reads a directory full of files and reports the number of completely observed cases in each data file.
complete<-function(dir,id=1:332) {
    files<-readAllCsv(dir)
    selectedFiles<-files[id]
    cleanedData<-lapply(selectedFiles,na.omit)
    completeRows<-unlist(lapply(cleanedData,nrow))
    output<-data.frame(id,completeRows)
    names(output)<-c("id","nobs")
    return(output)
}

#This function that takes a directory of data files and a threshold for complete cases and calculates the 
#correlation between sulfate and nitrate for monitor locations where the number of completely observed cases 
#(on all variables) is greater than the threshold. 
corr<-function(dir, threshold=0) {
    completeData<-complete(dir)
    idAboveThreshold<-completeData[completeData$nobs>threshold,]$id
    
    files<-readAllCsv(dir)
    selectedFiles<-files[idAboveThreshold]
    cleanedSelectedFiles<-lapply(selectedFiles,na.omit)
    sapply(cleanedSelectedFiles,function(x)cor(x$sulfate,x$nitrate))
}