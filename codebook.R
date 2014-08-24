library(stringr)
getDescription <- function(name){
    time.variable <- grepl("^t",name)
    mean.variable <- grepl("\\-mean()",name) 
    xyz.variable <- str_extract(name,"\\-[XYZ]")
    body.variable <- grepl("Body",name)
    acc.variable <- grepl("Acc",name)
    jerk.variable <- grepl('Jerk',name)
    description <- "The average of"
    if (mean.variable){
        description <- paste(description, "mean measurements of")
    } else{
        description <- paste(description, "standard deviation measurements of")        
    }
    
    if(is.na(xyz.variable)){
        description <- paste(description, "magnitude of")                
    }
    
    if (body.variable){
        description <- paste(description, "body")
    } else{
        description <- paste(description, "gravitational")        
    }
    
    if (acc.variable){
        description <- paste(description, "linear acceleration")
    } else{
        description <- paste(description, "angular acceleration")        
    }
    
    if (jerk.variable){
        description <- paste(description, "'s jerk")
    }
    
    if (time.variable){
        description <- paste(description, "time domain signals")
    }else{
        description <- paste(description, "frequency domain signals")        
    }
    
    
    if (!is.na(xyz.variable)){
        description <- paste(description, "obtained in",substring(xyz.variable,2,2),"direction")
    }
    description
}

writeAttribute <- function(v,name){
    write(paste("####",name),'CodeBook.md',append = T)
    write("",'CodeBook.md',append = T)
    if (name == 'Subject'){    
        write("Type: Integer, Range: [1,30]",'CodeBook.md',append = T)
        write("",'CodeBook.md',append = T)
        write("Id of the involved subject",'CodeBook.md',append = T)
    }
    else if(name =='Activity'){
        write("Type: String",'CodeBook.md',append = T)
        write("",'CodeBook.md',append = T)        
        write("Values: LAYING, SITTING, STANDING, WALKING, WALKING_DOWNSTAIRS, WALKING_UPSTAIRS",'CodeBook.md',append = T)  
    }
    else
    {
        write(paste("Type: Numeric," , "Range: [-1,1]") ,'CodeBook.md',append = T)
        write("",'CodeBook.md',append = T)
        write(sprintf("%30s",getDescription(name)),'CodeBook.md',append = T)
    }
    write("",'CodeBook.md',append = T)
}

if (file.exists('CodeBook.md')){
    file.remove('CodeBook.md')    
}
for(i in 1:dim(outputData)[2]){
    writeAttribute(outputData[,i],colnames(outputData)[i])
}

