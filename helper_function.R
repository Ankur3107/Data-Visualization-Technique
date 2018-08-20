#Function 1: 
getNumeric <- function(feature){
                feature <- gsub("\\,", "", feature)
                feature <- gsub("\\$", "", feature)
                feature <- as.numeric(gsub("\\%", "", feature))
                feature
            }

#Function 2:
changeFeaturesIntoNumeric <- function(df, features){
    for(feature in features){
        df[feature] <- as.data.frame(getNumeric(getColumn(feature, df)))
    }
    df
}

#Function 3:
getCleanChar <- function(feature){
    feature <- ifelse(feature=="N/A", NA, feature)
    feature <- ifelse(feature=="No Data", NA, feature)
    feature
}

#Function 4:
getMissingUpdatedDataFrame <- function(df){
    miceMod <- mice(df, method="rf")
    miceModComplete <- complete(miceMod)
    miceModComplete
}

#Function 5:
trim <- function (x){
    x <- gsub("^\\s+|\\s+$", "", x)
    x <- as.character(x)
} 

#Function 6:
getOneHotEncodeConvertedDF <- function(df){
    charDF <- select_if(df,is.character)
    numDF <- select_if(df,is.numeric)
    
    dmy <- dummyVars(~., data = charDF)
    trsf <- data.frame(predict(dmy, newdata = charDF))
    
    cbind(trsf,numDF)
}