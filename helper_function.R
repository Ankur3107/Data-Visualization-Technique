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

