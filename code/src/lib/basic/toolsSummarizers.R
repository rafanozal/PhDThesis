# -----------------------------------------------------------------------------
#
# This script contain functions to summarize dataframes into other dataframe,
# such as summarize categorical column or summarize numerical column. Always
# in a human friendly format.
#
# -----------------------------------------------------------------------------


# R is horrible, and you can't comment whole blocks, sort of like:
# /**
#
# write whatever here
#
# */



    


# Given a categorical column of a table, find the count, relative frequency,
# and cumulative relative frequency of each category, and return it as a
# dataframe. The numbers are not rounded. (if you want round them yourself).
# However for irrational numbers you might get a cumulative not equal to 1.
#
# Example of the returning dataframe:
#
# --------------------------------------------------
# "Modality" | "Count" | "Relative" | "Cumulative" |
# --------------------------------------------------
# Category 1 |    49   | 0,556818   | 0,556818     |
# Category 5 |    19   | 0.215909   | 0.772727     |
# Category 2 |    9    | 0.102273   | 0.875        |
# Category 3 |    7    | 0.079545   | 0.954545     |
# Category 4 |    4    | 0.045455   | 1            |
# --------------------------------------------------
#
# You can sort in ascending or descending or random order. But it always sort
# by the absolute count.
#
# If you run this with a non categorical column, it will automatically
# transform the numbers into categories, but it will print/log a warning
# telling you that you run this on a numerical and you should have maybe run
# the summary for numerical instead.
#
# String sorted ; "top"    = From bigger to smaller (DEFAULT)
#                 "bottom" = From smaller to bigger
#                 Any other string = Random order, probably by the same order as
#                                    they are encounter in the dataframe. But it
#                                    respect the factor order
#                                    (ie: none, low, medium, high)
# 
# Int crop      ; how many rows in the resulting dataframe do you want.
#                 For example, crop = 10 means you will take the first 10 lines
#                 only.
#                 crop <= 0 means you take them all (DEFAULT)
#
# Int round     ; If you want the results to be round to the nearest number up
#                 to whatever decimals you indicate in this variable. Default
#                 is 0 and it means no rounding.


summarizeCategorical <- function(tableBase, countingIndex, sorted = "top", crop = 0, roundMe = 0){
  
  myTableName = deparse(substitute(tableBase))
  
  # Convert the numerical into categories if needed
  countingVariableType = class(tableBase[,countingIndex])
  
  if(countingVariableType != "character" && countingVariableType != "factor"){
    
	    # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
	    myLevels = sort(unique(tableBase[,countingIndex]))
	    myLevels = as.character(myLevels)
	    
	    tableBase[,countingIndex] = as.character(tableBase[,countingIndex])
	    tableBase[,countingIndex] = factor(tableBase[,countingIndex], levels = myLevels)
	    
	    print("WARNING!!")
	    print("Doing the summary table for:")
	    print(myTableName)
	    print("With index:")
	    print(countingIndex)
	    print("I found a numerical variable")
	    print("I transformed into categorical automatically and did the summary anyway")
	    print("Maybe you wanted to do something else?")
    
  }
  
  # Init the modalities to levels if they exist, otherwise, get them by appareance order
  
  myModalities = getCategories(tableBase,countingIndex)
  

  totalModalities = length(myModalities)
  totalRows       = nrow(tableBase)
  
  summaryDF  =  data.frame(matrix(NA, nrow = totalModalities, ncol = 4))
  colnames(summaryDF) = c("Modality", "Count", "Relative", "Cumulative")

  for(i in 1:totalModalities){
    
	    currentModality = myModalities[i]
	    summaryDF$Modality[i] = as.character(currentModality)
	    summaryDF$Count[i]    = sum( tableBase[,countingIndex] == currentModality , na.rm = TRUE) # FUCK THE NAAS IN THIS STIPID LANGUAGE!! FUCK!"! 3 FUCKING DAYS TO FIND THIS POS BUG FROM HELL!!!!
	    summaryDF$Relative[i] = summaryDF$Count[i] / totalRows

  }
  
  # Make sure the modality column follow the same order
  summaryDF$Modality = factor(summaryDF$Modality , levels = myModalities)
  
  # Sort by option
  # -- Big to small
  # -- Small to big
  # -- Do nothing
  if(sorted == "top"){
    
    summaryDF = summaryDF[rev(order(summaryDF$Count)),]
    
  }
  else{
    
    if(sorted == "bottom"){
      
      summaryDF = summaryDF[order(summaryDF$Count),]  
      
      
    }
    
  }
  
  
    # Crop the X first results only
    {
    totalCropping = totalModalities
    if(crop > 0) totalCropping = crop
    summaryDF = summaryDF[1:totalCropping,]
  }
  
    summaryDF$Cumulative = cumsum(summaryDF$Relative)
  
  
    # If you want some rounding to the final numbers
    if(roundMe != 0){
    
        summaryDF$Relative   = round(summaryDF$Relative,   roundMe)
        summaryDF$Cumulative = round(summaryDF$Cumulative, roundMe)
      
    }
  
  return(summaryDF)
  
  
}



# For a given table and column index, tells you which type of variable you have
#
# The possible datatypes are:
#
#     - Date
#     - Discrete
#     - Continuous
#     - Categorical
#
getVariableType <- function(tableBase, columnIndex){
    
    currentType    = "Date"
    myCurrentClass = class(tableBase[,columnIndex])

    if(myCurrentClass == "character" || myCurrentClass == "factor"){    
    
        currentType = "Categorical"
        
    }
    else{

        if(myCurrentClass == "integer"){
    
            currentType = "Discrete"
            
        }
        else{
            
            if(myCurrentClass == "numeric"){

                currentType = "Continuous"

            }

        }
        
    }
 
    return(currentType)    
     
}
    



    
# Count how many categories of a given index are in a table
# NA don't work because R is shit
countCategories <- function(tableBase, columnIndex, categoryName){
    
    return(sum(tableBase[, columnIndex] == categoryName))
    
}

# Count how many categories of a given combination are in a table
# NA don't work because R is shit
countMarginals <- function(tableBase, columnIndexA, categoryA, columnIndexB, categoryB){
    
    currentComboTable  = subset(tableBase, tableBase[,columnIndexA] == categoryA & tableBase[,columnIndexB] == categoryB)
    return(nrow(currentComboTable))
    
}
    
    


# For a table and a couple of categorical variables, count how many combination of each do you have
#
# The result is a table such as this, including marginals
#
# ---------------------------------------------------------
#             | CategoryB1 | CategoryB2 | CategoryB3 | 
# ---------------------------------------------------------
# Category A1 |    9      |     45      |    2       |  56
# Category A2 |    19     |     6       |    0       |  25
# Category A3 |    1      |     0       |    0       |  1
# Category A4 |    0      |     80      |    4       |  84
# ---------------------------------------------------------
#                 29            131          6       |  166

# --------------------------------------------------
summarizeBicategorical <- function(tableBase, countingIndexA, countingIndexB){

    # Convert the numerical in categories if needed
    {
        groupingVariableA = class(tableBase[,countingIndexA])
        if(groupingVariableA != "character" && groupingVariableA != "factor") {
            
            # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
            myLevels = sort(unique(tableBase[,countingIndexA]))
            myLevels = as.character(myLevels)
            
            tableBase[,countingIndexA] = as.character(tableBase[,countingIndexA])
            tableBase[,countingIndexA] = factor(tableBase[,countingIndexA], levels = myLevels)
            
        }
        
        groupingVariableB = class(tableBase[,countingIndexB])
        if(groupingVariableB != "character" && groupingVariableB != "factor") {
            
            # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
            myLevels = sort(unique(tableBase[,countingIndexB]))
            myLevels = as.character(myLevels)
            
            tableBase[,countingIndexB] = as.character(tableBase[,countingIndexB])
            tableBase[,countingIndexB] = factor(tableBase[,countingIndexB], levels = myLevels)
            
        }        
        
    }
    
    # Get info about different categories
    {
          
        # Factors, I hate you, that's why I'm using the summarize function
        # that has everything inside done properly.
        currentSummaryA = summarizeCategorical(tableBase, countingIndexA, sorted="none")
        currentSummaryB = summarizeCategorical(tableBase, countingIndexB, sorted="none")

        myCategoriesA   = as.character(currentSummaryA[,1])
        nCategoriesA    = length(myCategoriesA)
        groupingNameA   = colnames(tableBase)[countingIndexA]
          
        myCategoriesB   = as.character(currentSummaryB[,1])
        nCategoriesB    = length(myCategoriesB)
        groupingNameB   = colnames(tableBase)[countingIndexB]
                
    }
    
    # Create the resulted DF and init to 0,
    # and start couting all combinations
    {
    
        # Create the empty DF
        resultsDF = DF(nCategoriesA+1, nCategoriesB+2, defaultValue = 0)
        colnames(resultsDF) = c("",myCategoriesB,"")
        for(i in 1:nCategoriesA){
            resultsDF[i,1] = myCategoriesA[i]
        }
        
        # Count all combinations
        for(i in 1:nCategoriesA){

            # For this A modality        
            currentAModality = myCategoriesA[i]
            
            for(j in 1:nCategoriesB){
            
                # And this B modality
                currentBModality   = myCategoriesB[j]
                    
                # Get the subset table and count how many of those we have
                currentComboTable  = subset(tableBase, tableBase[,countingIndexA] == currentAModality & tableBase[,countingIndexB] == currentBModality)
                resultsDF[i,j+1]   = nrow(currentComboTable)
            }
                
        }
        
        # Count the marginals
        {
            
            # Marginals for A
            for(i in 1:nCategoriesA){
                # For this A modality        
                currentAModality = myCategoriesA[i]
                
                # Get the subset table and count how many of those we have
                currentComboTable           = subset(tableBase, tableBase[,countingIndexA] == currentAModality)
                resultsDF[i,nCategoriesB+2] = nrow(currentComboTable)
            }
            
            # Marginals for B
            for(j in 1:nCategoriesB){
                # For this B modality        
                currentBModality = myCategoriesB[j]
                
                # Get the subset table and count how many of those we have
                currentComboTable             = subset(tableBase, tableBase[,countingIndexB] == currentBModality)
                resultsDF[nCategoriesA+1,j+1] = nrow(currentComboTable)
            }
            
            # Total of everything
            resultsDF[nCategoriesA+1,nCategoriesB+2] = nrow(tableBase)
        }
        
        # There is one last marginal that needs to be coerced to ""
        # Since the result table has been init to 0, the result that is
        # under the list of A modalities is a 0, which looks weird
        resultsDF[nCategoriesA+1,1] = ""
             
           
    }
    
    # Return the resulting DF
    
    return(resultsDF)
    
}

summarizeBicategoricalV2 <- function(tableBase, countingIndexA, countingIndexB, skipModalities){

    # Convert the numerical in categories if needed
    {
        groupingVariableA = class(tableBase[,countingIndexA])
        if(groupingVariableA != "character" && groupingVariableA != "factor") {
            
            # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
            myLevels = sort(unique(tableBase[,countingIndexA]))
            myLevels = as.character(myLevels)
            
            tableBase[,countingIndexA] = as.character(tableBase[,countingIndexA])
            tableBase[,countingIndexA] = factor(tableBase[,countingIndexA], levels = myLevels)
            
        }
        
        groupingVariableB = class(tableBase[,countingIndexB])
        if(groupingVariableB != "character" && groupingVariableB != "factor") {
            
            # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
            myLevels = sort(unique(tableBase[,countingIndexB]))
            myLevels = as.character(myLevels)
            
            tableBase[,countingIndexB] = as.character(tableBase[,countingIndexB])
            tableBase[,countingIndexB] = factor(tableBase[,countingIndexB], levels = myLevels)
            
        }        
        
    }
    
    # Get info about different categories
    {
        
        # Factors, I hate you, that's why I'm using the summarize function
        # that has everything inside done properly.
        currentSummaryA = summarizeCategorical(tableBase, countingIndexA, sorted="none")
        currentSummaryB = summarizeCategorical(tableBase, countingIndexB, sorted="none")

        myCategoriesA   = getModalitiesV2(tableBase,      countingIndexA, skipModalities)
        nCategoriesA    = getTotalModalitiesV2(tableBase, countingIndexA, skipModalities)
        groupingNameA   = colnames(tableBase)[countingIndexA]
          
        myCategoriesB   = getModalitiesV2(tableBase,      countingIndexB, skipModalities)
        nCategoriesB    = getTotalModalitiesV2(tableBase, countingIndexB, skipModalities)
        groupingNameB   = colnames(tableBase)[countingIndexB]
                
    }
    
    # Create the resulted DF and init to 0,
    # and start couting all combinations
    {
    
        # Create the empty DF
        resultsDF = DF(nCategoriesA+1, nCategoriesB+2, defaultValue = 0)
        colnames(resultsDF) = c("",myCategoriesB,"")
        for(i in 1:nCategoriesA){
            resultsDF[i,1] = myCategoriesA[i]
        }
        
        # Count all combinations
        for(i in 1:nCategoriesA){

            # For this A modality        
            currentAModality = myCategoriesA[i]
            
            for(j in 1:nCategoriesB){
            
                # And this B modality
                currentBModality   = myCategoriesB[j]
                    
                # Get the subset table and count how many of those we have
                currentComboTable  = subset(tableBase, tableBase[,countingIndexA] == currentAModality & tableBase[,countingIndexB] == currentBModality)
                resultsDF[i,j+1]   = nrow(currentComboTable)
            }
                
        }
        
        # Count the marginals
        {
            
            # Marginals for A
            for(i in 1:nCategoriesA){
                # For this A modality        
                currentAModality = myCategoriesA[i]
                
                # Get the subset table and count how many of those we have
                currentComboTable           = subset(tableBase, tableBase[,countingIndexA] == currentAModality)
                resultsDF[i,nCategoriesB+2] = nrow(currentComboTable)
            }
            
            # Marginals for B
            for(j in 1:nCategoriesB){
                # For this B modality        
                currentBModality = myCategoriesB[j]
                
                # Get the subset table and count how many of those we have
                currentComboTable             = subset(tableBase, tableBase[,countingIndexB] == currentBModality)
                resultsDF[nCategoriesA+1,j+1] = nrow(currentComboTable)
            }
            
            # Total of everything
            resultsDF[nCategoriesA+1,nCategoriesB+2] = sum(resultsDF[,nCategoriesB+2])
        }
        
        # There is one last marginal that needs to be coerced to ""
        # Since the result table has been init to 0, the result that is
        # under the list of A modalities is a 0, which looks weird
        resultsDF[nCategoriesA+1,1] = ""
             
           
    }
    
    # Return the resulting DF
    return(resultsDF)
    
}

summarizeBicategoricalV3 <- function(tableBase, countingIndexA, countingIndexB){

    # Convert the numerical in categories if needed
    {
        groupingVariableA = class(tableBase[,countingIndexA])
        if(groupingVariableA != "character" && groupingVariableA != "factor") {
            
            # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
            myLevels = sort(unique(tableBase[,countingIndexA]))
            myLevels = as.character(myLevels)
            
            tableBase[,countingIndexA] = as.character(tableBase[,countingIndexA])
            tableBase[,countingIndexA] = factor(tableBase[,countingIndexA], levels = myLevels)
            
        }
        
        groupingVariableB = class(tableBase[,countingIndexB])
        if(groupingVariableB != "character" && groupingVariableB != "factor") {
            
            # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
            myLevels = sort(unique(tableBase[,countingIndexB]))
            myLevels = as.character(myLevels)
            
            tableBase[,countingIndexB] = as.character(tableBase[,countingIndexB])
            tableBase[,countingIndexB] = factor(tableBase[,countingIndexB], levels = myLevels)
            
        }        
        
    }
    
    # Get info about different categories
    {
        
        # Factors, I hate you, that's why I'm using the summarize function
        # that has everything inside done properly.
        currentSummaryA = summarizeCategorical(tableBase, countingIndexA, sorted="none")
        currentSummaryB = summarizeCategorical(tableBase, countingIndexB, sorted="none")

        myCategoriesA   = getCategories(tableBase,countingIndexA)
        nCategoriesA    = length(myCategoriesA)
        groupingNameA   = colnames(tableBase)[countingIndexA]
        
        myCategoriesB   = getCategories(tableBase,countingIndexB)
        nCategoriesB    = length(myCategoriesB)
        groupingNameB   = colnames(tableBase)[countingIndexB]
                
    }
    
    # Create the resulted DF and init to 0,
    # and start couting all combinations
    {
    
        # Create the empty DF
        resultsDF = DF(nCategoriesA+1, nCategoriesB+2, defaultValue = 0)
        colnames(resultsDF) = c("",myCategoriesB,"")
        for(i in 1:nCategoriesA){
            resultsDF[i,1] = myCategoriesA[i]
        }
        
        # Count all combinations
        for(i in 1:nCategoriesA){

            # For this A modality        
            currentAModality = myCategoriesA[i]
            
            for(j in 1:nCategoriesB){
            
                # And this B modality
                currentBModality   = myCategoriesB[j]
                    
                # Get the subset table and count how many of those we have
                currentComboTable  = subset(tableBase, tableBase[,countingIndexA] == currentAModality & tableBase[,countingIndexB] == currentBModality)
                resultsDF[i,j+1]   = nrow(currentComboTable)
            }
                
        }
        
        # Count the marginals
        {
            
            # Marginals for A
            for(i in 1:nCategoriesA){
                # For this A modality        
                currentAModality = myCategoriesA[i]
                
                # Get the subset table and count how many of those we have
                currentComboTable           = subset(tableBase, tableBase[,countingIndexA] == currentAModality)
                resultsDF[i,nCategoriesB+2] = nrow(currentComboTable)
            }
            
            # Marginals for B
            for(j in 1:nCategoriesB){
                # For this B modality        
                currentBModality = myCategoriesB[j]
                
                # Get the subset table and count how many of those we have
                currentComboTable             = subset(tableBase, tableBase[,countingIndexB] == currentBModality)
                resultsDF[nCategoriesA+1,j+1] = nrow(currentComboTable)
            }
            
            # Total of everything
            resultsDF[nCategoriesA+1,nCategoriesB+2] = sum(resultsDF[,nCategoriesB+2])
        }
        
        # There is one last marginal that needs to be coerced to ""
        # Since the result table has been init to 0, the result that is
        # under the list of A modalities is a 0, which looks weird
        resultsDF[nCategoriesA+1,1] = ""
             
    }
    
    # Return the resulting DF
    return(resultsDF)
    
}

# Given a numerical column of a table, find a bunch of descriptive centrality
# measures and how many NAs and NULLs we have; and return it as a dataframe.
# The numbers are not rounded. (if you want round them yourself).
#
# Example of the returning dataframe:
#
# ---------------------
#             |       |
# ---------------------
# Total Rows  | 392   |
# Mean        | 3.412 |
# SD          | 1.32  |
# Min         | 0.21  |
# 1stQ        | 1.01  |
# Median      | 3.67  |
# 3rdQ        | 4.32  |
# Max         | 5.32  |
# Total NAs   | 10    |
# Total NULLs | 7     |
#
# If you try to run this on a non-numerical column will return a dataframe
# With everything initialize to NA and will print/log a warning.
summarizeNumerical <- function(tableBase, variableIndex){
  
  myTableName = deparse(substitute(tableBase))
  
  # The summary we are going to return
  summaryDF  =  data.frame(matrix(NA, nrow = 10, ncol = 2))
  colnames(summaryDF) = c(" ", " ")
  rowNames   = c("Total Rows", "Mean", "SD", "Min", "1stQ", "Median", "3rdQ",
                 "Max", "Total NAs", "Total NULLs")
  
  # Check if we actually have numerical
  countingVariableType = class(tableBase[,variableIndex])
  # -- If NOT numerical, error
  if(countingVariableType == "character" || countingVariableType == "factor"){
    
    print("WARNING!!")
    print("Can't do numerical summary of a categorical variable")
    print(myTableName)
    print("With index:")
    print(variableIndex)
    print("I found a categorical variable and returned a NA dataframe")
    
  }
  # -- If numerical, do stuff
  else{
    
    # Get the quantiles
    summaryData = summary(tableBase[,variableIndex])
    
    totalRows       = nrow(tableBase)
    
    totalData = totalRows
    myMean    = mean(tableBase[,variableIndex], na.rm=TRUE)
    mySD      = sd(tableBase[,variableIndex],   na.rm=TRUE)
    myMin     = summaryData[[1]]
    my1stQ    = summaryData[[2]]
    myMedian  = summaryData[[3]]
    my3stQ    = summaryData[[5]]
    myMax     = summaryData[[6]]
    totalNA   = sum(is.na(tableBase[,variableIndex]))
    totalNULL = sum(is.null(tableBase[,variableIndex]))
    
    
    # Add the concepts to the summary DF
    summaryDF[,1] = rowNames
    
    # Add each value to the proper concept.
    summaryDF[1,2]  = totalData
    summaryDF[2,2]  = myMean    
    summaryDF[3,2]  = mySD      
    summaryDF[4,2]  = myMin     
    summaryDF[5,2]  = my1stQ    
    summaryDF[6,2]  = myMedian  
    summaryDF[7,2]  = my3stQ    
    summaryDF[8,2]  = myMax     
    summaryDF[9,2]  = totalNA   
    summaryDF[10,2] = totalNULL
    
    
  }
  
  
  
  return(summaryDF)
  
}


# Get how many unknows or NAs are in a column of a given table
getTotalUnknowns <- function(tableBase, columnIndex){
    
    return(nrow(tableBase[ (tableBase[,columnIndex] == "Unknown" | is.na(tableBase[,columnIndex])), ]))
    
}

# For a given table, and a column index tells you which are the modalities in
# level order if possible.
getModalities <- function(tableBase, columnIndex, skipUnknowns = FALSE){
    
    currentModalities = levels(tableBase[,columnIndex])
    if(skipUnknowns == TRUE){
            currentModalities = currentModalities[ !(currentModalities == "Unknown" | is.na(currentModalities)) ]
    }
    
    return(currentModalities)

}

getModalitiesV2 <- function(tableBase, columnIndex, skipUnknowns){
    
    currentModalities = levels(tableBase[,columnIndex])
    totalSkips        = length(skipUnknowns)
    
    # If you have 1 or more skips
    if(totalSkips > 0){
        
        # For each skip
        for (j in 1:totalSkips){
                    
            currentSkip = skipUnknowns[j]
                    
            # Check if you have an NA or a string, because R is the stupidiest language and you need to micromanage each case accordingly :(
            if(is.na(currentSkip)){
                            
                # Delete the NAs for the list of modalities
                currentModalities = currentModalities[ !is.na(currentModalities) ]
                        
            }
            else{

                # Delete whatever you have from the list of modalities
                currentModalities = currentModalities[ !(currentModalities == currentSkip ) ]
                        
            }
                    
        }
        
    }
    
    return(currentModalities)

}
   
# For a given table, and a given collection of independent variables, tells you
# how many modalities are in each variable.
getTotalModalities <- function(tableBase, explanatoryIndexesList, skipUnknowns = FALSE){
    
        # Init some variables
        totalExplanatoryIndexes = length(explanatoryIndexesList)
        modalitiesList          = rep(0,totalExplanatoryIndexes)
        
        # Count how many modalities you have in the explanatory list
        for(i in 1:totalExplanatoryIndexes){

            currentExplanatoryIndex = explanatoryIndexesList[i]
            currentModalities       = as.character(unique(tableBase[,currentExplanatoryIndex]))
        
            if(skipUnknowns == TRUE){
            
                currentModalities = currentModalities[ !(currentModalities == "Unknown" | is.na(currentModalities)) ]
            
            }

            modalitiesList[i] = length(currentModalities)
            
        }
        
        return(modalitiesList)
        
}

# For a given table, and a given collection of independent variables, tells you
# how many modalities are in each variable.
#
# tableBase, the data you want
#
# explanatoryIndexesList, the indexes with the columns that you want to analyse
#
# skipUnknown is the list of strings that you want to skip, can include NAs and NULLs
getTotalModalitiesV2 <- function(tableBase, explanatoryIndexesList, skipUnknowns){
    
        # Init some variables
        totalExplanatoryIndexes = length(explanatoryIndexesList)
        modalitiesList          = rep(0,totalExplanatoryIndexes)
        
        
        for(i in 1:totalExplanatoryIndexes){
            
            currentModalities = getModalitiesV2(tableBase, explanatoryIndexesList[i], skipUnknowns)
            modalitiesList[i] = length(currentModalities)
            
        }
        
        return(modalitiesList)
        
}



# For a given table, and a given collection of independent variables and
# dependent variables, this function returns an empty DF that is ready to be
# filled with the following format:
#
#                 | target variable 1 | ... | target variable N |
# --------------------------------------------------------------
# Variable 1     |                   | ... |                    |
# Variable 2     |                   | ... |                    |
#                                      ...
# Variable M     |                   | ... |                    |
readyDFVariables <- function(tableBase, explanatoryIndexesList, targetIndexesList){
    
    # Init some variables
    totalExplanatoryIndexes = length(explanatoryIndexesList)
    totalTargetIndexes      = length(targetIndexesList)
    explanatoryNames        = colnames(tableBase)[explanatoryIndexesList]
    targetNames             = colnames(tableBase)[targetIndexesList]

    # Init the DF and add the information to it
    summaryTable            = DF(totalExplanatoryIndexes, totalTargetIndexes + 1)
    colnames(summaryTable)  = c("Variable", targetNames)
    
    for (i in 1:totalExplanatoryIndexes) {
    
        summaryTable[i,1] = explanatoryNames[i]
            
    }
    
    return(summaryTable)
    
}

# For a given table, and a given collection of independent variables, tells you
# how many modalities are in each variable.
#
#                 | target variable 1 | ... | target variable N |
# --------------------------------------------------------------
# Variable 1     |                   | ... |                    |
# V1 Modality A  |                   | ... |                    |
# V1 Modality B  |                   | ... |                    |
# V1 Modality C  |                   | ... |                    |
#                                      ...
# Variable 2     |                   | ... |                    |
# V2 Modality A  |                   | ... |                    |
# V2 Modality B  |                   | ... |                    |
#                                      ...
#                                      ...
# Variable M     |                   | ... |                    |
# VM Modality A  |                   | ... |                    |
# VM Modality B  |                   | ... |                    |
#                                      ...
#
# All variables needs to be categorical type variables. If you introduce a non
# categorical variable, the data will be coerce to categorical and present the
# categorical version of it. (ie: 1 2 3 to "1" "2" "3").
#
# The function respect the level order of each variable if present, otherwise
# it will sort it by alphabetic order.
#
# tableBase                 Which table do you want to use
#
# explanatory indexes list  Which columns are you going to use as independent
#                           Variables 1 to M
#
# target indexes list       Which columns are you going to use as dependent
#                           Variables 1 to N
#
# skipUnknowns              (bool) Tells if you should include in the modalities
#                           NAs and "Unknown" categories.
#
# The function return:
#
#     A ready empty DF that you can use to fill later
# 
readyDFModalities <- function(tableBase, explanatoryIndexesList, targetIndexesList, skipUnknowns = FALSE){
    
    # Init some variables
    totalExplanatoryIndexes   = length(explanatoryIndexesList)
    totalTargetIndexes        = length(targetIndexesList)
    explanatoryVariablesNames = colnames(tableBase)[explanatoryIndexesList]
    targetVariablesNames      = colnames(tableBase)[targetIndexesList]
    
    # Count how many modalities you have in the explanatory list
    totalModalities = sum(getTotalModalities(tableBase, explanatoryIndexesList, skipUnknowns=skipUnknowns))
    
    # Create the dataframe that needs to be initialized
    summaryTable            = DF(totalModalities + totalExplanatoryIndexes, totalTargetIndexes + 1)
    colnames(summaryTable)  = c(" ", targetVariablesNames)
 
    # Initialize the dataframe
    currentIndex    = 1
    for(i in 1:totalExplanatoryIndexes){

        # Write the variable name into the DF
        currentExplanatoryIndex      = explanatoryIndexesList[i]
        currentVariableName          = explanatoryVariablesNames[i]
        summaryTable[currentIndex,1] = currentVariableName
        currentIndex                 = currentIndex + 1
        
        # For each of the modalities of the current variable
        # I HATE RRRR!! changing tableBase for something else doesn't even
        # give you a warning that the variable doesn't exist in this function context AAAAAG!
        currentModalities       = levels(tableBase[,currentExplanatoryIndex])
        if(skipUnknowns == TRUE){
            currentModalities = currentModalities[ !(currentModalities == "Unknown" | is.na(currentModalities)) ]
        }
        currentTotalModalities = length(currentModalities)
        
        for(j in 1:currentTotalModalities){
        
            # Write the modality name in the DF
            currentModalityName          = currentModalities[j]
            summaryTable[currentIndex,1] = currentModalityName
            currentIndex                 = currentIndex + 1

        }
        
    }
    
    return(summaryTable)
       
}




# This function return an empty dataframe ready to be filled with this form:

# ----------------------------------------------------------------------------------------------------
#                 |      target variable 1           | ... |       target variable N         |
# ----------------------------------------------------------------------------------------------------
#                 | modality11 | ... | modality1M    | ... | modalityN1 | ... | modalityNM   |
# ----------------------------------------------------------------------------------------------------
#      V1         |        xi2 V1 vs variable 1      | ... |        xi2 V1 vs variable N     |
# ----------------------------------------------------------------------------------------------------
#  Modality A     |            | ... |               | ... |            | ... |              | total
#  Modality B     |            | ... |               | ... |            | ... |              | total
#  Modality C     |            | ... |               | ... |            | ... |              | total
# ----------------------------------------------------------------------------------------------------
#       V2        |        xi2 V2 vs variable 1      | ... |        xi2 V2 vs variable N     |
# ----------------------------------------------------------------------------------------------------
#  Modality A     |            | ... |               | ... |            | ... |              | total
#  Modality B     |            | ... |               | ... |            | ... |              | total
#                                                      ...
# ----------------------------------------------------------------------------------------------------
#       VM        |        xi2 VM vs variable 1      | ... |        xi2 VM vs variable N     |
# ----------------------------------------------------------------------------------------------------
#  Modality A     |            | ... |               | ... |            | ... |              | total
#  Modality B     |            | ... |               | ... |            | ... |              | total
# ----------------------------------------------------------------------------------------------------
#                 | total      | ... | total         | ... | total      | ... | total        | total
# ----------------------------------------------------------------------------------------------------
#
# The input variables are as follow:
#
# tableBase                 Which table do you want to use
#
# explanatory indexes list  Which columns are you going to use as independent
#                           Variables 1 to M
#
# target indexes list       Which columns are you going to use as dependent
#                           Variables 1 to N
#
# skipUnknowns              list of strings that you want to skip, tipically
#                           NAs, "Unknown", "Didn't answer", categories.
#
# fillWith                  (string)
#                           "none"                  don't fill with anything
#                           "absolute"              numbers for each combination
#                           "frequency"             frecuency for each combination
#                           "percentage" (default)  same as frequency, but transform to % levels
readyXiSummary <- function(tableBase, explanatoryIndexesList, targetIndexesList, skipUnknowns = NULL, fillWith = "none"){
    
    # Init some variables
    totalExplanatoryIndexes   = length(explanatoryIndexesList)
    totalTargetIndexes        = length(targetIndexesList)
    explanatoryVariablesNames = colnames(tableBase)[explanatoryIndexesList]
    targetVariablesNames      = colnames(tableBase)[targetIndexesList]
    
    # Count how many modalities you have
    modalitiesPerExplanatory   = getTotalModalitiesV2(tableBase, explanatoryIndexesList, skipUnknowns=skipUnknowns)
    totalExplanatoryModalities = sum(modalitiesPerExplanatory)
    modalitiesPerTarget        = getTotalModalitiesV2(tableBase, targetIndexesList,      skipUnknowns=skipUnknowns)
    totalTargetModalities      = sum(modalitiesPerTarget)

    # Create the dataframe that needs to be initialized
    totalRows       = totalExplanatoryModalities + totalExplanatoryIndexes + 3
    totalColumns    = totalTargetModalities + 2
    summaryTable    = DF(totalRows, totalColumns)
    xiTable         = DF(totalExplanatoryIndexes, totalTargetIndexes+1)
    
    # Initialize the dataframe with the names of things
    {
        
        # All column are init to nothing
        # fuck your stupid dataframe structures
        colnames(summaryTable) = rep("", totalColumns)
        
        # Fill the metasummary table name 
        colnames(xiTable) = c("",targetVariablesNames)
        
        # ---- Mark where we need the columns names
        columnVNamesIndexes    = modalitiesPerTarget
        columnVNamesIndexes[1] = 2
        # Check if we have more than one target, because R is a stupid language and won't skip the for otherwise. I hate you.
        if(totalTargetIndexes>1){
            for(i in 2:totalTargetIndexes){
    
                columnVNamesIndexes[i] = columnVNamesIndexes[i-1] + modalitiesPerTarget[i-1]
        
            }    
        }
        
        
        # ---- The first and last cells don't have any name
        summaryTable[1,1]            = ""
        summaryTable[1,totalColumns] = ""
        
        # ---- Also don't have any modality name
        summaryTable[2,1]            = ""
        summaryTable[2,totalColumns] = ""
        
        currentTarget                = 1
        currentModality              = 1
        
        currentTotalModalities  = modalitiesPerTarget[currentTarget]
        currentTargetModalities = getModalitiesV2(tableBase, targetIndexesList[currentTarget], skipUnknowns)
        # ---- The rest, we see
        for(i in 2:(totalColumns-1)){
    
            # If i belong to one of the market column, put the name there
            if(i %in% columnVNamesIndexes){
                
                summaryTable[1,i] = targetVariablesNames[currentTarget]
                
                
            }
            # Otherwise, write nothing
            else{
                
                summaryTable[1,i] = ""
                
            }
            
            # In any case, write the modality in the following cell
            summaryTable[2,i] = currentTargetModalities[currentModality]
            currentModality   = currentModality + 1
            if(currentModality > currentTotalModalities){
                currentModality = 1
                currentTarget = currentTarget + 1        
                
                if(currentTarget < totalTargetIndexes){
                    currentTotalModalities  = modalitiesPerTarget[currentTarget]
                    currentTargetModalities = getModalitiesV2(tableBase, targetIndexesList[currentTarget], skipUnknowns)                                    
                }

                
            }

        
        }
     
        
        # Now we have the columns, lets go for the rows
        {
            
            currentExplanatory           = 1
            currentModality              = 1
        
            currentTotalModalities       = modalitiesPerExplanatory[currentExplanatory]
            currentExplanatoryModalities = getModalitiesV2(tableBase, explanatoryIndexesList[currentExplanatory], skipUnknowns)            
            currentExplanatoryName       = explanatoryVariablesNames[currentExplanatory]
            
            fillingName                  = TRUE

            # The first and second row are filled now
            # The last row is empty
            for(i in 3:(totalRows-1)){
                
                # If we are in a name part
                if(fillingName == TRUE){
                
                    # Fill the variable name
                    summaryTable[i,1] = currentExplanatoryName
                    summaryTable[i,2] = "Xi test"
                    
                    # Fill the rest of the row as empty
                    for(k in 3:totalColumns){
                        summaryTable[i,k] = ""
                    }
                    
                    
                    fillingName = FALSE
                }
                # If filling modalities
                else{
                    
                    summaryTable[i,1] = currentExplanatoryModalities[currentModality]
                    currentModality   = currentModality + 1
                    
                    if(currentModality > currentTotalModalities){
                        
                        fillingName = TRUE
                        
                        currentModality    = 1
                        currentExplanatory = currentExplanatory + 1                
                        
                        if(currentExplanatory <= totalExplanatoryIndexes){
                            currentTotalModalities       = modalitiesPerExplanatory[currentExplanatory]
                            currentExplanatoryModalities = getModalitiesV2(tableBase, explanatoryIndexesList[currentExplanatory], skipUnknowns)            
                            currentExplanatoryName       = explanatoryVariablesNames[currentExplanatory]                                                    
                        }
    
                    }                    
                    
                }
                
                
                
            }
        }   
        
        # The metasummary is eaiser
        for(i in 1:totalExplanatoryIndexes){
            # Enter the name of the row
            xiTable[i,1] = explanatoryVariablesNames[i]
        }

        
        # The last row doesn't have a name either
        summaryTable[totalRows,1] = ""
        
    }

    # Fill the dataframe with numbers
    {
     
        # For each of the explanatory variables
        {
            
            # Prepare the explanatory variables
            currentExplanatory           = 1
            currentExplanatoryModality   = 1
        
            currentExplanatoryIndex           = explanatoryIndexesList[currentExplanatory]
            currentTotalExplanatoryModalities = modalitiesPerExplanatory[currentExplanatory]
            currentExplanatoryModalities      = getModalitiesV2(tableBase, currentExplanatoryIndex, skipUnknowns)            
            currentExplanatoryName            = explanatoryVariablesNames[currentExplanatory]
            
            fillingName                       = TRUE

            # The first and second row are filled now
            # The last row is empty
            for(i in 3:(totalRows-1)){
                
                # If we are in a name part
                if(fillingName == TRUE){
                
                    # Fill the variable name
                    summaryTable[i,2] = "Xi test pending"

                    fillingName = FALSE
                }
                # If filling modalities
                else{
                    
                    # Get which modality we are using
                    currentExplanatoryModalityName = currentExplanatoryModalities[currentExplanatoryModality]
                    currentExplanatoryModality     = currentExplanatoryModality + 1
                    
                    # Count how many of these we have, and put it on the marginals
                    summaryTable[i,totalColumns] = countCategories(tableBase, currentExplanatoryIndex, currentExplanatoryModalityName)
                    
                    # For each target, count the marginals and put it on the table
                    {

                        currentTarget                = 1
                        currentTargetModality        = 1
        
                        currentTargetIndex           = targetIndexesList[currentTarget]
                        currentTargetTotalModalities = modalitiesPerTarget[currentTarget]
                        currentTargetModalities      = getModalitiesV2(tableBase, currentTargetIndex, skipUnknowns)
        
                        for(k in 2:(totalColumns-1)){
    
                            # Get the target modality
                            currentTargetModalityName = currentTargetModalities[currentTargetModality]
                            
                            # Count the marginals for that combination
                            # R is beyond horrible, variable names need to keep Name/Index because we don't have string/int information anymore
                            summaryTable[i,k]      = countMarginals(tableBase, currentExplanatoryIndex, currentExplanatoryModalityName, currentTargetIndex, currentTargetModalityName)
                            
                            # Pass to the next modality
                            currentTargetModality  = currentTargetModality + 1
                        
                            # If we pass modalities, get the next target
                            if(currentTargetModality > currentTargetTotalModalities){
                                currentTargetModality = 1
                                currentTarget = currentTarget + 1        
                                
                                # If we are still within range of targets, get the next target
                                if(currentTarget < totalTargetIndexes){
                                    currentTargetIndex           = targetIndexesList[currentTarget]
                                    currentTargetTotalModalities = modalitiesPerTarget[currentTarget]
                                    currentTargetModalities      = getModalitiesV2(tableBase, currentTargetIndex, skipUnknowns)
                                }                                
                                
                            }

                        } 

                    }

                    # If we overload, get the next modality of explanatories
                    if(currentExplanatoryModality > currentTotalExplanatoryModalities){
                        
                        fillingName = TRUE
                        
                        currentExplanatoryModality = 1
                        currentExplanatory         = currentExplanatory + 1                
                        
                        # If we are still within explanatory range, go on
                        if(currentExplanatory <= totalExplanatoryIndexes){

                            currentExplanatoryIndex           = explanatoryIndexesList[currentExplanatory]
                            currentTotalExplanatoryModalities = modalitiesPerExplanatory[currentExplanatory]
                            currentExplanatoryModalities      = getModalitiesV2(tableBase, currentExplanatoryIndex, skipUnknowns)            
                            currentExplanatoryName            = explanatoryVariablesNames[currentExplanatory]                            
                            
                            
                        }
    
                    }                    
                    
                }
                
            }
        }   
        
        
           
    }
    
    # Fill the dataframe with the xi2 tests
    {

        # Special indexes
        currentRowIndex    = 4
        currentColumnIndex = 2
        
        # Init the explanatory indexes
        currentExplanatory           = 1
        currentExplanatoryModality   = 1
        
        # For each explanatory variable
        for(i in 1:totalExplanatoryIndexes){

            # Get the explanatory info
            currentExplanatoryIndex           = explanatoryIndexesList[currentExplanatory]
            currentTotalExplanatoryModalities = modalitiesPerExplanatory[currentExplanatory]
            currentExplanatoryModalities      = getModalitiesV2(tableBase, currentExplanatoryIndex, skipUnknowns)            
            currentExplanatoryName            = explanatoryVariablesNames[currentExplanatory]

            # Init the target indexes
            currentTarget                = 1
            currentTargetModality        = 1            
            
            # For each target variable
            for(j in 1:totalTargetIndexes){
            
                    currentTargetIndex           = targetIndexesList[currentTarget]
                    currentTargetTotalModalities = modalitiesPerTarget[currentTarget]
                    currentTargetModalities      = getModalitiesV2(tableBase, currentTargetIndex, skipUnknowns)
                
                    # First we need to find the numbers in the big table
                    # -- Create the little table
                    currentDF = DF(currentTotalExplanatoryModalities, currentTargetTotalModalities)
                    # -- Fill it with numbers
                    firstCoordinateRow    = i + currentRowIndex - 1
                    firstCoordinateColumn = j + currentColumnIndex - 1
                    
                    for(k in 1:currentTotalExplanatoryModalities){
                        for(l in 1:currentTargetTotalModalities){
                            
                            # R, Really, I fucking shit inside the throat of the asshole that invent you
                            # datatypes get transformed around randomly just because. If you allow for multiple datatype, then DONT FUCKING CONVERTED BACK TO STRING YOU FUCKING PIECE OF SHIT!!!!
                            # I hate everything that has to do with this STUPID LANGUAGE
                            # I hope I never got to see you again when I finish the PhD
                            
                            currentDF[k,l] = as.integer(summaryTable[ (firstCoordinateRow + k - 1) , (firstCoordinateColumn + l - 1)])
                        }
                    }
                        
                    # print(currentDF)
                    
                    # Do the xi2 test
                    xiTotalResults = chisq.test(currentDF)
                    
                    #print(xiTotalResults)

                    pValue         = xiTotalResults$p.value
                    pValueRounded  = round(pValue,4)
                    if(pValueRounded < 0.0001) pValueRounded = 0.0001
                    
                    #print(pValue)
                    
                    # Save it in places
                    xiTable[i,j+1]   = pValueRounded
                    summaryTable[firstCoordinateRow-1, firstCoordinateColumn] = pValueRounded
                    
                    # Update metaindexes
                    currentColumnIndex = currentColumnIndex + currentTargetTotalModalities
                
                    # Pass to the next modality
                    currentTargetModality  = currentTargetModality + 1
                        
                    # If we pass modalities, get the next target
                    if(currentTargetModality > currentTargetTotalModalities){
                        currentTargetModality = 1
                        currentTarget = currentTarget + 1        
                                
                        # If we are still within range of targets, get the next target
                        if(currentTarget < totalTargetIndexes){
                            currentTargetIndex           = targetIndexesList[currentTarget]
                            currentTargetTotalModalities = modalitiesPerTarget[currentTarget]
                            currentTargetModalities      = getModalitiesV2(tableBase, currentTargetIndex, skipUnknowns)
                        }                                
                                
                    }                    
                    
                    
            }
            
            # Now we are finish with the targets
            # Update the metaindexes
            currentColumnIndex = 2
            currentRowIndex    = currentRowIndex + currentTotalExplanatoryModalities
            

            currentExplanatory         = currentExplanatory + 1                
                        
            # If we are still within explanatory range, go on
            if(currentExplanatory <= totalExplanatoryIndexes){

                currentExplanatoryIndex           = explanatoryIndexesList[currentExplanatory]
                currentTotalExplanatoryModalities = modalitiesPerExplanatory[currentExplanatory]
                currentExplanatoryModalities      = getModalitiesV2(tableBase, currentExplanatoryIndex, skipUnknowns)            
                currentExplanatoryName            = explanatoryVariablesNames[currentExplanatory]                            
                            
            }

        }

    }
    
    
    # Return
    myReturn = vector("list", length = 2)
    myReturn[[1]] = xiTable
    myReturn[[2]] = summaryTable

    return (myReturn)

}






# This function return an empty dataframe ready to be filled
# Same as previous function but in melted form
#
# Variable Name | Modality Name | Variable Index | target variable 1 | ... | target variable N |
# --------------------------------------------------------------
#     V1        |  Modality A  |  1   | ... |                    |
#     V1        |  Modality B  |  1   | ... |                    |
#     V1        |  Modality C  |  1   | ... |                    |
#                                       ...
#     V2        |  Modality A  |  2   | ... |                    |
#     V2        |  Modality B  |  2   | ... |                    |
#                                       ...
#                                       ...
#     VM        |  Modality A  |  M   | ... |                    |
#     VM        |  Modality B  |  M   | ... |                    |
#readyDFMeltedModalities <- function(tableBase, explanatoryIndexesList, targetIndexesList, skipUnknowns = FALSE)

# For a given table, and a given collection of independent variables and
# dependent variables, this function returns the prevalence statistics for the
# given target variables.
#
# The return is a DF such as this:
#
# tableName       | target variable 1  |  ... | target variable N   |
#                 | T11 | ... | T1x |u1|  ... | TN1 | ... | TNz |uN|
# --------------------------------------------------------------
# Variable 1     |                     | ... |                    |
# V1 Modality A  |                     | ... |                    |
# V1 Modality B  |                     | ... |                    |
# V1 Modality C  |                     | ... |                    |
#                                        ...
# Variable 2     |                     | ... |                    |
# V2 Modality A  |                     | ... |                    |
# V2 Modality B  |                     | ... |                    |
#                                        ...
#                                        ...
# Variable M     |                     | ... |                    |
# VM Modality A  |                     | ... |                    |
# VM Modality B  |                     | ... |                    |
#
#
# Where:
#
#     Txx are the target variables modalities
#     ux are the prevalence avarage for the whole population
#
#     Each variable 1 to M has it own list of modalities
#
# The function respect the level order of each variable if present, otherwise
# it will sort it by alphabetic order.
#
# tableBase                 Which table do you want to use
#
# explanatory indexes list  Which columns are you going to use as independent
#                           Variables 1 to M
#
# target indexes list       Which columns are you going to use as dependent
#                           Variables 1 to N
#
# prevalenceTarget          (string) Which is considered to be the positive rate
#                           to compare against prevalence.
#
# skipUnknowns              (bool) Tells if you should include in the modalities
#                           NAs and "Unknown" categories.
#
# computeUnknowns           (bool) Tells if the unknowns modalities should count
#                           for the total in prevalence. (ie: Man Woman Unknown,
#                           has 33% men with compute TRUE, and 50% men with 
#                           compute FALSE)
#
#                           TODO: Actually useless right?
#
# showInPercentage          (bool) if TRUE, it will show 13% instead of 0.13
#
# roundMe                   (int) How many decimal points should be use for
#                           rounding the result
#
# The function return:
#
#     A DF with the described format.
#
#
summarizePrevalences <- function(tableBase, explanatoryIndexesList,
                                 targetIndexesList, prevalenceTarget = "Positive",
                                 skipUnknowns = FALSE, computeUnknowns = TRUE,
                                 showInPercentage = FALSE, roundMe = 2){
    
    # Init some variables
    totalExplanatoryIndexes   = length(explanatoryIndexesList)
    totalTargetIndexes        = length(targetIndexesList)
    explanatoryVariablesNames = colnames(tableBase)[explanatoryIndexesList]
    targetVariablesNames      = colnames(tableBase)[targetIndexesList]

    # Get the base dataframe
    # We need to create a new header, but we will keep the first row with all the variables
    baseDF = readyDFModalities(tableBase, explanatoryIndexesList, targetIndexesList, skipUnknowns = skipUnknowns)

    # Create the new empty dataframe with the prevalence header
    targetModalities      = getModalities(tableBase, targetIndexesList)
    totalTargetModalities = length(targetModalities)
    prevalenceDF = DF( (nrow(baseDF) + 2), (totalTargetModalities + totalTargetIndexes + 1) )
    # -- Init the rows
    for(i in 1:nrow(baseDF)){
        
        prevalenceDF[i+2,1] = baseDF[i,1]
        
    }

    # -- Init the columns
    
    prevalenceDF[1,4] = "Prevalence"
    currentIndex = 2
    for(i in 1:totalTargetIndexes){

        # Write the name of the variable in the first row
        prevalenceDF[1,currentIndex] = targetVariablesNames[i]

        # Write each of the modalities
        currentTargetIndex  = targetIndexesList[i]
        currentModalities   = levels(tableBase[,currentTargetIndex])
        if(skipUnknowns == TRUE){
            currentModalities = currentModalities[ !(currentModalities == "Unknown" | is.na(currentModalities)) ]
        }
        currentTotalModalities = length(currentModalities)
        for(j in 1:currentTotalModalities){
        
            prevalenceDF[2,currentIndex] = currentModalities[j]
                
            currentIndex = currentIndex + 1
        }
            
        # Write the population prevalence
        # -- Find the population prevalence first obviously
        currentPrevalence = sum(tableBase[,currentTargetIndex] == prevalenceTarget , na.rm=TRUE) / nrow(tableBase)
        if(showInPercentage == TRUE){
            currentPrevalence = currentPrevalence * 100
            currentPrevalence = round(currentPrevalence, roundMe)
            currentPrevalence = paste0(currentPrevalence, "%")
        }
        else{
            currentPrevalence = round(currentPrevalence, roundMe)    
        }
            
        prevalenceDF[2,currentIndex] = currentPrevalence
        currentIndex = currentIndex + 1
        
    }
    
    # Here we have finally initialize the rows and column names. Now we need to
    # fill each cell
    currentRowIndex    = 3 # First two rows are the header

    # For each explanatory variable
    for(i in 1:totalExplanatoryIndexes){
        
        # Skip one line, since the first line is for the variable name
        currentRowIndex = currentRowIndex + 1
        
        # Get the index
        currentExplanatoryIndex = explanatoryIndexesList[i] 
        
        # Get the modalities for this variable
        currentExplanatoryModalities = getModalities(tableBase, currentExplanatoryIndex, skipUnknowns = skipUnknowns)
        currentTotalExplanatoryModalities = length(currentExplanatoryModalities)
    
        # Reset the index for the columns
        currentColumnIndex = 2 # First column are the variable names
            
        # For each target variable
        for(j in 1:totalTargetIndexes){
            
            # We don't skip the first column here, but we need to do something especial in the last one
            
            # Get the index
            currentTargetIndex = targetIndexesList[j] 
        
            # Get the modalities for this variable
            currentTargetModalities      = getModalities(tableBase, currentTargetIndex, skipUnknowns = skipUnknowns)
            currentTotalTargetModalities = length(currentTargetModalities)
            
            # Now we have the modalities for both explanatory and the target, so we fill the subtable
            for(x in 1:currentTotalExplanatoryModalities){
                for(y in 1:currentTotalTargetModalities){
                
                    # Get the modalities
                    # (R is horrible, no warning for unused variable)
                    currentExplanatoryModality = currentExplanatoryModalities[x]
                    currentTargetModality      = currentTargetModalities[y]
                    
                    # Count how many of these are in the subtable
                    currentSubtable  = tableBase[ (tableBase[, currentExplanatoryIndex] == currentExplanatoryModality & tableBase[, currentTargetIndex] == currentTargetModality), ]
                    totalCurrentRows = nrow(currentSubtable)
                    
                    # Write the result into the summary table
                    prevalenceDF[currentRowIndex + x - 1, currentColumnIndex + y - 1] = totalCurrentRows

                }
                
            }
            
            # The subtable is filled, move the column index to the end of whatever number of modalities we had
            currentColumnIndex = currentColumnIndex + currentTotalTargetModalities

            # Now for each of the modalities of the explanatory variables, we need to find the prevalence
            for(x in 1:currentTotalExplanatoryModalities){

                    # Get the current Modality
                    currentExplanatoryModality = currentExplanatoryModalities[x]
                
                    # Find the prevalence
                    currentSubtable    = tableBase[ (tableBase[, currentExplanatoryIndex] == currentExplanatoryModality & tableBase[, currentTargetIndex] == prevalenceTarget), ]
                    currentPrevalence  = nrow(currentSubtable)
                    currentDenominator = nrow(tableBase[ (tableBase[, currentExplanatoryIndex] == currentExplanatoryModality),])
                    
                    # Compute unknows is actually useless?
                    # if(computeUnknowns == FALSE) currentDenominator = currentDenominator - getTotalUnknowns(tableBase, currentExplanatoryIndex)
                    
                    currentPrevalence  = currentPrevalence / currentDenominator
                    
                    if(showInPercentage == TRUE){
                        currentPrevalence = currentPrevalence * 100
                        currentPrevalence = round(currentPrevalence, roundMe)
                        currentPrevalence = paste0(currentPrevalence, "%")
                    }
                    else{
                        currentPrevalence = round(currentPrevalence, roundMe)    
                    }

                    prevalenceDF[ (currentRowIndex + x -1) , currentColumnIndex] = currentPrevalence
            }
            
            # We are finish with the prevalence column, move away to the next
            currentColumnIndex = currentColumnIndex + 1
            
        }
        
        # Update the index and move to the next subtable
        currentRowIndex = currentRowIndex + currentTotalExplanatoryModalities
        
    }
    
    return(prevalenceDF)
    
}


# Get the average conditional to whatever categories you have into a given
# variable.The results are:

#
# The return is a DF such as this:
#
# tableName       | target variable 1  |  ... | target variable N   |
#                 | C1 | ...      | CX |  ... | C1 | ...       | CX |
#                 | AvT1C1 | ...|AvT1CX|  ... | AvTNC1 | ...|AvTNCX|
# --------------------------------------------------------------
# Variable 1     |                     | ... |                    |
# V1 Modality A  |                     | ... |                    |
# V1 Modality B  |                     | ... |                    |
# V1 Modality C  |                     | ... |                    |
#                                        ...
# Variable 2     |                     | ... |                    |
# V2 Modality A  |                     | ... |                    |
# V2 Modality B  |                     | ... |                    |
#                                        ...
#                                        ...
# Variable M     |                     | ... |                    |
# VM Modality A  |                     | ... |                    |
# VM Modality B  |                     | ... |                    |

# Same as before, but gives you a bunch of averages. You don't need to specify
# the prevalence target here, it will take the general population average
summarizeAverages <- function(tableBase, explanatoryIndexesList,
                                 targetIndexesList, conditionalIndex,
                                 skipUnknowns = FALSE, computeUnknowns = TRUE,
                                 roundMe = 2){
    
    # Init some variables
    totalExplanatoryIndexes   = length(explanatoryIndexesList)
    totalTargetIndexes        = length(targetIndexesList)
    explanatoryVariablesNames = colnames(tableBase)[explanatoryIndexesList]
    targetVariablesNames      = colnames(tableBase)[targetIndexesList]
    
    # Get the base dataframe
    # We need to create a new header, but we will keep the first row with all the variables
    baseDF = readyDFModalities(tableBase, explanatoryIndexesList, targetIndexesList, skipUnknowns = skipUnknowns)
    
    # Create the new empty dataframe with the prevalence header
    conditionalModalities      = getModalities(tableBase, conditionalIndex)
    totalConditionalModalities = length(conditionalModalities)
    
    averagesDF = DF( (nrow(baseDF) + 3), (totalTargetIndexes * totalConditionalModalities + 1))
    
    # -- Init the rows
    for(i in 1:nrow(baseDF)){
        
        averagesDF[i+3,1] = baseDF[i,1]
        
    }
    
    # -- Init the columns
    currentIndex = 2
    for(i in 1:totalTargetIndexes){
        
        currentTargetIndex = targetIndexesList[i]
        
        # Write the name of the variable in the first row
        averagesDF[1,currentIndex] = targetVariablesNames[i]
        
        # Write each of the conditional modalities and find the general
        # average for this variables combination
        for(j in 1:totalConditionalModalities){
            
            currentConditionalName = conditionalModalities[j]
            
            # Find the subtable
            currentSubtable = tableBase[ tableBase[,conditionalIndex] == currentConditionalName, currentTargetIndex ]
            
            currentAverage  = mean(currentSubtable, na.rm = TRUE)
            
            averagesDF[2,currentIndex] = currentConditionalName
            averagesDF[3,currentIndex] = round(currentAverage, roundMe) 
            
            currentIndex = currentIndex + 1
        }
    }
    
    # Here we have finally initialize the rows and column names. Now we need to
    # fill each cell
    currentRowIndex = 4 # First three rows are the header
    
    # For each explanatory variable
    for(i in 1:totalExplanatoryIndexes){
        
        # Skip one line, since the first line is for the variable name
        currentRowIndex = currentRowIndex + 1
        
        # Get the index
        currentExplanatoryIndex = explanatoryIndexesList[i] 
        
        # Get the modalities for this variable
        currentExplanatoryModalities      = getModalities(tableBase, currentExplanatoryIndex, skipUnknowns = skipUnknowns)
        currentTotalExplanatoryModalities = length(currentExplanatoryModalities)
        
        # Reset the index for the columns
        currentColumnIndex = 2 # First column is the variable names and average
        
        # For each target variable
        for(j in 1:totalTargetIndexes){
            
            # We don't skip the first column here, but we need to do something especial in the last one
            
            # Get the index
            currentTargetIndex = targetIndexesList[j] 
            
            # For each modality in the conditional variable
            for(k in 1:totalConditionalModalities){
                
                currentConditionalName = conditionalModalities[k]
                
                for(x in 1:currentTotalExplanatoryModalities){
                    
                    currentExplanatoryModality = currentExplanatoryModalities[x]
                    
                    # Find the average of this subgroup
                    currentSubtable  = tableBase[ (tableBase[, currentExplanatoryIndex] == currentExplanatoryModality & 
                                                   tableBase[, conditionalIndex]        == currentConditionalName), 
                                                   currentTargetIndex]
                    
                    currentAverage   = mean(currentSubtable, na.rm = TRUE)
                    
                    averagesDF[currentRowIndex + x - 1, currentColumnIndex + k - 1] = round(currentAverage, roundMe)
                    
                }
                
                
                
            }
            
            # The subtable is filled, move the column index to the end of whatever number of modalities we had
            currentColumnIndex = currentColumnIndex + totalConditionalModalities
            
        }
        
        # Update the index and move to the next subtable
        currentRowIndex = currentRowIndex + currentTotalExplanatoryModalities
        
    }
    
    return(averagesDF)
    
}


