# -----------------------------------------------------------
#
# This script contain basic tools to manipulate strings,
# find dates, get automatic naming, and things of the like
#
# -----------------------------------------------------------

library(foreign)   # For reading SPSS files
library(lubridate) # For dealing with proper dates

############################
# NUMBERS MANIPULATION
# -- number2binary()     Transform an integer to binary
# -- myIsInteger()       Check if a number is an integer, the R version doesn't 
#                        works properly.
# -- logit2prob()        Transform a logit probability into a normal probability
# -- checkStrictGrow()   Checks if a list of numbers is strictly growing
# -- scaleValues()       Transform all values in [a,b] to values between [a', b']
# -- properPValue()      Transform a pValue number into 0.xxxx if it is > 0.0001 or scientific notation otherwise
############################
{
    # Transform a number into binary
    # -- number is the integer that you want to transform
    # -- noBits is how many bits you want to gets. ie:
    #        14 = 1110
    #        noBits = 7 -> 0001110
    #        noBits = 2 -> 10
    # Positive numbers leads with 0s while negative numbers leads with 1s
    number2binary = function(number, noBits) {
    
        binary_vector = rev(as.numeric(intToBits(number)))
    
        if(missing(noBits)) return(binary_vector)
    
        else binary_vector[-(1:(length(binary_vector) - noBits))]
    
    }
  
    # Check if a number is an integer, since the R version is confusing
    myIsInteger <- function(x){
    
        return (x%%1==0)
    
    }
  
    # Get logit probability into normal probability
    logit2prob <- function(logit){
        odds = exp(logit)
        prob = odds / (1 + odds)
        return(prob)
    }
  
    # Checks if a list of numbers is strictly growing
    checkStrictGrow <- function(myVector){
    
        growing        = TRUE
        currentMaximum = myVector[1]
        vectorSize     = length(myVector)
    
        if(vectorSize > 1){
            for(i in 2:vectorSize){
        
                if(myVector[i] <= currentMaximum) growing = FALSE
        
                currentMaximum = myVector[i]
        
            }    
        }
    
        return (growing)
    
    }
  
  
    # Scale a bunch of values that has this range:
    #  [A , B]
    #
    # to this other range:
    #  [C , D]
    scaleValues <- function(values, newMinimum, newMaximum){
    
        maximumOriginalRange = max(values)
        minimumOriginalRange = min(values)
        deltaRange = maximumOriginalRange - minimumOriginalRange
    
        newValues = ((values - minimumOriginalRange) / deltaRange) *  (newMaximum - newMinimum) + newMinimum
    
        return(newValues)
    
    }
  
    # Get a p-value into a human readable format
    #
    # so if your p-value is:
    #
    #     0.005383822783736746
    #     5.383822783736746e-3
    #
    #     or any other bizarre numeric display that R does randomly
    #
    # transform it into 0.0054
    properPValue <- function(x, roundMe = 4){
      
        returnThis = 0
      
        if(x > 0.0001) returnThis = format(round(x, roundMe), scientific = F )
        else           returnThis = signif(x, digits=3)
      
        return(returnThis)
      
    }
  
}

############################
# DATES MANIPULATION
# isDate()             Tells if a variable is a date
# calc_age()           Find the years in between two dates
# transformToNumeric() For dates given table and index , transform them to numeric
############################
{
  
    # Check if a value is date value
    isDate <- function(x) inherits(x, 'Date')
  
    # Calculate an age in years (decimal) from a given reference date
    #
    # Date birthDate ; the birthday of someone
    # Date refDate   ; the reference date from birthDate. If you give a previous
    #                  date you will get a negative number.
    #                  Default to the time in which you run the function.
    #
    # return         ; a float with how many years have pass from the brithDate to
    #                  the refDate.
    #
    # Example:
    # 
    #    now = as.Date("1970-01-01")
    #    later = as.Date("1971-10-31")
    #    calc_age(now, later)
    #
    #    > 1.833333
    calc_age <- function(birthDate, refDate = Sys.Date()) {
    
        require(lubridate)
    
        period = as.period(interval(birthDate, refDate), unit = "year")
    
        totalAge    = 0
        totalYears  = period$year
        totalMonths = period$month
        totalDays   = period$day
        
        totalMonths = totalMonths + (totalDays/30)
    
        totalAge    = totalYears + (totalMonths/12)
    
        return(totalAge)
    
  }
  
    # Transform a bunch of data that is date into numeric
    # This is useful for regression analysis.
    #
    # If you transform your data starting at 0, or some negative value, this
    # can run linear regression but it will run into troubles when running
    # logarithmic regression for example. So I suggest that you start from
    # another arbitrary number.
    #
    # This transform dates into days passed since 1970
    #
    # deltaMinimum   (bool) FALSE (default) do nothing and keep the 1970 reference
    #                       TRUE             transform it into dates since the minimum date (0)
    #
    # offset         (int)  how many extra time units you want to add to your data.
    #                       If you are going to divide it, or apply logarithms, this ensure
    #                       that you don't end up with an indetermination (1/0 , log(1), ...)
    transformToNumeric <- function(tableBase, columnIndex, deltaMinimum = FALSE, offset = 0){
    
        returnVector = rep(NA, nrow(tableBase))
    
        minimumDate = 0
        if(deltaMinimum == TRUE) minimumDate = min(tableBase[,columnIndex], na.rm = TRUE)
        
        for(i in 1:nrow(tableBase)){
        
            if( !is.na(tableBase[i,columnIndex]) ) returnVector[i] = tableBase[i,columnIndex] - minimumDate + offset
            
        }
             
        return (returnVector)   
        
    }
  
  
}

################################################################################
# STRINGS MANIPULATION
# -- trim()                   Delete start and end spaces in a string
# -- cleanWeirdCharacters()   Delete ":$%'/\,""<>[]() " from a string
# -- writeLinesBN()           For a given file descriptor, makes sure that '/n'
#                             characters actually breaks the line
#                             (which R doesn't do)
# -- writeLinesDisk()         For a given path, and a given string, write it
#                             into disk line by line, and break by '/n'
################################################################################
{
 
    # Delete trailing and leading whitespaces from a string
    #
    # String x ; the string you want to trim
    #
    # return     String without trailing and leading spaces
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  
    # Delete the strange characters from a string and transform the result into a
    # ISO_8859-2 string. White spaces are transformed into "_" characters instead
    #
    # This is intended to use to clean filenames and filepath incompatible 
    # characters.
    #
    # List of characters:
    #
    # : $ % ' / \ , " " (white space)  < > [ ] ( )
    #
    # String weirdString ; The string that you want to clean
    #
    # return             ; a new string with the weirdesness removed
    #
    # Example
    #
    #     cleanWeirdCharacters("This(is),a, very$$ weird$:String")
    #
    #     > "Thisisa_very_weirdString"
    cleanWeirdCharacters <- function(weirdString){
    
        wierdString <- as.character(weirdString)
        weirdString <- gsub(":", "", weirdString)
        weirdString <- gsub("\\$", "", weirdString)
        weirdString <- gsub("%", "", weirdString)
        weirdString <- gsub("'", "", weirdString)
        weirdString <- gsub("/", "", weirdString)
        weirdString <- gsub("\"", "", weirdString)
        weirdString <- gsub(",", "", weirdString)
        weirdString <- gsub(" ", "_", weirdString)
        weirdString <- gsub("<", "", weirdString)
        weirdString <- gsub(">", "", weirdString)
        weirdString <- gsub("[()]", "", weirdString)
    
        weirdString <- gsub("\\[", "", weirdString)
        weirdString <- gsub("\\]", "", weirdString)
    
        iconv(weirdString, to = "ISO_8859-2")
    
        return(weirdString)
    }
  
    # Write lines into file but properly breaking lines for \n characters
    #
    # The file will ALWAYS be overwrite.
    writeLinesBN <- function(myString, myFileConn){
    
        # Separate the string into a list of strings
        allStringsList = strsplit(myString, "\n")[[1]]
    
        # Write each line individually
        writeLines(allStringsList, myFileConn)
        close(myFileConn)
    
    }
  
    # Write lines into file but properly breaking lines for \n characters
    # Is different from the previous function because this one takes the filepath
    # and create, and close, the file connection inside the function
    #
    # The file will ALWAYS be overwrite.
    writeLinesDisk <- function(myString, myFilePath){
    
        # Create the file connection
        myFileConn = file(myFilePath, 'w')
      
        # Separate the string into a list of strings
        allStringsList = strsplit(myString, "\n")[[1]]
    
        # Write each line individually
        writeLines(allStringsList, myFileConn)
        
        # Close the connection
        close(myFileConn)
    
    }
  
  
}

############################
# DATA MANIPULATION
#
# -- getCategoricalVariables() For a given DF, tells you which columns are
#                              categorical
#
# -- getDFFromSPSS()           Return a R DF from a .sav filepath
#
# -- getIndexVector()          For a given vector, tells which column is the
#                              given name.
#
# -- getIndexDF()              For a given DF, tells which column is the given
#                              name.
#
# -- transposeDF()             Transpose a DF properly, and not the weird thing
#                              that stupid R does in the base code
#
# -- getCategories()           Get the categories for a given table and column
#                              This get you everything, whether getModalitiesV2
#                              filters out unwanted categories
#                              This is in the summarizers lib
#
# -- deleteCategory()          Delete a category from a column
#
# -- keepCategory()            Keep a category and delete the rest
#
# -- deleteNA()                Delete all rows containing a NA for a given column
#
# -- greaterThan()             Delete all rows that are greater than a given value
############################
{
    # For a given dataframe, return a boolean vector telling which variables
    # are categoricals and which one arent.
    getCategoricalVariables <- function(variablesDF){
    
        # Get the dimmension
        totalColumns = ncol(variablesDF)
    
        # Get the class of each column
        myClasses = sapply(variablesDF, class)
    
        # Simplify types to boolean
        # - FALSE = Numerical
        # - TRUE  = Categorical
        categoricalColumns = rep(FALSE,totalColumns)
        for(i in 1:totalColumns){
      
            if(myClasses[[i]] == "character" || myClasses[[i]] == "factor") categoricalColumns[i] = TRUE

        }
    
        return(categoricalColumns)
    
    }
  
    # From a file in disk with .sav extension (SPSS) gets a DF with the information
    # that is ready to use in R.
    # -- filepath is where you have the .sav file
    # -- saveCSVCopy is another path where you want to save a CSV copy of this data
    getDFFromSPSS <- function(filepath, saveCSVCopy = NULL){
    
        data = read.spss(filepath, to.data.frame=TRUE)
    
        if(!is.null(saveCSVCopy)) write.csv(data, file = saveCSVCopy, row.names = FALSE)
    
        return (data)
    
    }

    # For a given vector, tells which column is the given name.
    getIndexVector <- function(myName, myVector){
    
        return (grep(paste0("^",myName,"$"), myVector))
        
    }
  
    # For a given DF, tells which column is the given name.
    getIndexDF    <- function(myName, myDF){
        
        myVector = colnames(myDF)
        
        return (grep(paste0("^",myName,"$"), myVector))
        
    }
    
    # Experimental, since R doesn't have operator override and is a third-class
    # language, it would be great to unify all functions and have proper operation
    # abstraction.
    #
    # So for any given data structure (lists, DF, vectors, whatever), that
    # can be iterated, give the index of the string that you are looking for.
    getIndex      <- function(myName, myDatastructure){
        
        myReturnIndex = -1
        
        if(is.data.frame(myDatastructure) == TRUE){
            
            myReturnIndex = getIndexDF(myName, myDatastructure)
            
        }
        else{
            
            myReturnIndex = getIndexVector(myName, myDatastructure)
            
        }
    
        return ( myReturnIndex )

    }
    
    # Transpose a DF properly, and not the weird thing that stupid R does in the base code
    transposeDF   <- function(myDF){
        
        # Get the original metadata
        originalNRows    = nrow(myDF)
        originalNColumns = ncol(myDF)
        originalNames    = colnames(myDF)
        
        # Prepare the new metadata
        newNRows    = originalNColumns - 1
        newNColumns = originalNRows + 1
        
        # Create the new DF
        trasposeDF = DF(newNRows, newNColumns)
        # -- Gives the column names
        colnames(trasposeDF) = c(originalNames[1], myDF[,1])
        # -- Gives the row names
        trasposeDF[,1] = colnames(myDF)[2:originalNColumns]

        # Start putting data in each place
        for(j in 2:originalNColumns){
        
            for(i in 1:originalNRows){
                
                trasposeDF[(j-1),(i+1)] = myDF[i,j]
                
            }    
            
        }
        
        return(trasposeDF)
        
    }
    
  
    # For a given table and an index, get the categories inside that column
    # You can get either levels or uniques. Levels if levels exist and have been defined, otherwise unique
    # Return an array with strings
    getCategories <- function(tableBase, groupIndex){
        
        # Let me write clearly so you understand how much of a complete garbage the R based libraries are
        #
        # A variable have levels, which are sorted and you need to keep them if you want consistence in your plots
        # or if you have variables that are categorical and have meaning in the order
        #
        # But the levels are defined at table high, not column high, so if I delete all the instances of something, the levels are still there
        #
        # So if I want only people who anwsered a question, and I filter out those who didn't, they are still there.
        #
        # So in here now, I can't get the levels that are sorted and only those that exist
        #
        # I NEED TO FILTER OUT NON-EXISTING LEVELS INSIDE A FUCKING FUCTION BECAUSE THERE IS NO WAY TO DO THIS
        #
        # Fuck all of this, I'm done with R. Never again.!!!!        
        
        # Stupid R fucking shit!
            
        # R is so fucking fuck shit. There is no way to get levels or uniques
            
        # R is the son of one thousand whores, NA %in% Something is not even NA, is nothing, is logical(0) what the fuck does that even mean bitch???!??
            
        
        myStupidRLevels  = levels(tableBase[,groupIndex]) # Default because shitty R
        myStupidRUniques =  as.character(unique(tableBase[,groupIndex]))
        #myCategories     = myStupidRUniques # Default because shitty R

        # If you have something
        if(!is.null(myStupidRLevels)){  # RStudio is shit. First, R doesn't tell you where the errors are when it fails. Literally, it doesn't give you a line, you need to find it via print() traces; and secondly, RStudio open all the brackets all the time every time you have to open a file. Finding things in the code is unnecesarelly difficult and anoying
            # print("not null")
            # 
            # print("I have this levels")
            # print(myStupidRLevels)
            # 
            # print("I have this uniques")
            # print(myStupidRUniques)
            # 
            # 
            # print("I want to take away the levels that don't appear in the unique")
            # print("Which are these")
            myStupidRUniques = myStupidRLevels[!(myStupidRLevels %in% myStupidRUniques )]
            #print(myStupidRUniques)
                    
                    
            myStupidRLevels  = setdiff(myStupidRLevels, myStupidRUniques)
            #print("final results")
            #print(myStupidRLevels)               
            
            # If any of that something not NA
   #         if(sum(!is.na(myStupidRLevels)) < length(myStupidRLevels)){
                
                # If you don't have the same length
    #            if( length(myStupidRUniques) != length(myStupidRLevels) ){
                
                    #myCategories = setdiff(myStupidRLevels, myStupidRUniques)
                    
                    #areTheLevelsInTheUnique = myStupidRLevels[ !(myStupidRLevels %in% myStupidRUniques) ]        
                    #myCategories = setdiff(myStupidRLevels, areTheLevelsInTheUnique)
                    #myCategories  = areTheLevelsInTheUnique
                    
 
           
             #   }
                
                
            #}
        }
        
        # If you don't have anything
        else{
            myStupidRLevels  = unique(tableBase[,groupIndex])    
            
        }
        
        return (myStupidRLevels)
        
        
    }
      

    
    # For a given table and column index, delete the given category if it exist
    # Remake the column and reformat the levels accordingly
    deleteCategory <- function(tableBase, groupIndex, categoryName){
        
        # Define the return table
        toReturn = tableBase
        
        # Check how many of this category do we have
        totalDelete = sum(tableBase[,groupIndex] == categoryName, na.rm = TRUE)

        # If we have more than one, do the thing
        if(totalDelete > 0){

            # Fuck it, let do it with a loop even in R this is shit, I'm tired to optimize things for this stupid language
            keepTheseRows = rep(FALSE, nrow(tableBase)) # There was a bug here with these two parameters interchanged, R run it any way and did something impossible. I hate this language

            for(i in 1:nrow(tableBase)){
            
                # We need to keep the NAs even though in R are poisonous and screw up your data    
                if(is.na(tableBase[i,groupIndex]) == TRUE){
                    
                    keepTheseRows[i] = TRUE
                    
                }
                # If we don't have an NA
                else{
                    
                    # Check if it is different of what we look for
                    if(tableBase[i,groupIndex] != categoryName){
                        
                      keepTheseRows[i] = TRUE  
                      
                    } 
                    
                }
                
            }

            toReturn = tableBase[keepTheseRows,]
            #print("FFFFFFFFFFUCK RRRRRRRRRRRRRRRRR")
            #print(as.character(levels(toReturn[,groupIndex])))
            myCategories = getCategories(toReturn, groupIndex)
            toReturn[,groupIndex] = factor(toReturn[,groupIndex] , levels = myCategories)
            #print(as.character(levels(toReturn[,groupIndex])))
            #print("FFFFFFFFFFUCK RRRRRRRRRRRRRRRRR")
            
            if(FALSE){
            # I have no idea why, but I can't use the getCategories() function here.
            # It get an error in the live with the levels, which is the same line that I'm using here
            # but here I got no error. I swear to god, I'm going to kill every person related with the
            # design and implementation of R, and RStudio, since v0.01 until today
            
            # Delete the levels that don't exist anymore
            myStupidRLevels  = levels(toReturn[,groupIndex])
            myStupidRUniques =  as.character(unique(toReturn[,groupIndex]))
            myCategories     = myStupidRUniques # Default because shitty R

            areTheLevelsInTheUnique = myStupidRLevels[ myStupidRLevels %in% myStupidRUniques ]        
            myCategories  = areTheLevelsInTheUnique

            toReturn[,groupIndex] = factor(toReturn[,groupIndex] , levels = myCategories)
            }
            
        }
        
        # If you don't deleted anything, you might still have the modality in the factors (I HATE YOU R!!!)
        if(totalDelete == 0){
            
            myLevels         = levels(tableBase[,groupIndex])
            #print(myLevels)
            keepTheseLevels  = !(myLevels %in% categoryName)
            #print(keepTheseLevels)
            finalLevels      = myLevels[keepTheseLevels]
            #print(finalLevels)
            
            tableBase[,groupIndex] = factor(tableBase[,groupIndex], levels = finalLevels) # Hijo de puta cabrón de los putos huevos. La madre que parió a los putos levels en el maldito R de la polla en vinagre.
                                                                                          # POR QUÉ HOSTIAS PONES ESTO DENTRO DE NUEVO HIJO DE PUTA???
                                                                                          # NI UN PUTO WARING DE QUE TABLEBASE NO ESTA EN USO!!
            
            toReturn = tableBase
        }
        
        

        return(toReturn)
        
    }
    

    # For a given table and column index, delete the given category if it exist
    # Remake the column and reformat the levels accordingly
    keepCategory <- function(tableBase, groupIndex, categoryName){
        
        # Stupids NAs really
        keepTheseRows = rep(FALSE, nrow(tableBase))
        
        for(i in 1:nrow(tableBase)){
        
            # We only do for non-NA, the NA get deleted too always
            if( !is.na(tableBase[i,groupIndex]) ){
                
                if( as.character(tableBase[i,groupIndex]) == categoryName) keepTheseRows[i] = TRUE
                
            }
            
        }
        
        tableBase = tableBase[keepTheseRows,]
        
        return(tableBase)
        
    }

    # For a given table and column index, delete all the rows that contain NA
    deleteNA <- function(tableBase, groupIndex){
    
        toReturn = tableBase
        
        toReturn = toReturn[!is.na(tableBase[,groupIndex]),]
        
        # If the variable was categorical, redo the levels
        # Seriously R, how fuck up is that you can't do this yourself.
        if(getCategoricalVariables(tableBase)[groupIndex] == TRUE)
            toReturn[,groupIndex] = factor(toReturn[,groupIndex] , levels = getCategories(tableBase, groupIndex))
        
        return(toReturn)
        
    }
    
    # For a given table and column index, transform NA into "Unknown" (default)
    # The factors are keep as the original ones
    transformNA <- function(tableBase, groupIndex, newValue = "Unknown"){
    	
        toReturn = tableBase
        
        totalNA = sum(is.na(toReturn[,groupIndex]))
        
        if(totalNA > 0)

        	toReturn[is.na(toReturn[,groupIndex]),groupIndex] = newValue
        
        return(toReturn)
    	
    }
    
    
    # Really, fuck completely everything about R. There is no base function to do this
    # and all the stupid forcat functions are unreadable. This is is retarded at exponential levels.
    #
    # Delete all rows that are greater than a given value
    #
    # greaterOrEqual       (FALSE) Will also delete values that are equal
    deleteGreaterThan <- function(tableBase, columnIndex, value, keepNA = TRUE, greaterOrEqual = FALSE){
        
        # Find how many rows do we have
        totalRows = nrow(tableBase)
        
        # Prapare the keep this vector
        keepTheseRows = rep(FALSE, totalRows)
        
        # Go through all the values one by one and mark the result
        for(i in 1:totalRows){
        
            currentValue = tableBase[i, columnIndex]
            
            # If it is NA
            if(is.na(currentValue)){
            
                # Do we keep NA?
                if(keepNA == TRUE) keepTheseRows[i] = TRUE
                
            }
            # If it is not NA
            else{
                
                # Do we check for equality?
                if(greaterOrEqual == TRUE){
                    
                    if(tableBase[i, columnIndex] < value) keepTheseRows[i] = TRUE
                    
                }
                else{
                
                    if(tableBase[i, columnIndex] <= value) keepTheseRows[i] = TRUE
                    
                }
                
            }
            
        }
        
        
        return(tableBase[keepTheseRows,])
        
    }
    
 }


############################
# INDEXING
# -getMinimumRow            From a dataframe and column index, which row is the minimum
############################
{
    
    getMinimumRow <- function(tableBase, columnIndex){
    
        toReturn       = 1
        currentMinimum = tableBase[1,columnIndex]
        for (i in 2:nrow(tableBase)) {
            
            candidateMinimum = tableBase[i,columnIndex]
            
            if(candidateMinimum < currentMinimum){
            
                currentMinimum = candidateMinimum
                toReturn = i
                
            }
            
            
        }
            
        return(toReturn)
    }
    
}

############################
# FORMATING
# -- getAlignment()          How many white spaces you need after a string to
#                            align.
# -- getAsterkisPValue()     For a float representing a p-value, return the
#                            asterisk format.
# -- getProgressCharacters() For a float, return a progress bar filled
#                            accordingly.
############################
{
  # Count how many spaces you need to put in between a String and a number, so
  # a bunch of lines with String + Number looks aligned.
  #
  # Cat1: 59                   Cat1:       59
  # Category 2: 100   ---->    Category 2: 100
  # myCat3: 39                 myCat3:     39
  #
  # Return the number of spaces after each string in a list, with minimum 1
  # Important, a NA or NULL must not be in the string list. But it can be "NA" or
  # "NULL"
  #
  # This function doesn't change the string itself.
  getAlignment <- function(stringList){
    
    stringLengths = nchar(stringList)
    maximumLength = max(stringLengths)
    totalSpaces   = maximumLength - stringLengths + 2 # 1 character for the ":" after category name, 1 character for the " " after the ":"
    
    return(totalSpaces)
    
  }
  
  
    # Count how many decimals a number has
    # x needs to be a number.
    getTotalDecimals <- function(x){

        totalDecimals = 0
        
        if (abs(x - round(x)) > .Machine$double.eps^0.5) {
            totalDecimals = nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
        }
        
        return(totalDecimals)

    }
  
    # For a given dataframe, return a string representation with the string
    # properly aligned.
    #
    # Values such as NA and NULL can exist in the DF, but they will be converted
    # to "NA" and "NULL" in the final result. Same for any numerical value.
    getStringFromDF <- function(tableBase, extraOffset = 0){
    
        returnString = ""
        
        # Basic info
        totalColumn   = ncol(tableBase)
        totalRows     = nrow(tableBase)
        columnsNames  = colnames(tableBase)

        # The original DF has info attached to the columns already about the
        # datatypes. So we create an empty one filled with strings only.
        copyBase = DF(totalRows, totalColumn)
        
        # The user might want to add extra spaces at the beggining of each
        # line, which is handled in extraOffset.
        offsetSpaces = paste0(rep(" ",     extraOffset), collapse = "")

        # Convert all cells to character
        for (j in 1:totalColumn) {

            # Convert column names if needed
            if(is.na(columnsNames[i])   == TRUE) columnsNames[i] = "NA"
            if(is.null(columnsNames[i]) == TRUE) columnsNames[i] = "NULL"

            # Convert each cell
            for (i in 1:totalRows) {
            
                currentCell = tableBase[i,j]

                if(is.na(currentCell)   == TRUE) currentCell = "NA"
                if(is.null(currentCell) == TRUE) currentCell = "NULL"                    

                currentCell = as.character(currentCell)

                copyBase[i,j] = currentCell
                            
            }            
                                    
        }
        

        # Find out the longest string in each column
        longestString = rep(0,totalColumn)
        for(j in 1:totalColumn){
        
            longestString[j] = max(nchar(copyBase[,j]), nchar(columnsNames[j]))
            
        }

        # Ok, so now, you need to create a string that has:
        #  - as many characters as the combination of the previous maximi.
        #  - four extra spaces per column, because I think it looks nice.
        #
        # Every line will have this string as base.
        #
        # You fill the info that you need
        
        # Make the first row with the columns names
        returnString = paste0(returnString, offsetSpaces)
        for(j in 1:totalColumn){
         
            currentColumnName      = columnsNames[j]
            currentTotalCharacters = nchar(currentColumnName)
            totalExtraSpaces       = longestString[j] - currentTotalCharacters
            extraSpaces            = paste0(rep(" ",totalExtraSpaces), collapse = "")
            currentColumnString    = paste0(extraSpaces, currentColumnName)
            
            returnString = paste0(returnString, "  ", currentColumnString, "  ")
               
        }
        returnString = paste0(returnString, "\n")
        
        # Now make each of the rows
        for(i in 1:totalRows) {
        
            returnString = paste0(returnString, offsetSpaces)
            
            for(j in 1:totalColumn){
            
                currentCellText        = copyBase[i,j]
                currentTotalCharacters = nchar(currentCellText)
                totalExtraSpaces       = longestString[j] - currentTotalCharacters
                extraSpaces            = paste0(rep(" ",totalExtraSpaces), collapse = "")
                currentColumnString    = paste0(extraSpaces, currentCellText)  
                
                returnString = paste0(returnString, "  ", currentColumnString, "  ")
                    
            }
        
            returnString = paste0(returnString, "\n")
                    
        }
        
        return(returnString)
        
    }
  
    
    # From a summary of numerical variable, get a string summary for the .info
    # files
    getStringFromNumerical <- function(tableBase, variableIndex, extraOffset = 0){
        
        # Get the info that we need
        summaryDF = summarizeNumerical(tableBase, variableIndex)
        interestedValues = c(summaryDF[4,2],summaryDF[5,2],summaryDF[6,2],
                             summaryDF[2,2],summaryDF[7,2],summaryDF[8,2],
                             summaryDF[3,2])
        integerInterested = round(interestedValues,0)
        interestedValues  = round(interestedValues,2)
        integerInterested = as.character(integerInterested)
        interestedValues  = as.character(interestedValues)
        

        # Prepare the offset
        # This is for the beggining of the line
        offsetSpaces = paste0(rep(" ",     extraOffset), collapse = "")
        
        # We want the numbers to be properly aligned in the file, so let
        # find the bigger one, and add spaces to the other as needed.
        #
        # We need to account for decimal places too, as there will be 2 at most
        maximumValueLength = max(nchar(integerInterested))
        for (i in 1:length(interestedValues)) {
        
            # Get the decimals for this number if any
            decimalInterested = strsplit(interestedValues[i], ".", fixed = TRUE)[[1]][2]
            
            currentTotalCharacters = nchar(integerInterested[i])
            extraSpaces = paste0(rep(" ",maximumValueLength - currentTotalCharacters), collapse = "")
            
            if(is.na(decimalInterested)) interestedValues[i] = paste0(extraSpaces,integerInterested[i])
            else                         interestedValues[i] = paste0(extraSpaces,integerInterested[i],".",decimalInterested)
            
        }
        
        #
        returnString = ""
        returnString = paste0(returnString, offsetSpaces, "Min:    ", interestedValues[1], "\n")
        returnString = paste0(returnString, offsetSpaces, "Q1:     ", interestedValues[2], "\n")
        returnString = paste0(returnString, offsetSpaces, "Median: ", interestedValues[3], "\n")
        returnString = paste0(returnString, offsetSpaces, "Mean:   ", interestedValues[4], "\n")
        returnString = paste0(returnString, offsetSpaces, "Q3:     ", interestedValues[5], "\n")
        returnString = paste0(returnString, offsetSpaces, "Max:    ", interestedValues[6], "\n")
        returnString = paste0(returnString, offsetSpaces, "SD:     ", interestedValues[7], "\n")
            

        return(returnString)
    }
    
    
    # Return number of asterisk for p-values
    #
    # From (0.05   to 1]      return "ns"
    # From (0.01   to 0.05]   return "*"
    # From (0.001  to 0.01]   return "**"
    # From [0.0001 to 0.001]  return "***"
    # From [0.00001 to 0.001] return "****"
    #
    #      x     (Float) The number you want to transform
    #
    #    nsEmpty (bool)  If FALSE (default), p-values greater than 0.05 are
    #                    transformed into "ns". If TRUE, tranform to ""
    #
    #                     This is useful if you have a gigantic table full of
    #                     p-values, and you want to avoid visual cluttering
    #                     by removing all "ns" characters
    getAsterkisPValue <- function(x, nsEmpty = FALSE){
    
        result = NA
        # Check wether we have a value or a vector of values
        if(length(x) == 1){
            
            result = "ns"
            if(nsEmpty == TRUE) result = ""
    
            if(!is.na(x)){
      
                if(x < 0.05)   result = "*"
                if(x < 0.01)   result = "**"
                if(x < 0.001)  result = "***"
                if(x < 0.0001) result = "****"
      
            }
            
        }
        else{
            
            result = rep("ns", length(x))
            if(nsEmpty == TRUE) result = rep("", length(x))
            
            for(i in 1:length(x)){
                
                if(!is.na(x[i])){
      
                    if(x[i] < 0.05)   result[i] = "*"
                    if(x[i] < 0.01)   result[i] = "**"
                    if(x[i] < 0.001)  result[i] = "***"
                    if(x[i] < 0.0001) result[i] = "****"
      
                }
                
            }
            
            
        }

        return(result)
    
     }
  
    # Give you an ACSII progress bar
    # x: float in percentage, ie: 45.332 (without the % of course)
    #
    # This doesn't clean the screen.
    getProgressCharacters <- function(x){
    
        #cat("\014")
        baseLine     = "--------- -------- ----------"
        totalChars   = nchar(baseLine)
        totalBars    = ceiling(totalChars * x / 100)
        barsString   = rep("|",totalBars)
        barsString   = paste(barsString, sep = '', collapse='')
        barsString   = paste(barsString, " ",round(x,2), "%", sep = "")
        currentChars = nchar(barsString)
        addChars     = totalChars - currentChars
        if(addChars < 0) addChars = 0
        extraChars   = rep("_",addChars)
        extraChars   = paste(extraChars, sep = '', collapse='')
        barsString   = paste(barsString, extraChars, sep = "")
    
        return(barsString)
    
    }
  
}

############################
# OS in/out
# -- checkIfFolder()          Tells if a path is a folder
# -- automaticFilePath()      For a folder and something you want to save there, 
#                             gives an automatic name for what you want to save.
# -- getFileExtension()       For a filepath, return the extension (if any)
# -- getFileName()            For a filepath, return the file name (no extension)
# -- getFileFolder()          For a filepath, return the complete folder path
# -- getFilePathNoExtension() For a filepath, return the same path with no extension
#
# -- changeFileExtension()    For a filepath, change the extension for a new one
############################
{
  
    # Check if a string is a filename or a folder name
    #
    # Simply check that it ends with a .xxxxx extension or not
    # Right now it only checks for PNGs since is the only image format that matters
    # (aside from vector images, which doesn't work in ggplot2)
    #
    # Return
    # 
    #     TRUE  = This is a filepath of a folder (/home/user/doc)
    #     FALSE = This is a filepath of a file   (/home/user/doc/myImage.png)
    checkIfFolder <- function (myFileString){
    
        result = TRUE
        
        # If the file string is NULL, we don't have a file
        if(is.null(myFileString)){
          
          result = FALSE
          
        }
        else{
          
          # If the file string doesn't ends in PNG, or TEX, we don't have a file
          if( grepl("\\.png$", myFileString) == FALSE &&
              grepl("\\.tex$", myFileString) == FALSE   ){
            
            result = FALSE
          }
          
        }
    
    return (result)
    
  }
  
  
    # Gives an automatic filepath to save your file.
    #
    # This is very useful when you want to generate filepath for doing a lot of
    # plots, analysis or whatever, and give consistent naming rules.
    #
    # -- If your filePath is a file, do nothing and return the same filepath.
    #
    # -- If your filePath is a folder, it make up a filepath for you inside that
    #    folder with these rules:
    #
    #     <File_Type> + <Table Name> + <List of names from the relevant variables> +
    #     <extension type>
    #
    #     Example:
    #
    #     "/../plots/BMIPlot_myPatientsDataFrame_Sex_BMI.png"
    # 
    #    "/../tables/LatexTable_myPatientsDataFrame.tex"
    #
    # String filePath       ; Either the path of a file, or a path of a folder (read above)
    #
    # DataFrame myDataFrame ; (NULL) The data frame that you want to use for
    #                                automatic name giving
    #
    # String tableName      ; (NULL) The name of myDataFrame. You can't get this
    #                                automatically with deparse inside the function.
    #                                so you need to call deparse before the function
    #                                if you want the same name as the variable, or,
    #                                you can override the name of the dataframe by
    #                                giving whatever name you want here.
    #
    # String fileType       ; (NULL) What kind of file you are trying to generate.
    #                                It recommended that you don't leave this to
    #                                NULL as default. The options are:
    #
    #                                NULL = You have no idea of what you are
    #                                       generating, but the function will try
    #                                       to generate a meaningful name anyway
    #                                       with prefix "Unknown_File_Type"
    #
    #                                images = You are making an image, probably a
    #                                         plot from the plotting library.
    #
    #
    #                                          -- Barplots:
    #
    #                                             "LongAbsBarPlot"
    #                                             "LongRelBarPlot"
    #                                             "AbsBarPlot"
    #                                             "RelBarPlot"
    #                                             "CombinedLongAbsBarPlot"
    #                                             "CombinedLongRelBarPlot"
    #                                             "CombinedAbsBarPlot"
    #                                             "CombinedRelBarPlot"        
    #
    #                                          -- Density plots:
    #
    #                                             "BMIPlot"
    #                                             "Density"
    #                                             "CategoricalDensity"    
    #
    #                                          -- Boxplots:
    #    
    #                                             "Boxplot"    
    #
    #                                          -- Histograms:
    #        
    #                                             "Histogram"
    #                                             "CategoricalHistogram"
    #
    #                                          -- Heatmaps:
    #            
    #                                             "pValuesHeatmap"
    #
    #                                          -- Scatterplots:
    #            
    #                                             "Scatterplot"
    #                                             "ScatterplotCategorical"        
    #
    #                                          -- Specials:
    #        
    #                                             "QQ"
    #                                             "RechabilityBoxplot"
    #                                             "SimulationLineplot"    
    #                                             "Tableplot"
    #
    #
    #                                latex = You are making a latex file. It
    #                                        could be a file that contain an
    #                                        image or a table, but a .tex file
    #                                        nevertheless.
    #
    #                                             "LatexTable"
    #                                             "LatexImage"
    #
    #                                tables = A .txt table. Probably you run
    #                                         already all the relevant functions
    #                                         to make this txt human friendly.
    #
    #                                             "txtTable"
    #
    #
    #                                Finally, if you didn't gave NULL, but you gave
    #                                an option that is not in the list of option,
    #                                you will get a "Invalid_File_Type" prefix.
  
    # Int variableIndexX    ; (NULL) The column index of myDataFrame that you want
    #                                to use for the automatic name giving. You can
    #                                have up to 3 variables depending of the type
    #                                of plot or file that you are making
  
    # bool rootPath ; (FALSE) ignore the extension and return the filePath with no
    #                         extension. Usefull to generate automatic names for
    #                         the same analysis where you are going to have a .png,
    #                         .tex, and so on.
  
    automaticFilePath <- function(filePath, rootPath = FALSE,
                                  myDataFrame = NULL, tableName = NULL,
                                  fileType = NULL, variableIndex1 = NULL,
                                  variableIndex2 = NULL, variableIndex3 = NULL){
    
        # This is the final string variable to return
        finalPath = filePath
    
        # If we have a complete filepath, do nothing.
        # Otherwise, gives an automatic name accordingly
        haveFile = checkIfFolder(filePath)
        if(haveFile == FALSE){
      
            # Defaults
            fileExtension = ""
            
            # If you didn't go for NULL filetype, check if the filetype you have is a
            # valid one and give the proper file extension.
            if(!is.null(fileType)){
        
                # (these list are sorted alphabetically)   # R is stupid, this fuck shouldn't be here. THIS IS A CONSTANT VECTOR IN A CONSTANT GATHERING PLACE AWAY FROM THE CODE SO YOU CAN ACTUALLY READ WHATEVER THE FUCK YOU ARE WRITTING!
                                                           # AND THIS SHOULD BE AN ENUM!!!! ONLY VALID TO DECLARE PLOTS IN THIS FUCKING ENUM. FFFUUUUUCK I HATE THIS!!!!
                validImages = c("LongAbsBarPlot",
                                "LongRelBarPlot",
                                "AbsBarPlot",
                                "RelBarPlot",
                                "CombinedLongAbsBarPlot",
                                "CombinedLongRelBarPlot",
                                
                    
                                "BinaryLogitPlot",
                                "BMIPlot",
                                "Boxplot",
                                "CategoricalBoxplot",
                                "CategoricalDensity",
                                "CategoricalHeatmap",
                                "CategoricalHistogram",
                                "CombinedRelBarPlot",
                                "CombinedAbsBarPlot",  # I fucking hate R and the lack of enum, there was a bug here that took one hour to find. I don't want to see R never again when I'm finish with the PhD
                                "CategoricalHeatmap",  # R is an unnecesary headache with an old and stupid arquitecture 
                                "Density",             # And the worse thing is that you change this here, another function stop working, because is not a const enum as described above!
                                "DoubleCategoricalBoxplot",
                                "Histogram",
                                "LongAbsBarplot",
                                "pValuesHeatmap",
                                "QQ",
                                "RechabilityBoxplot",
                                "RelBarplot",
                                "SimulationLinePlot",
                                "Scatterplot",
                                "DateScatterplot",                    
                                "ScatterplotCategorical",                    
                                "DateScatterplotCategorical",                    
                                "Tableplot")
                
                validAnalysis = c("XiTable", "MetaXiTable")
                
                validLatex  = c("LatexImage",
                                "LatexTable")
                
                validTables = c("txtTable")
                
                validHTML   = c("HTMLTable")
        
                if(fileType %in% validImages)   fileExtension = ".png"
                if(fileType %in% validLatex)    fileExtension = ".tex"
                if(fileType %in% validTables)   fileExtension = ".txt"
                if(fileType %in% validAnalysis) fileExtension = ".tex"
                if(fileType %in% validHTML)     fileExtension = ".html"
        
                # If you fail all the check, you made a human error
                if(fileExtension == "") fileType = "Invalid_File_Type"
        
            }
            else{
        
                fileType      = "Unknown_File_Type" # Gives you a very generic plot name
                
            }
      
            # The name of your dataframe variable is given as a parameter
            #
            # Note that if you do this:
            #
            #    tableName = deparse(substitute(myDataFrame))
            #
            # It will not copy the original dataframe name, but the name of which
            # whatever function you are calling this function. So you need to pass the
            # tableName manually. Also note that R doesn't need to pass variables as
            # reference for performance. (or so they say so at CRAN)
            #
            # In any case, give an automatic name if you don't have one
            if(is.null(tableName)){
            
                tableName = "NoTableNameGiven"
            
            }
          
            # Name of each of the variables
            name1 = colnames(myDataFrame)[variableIndex1] # It doesn't matter if it is NULL, you get an empty string
            name2 = colnames(myDataFrame)[variableIndex2]
            name3 = colnames(myDataFrame)[variableIndex3]
      
            # Depending of your file type, you return one type of string or another
            # In any case, put everything together in this variable
            fileName = ""
      
            # Unknown and Invalid
            if(fileType == "Unknown_File_Type")    fileName = paste(fileType, "_", tableName, "_", name1, "_", name2, "_", name3, sep="")
            if(fileType == "Invalid_File_Type")    fileName = paste(fileType, "_", tableName, "_", name1, "_", name2, "_", name3, sep="")
            # TXT
            # HTML
            if(fileType == "HTMLTable")            fileName = paste(fileType, "_", tableName,                                     sep="")
            # Latex
            if(fileType == "LatexTable")           fileName = paste(fileType, "_", tableName,                                     sep="")
            # Images
            #     0 Variables (Specials)
            if(fileType == "RechabilityBoxplot")   fileName = paste(fileType, "_", tableName,                                     sep="")
            if(fileType == "TablePlot")            fileName = paste(fileType, "_", tableName,                                     sep="")
            if(fileType == "SimulationLinePlot")   fileName = paste(fileType, "_", tableName,                                     sep="")
      
            #     1 Variable
            #         Categoricals
            if(fileType == "AbsBarPlot")           fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
            if(fileType == "RelBarPlot")           fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
            if(fileType == "LongAbsBarPlot")       fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
            if(fileType == "LongRelBarPlot")       fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")            
            #         Numericals
            if(fileType == "BinaryLogitPlot")      fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
            if(fileType == "Boxplot")              fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
            if(fileType == "Density")              fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
            if(fileType == "Histogram")            fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
            if(fileType == "QQ")                   fileName = paste(fileType, "_", tableName, "_", name1,                         sep="")
            #     2 Variables
            #         Categorical + Categorical
            if(fileType == "CombinedRelBarPlot")     fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,           sep="")
            if(fileType == "CombinedAbsBarPlot")     fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,           sep="")
            if(fileType == "CombinedLongRelBarPlot") fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,           sep="")
            if(fileType == "CombinedLongAbsBarPlot") fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,           sep="")            
            if(fileType == "XiTable")                fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,           sep="")
            if(fileType == "CategoricalHeatmap")     fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,           sep="")
            #         Categorical + Numerical
            if(fileType == "BMIPlot")              fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
            if(fileType == "CategoricalBoxplot")   fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
            if(fileType == "CategoricalHistogram") fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
            if(fileType == "CategoricalDensity")   fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
            if(fileType == "pValuesHeatmap")       fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
            #         Numerical   + Numerical
            if(fileType == "Scatterplot")          fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")
            if(fileType == "DateScatterplot")      fileName = paste(fileType, "_", tableName, "_", name1, "_", name2,             sep="")            
            #     3 Variables
            #         Numerical + Categorical + Categorical
            if(fileType == "DoubleCategoricalBoxplot") fileName = paste(fileType, "_", tableName, "_", name1, "_", name2, "_", name3, sep="")
      
            #         Numerical + Numerical + Categorical
            if(fileType == "ScatterplotCategorical")     fileName = paste(fileType, "_", tableName, "_", name1, "_", name2, "_", name3, sep="")
            if(fileType == "DateScatterplotCategorical") fileName = paste(fileType, "_", tableName, "_", name1, "_", name2, "_", name3, sep="")            
            #     Several combination of Variables
            if(fileType == "MetaXiTable")                fileName = paste(fileType, "_", tableName,                                     sep="")
      
            # Merge all the information into a variable and return it
            # -- If you have an actual filepath, use that (TODO: FIX THIS!! This has nothing to do with roots)
            if(rootPath == TRUE){
                finalPath = paste(filePath, fileName, sep="")
            }
            # -- If you have a folder
            else{
        
                # If you have one of the special analysis where you generate several
                # tables and images, each with it own name
                if(fileType == "XiTable"){
          
                    finalPath      = vector("list", length = 5)
                    finalPath[[1]] = paste0(filePath, fileName, "_frequency_", name1, fileExtension)    
                    finalPath[[2]] = paste0(filePath, fileName, "_frequency_",  name2, fileExtension)    
                    finalPath[[3]] = paste0(filePath, fileName, "_absolute",   fileExtension)    
                    finalPath[[4]] = paste0(filePath, fileName, "_relative",   fileExtension)    
                    finalPath[[5]] = paste0(filePath, fileName, "_difference", fileExtension)    
                    finalPath[[6]] = paste0(filePath, fileName, "_pvalues",    fileExtension)    
                    finalPath[[7]] = paste0(filePath, fileName, "_summary",    fileExtension)    
          
                }
                # In any other case, return what you need
                else{
                    finalPath = paste(filePath, fileName, fileExtension, sep="")    
                }
            }
        }
        # 
        # print("----------")
        # print(fileType)
        # print(fileName)
        # print(name1)
        # print(name2)
        # print(name3)
        # print("----------")
        
        return(finalPath)
    
    }
  
  
    # Get the extension of a filePath
    #
    # (string) filePath is any form of path as in "../asfaf/asdfas/asf/myFile.txt"
    #
    # return extension including the dot, if exist
    #
    # ie:
    # 
    # "../asfaf/asdfas/asf/myFile.txt"   -> ".txt"
    # "../asfaf/asdfas/asf/myFile"       -> ""
    # "../asfaf/asdfas/asf/myFile."      -> "."
    # "../asfaf/asdfas/asf/my.Fi.le.txt" -> ".txt"
    #
    getFileExtension <- function(filePath){
        
        myExtension = ""
        
        texFilePathSplitted       = strsplit(filePath, "/")[[1]] # Get rid of the filePath and get only the fileName
        texRelativeLocationPath   = texFilePathSplitted[length(texFilePathSplitted)]
        
        texFilePathExtension      = strsplit(texRelativeLocationPath, "\\.")[[1]]
        
        if(length(texFilePathExtension) > 1){
          
          myExtension = paste(".", texFilePathExtension[length(texFilePathExtension)], sep='')
          
        }
        
        return(myExtension)
        
    }
 
  
    # Get the fileName with the extension
    # (string) filePath is any form of path as in "../asfaf/asdfas/asf/myFile.txt"
    # Return "myFile.txt"
    getFileName <- function(filePath, includeExtension = TRUE){
    
        filePathSplitted = strsplit(filePath, "/")[[1]] # Get rid of the filePath and get only the fileName
        fileName         = filePathSplitted[length(filePathSplitted)]
    
        if(includeExtension == FALSE){
      
            myFileExtension         = getFileExtension(filePath)
            totalCharFileExtension  = nchar(myFileExtension)
            totalCharFileName       = nchar(fileName)
            fileName                = substr(fileName, 1, totalCharFileName - totalCharFileExtension)
      
        }
    
        return (fileName)
    
    }
  
    # Get the folder of the filePath where a file is located
    # (string) filePath is any form of path as in "../asfaf/asdfas/asf/myFile.txt"
    # Return "../asfaf/asdfas/asf"
    # You can return "../asfaf/asdfas/asf/" if lastSlash = TRUE
    getFileFolder <- function(filePath, includeLastSlash = FALSE){
    
        myFileName        = getFileName(filePath)
        totalCharFileName = nchar(myFileName)
        totalCharFilePath = nchar(filePath)
        myFileFolder      = substr(filePath, 1, totalCharFilePath - totalCharFileName)
    
        if(includeLastSlash == FALSE) myFileFolder = gsub('.{1}$', '', myFileFolder)
    
        return(myFileFolder)
    }
  
    # Get the filePath and delete the extension
    # (string) filePath is any form of path as in "../asfaf/asdfas/asf/myFile.txt"
    # Return  "../asfaf/asdfas/asf/myFile"
    getFilePathNoExtension <- function(filePath){
    
        myFileExtension        = getFileExtension(filePath)
        totalCharFileExtension = nchar(myFileExtension)
        totalCharFilePath      = nchar(filePath)
        fileName               = substr(filePath, 1, totalCharFilePath - totalCharFileExtension)
    
        return(fileName)
    
    }
    
    
    # Change the filePath extension for a new one
    changeFileExtension <- function(filePath, newExtension){
    	
    	return(paste0(getFilePathNoExtension(filePath), ".", newExtension))
    	
    }
    
   
}



############################
# AUXILIAR
# -- distance2D() Get the distance between two 2D points
############################
{

    distance2D <- function(x1,x2,y1,y2){
    
        return (sqrt((x1-x2)^2 + (y1-y2)^2))
        
    }
        
}


################################################################################
# ESIER R SYNTAX
# Function that goes a easiest interface that looks like more proper C++ objects
#
# DF()      Creates a dataframe
# newList() Creates a list
################################################################################

DF <- function(n_row, n_col, defaultValue = NA){
  
    # Seriously, R is shit, look how complicated is to read a line where you create a dataframe
    myNewDF = data.frame(matrix(defaultValue, nrow = n_row, ncol = n_col))
    return(myNewDF)
}

newList <- function(mylength){
    
    myNewList = vector("list", length = mylength)
    return(myNewList)
    
}




# I have no idea why I made this one:


# Find all the unique integers values in between those column and the maximum count of each per column
#
# ie
#
# A B C D E
# 1 2 3 4 5
# 2 3 3 3 4
# 1 1 1 0 2
#
# with leftColumn = 2 and rightColumn = 4 ( B to D)
#
# Unique Integers: 0 1 2 3 4
#       Max Total: 1 1 1 2 1
maximumInteger <- function(tableBase, leftColumnIndex, rightColumnIndex){

    # This is used in the histogram for finding proper bin size, but
    # I have no idea of why. It does works though
        
    uniqueIntegers = NULL
    totalColumns   = rightColumnIndex - leftColumnIndex + 1
    
    # Find the unique integers
    for (i in 1:totalColumns) {
      currentIndex   = leftColumnIndex + i - 1
      currentUniques = unique(completeTable[,currentIndex])
      uniqueIntegers = unique(c(uniqueIntegers, currentUniques))
      
    }
    
    totalUniques = length(uniqueIntegers)
    totalForEach = rep(0, totalUniques)
    maxForEach   = rep(0, totalUniques)
    
    # Count each
    for (i in 1:totalUniques) {
      
      currentUnique = uniqueIntegers[i]
      currentTotal  = 0
      
      for (j in 1:totalColumns) {
        
        currentIndex   = leftColumnIndex + j - 1
        currentTotal   = sum(tableBase[,currentIndex] == currentUnique, na.rm = TRUE)
        maxForEach[i]  = max(maxForEach[i], currentTotal)
        
      }
      
    }
    
    return(maxForEach)
    
}
 

