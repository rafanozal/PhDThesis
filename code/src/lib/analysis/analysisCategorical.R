source( paste0(MAIN_PROJECT_FOLDER,"src/lib/basic/toolsSummarizers.R"),     encoding="utf-8")

#' Find a p-value between two categorical variables
#' 
#' This function takes a categorical column and a numerical column, and run all
#' possible p-values combinations doing a T-test
#' 
#' The data has this form
#' 
#' -----------------------------------------------------------------------------
#' | ... | Some variables | NUMERICAL VARIABLES | CATEGORICAL VARIABLE | ...
#' -----------------------------------------------------------------------------
#' 
#'                                  ...
#' 
#' | ... |                |       3.4           |       A              | ...
#' | ... |                |       6.4           |       B              | ...
#' | ... |                |       2.6           |       A              | ...
#' | ... |                |       1.2           |       A              | ...
#' | ... |                |       7.8           |       C              | ...
#' | ... |                |       6.6           |       B              | ...
#' 
#'                                  ...
#'                                  
#' -----------------------------------------------------------------------------
#'    
#' The following tests are available:
#'
#'     Welch’s t-test (default)
#'
#'         - Assume that your variances are not equal
#'         - Assume that sample size is not the same
#'
#'     Student's t-test
#'
#'        - Equal variance
#'        - Equal sample size
#'
#' Regardless of the test, the following options are also available
#'
#'     - Two sided (default) testing if the means are different
#'     - Greater
#'     - Less
#'
#' If more than two categories exist, the function also return a one-way ANOVA
#'
#' @param tableBase        (Dataframe) where your data is located
#' 
#' @param groupIndex       (int)       column's with the categorical data
#' 
#' @param variableIndex    (int)       column's with the numerical data
#' 
#' @param hypothesis       (string)    Which hypothesis are you testing
#' 
#                                     "two"      (default) two-sided t-test
#                                     "greater"
#                                     "less"
#
#' @param type             (string)   Which type of t-test do you want to sue
#' 
#                                    "welch"    (default) Welch’s t-test
#                                    "student"            Sudent's t-test
#
#' @param supressWarnings  (bool)    If you want to show warning in the standard
#'                                   output (ie: the console)
#' 
#'                                   FALSE (default) print all the errors
#                                    TRUE            don't print anything
#'
#' @return A list os size 4 containing the following variables:
#' 
#' 
#'     errorCode      (int) A number representing one of the following errors:
#'     
#'                          0 all the t-test were fine
#                          -1 at least one p-value is not a valid number
#
#
#      pValue         (float) a number representing either a p-value or an
#                             error code:
#
#                             [0-1] =  no error, your p-value was done as
#                                      specified and everything went well
#
#                            -1     =  not enough categories, there are none,
#                                      or only one
#
#                            -2     =  not enough samples
#
#                            -3     =  all samples are the same in both groups
#
#                             If only one combination of categories exist, then
#                             this is the result of the T-test, otherwise of the
#                             ANOVA
#
#
#      analysisDF     (dataframe) A dataframe with all the combinations. If only
#                                 one combination exist, it return a dataframe
#                                 with only that combination, equal to the
#                                 previous p-value
#
#                     -------------------------------------------------------
#                                    |  Category 1 | ... | Category  N
#                     -------------------------------------------------------
#                     Category 1
#                        ...
#                     Category N
#
#                             Categories that combine with itself are empty
#
#
#      centralitiesDF (dataframe) A dataframe with all the centralities measures
#
#                      ---------------------------------------------------------
#                                     |  Mean | Median | Variance | Sigma | Size
#                      ---------------------------------------------------------
#                      Category 1
#                        ...
#                      Category N
#
#' 
#' 
#' 
#' @export
#'
#' @examples
simpleCategoricalPValueV2 <- function(tableBase, groupIndex, variableIndex,
                                      hypothesis = "two", type = "welch",
                                      supressWarnings = FALSE){
            
    # Check out the preconditions data
    #
    #         tTestString = paste(" T-Test performed under these conditions:",
    # "     + Assumed unequal variance (Welsh df)",
    # "     + Two Sided",
    # "     + Mu = 0",
    # "",
    # " - Whas your population sampled randomly properly?",
    # "     If not try Resampling: http://finzi.psych.upenn.edu/R/library/coin/doc/coin.pdf",
    # " - Whas your data non normally distributed?",
    # "     For non-parametric test try Mann-Whitney U, Wilcoxon Signed Rank, Kruskal Wallis, and Friedman tests.",
    # " - Are you doing a lot of T-Test at the same time?",
    # "     Consider doing an ANOVA instead, and make sure you run a post-hoc if you don't.",
    # sep="\n")                

    
    # Get the name of the table, save it for later
    myTableName = deparse(substitute(tableBase))
            
    # Init the variables to be returned
    errorCode      = 0
    pValueReturn   = -1
    centralitiesDF = NA
    pValuesDF      = NA
            
    # Init the R variables for the t-tests
    myAlternative  = "two.sided"
    if(hypothesis != "two") myAlternative = hypothesis
    myVariance     = FALSE
    if(type!="weltch") myVariance = TRUE
            
    # Create a copy of the data that we can modify freely
    {
            
    	copyTable = tableBase
                
        # Take away the NA rows in the grouping index
        keepTheseRows = rep(TRUE, nrow(tableBase))    
        keepTheseRows = keepTheseRows & (!is.na(tableBase[,groupIndex]))
        copyTable = copyTable[keepTheseRows,]
                
        # We don't care about the NA in variables for now, they are filtered
        # away automatically by each function, and we will check that we have
        # a valid standard deviation and sufficient categories anyway
                
	}
            
    # Now count how many categories do you have, in order to do an
    # ANOVA or t-test (or something else, according to preconditions)
    myCategories  = getCategories(tableBase, groupIndex)
    nCategories   = length(myCategories)
    groupingName  = colnames(copyTable)[groupIndex]
    numericalName = colnames(copyTable)[variableIndex]    
                
            
    # Check that we have enough categories to do something
            
    # In the case that we have nothing and we screw up
    if(nCategories < 2){
    	
        errorCode = -1        
        
    	if(supressWarnings == FALSE){

        	print( "WARNING!! Can't generate p-value")
            print( "          Only one categorie was found")
            print( "          To make a p-value I need at least two categories with two different values in each")
            print( "          This was the attemp: ")
            print( paste("          Table:              ", myTableName,                                                  sep=''))
            print( paste("          Category variable:  ", groupingName , " only found ", as.character(myCategories[1]), sep='' ))
            print( paste("          Numerical variable: ", numericalName,                                                sep=''))                    
                    
		}
                
	}
    # Otherwise, continue as normal
    else{
       
    	# Init the dataframe with the centralities
        centralitiesDF           =  data.frame(matrix(NA, nrow = nCategories, ncol = 5 + 1))
        colnames(centralitiesDF) = c("ID", "Mean", "Median", "Variance", "Sigma", "Size")
        centralitiesDF[,1]       = myCategories   
            
        # Init the dataframe with the p-values
        pValuesDF           =  data.frame(matrix(NA, nrow = nCategories, ncol = nCategories + 1))
        colnames(pValuesDF) = c("",myCategories)
        pValuesDF[,1]       = myCategories                
                         
        # Create as many subsets as there are categories, and put them into this list
        subsetsDF     = rep(list(data.frame(NULL)), nCategories)
        for(i in 1:nCategories){
            
			subsetsDF[[i]] = subset(copyTable, copyTable[,groupIndex] == as.character(myCategories[i]))
            
		}
                
		# For each category (start at 1 because we want to find all centralities)
        for(i in 1:nCategories){
          
        	samplesI      = subsetsDF[[i]][,variableIndex]
            samplesI      = samplesI[!is.na(samplesI)]

            meanSamplesI   = mean(samplesI)
            medianSamplesI = median(samplesI)
            totalSamplesI  = length(samplesI)
            varSamplesI    = var(samplesI)
            sdSamplesI     = sd(samplesI)
                    
            centralitiesDF[i,2] = meanSamplesI
            centralitiesDF[i,3] = medianSamplesI
            centralitiesDF[i,4] = varSamplesI
            centralitiesDF[i,5] = sdSamplesI
            centralitiesDF[i,6] = totalSamplesI
                    
            # For each other category, run the t-test
            for(j in 1:nCategories){
                    
            	# Don't run a p-value on yourself
                # and don't run the upper diagonal matrix
                if(i>j){

                	# Count that you have enough data to do a T-Test
                    {
                            
                    	samplesJ      = subsetsDF[[j]][,variableIndex]
                        samplesJ      = samplesJ[!is.na(samplesJ)]
                                
                        meanSamplesJ   = mean(samplesJ)
                        medianSamplesJ = median(samplesJ)
                        totalSamplesJ  = length(samplesJ)
                        varSamplesJ    = var(samplesJ)
                        sdSamplesJ     = sd(samplesJ)                                
                                
					}
                                
					# If you do...
                    if( (totalSamplesI >= 2) && (totalSamplesJ >= 2) ){
                  
                    	# Count that not everything is the same
                        # If you do, then you can run a T-Test
                        if((sdSamplesI > 0) && (sdSamplesJ > 0) ){
                    
                        	# Run the t-test with the given options
                            pValueReturn = t.test(samplesI, samplesJ,
                                                  alternative = myAlternative,
                            	                  var.equal = myVariance)$p.value
                                    
                                    
                        }
                    	
                        # Otherwise, you can't run the test
                        else{
                    
                        	pValueReturn = -3    
                        
						}
 
					}
                    else{
                
                    	pValueReturn = -2
                        
                    }

                	# Write the result into the p-values matrix
                    pValuesDF[i,j+1] = pValueReturn # If we only have one combination, pValue return already have the returned option                            
                                                        
				}

			}
                    
		}

                
        # If you are in the case where several categories exist, substitu the given p-value for a ANOVA
        if(nCategories > 2){
                    
        	anovaDF = copyTable[,c(groupIndex,variableIndex)]  
            # Delete the NA rows
            anovaDF = anovaDF[is.na(anovaDF[,2]) == FALSE,]
            anovaDF = anovaDF[is.na(anovaDF[,1]) == FALSE,]

            # Gives names because R sucks and can't give numerical indexes    
            colnames(anovaDF) = c("Categorical", "Numerical")
    
            anovaResult = aov(Numerical ~ Categorical, data = anovaDF)

            pValueReturn = summary(anovaResult)[[1]][["Pr(>F)"]][1]         
                    
		}
           
                
	}
            
    # Return everything
    toReturn = newList(4)
    toReturn[[1]] = errorCode
    toReturn[[2]] = pValueReturn
    toReturn[[3]] = centralitiesDF
    toReturn[[4]] = pValuesDF

    return(toReturn)

}
      

# This function takes a table, and two categorical columns, and return a complete
# Xi² test. For 2x2 tables it will perform a Yates correction.
#
# The results are as follow:
#
# 1:
#
# A dataframe with the frequencies for groupIndex
#
# 2:
#
# A dataframe with the frequencies for variableIndex
#
# 3:
#
# A dataframe with the absolute frequency for the combination of the two variables
#
# 4:
#
# A dataframe with the relative frequency for the combination of the two variables
#
# 5:
#
# A dataframe with the relative frequency that is above expected, or bellow
# expected. A value equal to 1 means everything is as it is suppose to be.
# Greater than 1 means increase, so 1.15 means a 15% increase. Lower than 1
# means decrease, so 0.75 means a 25% decrease.
#
# 6:
#
# A binomial test for each of the combinations for each cell. Individual cells
# that give you a low p-value doesn't necessarily means that the whole table
# is statistically significant. In fact, all numbers can be low p-value in this
# dataframe and the xi2 test still be really high p-value.
#
# 7:
#
# A summary dataframe with all of the above, including a xi2 test at [1,1]
#
# 8:
# 
# The result of the xi2 test.
#
# 9:
#
# A summary dataframe with all the frequencies.
#
categoricalXiV2 <- function(tableBase, groupingIndex, categoricalIndex,
                            supressWarnings   = FALSE){
        
    # The results dataframes goes here
    {
        myReturn = newList(9)
        myReturn[[1]]   = 0  # Frequency table for the grouping (Rows)
        myReturn[[2]]   = 0  # Frequency table for the categorical (columns)
        myReturn[[3]]   = 0  # Absolute count for each combination
        myReturn[[4]]   = 0  # Relative count for each combination
        myReturn[[5]]   = 0  # Difference from theoretical distribution
        myReturn[[6]]   = 0  # Binomial test for each cell
        myReturn[[7]]   = 0  # The ultra-summary deluxe for Xi tables 
        myReturn[[8]]   = -1 # This number tells you if there was an error (-1)
                                    # Or if everything went fine, p-value (0,1)          
        myReturn[[9]]   = -1 # This is just a nice looking summary of frequencies
        myReturn[[10]]  = -1 # This is given frequency vs expected frequency. Is the same as [[7]], but in relative frequency form
        
    }

    # Get some variables information
    {
        # Table name
        myTableName = deparse(substitute(tableBase))   
    }
    
    
    # Convert the numerical in categories if needed
    {
        
        # -- For the grouping    variable (rows)
        {
                
            groupingVariableType = class(tableBase[,groupingIndex])
            if(groupingVariableType != "character" && groupingVariableType != "factor") { 
                
                # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
                myLevels = sort(unique(tableBase[,groupingIndex]))
                myLevels = as.character(myLevels)
                
                tableBase[,groupingIndex] = as.character(tableBase[,groupingIndex])
                tableBase[,groupingIndex] = factor(tableBase[,groupingIndex], levels = myLevels)
               
                # Tell the user that something wonky happens
                if(supressWarnings == FALSE){
                  
                    print("WARNING!!")
                    print("Doing the xi-tables for:")
                    print(myTableName)
                    print("With index")
                    print(groupingIndex)
                    print("I found a numerical variable")
                    print("I transformed into categorical automatically and did the xi-table anyway")
                    print("Maybe you wanted to do something else?")
                  
                }
                 
            }
               
        }
        
        # -- For the categorical variable (columns)
        {
            
            categoricalVariableType = class(tableBase[,categoricalIndex])
            if(categoricalVariableType != "character" && categoricalVariableType != "factor") { 
            
                # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
                myLevels = sort(unique(tableBase[,categoricalIndex]))
                myLevels = as.character(myLevels)
            
                tableBase[,categoricalIndex] = as.character(tableBase[,categoricalIndex])
                tableBase[,categoricalIndex] = factor(tableBase[,categoricalIndex], levels = myLevels)
            
                # Tell the user that something wonky happens
                if(supressWarnings == FALSE){
              
                    print("WARNING!!")
                    print("Doing the xi-tables for:")
                    print(myTableName)
                    print("With index")
                    print(categoricalIndex)
                    print("I found a numerical variable")
                    print("I transformed into categorical automatically and did the xi-table anyway")
                    print("Maybe you wanted to do something else?")
              
                }
            
            }            
            
        }
            
    }

    # Get info about different categories
    {

        myCategoriesA   = getCategories(tableBase, groupingIndex)
        nCategoriesA    = length(myCategoriesA)
        groupingNameA   = colnames(tableBase)[groupingIndex]
          
        myCategoriesB   = getCategories(tableBase, categoricalIndex)
        nCategoriesB    = length(myCategoriesB)
        groupingNameB   = colnames(tableBase)[categoricalIndex]

        # Make sure that you have at least 2 categories in each group
        # -- If you have less than 2 in either, print a warning
        if(nCategoriesA<2 || nCategoriesB <2){
          
            if(supressWarnings == FALSE){
                print("WARNING: Making xi² tables")
                print("You gave me two indexes and one of them have less than 2 categories")
                print(myTableName)
                print(groupingIndex)
                print(categoricalIndex)            
            }
        }

        # -- Otherwise, continue as normal
        else{

            # Count how many rows in the DB we have
            totalRows = nrow(tableBase)
          
            # Count how many are for each modality of the grouping variable
            uniquesDFA           = DF(nCategoriesA, 3)
            colnames(uniquesDFA) = c(groupingNameA, "NTotal", "NFreq")
          
            uniquesDFB           = DF(nCategoriesB, 3)
            colnames(uniquesDFB) = c(groupingNameB, "NTotal", "NFreq")
          
            # Make the df with the absolute , relative, diff, and binomial test
            xiDF           = DF(nCategoriesA, nCategoriesB+1, 0)
            colnames(xiDF) = c("Absolute", myCategoriesB)

            # Init some of the other tables that we will use later, they have the same amount of cells
            # and the same names in the rows and columns
            xiFqcDF  = xiDF
            xiDiffDF = xiDF
            xiBiDF   = xiDF

            # Rename the top square with the proper table title
            colnames(xiFqcDF)[1]  = "Relative"  
            colnames(xiDiffDF)[1] = "Difference"
            colnames(xiBiDF)[1]   = "Binomial"
          
            # Count the combinations
            # -- For each of the grouping modalities (rows)
            for(i in 1:nCategoriesA){
            
                    # Init variables
                    currentModality    = myCategoriesA[i]
                    print(currentModality)
                    uniquesDFA[[i,1]]  = currentModality
                    xiDF[[i,1]]        = currentModality
                    xiFqcDF[[i,1]]     = currentModality
                    xiDiffDF[[i,1]]    = currentModality
                    xiBiDF[[i,1]]      = currentModality

                    # Subset of the tableBase with respect the grouping modality
                    currentModalityOnlyTable  = subset(tableBase, tableBase[,groupingIndex] == currentModality)
                        
                    # Count the absolute and relative frequencies
                    totalModality     = nrow(currentModalityOnlyTable)
                    fqcModality       = totalModality/totalRows
            
                    # Add it to the total frequency table
                    uniquesDFA[[i,2]]  = totalModality
                    uniquesDFA[[i,3]]  = fqcModality
            
                    # -- For each of the categorical modalities (column)
                    for(j in 1:nCategoriesB){
              
                        # Init variables  
                        destinyModality        = myCategoriesB[j]
              
                        # Count how many of these two you have in the table
                        # and write it down in the proper DF
                        # -- From this subset, count how many do you have of the categorical modality
                        destinyModalityTotal   = sum(currentModalityOnlyTable[,categoricalIndex] == destinyModality, na.rm = TRUE)
                        # -- Write it down
                        xiDF[[i,1+j]]    = destinyModalityTotal
                        xiFqcDF[[i,1+j]] = destinyModalityTotal / totalRows
              
                    }
            
                }
          
            # Count the uniques for category B
            for(j in 1:nCategoriesB){
            
                    # Init variables
                    currentModality   = myCategoriesB[j]
                    uniquesDFB[[j,1]] = currentModality

                    # Subset of the tableBase with respect the grouping modality
                    currentModalityOnlyTable  = subset(tableBase, tableBase[,categoricalIndex] == currentModality)
            
                    # Count the absolute and relative frequencies
                    totalModality     = nrow(currentModalityOnlyTable)
                    fqcModality       = totalModality/totalRows
            
                    # Add it to the table
                    uniquesDFB[[j,2]]  = totalModality
                    uniquesDFB[[j,3]]  = fqcModality
            
                }

            # We have an special number which is the total of everything, save it for later
            absoluteTotal = sum(uniquesDFA[,2])
            
            # Now that each of the the uniqueDF tables are complete,
            # we can do the Binomial test in each cell
            # for each cell
            for(i in 1:nCategoriesA){
                    
                    for(j in 1:nCategoriesB){

                        # First, we need to see how much is above/bellow what the theoretical value is suppose to be
                        # The theoretical value is the Frequency of the Row x Frequency of the Column
                        theoreticalValue = uniquesDFA[[i,3]] * uniquesDFB[[j,3]]
                        # The actual vallue is what we have in the Frequency table
                        actualValue = xiFqcDF[[i,1+j]]
                        # See how much is above the rest
                        xiDiffDF[[i,1+j]]  = actualValue / theoreticalValue 
                        # Do the binomial test for that cell, it doesn't matter if you do it by rows or columns
                        #xiBiDF[[i,1+j]]    = binom.test(xiDF[[i,1+j]], uniquesDFA[[i,2]], uniquesDFA[[i,3]], alternative = "two.sided")$p.value
                        xiBiDF[[i,1+j]]    = binom.test(xiDF[[i,1+j]], sum(xiDF[,1+j]), uniquesDFA[[i,3]], alternative = "two.sided")$p.value
                        #xiBiDF[[i,1+j]]    = binom.test(xiDF[[i,1+j]], absoluteTotal, uniquesDFA[[i,3]], alternative = "two.sided")$p.value
                        #                               how many     , col sum         ,  probability

                        # Addjust p-values to be negative which represent UNDER BIAS
                        if(xiDiffDF[[i,1+j]] < 1){

                            # If the value is very close to 0 (around e^-300), R will round it to 0
                            # We need to keep the 0s that are negative
                            if(xiBiDF[[i,1+j]]      < 1/10^250) xiBiDF[[i,1+j]]      = 1/10^250
                            # In any case, flip the sign
                            xiBiDF[[i,1+j]]      = (-1) * xiBiDF[[i,1+j]]


                        }

                    }

                }
          
            # The table is complete, now we can make the proper R analysis
 
            # Do the Chi² Test for the whole contingency table
            xiTotalResults = chisq.test(xiDF[,-1])
            pValue         = xiTotalResults$p.value

            # Make the ultra summary table
            {
                    
                    summaryDF      = data.frame(matrix(0, nrow = nCategoriesA + 2, ncol = (nCategoriesB*2) + 3))
            
                    # -- Init the table labels and names
                    {
                        # ---- First and last column
                        colnames(summaryDF)[1]                   =  signif(pValue,1) 
                        colnames(summaryDF)[ncol(summaryDF) - 1] = "Total"
                        colnames(summaryDF)[ncol(summaryDF)]     = "Freq"
                        # ---- Rest of columns
                        for(j in 1:(nCategoriesB*2)){
                
                            if(j %% 2 == 0) colnames(summaryDF)[j+1] = ""
                            else colnames(summaryDF)[j+1] = myCategoriesB[floor(j/2)+1]
                
                        }
                        # ---- Last rows
                        summaryDF[(nCategoriesA+1), 1] = "Total"
                        summaryDF[(nCategoriesA+2), 1] = "Freq"
                        # ---- Each of the rows
                        for(i in 1:nCategoriesA){
                
                            summaryDF[i, 1] = myCategoriesA[i]
                
                        }
                    }
            
                    # -- Fill the data that we already have, absolute numbers and totals
                    {
                        # ---- Remove the very last cell, which is Total by Total (it doesn't make sense)
                        {
                            summaryDF[nrow(summaryDF)-1 , ncol(summaryDF) - 1] = totalRows
                            summaryDF[nrow(summaryDF)-1 , ncol(summaryDF)    ] = ""
                            summaryDF[nrow(summaryDF)   , ncol(summaryDF) - 1] = ""
                            summaryDF[nrow(summaryDF)   , ncol(summaryDF)    ] = 1
                        }

                        # ---- Total and frequency of each row
                        for(i in 1:nCategoriesA){
                
                            summaryDF[i, ncol(summaryDF) - 1] = uniquesDFA[i,2]
                            summaryDF[i, ncol(summaryDF) ]    = round(uniquesDFA[i,3],2)
                
                        }
              
                        # ---- Fill the absolute numbers from the xiDF table
                        for(i in 1:nCategoriesA){
                
                            for(j in 1:nCategoriesB){
                  
                                summaryDF[i,(j*2)] = xiDF[[i,1+j]]
                  
                            }
                
                        }
              
                        # ---- Total of each column
                        for(j in 1:nCategoriesB){
                
                            summaryDF[nrow(summaryDF)-1,(j*2)] = sum(summaryDF[,(j*2)])
                            summaryDF[nrow(summaryDF),  (j*2)] = round(uniquesDFB[j,3],2)
                
                        }
              
              
                    }
            
                    # -- Finally, fill the extra info that tells you if something grows or shrink
                    #    We only add the extra info in places where is significant, so we
                    #    use the binomial table for that
            
                    # In here we save another type of summary  
                    overviewDF    = summaryDF
                    frequenciesDF = summaryDF
              
                    # ---- Fill the cells that are not the total
                    for(i in 1:nCategoriesA){
              
                        for(j in 1:nCategoriesB){
                
                            # Same value as in the absolute table
                            # Also add the theoretical values we should get
                
                            summaryDF[i,(j*2)]      = paste0(xiDF[[i,1+j]] , "/" , floor(totalRows * uniquesDFA[i,3] * uniquesDFB[j,3]) )
                            overviewDF[i,(j*2)]     = xiDF[[i,1+j]] 
                            overviewDF[i,((j*2)+1)] = paste0("(" , round(xiFqcDF[[i,1+j]],2) , ")")
                            
                            frequenciesDF[i,(j*2)]     = round(xiFqcDF[[i,1+j]],2) 
                            frequenciesDF[i,((j*2)+1)] = paste0("(" , round(uniquesDFA[i,3] * uniquesDFB[j,3],2) , ")")                
                            
                            
                            # Add a little "+"/"-" in the next cell indicating something is wrong
                            if(xiBiDF[[i,1+j]] > -0.1 && xiBiDF[[i,1+j]] < 0.1){
                  
                                # Add a lot of +/- if the difference is too big
                                signPower = 1
                                if (abs(xiBiDF[[i,1+j]])<0.05)   signPower  = 2
                                if (abs(xiBiDF[[i,1+j]])<0.01)   signPower  = 3
                                if (abs(xiBiDF[[i,1+j]])<0.001)  signPower  = 4
                                if (abs(xiBiDF[[i,1+j]])<0.0001) signPower  = 5
                  
                                if(xiBiDF[[i,1+j]] > 0){
                    
                                    summaryDF[i,(j*2)+1] = paste(rep("+", signPower), collapse="") # Seriously, R is horrible!
                    
                                }
                                else{
                                    
                                    summaryDF[i,(j*2)+1] = paste(rep("-", signPower), collapse="")
                                    
                                }
                  
                            }
                            else{
                                
                                summaryDF[i,(j*2)+1] = ""
                                
                            }
                
                        }
                        
                    }
            
                    # ---- Delete the 0's in the last "Total" row
                    #      This is never significant since is just the description
                    for(j in 1:nCategoriesB){
              
                        summaryDF[nrow(summaryDF)-1,1 + (j*2)] = ""
                        summaryDF[nrow(summaryDF),  1 + (j*2)] = ""
              
                    }
            
                    # ---- Move the frequency numbers +1 to the right
                    #      Also delete the extra 0's in Total
                    for(j in 1:nCategoriesB){
            
                        overviewDF[nrow(summaryDF)-1,1 + (j*2)] = ""
                        overviewDF[nrow(overviewDF), 1 + (j*2)] = overviewDF[nrow(overviewDF), (j*2)]
                        overviewDF[nrow(overviewDF), (j*2)]     = ""
                        
                        frequenciesDF[nrow(summaryDF)-1,1 + (j*2)] = ""
                        frequenciesDF[nrow(overviewDF), 1 + (j*2)] = frequenciesDF[nrow(frequenciesDF), (j*2)]
                        frequenciesDF[nrow(overviewDF), (j*2)]     = ""                        
                    
                    }
            
                }

            # Addjust the tableFilePath return value because R is horrible
            # and if you return a NULL it shrink the vector size instead of having
            # A PROPER OBJECT which value is null
          
            # Fill the return vector
            myReturn[[1]]  = uniquesDFA
            myReturn[[2]]  = uniquesDFB
            myReturn[[3]]  = xiDF
            myReturn[[4]]  = xiFqcDF
            myReturn[[5]]  = xiDiffDF
            myReturn[[6]]  = xiBiDF
            myReturn[[7]]  = summaryDF
            myReturn[[8]]  = pValue
            myReturn[[9]]  = overviewDF
            myReturn[[10]] = frequenciesDF
          
        }

    }

 
    return (myReturn)
       
}



# This function takes a table and a list of explanatory indexes and 
# another list of target indexes and it runs all the xi² combinations
#
# Returns:
#    - a simple dataframe with the results
multiplesXis <- function(tableBase, explanatoryIndexesList, targetIndexesList){
          
	# Init some variables
    totalExplanatoryIndexes   = length(explanatoryIndexesList)
    totalTargetIndexes        = length(targetIndexesList)

    # Init the dataframe where we put the results
    # 
    # R is horrible, there was a bug here where it was use completeTable
    # instead of tableBase, and other variable names for the explanatory
    # and target lists. Because R doesn't care about variable type,
    # since completeTable is not declared as a global variable, it
    # should have been a pre-compilation error easy to fix instead of
    # making me lost 20 mins finding this.
    #
    # I should make an script that looks at variable declaration within
    # function and fix all of that forever.
    xi2SummaryDF = readyDFVariables(tableBase, explanatoryIndexesList, targetIndexesList)
            
    for(i in 1:totalExplanatoryIndexes){
            
    	currentExplanatoryIndex = explanatoryIndexesList[i]
                
        for(j in 1:totalTargetIndexes){
              
        	currentTargetIndex = targetIndexesList[j]
            xiResults = categoricalXiV2(tableBase, currentExplanatoryIndex, currentTargetIndex)

            xi2SummaryDF[i,j+1] = xiResults[[8]]

		}
                    
	}
            
	return(xi2SummaryDF)
            
}





# This function takes a table with two variables
#
#     yIndex is the categorical column with the output. It needs to have only two categories
#
#     xIndexes is a list with any other categorical variable that you want to analyise.
#
# The function returns three list:
#
#     (1)
#
#     The first list are the dataframes with the summarized values that we are
#     using in each analysis, including marginals:
#
#              Positive   Negative     
#        A          363        419    782
#        B            0          0      0
#        C            4          3      7
#        D            7          9     16
#        E            4          9     13
#        F            5          7     12
#        G          105        103    208
#                   488        550   1038
#       
#        You might have the bad luck, such as in this example, that one modality
#        exist, with 0 values. The function present you this table, but it will
#        skip it for the rest of the analysis.
#
#
#     (2)
#
#     The second list is a single dataframe with the risk ratio results:
#
#
#     Variable   | Modlity  |  Estimate     |    Lower   |   Upper   |  pv1  pv2 pv3
#     -------------------------------------------------------------------          
#     Variable 1 |          |               |            |           |
#                |  Cat A   |         1     |            |           |
#                |  Cat B   |        -1     |   -1       |   -1      |   1   1   1
#                |  Cat C   |         2.3   |    1.2     |    4.5    |
#                |  Cat D   |         0.4   |    0.1     |    1.3    |
#                |  Cat E   |         2     |    1       |    3      |
#                |  Cat F   |         1.2   |    0.9     |    1.5    |
#                |  Cat G   |         1.1   |    0.5     |    1.4    |
#
#
#
# For each variable:
#
#                        Risk/Odd     Lower      Upper       P-value
# Category 1 (Base)        1          1          1              1
# Category 2             0.5          0.34       0.85           0.04
# ...
#
# With the plotting library, you can use this later to plot something like this:
#
#  Variable A
#       Cat A   |           X
#       Cat B   |         [-----------X-----]
#       Cat C   |           |      [------------X-----------]
#
#  Variable B
#       Cat A   |           X
#       Cat B   |  [-X---]  |
#       Cat C   |           | [---X-]
#
#  ------------|---------------------------------------------------------------
#             (0)          (1)
#
#
#
#
doOddRiskRatio <- function(tableBase, yIndex, xIndexes, skipUnknowns){
 
    # Init the error flag and return object
    myError  = 0
    myReturn = newList(3)
    myReturn[[1]] = 0
    myReturn[[2]] = 0
    myReturn[[3]] = 0
    
    # Get the modalities and how many for Y variable
    yModalities      = getModalitiesV2(tableBase,      yIndex, skipUnknowns)
    yModalitiesTotal = getTotalModalitiesV2(tableBase, yIndex, skipUnknowns)
    if(yModalitiesTotal != 2) myError = -1
    
    # Check if the function is good so far
    if(myError<0){
        
        if(myError == -1){
            print("ERROR!:")
            print("")
            print("You are trying to do an odd or risk ratio with a dependent variable that has more than two categories")
            print("You need to have exactly 2 (ie: Sick, not Sick), please correct that and try again.")
                
            
        }
    }
    # If so, keep going
    else{
        
        # Get the name of the variables
        xNames = colnames(tableBase)[xIndexes]
        
        # Prepare the return object
        {
        
            totalModalities = 0
            
            totalXs = length(xIndexes)
            for(i in 1:totalXs){
                currentXModalities      = getModalitiesV2(tableBase,      xIndexes[i], skipUnknowns)
                currentXModalitiesTotal = getTotalModalitiesV2(tableBase, xIndexes[i], skipUnknowns)  
                totalModalities         = totalModalities + currentXModalitiesTotal
            }
            
            oddReturn = DF((totalModalities + totalXs), 8)
            colnames(oddReturn) = c("Variable", "Modality", "Estimate", "Lower", "Upper", "MedianUnbias", "FisherExact", "ChiSquared")
            riskReturn = oddReturn
            # Prepare the list with the marginals
            marginalsDFs = newList(totalXs)
        }
        
        print(" -- Init done --")
        
        # Run all combinations
        {

            totalXs    = length(xIndexes)
            currentRow = 1
            for(i in 1:totalXs){
                
                print("--------")
                print("X:")
                print(i)
                
                # Init the name of the variable. Everything else in that row is blank (+1)
                oddReturn[currentRow,1]  = xNames[i]
                riskReturn[currentRow,1] = xNames[i]
                currentRow = currentRow + 1
                    
                # Get the counting table
                currentSummary   = summarizeBicategoricalV2(tableBase, xIndexes[i], yIndex, skipUnknowns)
                currentXModalities      = getModalitiesV2(tableBase,      xIndexes[i], skipUnknowns)
                currentXModalitiesTotal = getTotalModalitiesV2(tableBase, xIndexes[i], skipUnknowns)
                currentXBadY1Rows       = currentSummary[,2] == 0
                currentXBadY2Rows       = currentSummary[,3] == 0
                currentXBadRows         = currentXBadY1Rows | currentXBadY2Rows
                finalXModalities        = currentXModalities
                
                # Add the marginals to the results list
                marginalsDFs[[i]] = currentSummary
                
                # Delete the rows that are bad, if any
                if( sum(currentXBadRows)>=1 ){
                    currentSummary   = currentSummary[!currentXBadRows,]
                    finalXModalities = currentXModalities[!currentXBadRows]  # R is a stupid language, this would be SO MUCH EASIER if you have a proper container classes where you can do .pop to extract whatever the fuck you want, like in any serious language!!!
                    finalXModalities = finalXModalities[1:length(finalXModalities)-1]
                    
                }

                print(currentSummary)
                print(finalXModalities)
                
                # Create the matrix for the epitool function
                listOfValues = rep(0, (nrow(currentSummary)-1) * yModalitiesTotal) # -1 because it includes the totals
                currentIndex = 1
                for(j in 1:(nrow(currentSummary)-1)){
                    for(k in 1:yModalitiesTotal){
                        listOfValues[currentIndex] = currentSummary[j,k+1]
                        currentIndex = currentIndex + 1
                    }
                }
                
                currentData           = matrix(listOfValues, nrow=(nrow(currentSummary)-1), ncol=yModalitiesTotal, byrow=TRUE)
                dimnames(currentData) = list('Variable' = finalXModalities, 'Outcome' = yModalities)
                
                print(currentData)
                
                oddResults  = oddsratio(currentData)
                riskResults = riskratio(currentData)
                
                # For each modality
                badResultCounter = 0
                for(j in 1:currentXModalitiesTotal){
                
                    # The first column is blank on porpouse
                    
                    # Init the name of the modality
                    oddReturn[currentRow,2]   = currentXModalities[j]
                    riskReturn[currentRow,2]  = currentXModalities[j]
                    
                    # If this was a weird combination with all 0, init all to -1/1
                    if(currentXBadRows[j] == TRUE){
                        
                        # ODD
                        # Init the estimator
                        oddReturn[currentRow,3]  = -1
                        
                        # Init the confidences
                        oddReturn[currentRow,4]  = -1
                        oddReturn[currentRow,5]  = -1
                        
                        # Init the p-values
                        oddReturn[currentRow,6]  =  1
                        oddReturn[currentRow,7]  =  1
                        oddReturn[currentRow,8]  =  1
                        
                        # RISK
                        # Init the estimator
                        riskReturn[currentRow,3]  = -1
                        
                        # Init the confidences
                        riskReturn[currentRow,4]  = -1
                        riskReturn[currentRow,5]  = -1
                        
                        # Init the p-values
                        riskReturn[currentRow,6]  =  1
                        riskReturn[currentRow,7]  =  1
                        riskReturn[currentRow,8]  =  1
                        
                        badResultCounter = badResultCounter + 1
                        
                    }
                    # Otherwise put the real numbers
                    else{
                        
                        # ODD
                        # Init the estimator
                        oddReturn[currentRow,3]  = oddResults$measure[j-badResultCounter,1]
                        
                        # Init the confidences
                        oddReturn[currentRow,4]  = oddResults$measure[j-badResultCounter,2]
                        oddReturn[currentRow,5]  = oddResults$measure[j-badResultCounter,3]
                        
                        # Init the p-values
                        oddReturn[currentRow,6]  = oddResults$p.value[j-badResultCounter,1]
                        oddReturn[currentRow,7]  = oddResults$p.value[j-badResultCounter,2]
                        oddReturn[currentRow,8]  = oddResults$p.value[j-badResultCounter,3]
                        
                        # RISK
                        # Init the estimator
                        riskReturn[currentRow,3]  = riskResults$measure[j-badResultCounter,1]
                        
                        # Init the confidences
                        riskReturn[currentRow,4]  = riskResults$measure[j-badResultCounter,2]
                        riskReturn[currentRow,5]  = riskResults$measure[j-badResultCounter,3]
                        
                        # Init the p-values
                        riskReturn[currentRow,6]  = riskResults$p.value[j-badResultCounter,1]
                        riskReturn[currentRow,7]  = riskResults$p.value[j-badResultCounter,2]
                        riskReturn[currentRow,8]  = riskResults$p.value[j-badResultCounter,3]                        
                    }
                    
                    # Advance to the next row
                    currentRow = currentRow + 1
                    
                }
                
                
            }
            
        }
        
        # Return
        myReturn = newList(3)
        myReturn[[1]] = marginalsDFs
        myReturn[[2]] = riskReturn
        myReturn[[3]] = oddReturn
        
    }

    return (myReturn)
    
}