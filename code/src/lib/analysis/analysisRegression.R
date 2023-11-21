library(broom) # What an absolute clusterfuck R is, that you need an external library to extract information from lm() >:(


# For a given table with data, and two indexes that are expected to be numerical
# variables, perform a simple regression analysis.
#
# If your data is compose of dates, that is perfectly fine, but you need to convert
# it to numbers manually first, sometimes is better seconds, sometimes is better days,
# and so on. Check the basic tools for transformToNumeric() function.
#
# We have these paremeters with these parameters:
#
# tableBase (dataframe) Where is your data
#
# independentIndex (int) Which column of tableBase do you want to use as independent variable
#
# dependentIndex   (int) Which column of tableBase do you want to use as dependent variable
#
# supressWarnings  (bool) FALSE (default) print verbose information as to why this function is not working
#                         TRUE            don't print warnings
#
# roundFormulas    (int) The function return a list of formulas in string format for each model.
#                        The formulas are made of coefficients that you can modify the rounding by adjusting this number
#                        If you don't like it, you can create your own formula from the model list that is also returned.
#
# The function return:
#
# Error code       (int) 0 = no error, everything went fine
#                        1 = not enough samples in each variable group
#                        2 = not enough samples
#                        3 = all samples are the same
#                        4 = everything went mostly fine, but some of the models
#                            have infinite values somewhere and the lineal model
#                            was use as replacement.
#
# bestModel        (int) Of the following models, which one has the lower p-value
#
# analysisDF       (dataframe) A dataframe with the following information:
#
#
#                   ------------------------------------------------------------
#                       Model                     | R2   |   p-value 
#                   ------------------------------------------------------------
#                       Linear
#                       Quadratic
#                       Inverse
#                       Logarithm
#                       Inverse logarithm
#
# model            (list) The R model with all models described above in the same order
doRegressionAnalysis <- function(tableBase, independentColumnIndex, dependentColumnIndex, supressWarnings = FALSE, roundFormulas = 2){
    
    # Init basic variables
    returnError  = -1
    bestModel    = 0
    analysisDF   = NA
    modelList    = NA
    formulaList  = NA
    
                    
    # Find how many samples we have
    totalSampleIndependent = sum(!is.na(tableBase[,independentColumnIndex]))
    totalSampleDependent   = sum(!is.na(tableBase[,dependentColumnIndex]))    

    # Check if whatever we have is enough to do a analysis                
    {

        # Do we have enough samples?    
        if( totalSampleIndependent >= 2 && totalSampleDependent >= 2){
            
            # Are these samples correlated to each other and do we have enough of them?
            tableComplete = sum((!is.na(tableBase[,independentColumnIndex])) & (!is.na(tableBase[,dependentColumnIndex])))
            
            if(tableComplete >=2 ){
                            
                # Delete the rows where are NAs, we have already check that this doesn't let you with empty sets
                naDependent   = is.na(tableBase[,dependentColumnIndex])
                naIndependent = is.na(tableBase[,independentColumnIndex])
                naRows        = naDependent | naIndependent
                tableBase     = tableBase[!naRows,]

                # Check if all the values are the same
                varIndependent = var(tableBase[,independentColumnIndex])
                varDependent   = var(tableBase[,dependentColumnIndex])
                
                # ERROR: 3 - All the values are the same
                if(varIndependent == 0 || varDependent == 0){
                    
                    returnError  = 3
                    
                    if(supressWarnings == FALSE){
                                
                        print("-------------------------------------------------------")
                        print("I can't make the regression of these two variables")
                        print("Because all datapoints are the same")
                        print("")
                        print("For example:")
                        print("  A  |  B  |")
                        print("  1  |  3  |")
                        print("  1  |  3  |")                        
                        print("  1  |  3  |")
                        print("  1  |  3  |")                                            
                        print("-------------------------------------------------------")
                        print("Columns:")
                        print(paste("independent:",independentColumnIndex))
                        print(paste("dependent:"  ,dependentColumnIndex))
                        print("-------------------------------------------------------")
                        
                    }                            
                    
                }
               
                # ERROR: 0 - Everything is ok and we can do regression
                else{
                    
                    returnError  = 0
                    
                    # Init the return dataframe and the list
                    analysisDF  = DF(5,3)
                    modelList   = newList(5)
                    formulaList = newList(5)
                    currentBest = 0
                    colnames(analysisDF) = c("Model", "R2", "p-value")
                    
                    # ---- Lineal model
                    # -------- Name
                    analysisDF[1,1] = "Lineal"
                    # -------- Model
                    modelList[[1]]   = lm(tableBase[,dependentColumnIndex] ~ tableBase[,independentColumnIndex] , tableBase)                    
                    # -------- R2
                    analysisDF[1,2]  = as.numeric(glance(modelList[[1]])$r.squared)
                    # -------- P-value
                    analysisDF[1,3]  = as.numeric(glance(modelList[[1]])$p.value)
                    # -------- Formula
                    formulaList[[1]] = extractFormula("Linear", modelList[[1]], roundMe = roundFormulas)
                    # -------- So far, this is the best result
                    bestModel   = 1
                    currentBest = analysisDF[1,3]
                    
                    #print("Lineal")
                    
                    
                    
                    # ---- Quadratic model
                    analysisDF[2,1] = "Quadratic"
                    modelList[[2]]  = lm(tableBase[,dependentColumnIndex] ~ tableBase[,independentColumnIndex] + I(tableBase[,independentColumnIndex]^2), tableBase) # R is completely FUBAR, Because ^ is one of the "symbolic" operators used by formula, it will not be interpreted numerically without the protection by I
                    analysisDF[2,2] = as.numeric(glance(modelList[[2]])$r.squared)
                    analysisDF[2,3] = as.numeric(glance(modelList[[2]])$p.value)
                    formulaList[[2]] = extractFormula("Quadratic", modelList[[2]], roundMe = roundFormulas)
                    
                    if( analysisDF[2,3] < currentBest){
                    
                        bestModel   = 2  
                        currentBest = analysisDF[2,3]
                        
                    }
                    
                    #print("Quadratic")
                    
                    # ---- Inverse
                    analysisDF[3,1] = "Inverse"
                    # ---- ---- Be careful that you are not pulling an infinite number somewhere
                    independentValues = 1/tableBase[,independentColumnIndex]
                    totalInfinite     = sum(is.infinite(independentValues))
                    if( totalInfinite == 0){
                        modelList[[3]]  = lm(tableBase[,dependentColumnIndex] ~ I(1/tableBase[,independentColumnIndex]), tableBase)
                        analysisDF[3,2] = as.numeric(glance(modelList[[3]])$r.squared)
                        analysisDF[3,3] = as.numeric(glance(modelList[[3]])$p.value)
                        formulaList[[3]] = extractFormula("Inverse", modelList[[3]], roundMe = roundFormulas)
                        
                        if( analysisDF[3,3] < currentBest){
                        
                            bestModel   = 3  
                            currentBest = analysisDF[3,3]
                            
                        }
                    }
                    # ---- ---- If we are, just copy the lineal model here
                    else{
                        
                        modelList[[3]]   = modelList[[1]]
                        analysisDF[3,2]  = 0
                        analysisDF[3,3]  = 1
                        formulaList[[3]] = formulaList[[1]]
                        
                        returnError  = 4
                        
                    }
                    
                    #print("Inverse")
                    
                    # ---- Logarithm
                    analysisDF[4,1] = "Logarithm"
                    # ---- ---- Be careful that you are not pulling an infinite number somewhere
                    independentValues = log(tableBase[,independentColumnIndex])
                    totalInfinite     = sum(is.infinite(independentValues))
                    totalNaN          = sum(is.nan(independentValues))
                    
                    
                    if( (totalNaN + totalInfinite) == 0){
                        
                        modelList[[4]]  = lm(formula = tableBase[,dependentColumnIndex] ~ log(tableBase[,independentColumnIndex]), tableBase)
                        analysisDF[4,2] = as.numeric(glance(modelList[[4]])$r.squared)
                        analysisDF[4,3] = as.numeric(glance(modelList[[4]])$p.value)
                        formulaList[[4]] = extractFormula("Logarithm", modelList[[4]], roundMe = roundFormulas)
                        
                        if( analysisDF[4,3] < currentBest){
                        
                            bestModel   = 4  
                            currentBest = analysisDF[4,3]
                            
                        }
                    
                    }
                    # ---- ---- If we are, just copy the lineal model here
                    else{
                        
                        modelList[[4]]   = modelList[[1]]
                        analysisDF[4,2]  = 0
                        analysisDF[4,3]  = 1
                        formulaList[[4]] = formulaList[[1]]
                        
                        returnError  = 4
                        
                    }                    
                    
                    #print("Log")
                    
                    # ---- Inverse logarithm
                    analysisDF[5,1] = "Inverse logarithm"
                    # ---- ---- Be careful that you are not pulling an infinite number somewhere
                    independentValues = 1/log(tableBase[,independentColumnIndex])
                    totalInfinite     = sum(is.infinite(independentValues))
                    totalNaN          = sum(is.nan(independentValues))
                    if( (totalNaN + totalInfinite) == 0){
                        
                        modelList[[5]]  = lm( tableBase[,dependentColumnIndex] ~ I(1/log(tableBase[,independentColumnIndex])), tableBase)
                        analysisDF[5,2] = as.numeric(glance(modelList[[5]])$r.squared)
                        analysisDF[5,3] = as.numeric(glance(modelList[[5]])$p.value)
                        formulaList[[5]] = extractFormula("Inverse logarithm", modelList[[5]], roundMe = roundFormulas)                        
                        
                        if( analysisDF[5,3] < currentBest){
                    
                            bestModel   = 5
                            currentBest = analysisDF[5,3]
                        
                        }                         
                        
                    } 
                    # ---- ---- If we are, just copy the lineal model here
                    else{
                        
                        modelList[[5]]   = modelList[[1]]
                        analysisDF[5,2]  = 0
                        analysisDF[5,3]  = 1
                        formulaList[[5]] = formulaList[[1]]
                        
                        returnError  = 4
                        
                    }
                              
                }
                
                            
            }
            # ERROR: 1 - Not enought indpendent samples
            else{
                
                returnError  = 1                
                
                if(supressWarnings == FALSE){
                                
                    print("-------------------------------------------------------")
                    print("I can't make the regression of these two variables")
                    print("There are enought samples in each variable group, but they don't coincide in the same row")
                    print("")
                    print("For example:")
                    print("  A  |  B  |")
                    print(" Na  |  1  |")
                    print(" Na  |  2  |")
                    print("  3  |  Na  |")
                    print("  4  |  Na  |")
                    print("-------------------------------------------------------")
                    print("Columns:")
                    print(paste("independent:",independentColumnIndex))
                    print(paste("dependent:"  ,dependentColumnIndex))
                    print("Total data found:")
                    print(totalSampleIndependent)
                    print(totalSampleDependent)
                    print("-------------------------------------------------------")
                }                
                
            }
                                    
            
        }
        # ERROR 2 - Not enough samples
        else{

            returnError  = 2
            
            if(supressWarnings == FALSE){
                         
                print("-------------------------------------------------------")
                print("I can't make the regression of these two variables")
                print("I don't have enough samples, need at least 2 for each variable")
                print("-------------------------------------------------------")
                print("Columns:")
                print(paste("independent:",independentColumnIndex))
                print(paste("dependent:"  ,dependentColumnIndex))
                print("Total data found:")
                print(totalSampleIndependent)
                print(totalSampleDependent)
                print("-------------------------------------------------------")   
                            
            }

        }
            
    }
    
    # Return the whole analysis thing
    toReturn      = newList(5)
    toReturn[[1]] = returnError
    toReturn[[2]] = bestModel
    toReturn[[3]] = analysisDF
    toReturn[[4]] = modelList
    toReturn[[5]] = formulaList
    
    
    return(toReturn)
    
}


# R is a stupid language, models don't have information about the type of model that you are using
# you need to parse the formula to understand what you are doing, instead of having a simple modelType = "Whatever"
#
# From a given model, get the formula for that model in string format.
#
# modelType (string)    Linear
#                       Quadratic
#                       Inverse
#                       Logarithm
#                       Inverse logarithm
#
# myRModel (object) The typical lm() model you get in R
#
# roundMe (int) for the formula, use round numbers
extractFormula <- function(modelType, myRModel, roundMe = 0){

    myFormula = ""
    
    # y = mx+b
    if(modelType == "Linear"){
        coefficientB = summary(myRModel)$coefficients[1,1]
        coefficientM = summary(myRModel)$coefficients[2,1]
        
        if(roundMe > 0) coefficientB = round(coefficientB, roundMe)
        if(roundMe > 0) coefficientM = round(coefficientM, roundMe)
        
        
        if(coefficientB < 0)
        	myFormula = paste0("y = ",coefficientM,"x ",coefficientB)
        else
        	myFormula = paste0("y = ",coefficientM,"x + ",coefficientB)
        
    }
    
    # y = ax² + bx + c
    if(modelType == "Quadratic"){
        
        coefficientA = summary(myRModel)$coefficients[3,1]
        coefficientB = summary(myRModel)$coefficients[2,1]
        coefficientC = summary(myRModel)$coefficients[1,1]
        
        if(roundMe > 0) coefficientA = round(coefficientA, roundMe)
        if(roundMe > 0) coefficientB = round(coefficientB, roundMe)
        if(roundMe > 0) coefficientC = round(coefficientC, roundMe)
        
        
        if(coefficientB < 0)
        	myFormula = paste0("y = ",coefficientA,"x^2 ",coefficientB,"x + ",coefficientC)
        else
        	myFormula = paste0("y = ",coefficientA,"x^2 + ",coefficientB,"x + ",coefficientC)
        
        
        
    }    
    # y = 1/mx + b
    if(modelType == "Inverse"){
        coefficientB = summary(myRModel)$coefficients[1,1]
        coefficientM = summary(myRModel)$coefficients[2,1]
        coefficientM = 1/coefficientM
        
        if(roundMe > 0) coefficientB = round(coefficientB, roundMe)
        if(roundMe > 0) coefficientM = round(coefficientM, roundMe)
        
        #myFormula = paste0("y = 1/",coefficientM,"x + ",coefficientB)
        
        
        if(coefficientB < 0)
        	myFormula = paste0("y = ",coefficientM,"x ",coefficientB)
        else
        	myFormula = paste0("y = ",coefficientM,"x + ",coefficientB)
        
        
        
    }    
    
    # y = m*log(x) + b
    if(modelType == "Logarithm"){
        coefficientB = summary(myRModel)$coefficients[1,1]
        coefficientM = summary(myRModel)$coefficients[2,1]
        
        if(roundMe > 0) coefficientB = round(coefficientB, roundMe)
        if(roundMe > 0) coefficientM = round(coefficientM, roundMe)
        
        if(coefficientB < 0)
        	myFormula = paste0("y = ",coefficientM,"*log(x) ",coefficientB)
        else
        	myFormula = paste0("y = ",coefficientM,"*log(x) + ",coefficientB)
        
        
    }        
    
    # y = 1/(mlog(x) + b)
    if(modelType == "Inverse logarithm"){
        coefficientB = summary(myRModel)$coefficients[1,1]
        coefficientM = summary(myRModel)$coefficients[2,1]
        coefficientM = 1/coefficientM
        
        if(roundMe > 0) coefficientB = round(coefficientB, roundMe)
        if(roundMe > 0) coefficientM = round(coefficientM, roundMe)
        
        #myFormula = paste0("y = 1/(",coefficientM,"log(x)) + ",coefficientB)
        
        
        if(coefficientB < 0)
        	myFormula = paste0("y = ",coefficientM,"log(x) ",coefficientB)
        else
        	myFormula = paste0("y = ",coefficientM,"log(x) + ",coefficientB)
        
        
    }            
    
    return(myFormula)

}


# Multivariable logistic regression analysis
#
# This function takes data in this format:
#
#  Numerical 1 | Numerical 2 | ... | Numerical N | Categorical
# -------------------------------------------------------------
#     23             34                 12            Type A
#      0.4            0.1                1.2          Type B
#      2              7                  8            Type C
#
# And makes a model classifier.
#
# Training ratio
#
# Return the model and shit
#
# https://www.r-bloggers.com/2020/05/multinomial-logistic-regression-with-r/
                    



#https://www.statmethods.net/advstats/glm.html
#Generalized Linear Models
#Generalized linear models are fit using the glm( ) function. The form of the glm function is

#glm(formula, family=familytype(link=linkfunction), data=)

#Family	Default Link Function
#binomial	(link = "logit")
#gaussian	(link = "identity")
#Gamma	(link = "inverse")
#inverse.gaussian	(link = "1/mu^2")
#poisson	(link = "log")
#quasi	(link = "identity", variance = "constant")
#quasibinomial	(link = "logit")
#quasipoisson	(link = "log")
#See help(glm) for other modeling options. See help(family) for other allowable link functions for each family. Three subtypes of generalized linear models will be covered here: logistic regression, poisson regression, and survival analysis.

# Logit regression for two columns with numerical values
# Usually for only 0/1 values in the y data, but you can use whathever you want
# and use a continous data too.
#
# The function needs
#
#     tableBase (Dataframe)  : dataframe with the data
#
#     xIndex    (int)        : index with data for x axys
#
#     yIndex    (int)        : index with data for y axys
#
# Return the model things
doLogitAnalysis <- function(tableBase, xIndex, yIndex){
    
    myModel = glm(tableBase[,yIndex] ~ tableBase[,xIndex], family=binomial(link="logit"))
    
    myE    = as.numeric(myModel$coefficients[1])
    myBeta = as.numeric(myModel$coefficients[2])
    
    myReturn = newList(3)
    myReturn[[1]] = myModel
    myReturn[[2]] = myE
    myReturn[[3]] = myBeta
    
    return(myReturn)

    if(FALSE){
    
            ccc = myHfunction(seq(10,0,-1))

            averageC = 0
            for(i in 1:(length(ccc)-1)){
    
                difference = ccc[i] - ccc[i+1] 
                averageC = averageC + difference
    
            }
            averageC = averageC / (length(ccc)-1)
  
  
            # Find CI for increase probability by numbers of friends
            {
    
                ccc2 = ccc
                for(i in 1:(length(ccc)-1)){

                    ccc2[i] = ccc[i] - ccc[i+1] 
            
                }
                ccc2 = ccc2[1:length(ccc2)-1]
        
                getCI(ccc2)
        
            }   
            
    }
    
}


# Prepare the exponential functions
logitFunction <- function(x,e,b){
    return(1/(1 + exp(-(b*x+e) ) ) )
}
                    

# This function takes a table with two variables
#
#     yIndex is the categorical column with the output. It needs to have only two categories
#
#     xIndexes is a list with any other categorical variable that you want to analyise.
#
# The function transform the xIndexes into dummy variables automatically.
# It will gives you a regression model based on logistic regression for that
# particular set of variables
#
# Consider using regression for mixed models if you have too much categorical data
# and it has no ordinal meaning (sex: men, women ; country: Spain, Norway, Germany, Italy ; etc...)
doMulticategoricalRegression <- function(tableBase, yIndex, xIndexes){
    
}


# Do PCR using a table with INDEXES, not the stupid names that R use all the time
doPCRanalysis <- function(tableBase, yIndex, xIndexes, ncomp = 2, training = 0.7){
	
	#Create the table that you want to analyze and throw away the rest
	myTable = tableBase
	myTable = myTable[,c(yIndex, xIndexes)]
	
	# For each column, find the NA, STUPID NA R VALUES THAT FUCK EVERYTHING, and
	# change them for the average of that column
	for(j in 1:ncol(myTable)){
	
		# Find the average
		currentAverage = mean(myTable[,j], na.rm = TRUE)
		
		# For each NA in that column, replace for the average
		myTable[is.na(myTable[,j]),j] = currentAverage
		
	}
	
	
	# Set the Y variable
	colnames(myTable)[1] = "y"
	
	# Create the model
	myModel = pcr(formula = y~ ., data = myTable, scale=TRUE, validation="CV")
	
	# split the data
	sampleSize = floor(training*nrow(myTable))
	pickThese  = sample(seq_len(nrow(myTable)),size = sampleSize)
	trainDF    = myTable[pickThese,]
    testDF     = myTable[-pickThese,]

	# Make predictions
    myTrainModel = pcr(formula = y~ ., data = trainDF, scale=TRUE, validation="CV")
	PCRValues    = predict(myTrainModel, testDF, ncomp = ncomp)

	#calculate RMSE
	RMSE = mean( sqrt(( PCRValues - testDF[,1])^2 )   )
    SSE  = sd(   sqrt(( PCRValues - testDF[,1])^2 )   )

	# Return
    myReturn = vector("list", length = 3)
    myReturn[[1]] = myModel
    myReturn[[2]] = RMSE
    myReturn[[3]] = SSE

    return (myReturn)

}
