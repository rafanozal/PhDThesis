library(ggplot2)
library(rcompanion)   # For the multiple logistic regression (https://cran.r-project.org/src/contrib/Archive/rcompanion/)

# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("./tools/toolsBasic.R",       encoding="utf-8")
source("./tools/toolsNetworks.R",    encoding="utf-8")
source("./tools/toolsLatex.R",       encoding="utf-8")
source("./tools/toolsSummarizers.R", encoding="utf-8")
source("./tools/toolsPlotting.R",    encoding="utf-8")



# ---------------------------------
#     ONE VARIABLE ANALYSIS
# ---------------------------------

# For a given vector of numbers return the CI for a given alpha (typically 95%)
#
# Return (low interval, average, high interval)
getCI <- function(myVector, alpha = 0.95){

        # I'm tired of the R logic, always poisoning the data with it stupid
        # NA policies
        myVector = myVector[!is.na(myVector)]
    
        #input sample size, sample mean, and sample standard deviation
        n    = length(myVector)
        xbar = mean(myVector)
        s    = sd(myVector)

        #calculate margin of error
        margin = qt(  (1-((1-alpha)/2)) , df = n - 1) * s / sqrt(n)

        #calculate lower and upper bounds of confidence interval
        low  = xbar - margin
        high = xbar + margin    
        
        return(c(low,xbar,high))
    
}

# For a given vector that is a sample, find the population standard deviation
getPopulationSD <- function(myVector){
    
    myVector = myVector[!is.na(myVector)]
    
    return (sqrt((length(myVector)-1)/length(myVector)) * sd(myVector))
}
    
    
    


# For a given table and column, return the type of variable that thiis column contain.
getColumnType <- function(tableBase, index){}

# Gives you an automatic analysis of one variable

# Gives you the automatic analysis of one variable that is numerical
# Cullen and Frey graph here
# Gives you the automatic analysis of one variable that is categorical
# Gives you the automatic analysis of one variable that is date

# ---------------------------------
#     MULTIPLE VARIABLES ANALYSIS
# ---------------------------------
{

  # ---------------------------------
  #     TWO CATEGORICAL ANALYSIS
  # ---------------------------------
  {
    

 
  }
  
 
  # ---------------------------------------------------------
  #     ONE CATEGORICAL ONE NUMERICAL VARIABLE
  # ---------------------------------------------------------
  {

     

    # ---------------------------------
    #          HISTOGRAM
    # ---------------------------------
    {



    }

    # ---------------------------------
    #          DENSITY
    # ---------------------------------
    {
      doCategoricalDensityPlot <- function(tableBase, variableIndex, categoryIndex, plotFilePath = NULL,
                                           plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                           colorsVector = NULL, plotTheme = NULL,
                                           borderPlot = FALSE, rotatePlot = FALSE,
                                           overrideTableName = NULL,
                                           overrideCaption = NULL){

        # Init variables
        {
          myPlotType = "CategoricalDensity"
          myTableName = deparse(substitute(tableBase))
          
          if(!is.null(overrideTableName)){
            myTableName = overrideTableName
            
          }          
        }

        # Get an automatic name if you don't have a proper one
        {
          
          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = categoryIndex, variableIndex2 = variableIndex)
          
        }

        # Get the theme information
        themeData = getThemeParameters(plotTheme)
        
        # Get info about different categories
        {
          myCategories  = unique(tableBase[,categoryIndex])
          nCategories   = length(myCategories)
          groupingName  = colnames(tableBase)[categoryIndex]
          numericalName = colnames(tableBase)[variableIndex]
          totalRows     = nrow(tableBase)          
        }
        
        
        # ---- Prepare the defaults
        {
          
          defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                          colorsVector = colorsVector, plotTitle   = plotTitle,
                                                          plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                          plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                          fileType = myPlotType)
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }

        # Do the plot
        {
          
          densityPlot = ggplot(data = tableBase, aes(x = tableBase[,variableIndex], fill = tableBase[,categoryIndex])) +
            
            # Add the density plot
            geom_density(alpha=0.8) +
            scale_fill_manual(values=colorsVector) +
            
            # Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 color    = groupingName,
                 fill     = groupingName,
                 x = plotXLabel, y = plotYLabel) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]],
                  legend.position    = themeData[[5]])

          # Something about the border
          if(borderPlot == TRUE)
            densityPlot = densityPlot + border()
          
          # Rotate the plot if asked
          if(rotatePlot == TRUE )
            densityPlot = densityPlot + coord_flip()
          
        }
        
        # Save the image
        imageWidth = 8
        ggsave(imageFilePath, plot = densityPlot, width = imageWidth)
        latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
        
        # Return
        myReturn = vector("list", length = 3)
        myReturn[[1]] = densityPlot
        myReturn[[2]] = imageFilePath
        myReturn[[3]] = latexFilePath
        
        return (myReturn)
        
      }  
      
    }

    # ---------------------------------
    #          SPECIAL
    # ---------------------------------
    {
      # ---- BMI for each grouping
      doBMIPlot <- function(tableBase, BMIindex, groupIndex, plotFilePath,
                            colorsVector = NULL,
                            plotTitle = NULL, plotSubtitle = NULL,
                            plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                            plotTheme = NULL,
                            overrideTableName = NULL,
                            overrideCaption   = NULL){

        # Init variables
        myPlotType = "BMIPlot"
        myTableName = deparse(substitute(tableBase))

        if(!is.null(overrideTableName)){
          myTableName = overrideTableName

        }

        # Get an automatic name if you don't have a proper one
        {
          
          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = groupIndex, variableIndex2 = BMIindex)
          
        }

        # Get the theme information
        themeData = getThemeParameters(plotTheme)

        # Get info about different categories
        {
          myCategories  = unique(tableBase[,groupIndex])
          nCategories   = length(myCategories)
          groupingName  = colnames(tableBase)[groupIndex]
          numericalName = "BMI"
          totalRows     = nrow(tableBase)          
        }


        # ---- Prepare the defaults
        {

          defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                          colorsVector = colorsVector, plotTitle   = plotTitle,
                                                          plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                          plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                          fileType = myPlotType)
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }
        
        # ---- Find the maximum density
        myMaxDensity = max(density(tableBase[,BMIIndex], na.rm=TRUE)$y)

        for (i in 1:nCategories) {

          subTable      = tableBase[tableBase[,groupIndex] == myCategories[i],]
          subMaxDensity = max(density(subTable[,BMIIndex], na.rm=TRUE)$y)
          myMaxDensity  = max(myMaxDensity, subMaxDensity)

        }

        lowerCoordinate = myMaxDensity * -0.02
        middleCoordinate = lowerCoordinate/2

        # Do the plot
        {
          myBMIPlot = ggplot(tableBase, aes(x = tableBase[,BMIIndex],  fill = tableBase[,groupIndex]), colour = "Black") +

                             # Background rectangles
                             geom_rect(xmin=0,    xmax=18.5, ymin=-Inf, ymax=Inf, fill = "#ffffbb", color="black", alpha=0.05) +
                             geom_rect(xmin=18.5, xmax=25,   ymin=-Inf, ymax=Inf, fill = "#bbff9e", color="black", alpha=0.05) +
                             geom_rect(xmin=25,   xmax=30,   ymin=-Inf, ymax=Inf, fill = "#ffe3bb", color="black", alpha=0.05) +
                             geom_rect(xmin=30,   xmax=Inf,  ymin=-Inf, ymax=Inf, fill = "#ffbbbb", color="black", alpha=0.05) +

                             # Labels for the rectangles
                             geom_text(x = 14.5, y = middleCoordinate, label="Underweight", size=4, color="black", vjust = 1.5) +
                             geom_text(x = 21.5, y = middleCoordinate, label="Healthy",     size=4, color="black", vjust = 1.5) +
                             geom_text(x = 27.5, y = middleCoordinate, label="Overweight",  size=4, color="black", vjust = 1.5) +
                             geom_text(x = 33,   y = middleCoordinate, label="Obese",       size=4, color="black", vjust = 1.5) +

                             # Do a density plot
                             geom_density(alpha = 0.6) +

                             # Specify colors
                             scale_color_manual(values = colorsVector, na.value = COLOR_NA) +
                             scale_fill_manual(values  = colorsVector, na.value = COLOR_NA) +

                             # Set the limits for the IMC
                             xlim(12, 35) +
                             ylim(lowerCoordinate , myMaxDensity * 1.1) +

                             # Create titles and subtitles
                             labs(title    = plotTitle,
                                  subtitle = plotSubtitle,
                                  caption  = plotCaption,
                                  color    = groupingName,
                                  fill     = groupingName,
                                  x = plotXLabel, y = plotYLabel) +

                            # Apply the theme
                            theme(panel.background   = themeData[[1]],
                                  axis.line          = themeData[[2]],
                                  panel.grid.major.y = themeData[[3]],
                                  panel.grid.major.x = themeData[[4]])
        }

        # Save the image
        imageWidth    = 8
        ggsave(imageFilePath, plot = myBMIPlot, width = imageWidth)
        latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
        
        # Return
        myReturn = vector("list", length = 3)
        myReturn[[1]] = myBMIPlot
        myReturn[[2]] = imageFilePath
        myReturn[[3]] = latexFilePath
        
        return (myReturn)

      }
    }

  }


  # ---------------------------------------------------------
  #     MULTIPLE CATEGORICAL AND MULTIPLE NUMERICAL
  #
  #     If your dependent variable is continuous (and the residuals are normally distributed),
  #     but all of your independent variables are categorical, this is just an ANOVA.
  #
  #     If your dependent variable is categorical and your independent variables are continuous,
  #     this would be logistic regression (possibly binary, ordinal, or multinomial, depending).
  #
  #     If both your dependent variable and your independent variables are categorical variables,
  #     you can still use logistic regression—it's kind of the ANOVA-ish version of LR.

  
  # ---------------------------------------------------------
  {
    
    # This function gets a bunch of explicative variables.
    # Those which are categorical are transformed into independent dummy
    # variables ie:
    #
    #            Age Sex
    #            18  Female
    #            48  Male
    #            20  Male
    #
    #            Age Sex_Female Sex_Male
    #             18          1        0
    #             48          0        1
    #             20          0        1
    #
    # explicativeVariableDF is a dataframe with the variables that you want to use
    #
    # depedentVariableVector is the resulting vector (it doesn't matter if it is categorical or not)
    #
    # deleteUnknowns (string) If you have categories that has unknown strings to
    #                         notate unknown category ie:
    #
    #                                 BMI
    #                             -----------
    #                             Underweight
    #                             Overweight
    #                             Unknown
    #                             Healthy
    #                             Healthy
    #                 
    #                         And you want to exclude them from analysis, then
    #                         gives here whatever string you have use to notate
    #                         the unknown categories ie "Unknown".
    #
    # maxCombos (int) Is how many possible combinations of variables do you want
    #                 to try automatically. If the total possible combinations
    #                 Is to high, the function will stop, and you will get some
    #                 clues as to what you should delete to make the model
    #                 more handy
    multipleLogisticRegression <-function(explicativeVariablesDF, dependentVariableVector, deleteUnknowns = NULL, maxCombos = 100000){
      
      # Get the dimensions
      totalColumns = ncol(explicativeVariablesDF)
      totalRows    = nrow(explicativeVariablesDF)
      
      # First we need to figure it out which variables are categoricals and which one isn't
      categoricalDataVector = getCategoricalVariables(explicativeVariablesDF)
        
      # Get a DF with only the categorical values if any, and transform it to dummy variables
      # Add it to the explicative DF and delete the old variables
      totalCategorical = sum(categoricalDataVector)
      if(totalCategorical > 0){
        
        # Create the dummy DF
        categoricalDF               = explicativeVariablesDF[,categoricalDataVector]
        dummyExplicativeVariablesDF = fastDummies::dummy_cols(categoricalDF, remove_most_frequent_dummy = TRUE)
        dummyExplicativeVariablesDF = dummyExplicativeVariablesDF[,-c(1:totalCategorical)]

        # If the user wants to, we must delete the unknown columns from the analysis
        if(!is.null(deleteUnknowns)){
          
          unknownColumns = grep(deleteUnknowns, colnames(dummyExplicativeVariablesDF))
          dummyExplicativeVariablesDF = dummyExplicativeVariablesDF[,-c(unknownColumns)]
          
        }
        
        # Add a single column to the explicative variables to ensure that we
        # don't delete the entire DF ( I hate you R ), when we delete the
        # categorical columns
        explicativeVariablesDF$IHateYouR = 0
        
        # Delete the old categorical variables, whatever is left is only
        # numerical variables (if any)
        explicativeVariablesDF = explicativeVariablesDF[,!categoricalDataVector]
        
        # Add it to the original DF with all the variables
        explicativeVariablesDF = cbind(explicativeVariablesDF,dummyExplicativeVariablesDF)
      
        # Delete the extra column
        explicativeVariablesDF$IHateYouR = NULL
        
        
      }

      # From here we only have numerical values in the DF
      explicativeVariablesDF$Target = dependentVariableVector
      logisticReadyDF               = explicativeVariablesDF #alias, so I don't have to rewrite variables names

      # Get the new updated dimensions
      totalColumns = ncol(logisticReadyDF)
      totalExplicativeVariables = totalColumns - 1

      # Clean the rows that has NA values if any
      {
        keepTheseRows = rep(TRUE, totalRows)
        for (i in 1:totalColumns){
          keepTheseRows = keepTheseRows & (!is.na(logisticReadyDF[,i]))
          
        }
        logisticReadyDF  = logisticReadyDF[keepTheseRows,]
      }

      # Get the new updated dimensions
      totalRows    = nrow(logisticReadyDF)
      
      # We need to clean the columns names and clean any weird character
      # because R hates us, and it also hates proper programming
      currentNames = colnames(logisticReadyDF)
      newNames     = cleanWeirdCharacters(currentNames)
      colnames(logisticReadyDF) = newNames
      
      # Create the formula for the GLM programatically (stupid R syntax -_- )
      {
        valid.names = names(logisticReadyDF)[names(logisticReadyDF) != "Target"]  # all but group

        formulaString = "Target ~ "
        for(i in 1:length(valid.names)){
          formulaString = paste0(formulaString, " ", valid.names[i], " +") 
        }
        formulaString = substr(formulaString,1,nchar(formulaString)-1)
        myFormula = as.formula(formulaString)
      }
      
      # We are going to try to male all possible models if possible.
      totalBruteRows = (2^totalExplicativeVariables) - 1
      totalBruteCols = totalExplicativeVariables + 3
      
      # Show the step by step and full model summary
      {
        print(" Step by step procedure results ")
        
        model.null = glm(Target ~ 1,
                         data   = logisticReadyDF,
                         family = binomial(link="logit")
        )
        
        model.full = glm(myFormula,
                         data   = logisticReadyDF,
                         family = binomial(link="logit")
        )
        
        step(model.null,
             scope     = list(upper=model.full),
             direction = "both",
             test      = "Chisq",
             data      = logisticReadyDF
        )
        
        print(summary(model.full))      
      }

      # If we have too much models
      if(totalBruteRows > maxCombos){
        
        print("--------------------------------------")
        print("You have way to many possible combination of variables to try this analytically")
        print("")
        print(paste0("Total possible combinations = ", totalBruteRows))
        print("")
        print("Try the summary displayed above to restrict your variables a bit")
        print("--------------------------------------")
       

    }
      
      # If the size is enough, keep going
      else{
        
        print("Else")
        
        # Show the base model with all the columns
        model.full = glm(myFormula,
                         data   = logisticReadyDF,
                         family = binomial(link="logit")
        )
        
        print(summary(model.full))

        # Try all possible combination of models with the given variables
        # Evaluate the model with pseudo R2
        {
          
          # Keep track of the best scores
          bestMcFadden   = -99
          bestCox        = -99
          bestNagelkerke = -99
          
          bestModelDF    = data.frame(matrix(FALSE, nrow = totalBruteRows, ncol = totalBruteCols))
          colnames(bestModelDF) = c(valid.names, "McFadden", "Cox", "Nagelkerke")

          # For each of the possible brute force combinations
          for(i in 1:totalBruteRows){
            
            myCurrentBinary = number2binary(i, totalExplicativeVariables)
            
            # Mark the columns that you are using and init the pseudo R² to -99
            # The -99 means nothing, is just a value to write something in the DF
            # and update it later
            bestModelDF[i,] = c(myCurrentBinary, -99, -99, -99)
            
            # Make the formula and evaluate the model
            currentFormulaString = "Target ~ "
            for(j in 1:totalExplicativeVariables){
              # If it is a valid variable
              if(bestModelDF[i,j] == 1){
                currentFormulaString = paste0(currentFormulaString, " ", valid.names[j], " +")   
              }
            }

            # Delete the last  "+"
            currentFormulaString = substr(currentFormulaString,1,nchar(currentFormulaString)-1)
            myFinalFormula = as.formula(currentFormulaString)
            
            print("Before Full")
            print(myFormula)
            print(myFinalFormula)
            
            # Show the base model with all the columns
            model.full = glm(myFormula,
                             data   = logisticReadyDF,
                             family = binomial(link="logit")
            )
            
            # Make the model
            model.current = glm(formula = myFinalFormula,
                                data    = logisticReadyDF,
                                family  = binomial(link="logit"))
            
            modelEvaluation = nagelkerke(model.current)
            
            bestModelDF[i,(totalBruteCols - 2)] = modelEvaluation$Pseudo.R.squared.for.model.vs.null[1]
            bestModelDF[i,(totalBruteCols - 1)] = modelEvaluation$Pseudo.R.squared.for.model.vs.null[2]
            bestModelDF[i, totalBruteCols     ] = modelEvaluation$Pseudo.R.squared.for.model.vs.null[3]
            
          }
          
          # Update the score of the best model
          bestMcFadden   = max(bestModelDF$McFadden)
          bestCox        = max(bestModelDF$Cox)
          bestNagelkerke = max(bestModelDF$Nagelkerke)
          
          
          print(bestMcFadden)
          print(bestCox)
          print(bestNagelkerke)
          
        }
        
      }
      

    }

    # # Get a dependent variable Vector
    # dependentVariable = completeTable$C_NasalCarrier
    # # dependentVariable = as.character(completeTable$C_NasalCarrier)
    # # dependentVariable = dependentVariable[dependentVariable == "Positive"] = 1
    # # dependentVariable = dependentVariable[dependentVariable == "Negative"] = 0
    # 
    # # Prepare a dataframe with all the explicative variables
    # explicativeDF = completeTable[,c(importantNumericalIndexes, importantCategoricalIndexes)]
    # # Remove the BMI Categorical, because BMI is better
    # explicativeDF$BMICategorical = NULL
    # 
    # # Best model
    # # explicativeDF = completeTable[,c(sexIndex, sportsIndex)]
    # 
    # # # Make the numbers for the model and delete Unknowns
    # # explicativeDF$SexNumerical    = 0
    # # explicativeDF$SportsNumerical = 0
    # # explicativeDF$SexNumerical[explicativeDF$Sex       == "Man"]    = 1
    # # explicativeDF$SportsNumerical[explicativeDF$Sports == "Light"]  = 1
    # # explicativeDF$SportsNumerical[explicativeDF$Sports == "Medium"] = 1
    # # explicativeDF$SportsNumerical[explicativeDF$Sports == "Hard"]   = 2
    # # 
    # # # Delete the stuff from the dependent vector
    # # dependentVariable = dependentVariable[c(explicativeDF$Sports != "Unknown")]
    # # # Delete the stuff from the explicative variables
    # # # -- Unknowns
    # # explicativeDF = explicativeDF[explicativeDF$Sports != "Unknown",]
    # # # --Delete the not numerical columns
    # # explicativeDF$Sex    = NULL
    # # explicativeDF$Sports = NULL
    # 
    # #multipleLogisticRegression(explicativeDF, dependentVariable)
    # multipleLogisticRegression(explicativeDF, dependentVariable, deleteUnknowns="Unknown")
    
  }

}

# ---------------------------------
#     OTHER ANALYSIS
# ---------------------------------
{

  # ---------------------------------
  #     GRAPHS and NETWORKS
  # ---------------------------------
  {


  
    # Reachability plot
    # A bunch of steps to see how is the coverage of your network after that many steps
    doReachabilityPlot <- function(edgesDF, nodesDF, plotFilePath, directedPlot = FALSE, totalSteps = 15,
                                   plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                   plotTheme = NULL,
                                   overrideTableName = NULL,
                                   overrideCaption = NULL,
                                   ymin = NULL, ymax = NULL){


      # Define plot type
      myPlotType = "RechabilityBoxplot"
      myTableName = deparse(substitute(edgesDF))
      
      # If you need to override your table name, then do it
      if(!is.null(overrideTableName)){
        myTableName = overrideTableName
        
      }

      # Get the theme information
      themeData = getThemeParameters(plotTheme)
      
      # Create the graph objects and check for distances
      myGraph      = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot)
      distanceMap  = distances(myGraph)

      # Get how many nodes we have
      totalNodes   = nrow(nodesDF)

      # We need to create a table like this to make the boxplots:
      # -----------------------------------
      # Node ID \ Steps \ Coverage \
      #    A        1       10%
      #    A        2       15%
      #    B        1        7%
      #    C        3       25%
      #   ...      ...       ...
      #    Z        x        y

      # The easies way to make this, is to make a NumberOfNodes X Steps matrix, and then melt it
      coverageMatrix = data.frame(matrix(0, nrow = totalNodes, ncol = totalSteps))
      colnames(coverageMatrix) = c(1:totalSteps)
      coverageMatrix$ID = nodesDF$ID

      # Now we check all the combinations in the distance map
      for(i in 1:totalNodes){

        for(j in 1:totalSteps){

          # Get the row (or the column, since is undirected it doesn't matter) TODO: Make it directed
          currentRow = distanceMap[i,]

          # Check how many are in the current reach
          totalReached = sum(currentRow <= j)

          # Add that info to the matrix
          coverageMatrix[i,j] = totalReached/totalNodes

        }

      }

      # In here we have the coverage matrix finished, so we melted it
      meltedCoverage = melt(coverageMatrix, id.vars = "ID")

      # Prepare the color vector which is going to be plain grey for all the steps
      boringColorVector = rep("grey", totalSteps)

      # Get an automatic name if you don't have a proper one
      {
        
        imageFilePath = ""
        
        myFileExtension = getFileExtension(plotFilePath)
        
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = meltedCoverage,
                                               tableName = myTableName, fileType = myPlotType)
        
      }
      
      # We don't care about the ID anymore, so we can throw that away (we care so little that we don't care about deleting it even)

      # Now we do the plot
      # 2 = Number of steps
      # 3 = Coverage
      # ---- Boxplot
      
      
      #TODO: Print/Add underlines of only p-values under certain limit
      plotResults = doCategoricalBoxPlot (meltedCoverage, 2, 3, imageFilePath,
                                          outlierShape = NA,
                                          colorsVector = boringColorVector, showPValues = FALSE,
                                          ymin = ymin, ymax = ymax,
                                          plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                          plotCaption = plotCaption, plotXLabel = plotXLabel, plotYLabel = plotYLabel,
                                          plotTheme = plotTheme,
                                          overrideTableName = myTableName,
                                          overrideCaption = overrideCaption) 
      
      # Save the image
      # imageWidth = totalSteps 
      # ggsave(, plot = myBoxPlot, width = imageWidth)
      # latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
      
      # Return
      myReturn = vector("list", length = 5)
      myReturn[[1]] = plotResults[[1]]
      myReturn[[2]] = plotResults[[2]]
      myReturn[[3]] = plotResults[[3]]
      myReturn[[4]] = plotResults[[4]]
      myReturn[[5]] = plotResults[[5]]
      
      return (myReturn)

    }

    
    # Simulation plot
    # A bunch of steps to see how a disease advance in your network after that many steps
    doSimulationPlot <- function(edgesDF, nodesDF, plotFilePath, directedPlot = FALSE, totalSteps = 10, totalSimulations = 5,
                                 plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                 plotTheme = NULL,
                                 overrideTableName = NULL,
                                 overrideCaption = NULL,
                                 ymin = NULL, ymax = NULL){
      
      
      # Define plot type
      myPlotType  = "SimulationLinePlot"
      myTableName = deparse(substitute(edgesDF))
      
      # If you need to override your table name, then do it
      if(!is.null(overrideTableName)){
        myTableName = overrideTableName
        
      }
      
      # Get the theme information
      themeData = getThemeParameters(plotTheme)
      
      # Get an automatic name if you don't have a proper one
      {
        
        imageFilePath = ""
        
        myFileExtension = getFileExtension(plotFilePath)
        
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = edgesDF,
                                               tableName = myTableName, fileType = myPlotType)
        
      }
      
      print(imageFilePath)
      print("Simulating, please wait...")
      
      # Create the graph objects and check for distances
      myGraph      = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot)
      distanceMap  = distances(myGraph)
      
      # Get how many nodes we have
      totalNodes   = nrow(nodesDF)
      
      # Initialize the conditions matrices
      # -- Probability to infect others and be infected yourself by others.
      #    (right now a very simple homogeneous model)
      
      # -- Probability to get a recurrent virus after cured
      #    (herpes type, very simple, now is none)
      #
      # -- Probability of getting immunity
      #    (if somebody try to infect you and fails, do you become immune forever, now is NO)
      #
      # -- Probability of dying
      #    (in each given step, how likely is that you are remove from the infected set, no is ZERO)
      #
      # TODO: There is more, see slides
      
      # CONTAGIOUS MATRIX
      # ----  If you are infected, what is the probability of giving the disease to another
      #       (leave it constant for simplicity, but you can model people giving away disease
      #        to other being more easy/difficult, ie: they live together and person A doesn't
      #        wash his hands ever, agh!)
      giveDeseaseMatrix           = data.frame(matrix(0.3, nrow = totalNodes, ncol = totalNodes))
      colnames(giveDeseaseMatrix) = c(1:totalNodes)
      giveDeseaseMatrix $ID       = nodesDF$ID
      
      # INFECTABILITY MATRIX
      # ---- If you are not infected, what is the probability of receiving the disease if you are expose
      #      (how good your immune system is with respect the person that gives you the disease,
      #       if the person doesn't matter, leave the rows constant)
      receiveDeseaseMatrix           = data.frame(matrix(0.9, nrow = totalNodes, ncol = totalNodes))
      colnames(receiveDeseaseMatrix) = c(1:totalNodes)
      receiveDeseaseMatrix$ID        = nodesDF$ID
      
      # DYING VECTOR
      # ---- If you are infected, what is the probability of dying at any given time
      #      (you can change this to follow a distribution, so you have more chances the more the disease advance for example)
      #      (this is a vector, you don't have more chances of dying depending of how many people you are friend, you
      #       have more probabity of getting infected, but dying is an independent event)
      dyingProbabilityVector = rep(0, totalNodes)
      
      
      # We need to create a table like this to make the lineplot:
      # -----------------------------------
      # Start X  | Start Y \ End X | End Y
      #  1.3        5.3       5.2     6.7
      #          ................
      # (There is going to be TotalSteps x Total Simulation lines)
      evolutionDF                = data.frame(matrix(0, nrow = totalSteps * totalSimulations  , ncol = 4))
      colnames(evolutionDF)      = c("StartX","StartY","EndX","EndY")
      currentEvolutionRow        = 1
      
      # Pick up your starting nodes
      startingNodes = sample(x = nodesDF$ID,size = totalSimulations, replace = FALSE)
      
      # ------------------------------------------------------
      # FOR EACH SIMULATION (each one of the starting points)
      # ------------------------------------------------------
      for(i in 1:totalSimulations){
        
        print("Simulation Started:")
        print(i)
        
        # Initialize everything
        {
          # Get how many nodes we have, this might vary during the simulation if people die
          totalNodes        = nrow(nodesDF)
          currentTotalNodes = totalNodes
          
          # Initialize your set of infected to the first person
          infectedSet = c(startingNodes[i])

          # The dead people come here
          deadPeopleSet = c()
        }
        
        # ------------------------------------------------------
        # For each step in the simulation
        # ------------------------------------------------------
        for(j in 1:totalSteps){
          
          print( paste("        ", round(j/totalSteps,2), sep="") )

          # Save the original infected for later
          lastStepInfected = infectedSet

          # Count how many infected we have
          totalInfected    = length(infectedSet)

          # Check how many people get infected in this step
          # For each infected person (if there are any left)
          if(totalInfected > 0){
            
            # People that have been infected during this step
            newInfected = c()
            
            # For each infected person that we have
            for(k in 1:totalInfected){
              
              # Who is giving the disease
              currentIllPersonID     = infectedSet[k]

              # Check his neighbors
              currentNeighbours = unique(neighbors(myGraph, currentIllPersonID, mode = "all"))
              totalNeighbours   = length(currentNeighbours)

              # Check in the matrix if the person get infected or not based on the die roll
              if(totalNeighbours > 0){
                
                # Make the disease roll
                currentGivingDiseaseRoll = runif(totalNeighbours, 0, 1)
                currentSavingDiseaseRoll = runif(totalNeighbours, 0, 1)
                
                for(l in 1:totalNeighbours){
                  
                  #The person candidate to receive the disease
                  currentNeighbourID = currentNeighbours[l]

                  if( TRUE ){
                    
                    # Get the probabilities for giving and receiving
                    probOfGiving       = giveDeseaseMatrix[   currentIllPersonID, currentNeighbourID]    
                    probOfReceiving    = receiveDeseaseMatrix[currentIllPersonID, currentNeighbourID] 

                    # First if you manage to give it away
                    if(currentGivingDiseaseRoll[l] < probOfGiving){
                      # Second, if the person defended himself
                      # print("...It gave him the disease")
                      if(currentSavingDiseaseRoll[l] < probOfReceiving){
                        # At this point, the neighbor is mark as infected
                        newInfected = union(newInfected, currentNeighbourID)
                        
                      }  
                      else{
                        # print("...but the inmune system counter it")
                      }
                    }
                    else{
                      # print("...but the disease didn't transmit")
                    }
                    
                  }
                  
                }
              }
            } 
            
            # Get how many new people are infected
            totalCurrentInfected = length(newInfected)

            # Add then to the list of infected
            if(totalCurrentInfected > 0) infectedSet = union( infectedSet, newInfected)
            
            totalNewInfected = length(infectedSet)
            
            
          }
          
          # Now we have all the information we need to add a line to the dataframe
          {
            startY = totalInfected/totalNodes    # (old number)
            endY   = totalNewInfected/totalNodes # (new number)
            
            startX = j - 1
            endX   = j
            
            evolutionDF$StartX[currentEvolutionRow] = startX
            evolutionDF$StartY[currentEvolutionRow] = startY
            evolutionDF$EndX[currentEvolutionRow]   = endX
            evolutionDF$EndY[currentEvolutionRow]   = endY
            
            currentEvolutionRow = currentEvolutionRow + 1  
          }
          
          # Now remove the people that die (or go into isolation, or whatever)
          {
            # -- The newly infected people will not die until the next step, so we only eliminate the old ones

            deathRoll           = runif(totalNewInfected, 0, 1)
            deathCandidates     = (nodesDF$ID %in% infectedSet)
            deathCandidates     = dyingProbabilityVector[deathCandidates]
            deathResults        = deathRoll < deathCandidates
            currentDeadPeople   = lastStepInfected[deathResults]
            totalNewDeadPeople  = length(currentDeadPeople)
            
            # -- Add the reaped ones to the death set
            deadPeopleSet   = union(deadPeopleSet, currentDeadPeople)
            totalDeadPeople = length(deadPeopleSet)
            
            # -- Delete those from the infected set
            infectedSet   = setdiff(infectedSet, currentDeadPeople)        
            
          }
          
          # Print some info for debugging
          # print("Step")
          # print(j)
          # print("Accumulated Infected: ")
          # print(totalInfected)
          # print("New:")
          # print(totalCurrentInfected)
          # print("Died:")
          # print(totalNewDeadPeople)
          # print("Accumulated deaths:")
          # print(totalDeadPeople)
          
          
        }
        
        print("Simulation Ended:")
        print(i)
        print(round(i/totalSimulations,2))
        
      }

      # ------------------------------------------------------
      # DO THE PLOT
      # ------------------------------------------------------
      
      # Do the plot with all the lines
      simulationPlot = ggplot(evolutionDF) +
        
        # Draw each line
        geom_segment(aes(x = StartX, y = StartY, xend = EndX, yend = EndY), colour = "red", alpha = 0.1) +
        
        # Draw ALL the steps in the X axys
        scale_x_continuous(breaks=c(0:totalSteps)) +
        
        # Scale the y axis to whatever
        scale_y_continuous(limits=c(ymin, ymax)) +
        
        # Create titles and subtitles
        labs(title    = plotTitle,
             subtitle = plotSubtitle,
             caption  = plotCaption,
             x = plotXLabel, y = plotYLabel) +
        
        # Apply the theme
        theme(panel.background   = themeData[[1]],
              axis.line          = themeData[[2]],
              panel.grid.major.y = themeData[[3]],
              panel.grid.major.x = themeData[[4]])
      
      # Save the image
      imageWidth = totalSteps/2 
      ggsave(imageFilePath, plot = simulationPlot, width = imageWidth)
      latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
      
      # Return
      myReturn = vector("list", length = 3)
      myReturn[[1]] = simulationPlot
      myReturn[[2]] = imageFilePath
      myReturn[[3]] = latexFilePath
      
      return (myReturn)
      
    }
    

    # Simulation plot
    # THIS IS IMPOSSIBLE!!!! to run good on R, need to use Rcpp to make anything good of this
    # A bunch of steps to see how a disease advance in your network after that many steps
    doSimulationPlot2 <- function(edgesDF, nodesDF, plotFilePath, directedPlot = FALSE, totalSteps = 10, totalSimulations = 5,
                                 plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                 plotTheme = NULL,
                                 overrideTableName = NULL,
                                 overrideCaption = NULL,
                                 ymin = NULL, ymax = NULL){

      
      # Define plot type
      myPlotType  = "SimulationLinePlot"
      myTableName = deparse(substitute(edgesDF))
      
      # If you need to override your table name, then do it
      if(!is.null(overrideTableName)){
        myTableName = overrideTableName
        
      }
  
      # Get the theme information
      themeData = getThemeParameters(plotTheme)

      # Get an automatic name if you don't have a proper one
      {
        
        imageFilePath = ""
        
        myFileExtension = getFileExtension(plotFilePath)
        
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = edgesDF,
                                               tableName = myTableName, fileType = myPlotType)
        
      }

      print(imageFilePath)
      print("Simulating, please wait...")

      # Create the graph objects and check for distances
      myGraph      = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot)
      distanceMap  = distances(myGraph)

      # Get how many nodes we have
      totalNodes   = nrow(nodesDF)

      # Initialize the conditions matrices
      # -- Probability to infect others and be infected yourself by others.
      #    (right now a very simple homogeneous model)
      
      # -- Probability to get a recurrent virus after cured
      #    (herpes type, very simple, now is none)
      #
      # -- Probability of getting immunity
      #    (if somebody try to infect you and fails, do you become immune forever, now is NO)
      #
      # -- Probability of dying
      #    (in each given step, how likely is that you are remove from the infected set, no is ZERO)
      #
      # TODO: There is more, see slides

      # CONTAGIOUS MATRIX
      # ----  If you are infected, what is the probability of giving the disease to another
      #       (leave it constant for simplicity, but you can model people giving away disease
      #        to other being more easy/difficult, ie: they live together and person A doesn't
      #        wash his hands ever, agh!)
      giveDeseaseMatrix           = data.frame(matrix(0.3, nrow = totalNodes, ncol = totalNodes))
      colnames(giveDeseaseMatrix) = c(1:totalNodes)
      giveDeseaseMatrix $ID       = nodesDF$ID

      # INFECTABILITY MATRIX
      # ---- If you are not infected, what is the probability of receiving the disease if you are expose
      #      (how good your immune system is with respect the person that gives you the disease,
      #       if the person doesn't matter, leave the rows constant)
      receiveDeseaseMatrix           = data.frame(matrix(0.9, nrow = totalNodes, ncol = totalNodes))
      colnames(receiveDeseaseMatrix) = c(1:totalNodes)
      receiveDeseaseMatrix$ID        = nodesDF$ID
      
      # DYING VECTOR
      # ---- If you are infected, what is the probability of dying at any given time
      #      (you can change this to follow a distribution, so you have more chances the more the disease advance for example)
      #      (this is a vector, you don't have more chances of dying depending of how many people you are friend, you
      #       have more probabity of getting infected, but dying is an independent event)
      dyingProbabilityVector = rep(0, totalNodes)

      
      # We need to create a table like this to make the lineplot:
      # -----------------------------------
      # Start X  | Start Y \ End X | End Y
      #  1.3        5.3       5.2     6.7
      #          ................
      # (There is going to be TotalSteps x Total Simulation lines)
      evolutionDF                = data.frame(matrix(0, nrow = totalSteps * totalSimulations  , ncol = 4))
      colnames(evolutionDF)      = c("StartX","StartY","EndX","EndY")
      currentEvolutionRow        = 1

      # Pick up your starting nodes
      startingNodes = sample(x = nodesDF$ID,size = totalSimulations, replace = FALSE)

      # ------------------------------------------------------
      # FOR EACH SIMULATION (each one of the starting points)
      # ------------------------------------------------------
      for(i in 1:totalSimulations){

        print("Simulation Started:")
        print(i)

        # Initialize everything
        {
          # Get how many nodes we have, this might vary during the simulation if people die
          totalNodes        = nrow(nodesDF)
          currentTotalNodes = totalNodes

          # Initialize your set of infected to the first person
          #infectedSet = c(startingNodes[i])
          infectedSet2 = rep(FALSE, totalNodes)
          infectedSet2[as.numeric(startingNodes[i])] = TRUE  # Stupid R doesn't want the IDs as numbers but strings :(
                                                             # Like, this whole thing with boolean vector to optimize is silly
                                                             # a real programing language has pointers to optimize all of this :((((

          # The dead people come here
          #deadPeopleSet = c()
          deadPeopleSet2 = rep(FALSE, totalNodes)

          # In here we put the people who are candidate to be infected in whatever current step
          #potentialCurrentStep = c()
        }

        # ------------------------------------------------------
        # For each step in the simulation
        # ------------------------------------------------------
        for(j in 1:totalSteps){

          print( paste("        ", round(j/totalSteps,2), sep="") )
          print("A")
          # Update how many people we have left
          #currentTotalNodes = totalNodes - length(deadPeopleSet)
          #currentTotalNodes = totalNodes - sum(deadPeopleSet2)
          print("B")
          # Save the original infected for later
          #lastStepInfected = infectedSet
          lastStepInfected2 = infectedSet2
          print("C")
          # Count how many infected we have
          #totalInfected    = length(infectedSet)
          totalInfected      = sum(infectedSet2)
          currentInfectedIDs = nodesDF[infectedSet2,]$ID
          print( paste("        ", round(j/totalSteps,2), sep="") )
          print("D")
          # Check how many people get infected in this step
          # For each infected person (if there are any left)
          if(totalInfected > 0){
          print("D2")  
            # People that have been infected during this step
            newInfected = rep(FALSE, totalNodes)
            print("D3")
            # For each infected person that we have
            for(k in 1:totalInfected){
              print("D31")  
              # Who is giving the disease
              #currentIllPersonID     = infectedSet[k]
              currentIllPersonID = currentInfectedIDs[k]
              currentIllPersonID = as.numeric(currentIllPersonID)
              
              # print("The person that is currently Ill is")
              # print(currentIllPersonID)
              print("D32")
              # Check his neighbors
              results = getFrienshipTypes(currentIllPersonID, overallNetworkDF)
              #currentNeighbours = unique(c(results[[4]], results[[5]]))
              currentNeighbours = results[[4]]
              if (length(currentNeighbours) == 1){
                if(currentNeighbours == 0) currentNeighbours = NULL # Another shitty thing from R, I can't return a constant list of NULLs,
                # because it just delete that element of the list and make it shorter!!!                
              }

              #currentNeighbours = unique(neighbors(overallGraph, currentIllPersonID, mode = "all"))
              totalNeighbours   = length(currentNeighbours)

               # print("Me")
               # print(currentIllPersonID)
               # print("I have these neighbours")
               # print(currentNeighbours)
               
              print("D33")
              # Check in the matrix if the person get infected or not based on the die roll
              if(totalNeighbours > 0){

                # Make the disease roll
                currentGivingDiseaseRoll = runif(totalNeighbours, 0, 1)
                currentSavingDiseaseRoll = runif(totalNeighbours, 0, 1)
                
                for(l in 1:totalNeighbours){
                  
                  #The person candidate to receive the disease
                  currentNeighbourID = currentNeighbours[l]
                  
                  currentNeighbourID = as.numeric(currentNeighbourID)
                  
                  # print("My next neighbour ID")
                  # print(currentNeighbourID)
                  
                  # If the person that we are trying to infect:
                  # -- has already been infected in this step
                  # -- is dead
                  # -- is immune
                  # -- is already infected
                  # Then skip it
                  skipThis = ( newInfected[currentNeighbourID]    ||
                               deadPeopleSet2[currentNeighbourID] ||
                               infectedSet2[currentNeighbourID] )
                  
                  # print("-- skip test --")
                  # print(skipThis)
                  # print("----")
                  # print(newInfected[currentNeighbourID])
                  # print(deadPeopleSet2[currentNeighbourID])
                  # print(infectedSet2[currentNeighbourID])
                  # print("----")
                  
                  # print(paste0(currentIllPersonID, " is trying to infect ", currentNeighbourID))
                  # if(infectedSet2[currentNeighbourID]){
                  #   print("...But he is already infected")
                  # }
                  # if(newInfected[currentNeighbourID]){
                  #   print("...But somebody else infected him in this round")
                  # }
                  # if(deadPeopleSet2[currentNeighbourID]){
                  #   print("...But his friend is already dead")
                  # }
                  
                  
                  if( skipThis == FALSE ){
                    
                    # Get the probabilities for giving and receiving
                    probOfGiving       = giveDeseaseMatrix[   currentIllPersonID, currentNeighbourID]    
                    probOfReceiving    = receiveDeseaseMatrix[currentIllPersonID, currentNeighbourID] 
                    
                    # print(currentIllPersonID)
                    # print(currentNeighbourID)
                    # print(probOfGiving)
                    # print(currentGivingDiseaseRoll[l])
                    
                    # First if you manage to give it away
                    if(currentGivingDiseaseRoll[l] < probOfGiving){
                      # Second, if the person defended himself
                      # print("...It gave him the disease")
                      if(currentSavingDiseaseRoll[l] < probOfReceiving){
                        # At this point, the neighbor is mark as infected
                        newInfected[currentNeighbourID] = TRUE
                        # print( paste0("NEW INFECTED!! ", currentNeighbourID )  )
                      }  
                      else{
                        # print("...but the inmune system counter it")
                      }
                    }
                    else{
                      # print("...but the disease didn't transmit")
                    }
                    
                  }

                }
              }
            } 
            print("D4")
            # Get the ID of the newly infected people
            newInfectedID    = nodesDF[newInfected,]$ID
            #totalNewInfected = length(newInfected)
            totalNewInfected = sum(newInfected)
            print("D5")
            # Add then to the list of infected
            #if(totalNewInfected > 0) infectedSet = union( infectedSet, newInfectedID)
            
            # print("Pre")
            # # print(infectedSet2)
            # print(sum(infectedSet2))
            
            if(totalNewInfected > 0) infectedSet2 = ( infectedSet2 | newInfected)
            
            # print("---")
            # print("Post")
            # #print(infectedSet2)
            # print(sum(infectedSet2))
            
          }
          print("E")
          # Now we have all the information we need to add a line to the dataframe
          {
            startY = totalInfected/totalNodes       # (old number)
            #endY   = length(infectedSet)/currentTotalNodes # (new number)
            endY   = sum(infectedSet2)/totalNodes # (new number)
            startX = j
            endX   = j + 1
            
            evolutionDF$StartX[currentEvolutionRow] = startX
            evolutionDF$StartY[currentEvolutionRow] = startY
            evolutionDF$EndX[currentEvolutionRow]   = endX
            evolutionDF$EndY[currentEvolutionRow]   = endY
            
            currentEvolutionRow = currentEvolutionRow + 1  
          }
          print("F")
          # Now remove the people that die (or go into isolation, or whatever)
          {
            # -- The newly infected people will not die until the next step, so we only eliminate the old ones
            #deathRoll           = runif(totalInfected, 0, 1)
            #deathRoll           = deathRoll < dyingProbabilityVector
            #currentDeadPeople   = lastStepInfected[deathRoll]
            
            # -- Add the reaped ones to the death set
            #deadPeopleSet = union(deadPeopleSet, currentDeadPeople)
            #deadPeopleSet = (deadPeopleSet2 |  currentDeadPeople)
            # -- Delete those from the infected set
            #infectedSet   = setdiff(infectedSet, currentDeadPeople)        
            #infectedSet   = (infectedSet2  currentDeadPeople)        
            
            # infected    ,  # dead  , # Z    (A and B)   (A !xor B)
            # t                t         f       t          t
            # t                f         t       f          f
            # f                t         f       f          f
            # f                f         f       f          t
            # t                f         t       f          f
            
            
            deathRoll           = runif(totalNodes, 0, 1)
            deathRoll           = deathRoll < dyingProbabilityVector # People marked for death
            currentDeadPeople   = (lastStepInfected2 & deathRoll) # Only infected people die
            deadPeopleSet2      = (deadPeopleSet2 |  currentDeadPeople) # Update the people that is dead
            # Remove the people from the infected set
            # I don't know how to do this in R with a single vector operation
            # so into a loop we go.
            # If infected AND dead , infected = FALSE
            # otherwise keep the infected value
            for(z in 1:totalNodes){
              
              if(infectedSet2[z] == TRUE && deadPeopleSet2[z] == TRUE) infectedSet2[z] == FALSE
              
            }

          }

          # # Print some info for debugging
          # print("Step")
          # print(j)
          # print("Accumulated Infected: ")
          # print(sum(infectedSet2))
          # print("New:")
          # print(sum(newInfected))
          # print("Died:")
          # print(sum(currentDeadPeople))
          # print("Accumulated deaths:")
          # print(sum(deadPeopleSet2))
          
          
        }


        #print("Final infected:")
        #print(infectedSet)
        print("Simulation Ended:")
        print(i)
        print(round(i/totalSimulations,2))

      }

      #print(evolutionDF)
      
      # ------------------------------------------------------
      # DO THE PLOT
      # ------------------------------------------------------

      # Do the plot with all the lines
      simulationPlot = ggplot(evolutionDF) +

                       # Draw each line
                       geom_segment(aes(x = StartX, y = StartY, xend = EndX, yend = EndY), colour = "red", alpha = 0.1) +

                       # Draw ALL the steps in the X axys
                       scale_x_continuous(breaks=c(1:totalSteps)) +

                       # Scale the y axis to whatever
                       scale_y_continuous(limits=c(ymin, ymax)) +

                       # Create titles and subtitles
                       labs(title    = plotTitle,
                            subtitle = plotSubtitle,
                            caption  = plotCaption,
                            x = plotXLabel, y = plotYLabel) +

                       # Apply the theme
                       theme(panel.background   = themeData[[1]],
                             axis.line          = themeData[[2]],
                             panel.grid.major.y = themeData[[3]],
                             panel.grid.major.x = themeData[[4]])

      # Save the image
      imageWidth = totalSteps 
      ggsave(imageFilePath, plot = simulationPlot, width = imageWidth)
      latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
      
      # Return
      myReturn = vector("list", length = 3)
      myReturn[[1]] = simulationPlot
      myReturn[[2]] = imageFilePath
      myReturn[[3]] = latexFilePath
      
      return (myReturn)

    }


    # Here comes the function to do the network simulations. In general this is
    # quite straight forward, however we have an special case for a function
    # that we do later.
    #
    # I tried for more than a week to generalize that function into the general
    # case in a simple way, and is simply not possible to make it simple and in
    # a code that is understandable. So for the sake of simplicity, I'm not 
    # making the function generalization, but rather a specific function for a
    # very specific case later on.
    
    # Here come first the general simple cases that are intuitive:
    
    
    # This function does a bunch of simulations and return a vector of size
    # total simulations. Each element of the vector contain how many same to
    # same relationships we have in that simulation.
    #
    # This function doesn't tell you whether the relationships are bias or not
    # it only generate the vector, later on you have to analyize that.
    #
    # There are 3 ways to run the bootstrap vector:
    #
    #     A)
    #
    #        Get the general prevalence. Whatever you give in the categoricalIndex
    #        variable has a prevalence (ie Positive 60% Negative 40%), and that
    #        frequency is apply to everyone in the nodes table.
    #
    #     B)
    #
    #        Get an specific prevalence. You might have an special categorical
    #        variable given by overrideFrequenciesIndex, which has an specific
    #        modality given by overrideFrequencyCategory (ie Sex -> Woman).
    #        This modality has a prevalence likely different from the general
    #        population (ie Positive 60% Negative 40% was the original, but 
    #        women has a Positive 20% and Negative 80%). Then this frequency
    #        is apply to everyone in the table.
    #
    #        The point of this is to compare what would happens if we consider
    #        everybody to have this category. If the result are significant
    #        it means that this modality has a different risk (higher or lower)
    #        than the rest.
    #
    #     C)
    #
    #        Get an specific prevalence. You might have an special categorical
    #        variable given by overrideFrequenciesIndex, which doesn't has an
    #        specific modality given by overrideFrequencyCategory (NULL/NA).
    #
    #        This means that you are going to take each modality (Men, Women)
    #        And apply that prevalence to each element of the node table.
    #        (ie Men Positive 80% Negative 20%, Women Positive 20% and
    #        Negative 80%). Then this frequency is apply to everyone in the table.
    #        accordingly to it modality.
    #
    #
    # tableBase                = which DF do you want
    #
    # tableEdges               = edges with the real relationships
    #                            with "from" , "to" variables, "value" optional
    #
    # categoricalIndex         = For the given DF in tableBase, which column do you
    #                            want to analyze. The column must contain a
    #                            categorical type of variable.
    #                            (ie: SPAType or Carrier Status)
    #
    #                            If you have a numerical variable, you need to
    #                            convert it to categorical first.
    #
    # totalSimulations         = How many simulations do you want to do. As a
    #                            general rule, 1000 simulations is good enough.
    #
    # simulateRelationships    = Whether to use original relationships or simulate
    #                            new random ones. (DEFAULT = FALSE)
    #
    #                            Keeping the same relationships maintain the network
    #                            topology, so you are analyzing if there is a bias
    #                            in the relationships (ie: Do same type of carrier
    #                            are more likely to be friends with each others)
    #
    #                            If you don't keep the relationship, the results
    #                            will have several different meanings depending of
    #                            what you are doing. Consult your local statician
    #                            for more information.
    #                           
    #
    # overrideFrequenciesIndex = Which column are you going to use to build the
    #                            frequency table. The default is the same as
    #                            categoricalIndex, so nothing change by default.
    #
    #                            (ie: Index for Sex, BMI, Smoking, or whatever)
    #
    #                            You might want to do, for example, the analysis
    #                            for smoking, but using the frequency of Sex: MEN
    #                            and Sex: WOMEN, to check whether men or women have
    #                            some sort of bias, higher risk, or whatever.
    #
    # overrideFrequencyCategory = If you want to use the previous variable, I need
    #                             a category to filter by (ie: "Woman", "Yes", "40")
    #                             If you give me an index in the previous variable,
    #                             but this is still NULL (default) I will run
    #                             B2 instead of B1.
    #
    #
    # showProgressBar         =  (String) If not NULL (default), the console will show
    #                            a little progress bar from 0% to 100% telling
    #                            how far we are doing the simulations.
    #
    #                            Beware that the progress bar will clear the console
    #                            of all text.
    #
    #                            If not NULL, you need to add a string here that
    #                            will show up later in console. Recomendation is
    #                            that you give something meaningfull like
    #
    #                            "Doing simulations for school: "
    #
    #                            Is useful to set it to NULL if you use this inside
    #                            another function, of if you don't want to loose the
    #                            text in the console for whatever reason.
    #
    # Return: A vector of size totalSimulations, which how many same to same relationship
    #         where found in each particular simulation
  
    
    # This function tells you if your relationships are bias or not
    # Only for categorical variables. It can run the version A, B and C,
    # of the Bootstrap vector.
    #
    # In order to select each version, fill the input variables accordingly.
    #
    # nodesTable    = Dataframe with the information about your nodes
    #                 It can have any structure you want, the only restriction is
    #                 the first column must be the ID column, and it must be
    #                 a numerical ID
    #
    #
    # listOfEdgesDF = List of Dataframes. Each dataframe have a different network
    #                 The dataframe structure goes like this:
    #
    #                 from     to     value 
    #                    1      2         3
    #                    2      3         5
    #                          ...
    #
    #                The "value" column is irrelevant for this analysis, but
    #                is the standard way to save the edges in a network.
    #
    # listOfNetworkNames = List of Strings with the name of each network. The
    #                      default value is NULL and it will be named from 1 to 
    #                      X. Otherwise, if you want proper names, give them to
    #                      the function in this paremeter.
    #
    #
    # listOfConsequenceIndexes = List of Indexes with the variables that you want
    #                            to study. The variables must be categorical, and
    #                            the indexes must be a valid index contain within
    #                            the nodeTable.
    #
    # totalSimulations         = How many simulations you want to run
    #                            There is no default, 10 is good for testing
    #                            And 1000 is good for getting results.
    # 
    # Return:
    #
    # 
    # A list of dataframes with the following columns
    #
    #     "Network"               Name of the Network
    #     "Total Relationships",  How many relationships we have
    #     "Equal Relationships",  How many same to same relationships we have
    #  
    #     "MIN",                  Minimum same to same relationships count that we found in the simulation
    #     "Q1",                   Percentile 25 of same to same relationships count that we found in the simulation
    #     "Median",               Median same to same relationships count that we found in the simulation
    #     "Average",              Average same to same relationships count that we found in the simulation
    #     "Q3",                   Percentile 75 same to same relationships count that we found in the simulation
    #     "MAX",                  Maximum same to same relationships count that we found in the simulation
    #     "SD",                   Standard Deviation same to same relationships count that we found in the simulation
    #
    #     "ConsequenceIndex"      The actual name of this column change depending of the name of each consequence index.
    #                             In here we have the actual p-value for each network, on whether your relationship
    #                             is bias or not.
    # 
    doCategoricalBiasAnalysis <- function(nodesTable, listOfEdgesDF,
                                          listOfConsequenceIndexes,
                                          totalSimulations,
                                          overrideFrequenciesIndex  = NULL,
                                          overrideFrequencyCategory = NULL,
                                          listOfNetworksNames = NULL){

        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
        }
        
        # Prepare the blank base DF where we write the results.
        # We will have one of this for each consequence index.
        biasSimulationsDF           =  data.frame(matrix(NA, nrow = totalNetworks, ncol = 10 + 1 ))
        colnames(biasSimulationsDF) = c("Network", "Total Relationships", "Equal Relationships", "MIN", "Q1", "Median", "Average", "Q3", "MAX", "SD",  "ConsequenceIndex"  )
        for(i in 1:totalNetworks){
            biasSimulationsDF[i,1]  = myNetworkNames[i]
        }
        
        # We have a list of result for each of the consequence index
        # So in here we prepare such list, and give a blank DF to each
        biasResultsList =  newList(totalConsequenceIndexes)
        for( i in 1:totalConsequenceIndexes){
      
            # Init the DF to empty
            biasResultsList[[i]] = biasSimulationsDF

            # Change the name of the variable we are interested in for this DF
            colnames(biasResultsList[[i]])[11] = consequenceNames[i]

        }

        # For each of the consequence index, we do these 1000-ish simulation for
        # each of the networks that you have.
        for (i in 1:totalConsequenceIndexes){
      
            # Get the DF where we save the results for this variable
      
            # R is stupid, why can't I pass a reference? why do I need to use an index
            # here when then an alias to the variable would make everything more
            # readable and efficient??? >:[
            # currentDF = biasResultsList[[i]]
      
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[i]
                currentName  = consequenceNames[i]        
            }

            # For each of the network, do the proper bias analysis
            # For each network ( each network is represented by an edges DF)
            for(j in 1:totalNetworks){
                    
                    # Get the current edges
                    currentEdges = listOfEdgesDF[[j]]
                    
                    # Find out the if "from" - "to" have the same relationship
                    currentEdges$SameRelationship = addEdgeRelationship(currentEdges,  nodesTable, currentIndex)
                    
                    # Find out how many relationships we have
                    currentTotalRelationships          = nrow(currentEdges)
                    
                    # Find out how many same to same relationships we have
                    # This is the real value that we use in the p-value calculation
                    currentTotalSameRelationships      = sum(currentEdges$SameRelationship == TRUE)
                 
                    # Check if carrier have bias friendship towards people with the same carrier status
                    #
                    # -- Prepare the custom message 
                    currentWaitingMessage = paste0( "Doing simulations for ", myNetworkNames[j], " please wait..." )

                    # -- Do the bias analysis
                    currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentIndex, totalSimulations,
                                                           overrideFrequenciesIndex  = overrideFrequenciesIndex,
                                                           overrideFrequencyCategory = overrideFrequencyCategory,
                                                           showProgressBar           = currentWaitingMessage)
                    
                    
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write all this info in the appropriate part of the results
                    biasResultsList[[i]][j,  2 ] = currentTotalRelationships
                    biasResultsList[[i]][j,  3 ] = currentTotalSameRelationships
                    biasResultsList[[i]][j,  4 ] = min(currentBiasResult)
                    biasResultsList[[i]][j,  5 ] = as.integer(summary(currentBiasResult)[2])
                    biasResultsList[[i]][j,  6 ] = median(currentBiasResult)
                    biasResultsList[[i]][j,  7 ] = currentAverage
                    biasResultsList[[i]][j,  8 ] = as.integer(summary(currentBiasResult)[5])
                    biasResultsList[[i]][j,  9 ] = max(currentBiasResult)
                    biasResultsList[[i]][j, 10 ] = currentSD
                    biasResultsList[[i]][j, 11 ] = currentPValue
                       
                }

            
        }
        
                
        # Everything is finish, give back the list of biases and close.
        return(biasResultsList)
        
        
    }
    
    
    # This function tells you if your relationships are bias or not with respect
    # each modality of each categorical variable.
    #
    # It also find the confident interval
    #
    # Only for categorical variables (obviously). It compares the simulations
    # against the results A from the categorical bias function
    #
    # Return
    #
    # 
    # A list of dataframes with the following columns
    #
    #     "Variable"
    #     "Modality"
    #     "Index"
    #     "Network"
    #     "Real Relationships"
    #     "Real Same to Same"
    #     "Simulated Unbias Average Same"
    #     "Simulated Unbias Minimum Same"
    #     "Simulated Bias Average Same"
    #     "Simulated Bias SD"
    #     "Target Variable"
    #     "Base Risk"
    #     "Low CI"
    #     "High CI"
    doModalityBiasAnalysis <-function(nodesTable, listOfEdgesDF,
                                      listOfConsequenceIndexes,
                                      listOfExplanatoryIndexes,
                                      totalSimulations,
                                      listOfNetworksNames = NULL,
                                      confidentValue = 95){
        
        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
            explanatoryIndexes = listOfExplanatoryIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
            # Independent variables information
            #
            # How many variables are we going to study
            totalExplanatoryIndexes = length(explanatoryIndexes)
            # The name of each 
            explanatoryNames = colnames(nodesTable)[explanatoryIndexes]
            
        }
        
        # Prepare the z-score for the given confident interval value
        confidentMargin = qnorm( 1 - ( (1 - ( confidentValue / 100 ) ) / 2 ) )
            
        # Prepare the list where we are going to write the results
        simulationByVariablesDFList = newList(totalConsequenceIndexes)
      
        # Create a blank DF that we are going to copy inside each element of the list
        # -- Count how many rows do we have
        totalModalities = 0
        for (i in 1:totalExplanatoryIndexes) {
        
            explanatoryModalities = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            totalModalities       = totalModalities + length(explanatoryModalities)
        
        }
        # -- Create the dataframe
        blankSimulationResultsDF           = DF(totalModalities, 14)
        colnames(blankSimulationResultsDF) = c("Variable", "Modality", "Index", "Network",
                                               "Real Relationships", "Real Same to Same",
                                               "Simulated Unbias Average Same", "Simulated Unbias Minimum Same",
                                               "Simulated Bias Average Same",   "Simulated Bias SD",
                                               "Target Variable", "Base Risk",  "Low CI", "High CI")
        # -- Init the basic columns
        importantIndex = 1
        for (i in 1:totalExplanatoryIndexes) {
        
            # Get the name of the variable
            currentCategory        = explanatoryNames[i]
            
            currentModalities = NA
            if(is.null(levels(nodesTable[,explanatoryIndexes[i]])))
                currentModalities      = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            else
                currentModalities      = as.character(levels(nodesTable[,explanatoryIndexes[i]]))            
            
            totalCurrentModalities = length(currentModalities)
            
            # For each modality, add it to the dataframe
            for (j in 1:totalCurrentModalities) {
          
                blankSimulationResultsDF[importantIndex,1] = currentCategory
                blankSimulationResultsDF[importantIndex,2] = currentModalities[j]
                blankSimulationResultsDF[importantIndex,3] = explanatoryIndexes[i]
          
                importantIndex = importantIndex + 1
            }
        
        }

        # For each consequence variable and each network, we are going to need
        # to Simulate the bias analysis, and get the bias average. We do that
        # now, and save it for later
        biasResultsDF = doCategoricalBiasAnalysis (nodesTable, listOfEdgesDF,
                                                   consequenceIndexes,
                                                   totalSimulations,
                                                   listOfNetworksNames = myNetworkNames)

        # Everything is initialized at this point.
        #
        # We are going to get A LOT of results from this function.
        #
        # For each consequence index, we have several networks, and for each
        # network we have a huge dataframe with all the modalities.        
        
        # Get the starting time so we can tell aprox. how much to finish
        startTime = Sys.time()
        
        
        # For each consequence variable...
        for (j in 1:totalConsequenceIndexes) {
            
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[j]
                currentName  = consequenceNames[j]        
            }
            
            # Give feedback to the user
            print("----------------------------")
            print( paste0(" Doing index: ", currentName, " ", round(100*j / totalConsequenceIndexes ,2), "%" ))
            print("----------------------------")
            
            # Prepare the network list
            simulationByVariablesDFList[[j]] = newList(totalNetworks)

            # For each of the network...
            for(k in 1:totalNetworks){

                # Prepare the blank dataframe
                simulationByVariablesDFList[[j]][[k]] = blankSimulationResultsDF
                                    
                # Get the current edges
                currentEdges = listOfEdgesDF[[k]]
                currentNetwork = myNetworkNames[k]
                
                # Get the index and name
                currentConsequenceIndex = consequenceIndexes[j]
                currentConsequenceName  = consequenceNames[j]
      
                # Tell the user where are you 
                print("------------------------")
                print( paste0(" Doing network: ", currentNetwork, " ", round(100*k / totalNetworks ,2), "%" ))
                print("------------------------")
                
                # Get the results from the pre-simulations, these are the
                # unbias values and the total relationships values
                currentTotalRelationships     = biasResultsDF[[j]][k,2]
                currentTotalSameRelationships = biasResultsDF[[j]][k,3]
                currentUnbiasSameMinimum      = biasResultsDF[[j]][k,4]
                currentUnbiasSameAverage      = biasResultsDF[[j]][k,7]
                
                # For each row in the table
                for(i in 1:totalModalities){

                    # Get the variable index and modality name
                    currentVariableName  = simulationByVariablesDFList[[j]][[k]][i,1]
                    currentModalityName  = simulationByVariablesDFList[[j]][[k]][i,2]
                    currentVariableIndex = simulationByVariablesDFList[[j]][[k]][i,3]
                    
                    # Give some feedback to the user
                    # -- Time to finish
                    currentTime          = Sys.time()
                    secondsFromStart     = as.numeric(currentTime-startTime,units="secs")
                    proportionFromStart  = ((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)
                    percentagePerSecond  = proportionFromStart / secondsFromStart
                    secondsToFinish      = ((1 - proportionFromStart) * secondsFromStart) / proportionFromStart
                    # -- String to user
                    print("--------------------")
                    print( paste0(" Doing row: ", currentModalityName, " / ", currentVariableName, " - " ,  round(100 * i / totalModalities ,2), "%" ))
                    print("--------------------")
                    print( paste0(" Total: ",   round(100 * (((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)) , 2),  "%" ))
                    print( paste0(" Finish in: ", secondsToFinish))
                    print("--------------------")

                    # Get the bootstrap vector (version B)
                    currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentConsequenceIndex, totalSimulations,
                                                           overrideFrequenciesIndex  = currentVariableIndex,
                                                           overrideFrequencyCategory = currentModalityName)

                    # Get how much is the average and sd of the simulations
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write everything into the current results dataframe
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  4 ] = currentNetwork
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  5 ] = currentTotalRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  6 ] = currentTotalSameRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  7 ] = currentUnbiasSameAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  8 ] = currentUnbiasSameMinimum
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  9 ] = currentAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 10 ] = currentSD
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 11 ] = currentPValue
                    
                }

            }
            
            
        }        

        # Everything is simulated at this point.
        #
        # Now that this is finish, we have the base value for each modality
        # and we can run the CI columns in each result. So we do the same run
        # again, for each consequence index, for each network, for each row:
        
        for (j in 1:totalConsequenceIndexes){
            
            for(k in 1:totalNetworks){
                
                # Get the default base values
                currentReferenceIndex = simulationByVariablesDFList[[j]][[k]][1,3]
                currentBaseValue      = simulationByVariablesDFList[[j]][[k]][1,9]
                currentOrderValue     = 1
                
                
                for(i in 1:totalModalities){
                    
                    # Compare the average with the base value, if they are the same, this is the reference and is equal to 1 
                    simulationByVariablesDFList[[j]][[k]][i,12] = simulationByVariablesDFList[[j]][[k]][i,9] / currentBaseValue
        
                    # Get the lower and upper intervals for a CI of 95%
                    # -- If you are the base value, you don't have a confident interval
                    if(currentOrderValue == 1){
                        simulationByVariablesDFList[[j]][[k]][i,13] = NA
                        simulationByVariablesDFList[[j]][[k]][i,14] = NA
                    }
                    # -- Every other modality of that categorical variable, has a CI
                    else{
                        simulationByVariablesDFList[[j]][[k]][i,13] = ( (-confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                        simulationByVariablesDFList[[j]][[k]][i,14] = ( (+confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                    }
          
                    # Now go to the next row, but we need to check if we are still
                    # studying modalities of the same variable, or if we have jump
                    # to another different variable.
        
                    # Is there a next index?
                    # -- If yes...

                    if(i < totalModalities){
                        # Which one?
                        nextReferenceIndex = simulationByVariablesDFList[[j]][[k]][(i+1),3]
                        
                        # Is the same as the current?
                        # -- If yes...
                        #    Take the next order
                        if(currentReferenceIndex == nextReferenceIndex){
                            
                            currentOrderValue = currentOrderValue + 1
            
                        }
                        # -- If no...
                        #    Change the base value and reference index, reset order to 1
                        else{
                            
                            currentOrderValue     = 1
                            currentReferenceIndex = nextReferenceIndex
                            currentBaseValue      = simulationByVariablesDFList[[j]][[k]][(i+1),9]
            
                        }
          
                    }
                    # -- If no... we are finish
                    
                    
                }
                
            }
            
        }
        
        
        # Finally, everything is done, we just return the final list
        return(simulationByVariablesDFList)

        
    }
    
    # The simulations for the contraceptives is a very special case that can't
    # be put into a function form in a simple way.
    #
    # We need to simulate the bootstrap vector with men using their original
    # prevalence, and women using the prevalence of each contraceptive method.
    # And on top of that, we need to compare with the base of each sex for it
    # original prevalence. Is very chaotic.
    #
    # Hence, I'm copypasting most of the previous code here in this function.
    doContraceptivesCase <-function(nodesTable, listOfEdgesDF,
                                    listOfConsequenceIndexes,
                                    listOfExplanatoryIndexes,
                                    totalSimulations,
                                    mySexIndex,
                                    myHormonalIndex,
                                    listOfNetworksNames = NULL,
                                    confidentValue = 95){
        
        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
            explanatoryIndexes = listOfExplanatoryIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
            # Independent variables information
            #
            # How many variables are we going to study
            totalExplanatoryIndexes = length(explanatoryIndexes)
            # The name of each 
            explanatoryNames = colnames(nodesTable)[explanatoryIndexes]
            
        }
        
        # Prepare the z-score for the given confident interval value
        confidentMargin = qnorm( 1 - ( (1 - ( confidentValue / 100 ) ) / 2 ) )
            
        # Prepare the list where we are going to write the results
        simulationByVariablesDFList = newList(totalConsequenceIndexes)
      
        # Create a blank DF that we are going to copy inside each element of the list
        # -- Count how many rows do we have
        totalModalities = 0
        for (i in 1:totalExplanatoryIndexes) {
        
            explanatoryModalities = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            totalModalities       = totalModalities + length(explanatoryModalities)
        
        }
        # -- Create the dataframe
        blankSimulationResultsDF           = DF(totalModalities, 14)
        colnames(blankSimulationResultsDF) = c("Variable", "Modality", "Index", "Network",
                                               "Real Relationships", "Real Same to Same",
                                               "Simulated Unbias Average Same", "Simulated Unbias Minimum Same",
                                               "Simulated Bias Average Same",   "Simulated Bias SD",
                                               "Target Variable", "Base Risk",  "Low CI", "High CI")
        # -- Init the basic columns
        importantIndex = 1
        for (i in 1:totalExplanatoryIndexes) {
        
            # Get the name of the variable
            currentCategory        = explanatoryNames[i]
            
            currentModalities = NA
            if(is.null(levels(nodesTable[,explanatoryIndexes[i]])))
                currentModalities      = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            else
                currentModalities      = as.character(levels(nodesTable[,explanatoryIndexes[i]]))
            
            totalCurrentModalities = length(currentModalities)
        
            # For each modality, add it to the dataframe
            for (j in 1:totalCurrentModalities) {
          
                blankSimulationResultsDF[importantIndex,1] = currentCategory
                blankSimulationResultsDF[importantIndex,2] = currentModalities[j]
                blankSimulationResultsDF[importantIndex,3] = explanatoryIndexes[i]
          
                importantIndex = importantIndex + 1
            }
        
        }

        # (SPECIAL PART)
        # We want to compare we the case where each men and women have their
        # simulation done with the stratified frequency table (case C)
        biasResultsDF = doCategoricalBiasAnalysis (nodesTable, listOfEdgesDF,
                                                   consequenceIndexes,
                                                   totalSimulations,
                                                   overrideFrequenciesIndex = sexIndex,
                                                   listOfNetworksNames      = myNetworkNames)

        # Everything is initialized at this point.
        #
        # We are going to get A LOT of results from this function.
        #
        # For each consequence index, we have several networks, and for each
        # network we have a huge dataframe with all the modalities.        
        
        # Get the starting time so we can tell aprox. how much to finish
        startTime = Sys.time()
        
        
        # For each consequence variable...
        for (j in 1:totalConsequenceIndexes) {
            
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[j]
                currentName  = consequenceNames[j]        
            }
            
            # Give feedback to the user
            print("----------------------------")
            print( paste0(" Doing index: ", currentName, " ", round(100*j / totalConsequenceIndexes ,2), "%" ))
            print("----------------------------")
            
            # Prepare the network list
            simulationByVariablesDFList[[j]] = newList(totalNetworks)

            # For each of the network...
            for(k in 1:totalNetworks){

                # Prepare the blank dataframe
                simulationByVariablesDFList[[j]][[k]] = blankSimulationResultsDF
                                    
                # Get the current edges
                currentEdges = listOfEdgesDF[[k]]
                currentNetwork = myNetworkNames[k]
                
                # Get the index and name
                currentConsequenceIndex = consequenceIndexes[j]
                currentConsequenceName  = consequenceNames[j]
      
                # Tell the user where are you 
                print("------------------------")
                print( paste0(" Doing network: ", currentNetwork, " ", round(100*k / totalNetworks ,2), "%" ))
                print("------------------------")
                
                # Get the results from the pre-simulations, these are the
                # unbias values and the total relationships values
                currentTotalRelationships     = biasResultsDF[[j]][k,2]
                currentTotalSameRelationships = biasResultsDF[[j]][k,3]
                currentUnbiasSameMinimum      = biasResultsDF[[j]][k,4]
                currentUnbiasSameAverage      = biasResultsDF[[j]][k,7]
                
                # For each row in the table
                for(i in 1:totalModalities){

                    # Get the variable index and modality name
                    currentVariableName  = simulationByVariablesDFList[[j]][[k]][i,1]
                    currentModalityName  = simulationByVariablesDFList[[j]][[k]][i,2]
                    currentVariableIndex = simulationByVariablesDFList[[j]][[k]][i,3]
                    
                    # Give some feedback to the user
                    # -- Time to finish
                    currentTime          = Sys.time()
                    secondsFromStart     = as.numeric(currentTime-startTime,units="secs")
                    proportionFromStart  = ((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)
                    percentagePerSecond  = proportionFromStart / secondsFromStart
                    secondsToFinish      = ((1 - proportionFromStart) * secondsFromStart) / proportionFromStart
                    # -- String to user
                    print("--------------------")
                    print( paste0(" Doing row: ", currentModalityName, " / ", currentVariableName, " - " ,  round(100 * i / totalModalities ,2), "%" ))
                    print("--------------------")
                    print( paste0(" Total: ",   round(100 * (((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)) , 2),  "%" ))
                    print( paste0(" Finish in: ", secondsToFinish))
                    print("--------------------")

                    
                    # (SPECIAL PART)
                    #
                    # We need to run a bootstrap where the men stay with constant
                    # men frequency, and women change the frequency according to
                    # the case of contraceptives that we are in
                    currentBiasResult = getContraceptiveVector(nodesTable, currentEdges, currentConsequenceIndex, totalSimulations,
                                                               mySexIndex                = mySexIndex,
                                                               myHormonalIndex           = myHormonalIndex)
                    
                    # Get how much is the average and sd of the simulations
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write everything into the current results dataframe
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  4 ] = currentNetwork
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  5 ] = currentTotalRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  6 ] = currentTotalSameRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  7 ] = currentUnbiasSameAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  8 ] = currentUnbiasSameMinimum
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  9 ] = currentAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 10 ] = currentSD
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 11 ] = currentPValue
                    
                }

            }
            
            
        }        

        # Everything is simulated at this point.
        #
        # Now that this is finish, we have the base value for each modality
        # and we can run the CI columns in each result. So we do the same run
        # again, for each consequence index, for each network, for each row:
        
        for (j in 1:totalConsequenceIndexes){
            
            for(k in 1:totalNetworks){
                
                # Get the default base values
                currentReferenceIndex = simulationByVariablesDFList[[j]][[k]][1,3]
                currentBaseValue      = simulationByVariablesDFList[[j]][[k]][1,9]
                currentOrderValue     = 1
                
                
                for(i in 1:totalModalities){
                    
                    # Compare the average with the base value, if they are the same, this is the reference and is equal to 1 
                    simulationByVariablesDFList[[j]][[k]][i,12] = simulationByVariablesDFList[[j]][[k]][i,9] / currentBaseValue
        
                    # Get the lower and upper intervals for a CI of 95%
                    # -- If you are the base value, you don't have a confident interval
                    if(currentOrderValue == 1){
                        simulationByVariablesDFList[[j]][[k]][i,13] = NA
                        simulationByVariablesDFList[[j]][[k]][i,14] = NA
                    }
                    # -- Every other modality of that categorical variable, has a CI
                    else{
                        simulationByVariablesDFList[[j]][[k]][i,13] = ( (-confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                        simulationByVariablesDFList[[j]][[k]][i,14] = ( (+confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                    }
          
                    # Now go to the next row, but we need to check if we are still
                    # studying modalities of the same variable, or if we have jump
                    # to another different variable.
        
                    # Is there a next index?
                    # -- If yes...

                    if(i < totalModalities){
                        # Which one?
                        nextReferenceIndex = simulationByVariablesDFList[[j]][[k]][(i+1),3]
                        
                        # Is the same as the current?
                        # -- If yes...
                        #    Take the next order
                        if(currentReferenceIndex == nextReferenceIndex){
                            
                            currentOrderValue = currentOrderValue + 1
            
                        }
                        # -- If no...
                        #    Change the base value and reference index, reset order to 1
                        else{
                            
                            currentOrderValue     = 1
                            currentReferenceIndex = nextReferenceIndex
                            currentBaseValue      = simulationByVariablesDFList[[j]][[k]][(i+1),9]
            
                        }
          
                    }
                    # -- If no... we are finish
                    
                    
                }
                
            }
            
        }
        
        
        # Finally, everything is done, we just return the final list
        return(simulationByVariablesDFList)

        
    }
    
    
    
  }


  # ---------------------------------
  #     HEATMAPS
  # ---------------------------------
  {

    doPValuesHeatmap <- function(tableBase, plotFilePath,
                                 plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                 plotTheme = NULL,
                                 overrideTableName = NULL){

      # Define plot type
      myPlotType  = "pValuesHeatmap"
      myTableName = deparse(substitute(tableBase))

      # If you need to override your table name, then do it
      if(!is.null(overrideTableName)){
        myTableName = overrideTableName

      }

      # Get an automatic name if you don't have a proper one
      {

        genericFilePath = automaticFilePath( plotFilePath, tableBase, tableName = myTableName, plotType = myPlotType)

        if(genericFilePath == plotFilePath) genericFilePath = getFilePathNoExtension(genericFilePath)

        imageFilePath   = paste(genericFilePath,".png",sep="")
        tableFilePath   = paste(genericFilePath,".txt",sep="")
        latexFilePath   = paste(genericFilePath,".tex",sep="")

      }

      # Get the theme information
      themeData = getThemeParameters(plotTheme)

      # Get info about the table
      modalitiesNames = tableBase[,1]
      totalModalities = nrow(tableBase)
      totalX = totalModalities
      totalY = totalModalities


      # Init a matrix with the total for each combination
      matrixCount = matrix(0, nrow = totalY, ncol = totalX)

      # Make a new dataframe so we can draw the heatmap
      # Category X / Category Y / Total / X coordinate / Y coordinate / Color of tile / Size for text
      totalRows              = totalX * totalY
      heatmapTable           = data.frame(matrix(NA, nrow = totalRows, ncol = 8))
      colnames(heatmapTable) = c( "CategoryX", "CategoryY", "Total" , "Xcoordinate" , "Ycoordinate" , "myColor" , "Size", "Label" )
      absoluteCount          = totalModalities

      # Fill the dataframe
      for(i in 1:totalX){

        for(j in 1:totalY){

          # Get the current line and categories
          currentLine = (i-1)*totalY + j

          currentXCategory = modalitiesNames[i]
          currentYCategory = modalitiesNames[j]

          # Init the row in the dataframe
          heatmapTable$CategoryX[currentLine] = currentXCategory
          heatmapTable$CategoryY[currentLine] = currentYCategory

          # Count how many we have
          heatmapTable$Total[currentLine]     = tableBase[[i,j+1]]
          # -- Transform numbers
          # ---- Negative very close to 0 goes to -1
          if(heatmapTable$Total[currentLine] < 0) heatmapTable$Total[currentLine] = -1 - heatmapTable$Total[currentLine]
          if(heatmapTable$Total[currentLine] > 0) heatmapTable$Total[currentLine] =  1 - heatmapTable$Total[currentLine]


          # ---- Positive very close to 0 goes to +1

          # Set the proper X / Y coordinates
          heatmapTable$Xcoordinate[currentLine] = i
          heatmapTable$Ycoordinate[currentLine] = j

          # Set the manual color and size for the text (not in use)
          heatmapTable$myColor[currentLine] = "Red"
          heatmapTable$Size[currentLine]    = 1

          # Set the label
          # heatmapTable$Label[currentLine] = round(heatmapTable$Total[currentLine],2)
          heatmapTable$Label[currentLine] = getAsterkisPValue(tableBase[[i,j+1]])

        }

      }

      cols = brewer.pal(n = 5, name = "RdBu")

      blueColors = brewer.pal(n = 3, name = "PuBu")
      redColors  = brewer.pal(n = 3, name = "OrRd")
      lastBlue   = blueColors[3]
      lastRed    = redColors[3]
      whiteColor = "#FFFFFF"
      blackColor = "#000000"

      #joinColors = c(whiteColor, whiteColor, redColors, lastRed, blackColor, lastBlue, rev(blueColors), whiteColor, whiteColor)

      joinColors = c(rev(redColors), whiteColor, whiteColor, whiteColor, blueColors)

      #print(joinColors)

      ggplot( heatmapTable, aes( x = Xcoordinate, y = Ycoordinate, fill = Total )) +

              # The background rectangle
              geom_tile(color = "black") +
              # What is written inside the rectangle
              geom_shadowtext( aes(label= Label), color = "white", fontface = "bold") +
              #geom_shadowtext( aes(label= round(Total, 4)), color = "white", fontface = "bold") +
              #geom_shadowtext( aes(label= Total), color = "white", fontface = "bold") +

              # What is written in the X axys
              geom_text(aes(label = CategoryX, y = 0.3), angle = 90, hjust = 1) +
              # What is written in the Y axys
              geom_text(aes(label = CategoryY, x = 0.3), hjust = 1) +

              # Give an extra space so we can see the labels clearly
              coord_cartesian( xlim = c(-1, totalX + 1),
                               ylim = c(-1, totalY + 1)) +

              # Force the leyend to be in between -1 and +1
              # scale_fill_gradient2(limits = c(-1, 1)) +

              # scale_fill_gradientn(colours = joinColors,
              #                      values  = rescale(c(-1, -0.1, -0.05, -0.01, -0.001, -1/1e100, 0, 1/1e100, 0.001, 0.01, 0.05, 0.1, 1)),
              #                      guide   = "colorbar", limits=c(-0.05, 0.05)) +

              scale_fill_gradientn(colours = joinColors,
                                   values  = rescale(c(-1, -0.999, -0.99, -0.95, -0.94, 0, 0.94, 0.95, 0.99, 0.999, 1)),
                                   guide   = "colorbar", limits=c(-1, 1)) +

              #scale_colour_gradient(limits = c(-1, 1)) +

              #scale_fill_gradient(limits = c(2, 4))

              # Create titles and subtitles
              labs(title    = plotTitle,
                   subtitle = plotSubtitle,
                   caption  = plotCaption,
                   color    = "p-value",
                   x = plotXLabel, y = plotYLabel) +

              # Apply the theme
              # theme(panel.background   = themeData[[1]],
              #       axis.line          = themeData[[2]],
              #       panel.grid.major.y = themeData[[3]],
              #       panel.grid.major.x = themeData[[4]],
              #       legend.position    = themeData[[5]])

              # Remove the background default lines and grey panel
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line        = element_blank(),
                    axis.text.x      = element_blank(),
                    axis.text.y      = element_blank(),
                    axis.ticks       = element_blank())


      # Save everything
      ggsave(imageFilePath)

      print(imageFilePath)

    }

  }

}




