# Load libraries
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/analysis/analysisCategorical.R"),   encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingCommon.R"),   encoding="utf-8")


# Boxplot with optional p-values under it
#
# - tableBase (dataframe) A table where your data is contained
#
# - variableIndex (int) column's index with the numerical values
#
# - groupIndex    (int) column's index with the categorical values.
#                       Default is NULL, meaning that you only want to do a simple boxplot
#
# - outlierShape  (int) The form of the outlier in the boxplot.
#                       Default is 19
#
# - showPValues    (bool) Show the p-values under the boxplot
#
# - pValuesFormat (string) "simple" (DEFAULT) Shows the asterisk format for p-values
#                          "number2"          Shows the number rounded to two
#                          "number3"          Shows the number rounded to three
#                          "number4"          Shows the number rounded to four
#                          "all"              Shows the entire number
#
# - significantPValues (float) Only show p-values that are under this threshold
#                              default is 1, so it will show all p-values.
doBoxPlotV2 <- function(tableBase, variableIndex, plotFilePath,
                        groupIndex = NULL,
    
                        # p-values options
                        hypothesis = "two", type = "welch",
                        supressWarnings = TRUE,
    
                        # plots options
                        outlierShape = 19,
                        cutOffLine   = NULL,
                        colorsVector = NULL,
                        showPValues  = TRUE, pValuesFormat = "simple",
						showANOVA    = TRUE,
                        significantPValue = 1,
                        ymin = NULL, ymax = NULL,
                        plotTitle = NULL, plotSubtitle = NULL,
                        plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                        plotTheme = NULL,
                        angleXLabels = 0,
                        overrideImageWidth  = NULL,
                        overrideImageHeight = NULL,
                        overrideScaleToZero = FALSE, # Why is this here? ymin = 0 does the same
                        overrideTableName = NULL,
                        overrideCaption   = NULL,
    
                        # Final Summary Table
                        roundMean   = 2,
                        roundMedian = 2,
                        roundSigma  = 2,
                        roundPvalue = 4
    
                        ){
    
    # Define plot type
    myPlotType  = "CategoricalBoxplot"
    if(is.null(groupIndex)) myPlotType = "Boxplot"
    
    # Get the table name and override it if needed
    myTableName = deparse(substitute(tableBase))
    if(!is.null(overrideTableName)) myTableName = overrideTableName

    # Get an automatic name if you don't have a proper one
    {

        imageFilePath = ""
          
        myFileExtension = getFileExtension(plotFilePath)
          
        if(myFileExtension == ".png") imageFilePath = plotFilePath
          
        else{
            
            if(myPlotType == "CategoricalBoxplot"){
            
                imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                  tableName = myTableName, fileType = myPlotType,
                                                  variableIndex1 = groupIndex, variableIndex2 = variableIndex)
            }
            else{

                imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                  tableName = myTableName, fileType = myPlotType,
                                                  variableIndex1 = variableIndex)
            }

        }
        
    }

    # Get the theme information
    themeData = getThemeParameters(plotTheme)

    # Prepare the Y axis limits
    if(is.null(ymin)) ymin = min(tableBase[,variableIndex], na.rm= TRUE)
    if(is.null(ymax)) ymax = max(tableBase[,variableIndex], na.rm= TRUE)    

    # Init some variables
    nCategories   = 1
    groupingName  = ""
    numericalName = colnames(tableBase)[variableIndex]
    # And check whether you have categories or not
    if(!is.null(groupIndex)){
         
        myCategories  = getCategories(tableBase, groupIndex)
        nCategories   = length(myCategories)
                        
        # Gives the X label to grouping name if it exist
        if((str_trim(plotXLabel) != "")&&(!is.null(plotXLabel))){
            groupingName  = plotXLabel
        }
        # Otherwise, whatever is the name of that column
        else{
            groupingName  = colnames(tableBase)[groupIndex]
        }
         
    }
    
    
    # Do the analysis of your data
    {
        
        anovaLabel       = ""
        continueFunction = FALSE
        analysisResults  = simpleCategoricalPValueV2(tableBase, groupIndex, variableIndex,
                                                    hypothesis = hypothesis, type = type,
                                                    supressWarnings = supressWarnings)
    
        # Check that we have a valid results
        if(analysisResults[[2]] < 0){
        
            print("WARNING: Something when wrong")
            
            if(analysisResults[[2]] == -1) print("         You don't have enough categories")
            if(analysisResults[[2]] == -2) print("         You don't have enough samples in one of the categories")
            if(analysisResults[[2]] == -3) print("         Two categories have the same values")
            
            
        }
        else{
                  
            anovaLabel = analysisResults[[2]]
            if(anovaLabel < 0.0001) anovaLabel = " < 0.0001"
            else anovaLabel = round(anovaLabel,4)
            anovaLabel = paste0("owANOVA: ",anovaLabel)  
            continueFunction = TRUE
            
        }
        

    }
    
    # If everyting is ok, continue with the function
    myBoxPlot        = 0
    summaryADF       = 0
    summaryBDF       = 0
    
    if(continueFunction == TRUE){
    
        # Get the default style for the plot
        {
        
            defaultVector = NA
            if(nCategories == 1){
                    
                defaultVector = getNumericalDefaults(numericalName,
                                                     colorsVector = colorsVector, plotTitle   = plotTitle,
                                                     plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                     plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel)
                    
        
            }
            else{
                
                defaultVector = getCategoricalNumericalDefaults(groupingName, myCategories, numericalName,
                                                                colorsVector = colorsVector, plotTitle   = plotTitle,
                                                                plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                                plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                                fileType = myPlotType)
            }
            
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
    
        }
        
        # Prepare the dataframe with the horizontal lines
        # that are under each box combination
        {
            horizontalDF = NA
            if(showPValues == TRUE){
                
                # Alias the results dataframe  (which is not a real alyas, because R is shit, and need to copy everything!)
                pValuesDF = analysisResults[[4]]
                
                # Init the p-values labels and lines DF
                {
                    totalRows              = choose(nCategories,2) # All possible combinations 2 by 2 without repetition
                    horizontalDF           = data.frame(matrix(0, nrow = totalRows, ncol = 4))
                    colnames(horizontalDF) = c("StartX", "EndX", "StartY", "pvalue")
                }        
    
                
                # ---- Find where we can plot the p-values labels
                {
                    yMinimum         = min(tableBase[,variableIndex], na.rm=T)
                    yMaximum         = max(tableBase[,variableIndex], na.rm=T)
                    distanceRelative = yMaximum - yMinimum
                        
                    constantSpace    = (distanceRelative*0.1) # Arbitrary number here
                    upperBound       = yMinimum
                    lowerBound       = upperBound - (constantSpace * totalRows * 1.2) # 1.2 because I say so, trying to making look nice with an arbitrary number
                    firstStep        = upperBound
                }          
                
                # ---- Calculate the coordinates for each of the lines
                currentLine = 1
    
                # For each category
                pValueCandidate = 0 # Need to init this variable here
                
                for(i in 1:nCategories){
                
                    # For each other category
                    for(j in 1:nCategories){
                        
                        # (we starts a i+1 so the lines are sorted nicely
                        if(j>i){
                        
                            # Get the p-value label
                            roundedPValue = "none"
                            
                            pValueCandidate = pValuesDF[j,i+1]
                            
                            if(pValuesFormat == "simple")  roundedPValue = getAsterkisPValue(pValueCandidate)
                            if(pValuesFormat == "number2") roundedPValue = signif(pValueCandidate,2)
                            if(pValuesFormat == "number3") roundedPValue = signif(pValueCandidate,3)
                            if(pValuesFormat == "number4") roundedPValue = signif(pValueCandidate,4)
                            if(pValuesFormat == "all")     roundedPValue = pValueCandidate
                              
                            # Get where the lines start and end
                            horizontalDF$StartX[currentLine] = i
                            horizontalDF$EndX[currentLine]   = j
                              
                            horizontalDF$StartY[currentLine] = firstStep - ( currentLine * constantSpace )
                            horizontalDF$pvalue[currentLine] = roundedPValue
                            
                            # Finally, check if this is a valid p-value, otherwise keep this line
                            # and continue
                            if(pValueCandidate < significantPValue) currentLine = currentLine + 1                        
                            
                        }                    
                        
                    }
                   
                }
    
                # We need to fix the last line
                if(currentLine < totalRows){
                
                    if(horizontalDF[currentLine,4] > pValueCandidate){
                        horizontalDF[currentLine,1] = 0
                        horizontalDF[currentLine,2] = 0
                        horizontalDF[currentLine,3] = 0
                        horizontalDF[currentLine,4] = 0
                    }
                
                }
    
                # Destroy the rows that start with 0, otherwise you get a weird 0 at (0,0)
                horizontalDF = horizontalDF[horizontalDF[,1] != 0,]
    
                # If we are going to write the p-values, we need to lower the ymin limit where we draw the plot
                # Otherwise it won't show anything    
                if(nrow(horizontalDF) > 0) # We need at least one line to show, otherwise the stupid fuck R language doesn't even tell you that there is an error of trying to find A FUCKING NUMER, WHICH IS A MINIMUN THAT DOESN?T EXIST!!!!. Tell me, what the fuck is the minimun of an empty set you fucking piece of shit designer!???, TELL ME! FUCK YOU!!!! And then, tell me that is that number minus a constant. I hope you die horribly.
                    ymin = min(horizontalDF$StartY) - constantSpace/3
                
            }
          
        }    
        
        # Init the plot object
        myBoxPlot = NA
        
        # If we are not grouping
        if(nCategories == 1)
        
            print("TODO")
            
        # If we are grouping
        else{
    
            # Do the plot
            myBoxPlot = ggplot()+
                
            # ---- Boxplot
            geom_boxplot(data = tableBase, aes(x = tableBase[,groupIndex], y = tableBase[,variableIndex], fill = tableBase[,groupIndex]), outlier.shape = outlierShape) +
            scale_fill_manual(values=colorsVector) +
                
            # ---- With tiny points for the samples
            geom_jitter(data = tableBase, aes(x = tableBase[,groupIndex], y = tableBase[,variableIndex], fill = tableBase[,groupIndex]), position = position_jitterdodge(), color="black", size=0.2, alpha=0.1) +
                
            # ---- Apply the Y limits
            scale_y_continuous(limits=c(ymin, ymax)) +
                
            # ---- Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 fill     = groupingName,
                 x = plotXLabel, y = plotYLabel)
            
            # Apply the theme
            # The difference here is whether or not you want angled labels or not
            # -- Non angle labels
            if(angleXLabels == 0){
    
                myBoxPlot = myBoxPlot +
                
                theme(panel.background   = themeData[[1]],
                      axis.line          = themeData[[2]],
                      panel.grid.major.y = themeData[[3]],
                      panel.grid.major.x = themeData[[4]],
                      legend.position    = themeData[[5]])
                            
            }
            # -- Angled labels
            else{
                
                myBoxPlot = myBoxPlot +
    
                theme(panel.background   = themeData[[1]],
                      axis.line          = themeData[[2]],
                      axis.text.x        = element_text(angle = angleXLabels, vjust = 0.95, hjust = 0.95),
                      panel.grid.major.y = themeData[[3]],
                      panel.grid.major.x = themeData[[4]],
                      legend.position    = themeData[[5]])
                
            }
    
            # The plot is finished here
            # We can add now optional stuff to it:
            
            # Add the p-values under the boxes
            if(showPValues == TRUE){
    
                # If we DO have p-values to show
                if( nrow(horizontalDF) > 0 ){
    
                    myBoxPlot = myBoxPlot +
                  
                    # ---- With the actual p-values under it
                    # -------- Add the relative segments
                    geom_segment(data = horizontalDF, aes(x = StartX,  y = StartY, xend = EndX, yend = StartY) ) +
                    # -------- Add nice accotation segments
                    geom_segment(data = horizontalDF, aes(x = StartX,  y = StartY + constantSpace/5, xend = StartX, yend = StartY - constantSpace/5) ) +
                    geom_segment(data = horizontalDF, aes(x = EndX,    y = StartY + constantSpace/5, xend = EndX,   yend = StartY - constantSpace/5) ) +
                    # -------- Add the pvalues text
                    geom_text(data = horizontalDF, aes(x = (StartX + EndX)/2 , y = StartY - constantSpace/3, label = pvalue) , color="black")                
                    
                }
                
            }
            
            # Show the cut line
            if(!is.null(cutOffLine)){
            
                # The cutOffLine is a list of size 2, that contain a vector of size N in each square
                #
                # The first list is the intercept values
                # The second list is the variable names
                # The third list is the colors for each line (mandatory no default yet)
                cutOffData = data.frame(yintercept = cutOffLine[[1]], Lines = cutOffLine[[2]])
                myBoxPlot  = myBoxPlot + 
                    
                                 new_scale_color() +
                                 geom_hline( data = cutOffData, aes(yintercept = yintercept, color = Lines)) +
                                 scale_colour_manual(values = cutOffLine[[3]])
                             
            }
                    
            # If we have more than two categories, we need to show the owANOVA
            
            if(showANOVA == TRUE){
            
            	if(nCategories>2){
            		myBoxPlot = myBoxPlot +  annotate("text", x = 0, y = ymax, label = anovaLabel , color = "blue", parse = FALSE, hjust = 0, vjust = 1  )
    
            	}
            	
            }
            
        }
        
        # Decide how big the image should be
        {
            
            # Count the number of characters in the title
            maximumTitlechars    = nchar(plotTitle)
            
            # Count the number of characters in the categories in order to decide the image width factor
            maximumCategoryChars = max(nchar(as.character(myCategories)))
            maximumCategoryChars = max(maximumCategoryChars, 8) # Tiny names looks weird, let take 8 as minimum char size        
    
            # Get the default values for with and height
            imageWidth  = maximumCategoryChars * 0.1 * nCategories + maximumCategoryChars * 0.2 # The multiplying number is arbitrary based on what I think look best
            imageWidth  = max(imageWidth, maximumTitlechars * 0.1) # + Add long title correction to image size                
                
            imageHeight = 8        
                    
            # But override if the user want to
            if(!is.null(overrideImageWidth))  imageWidth  = overrideImageWidth
            if(!is.null(overrideImageHeight)) imageHeight = overrideImageHeight
        }
        
        # Save the image
        ggsave(imageFilePath, plot = myBoxPlot, width = imageWidth, height = imageHeight)
        ggsave(changeFileExtension(imageFilePath, "pdf"), plot = myBoxPlot, width = imageWidth, height = imageHeight)  
                
        # Prepare a summarizing table with the info
        # All of this is already in the analysis result, but is a summary version of that
        # specially useful for the latex summary
        {
         
            # Return two DFs
            
            #             N      Mean    Median     Sigma
            # ----------------------------------------------
            #        A   50    34.23      35.9       5.6
            #        B   20  
            #        C   60
            # ----------------------------------------------
            #     Total 130    Mean     Median      Sigma
    
            #             A      B 
            # --------------------------
            #        B   0.04    0.45  
            #        C   0.01    0.2
            # --------------------------
            # owANOVA  0.01        
            
            dataInfo = analysisResults[[3]]
            pVInfo   = analysisResults[[4]]
    
            totalModalities = nrow(dataInfo)
            modalitiesNames = dataInfo[,1]
    
            summaryADF          = DF(totalModalities + 1, 5 )
            colnames(summaryADF) = c(" ", "N", "Mean", "Median", "Sigma")
            summaryBDF           = DF(totalModalities, totalModalities)
            colnames(summaryBDF) = c(" ", modalitiesNames[1:(totalModalities-1)])
            
            # Start filling
            # ---- A
            for(i in 1:totalModalities){
                # Name
                summaryADF[i,1] = modalitiesNames[i]
                
                # N
                summaryADF[i,2] = dataInfo[i,6]
                
                # Mean
                summaryADF[i,3] = round(dataInfo[i,2], roundMean)
                
                # Median
                summaryADF[i,4] = round(dataInfo[i,3], roundMedian)
                
                # Sigma
                summaryADF[i,5] = round(dataInfo[i,5], roundSigma)            
            }
            # Find the total
            summaryADF[(totalModalities+1),1] = "Total"
            summaryADF[(totalModalities+1),2] = sum(summaryADF[1:totalModalities,2])
            summaryADF[(totalModalities+1),3] = round(mean(   tableBase[,variableIndex]     , na.rm = TRUE ) , roundMean   )
            summaryADF[(totalModalities+1),4] = round(median( tableBase[,variableIndex]     , na.rm = TRUE ) , roundMedian )
            summaryADF[(totalModalities+1),5] = round(sd(     tableBase[,variableIndex]     , na.rm = TRUE ) , roundSigma  )             
    
            # ---- B
            for(i in 1:(totalModalities - 1)){
                
                # Name
                summaryBDF[i,1] = modalitiesNames[i+1]            
                
                # P-values
                for(j in 1:(totalModalities-1)){
    
                    if(i>=j){
                        summaryBDF[i,j+1] = properPValue(pVInfo[i+1,j+1], roundMe = roundPvalue)
                        
                    }
                    else{
                        summaryBDF[i,j+1] = "" # You want to know also why R is shit? Because if you init the DF to NA then everything is a number and you can sum by columns later
                    }                         # but if you init to "", then everything is a char, and you can't add columns, even though you have transform everything to num along
                                              # the way. This is the most retarded way of handling matrices and dataframes that I have ever use in my life.
                }
                
            }
    
            summaryBDF[totalModalities,1] = "owANOVA"
            summaryBDF[totalModalities,2] = properPValue(analysisResults[[2]], roundMe = roundPvalue)           # I HATE YOU! Sometimes scientific notation, sometimes don't. And there is no way to tell when you are going to pull which out of your aRse
            # Delete the extra NA in the last row
            for(j in 3:ncol(summaryBDF)){
            
                summaryBDF[nrow(summaryBDF),j] = ""
                    
            }
            
        }
    }
    
    
    # Return
    myReturn = vector("list", length = 5)
    myReturn[[1]] = myBoxPlot
    myReturn[[2]] = imageFilePath
    myReturn[[3]] = analysisResults
    myReturn[[4]] = summaryADF
    myReturn[[5]] = summaryBDF    
    
    return (myReturn)            
    
}