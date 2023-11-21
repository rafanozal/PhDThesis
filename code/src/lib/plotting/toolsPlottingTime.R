library(tseries)

# This function takes a dataframe where you have multiple observations per time, and reduce it to a single
# datapoint per time. Optionally, is divided by group if you want a particular variable to be separated.
timeReduction <- function(tableBase, dateIndex, variableIndex, groupIndex = NULL, reductionMethod = "mean"){
    
}


#doTimeSeriesRegression


        # Boxplot with optional p-value under it
        #
        # - variableIndex is the column index with the numerical values
        # - groupIndex    is the column index with the categorical values
        #
        # groupIndex can be NULL. Then, you won have any grouping of course and
        #                         you just have a boxplot of an univariable case
        doTimeSeriesNumericalPlot <- function(tableBase, dateIndex, variableIndex, plotFilePath,
                                              groupIndex = NULL,
                                              outlierShape = 19,
                                              cutOffLine   = NULL,
                                              colorsVector = NULL, showPValues = TRUE,
                                              ymin = NULL, ymax = NULL,
                                              plotTitle = NULL, plotSubtitle = NULL,
                                              plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                              plotTheme = NULL,
                                              angleXLabels = 0,
                                              overrideImageWidth  = NULL,
                                              overrideImageHeight = NULL,
                                              overrideScaleToZero = FALSE,
                                              overrideTableName = NULL,
                                              overrideCaption   = NULL){

            # Define plot type
            myPlotType  = "NumericalTimeSeries"
            myTableName = deparse(substitute(tableBase))

            # If you need to override your table name, then do it
            if(!is.null(overrideTableName)) myTableName = overrideTableName

            # Get an automatic name if you don't have a proper one
            {

                imageFilePath = ""
          
                myFileExtension = getFileExtension(plotFilePath)
          
                if(myFileExtension == ".png") imageFilePath = plotFilePath
          
                else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                       tableName = myTableName, fileType = myPlotType,
                                                       variableIndex1 = dateIndex, variableIndex2 = variableIndex)

            }

            # Get the theme information
            themeData = getThemeParameters(plotTheme)

            # Prepare the Y axis limits
            if(is.null(ymin)) ymin = min(tableBase[,variableIndex], na.rm= TRUE)
            if(is.null(ymax)) ymax = max(tableBase[,variableIndex], na.rm= TRUE)



            # ---------------------------------------------------------
            # GET INFO ABOUT THE DIFFERENT VARIABLES
            # ---------------------------------------------------------
            # ---- Info about the numerical variable
            numericalName = colnames(tableBase)[variableIndex]
            # ---- Info about the date variable
            dateName      = colnames(tableBase)[dateIndex]
            # ---- Info about the grouping variable, if any
            nCategories   = 1
            groupingName  = NA
            if(!is.null(groupIndex)){
                groupingName  = colnames(tableBase)[groupIndex]    
                myCategories  = levels(groupIndex)
                nCategories   = length(myCategories)
            }
            


            # ---------------------------------------------------------
            # DIVIDE THE DATA INTO SUBSETS IF ANY
            # ---------------------------------------------------------
            # If there are categories, divide them into several subset
            subsetsDF = NA
            if(nCategories > 1){
            
                # Create as many subsets as there are categories, and put them into this list
                subsetsDF     = rep(list(data.frame(NULL)), nCategories)

                for(i in 1:nCategories){
            
                    subsetsDF[[i]] = subset(tableBase, tableBase[,groupIndex] == as.character(myCategories[i]))
            
                }
                                    
                
            }
            
            
            


            # Init the analysisDF, it will have this format:
            # In here we put all the analysis info, such as p-values, ANOVAS, or whatever
            #
            # -------------------------------------------------------
            #                               | p-value |
            # -------------------------------------------------------
            # augmented Dickey-Fuller test
            #     (Category 1)
            #     (Category 2)
            #         ....
            # Other tests
            #     (Category 1)
            #     (Category 2)
            #         ....
            #
            # Or this other format:
            #
            # -------------------------------------------------------
            #                               | p-value |
            # -------------------------------------------------------
            # augmented Dickey-Fuller test
            # Other tests
            #
            analysisDF     = NULL
            analysisDF = DF(nCategories + 1, 2)
            colnames(analysisDF) = c("Test", "P-value")
            analysisDF [1,1] = "Augmented Dickey-Fuller test"
            
            if(nCategories == 1){
             
                ADFTestResults = adf.test( tableBase[,variableIndex])
                   
            }
            else{
                
                for(i in 1:nCategories){
                    
                  analysisDF[i+1,1] = myCategories[i]
                    
                }
                
                
            }
            
            
            
            
            
            # Init the centralities dataframe, it will have this format
            #
            # -------------------------------------------------------
            #                               | Mean | Median |
            # -------------------------------------------------------
            #     (Category 1)
            #     (Category 2)
            #         ....
            #
            # Or this other format:
            #
            # -------------------------------------------------------
            #                               | Mean | Median |
            # -------------------------------------------------------
            #     Variable name
            #
            # Init the centralities, even though it could be impossible to do them
            centralitiesDF           = DF(nCategories, 3)
            colnames(centralitiesDF) = c("", "Mean", "Median")
            # -- If we only have one, just do the general mean and median
            if(nCategories == 1){
                centralitiesDF[1,1] = numericalName
                centralitiesDF[1,2] = mean(tableBase[,variableIndex],   na.rm = TRUE)
                centralitiesDF[1,3] = median(tableBase[,variableIndex], na.rm = TRUE)
            }
            # -- If we have more than one, do the mean and median for each category
            else{
                
                for(i in 1:nCategories){
                    
                    samplesI      = subsetsDF[[i]][,variableIndex]
                    
                    centralitiesDF[i,1] = myCategories[i]
                    centralitiesDF[i,2] = mean(samplesI,   na.rm = TRUE)
                    centralitiesDF[i,3] = median(samplesI, na.rm = TRUE)
                    
                }
                
            }
            

      
            # Finally, do the actual plot
            # ---- Init the plot object
            myTimeSeriesPlot            = NA
            # ---- Prepare the defaults
            {

                defaultVector = NA
                # For the case of no categories
                if(nCategories == 1){

                    defaultVector = getTimeSeriesNumericalDefaults(dateName, numericalName,
                                                                   colorsVector = colorsVector, plotTitle   = plotTitle,
                                                                   plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                                   plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel,
                                                                   fileType = myPlotType)                    
                                        
                }
                # For the case of several categories
                else{
                    
                    defaultVector = getTimeSeriesNumericalCategoricalDefaults(dateName, numericalName, myCategories,
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
            # ---- Change the yLimit if you want the scale to be forced to zero
            if(overrideScaleToZero == TRUE) ymin = 0

            # Do the plot
            myTimeSeriesPlot = ggplot()+
            
ggplot(data=completeTable, aes(x = completeTable[, bloodExtractionDateIndex], y = completeTable[,vitamimDIndex])) +
    geom_point()                
                
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
            # The difference here is whether or not you want angled labels
            if(angleXLabels == 0){

                myBoxPlot = myBoxPlot +
            
                theme(panel.background   = themeData[[1]],
                      axis.line          = themeData[[2]],
                      panel.grid.major.y = themeData[[3]],
                      panel.grid.major.x = themeData[[4]],
                      legend.position    = themeData[[5]])
                        
            }
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
            if(showPValues == TRUE){
            
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
        
            # -- Show the cut line
            if(!is.null(cutOffLine)){
        
                # The cutOffLine is a list of size 2, that contain a vector of size N in each square
                #
                # The first list is the intercept values
                # The second list is the variable names
                # The third list is the colors for each line (mandatory no default yet)
            
                cutOffData = data.frame(yintercept = cutOffLine[[1]], Lines = cutOffLine[[2]])
                myBoxPlot  = myBoxPlot + 
                
                             new_scale_color() +
                
                             geom_hline( data = cutOffData, aes(yintercept = yintercept, color = Lines, linetype = Lines)) +
                
                             scale_colour_manual(values = cutOffLine[[3]])
                         
            }
        
            # Save the image
            {
                # Get the default values for with and height
                imageWidth  = maximumCategoryChars * 0.1 * nCategories + maximumCategoryChars * 0.2 # The multiplying number is arbitrary based on what I think look best
                imageWidth  = max(imageWidth, maximumTitlechars * 0.1) # + Add long title correction to image size                
            
                imageHeight = 8
            
                # But override if the user want to
                if(!is.null(overrideImageWidth))  imageWidth  = overrideImageWidth
                if(!is.null(overrideImageHeight)) imageHeight = overrideImageHeight
            }
            
            ggsave(imageFilePath, plot = myBoxPlot, width = imageWidth, height = imageHeight)
            
            
            # Return
            myReturn = vector("list", length = 4)
            myReturn[[1]] = myBoxPlot
            myReturn[[2]] = pValuesDF
            myReturn[[3]] = centralitiesDF
            myReturn[[4]] = imageFilePath
        
            return (myReturn)
            
            
        }
