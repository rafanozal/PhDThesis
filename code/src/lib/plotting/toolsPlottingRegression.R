# Load libraries
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/analysis/analysisRegression.R"),   encoding="utf-8")

# This script contain all the plotting function related to regression.
# This includes
#
# ---- Regression of two numerical variables
# ---- Regression of two numerical variables grouped by a category
#
# ---- LOESS regression by dates and numbers
# ---- LOESS regression by dates and numbers grouped by a category

# Get two numerical columns and make the regression between then
doRegressionPlot <- function(tableBase, independentColumnIndex, dependentColumnIndex, plotFilePath,
                             groupingIndex = NULL,
							 highlightCategory = NULL,
                             model = "Cuadratic",
                             colorsVector = NULL, 
                             showRegressionLine = TRUE,
	                         myRounding = 2, # Rounding of the display formula
                             ymin = NULL, ymax = NULL,
                             xmin = NULL, xmax = NULL,
                             horizontalLinesCuts = NULL,
                             verticalLinesCuts = NULL,
                             plotTitle = NULL, plotSubtitle = NULL, 
                             plotCaption = NULL, 
                             plotXLabel = NULL, plotYLabel = NULL,
                             plotTheme = NULL,
                             overrideTableName = NULL,
                             supressWarnings = FALSE,
                             overrideImageWidth  = NULL,
                             overrideImageHeight = NULL){
                    
    # Define plot type
    myPlotType  = "Scatterplot"
    if( !is.null(groupingIndex) ) myPlotType  = "ScatterplotCategorical"
    # Get the table name and override it if needed
    myTableName = deparse(substitute(tableBase))
    if(!is.null(overrideTableName)) myTableName = overrideTableName
                    
    # Get an automatic name if you don't have a proper one
    {
                        
        imageFilePath = ""
                        
        myFileExtension = getFileExtension(plotFilePath)
        
        # If you have a filename keep that one                
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        # If you don't, then generate an automatic one
        else{
            
            if( is.null(groupingIndex) ) imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                               tableName = myTableName, fileType = myPlotType,
                                               variableIndex1 = independentColumnIndex, 
                                               variableIndex2 = dependentColumnIndex)
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                            		tableName = myTableName, fileType = myPlotType,
                                            		 variableIndex1 = independentColumnIndex, 
                                            		 variableIndex2 = dependentColumnIndex,
                                            		 variableIndex3 = groupingIndex)
            
            # Add the hightlighted category to the filepath if any
            if(!is.null(highlightCategory)){
            	
				
				split_string     = substr(imageFilePath, 1, nchar(imageFilePath) - 4)
				last_four_chars  = substr(imageFilePath, nchar(imageFilePath) - 3, nchar(imageFilePath))
				imageFilePath    = paste0(split_string, "_", highlightCategory, last_four_chars)

            }
            
        }
                        
    }
    
                    
    # Get the theme information
    themeData = getThemeParameters(plotTheme)
    
    # Prepare the X and Y axis limits
    if(is.null(xmin)) xmin = min(tableBase[,independentColumnIndex], na.rm= TRUE)
    if(is.null(xmax)) xmax = max(tableBase[,independentColumnIndex], na.rm= TRUE)    
    if(is.null(ymin)) ymin = min(tableBase[,dependentColumnIndex],   na.rm= TRUE)
    if(is.null(ymax)) ymax = max(tableBase[,dependentColumnIndex],   na.rm= TRUE)
                    
    # Get info about the variables
    # ---- Numerical
    numericalNameA = colnames(tableBase)[independentColumnIndex]
    numericalNameB = colnames(tableBase)[dependentColumnIndex]
    imageWidth     = 3 
    # ---- Categories
    myCategoriesA = NA
    nCategoriesA  = 1
    groupingNameA = NA
    if( !is.null(groupingIndex) ){
		 
          myCategoriesA = as.character(levels(tableBase[,groupingIndex]))
          nCategoriesA  = length(myCategoriesA)
          groupingNameA = colnames(tableBase)[groupingIndex]        
          
    }
    
    # ---- If it is NOT grouped by variable
    defaultVector  = NA
    if( is.null(groupingIndex) ){
       
        # Prepare the defaults
        defaultVector = getBiNumericalDefaults(numericalNameA, numericalNameB, 
                                               colorsVector = colorsVector,
                                               plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                               plotCaption = plotCaption, plotXLabel = plotXLabel,
                                               plotYLabel = plotYLabel, fileType = "Scatterplot")
                    
        colorsVector   = defaultVector[[1]]
        plotTitle      = defaultVector[[2]][1]
        plotSubtitle   = defaultVector[[3]][1]
        plotCaption    = defaultVector[[4]][1]
        plotXLabel     = defaultVector[[5]][1]
        plotYLabel     = defaultVector[[6]][1]   
        
    }
    # ---- If it is grouped by variable
    else{
        
        # Prepare the defaults
        defaultVector = getBiNumericalCategoricalDefaults(numericalNameA, numericalNameB, groupingNameA, myCategoriesA,
                                                          colorsVector = colorsVector,
                                                          plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                                          plotCaption = plotCaption, plotXLabel = plotXLabel,
                                                          plotYLabel = plotYLabel, fileType = "ScatterplotCategorical")

        colorsVector   = defaultVector[[1]]
        plotTitle      = defaultVector[[2]][1]
        plotSubtitle   = defaultVector[[3]][1]
        plotCaption    = defaultVector[[4]][1]
        plotXLabel     = defaultVector[[5]][1]
        plotYLabel     = defaultVector[[6]][1]           
        
    }
    
 
    # -------------------------------------------------------------------------
    # DO THE PLOT
    # -------------------------------------------------------------------------
    {
    
        myScatterPlot = NA
        myResults     = NA
        
        # Prepare the Return vector
        myReturn = vector("list", length = 5)
        
        # For the case in which only one variable is analyzed
        if(nCategoriesA == 1){

            # Do the analysis and format the texts properly for the plot
            myResults      = doRegressionAnalysis(tableBase, independentColumnIndex, dependentColumnIndex, supressWarnings = supressWarnings, roundFormulas = myRounding)
            myResultsError = myResults[[1]]
            
            # If the analysis was ok you can do the plot, otherwise you can't
            if(myResultsError == 0 || myResultsError == 4){
            
            	# Get the labels information and format it for the plot
            	myR2Text      = round(myResults[[3]][myResults[[2]],2],2)    
            	myPValueText  = round(myResults[[3]][myResults[[2]],3],4)
            	myFormulaText = myResults[[5]][myResults[[2]]][[1]]
            
            	myR2Text      = paste0("italic(R) ^ 2 == ",myR2Text)
            	if(myPValueText == 0) myPValueText = "italic(pv) < 0.0001"
            	else myPValueText = paste0("italic(pv) == ", myPValueText)	
            	
            	
				#--------------------------------------------------------------------
            	# Do a scatter plot
            	#--------------------------------------------------------------------            	
            	{
            		
					myScatterPlot = ggplot(tableBase, aes(x=tableBase[,independentColumnIndex], y=tableBase[,dependentColumnIndex])) +
				                            
				    					   # With hollow points
				                		   geom_point() +
				             
				                		   # ---- Apply the Y limits
				                		   scale_y_continuous(limits=c(ymin, ymax))
				            
				                			# -- Add the line with confident interval of 95%
				                			if(myResults[[2]] == 1)
				                    			myScatterPlot = myScatterPlot + geom_smooth(method = "lm", formula = y ~ x )
				                			if(myResults[[2]] == 2)
				                    			myScatterPlot = myScatterPlot + geom_smooth(method = "lm", formula = y ~ x + I(x^2))
				                			if(myResults[[2]] == 3)            
				                    			myScatterPlot = myScatterPlot + geom_smooth(method = "lm", formula = y ~ 1/x)
				                			if(myResults[[2]] == 4)                        
				                    			myScatterPlot = myScatterPlot + geom_smooth(method = "glm", formula = y ~ x,
				                                            						        method.args = list(family = gaussian(link = 'log')))
				                			if(myResults[[2]] == 5) 
				                    			myScatterPlot = myScatterPlot + geom_smooth(method = "glm", formula = y ~ x,
				                                						                    method.args = list(family = gaussian(link = 'log')))     
				                			
				                			
				                			
		            # -- Create titles and subtitles
		            myScatterPlot = myScatterPlot +
		            		        labs(title    = plotTitle,
		                    	    subtitle = plotSubtitle,
		                        	caption  = plotCaption,
		                        	x = plotXLabel, y = plotYLabel)    		
		            

            
                	# Apply the theme
                	myScatterPlot = myScatterPlot +
                					theme(panel.background   = themeData[[1]],
                    					  axis.line          = themeData[[2]],
                    					  panel.grid.major.y = themeData[[3]],
                    					  panel.grid.major.x = themeData[[4]],
                    					  legend.position    = themeData[[5]],
                    					  panel.border       = themeData[[9]]
                    					 )
                	
                	# R is shit, x == NULL, or x == NA doesn't work
                	# you need the stupid extra function of is.null is.na                

                	# Set up the lines with the limits, if any
                	if(!is.null(horizontalLinesCuts)){

                    	cutOffData    = data.frame(yintercept = horizontalLinesCuts[[1]], Lines = horizontalLinesCuts[[2]])
                    	myScatterPlot = myScatterPlot + 
                                    
                        	            new_scale_color() +
                            	        
                                	    geom_hline( data = cutOffData, aes(yintercept = yintercept, color = Lines)) +
                                    
                                    	scale_colour_manual(values = horizontalLinesCuts[[3]])
                                
                	}
                        
                	if(!is.null(verticalLinesCuts)){
                            
                    	cutOffData    = data.frame(xintercept = verticalLinesCuts[[1]], Lines = verticalLinesCuts[[2]])
                    	myScatterPlot = myScatterPlot + 
                                
                        		        new_scale_color() +
                                
                                		geom_vline( data = cutOffData, aes(xintercept = xintercept, color = Lines)) +
                                
                                		scale_colour_manual(values = verticalLinesCuts[[3]])
                            
                	}    
                
	                # -- Add the R2 , P-value , and formula text
	                xSquareMin = (xmin + ((xmax - xmin) * 0.01))
	                xSquareMax = (xmin + ((xmax - xmin) * 0.25))
	                ySquareMin = (ymax - ((ymax - ymin) * 0.13))
	                ySquareMax = (ymax - ((ymax - ymin) * 0.01))
	                xDelta     = xSquareMax - xSquareMin
	                yDelta     = ySquareMax - ySquareMin
	                xR2Text    = xSquareMin + xDelta * 0.05
	                yR2Text    = ySquareMax - yDelta * 0.05
	                yPText     = ySquareMax - yDelta * 0.5
	                yFText     = ySquareMax - yDelta * 0.85                	
                	
		            
	                myScatterPlot = myScatterPlot +
	                    
	                    # Background rectangle for easy reading
	                    annotate("rect", xmin  = xSquareMin,
	                                     xmax  = xSquareMax,
	                                     ymin  = ySquareMin,
	                                     ymax  = ySquareMax,
	                                     alpha = .8,
	                                     color = "grey20",
	                                     fill  = "#cbcbcb") +
	                    
	                    annotate("text", x = xR2Text, y = yR2Text, label = myR2Text,      color = "blue", parse = TRUE, hjust = 0, vjust = 1  ) +
	                    annotate("text", x = xR2Text, y = yPText,  label = myPValueText,  color = "blue", parse = TRUE, hjust = 0, vjust = 0.5) + 
	                    annotate("text", x = xR2Text, y = yFText,  label = myFormulaText, color = "blue",               hjust = 0, vjust = 0)	                
	                
	                
		            # Save the plot
		            {
		                  
		                imageHeight = 8
		                imageWidth  = 8
		                        
		                # But override if the user want to
		                if(!is.null(overrideImageWidth)){
		                    imageWidth  = overrideImageWidth
		                }
		                if(!is.null(overrideImageHeight)){
		                    imageHeight = overrideImageHeight
		                }  
		                        
		                # Final save
		                ggsave(imageFilePath, plot = myScatterPlot,
		                       width = imageWidth, height = imageHeight)
		                        
		            }
	                
            		
            	}
            	
            	
                # Fill the return vector, we are done.
	            myReturn[[1]] = myScatterPlot
	            myReturn[[2]] = imageFilePath
	            myReturn[[3]] = myResults[[1]]
	            myReturn[[4]] = myResults[[2]]            
	            myReturn[[5]] = myResults[[3]]
	            myReturn[[6]] = myResults[[4]]
	            myReturn[[7]] = myResults[[5]]
            	
            	
            }
            
            # The analysis told us that you can't do this plot
            else{
            	
                # Fill the return vector, we are done.
	            myReturn[[1]] = myScatterPlot
	            myReturn[[2]] = myResultsError
	            myReturn[[3]] = NA
	            myReturn[[4]] = NA
	            myReturn[[5]] = NA
	            myReturn[[6]] = NA
	            myReturn[[7]] = NA
            	
            }
            
        }
        # For the case in which variables are grouped by something categorical
        else{ 
            
        	# If you want to highlight one, and only one, category
        	if(!is.null(highlightCategory)){
        		
        		subTableBase = tableBase[ tableBase[ , groupingIndex] == highlightCategory ,]
        		
        		
				# Do the analysis and format the texts properly for the plot
            	myResults      = doRegressionAnalysis(subTableBase, independentColumnIndex, dependentColumnIndex, supressWarnings = supressWarnings, roundFormulas = myRounding)
            	myResultsError = myResults[[1]]
            	
            
            	# If the analysis was ok you can do the plot, otherwise you can't
            	if(myResultsError == 0 || myResultsError == 4){
            
            		# Get the labels information and format it for the plot
            		myR2Text      = round(myResults[[3]][myResults[[2]],2],2)    
            		myPValueText  = round(myResults[[3]][myResults[[2]],3],4)
            		myFormulaText = myResults[[5]][myResults[[2]]][[1]]
            		myFormulaText = paste0("  ", myFormulaText)
            		myR2Text      = paste0("italic(R) ^ 2 == ",myR2Text)
            		
            		if(myPValueText == 0) myPValueText = "italic(pv) < 0.0001"
            		else myPValueText = paste0("italic(pv) == ", myPValueText)	
            	
            		# Save the proper index for later
            		originalCategoryIndex = which(levels(tableBase[,groupingIndex]) == highlightCategory)
            		originalColor         = colorsVector[originalCategoryIndex]


            		# STUPID R FUCK YOU!! NEED TO DROP THE FACTORS AND ADD THEM AGAIN
            		tableBase[,groupingIndex] = as.character(tableBase[,groupingIndex])
            		
	            	# Change the category of other non-important values
	            	for(z in 1:nrow(tableBase)){

	            		if(tableBase[z,groupingIndex] != highlightCategory){
	            		
	            			tableBase[z,groupingIndex] <- "ShadowSpecialCategoryLalala"	

	            		}
	            		
	            	}
            		
            		# Set the levels of the column
					tableBase[,groupingIndex] = factor(tableBase[,groupingIndex], levels = c(highlightCategory, "ShadowSpecialCategoryLalala"))	            		
	            		
					
					print(sum(is.na(tableBase[,independentColumnIndex])))
					print(sum(is.na(tableBase[,dependentColumnIndex])))
					print(sum(is.na(tableBase[,groupingIndex])))
					
					aPoints = tableBase[ tableBase[,groupingIndex] == "ShadowSpecialCategoryLalala", ]
            	    bPoints = tableBase[ tableBase[,groupingIndex] == highlightCategory, ]
					
					#--------------------------------------------------------------------
	            	# Do a scatter plot
	            	#--------------------------------------------------------------------            	
	            	{
	            		
						#myScatterPlot = ggplot( tableBase, aes( x=tableBase[,independentColumnIndex], y=tableBase[,dependentColumnIndex], color = tableBase[,groupingIndex] ) ) +
					    myScatterPlot = ggplot( ) +                       
												#geom_point() +
							
							
							
					    					   # With hollow points
											   geom_point(data = aPoints, aes(x = aPoints[, independentColumnIndex], y = aPoints[, dependentColumnIndex]), color = "#CCCCCC" ) +
											   geom_point(data = bPoints, aes(x = bPoints[, independentColumnIndex], y = bPoints[, dependentColumnIndex]), shape = 21, color = "black", fill = originalColor ) +
							
											   # Manually set the colors for each type of point
											   #scale_color_manual(values = c(originalColor,"#CCCCCC")) + 
							
					                		   # ---- Apply the Y limits
					                		   scale_y_continuous(limits=c(ymin, ymax))
					            
					                			# -- Add the line with confident interval of 95%
					                			if(myResults[[2]] == 1)
					                    			myScatterPlot = myScatterPlot + geom_smooth(data = bPoints, aes(x = bPoints[, independentColumnIndex], y = bPoints[, dependentColumnIndex]), color = originalColor, method = "lm", formula = y ~ x )
					                			if(myResults[[2]] == 2)
					                    			myScatterPlot = myScatterPlot + geom_smooth(data = bPoints, aes(x = bPoints[, independentColumnIndex], y = bPoints[, dependentColumnIndex]), color = originalColor, method = "lm", formula = y ~ x + I(x^2))
					                			if(myResults[[2]] == 3)            
					                    			myScatterPlot = myScatterPlot + geom_smooth(data = bPoints, aes(x = bPoints[, independentColumnIndex], y = bPoints[, dependentColumnIndex]), color = originalColor, method = "lm", formula = y ~ 1/x)
					                			if(myResults[[2]] == 4)                        
					                    			myScatterPlot = myScatterPlot + geom_smooth(data = bPoints, aes(x = bPoints[, independentColumnIndex], y = bPoints[, dependentColumnIndex]), color = originalColor, method = "glm", formula = y ~ x,
					                                            						        method.args = list(family = gaussian(link = 'log')))
					                			if(myResults[[2]] == 5) 
					                    			myScatterPlot = myScatterPlot + geom_smooth(data = bPoints, aes(x = bPoints[, independentColumnIndex], y = bPoints[, dependentColumnIndex]), color = originalColor, method = "glm", formula = y ~ x,
					                                						                    method.args = list(family = gaussian(link = 'log')))     
					                			
					                			
					                			
			            # -- Create titles and subtitles
			            myScatterPlot = myScatterPlot +
			            		        labs(title    = plotTitle,
			                    	    subtitle = plotSubtitle,
			                        	caption  = plotCaption,
			                        	x = plotXLabel, y = plotYLabel)    		
			            
	
	            
	                	# Apply the theme
	                	myScatterPlot = myScatterPlot +
	                					theme(panel.background   = themeData[[1]],
	                    					  axis.line          = themeData[[2]],
	                    					  panel.grid.major.y = themeData[[3]],
	                    					  panel.grid.major.x = themeData[[4]],
	                    					  legend.position    = themeData[[5]],
	                    					  panel.border       = themeData[[9]]
	                    					 )
	                	
	                	# R is shit, x == NULL, or x == NA doesn't work
	                	# you need the stupid extra function of is.null is.na                
	
	                	# Set up the lines with the limits, if any
	                	if(!is.null(horizontalLinesCuts)){
	
	                    	cutOffData    = data.frame(yintercept = horizontalLinesCuts[[1]], Lines = horizontalLinesCuts[[2]])
	                    	myScatterPlot = myScatterPlot + 
	                                    
	                        	            new_scale_color() +
	                            	        
	                                	    geom_hline( data = cutOffData, aes(yintercept = yintercept, color = Lines)) +
	                                    
	                                    	scale_colour_manual(values = horizontalLinesCuts[[3]])
	                                
	                	}
	                        
	                	if(!is.null(verticalLinesCuts)){
	                            
	                    	cutOffData    = data.frame(xintercept = verticalLinesCuts[[1]], Lines = verticalLinesCuts[[2]])
	                    	myScatterPlot = myScatterPlot + 
	                                
	                        		        new_scale_color() +
	                                
	                                		geom_vline( data = cutOffData, aes(xintercept = xintercept, color = Lines)) +
	                                
	                                		scale_colour_manual(values = verticalLinesCuts[[3]])
	                            
	                	}    
	                
		                # -- Add the R2 , P-value , and formula text
		                xSquareMin = (xmin + ((xmax - xmin) * 0.01))
		                xSquareMax = (xmin + ((xmax - xmin) * 0.25))
		                ySquareMin = (ymax - ((ymax - ymin) * 0.13))
		                ySquareMax = (ymax - ((ymax - ymin) * 0.01))
		                xDelta     = xSquareMax - xSquareMin
		                yDelta     = ySquareMax - ySquareMin
		                xR2Text    = xSquareMin + xDelta * 0.05
		                yR2Text    = ySquareMax - yDelta * 0.05
		                yPText     = ySquareMax - yDelta * 0.5
		                yFText     = ySquareMax - yDelta * 0.85                	
	                	
		                # Add some space between texts
						imageHeight = 8
			            imageWidth  = 8
			            if(!is.null(overrideImageWidth)){
			               imageWidth  = overrideImageWidth
			            }
			            if(!is.null(overrideImageHeight)){
			                imageHeight = overrideImageHeight
			            }  		                
		                
			            # FUCKING SWITCHS, WHY NOT!!?? 
			            
			            if(imageHeight >= 8 && imageHeight <= 12){
			            	yPText = yPText - 5
			            	yFText = yFText - 10
			            	ySquareMin = ySquareMin - 11
			            	
			            }
			            
			            if(imageWidth >= 8 && imageWidth <= 12){

			            	ratio = (nchar(myFormulaText) / 33)
			            	
			            	xSquareMax = (xmin + ((xmax - xmin) * ratio))

			            }
		                
			            
		                myScatterPlot = myScatterPlot +
		                    
		                    # Background rectangle for easy reading
		                    annotate("rect", xmin  = xSquareMin,
		                                     xmax  = xSquareMax,
		                                     ymin  = ySquareMin,
		                                     ymax  = ySquareMax,
		                                     alpha = .8,
		                                     color = "grey20",
		                                     fill  = "#cbcbcb") +
		                    
		                    annotate("text", x = xR2Text, y = yR2Text, label = myR2Text,      color = "blue", parse = TRUE, hjust = 0, vjust = 1  ) +
		                    annotate("text", x = xR2Text, y = yPText,  label = myPValueText,  color = "blue", parse = TRUE, hjust = 0, vjust = 0.5) + 
		                    annotate("text", x = xR2Text, y = yFText,  label = myFormulaText, color = "blue",               hjust = 0, vjust = 0)	                
		                
		                
			            # Save the plot
			            {
			                  
			                imageHeight = 8
			                imageWidth  = 8
			                        
			                # But override if the user want to
			                if(!is.null(overrideImageWidth)){
			                    imageWidth  = overrideImageWidth
			                }
			                if(!is.null(overrideImageHeight)){
			                    imageHeight = overrideImageHeight
			                }  
			                        
			                # Final save
			                ggsave(imageFilePath, plot = myScatterPlot,
			                       width = imageWidth, height = imageHeight)
			                        
			            }
		                
	            		
	            	}
	            	
	            	
	                # Fill the return vector, we are done.
		            myReturn[[1]] = myScatterPlot
		            myReturn[[2]] = imageFilePath
		            myReturn[[3]] = myResults[[1]]
		            myReturn[[4]] = myResults[[2]]            
		            myReturn[[5]] = myResults[[3]]
		            myReturn[[6]] = myResults[[4]]
		            myReturn[[7]] = myResults[[5]]
            	
            	
            }
            
            	# The analysis told us that you can't do this plot
            	else{
            	
                	# Fill the return vector, we are done.
	            	myReturn[[1]] = myScatterPlot
	            	myReturn[[2]] = myResultsError
	            	myReturn[[3]] = NA
	            	myReturn[[4]] = NA
	            	myReturn[[5]] = NA
	            	myReturn[[6]] = NA
	            	myReturn[[7]] = NA
            	
            	}
        		
	
        		
        		
        	}
        	
        	# If you want all categories
        	else{
        	
        		print("todo")
        			
        	}
        	
        }
            
    }
 
    return (myReturn)
}



# Get one date and one numerical column, and make a pseudo-regression between them
# with the thrending of data.
doTimeSeriesPlot <- function(tableBase, dateColumnIndex, dependentColumnIndex, plotFilePath,
                             groupingIndex = NULL,
                             colorsVector = NULL, 
                             ymin = NULL, ymax = NULL,
                             xmin = NULL, xmax = NULL,
                             horizontalLinesCuts = NULL,
                             verticalLinesCuts = NULL,
                             plotTitle = NULL, plotSubtitle = NULL, 
                             plotCaption = NULL, 
                             plotXLabel = NULL, plotYLabel = NULL,
                             plotTheme = NULL,
                             overrideTableName = NULL,
                             supressWarnings = FALSE,
                             overrideImageWidth  = NULL,
                             overrideImageHeight = NULL){
                    
    # Define plot type
    myPlotType  = "DateScatterplot"
    if( !is.null(groupingIndex) ) myPlotType  = "DateScatterplotCategorical"
    # Get the table name and override it if needed
    myTableName = deparse(substitute(tableBase))
    if(!is.null(overrideTableName)) myTableName = overrideTableName
                    
    # Get an automatic name if you don't have a proper one
    {
                        
        imageFilePath = ""
                        
        myFileExtension = getFileExtension(plotFilePath)
        
        # If you have a filename keep that one                
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        # If you don't, then generate an automatic one
        else{
            
            if( is.null(groupingIndex) ) imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                                           tableName = myTableName, fileType = myPlotType,
                                                                           variableIndex1 = dateColumnIndex, 
                                                                           variableIndex2 = dependentColumnIndex)
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = dateColumnIndex, 
                                                   variableIndex2 = dependentColumnIndex,
                                                   variableIndex3 = groupingIndex)
            
            
        }
                        
    }
    
    # Get the theme information
    themeData = getThemeParameters(plotTheme)
    
    # Prepare the X and Y axis limits
    if(is.null(xmin)) xmin = min(tableBase[,dateColumnIndex], na.rm= TRUE)
    if(is.null(xmax)) xmax = max(tableBase[,dateColumnIndex], na.rm= TRUE)    
    if(is.null(ymin)) ymin = min(tableBase[,dependentColumnIndex],   na.rm= TRUE)
    if(is.null(ymax)) ymax = max(tableBase[,dependentColumnIndex],   na.rm= TRUE)
                    
    # Get info about the variables
    # ---- Numerical
    numericalNameA = colnames(tableBase)[dateColumnIndex]
    numericalNameB = colnames(tableBase)[dependentColumnIndex]
    imageWidth     = 3 
    # ---- Categories
    myCategoriesA = NA
    nCategoriesA  = 1
    groupingNameA = NA
    if( !is.null(groupingIndex) ){

          myCategoriesA = as.character(levels(tableBase[,groupingIndex]))
          nCategoriesA  = length(myCategoriesA)
          groupingNameA = colnames(tableBase)[groupingIndex]        
                
    }
    
    # ---- If it is NOT grouped by variable
    defaultVector  = NA
    if( is.null(groupingIndex) ){
       
        # Prepare the defaults
        defaultVector = getBiNumericalDefaults(numericalNameA, numericalNameB, 
                                               colorsVector = colorsVector,
                                               plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                               plotCaption = plotCaption, plotXLabel = plotXLabel,
                                               plotYLabel = plotYLabel, fileType = "DateScatterplot")
                    
        colorsVector   = defaultVector[[1]]
        plotTitle      = defaultVector[[2]][1]
        plotSubtitle   = defaultVector[[3]][1]
        plotCaption    = defaultVector[[4]][1]
        plotXLabel     = defaultVector[[5]][1]
        plotYLabel     = defaultVector[[6]][1]   
        
    }
    # ---- If it is grouped by variable
    else{
        
        # Prepare the defaults
        defaultVector = getBiNumericalCategoricalDefaults(numericalNameA, numericalNameB, groupingNameA, myCategoriesA,
                                                          colorsVector = colorsVector,
                                                          plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                                          plotCaption = plotCaption, plotXLabel = plotXLabel,
                                                          plotYLabel = plotYLabel, fileType = "DateScatterplotCategorical")

        colorsVector   = defaultVector[[1]]
        plotTitle      = defaultVector[[2]][1]
        plotSubtitle   = defaultVector[[3]][1]
        plotCaption    = defaultVector[[4]][1]
        plotXLabel     = defaultVector[[5]][1]
        plotYLabel     = defaultVector[[6]][1]           
        
    }
    
 
    # -------------------------------------------------------------------------
    # DO THE PLOT
    # -------------------------------------------------------------------------
    {
    
        myTimeSeries = NA
        myResults    = NA
        
        # For the case in which only one variable is analyzed
        if(nCategoriesA == 1){

            #--------------------------------------------------------------------
            # Do a scatter plot
            #--------------------------------------------------------------------
            myTimeSeries = ggplot(tableBase, aes(x=tableBase[,dateColumnIndex], y=tableBase[,dependentColumnIndex])) +
                            
                # With hollow points
                geom_point() +
            
                # ---- Apply the Y limits
                scale_y_continuous(limits=c(ymin, ymax)) +
            
                # ---- Apply the X labels properly
                scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
                
                # ---- Make the smooth line with the changes over time
                stat_smooth(colour="blue")
            
                # Create titles and subtitles
                myTimeSeries = myTimeSeries +
                    labs(title    = plotTitle,
                         subtitle = plotSubtitle,
                         caption  = plotCaption,
                         x = plotXLabel, y = plotYLabel)                    


        }
        
        # For the case in which variables are grouped by something categorical
        else{

            #--------------------------------------------------------------------
            # Do a scatter plot
            #--------------------------------------------------------------------
            myTimeSeries = ggplot(tableBase, aes(x=tableBase[,dateColumnIndex], y=tableBase[,dependentColumnIndex], color = tableBase[,groupingIndex])) +
                            
                # With hollow points
                geom_point() +
            
                scale_color_manual(values = colorsVector, na.value = COLOR_NA) +
                
                # ---- Apply the Y limits
                scale_y_continuous(limits=c(ymin, ymax)) +
            
                # ---- Apply the X labels properly
                scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
                
                # ---- Make the smooth line with the changes over time
                stat_smooth(size=1.5, method = "loess", level = 0.95, 
                            fullrange = TRUE, se = FALSE) 

                # Create titles and subtitles
                myTimeSeries = myTimeSeries +
                              labs(title    = plotTitle,
                                   subtitle = plotSubtitle,
                                   caption  = plotCaption,
                                   x = plotXLabel, y = plotYLabel,
                                   color = groupingNameA)              
                
        }
     
    
                  
            
        # Apply the theme
        myTimeSeries = myTimeSeries +
        theme(panel.background   = themeData[[1]],
              axis.line          = themeData[[2]],
              panel.grid.major.y = themeData[[3]],
              panel.grid.major.x = themeData[[4]],
              legend.position    = themeData[[5]],
              panel.border       = themeData[[9]],
                    
              )
                
        # R is shit, x == NULL, or x == NA doesn't work
        # you need the stupid extra function of is.null is.na                

        # Set up the lines with the limits, if any
        if(!is.null(horizontalLinesCuts)){

            cutOffData    = data.frame(yintercept = horizontalLinesCuts[[1]], Lines = horizontalLinesCuts[[2]])
            myTimeSeries  = myTimeSeries + 
                                    
                    new_scale_color() +
                    geom_hline( data = cutOffData, aes(yintercept = yintercept, color = Lines)) +
                    scale_colour_manual(values = horizontalLinesCuts[[3]])
                                
        }
                        
        if(!is.null(verticalLinesCuts)){
                            
            cutOffData    = data.frame(xintercept = verticalLinesCuts[[1]], Lines = verticalLinesCuts[[2]])
            myTimeSeries  = myTimeSeries + 
                                
                    new_scale_color() +
                    geom_vline( data = cutOffData, aes(xintercept = xintercept, color = Lines)) +
                    scale_colour_manual(values = verticalLinesCuts[[3]])
                            
        }    

                    
    }
    
                
    # Save the plot
    {
                  
        imageHeight = 8
        imageWidth  = 8
                        
        # But override if the user want to
        if(!is.null(overrideImageWidth)){
            imageWidth  = overrideImageWidth
        }
        if(!is.null(overrideImageHeight)){
            imageHeight = overrideImageHeight
        }  
                        
        # Final save
            ggsave(imageFilePath, plot = myTimeSeries,
                   width = imageWidth, height = imageHeight)
                        
    }    

    # Return
    myReturn = vector("list", length = 2)
    myReturn[[1]] = myTimeSeries
    myReturn[[2]] = imageFilePath

    return (myReturn)    
 
}


# Get two numbers and do a LOESS regression, same as regression, but no model
# is given back.
doLOESSRegressionPlot <- function(tableBase, independentColumnIndex, dependentColumnIndex, plotFilePath,
                             groupingIndex = NULL,
                             colorsVector = NULL, 
                             ymin = NULL, ymax = NULL,
                             xmin = NULL, xmax = NULL,
                             horizontalLinesCuts = NULL,
                             verticalLinesCuts = NULL,
                             plotTitle = NULL, plotSubtitle = NULL, 
                             plotCaption = NULL, 
                             plotXLabel = NULL, plotYLabel = NULL,
                             plotTheme = NULL,
                             overrideTableName = NULL,
                             supressWarnings = FALSE,
                             overrideImageWidth  = NULL,
                             overrideImageHeight = NULL){
                    
    # Define plot type
    myPlotType  = "Scatterplot"
    if( !is.null(groupingIndex) ) myPlotType  = "ScatterplotCategorical"
    # Get the table name and override it if needed
    myTableName = deparse(substitute(tableBase))
    if(!is.null(overrideTableName)) myTableName = overrideTableName
                    
    # Get an automatic name if you don't have a proper one
    {
                        
        imageFilePath = ""
                        
        myFileExtension = getFileExtension(plotFilePath)
        
        # If you have a filename keep that one                
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        # If you don't, then generate an automatic one
        else{
            
            if( is.null(groupingIndex) ) imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                                           tableName = myTableName, fileType = myPlotType,
                                                                           variableIndex1 = independentColumnIndex, 
                                                                           variableIndex2 = dependentColumnIndex)
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = independentColumnIndex, 
                                                   variableIndex2 = dependentColumnIndex,
                                                   variableIndex3 = groupingIndex)
            
            
        }
                        
    }
    
    # Get the theme information
    themeData = getThemeParameters(plotTheme)
    
    # Prepare the X and Y axis limits
    if(is.null(xmin)) xmin = min(tableBase[,independentColumnIndex], na.rm= TRUE)
    if(is.null(xmax)) xmax = max(tableBase[,independentColumnIndex], na.rm= TRUE)    
    if(is.null(ymin)) ymin = min(tableBase[,dependentColumnIndex],   na.rm= TRUE)
    if(is.null(ymax)) ymax = max(tableBase[,dependentColumnIndex],   na.rm= TRUE)
                    
    # Get info about the variables
    # ---- Numerical
    numericalNameA = colnames(tableBase)[independentColumnIndex]
    numericalNameB = colnames(tableBase)[dependentColumnIndex]
    imageWidth     = 3 
    # ---- Categories
    myCategoriesA = NA
    nCategoriesA  = 1
    groupingNameA = NA
    if( !is.null(groupingIndex) ){

          myCategoriesA = as.character(levels(tableBase[,groupingIndex]))
          nCategoriesA  = length(myCategoriesA)
          groupingNameA = colnames(tableBase)[groupingIndex]        
                
    }
    
    # ---- If it is NOT grouped by variable
    defaultVector  = NA
    if( is.null(groupingIndex) ){
       
        # Prepare the defaults
        defaultVector = getBiNumericalDefaults(numericalNameA, numericalNameB, 
                                               colorsVector = colorsVector,
                                               plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                               plotCaption = plotCaption, plotXLabel = plotXLabel,
                                               plotYLabel = plotYLabel, fileType = "DateScatterplot")
                    
        colorsVector   = defaultVector[[1]]
        plotTitle      = defaultVector[[2]][1]
        plotSubtitle   = defaultVector[[3]][1]
        plotCaption    = defaultVector[[4]][1]
        plotXLabel     = defaultVector[[5]][1]
        plotYLabel     = defaultVector[[6]][1]   
        
    }
    # ---- If it is grouped by variable
    else{
        
        # Prepare the defaults
        defaultVector = getBiNumericalCategoricalDefaults(numericalNameA, numericalNameB, groupingNameA, myCategoriesA,
                                                          colorsVector = colorsVector,
                                                          plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                                          plotCaption = plotCaption, plotXLabel = plotXLabel,
                                                          plotYLabel = plotYLabel, fileType = "DateScatterplotCategorical")

        colorsVector   = defaultVector[[1]]
        plotTitle      = defaultVector[[2]][1]
        plotSubtitle   = defaultVector[[3]][1]
        plotCaption    = defaultVector[[4]][1]
        plotXLabel     = defaultVector[[5]][1]
        plotYLabel     = defaultVector[[6]][1]           
        
    }
    
 
    # -------------------------------------------------------------------------
    # DO THE PLOT
    # -------------------------------------------------------------------------
    {
    
        myTimeSeries = NA
        myResults    = NA
        
        # For the case in which only one variable is analyzed
        if(nCategoriesA == 1){

            #--------------------------------------------------------------------
            # Do a scatter plot
            #--------------------------------------------------------------------
            myTimeSeries = ggplot(tableBase, aes(x=tableBase[,independentColumnIndex], y=tableBase[,dependentColumnIndex])) +
                            
                # With hollow points
                geom_point() +
            
                # ---- Apply the Y limits
                scale_y_continuous(limits=c(ymin, ymax)) +

                # ---- Make the smooth line with the changes over time
                stat_smooth(colour="blue")
            
                # Create titles and subtitles
                myTimeSeries = myTimeSeries +
                    labs(title    = plotTitle,
                         subtitle = plotSubtitle,
                         caption  = plotCaption,
                         x = plotXLabel, y = plotYLabel)                    


        }
        
        # For the case in which variables are grouped by something categorical
        else{

            #--------------------------------------------------------------------
            # Do a scatter plot
            #--------------------------------------------------------------------
            myTimeSeries = ggplot(tableBase, aes(x=tableBase[,independentColumnIndex], y=tableBase[,dependentColumnIndex], color = tableBase[,groupingIndex])) +
                            
                # With hollow points
                geom_point() +
            
                scale_color_manual(values = colorsVector, na.value = COLOR_NA) +
                
                # ---- Apply the Y limits
                scale_y_continuous(limits=c(ymin, ymax)) +
            
                # ---- Make the smooth line with the changes over time
                stat_smooth(size=1.5, method = "loess", level = 0.95, 
                            fullrange = TRUE, se = FALSE) 

                # Create titles and subtitles
                myTimeSeries = myTimeSeries +
                              labs(title    = plotTitle,
                                   subtitle = plotSubtitle,
                                   caption  = plotCaption,
                                   x = plotXLabel, y = plotYLabel,
                                   color = groupingNameA)              
                
        }
     
    
                  
            
        # Apply the theme
        myTimeSeries = myTimeSeries +
        theme(panel.background   = themeData[[1]],
              axis.line          = themeData[[2]],
              panel.grid.major.y = themeData[[3]],
              panel.grid.major.x = themeData[[4]],
              legend.position    = themeData[[5]],
              panel.border       = themeData[[9]],
                    
              )
                
        # R is shit, x == NULL, or x == NA doesn't work
        # you need the stupid extra function of is.null is.na                

        # Set up the lines with the limits, if any
        if(!is.null(horizontalLinesCuts)){

            cutOffData    = data.frame(yintercept = horizontalLinesCuts[[1]], Lines = horizontalLinesCuts[[2]])
            myTimeSeries  = myTimeSeries + 
                                    
                    new_scale_color() +
                    geom_hline( data = cutOffData, aes(yintercept = yintercept, color = Lines)) +
                    scale_colour_manual(values = horizontalLinesCuts[[3]])
                                
        }
                        
        if(!is.null(verticalLinesCuts)){
                            
            cutOffData    = data.frame(xintercept = verticalLinesCuts[[1]], Lines = verticalLinesCuts[[2]])
            myTimeSeries  = myTimeSeries + 
                                
                    new_scale_color() +
                    geom_vline( data = cutOffData, aes(xintercept = xintercept, color = Lines)) +
                    scale_colour_manual(values = verticalLinesCuts[[3]])
                            
        }    

                    
    }
    
                
    # Save the plot
    {
                  
        imageHeight = 8
        imageWidth  = 8
                        
        # But override if the user want to
        if(!is.null(overrideImageWidth)){
            imageWidth  = overrideImageWidth
        }
        if(!is.null(overrideImageHeight)){
            imageHeight = overrideImageHeight
        }  
                        
        # Final save
            ggsave(imageFilePath, plot = myTimeSeries,
                   width = imageWidth, height = imageHeight)
                        
    }    

    # Return
    myReturn = vector("list", length = 2)
    myReturn[[1]] = myTimeSeries
    myReturn[[2]] = imageFilePath

    return (myReturn)    
 
}


# Logit regression for a categorical variable with respect a numerical variable
# This is use to measure number of friends with respect SA or vitamin D
#
# The function needs
#
#     tableBase        (Dataframe)  : dataframe with the data
#
#     categoricalIndex (int)        : index with the categorical data that you want to analyze
#
#     modalityOne      (string)     : Which modality should be consider 1. Your data should only
#                                     have two modalities possible, otherwise doing this is wrong.
#                                     The function substitute this modality to 1 and everything
#                                     else to 0.
#
#     friendshipMatrix (int Matrix) : matrix with the frienship information
#                                     (see getFriendshipMatrix(edgesDF, totalPeople) to get one easily)
#
#
# And return the plot and analysis for the logit function
doBinaryLogitPlot <- function(tableBase, categoricalIndex, modalityOne, frienshipMatrix, plotFilePath,
                              myColor = NULL, 
                              ymin = NULL, ymax = NULL,
                              xmin = NULL, xmax = NULL,
                              horizontalLinesCuts = NULL,
                              verticalLinesCuts = NULL,
                              plotTitle = NULL, plotSubtitle = NULL, 
                              plotCaption = NULL, 
                              plotXLabel = NULL, plotYLabel = NULL,
                              plotTheme = NULL,
                              overrideTableName = NULL,
                              supressWarnings = FALSE,
                              overrideImageWidth  = NULL,
                              overrideImageHeight = NULL){
    
    # Define plot type
    myPlotType  = "BinaryLogitPlot"
    
    # Get the table name and override it if needed
    myTableName = deparse(substitute(tableBase))
    if(!is.null(overrideTableName)) myTableName = overrideTableName
    
    # Get an automatic name if you don't have a proper one
    {
                        
        imageFilePath = ""
                        
        myFileExtension = getFileExtension(plotFilePath)
        
        # If you have a filename keep that one                
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        # If you don't, then generate an automatic one
        else{
            
            imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                              tableName = myTableName, fileType = myPlotType,
                                              variableIndex1 = categoricalIndex)        
            
        }
                        
    }
    
    # Get the theme information (TODO)
    themeData = getThemeParameters(plotTheme)
    
    # The color here is constant (yet)
    if(is.null(myColor) == TRUE) myColor = "#e88e2e"
    
    # Prepare the X and Y axis limits
    # The limits are hardcoded and doesn't make sense to change them
    xBreaks = max(myLogitData[[1]])
    xmax    = xBreaks + 0.5
    
    
    
    
    
    if(FALSE){
        if(is.null(xmin)) xmin = min(tableBase[,independentColumnIndex], na.rm= TRUE)
        if(is.null(xmax)) xmax = max(tableBase[,independentColumnIndex], na.rm= TRUE)    
        if(is.null(ymin)) ymin = min(tableBase[,dependentColumnIndex],   na.rm= TRUE)
        if(is.null(ymax)) ymax = max(tableBase[,dependentColumnIndex],   na.rm= TRUE)    
    }
    
    # Do the logit model and get the coefficients
    myLogitData  = prepareLogitData(tableBase, categoricalIndex, modalityOne, frienshipMatrix)
    myLogitModel = doLogitAnalysis(myLogitData, 1, 2)
    myE          = myLogitModel[[2]]
    myB          = myLogitModel[[3]]

    # Prepare the exponential function values
    myCurrentLogitFunction = function(x){
            return(1/(1 + exp(-(myB*x+myE) ) ) )
    }
    
    
    
    # Prepare the dataframes for the plots
    logisticsPlotsDF           = data.frame(matrix(NA, nrow = length(myLogitData[[1]]), ncol = 3))
    colnames(logisticsPlotsDF) = c("H_Status", "TotalFriendsH", "H_Numerical")
    logisticsPlotsDF[,1]       = myLogitData[[3]]
    logisticsPlotsDF[,2]       = myLogitData[[1]]
    logisticsPlotsDF[,3]       = myLogitData[[2]]
    

  
    # This plot is ugly and useless, but the analysis tells us that there is a difference
    myPlot = doBoxPlotV2 (logisticsPlotsDF, 2, VITAMIND_FOLDER, groupIndex = 1) 
    myPvalue = myPlot[[5]][1,2]
    
    # Find the values for the manual boxplots
    {
            
        HNegatives = summary(logisticsPlotsDF[logisticsPlotsDF$H_Status != modalityOne,]$TotalFriendsH)
        HPositives = summary(logisticsPlotsDF[logisticsPlotsDF$H_Status == modalityOne,]$TotalFriendsH)

        # Boxplot bottom, the 0
        HNegativeMin = as.numeric(HNegatives[1])
        HNegativeMax = as.numeric(HNegatives[6])
        HNegative1   = as.numeric(HNegatives[2])
        HNegative2   = as.numeric(HNegatives[3])
        HNegative3   = as.numeric(HNegatives[5])
        HNegativeA   = as.numeric(HNegatives[4])

        # Boxplot on top, the 1
        HPositiveMin = as.numeric(HPositives[1])
        HPositiveMax = as.numeric(HPositives[6])
        HPositive1   = as.numeric(HPositives[2])
        HPositive2   = as.numeric(HPositives[3])
        HPositive3   = as.numeric(HPositives[5])
        HPositiveA   = as.numeric(HPositives[4])            
            
    }
        
    myPlot = ggplot() +
        # Jitter points
        geom_jitter(data = logisticsPlotsDF, aes(x=TotalFriendsH,y=H_Numerical),  width = 0.25, height = 0.025, color = "#e88e2e", alpha = 0.2) +
        # Horizontal lines
        geom_hline(yintercept=0) +
        geom_hline(yintercept=1) +
            
        # Formulas
        geom_function(fun = myCurrentLogitFunction, colour = myColor, lwd = 1, linetype = 1) +

        # -- E Negatives
        # ---- MIN to Q1
        geom_segment(aes(x = HNegativeMin, y = -0.09, xend = HNegative1,   yend = -0.09), colour = "black") + 
        # ---- Q3 to MAX
        geom_segment(aes(x = HNegative3,   y = -0.09, xend = HNegativeMax, yend = -0.09), colour = "black") + 
        # ---- Rectangle in the middle
        geom_rect(aes(xmin=HNegative1, xmax=HNegative3, ymin=-0.07, ymax=-0.11), fill=myColor, color="black", alpha=0.5) +
        # ---- Average tick in the middle-ish of the rectangle
        geom_segment(aes(x = HNegativeA,   y = -0.07, xend = HNegativeA,   yend = -0.11), colour = "black") +

        # -- E Positives
        geom_segment(aes(x = HPositiveMin, y = +1.15, xend = HPositive1,   yend = +1.15), colour = "black") + 
        geom_segment(aes(x = HPositive3,   y = +1.15, xend = HPositiveMax, yend = +1.15), colour = "black") + 
        geom_rect(aes(xmin=HPositive1, xmax=HPositive3, ymin=+1.13, ymax=+1.17), fill=myColor, color="black", alpha=0.5) +
        geom_segment(aes(x = HPositiveA,   y = +1.13, xend = HPositiveA,   yend = +1.17), colour = "black") +
        
        # Break in integers numbers only
        scale_x_continuous( breaks = seq( 0 , xBreaks , 1),   limits=c( -0.5 , xmax  ))    +
        scale_y_continuous( breaks = seq(0,1,0.2), limits=c(-0.17, 1.17))  +
        
        # Labels
        labs(x        = plotXLabel,
             y        = plotYLabel,
             title    = plotTitle,
             subtitle = plotSubtitle ) + 
        
        # Theme with line draws
        theme_linedraw()
        
    # Save the plot
    {
                  
        imageHeight = 8
        imageWidth  = 8
                        
        # But override if the user want to
        if(!is.null(overrideImageWidth)){
            imageWidth  = overrideImageWidth
        }
        if(!is.null(overrideImageHeight)){
            imageHeight = overrideImageHeight
        }  
                        
        # Final save
        ggsave(imageFilePath, plot = myPlot,
               width = imageWidth, height = imageHeight)

    }    

    # Return
    myReturn = vector("list", length = 4)
    myReturn[[1]] = myPlot
    myReturn[[2]] = myLogitModel
    myReturn[[3]] = imageFilePath
    myReturn[[4]] = myPvalue
    
    

    return (myReturn)      
    
}