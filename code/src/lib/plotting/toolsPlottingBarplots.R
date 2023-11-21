# Load external libraries
library(ggplot2)       # Basic ggplot2 library
library(ggnewscale)    # Allows for multiple color scales in one plot
library(RColorBrewer)  # Color settins and palettes
library(shadowtext)    # Drop shadows in text for better viewing
library(forcats)       # Reverse order of factors (fct_rev)

# load our proper libraries
source(paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingCommon.R"), encoding="utf-8") # Automatic pathing and theme

# Do a bar plot, this could have the following combination:
#
# GROUPED:
#
# -- Non grouped = simple bar plot, ie: How many men or women
#
# -- Grouped     = group by a given variable: How many men with respect BMI
#                  and how many women with respect BMI. This is the same as
#                  doing a Xi² test.
#
# COUNTING:
#
# -- absolute    = Count how many of each are, ie: 120 Men and 80 Women (default)
#
# -- relative    = Count how many of each are relative to the total (%)
#                  ie: 60% Men and 40% Women
#
# ROTATION:
#
# -- FALSE       = Bins are display in the x axys, total in the y axis (default)
#
# -- TRUE        = Bins are display in the y axys, total in the x axis
#                  This is useful when you have a lot of bins and use
#                  a portrait orientation page for the plot.
#
# SORTING:
#
# -- none         = Bins are display in level order, if no level is present
#                   is sorted by uniques order.
#
# -- alphabetical = Bins are display in alphabetical order
#
# -- descending   = Display the highgest bars first (default)
#
# -- ascending    = Display the shortest bars first
#
# CROPPING:
#
# -- minValue    = A bin needs to have at least this many to appear
#
# -- maxValue    = A bin needs to have less than this many to appear
#
# -- cropNumber  = If modality is too small, it get annoying to have (NOT IMPLEMENTED)
#                  the text on top of it. You can choose to crop the
#                  labels if it falls bellow a certain threshold.
#                  The default is 0 which means don't crop anything.
#
# (string) labelsAligment = How does the labels in the bar should be shown
#                           -- under           just under the bar
#                           -- above (default) just above the bar
#                           -- none            don't show labels
#
# (int)      barsFontSize = How big should the number displayed on the bar
#                           should be (default = 2)
#
# (bool)    colorCounting; FALSE Make a simple black and white bars and ignore
#                                the given color vector. (Default)
#
#                          TRUE  Fill the counting variable with the given
#                                color vector or the default generated one.
#                                This is not recommended for long plots!.
#
#                          
#
doBarPlotV2 <- function(tableBase, countingIndex, plotFilePath,
                        groupIndex   = NULL,
                        colorsVector = NULL,
    
                        countingType = "absolute",   # absolute, relative
                        rotation     = FALSE,        # FALSE, TRUE
                        sort         = "none",       # none, alphabetical, ascending, descending
                        minValue     = 0,            # Minimum value that bars should have
                        maxValue     = Inf,          # Maximum value that bars should have
	                    top          = 0,            # Show only the top X bars, if 0, show all
    
                        # cropNumbers   = 0, NOT IMPLEMENTED
                        labelsAlignment = "above",   # above, under, none
                        barsFontSize    = 2,
                        colorCounting   = FALSE,     # FALSE, TRUE
                                                         
						givenAngle      = 0,
						
						legendPosition  = "same",    # none = no lengend, bottom = bottom of the plot
	
                        plotTitle = NULL, plotSubtitle = NULL,
                        plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                        plotTheme = NULL,
                        overrideTableName   = NULL,
                        overrideCaption     = NULL,
                        supressWarnings     = FALSE,
                        overrideImageWidth  = 0,
                        overrideImageHeight = 0){

    
    # Let figure it out what type of plot you want to make first
    plotType = "Unknown"
    # ---- Simple or grouped?
    if( is.null(groupIndex) ){
        # ---- Rotated or not?
        if(rotation == TRUE){
    
            # ---- What type of counting?
            if(countingType == "absolute") plotType = "LongAbsBarPlot"
            else                           plotType = "LongRelBarPlot"
            
        }
        else{
                   
            # ---- What type of counting?
            if(countingType == "absolute") plotType = "AbsBarPlot"
            else                           plotType = "RelBarPlot"
            
        }
        
    }
    else{
        
        # ---- Rotated or not?
        if(rotation == TRUE){
            
            # ---- What type of counting?
            if(countingType == "absolute") plotType = "CombinedLongAbsBarPlot"
            else                           plotType = "CombinedLongRelBarPlot"         
            
        }
        else{
            
            # ---- What type of counting?
            if(countingType == "absolute") plotType = "CombinedAbsBarPlot"
            else                           plotType = "CombinedRelBarPlot"         
            
        }        
        
    }
    
    # Check the name of the table and override if needed
    myTableName = deparse(substitute(tableBase))
    if(!is.null(overrideTableName)) myTableName = overrideTableName

    # Get an automatic imagePath name if you don't have a proper one
    {
            
        imageFilePath = ""
        
        # If it is not null
        if(!is.null(plotFilePath)){
              
            # Check whether you have a file or a folder
            myFileExtension = getFileExtension(plotFilePath)
            
            # If you have a file extension, don't do anything and use that filepath as the final path to save the file  
            if(myFileExtension == ".png") imageFilePath = plotFilePath
              
            # If you don't, get an automatic one
            # If you have something else, probably a folder, get an automatic filepath
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = plotType,
                                                   variableIndex1 = countingIndex,
                                                   variableIndex2 = groupIndex)              
        }        
        # If it is null
        else{
            
            # It shouldn't be null. This might be use later for plot composites though
            if(supressWarnings == FALSE){
            
                print("WARNING!!")    
                print("In function doBarPlotV2()")
                print("The input plotFilePath is NULL, it should never be NULL")
                print("Give me a filepath ending in .png or a folder")
                
            }
            
        }

    }    

    
    # Delete the data that we are not going to use, if any
    {
    
    	# Take the top bars only
        if(top > 0){
            
        	mySummary = summarizeCategorical(tableBase, countingIndex, sorted = "top", crop = top)
            filterTheseCatOnly = mySummary[,1]
            tableBase = tableBase[(tableBase[,countingIndex] %in% filterTheseCatOnly),]
            
        }
    	
    	# Take away the minimum values
    	if(minValue !=0){
          
            summaryResult      = summarizeCategorical(tableBase,countingIndex)
            modalitiesToDelete = summaryResult[summaryResult$Count<minValue,1]
			print(modalitiesToDelete)
            
            totalToDelete      = length(modalitiesToDelete)
            
            
            for (i in 1:totalToDelete) {
              
            	# I FUCKING HATE THE FUCKING LEVELS.
            	# LISTEN HERE YOU STUPID SHIT, A STRING IS NOT A LEVEL
            	# FUCK YOU1!!
            	currentModality = as.character(modalitiesToDelete[i])
            	print(currentModality)
            	
                tableBase = deleteCategory(tableBase, countingIndex, currentModality) # Stupid R doesn't even tell you that a variable is missing from the function
                      
            }            
        
        }  
    	
    	# Take away the maximum values
    	{
    		
    	}
    	
       	
    	
    }
          
    # Get info about different categories
    myCountingCategories = getCategories(tableBase, countingIndex)
    nCountingCategories  = length(myCountingCategories)
    countingName         = colnames(tableBase)[countingIndex]

    myGroupingCategories = NA
    nGroupingCategories  = 0
    groupingName         = ""
    if(!is.null(groupIndex)){    
    
        myGroupingCategories = getCategories(tableBase, groupIndex)
        nGroupingCategories  = length(myGroupingCategories)
        groupingName         = colnames(tableBase)[groupIndex]
        
    }
    
    # Get info about the final image dimensions
    imageWidth   = max(5, nCountingCategories * 2)  # Minimum 5 , so we can at least see the legend if there are too little categories
                                                    # It sound counter intuitive that the minimum is found by the max(), but trust me.
    imageHeight  = 8                                # Default image height, usually 8 is perfect in all cases.
          
         
    # If you try to do a barplot with way too many categories, make a warning about it
    if(nCountingCategories > 16){
            
        if(rotation == FALSE){
            
            if(supressWarnings == FALSE){
              
              warningString = ""
              warningString = paste0(warningString , " ---------------"  , "\n")
              warningString = paste0(warningString , " -- WARNING!! --"  , "\n")
              warningString = paste0(warningString , " ---------------"  , "\n")
              warningString = paste0(warningString , ""                  , "\n")
              warningString = paste0(warningString , " You are trying to do a bar plot with more than 16 categories. Are you sure? " , "\n")
              warningString = paste0(warningString , " Notice that you can set rotation == TRUE which will be a better option."      , "\n")
              warningString = paste0(warningString , ""                  , "\n")
              
              print(warningString)
              
            }            
            
        }
        
    }
          
    # Prepare the defaults
    {
        print(plotType)
        # In any case, the theme is the same for any type of plot
        themeData = getThemeParameters(plotTheme)
        
        defaultVector = NULL
        # -- If you are not grouping a doing a simple barplot
        if(   is.null(groupIndex) ) defaultVector = getCategoricalDefaults(   countingName, myCountingCategories,
                                                                              colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, plotType = plotType)
        
        else                        defaultVector = getBiCategoricalDefaults( countingName, myCountingCategories,
                                                                              groupingName, myGroupingCategories,  
                                                                              colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, plotType = plotType)
            
        colorsVector  = defaultVector[[1]]
        plotTitle     = defaultVector[[2]][1]
        plotSubtitle  = defaultVector[[3]][1]
        plotCaption   = defaultVector[[4]][1]
        plotXLabel    = defaultVector[[5]][1]
        plotYLabel    = defaultVector[[6]][1]            

    }
    
    # Everything is ready, prepare the graphic object      
    myBarPlot = NA    
    
    
    # Are you doing a grouping plot?
    # .. NO
    if(is.null(groupIndex)){
        
        # Are you doing a relative plot?
        if(countingType == "absolute"){
        
            # Init the plot 
            if(colorCounting == TRUE) myBarPlot = ggplot(tableBase, aes(tableBase[,countingIndex], fill = tableBase[,countingIndex]))
            else                      myBarPlot = ggplot(tableBase, aes(tableBase[,countingIndex]))
            
            
            # OK, so this is really annoying, because ggplot2 has a bug somewhere
            # and doesn't allow to add a ggplot object to an ascending order
            # geom_bar. So we need to define all possible combinations instead
            # of adding one by one. R is the worse, please die as soon as
            # possible.

            if(colorCounting == TRUE){
            	#TODO
				myBarPlot = ggplot(tableBase, aes(tableBase[,countingIndex], fill = tableBase[,countingIndex]))
				myBarPlot = myBarPlot + scale_fill_manual(values = colorsVector)
				myBarPlot = myBarPlot + geom_bar(stat = "count", position=position_dodge(), colour="black")
            }
            else{
            	
				if(sort == "ascending"){
					
					myBarPlot = ggplot(tableBase, aes(x = fct_rev(fct_infreq(tableBase[,countingIndex]))))
					
				}
            	if(sort == "descending"){
            		
            		myBarPlot = ggplot(tableBase, aes(x = fct_infreq(fct_infreq(tableBase[,countingIndex]))))
            			
            	}
            	if(sort == "none"){
            	
            		myBarPlot = ggplot(tableBase, aes(x = tableBase[,countingIndex]))
            			
            	}
            	#if(sort == "alphabetically"){}
            	
            	
            	myBarPlot = myBarPlot + geom_bar(stat = "count", position=position_dodge(), colour="black")
            	
            }
            
            # Create bars
            # Give it a sorting order
            #myBarPlot = myBarPlot +
            #if(sort == "ascending")
                #myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = fct_rev(fct_infreq(tableBase[,countingIndex]))), position=position_dodge(), colour="black")
            #if(sort == "descending")
            	#myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = fct_infreq(tableBase[,countingIndex])),          position=position_dodge(), colour="black")
            #if(sort == "none")
                #myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = fct_rev(tableBase[,countingIndex])),             position=position_dodge(), colour="black")        
                #myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = tableBase[,countingIndex]),             position=position_dodge(), colour="black")        
            # alphabetical missing
            #if(sort == "none")
            
            print("BplotV2 - C")    
            # Write the text in the bar
            # This change slightly on whether you are rotated or not
            if(rotation == FALSE)
                myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), vjust = -0.5, color = "white", fontface = "bold")
            else{
                myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), hjust = -0.2, vjust = 0.5, color = "white", fontface = "bold")    
            }            
                
        }
    	
    	# RELATIVE , TODO (Can there be relative without grouping?)
        else{
            
        }
        
    }
    # .. YES
    else{
        print("B grouping")
    	
	    if(countingType == "absolute"){
	    	
	    	myBarPlot = ggplot(tableBase, aes(tableBase[,countingIndex], fill = tableBase[,groupIndex]))
	    	myBarPlot = myBarPlot + geom_bar(stat = "count", position = position_dodge(preserve = "single"), colour="black")
	    	
	        if(rotation == FALSE)
	        	myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), vjust = -0.5, color = "white", fontface = "bold")
	        else{
	        	print("LONG BAR TEXT")
	            myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), hjust = -0.5, vjust = 0.5, color = "white", fontface = "bold")    
	            #myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), color = "black", fontface = "bold")    
	        }      	

	    }
    	
	    else{
	    	
	    	print("LONG BAR TEXT RELATIVE")
	    	myBarPlot = ggplot(tableBase, aes(x = tableBase[,countingIndex], fill = tableBase[,groupIndex]))
	    	myBarPlot = myBarPlot + geom_bar(position = "fill", colour="black")
	    	myBarPlot = myBarPlot + scale_y_continuous(labels = scales::percent_format())
	    	#myBarPlot = myBarPlot + geom_bar(stat = "identity", position = "fill", colour="black")
	    	
	        #if(rotation == FALSE)
	        #	myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), vjust = -0.5, color = "white", fontface = "bold")
	        #else{
	        
	         #   myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), hjust = -0.5, vjust = 0.5, color = "white", fontface = "bold")    
	            #myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), color = "black", fontface = "bold")    
	              	
    
    	}    	
    	
        # In either case, check if there is a color vector
	    if(!is.null(colorsVector))
	    	myBarPlot = myBarPlot + scale_fill_manual(values=colorsVector)	    	

    		
    }
    
    #print("BplotV2 - D")
    
    # Add the common theme parts for both options,
    # but both change slightly if the plot is rotated or not
    {

    	#print("BplotV2 - D1")
    	print(plotTitle)
    	print(plotSubtitle)
    	print(plotCaption)
    	print(countingName)
    	print(plotXLabel)
    	print(plotYLabel)
    	
        myBarPlot = myBarPlot +
              
        # Create titles and subtitles
        labs(title    = plotTitle,
             subtitle = plotSubtitle,
             caption  = plotCaption,
             fill     = countingName,
             x = plotXLabel, y = plotYLabel)
                            
        
        #print("BplotV2 - D2")
        
        if(rotation == FALSE){
                  
            myBarPlot = myBarPlot +
                                  
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]],
                  legend.position    = themeData[[5]],
                  axis.ticks         = themeData[[6]],
                  axis.text.x        = themeData[[7]],
                  axis.text.y        = themeData[[8]]) 
                  
        }
        else{
                  
            myBarPlot = myBarPlot +
                                  
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[4]], # <- this is swapped
                  panel.grid.major.x = themeData[[3]],
                  legend.position    = themeData[[5]],
                  axis.ticks         = themeData[[6]],
                  axis.text.x        = themeData[[7]],
                  axis.text.y        = themeData[[8]]) 
                  
        }

        #print("BplotV2 - D3")

    }
         
    # Transform this into a pie chart if needed 
    if(FALSE){
    if(polarCoordinates == TRUE){
            
        myBarPlot =  myBarPlot + coord_polar("y", start=0)
        myBarPlot =  myBarPlot + theme(axis.text.x  = element_blank(),
                                       axis.text.y  = element_blank(),
                                       axis.line.x  = element_blank(),
                                       axis.line.y  = element_blank(),
                                       axis.title.x = element_blank(),
                                       axis.title.y = element_blank(),
                                       panel.border = element_blank(),
                                       axis.ticks   = element_blank())
            
    }
    }
          
    # Turn the plot around if needed
    if(rotation == TRUE)  myBarPlot = myBarPlot + coord_flip()
          
    # Turn the X axys labels by the given angle
    if(givenAngle != 0)
    myBarPlot = myBarPlot + theme(axis.text.x = element_text(angle = givenAngle, vjust = 0.5, hjust=1))
    
    # If you want to override the lagend options
    if(legendPosition != "same"){
    
    	if(legendPosition == "none")  myBarPlot = myBarPlot + theme(legend.position = 'none')
    		
    	if(legendPosition == "bottom") myBarPlot = myBarPlot + theme(legend.position = 'bottom')
    	
    }
    
    
    # Save the image and the txt files with the data
    # -- Check if the user want to override dimensions
    if(overrideImageWidth > 0)  imageWidth  = overrideImageWidth
    if(overrideImageHeight > 0) imageHeight = overrideImageHeight
    
    print(imageFilePath)
    print(myBarPlot)
    
    ggsave(imageFilePath, plot = myBarPlot, width = imageWidth, height = imageHeight, limitsize = FALSE)  
    ggsave(changeFileExtension(imageFilePath, "pdf"), plot = myBarPlot, width = imageWidth, height = imageHeight, limitsize = FALSE)  
    
    
    # Return and finish      
    myReturn = vector("list", length = 2)
    myReturn[[1]] = myBarPlot
    myReturn[[2]] = imageFilePath
          
    return (myReturn)        
    
}




# TODO: Clean code and delete these functions
if(FALSE){


        # Do a simple bar plot
        # (ie: Men and Women)
        #
        # This includes whether you have an absolute count plot, or relative
        # count plot.
        # (ie: 120 Men and 80 Women , or, 60% Men and 40% Women)
        #
        # You can also create a piechart plot directly from here. You can select
        # the absolute or relative count no matter whether you are using a bar
        # or piechart.
        #
        # Later on you will find another function that creates a piechart. This
        # is done just for clarity in the code, but that function the only thing
        # that does is call this function with the proper parameters. (TODO)
        #
        # (bool) showNumbers. Whether you want numbers counting how many things
        #                     do you have for each modality to appear in the
        #                     plot. Default is TRUE. It doesn't matter whether
        #                     you do an absolute count or relative count. But
        #                     if you do a relative count, the number will have
        #                     a "%" character next to it. (TODO)
        #
        # (int) cropNumbers.  If modality is too small, it get annoying to have
        #                     the text on top of it. You can choose to crop the
        #                     labels if it falls bellow a certain threshold.
        #                     The default is 0 which means don't crop anything.
        #
        # (string) sort       If you want the plot to have some order
        #                       "descending" default, from high to low,
        #                       "asscending"          from low to high,
        #                       "none"                alphabetical
        #
        # (bool) plotRotate   If you want to flip the X and Y axis. Check
        #                     the longbarplot() function if you want to plot
        #                     a very long barplot; that one is prepared to look
        #                     nicer without height restrictions. Default FALSE.
        #  
        # (float) overrideImageX don't do automatic addjustment of image size
        #                        and use this instead
        #   
        #
        # Numbers appears just under the bar always (RIGHT NOW IS THIS)
        #
        # TODO:
        #
        # (int) minValue.     If a modality count is too low, delete it
        #                     from the plot if total count is strict lower than
        #                     this minimal value. Default is 0, meaning that you
        #                     don't delete anything          
        # crop = Cut the bars after some value
        #
        # top = Take only the top X bars sorted by value count
        #
        # (string) countingType. "count"     / normal bar plot
        #                        "identity" /  relative bar plot
        
        doBarPlot <- function(tableBase, countingIndex, plotFilePath,
                              colorsVector = NULL,
                              countingType = "count",
                              showNumbers  = TRUE,
                              cropNumbers  = 0,
                              minValue     = 0,
                              sort = "descending",
                              plotRotate = FALSE,
                              polarCoordinates = FALSE,
                              plotTitle = NULL, plotSubtitle = NULL,
                              plotCaption = NULL,
                              plotXLabel = NULL, plotYLabel = NULL,
                              plotTheme = NULL,
                              overrideTableName = NULL,
                              overrideCaption   = NULL,
                              logFileDescriptor = NULL,
                              supressWarnings = FALSE,
                              overrideImageWidth  = 0,
                              overrideImageHeight = 0){
          
            
            
            
            
          # Init variables
          {
            myPlotType = "AbsBarplot"
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
            if(countingType == "identity"){
              myPlotType = "RelBarplot"  
            }          
            
          }
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
          
            # If the filePath is not null, find the final filepath
            # If the filePath is null it means that this plot is part of a composite of another plot
            if(!is.null(plotFilePath)){
              
                myFileExtension = getFileExtension(plotFilePath)
              
                if(myFileExtension == ".png") imageFilePath = plotFilePath
              
                else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                       tableName = myTableName, fileType = myPlotType,
                                                       variableIndex1 = countingIndex)              
            }
            

            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Delete the data that we are not going to use, if any
          if(FALSE){
          if(minValue !=0){
          
              summaryResult      = summarizeCategorical(tableBase,countingIndex)
              modalitiesToDelete = summaryResult[summaryResult$Count<minValue,1]
              totalToDelete      = length(totalToDelete)
              
              for (i in 1:totalToDelete) {
              
                  tableBase = tableBase[tableBase[,countingIndex] != modalitiesToDelete[i],]
                      
              }
              
          }
          }
          
          # Get info about different categories
          myCategories = unique(tableBase[,countingIndex])
          nCategories  = length(myCategories)
          groupingName = colnames(tableBase)[countingIndex]
          imageWidth   = max(5, nCategories * 2)  # Minimum 5 , so we can at least see the legend if there are too little categories
                                                  # It sound counter intuitive that the minimum is found by the max(), but trust me.
          imageHeight  = 8                        # Default image height, usually 8 is perfect in all cases.
          
          
          # If you try to do a barplot with way too many categories, make a warning about it
          if(nCategories > 16){
            
            if(supressWarnings == FALSE){
              
              warningString = ""
              warningString = paste0(warningString , " ---------------"  , "\n")
              warningString = paste0(warningString , " -- WARNING!! --"  , "\n")
              warningString = paste0(warningString , " ---------------"  , "\n")
              warningString = paste0(warningString , ""                  , "\n")
              warningString = paste0(warningString , " You are trying to do a bar plot with more than 16 categories. Are you sure? "       , "\n")
              warningString = paste0(warningString , " Notice that you have the function 'doLongBarPlot()' which will be a better option." , "\n")
              warningString = paste0(warningString , ""                  , "\n")
              
            }
            
            if(is.null(logFileDescriptor)){
              print(warningString)
            }
            else{
              print("No log!")
            }
            

          }
          
          # Prepare the defaults
          {
            defaultVector = getCategoricalDefaults(groupingName, myCategories, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, plotType = myPlotType)
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
          }
          
          # Generate an empty ggplot2 object
          myBarPlot = NA
          
          # You can either count all the element one by one (classic)
          if(countingType == "count"){
            
            # Init the plot 
            #myBarPlot = ggplot(tableBase, aes(tableBase[,countingIndex], fill = tableBase[,countingIndex]))
            myBarPlot = ggplot(tableBase, aes(tableBase[,countingIndex]))
            
            # Create bars
            # Give it a sorting order
            if(sort == "ascending")
                myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = fct_rev(fct_infreq(tableBase[,countingIndex]))), position=position_dodge(), colour="black")
            if(sort == "descending")
                myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = fct_infreq(tableBase[,countingIndex])),          position=position_dodge(), colour="black")
            if(sort == "none")
                myBarPlot = myBarPlot + geom_bar(stat = "count", aes(x = fct_rev(tableBase[,countingIndex])),             position=position_dodge(), colour="black")        
                
            # Write the text in the bar
            # This change slightly on whether you are rotated or not
            if(plotRotate == FALSE)
                myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), vjust = 1.5, color = "white", fontface = "bold")
            else{
                myBarPlot = myBarPlot + geom_shadowtext(stat = "count", aes(label=..count..), position = position_dodge(0.9), hjust = 1.3, vjust = 0.5, color = "white", fontface = "bold")    
            }
                
            
              # Create bars
              #geom_bar(stat = "count", position = position_dodge(), colour="black") +
              #scale_fill_manual(values = colorsVector, na.value = COLOR_NA) +
              
              
              
            
            
            
          }
          # Or have a table that is already summarized
          else{
            
            # Count the percentages
            currentSummary         = summarizeCategorical(tableBase, countingIndex, sorted="none")
            
            # Make the labels
            currentSummary$Label   = ""
            # -- If you don't want to crop any number, do the default 
            if(cropNumbers <= 0){
              currentSummary$Label   = paste0( round(currentSummary[,3]*100,2), "%")  
            }
            # -- Otherwise, check one by one, label only those above the given
            #    threshold.
            else{
              
              for (i in 1:nrow(currentSummary)) {
            
                if(currentSummary[i,3] >= cropNumbers){
                  currentSummary$Label[i] = paste0( round(currentSummary[i,3]*100,2), "%")    
                }
                
              }
              
            }
            
            currentSummary$Label_Y = cumsum(currentSummary[,3])
            
            # Init the plot 
            myBarPlot = ggplot(data = currentSummary, aes(x = 0, y = currentSummary[,3], fill = currentSummary[,1])) + 
              scale_fill_manual(values = colorsVector, na.value = COLOR_NA) +
              geom_bar(stat = "identity", color = "black") +
              
              geom_shadowtext(aes(label = Label), position = position_stack(vjust = 0.5)) +                 # Text on center
              # geom_shadowtext(aes(label = currentSummary[,4], y = currentSummary[,5]), color = "white", vjust = 1.5) + # Text on top
              scale_x_continuous(expand = c(0,0)) + 
              scale_y_continuous(labels = scales::percent) + 
              scale_fill_manual(values = colorsVector, na.value = COLOR_NA)
            
            
          }
          
          # Add the common parts for both options,
          # but both change slightly if the plot is rotated or not
          {

                            
              myBarPlot = myBarPlot +
              
              # Create titles and subtitles
              labs(title    = plotTitle,
                   subtitle = plotSubtitle,
                   caption  = plotCaption,
                   fill     = groupingName,
                   x = plotXLabel, y = plotYLabel)
                            
              if(plotRotate == FALSE){
                  
                  myBarPlot = myBarPlot +
                                  
                  # Apply the theme
                  theme(panel.background   = themeData[[1]],
                        axis.line          = themeData[[2]],
                        panel.grid.major.y = themeData[[3]],
                        panel.grid.major.x = themeData[[4]],
                        legend.position    = themeData[[5]],
                        axis.ticks         = themeData[[6]],
                        axis.text.x        = themeData[[7]],
                        axis.text.y        = themeData[[8]]) 
                  
              }
              else{
                  
                  myBarPlot = myBarPlot +
                                  
                  # Apply the theme
                  theme(panel.background   = themeData[[1]],
                        axis.line          = themeData[[2]],
                        panel.grid.major.y = themeData[[4]],
                        panel.grid.major.x = themeData[[3]],
                        legend.position    = themeData[[5]],
                        axis.ticks         = themeData[[6]],
                        axis.text.x        = themeData[[7]],
                        axis.text.y        = themeData[[8]]) 
                  
                  
               }


          }
          
          # Transform into a pie chart if you want to
          if(polarCoordinates == TRUE){
            
            myBarPlot =  myBarPlot + coord_polar("y", start=0)
            myBarPlot =  myBarPlot + theme(axis.text.x  = element_blank(),
                                           axis.text.y  = element_blank(),
                                           axis.line.x  = element_blank(),
                                           axis.line.y  = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.title.y = element_blank(),
                                           panel.border = element_blank(),
                                           axis.ticks   = element_blank(),
            )
            
          }
          
          # Turn the plot around if needed
          if(plotRotate == TRUE)  myBarPlot = myBarPlot + coord_flip()
          
          
          # Save the image and the txt files with the data
          # -- Check if the user want to override dimensions
          if(overrideImageWidth>0)  imageWidth  = overrideImageWidth
          if(overrideImageHeight>0) imageHeight = overrideImageHeight
          
          #latexFilePath = ""
          if(imageFilePath!=""){
            ggsave(imageFilePath, plot = myBarPlot, width = imageWidth, height = imageHeight)  
            #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)            
          }
          

          
          myReturn = vector("list", length = 2)
          myReturn[[1]] = myBarPlot
          myReturn[[2]] = imageFilePath
          #myReturn[[3]] = latexFilePath
          
          return (myReturn)
          
        }
        
        
          
      
        
        
        
        # Do a simple bar plot for variables with maaaaany categories
        # It doesn't apply any color and the labels are rotated 90º
        # Warning, no limit for image width/height
        #
        # crop = Cut the bars after some value
        #
        # top = Take only the top X bars sorted by value
        # 
        doLongBarPlot <- function(tableBase, countingIndex, plotFilePath,
                                  colorsVector = NULL,
                                  countingType = "count",
                                  plotTitle = NULL, plotSubtitle = NULL,
                                  plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                  plotTheme = NULL,
                                  barsFontSize = 2,
                                  overrideTableName = NULL,
                                  overrideCaption   = NULL,
                                  crop = 0, top = 0, sort = "descending",
                                  overrideHeigh = NULL){
          
          # Init variables
          {
            myPlotType = "LongAbsBarplot"
            if(countingType == "identity") myPlotType = "LongRelBarplot"
            
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
          }
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
            
            myFileExtension = getFileExtension(plotFilePath)
            
            if(myFileExtension == ".png") imageFilePath = plotFilePath
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = countingIndex)
            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Get info about different categories
          myCategories = unique(tableBase[,countingIndex])
          nCategories  = length(myCategories)
          groupingName = colnames(tableBase)[countingIndex]
          
          # Prepare the defaults
          {
            defaultVector = getCategoricalDefaults(groupingName, myCategories, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, plotType = myPlotType)
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
          }
          
          # Crop the values if needed
          if(top > 0){
            
            mySummary = summarizeCategorical(tableBase, countingIndex, sorted = "top", crop = top)
            filterTheseCatOnly = mySummary[,1]
            tableBase = tableBase[(tableBase[,countingIndex] %in% filterTheseCatOnly),]
            
          }
          
          # Do the plot
          myPlot = ggplot(tableBase, aes(tableBase[,countingIndex]))
            
          # Give it a sorting order
          if(sort == "descending")
            myPlot = myPlot + geom_bar( aes(x = fct_rev(fct_infreq(tableBase[,countingIndex]))), position=position_dodge())
          if(sort == "asscending")
            myPlot = myPlot + geom_bar( aes(x = (fct_infreq(tableBase[,countingIndex]))), position=position_dodge())
          if(sort == "none")
            myPlot = myPlot + geom_bar( aes(x = fct_rev(tableBase[,countingIndex])), position=position_dodge())
            
          # Add the rest of the things
          myPlot = myPlot +
            # Create bars
            #geom_bar( aes(x = fct_rev(fct_infreq(tableBase[,countingIndex]))), position=position_dodge()) +
            
            # Write the text in the bar
            #geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold") +
            
            #geom_text(stat='count', aes(label=..count..), position = position_dodge(0.9), vjust = 1.5, color = "white") +
            
            geom_shadowtext(stat='count', aes(label=..count..), position = position_dodge(0.9), hjust = 1.5, color = "white", fontface = "bold", size = barsFontSize) +
            #geom_shadowtext(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold", size = 2) +
            
            # Write the text in the bar
            #geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold") +
            #geom_shadowtext(stat='count', aes(label=..count..), position = position_stack(vjust = 0.95), color = "white", fontface = "bold") +
            
            #Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 fill     = groupingName,
                 x = plotXLabel, y = plotYLabel,
                 
                 axis.text.x=element_text(size=rel(0.3))
                 
                 #axis.text.x=element_text(margin = margin(t = 0))
                 
            ) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]]) + 
            
            # Turn it around for easy reading
            coord_flip()
          
          # Save the image and the txt files with the data
          imageHeigh   = nCategories/5
          if(!is.null(overrideHeigh)) imageHeigh = overrideHeigh
          ggsave(imageFilePath, height = imageHeigh, plot = myPlot, limitsize = FALSE)
          
          #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
          
          myReturn = vector("list", length = 2)
          myReturn[[1]] = myPlot
          myReturn[[2]] = imageFilePath
          #myReturn[[3]] = latexFilePath
          
          return (myReturn)
          
        }
        
     
        
      
        
      # Do a combine bar plot
      # With bar stacked up to relative total
      # colorsVector should be the same as the groupIndex if you want it to make sense
      doBarRelativeCombinePlot <- function(tableBase, countingIndex, groupIndex, plotFilePath,
                                           colorsVector = NULL,
                                           plotTitle = NULL, plotSubtitle = NULL,
                                           plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                           plotTheme = NULL,
                                           overrideTableName = NULL,
                                           overrideCaption   = NULL,
                                           supressWarnings   = FALSE){

        
        # Init variables
        {
          
          # Plot type
          myPlotType = "CombinedRelBarplot"
          
          # Table name
          myTableName = deparse(substitute(tableBase))
          
          if(!is.null(overrideTableName)){
            myTableName = overrideTableName
            
          }
          
        }
        
        # Get an automatic imagePath name if you don't have a proper one
        {
          
          imageFilePath = ""
          
          myFileExtension = getFileExtension(plotFilePath)
          
          if(myFileExtension == ".png") imageFilePath = plotFilePath
          
          else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                 tableName = myTableName, fileType = myPlotType,
                                                 variableIndex1 = countingIndex,
                                                 variableIndex2 = groupIndex)
          
        }
        
        # Get the theme information
        themeData = getThemeParameters(plotTheme)
        
        # Convert the numerical in categories if needed
        countingVariableType = class(tableBase[,countingIndex])
        
        if(countingVariableType != "character" && countingVariableType != "factor") {
          
          # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
          myLevels = sort(unique(tableBase[,countingIndex]))
          myLevels = as.character(myLevels)
          
          tableBase[,countingIndex] = as.character(tableBase[,countingIndex])
          tableBase[,countingIndex] = factor(tableBase[,countingIndex], levels = myLevels)
          
          # Sometimes you know that you are casting categories on porpoise
          # So don't show me warnings if I say so
          if(supressWarnings == FALSE){

            print("WARNING!!")
            print("Doing the relative barplot for:")
            print(myTableName)
            print("With indexes")
            print(countingIndex)
            print(groupIndex)
            print("I found a numerical variable")
            print("I transformed into categorical automatically and did the plot anyway")
            print("Maybe you wanted to do something else?")
            
          }
          
        }

        # Get info about different categories
        {
          myCategoriesA = unique(tableBase[,countingIndex])
          nCategoriesA  = length(myCategoriesA)
          groupingNameA = colnames(tableBase)[countingIndex]
          
          myCategoriesB = unique(tableBase[,groupIndex])
          nCategoriesB  = length(myCategoriesB)
          groupingNameB = colnames(tableBase)[groupIndex]
        }

        # Prepare the defaults
        {

          defaultVector = getBiCategoricalDefaults(groupingNameB, myCategoriesB, 
                                                   groupingNameA, myCategoriesA, 
                                                   colorsVector = colorsVector,
                                                   plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                                   plotCaption = plotCaption, plotXLabel = plotXLabel,
                                                   plotYLabel = plotYLabel, plotType = myPlotType)
          
          
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }
        
        # Transform into relative table
        # Mutate the table to do the cumulative sum easier
        # (I hate you dplyr!)
        {
          
          totalRows     = nrow(tableBase)
          
          copyTable   = data.frame(matrix(NA, nrow = totalRows, ncol = 2), stringsAsFactors = TRUE)
          
          colnames(copyTable) = c("G","C")
          copyTable$G = tableBase[,groupIndex]
          copyTable$C = tableBase[,countingIndex]
          
          proportionTable <-
            copyTable %>%
            group_by(G, C) %>%
            tally() %>%
            group_by(G) %>%
            mutate(pct = n / sum(n))
          
          proportionTable <- proportionTable %>%
            group_by(G) %>%
            mutate(label_y = cumsum(pct))
        }

        # Delete labels that are too little and are annoying in the plot
        {
          proportionTable$myLabel = paste(round(proportionTable$pct*100, 2),"%",sep = '')
          proportionTable$myLabel[proportionTable$pct < 0.05] = ""        
        }

        # Do the plot
        {
          
        
        myPlot = ggplot(proportionTable, aes(  x = G , y = pct , fill = fct_rev(proportionTable[,2][[1]]))) +
          
          geom_bar(stat = "identity", color = "black") +
          
          scale_fill_manual(values = rev(colorsVector)) +
          
          geom_shadowtext(aes(label = myLabel, y = label_y), vjust = 1.5, color = "white") +
          
          scale_y_continuous(labels = scales::percent) +
           
          # I'm trying to force all labels here, but I have no idea how     
          #scale_x_discrete(G, labels = G, breaks = G) + 
                

          # Create titles and subtitles
          labs(title    = plotTitle,
               subtitle = plotSubtitle,
               caption  = plotCaption,
               fill     = groupingNameA,
               x = plotXLabel, y = plotYLabel) +
          
          # Apply the theme
          theme(panel.background   = themeData[[1]],
                axis.line          = themeData[[2]],
                panel.grid.major.y = themeData[[3]],
                panel.grid.major.x = themeData[[4]],
                legend.position    = themeData[[5]])
        
        }
        
        # Save the image
        imageWidth    = max(5, nCategoriesB * 2)
        ggsave(imageFilePath, plot = myPlot, width = imageWidth)  
        # latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)

        myReturn = vector("list", length = 2)
        myReturn[[1]] = myPlot
        myReturn[[2]] = imageFilePath
        #myReturn[[3]] = latexFilePath

        return(myReturn)
        
      }
       
      
      
      # Same as before, but in horizontal
      doLongBarRelativeCombinePlot <- function(tableBase, countingIndex, groupIndex, plotFilePath,
                                               barsFontSize = 2,
                                               colorsVector = NULL,
                                               plotTitle = NULL, plotSubtitle = NULL,
                                               plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                               plotTheme = NULL,
                                               overrideTableName = NULL,
                                               overrideCaption   = NULL,
                                               supressWarnings   = FALSE, sort = "descending",
                                               imageHeight = NULL, imageWidth = NULL){
        
        
            # Init variables
            {
          
                # Plot type
                myPlotType = "CombinedRelBarPlot"
          
                # Table name
                myTableName = deparse(substitute(tableBase))
          
                if(!is.null(overrideTableName)) myTableName = overrideTableName
             
            }
        
            # Get an automatic imagePath name if you don't have a proper one
            {
          
                imageFilePath = ""
          
                myFileExtension = getFileExtension(plotFilePath)
          
                if(myFileExtension == ".png") imageFilePath = plotFilePath
          
                else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                       tableName = myTableName, fileType = myPlotType,
                                                       variableIndex1 = countingIndex,
                                                       variableIndex2 = groupIndex)
          
            }
          
            
        
            # Get the theme information
            themeData = getThemeParameters(plotTheme)
        
            # Convert the numerical in categories if needed
            countingVariableType = class(tableBase[,countingIndex])
        
            if(countingVariableType != "character" && countingVariableType != "factor") {
          
                # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
                myLevels = sort(unique(tableBase[,countingIndex]))
                myLevels = as.character(myLevels)
          
                tableBase[,countingIndex] = as.character(tableBase[,countingIndex])
                tableBase[,countingIndex] = factor(tableBase[,countingIndex], levels = myLevels)
          
                # Sometimes you know that you are casting categories on porpoise
                # So don't show me warnings if I say so
                if(supressWarnings == FALSE){
            
                    print("WARNING!!")
                    print("Doing the relative barplot for:")
                    print(myTableName)
                    print("With indexes")
                    print(countingIndex)
                    print(groupIndex)
                    print("I found a numerical variable")
                    print("I transformed into categorical automatically and did the plot anyway")
                    print("Maybe you wanted to do something else?")
            
                }
          
            }
        
            # Get info about different categories
            {
          
                myCategoriesA = NA
                myCategoriesB = NA  
            
                if(is.factor(tableBase[,countingIndex])){
                    myCategoriesA = levels(tableBase[,countingIndex])
                }
                else{
                    myCategoriesA = unique(tableBase[,countingIndex])
                }
            
                nCategoriesA  = length(myCategoriesA)
                groupingNameA = colnames(tableBase)[countingIndex]
          
                if(is.factor(tableBase[,groupIndex])){
                    myCategoriesB = levels(tableBase[,groupIndex])
                }
                else{
                    myCategoriesB = unique(tableBase[,groupIndex])
                }
            
                nCategoriesB  = length(myCategoriesB)
                groupingNameB = colnames(tableBase)[groupIndex]
            }
        
        # Prepare the defaults
        {
          
          defaultVector = getBiCategoricalDefaults(groupingNameB, myCategoriesB, 
                                                   groupingNameA, myCategoriesA, 
                                                   colorsVector = colorsVector,
                                                   plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                                   plotCaption = plotCaption, plotXLabel = plotXLabel,
                                                   plotYLabel = plotYLabel, plotType = myPlotType)
          
          
          
          colorsVector  = defaultVector[[1]]
          plotTitle     = defaultVector[[2]][1]
          plotSubtitle  = defaultVector[[3]][1]
          plotCaption   = defaultVector[[4]][1]
          plotXLabel    = defaultVector[[5]][1]
          plotYLabel    = defaultVector[[6]][1]
        }
        
        # Transform into relative table
        # Mutate the table to do the cumulative sum easier
        # (I hate you dplyr!)
        {
          
          totalRows     = nrow(tableBase)
          
          copyTable   = data.frame(matrix(NA, nrow = totalRows, ncol = 2), stringsAsFactors = TRUE)
          
          colnames(copyTable) = c("G","C")
          copyTable$G = tableBase[,groupIndex]
          copyTable$C = tableBase[,countingIndex]
          
          proportionTotalRows = nCategoriesA * nCategoriesB
          proportionTable = DF(proportionTotalRows,6)
          colnames(proportionTable) = c("G","C","n","pct","label_y","myLabel")
          
          currentIndex = 1
          
          preCumTotal  = 0
          cumTotal     = 0
          
          
          for(j in 1:nCategoriesB){
          
                preCumTotal  = 0  
                cumTotal     = 0 
              
                for(i in 1:nCategoriesA){
                    
                    myGCategory = as.character(myCategoriesB[j])
                    myCCategory = as.character(myCategoriesA[i])
                  
                    proportionTable[currentIndex,1] = myGCategory         
                    proportionTable[currentIndex,2] = myCCategory
                
                    currentSubtable    = copyTable[copyTable$G == myGCategory & copyTable$C == myCCategory,]
                    currentConditional = copyTable[copyTable$G == myGCategory,]
                  
                    proportionTable[currentIndex,3] = nrow(currentSubtable)
                    proportionTable[currentIndex,4] = nrow(currentSubtable)/nrow(currentConditional)
                  
                    cumTotal     = cumTotal + nrow(currentSubtable)/nrow(currentConditional)
                  
                    proportionTable[currentIndex,5] = (cumTotal + preCumTotal)/2
                  
                    preCumTotal = cumTotal
                  
                    currentIndex = currentIndex + 1
                  
              }  

          }

          # WHAT THE FUCKING FUCK IS THIS STUPID SYNTAXIS
          # What the hell dplyr?
          #
          # proportionTable <-
          #   copyTable %>%
          #   group_by(G, C) %>%
          #   #tally() %>%
          #   group_by(G) %>%
          #   mutate(pct = n / sum(n))
          # 
          # proportionTable <- proportionTable %>%
          #   group_by(G) %>%
          #   mutate(label_y = cumsum(pct))
          
        }
        
        # Delete labels that are too little and are annoying in the plot
        {
          proportionTable$myLabel = paste(round(proportionTable$pct*100, 2),"%",sep = '')
          proportionTable$myLabel[proportionTable$pct < 0.05] = ""        
        }
        
        # WHY IN FLYING SHIT, DOES NOT DATAFRAMES HAVE A SIMPLE REVERSE OPERATION!???
        # Really, look at this crap. There is no myDataframe.reverseRow()
        # You need to do it the supercomplicated way, do an apply rev,
        # and whatever the shit that function gives you need to be casted to dataframe again
        # FUCK R, and FUCK WHOEVER DECIDED ON THIS SYNTAX!
        
        # calculating reverse
        #proportionTable = as.data.frame(apply(proportionTable, 2, rev))
        
        
        # Do the plot
        {

          #myPlot = ggplot(proportionTable, aes(  x = G , y = pct , fill = fct_rev(proportionTable[,2][[1]])))
          
          
          #myPlot = ggplot(proportionTable, aes(  x = G , y = pct, fill = fct_rev(C)))
          
          myPlot = ggplot(proportionTable, aes(  x = fct_rev(G) , y = pct, fill = C))
            
          
          # Give it a sorting order
          if(sort == "descending")
            myPlot = myPlot + geom_bar(stat = "identity", color = "black",
                                       aes(x = fct_rev(fct_infreq(proportionTable[,2][[1]]))))
          if(sort == "asscending")
            myPlot = myPlot + geom_bar(stat = "identity", color = "black",
                                       aes(x = (fct_infreq(proportionTable[,2][[1]]))))
          if(sort == "none")
            myPlot = myPlot + geom_bar(stat = "identity", color = "black")

          # Do the rest
          myPlot = myPlot +
          
            scale_fill_manual(values = rev(colorsVector)) +
            
            geom_shadowtext(aes(label = myLabel, y = label_y), hjust = 0.5, color = "white", fontface = "bold", size = barsFontSize) +

            scale_y_continuous(labels = scales::percent) +
            
            # Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 fill     = groupingNameA,
                 x = plotXLabel, y = plotYLabel) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]],
                  legend.position    = themeData[[5]])
          
        }
        
        # Rotate the plot so the bars are horizontal and is easier
        # to read from top to bottom.
        myPlot = myPlot + coord_flip()
        
        # Save the image
        myImageHeight = nCategoriesB/20
        if(!is.null(imageHeight)) myImageHeight = imageHeight 
        
        myImageWidth  = 8
        if(!is.null(imageWidth))  myImageWidth = imageWidth
        print(myImageHeight)
        ggsave(imageFilePath, width = myImageWidth, height = myImageWidth, plot = myPlot, limitsize = FALSE)
        #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
        
        # Return everything
        myReturn = vector("list", length = 2)
        myReturn[[1]] = myPlot
        myReturn[[2]] = imageFilePath
        #myReturn[[3]] = latexFilePath
        
        return(myReturn)
        
      }
      
      
      
      
doLongBarAbsoluteCombinePlot <- function(tableBase, xIndex, yIndex, colorIndex,
                                         plotFilePath,
    
                                         barsFontSize = 2,
                                         colorsVector = NULL,
                                         plotTitle = NULL, plotSubtitle = NULL,
                                         plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                         plotTheme = NULL,
                                         overrideTableName = NULL,
                                         overrideCaption   = NULL,
                                         supressWarnings   = FALSE, sort = "descending",
                                         imageHeight = NULL, imageWidth = NULL){
          
          
    # Init variables
    {
    
        # Plot type
        myPlotType = "CombinedAbsBarPlot"
                          
        # Table name
        myTableName = deparse(substitute(tableBase))
                          
        if(!is.null(overrideTableName)) myTableName = overrideTableName
                          
    }
          
    # Get an automatic imagePath name if you don't have a proper one
    {
              
        imageFilePath = ""
              
        myFileExtension = getFileExtension(plotFilePath)
        
        if(myFileExtension == ".png") imageFilePath = plotFilePath
                  
        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                               tableName = myTableName, fileType = myPlotType,
                                               variableIndex1 = xIndex,
                                               variableIndex2 = colorIndex)
              
    }
          
    # Get the theme information
    themeData = getThemeParameters(plotTheme)
          
    # Convert the numerical in categories if needed
    countingVariableType = class(tableBase[,xIndex])
          
    if(countingVariableType != "character" && countingVariableType != "factor") {
              
        # Make the order of the number consistence (0 1 10 11 12 2 3 4 is not sorted!)
        myLevels = sort(unique(tableBase[,xIndex]))
        myLevels = as.character(myLevels)
              
        tableBase[,xIndex] = as.character(tableBase[,xIndex])
        tableBase[,xIndex] = factor(tableBase[,xIndex], levels = myLevels)
              
        # Sometimes you know that you are casting categories on porpoise
        # So don't show me warnings if I say so
        if(supressWarnings == FALSE){
                  
            print("WARNING!!")
            print("Doing the relative barplot for:")
            print(myTableName)
            print("With indexes")
            print(xIndex)
            print(colorIndex)
            print("I found a numerical variable")
            print("I transformed into categorical automatically and did the plot anyway")
            print("Maybe you wanted to do something else?")
                  
        }
              
    }
          
    # Get info about different categories
    {
                          
        myCategoriesA = NA
        myCategoriesB = NA  
                          
        if(is.factor(tableBase[,xIndex])) myCategoriesA = levels(tableBase[,xIndex])
        else myCategoriesA = unique(tableBase[,xIndex])
                          
        nCategoriesA  = length(myCategoriesA)
        groupingNameA = colnames(tableBase)[xIndex]
                          
        if(is.factor(tableBase[,colorIndex])) myCategoriesB = levels(tableBase[,colorIndex])
        else myCategoriesB = unique(tableBase[,colorIndex])
                        
        nCategoriesB  = length(myCategoriesB)
        groupingNameB = colnames(tableBase)[colorIndex]
                        
    }
          
    # Prepare the defaults
    {
              
        defaultVector = getBiCategoricalDefaults(groupingNameB, myCategoriesB, 
                                                 groupingNameA, myCategoriesA, 
                                                 colorsVector = colorsVector,
                                                 plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                                                 plotCaption = plotCaption, plotXLabel = plotXLabel,
                                                 plotYLabel = plotYLabel, plotType = myPlotType)
              
        colorsVector  = defaultVector[[1]]
        plotTitle     = defaultVector[[2]][1]
        plotSubtitle  = defaultVector[[3]][1]
        plotCaption   = defaultVector[[4]][1]
        plotXLabel    = defaultVector[[5]][1]
        plotYLabel    = defaultVector[[6]][1]
                        
    }
          
    # Create an standard table for the plot
    copyTable = tableBase
    colnames(copyTable)[xIndex]     = "myX"
    colnames(copyTable)[yIndex]     = "myY"
    colnames(copyTable)[colorIndex] = "myC"
    
    # RStudio is a stupid piece of shit software. Look at this shit:
    #
    #Error in `plot_dev()`:
    #! Unknown graphics device ''
    #Run `rlang::last_error()` to see where the error occurred.
    #
    # No feedback about what is unknown
    # No line on where the error occurred.
    # If you want to know the line, waste your time typing and running this function yourself
    # Because why save time when you can make the user micromanage every fucking thing!!??
       
    # Do the plot
    {
              
        myPlot = ggplot(copyTable, aes(  x = fct_rev(copyTable$myX) , y = copyTable$myY, fill = copyTable$myC))
              
        # Give it a sorting order
        if(sort == "descending")
            myPlot = myPlot + geom_bar(stat = "identity", color = "black",
            aes(x = fct_rev(fct_infreq(proportionTable[,2][[1]]))))
        if(sort == "asscending")
            myPlot = myPlot + geom_bar(stat = "identity", color = "black",
            aes(x = (fct_infreq(proportionTable[,2][[1]]))))
        if(sort == "none")
            myPlot = myPlot + geom_bar(stat = "identity", color = "black")
              
         # Do the rest
         myPlot = myPlot +
             
                  scale_fill_manual(values = rev(colorsVector)) +
                  
                  #geom_shadowtext(aes(label = myLabel, y = label_y), hjust = 0.5, color = "white", fontface = "bold", size = barsFontSize) +
                  
                  # Create titles and subtitles
                  labs(title    = plotTitle,
                       subtitle = plotSubtitle,
                       caption  = plotCaption,
                       fill     = groupingNameA,
                       x = plotXLabel, y = plotYLabel) +
                  
                  # Apply the theme
                  theme(panel.background   = themeData[[1]],
                        axis.line          = themeData[[2]],
                        panel.grid.major.y = themeData[[3]],
                        panel.grid.major.x = themeData[[4]],
                        legend.position    = themeData[[5]])
              
    }
          
    # Turn the plot 90º
    myPlot = myPlot + coord_flip()
          
    # Save the image
    # -- Set the width and height automatically unless the user says otherwise
    myImageHeight = nCategoriesB/20
    if(!is.null(imageHeight)) myImageHeight = imageHeight 
          
    myImageWidth  = 8
    if(!is.null(imageWidth))  myImageWidth = imageWidth
          print(imageFilePath)
    ggsave(imageFilePath, width = myImageWidth, height = myImageWidth, plot = myPlot, limitsize = FALSE)
          
          
    # Return everything
    myReturn = vector("list", length = 2)
    myReturn[[1]] = myPlot
    myReturn[[2]] = imageFilePath
          
    return(myReturn)
          
}



}