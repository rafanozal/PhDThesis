# Add the needed libraries
# ---- Basics
library(ggplot2)       # Basic ggplot2 library
library(ggnewscale)    # Allows for multiple color scales in one plot
library(RColorBrewer)  # Color settins and palettes
library(shadowtext)    # Drop shadows in text for better viewing
library(latex2exp)     # Allows latex expressions in ggplot
library(qqplotr)       # QQ plots with bands
library(forcats)       # Reverse order of factors (fct_rev)
library(gridExtra)     # grid.arrange()


# ------------------------------------------------
# ACTUAL PLOTS
# ------------------------------------------------

  
  # This section contain all the code for generating the actual plots.
  #
  # These are the common rules for ALL the plots
  #
  # (String) plotFilePath: This is the string where you want to save your plot.
  #                        It can be an specific file path, such as:
  #
  #                        "/home/username/Desktop/myPlot.png"
  #
  #                        In this case the function respect the original path
  #                        and save the image in this location exactly.
  #
  #                        Or it could be a specific folder instead: 
  #
  #                        "/home/username/Desktop/"
  #
  #                        In this case, the function will try to the best of it
  #                        habilities to gives an automatic filename which is
  #                        human readable and save it in that folder. This could
  #                        be something like:
  #
  #                        "Barplot_womenOnlyTable_Smoke.png"
  #
  #                        Which means that the file created is a bar plot, that
  #                        is using the dataframe named "womenOnlyTable" and is
  #                        using the "Smoke" variable to create this bar plot.
  #
  # Variables related to the automatic naming system:
  #
  # (String) plotTitle = NULL,
  # plotSubtitle = NULL,
  # plotCaption = NULL,
  # plotXLabel = NULL,
  # plotYLabel = NULL,
  # plotTheme = NULL,
  # overrideTableName = NULL,
  # overrideCaption   = NULL
  #
  # Variables related to the log system:
  #
  # TODO: logFileDescriptor here
  #
  # (bool) supressWarnings. Boolean that indicates whether you want to display
  #                         warnings related to the function or not. Default is
  #                         FALSE. By default it also print them in the console
  #                         Unless you have a file descriptor where you are
  #                         dumping all the warnings to analysize later.
  #
  # Returning variables:
  #
  # The function always return a list with some results. Some plots return a
  # bigger list than other and you need to check the specific information of
  # each function. But as a general rule, this is true for all the functions:
  #
  # (list) returnVector:
  #
  #                     1st element: The ggplot2 object
  #                     2nd element: The image complete filepath
  #                     3rd elment:  The latex filepath with the latex code
  #                                  that can be use to create this image
  #
  #                     ...
  #
  #                     1st from last element: Number of warnings raised
  #                     Last element:          Error code.
  #                                                0 = Everything goes right
  #                                             0 <  = Some warnings happen, but
  #                                                    the function finish right
  #                                             0 >  = Some error happen, and
  #                                                    the function halted. The
  #                                                    rest of the return vector
  #                                                    is probably nonsense or
  #                                                    invalid.
  
    # ---------------------------------
    #     UNIQUE VARIABLE ANALYSIS
    # ---------------------------------
    {}
    
    # ---------------------------------
    #     CATEGORICAL ANALYSIS
    # ---------------------------------
    {
      
    
        

      
    }
    
    # -----------------------------------------------
    #     NUMERICAL ANALYSIS
    # -----------------------------------------------
    {
      

      
      # -----------------------------------------------
      #     DENSITY
      # -----------------------------------------------
      {
        
        doDensityPlot <- function(tableBase, variableIndex, plotFilePath,
                                  colorsVector = NULL,
                                  borderPlot = FALSE, rotatePlot = FALSE,
                                  plotTitle = NULL, plotSubtitle = NULL,
                                  plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                  plotTheme = NULL,
                                  overrideTableName = NULL,
                                  overrideCaption   = NULL){
          
          # Init variables
          {
            
            # Plot type
            myPlotType = "Density"
            
            # Table name
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
            # Variable name
            numericalName = colnames(tableBase)[variableIndex]
            
          }
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
            
            myFileExtension = getFileExtension(plotFilePath)
            
            if(myFileExtension == ".png") imageFilePath = plotFilePath
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = variableIndex)
            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Get info about the variable
          numericalName = colnames(tableBase)[variableIndex]
          
          # Prepare the defaults
          {
            
            defaultVector = getNumericalDefaults(numericalName, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, fileType = myPlotType)
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
            
          }
          
          # Do the plot
          densityPlot = ggplot(data = tableBase, aes(x = tableBase[,variableIndex], fill = colorsVector)) +
            
            # Add the density plot
            geom_density(alpha=0.8) +
            scale_fill_manual(values=colorsVector) +
            
            # Create titles and subtitles
            labs(title           = plotTitle,
                 subtitle        = plotSubtitle,
                 caption         = plotCaption,
                 x = plotXLabel, y = plotYLabel) +
            
            theme(legend.position="none") +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]])
          
          # Something about the border
          if(borderPlot == TRUE)
            densityPlot = densityPlot + border()
          
          # Rotate the plot if asked
          if(rotatePlot == TRUE )
            densityPlot = densityPlot + coord_flip()
          
          
          # Save the image
          imageWidth    = 8
          ggsave(imageFilePath, plot = densityPlot, width = imageWidth)  
          latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
          
          
          myReturn = vector("list", length = 3)
          myReturn[[1]] = densityPlot
          myReturn[[2]] = imageFilePath
          myReturn[[3]] = latexFilePath
          
          return(myReturn)
          
        }
        
        
      }
      
      # -----------------------------------------------
      #     QQ
      # -----------------------------------------------
      {
        
        doQQPlot <- function(tableBase, variableIndex, plotFilePath,
                             colorsVector = NULL,
                             plotTitle = NULL, plotSubtitle = NULL,
                             plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                             plotTheme = NULL,
                             overrideTableName = NULL,
                             overrideCaption   = NULL){
          
          # Init variables
          {
            
            # Plot type
            myPlotType = "QQ"
            
            # Table name
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
            # Variable name
            numericalName = colnames(tableBase)[variableIndex]
          }
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
            
            myFileExtension = getFileExtension(plotFilePath)
            
            if(myFileExtension == ".png") imageFilePath = plotFilePath
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = variableIndex)
            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Prepare the defaults
          {
            
            defaultVector = getNumericalDefaults(numericalName, colorsVector, plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel, fileType = myPlotType)
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
            
          }
          
          # Run the normality test
          # (Shapiro-Wilk’s test., p-value > 0.05 => NORMALITY)
          # First we need to check if all the values are the same
          # In that case is not normally distributed
          normalValue   = 0
          areAllEqual   = sd(tableBase[,variableIndex], na.rm = TRUE)
          if(areAllEqual != 0)
            normalValue   = shapiro.test(tableBase[,variableIndex])$p.value
          
          plotLabelNormalValue = round(normalValue,2)
          
          # Do the plot
          myPlot = ggplot(tableBase, aes(sample = tableBase[,variableIndex] )) +
            
            # Add the qq related things  
            geom_qq_band(bandType = "pointwise", alpha = 0.3) +
            stat_qq_line(color="blue") +
            stat_qq_point() +
            
            # Add the normality text
            geom_text(aes(x=-Inf,y=Inf,hjust=0,vjust=1, label=plotLabelNormalValue), color="red", parse = TRUE) +
            
            # Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 color    = numericalName,
                 x = plotXLabel, y = plotYLabel) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]])
          
          # ---- Save the image
          imageWidth = 8
          ggsave(imageFilePath, plot = myPlot, width = imageWidth)  
          latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
          
          # ---- Return
          myReturn = vector("list", length = 3)
          myReturn[[1]] = myPlot
          myReturn[[2]] = imageFilePath
          myReturn[[3]] = latexFilePath
          
          return(myReturn)
          
        }
        
      }
      
      # -----------------------------------------------
      #     BOXPLOT
      # -----------------------------------------------
      {
        doBoxPlot <- function(tableBase, variableIndex, plotFilePath,
                              outlierShape = 19,
                              colorsVector = NULL, showPValues = TRUE,
                              ymin = NULL, ymax = NULL,
                              plotTitle = NULL, plotSubtitle = NULL,
                              plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                              plotTheme = NULL,
                              overrideTableName = NULL,
                              overrideCaption   = NULL){
          
          # Define plot type
          myPlotType  = "Boxplot"
          myTableName = deparse(substitute(tableBase))
          
          # If you need to override your table name, then do it
          if(!is.null(overrideTableName)) myTableName = overrideTableName
          
          # Get an automatic imagePath name if you don't have a proper one
          {
            
            imageFilePath = ""
            
            myFileExtension = getFileExtension(plotFilePath)
            
            if(myFileExtension == ".png") imageFilePath = plotFilePath
            
            else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                                   tableName = myTableName, fileType = myPlotType,
                                                   variableIndex1 = variableIndex)
            
          }
          
          # Get the theme information
          themeData = getThemeParameters(plotTheme)
          
          # Prepare the Y axis limits
          if(is.null(ymin)) ymin = min(tableBase[,variableIndex], na.rm= TRUE)
          if(is.null(ymax)) ymax = max(tableBase[,variableIndex], na.rm= TRUE)
          
          # Get info about different categories (there are none)
          groupingName = ""
          
          # Get info about the variable that is to be plotted
          numericalName = colnames(tableBase)[variableIndex]          
          
          # Count the number of characters in the title
          maximumTitlechars = nchar(plotTitle)
          
          # Set the imageWidth
          imageWidth = maximumTitlechars * 0.1
          
          # Init the plot object
          myBoxPlot               = NA
          
          # Init the centralities
          centralitiesDF           =  data.frame(matrix(NA, nrow = 1, ncol = 3))
          colnames(centralitiesDF) = c("ID", "Mean", "Median")
          centralitiesDF[,1]       = numericalName
          centralitiesDF[,2]       = mean(   tableBase[,variableIndex], na.rm = TRUE)
          centralitiesDF[,3]       = median( tableBase[,variableIndex], na.rm = TRUE)
          
          # Finally, do the actual plot
          # ---- Prepare the defaults
          {
            
            defaultVector = getNumericalDefaults(numericalName,
                                                 colorsVector = colorsVector, plotTitle   = plotTitle,
                                                 plotSubtitle = plotSubtitle, plotCaption = plotCaption,
                                                 plotXLabel   = plotXLabel,   plotYLabel  = plotYLabel)
            
            colorsVector  = defaultVector[[1]]
            plotTitle     = defaultVector[[2]][1]
            plotSubtitle  = defaultVector[[3]][1]
            plotCaption   = defaultVector[[4]][1]
            plotXLabel    = defaultVector[[5]][1]
            plotYLabel    = defaultVector[[6]][1]
          }
          
          # Do the plot
          myTempTableBase = tableBase
          myTempTableBase$Unique = numericalName
          colnames(myTempTableBase)[ncol(myTempTableBase)] = numericalName
          
          
          myBoxPlot = ggplot(data = tableBase, aes(x = myTempTableBase[,ncol(myTempTableBase)], y = myTempTableBase[,variableIndex], fill = myTempTableBase[,ncol(myTempTableBase)])) +
            
            # ---- Boxplot
            geom_boxplot(outlier.shape = outlierShape) +
            scale_fill_manual(values=colorsVector) +
            
            # ---- With tiny points for the samples
            geom_jitter(position = position_jitterdodge(), color="black", size=0.2, alpha=0.1) +
            
            # ---- Apply the Y limits
            scale_y_continuous(limits=c(ymin, ymax)) +
            
            # ---- Create titles and subtitles
            labs(title    = plotTitle,
                 subtitle = plotSubtitle,
                 caption  = plotCaption,
                 fill     = groupingName,
                 x = plotXLabel, y = plotYLabel) +
            
            # Apply the theme
            theme(panel.background   = themeData[[1]],
                  axis.line          = themeData[[2]],
                  panel.grid.major.y = themeData[[3]],
                  panel.grid.major.x = themeData[[4]],
                  legend.position    = themeData[[5]]) +
            
            # Override the theme and delete the name for the made up categorical
            theme(axis.text.x = element_blank())
          
          # Save everything
          ggsave(imageFilePath, plot = myBoxPlot, width = imageWidth)
          #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
          
          # Prepare the return vector
          myReturn = vector("list", length = 3)
          myReturn[[1]] = myBoxPlot
          myReturn[[2]] = centralitiesDF
          myReturn[[3]] = imageFilePath
          #myReturn[[4]] = latexFilePath
          
          return (myReturn)
          
        }
        
      }
      
      
      
      
    }
    
  
  
    # ---------------------------------
    #     MULTIPLE VARIABLE ANALYSIS
    # ---------------------------------
    {
        
        # ---------------------------------
        #     TWO CATEGORICAL ANALYSIS
        # ---------------------------------
        {
         
         
            
               
        }
     
        # ---------------------------------
        #          DENSITY PLOTS
        # (several categories, each one desity)
        # ---------------------------------
        {
            doCategoricalDensityPlot <- function(tableBase, variableIndex, categoryIndex, plotFilePath = NULL,
                                                 plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                                 colorsVector = NULL, plotTheme = NULL,
                                                 borderPlot = FALSE, rotatePlot = FALSE,
                                                 overrideTableName = NULL,
                                                 overrideCaption = NULL,
                                                 imageWidth = 8, imageHeight = 8){
    
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
                ggsave(imageFilePath, plot = densityPlot, width = imageWidth, height = imageWidth)
                #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
            
                # Return
                myReturn = vector("list", length = 3)
                myReturn[[1]] = densityPlot
                myReturn[[2]] = imageFilePath
                #myReturn[[3]] = latexFilePath
            
                return (myReturn)
            
          }  
      
        }
        
        # ---------------------------------
        #          BOX PLOTS
        # ---------------------------------
        {
            
        
        
        # Boxplot with optional p-value under it
        #
        # - variableIndex is the column index with the numerical values
        # - groupIndex    is the column index with the categorical values
        #
        # groupIndex can be NULL. Then, you won have any grouping of course and
        #                         you just have a boxplot of an univariable case
        doCategoricalBoxPlot <- function(tableBase, groupIndex, variableIndex, plotFilePath,
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
            myPlotType  = "CategoricalBoxplot"
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
                                                       variableIndex1 = groupIndex, variableIndex2 = variableIndex)

            }

            # Get the theme information
            themeData = getThemeParameters(plotTheme)

            # Prepare the Y axis limits
            if(is.null(ymin)) ymin = min(tableBase[,variableIndex], na.rm= TRUE)
            if(is.null(ymax)) ymax = max(tableBase[,variableIndex], na.rm= TRUE)

            # This is going to be the return object, might be empty though
            # "2" signify a p-value that is impossible. Thus marking that value was impossible to find out
            # This might happen because we don't have more than 2 categories, all numbers are NA, all numbers are the same, and so on
            pValuesDF     = NULL

            # Get info about different categories
        
            # ---- Init variables FOR THE NO CATEGORIES - NUMERICAL CASE:
            nCategories   = 1
            groupingName  = NA
            numericalName = colnames(tableBase)[variableIndex]
            myCategories  = c(numericalName)
        
            # ---- For the CATEGORICAL - NUMERICAL CASE:
            if(!is.null(groupIndex)){
          
                myCategories  = unique(tableBase[,groupIndex])
                nCategories   = length(myCategories)
                groupingName  = colnames(tableBase)[groupIndex]
          
                # Gives the X label to grouping name if it exist
                if((str_trim(plotXLabel) != "")&&(!is.null(plotXLabel))){
                    groupingName  = plotXLabel
                }
          
                numericalName = colnames(tableBase)[variableIndex]          
          
            }
            # ---- FOR THE NO CATEGORIES - NUMERICAL CASE:
            #      We still need a column with the unique variable
            else{
         
                newGroupIndex = ncol(tableBase) + 1
                tableBase[,newGroupIndex] = numericalName
                groupIndex = newGroupIndex
           
            }

            # Count the number of characters in the categories in order to decide the image width factor
            maximumCategoryChars = max(nchar(as.character(myCategories)))
            maximumCategoryChars = max(maximumCategoryChars, 8) # Tiny names looks weird, let take 8 as minimum char size

            # Count the number of characters in the title
            maximumTitlechars    = nchar(plotTitle)
        
            # Init the plot object
            myBoxPlot            = NA

            # Init the pValues, even though it could be impossible to do them
            pValuesDF            = DF(nCategories, nCategories + 1)
            colnames(pValuesDF)  = c("ID", as.character(myCategories))
            pValuesDF[,1]        = myCategories

            # Init the centralities, even though it could be impossible to do them
            centralitiesDF           = DF(nCategories, 3)
            colnames(centralitiesDF) = c("ID", "Mean", "Median")
            centralitiesDF[,1]       = myCategories
     
            # Create as many subsets as there are categories, and put them into this list
            subsetsDF     = rep(list(data.frame(NULL)), nCategories)
            for(i in 1:nCategories){
            
                subsetsDF[[i]] = subset(tableBase, tableBase[,groupIndex] == as.character(myCategories[i]))
            
            }
        
            # For each category (start at 1 because we want to find all centralities)
            for(i in 1:nCategories){
          
                samplesI      = subsetsDF[[i]][,variableIndex]
          
                centralitiesDF[i,2] = mean(samplesI,   na.rm = TRUE)
                centralitiesDF[i,3] = median(samplesI, na.rm = TRUE)
          
            }

            # Check out if we have enough categories to do p-values
            # If we have more than two categories, do something, otherwise
            # print a warning about impossible to find p-values
            if(nCategories >= 2){
              
                tTestString = paste(" T-Test performed under these conditions:",
                                    "     + Assumed unequal variance (Welsh df)",
                                    "     + Two Sided",
                                    "     + Mu = 0",
                                    "",
                                    " - Whas your population sampled randomly properly?",
                                    "     If not try Resampling: http://finzi.psych.upenn.edu/R/library/coin/doc/coin.pdf",
                                    " - Whas your data non normally distributed?",
                                    "     For non-parametric test try Mann-Whitney U, Wilcoxon Signed Rank, Kruskal Wallis, and Friedman tests.",
                                    " - Are you doing a lot of T-Test at the same time?",
                                    "     Consider doing an ANOVA instead, and make sure you run a post-hoc if you don't.",
                                    sep="\n")
              
                # Find the p-values
                # ---- We have nCategories! pValues.
                # ---- We are going to put them into a triangular matrix of nCategories x nCategories.
                # ---- Also notice that the main diagonal means nothing here, as it doesn't make sense to find the p-value with yourself
                {
    
                    # ---- Now do all the possible combinations
                    # ---- Be aware that for many categories this is not advice, you might want to do an ANOVA instead
                    # ---- With some type of TukeySD analysis afterwards
                
                    # For each category (start at 2 because we don't do p-values with self)
                    for(i in 2:nCategories){
                      
                        # For each other category that we haven't done already
                        for(j in 1:(i-1)){
                        
                        # Count that you have enough data to do a T-Test
                        {
                          samplesI      = subsetsDF[[i]][,variableIndex]
                          samplesJ      = subsetsDF[[j]][,variableIndex]
                          samplesI      = samplesI[!is.na(samplesI)]
                          samplesJ      = samplesJ[!is.na(samplesJ)]
                          totalSamplesI = length(samplesI)
                          totalSamplesJ = length(samplesJ)
                          sdSamplesI    = sd(samplesI)
                          sdSamplesJ    = sd(samplesJ)
                        }
                        
                        # If you do...
                        if( (totalSamplesI >= 2) && (totalSamplesJ >= 2) ){
                          
                          # Count that not everything is the same
                          sdSamplesI    = sd(samplesI)
                          sdSamplesJ    = sd(samplesJ)
                          
                          # If you do, then you can run a T-Test
                          if((sdSamplesI > 0) && (sdSamplesJ > 0) ){
                            
                            myPValue = t.test(subsetsDF[[i]][,variableIndex], subsetsDF[[j]][,variableIndex])$p.value
                            pValuesDF[i,j+1] = myPValue # +1 because the first column is the category ID
                            
                          }
         
                        }
                      }
                    }
    
                }
              
                # Find the averages and medians
              
              
            }
            
            else{
              
                print( "WARNING!! Can't generate p-values")
                print( "          Only one categorie was found")
                print( "          To make a p-value I need at least two categories with two different values in each")
                print( "          This was the attemp: ")
                print( paste("          Table:              ", myTableName,                                                  sep=''))
                print( paste("          Category variable:  ", groupingName , " only found ", as.character(myCategories[1]), sep='' ))
                print( paste("          Numerical variable: ", numericalName,                                                sep=''))
              
                showPValues = FALSE
              
            }

            # Finally, do the actual plot
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

            # Prepare the dataframe with the horizontal lines
            horizontalDF = NA
        
            if(showPValues == TRUE){
          
                # Init the p-values labels and lines DF
                {
                    totalRows              = choose(nCategories,2)
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
                for(i in 2:nCategories){
            
                    # For each other category
                    for(j in 1:(i-1)){

                      roundedPValue = signif(pValuesDF[i,j+1],3)
                      
                      horizontalDF$StartX[currentLine] = i
                      horizontalDF$EndX[currentLine]   = j
                      
                      horizontalDF$StartY[currentLine] = firstStep - ( currentLine * constantSpace )
                      horizontalDF$pvalue[currentLine] = roundedPValue
                      
                      currentLine = currentLine + 1
              
                    }
            
                }

                # If we are going to write the p-values, we need to lower the ymin limit where we draw the plot
                # Otherwise it won't show anything          
                ymin = min(horizontalDF$StartY) - constantSpace/3
          
            }

            # Change the yLimit if you want the scale to be forced to zero
            if(overrideScaleToZero == TRUE) ymin = 0

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

  
        # Box plot for a categorical variable, grouped by several concept.
        # For tables such as this:
        # 
        # | Value | Sex | Concept  |
        #    3      M      Blood A
        #    2      M      Blood 0
        #    6      F      Blood A
        #          ...
        #
        # Which will give you a plot such as this one:
        #
        #                    |
        #       |  |         |  |
        #       |  |         | []
        #      [] []         | []
        #      [] |         [] []         
        #      |            |  |
        #
        #       M F          M F          M F           M F
        #
        #     Blood 0      Blood A      Blood B      Blood AB
        doBiCategoricalBoxPlot <- function(tableBase, groupIndex, variableIndex, fillIndex, plotFilePath,
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
                                           overrideCaption   = NULL,
                                           longPlot = FALSE){

            # Define plot type
            myPlotType  = "DoubleCategoricalBoxplot"
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
                                                       variableIndex1 = groupIndex, variableIndex2 = variableIndex, variableIndex3 = fillIndex)

            }

            # Get the theme information
            themeData = getThemeParameters(plotTheme)

            # Prepare the Y axis limits
            if(is.null(ymin)) ymin = min(tableBase[,variableIndex], na.rm= TRUE)
            if(is.null(ymax)) ymax = max(tableBase[,variableIndex], na.rm= TRUE)

            # This is going to be the return object, might be empty though
            # "2" signify a p-value that is impossible. Thus marking that value was impossible to find out
            # This might happen because we don't have more than 2 categories, all numbers are NA, all numbers are the same, and so on
            pValuesDF     = NULL

            # Get info about different categories
            nGroupCategories  = 0
            nFillCategories   = 0
            myGroupCategories = ""
            myFillCategories  = ""
            groupingName      = colnames(tableBase)[groupIndex]
            fillingName       = colnames(tableBase)[fillIndex]
            numericalName     = colnames(tableBase)[variableIndex]
            
            # We are not going to check whether is numerical or not. Assume that everything is correct
        
            # Check whether we have levels or not
            # -- For the group by variable
            if(!is.null( levels(tableBase[,groupIndex]))){
             
                myGroupCategories = levels(tableBase[,groupIndex])
                nGroupCategories  = length(myGroupCategories)
                   
            }
            else{
                
                myGroupCategories = unique(tableBase[,groupIndex])
                nGroupCategories  = length(myGroupCategories)
                
            }
            # -- For the filling variable
            if(!is.null( levels(tableBase[,fillIndex]))){
             
                myFillCategories = levels(tableBase[,fillIndex])
                nFillCategories  = length(myFillCategories)
                   
            }
            else{
                
                myFillCategories = unique(tableBase[,fillIndex])
                nFillCategories  = length(myFillCategories)
                
            }

            # Count the number of characters in the categories in order to decide the image width factor
            maximumCategoryChars = max(nchar(as.character(myGroupCategories)))
            maximumCategoryChars = max(maximumCategoryChars, 8) # Tiny names looks weird, let take 8 as minimum char size

            # Count the number of characters in the title
            maximumTitlechars    = nchar(plotTitle)
        
            # Init the plot object
            myBoxPlot            = NA

            # Prepare the pValues dataframe
            # For this type of plot we are only going to make p-values labels if the fill by variable only have 2 categories
            pValuesDF           = DF(nGroupCategories, 2, defaultValue = "")
            colnames(pValuesDF) = c("Group", "Value")
            pValuesDF[,1]       = myGroupCategories
            
            # Prepare the averages dataframe, we are going to find the average only here
            averagesDF           = DF(nGroupCategories, (nFillCategories + 1), defaultValue = NA)
            colnames(averagesDF) = c("Group", myFillCategories)
            averagesDF[,1]       = myGroupCategories
            
            # Fill the p-values and avarages
            # For each group
            for(i in 1:nGroupCategories){
            
                # Get current Group
                currentGroup = as.character(myGroupCategories[i])
                
                # Subset by that group, we will find the p-values with this later
                subsetGroup  = subset(tableBase, tableBase[,groupIndex] == currentGroup)
                
                subsetA      = NA
                subsetB      = NA
                

                
                # For each fill (typically 2, but coded for several)
                # Find the avarages
                for(j in 1:nFillCategories){
                    
                    # Get which subgroup we are analyzing
                    currentFill  = as.character(myFillCategories[j])
                    
                    # Find the subset for this particular combination
                    subsetFill = subset(subsetGroup, subsetGroup[,fillIndex] == currentFill)
                    
                    # Count how many numbers that are not NA we have left, if more than 0, then find the average
                    if(sum(!is.na(subsetFill[,variableIndex])) > 0) averagesDF[i,(j+1)] = mean( subsetFill[,variableIndex] , na.rm = TRUE)
                    
                    # Prepare the subsets for the p-values
                    if(j == 1) subsetA = subsetFill
                    if(j == 2) subsetB = subsetFill
                }

                # Find the p-values if possible
                # TODO: Add the sd = 0, and n>2 condition
                
                
                #print(subsetA[,variableIndex])
                
                #print(subsetB[,variableIndex])
                
                # Check preconditions
                totalARows = length(subsetA[,variableIndex])
                totalBRows = length(subsetB[,variableIndex])
                
                if(totalARows > 2 & totalBRows > 2){
                
                    SDA = sd(subsetA[,variableIndex], na.rm = TRUE)
                    SDB = sd(subsetB[,variableIndex], na.rm = TRUE)
                    
                    if(SDA != 0 & SDB !=0){
                
                        myPValue       = t.test(subsetA[,variableIndex], subsetB[,variableIndex])$p.value
                        pValuesDF[i,2] = myPValue         
                        
                    }

                }
                
            }
    

            # Finally, do the actual plot
            # ---- Prepare the defaults
            {

                defaultVector = getBiCategoricalNumericalDefaults(groupingName, myGroupCategories,
                                                                  fillingName, myFillCategories,
                                                                  numericalName,
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

            # Prepare the Pvalues labels
            # Prepare the dataframe with the horizontal lines??
            horizontalDF = NA
        
            if(FALSE){
          
                  # Init the p-values labels and lines DF
                  {
                    totalRows              = choose(nCategories,2)
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
                  for(i in 2:myGroupCategories){
            
                    # For each other category
                    for(j in 1:(i-1)){
              
              #roundedPValue = signif(pValuesMatrix[j,i],3)
              #horizontalDF$StartX[currentLine] = j
              #horizontalDF$EndX[currentLine]   = i

              roundedPValue = signif(pValuesDF[i,j+1],3)
              
              horizontalDF$StartX[currentLine] = i
              horizontalDF$EndX[currentLine]   = j
              
              horizontalDF$StartY[currentLine] = firstStep - ( currentLine * constantSpace )
              horizontalDF$pvalue[currentLine] = roundedPValue
              
              currentLine = currentLine + 1
              
            }
            
          }

                  # If we are going to write the p-values, we need to lower the ymin limit where we draw the plot
                  # Otherwise it won't show anything          
                  ymin = min(horizontalDF$StartY) - constantSpace/3
          
               }

            # Change the yLimit if you want the scale to be forced to zero
            if(overrideScaleToZero == TRUE) ymin = 0
            
            # Do the plot
            {
                myBoxPlot = ggplot()+
            
                # ---- Boxplot
                geom_boxplot(data = tableBase, aes(x = tableBase[,groupIndex], y = tableBase[,variableIndex], fill = tableBase[,fillIndex]), outlier.shape = outlierShape,
                    width = 0.6)  +
                scale_fill_manual(values=colorsVector) +
            
                    
                    
                    
                # ---- With tiny points for the samples
                geom_jitter(data = tableBase, aes(x = tableBase[,groupIndex], y = tableBase[,variableIndex], fill = tableBase[,fillIndex]), position = position_jitterdodge(), color="black", size=0.2, alpha=0.1) +
            
                # ---- Apply the Y limits
                scale_y_continuous(limits=c(ymin, ymax)) +
            
                # ---- Create titles and subtitles
                labs(title    = plotTitle,
                     subtitle = plotSubtitle,
                     caption  = plotCaption,
                     fill     = groupingName,
                     x = plotXLabel, y = plotYLabel)
                
            }

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
            showPValues = FALSE
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
                imageWidth  = maximumCategoryChars * 0.1 * nGroupCategories + maximumCategoryChars * 0.2 # The multiplying number is arbitrary based on what I think look best
                imageWidth  = max(imageWidth, maximumTitlechars * 0.1) # + Add long title correction to image size                
            
                imageHeight = 8
            
                # But override if the user want to
                if(!is.null(overrideImageWidth)){
                    imageWidth  = overrideImageWidth
                }
                if(!is.null(overrideImageHeight)){
                    imageHeight = overrideImageHeight
                }

                if(longPlot == TRUE){
                
                    myBoxPlot = myBoxPlot + coord_flip()
                        
                    tempVariable = imageWidth
                    imageWidth   = imageHeight
                    imageHeight  = tempVariable
                }
                
                ggsave(imageFilePath, plot = myBoxPlot, width = imageWidth, height = imageHeight, limitsize = FALSE)
                #latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
            
             }

             # Return
             myReturn = vector("list", length = 4)
             myReturn[[1]] = myBoxPlot
             myReturn[[2]] = pValuesDF
             myReturn[[3]] = averagesDF
             myReturn[[4]] = imageFilePath
        
             return (myReturn)

      }
        
   
        
        }

        # ---------------------------------
        #          HEATMAPS
        # ---------------------------------
        {
        
              doCategoricalHeatmap <- function(tableBase, categoryXIndex, categoryYIndex, plotFilePath,
                                   roundDecimal = 0, addMarginals = TRUE,
                                   plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                   plotTheme = "no-grid",
                                   overrideTableName = NULL,
                                   overrideCaption   = NULL,
                                   logFileDescriptor = NULL,
                                   supressWarnings = FALSE){
    
    # Define plot type
    myPlotType  = "CategoricalHeatmap"
    myTableName = deparse(substitute(tableBase))
    
    # If you need to override your table name, then do it
    if(!is.null(overrideTableName)){
      myTableName = overrideTableName
      
    }

    # Get an automatic imagePath name if you don't have a proper one
    {
      
      imageFilePath   = ""
      myFileExtension = getFileExtension(plotFilePath)
      
      if(myFileExtension == ".png") imageFilePath = plotFilePath
      
      else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = tableBase,
                                             tableName = myTableName, fileType = myPlotType,
                                             variableIndex1 = categoryXIndex,
                                             variableIndex2 = categoryYIndex)
      
    }
    
    # Get the theme information
    themeData = getThemeParameters(plotTheme)

    # Get info about the categories
    categoryAName    = colnames(tableBase)[categoryXIndex]
    categoryBName    = colnames(tableBase)[categoryYIndex]
        
    # Get info about the modalities of each category
    modalitiesXNames = levels(tableBase[,categoryXIndex])
    if(is.null(modalitiesXNames)) modalitiesXNames = sort(unique(tableBase[,categoryXIndex]))
    modalitiesYNames = levels(tableBase[,categoryYIndex])
    if(is.null(modalitiesYNames)) modalitiesYNames = sort(unique(tableBase[,categoryYIndex]))
    totalXModalities = length(modalitiesXNames)
    totalYModalities = length(modalitiesYNames)
    
    # Init a matrix with the total for each combination
    matrixCount = matrix(0, nrow = totalYModalities, ncol = totalXModalities)
    
    # Make a new dataframe so we can draw the heatmap
    # Category X / Category Y / Total / X coordinate / Y coordinate / Color of tile / Size for text
    totalRows              = totalXModalities * totalYModalities
    heatmapTable           = data.frame(matrix(NA, nrow = totalRows, ncol = 8))
    colnames(heatmapTable) = c( "CategoryX", "CategoryY", "Total" , "Xcoordinate" , "Ycoordinate" , "myColor" , "Size", "Label" )
    
    
    # Fill the dataframe
    for(i in 1:totalXModalities){
      
      for(j in 1:totalYModalities){
        
        # Get the current line and categories
        currentLine = (i-1)*totalYModalities + j
        
        currentXModality = modalitiesXNames[i]
        currentYModality = modalitiesYNames[j]
        
        # Init the row in the dataframe
        heatmapTable$CategoryX[currentLine] = currentXModality
        heatmapTable$CategoryY[currentLine] = currentYModality
        
        # Count how many we have
        heatmapTable$Total[currentLine]     = sum(tableBase[tableBase[,categoryXIndex] == currentXModality, categoryYIndex ] == currentYModality)
        
        # Set the proper X / Y coordinates
        heatmapTable$Xcoordinate[currentLine] = i
        heatmapTable$Ycoordinate[currentLine] = j
        
        # Set the manual color and size for the text (not in use)
        heatmapTable$myColor[currentLine] = "Red"
        heatmapTable$Size[currentLine]    = 1
        
        # Set the label
        heatmapTable$Label[currentLine] = round(heatmapTable$Total[currentLine],roundDecimal)
        
        
      }
      
    }
    
    # cols = brewer.pal(n = 5, name = "RdBu")
    # 
    # blueColors = brewer.pal(n = 3, name = "PuBu")
    # redColors  = brewer.pal(n = 3, name = "OrRd")
    # lastBlue   = blueColors[3]
    # lastRed    = redColors[3]
    # whiteColor = "#FFFFFF"
    # blackColor = "#000000"
    
    #joinColors = c(whiteColor, whiteColor, redColors, lastRed, blackColor, lastBlue, rev(blueColors), whiteColor, whiteColor)
    
    # joinColors = c(rev(redColors), whiteColor, whiteColor, whiteColor, blueColors)
    
    #print(joinColors)
    
    heatmapPlot = ggplot( heatmapTable, aes( x = Xcoordinate, y = Ycoordinate, fill = Total )) +

                          scale_fill_gradient2("Combination", limits = c(0, max(heatmapTable$Total)), 
                                               mid = "#132b43", high = "#54adf2") +
            
                          # The background rectangle
                          geom_tile(color = "black") +

      
                          # What is written inside the rectangle
                          geom_shadowtext( aes(label= Label), color = "white", fontface = "bold") +

                          # What is written in the X axys
                          geom_text(aes(label = CategoryX, y = 0.3), angle = 90, hjust = 1) +
                          # What is written in the Y axys
                          geom_text(aes(label = CategoryY, x = 0.3), hjust = 1) +
      
                          # Give an extra space so we can see the labels clearly  
                          coord_cartesian( xlim = c(-1, totalXModalities + 0.5),
                                           ylim = c(-1, totalYModalities + 0.5)) +

                          # Create titles and subtitles
                          labs(title    = plotTitle,
                               subtitle = plotSubtitle,
                               caption  = plotCaption,
                               fill     = "Total",
                               x = plotXLabel, y = plotYLabel) +
      
                          # Apply the theme
                          theme(panel.background   = themeData[[1]],
                                axis.line          = themeData[[2]],
                                panel.grid.major.y = themeData[[3]],
                                panel.grid.major.x = themeData[[4]],
                                legend.position    = themeData[[5]],
                                axis.ticks         = themeData[[6]],
                                axis.text.x        = themeData[[7]],
                                axis.text.y        = themeData[[8]])

    if(addMarginals==TRUE){

      # Get the summaries for each categorical variable      
      XSummary = summarizeCategorical(tableBase, categoryXIndex, sorted="none")
      YSummary = summarizeCategorical(tableBase, categoryYIndex, sorted="none")

      # Prepare the dataframe with the marginal total
      totalRows              = totalXModalities + totalYModalities
      marginalTable          = data.frame(matrix(NA, nrow = totalRows, ncol = 8))
      colnames(marginalTable) = c( "CategoryX", "CategoryY", "Total" , "Xcoordinate" , "Ycoordinate" , "myColor" , "Size", "Label" )
      
      
      # Fill the dataframe
      for(i in 1:totalXModalities){

        # Get the current line and categories
        currentLine = i
        currentXModality = modalitiesXNames[i]

        # Init the row in the dataframe
        marginalTable$CategoryX[currentLine] = currentXModality
        marginalTable$CategoryY[currentLine] = ""
        
        # Count how many we have
        marginalTable$Total[currentLine]     = XSummary$Count[i]
        
        # Set the proper X / Y coordinates
        marginalTable$Xcoordinate[currentLine] = i
        marginalTable$Ycoordinate[currentLine] = totalYModalities + 1
        
        # Set the label
        marginalTable$Label[currentLine] = round(marginalTable$Total[currentLine],roundDecimal)
        
      }
        
      for(i in 1:totalYModalities){
          
        # Get the current line and categories
        currentLine = i + totalXModalities
        currentYModality = modalitiesXNames[i]
          
        # Init the row in the dataframe
        marginalTable$CategoryX[currentLine] = ""
        marginalTable$CategoryY[currentLine] = currentYModality
          
        # Count how many we have
        marginalTable$Total[currentLine]     = YSummary$Count[i]
          
        # Set the proper X / Y coordinates
        marginalTable$Xcoordinate[currentLine] = totalXModalities + 1
        marginalTable$Ycoordinate[currentLine] = i
          
        # Set the label
        marginalTable$Label[currentLine] = round(marginalTable$Total[currentLine],roundDecimal)
          
      }
        
      #print(marginalTable)
      
      
      # Add the marginal tiles
      heatmapPlot = heatmapPlot +
        
                    new_scale_fill() +
                    # The background rectangle
                    geom_tile(data = marginalTable, aes( x = Xcoordinate, y = Ycoordinate, fill = Total), color = "black") +
                    # What is written inside the rectangle
                    geom_shadowtext(data = marginalTable, aes(label= Label), color = "white", fontface = "bold") +
                    # Add a different gradient
                    scale_fill_gradient2("Marginal", 
                                         low = "black", high = "red") +
        
                    # Give an extra space so we can see the labels clearly  
                    coord_cartesian( xlim = c(-1, totalXModalities + 1.5),
                                     ylim = c(-1, totalYModalities + 1.5))
      
      
       

    }
    
    # Save the image and the txt files with the data
    # ---- Get the dimensions
    imageWidth = totalXModalities * 1.4
    if(addMarginals == TRUE) imageWidth = imageWidth   + 1
    imageHeight = totalYModalities * 1.4
    if(addMarginals == TRUE) imageHeight = imageHeight + 1
    # ---- Keep the ratio
    imageWidth  = max(imageWidth, imageHeight)
    imageHeight = max(imageWidth, imageHeight)
    # ---- Correct the ratio, for some reasons looks better this way
    imageWidth  = imageWidth * 1.1
    
    ggsave(imageFilePath, plot = heatmapPlot, width = imageWidth, height=imageHeight, scale=1.2)  
    latexFilePath = writeImageLATEX(imageFilePath, captionText = overrideCaption)
    
    myReturn = vector("list", length = 3)
    myReturn[[1]] = heatmapPlot
    myReturn[[2]] = imageFilePath
    myReturn[[3]] = latexFilePath
    
    return (myReturn)
    
  }
                
        }
        
        
        # ---------------------------------
        #          SPECIALS
        #
        #          -- BMI density with BMI values pre-filled
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
    
        
        # ---------------------------------------------------------
        #     TWO NUMERICALS VARIABLES
        # ---------------------------------------------------------
        {
            
            
            # ---------------------------------------------------------
            #     Linear regressions
            # ---------------------------------------------------------
            {
                
                # Get two numerical columns and make the regression between then
                doSimpleRegression <- function(tableBase, 
                                               independentColumnIndex, dependentColumnIndex, 
                                               plotFilePath,
                                               model = "Cuadratic",
                                               colorsVector = NULL, 
                                               showRegressionLine = TRUE,
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
                                                               variableIndex1 = independentColumnIndex, 
                                                               variableIndex2 = dependentColumnIndex)
                        
                    }
                    
                    # Get the theme information
                    themeData = getThemeParameters(plotTheme)
                    
                    # Get info about the variable
                    numericalNameA = colnames(tableBase)[independentColumnIndex]
                    numericalNameB = colnames(tableBase)[dependentColumnIndex]
                    imageWidth     = 3
                    
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
                    
                    # Init basic variables
                    returnR2 = 0
                    myModel  = 0
                    
                    # Find how many samples we have
                    totalSampleIndependent = sum(!is.na(tableBase[,independentColumnIndex]))
                    totalSampleDependent   = sum(!is.na(tableBase[,dependentColumnIndex]))
                    
                    # If we have enought do the plot, otherwise, skip the whole process
                    if( totalSampleIndependent >= 2 && totalSampleDependent >= 2){
                        
                        # Check that the values match in the same position of the vector
                        tableComplete = sum((!is.na(tableBase[,independentColumnIndex])) & (!is.na(tableBase[,dependentColumnIndex])))
                        
                        if(tableComplete >=2 ){
                            
                            doPlots = TRUE
                            
                        }
                        else{
                            doPlots = FALSE
                            
                            if(supressWarnings != FALSE){
                                
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
                    else{
                        
                        doPlots = FALSE
                        
                        if(supressWarnings != FALSE){
                         
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
                    
                    # If we have enought points...
                    if(doPlots){
                        
                        # Delete the rows where are NAs, we have already check that this doesn't let you with empty sets
                        naDependent   = is.na(tableBase[,dependentColumnIndex])
                        naIndependent = is.na(tableBase[,independentColumnIndex])
                        naRows        = naDependent | naIndependent
                        tableBase     = tableBase[!naRows,]
                        
                        # Make the model
                        # -- "Cuadratic" (default)
                        myModel = 0
                        if(model == "Cuadratic"){
                            # Make the X² + X + C model
                            myModel = lm(tableBase[,dependentColumnIndex] ~ tableBase[,independentColumnIndex] + I(tableBase[,independentColumnIndex]^2), tableBase)
                        }
                        if(model == "Inverse"){
                            myModel = lm(1/tableBase[,dependentColumnIndex] ~ tableBase[,independentColumnIndex], tableBase)
                        }
                        if(model == "Inverse Logarithmic"){
                            myModel = lm(1/tableBase[,dependentColumnIndex] ~ log(tableBase[,independentColumnIndex]), tableBase)
                        }            
                        
                        if(model == "Logarithmic"){
                            myModel = lm(formula = tableBase[,dependentColumnIndex] ~ log(tableBase[,independentColumnIndex]), tableBase)
                        }        
                        
                        # Get the R2 value
                        myR2value = summary(myModel)$r.squared
                        
                        # Get the P-value
                        myPvalue  = summary(myModel)$coefficients[2,4]
                        
                        # The returning values have all the digits
                        returnR2     = myR2value
                        returnPValue = myPvalue 
                         
                        # The value that we are going to print in the plot has only 2 decimals (ie R^2 = 0.94)
                        myR2value = format(myR2value, digits = 2)
                        r2String  = paste("R2: ", myR2value, sep = " ")
                        myPvalue  = format(myPvalue, digits = 4)
                        PString   = paste("p-v: ", myPvalue, sep = " ")
                        if(myPvalue == 0) PString = "p-v < 0.0001"
                        
                        # Do a scatter plot
                        #--------------------------------------------------------------------
                        myScatterPlot = ggplot(tableBase, aes(x=tableBase[,independentColumnIndex], y=tableBase[,dependentColumnIndex])) +
                            
                            # With hollow points
                            geom_point(shape=1)
                        
                        # -- Add the line with confident interval of 95%
                        if(model == "Cuadratic")
                            myScatterPlot = myScatterPlot + geom_smooth(method = "lm", formula = y ~ x + I(x^2))
                        if(model == "Inverse")    
                            myScatterPlot = myScatterPlot + geom_smooth(method = "lm", formula = y ~ 1/x)
                        if(model == "Inverse Logarithmic")    
                            myScatterPlot = myScatterPlot + geom_smooth(method = "glm", formula = y ~ x,
                                                                        method.args = list(family = gaussian(link = 'log')))
                        if(model == "Logarithmic")    
                            myScatterPlot = myScatterPlot + geom_smooth(method = "glm", formula = y ~ x,
                                                                        method.args = list(family = gaussian(link = 'log')))
                        
                        
                        # -- Add the R2 and P-value text
                        myScatterPlot = myScatterPlot +
                            geom_text(aes(x=-Inf,y=Inf,hjust=0,  vjust=1,   label= r2String), color="red") +
                            geom_text(aes(x=-Inf,y=Inf,hjust=0,  vjust=2.5, label= PString),  color="red") +      
                            
                        # -- Create titles and subtitles
                            labs(title    = plotTitle,
                                 subtitle = plotSubtitle,
                                 caption  = plotCaption,
                                 x = plotXLabel, y = plotYLabel)

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
                        ggsave(imageFilePath, plot = myScatterPlot,
                               width = imageWidth, height = imageHeight)
                        
                    }
                    
                    # Return
                    myReturn = vector("list", length = 5)
                    myReturn[[1]] = myScatterPlot
                    myReturn[[2]] = returnR2
                    myReturn[[3]] = returnPValue
                    myReturn[[4]] = myModel
                    myReturn[[5]] = imageFilePath

                    return (myReturn)
                    
                }
                
                
                
                
            }
            
            
            
        }
        
  
}


