      # -----------------------------------------------
      #     HISTOGRAMS
      # -----------------------------------------------
      {
        doHistogramPlot2 <- function(tableBase, variableIndex, plotFilePath,
                                     colorsVector = NULL,
                                     
                                     totalBins = NULL, binsWidth = NULL, oneTickPerBin = TRUE,
                                     
                                     writeValues    = TRUE,
                                     binFontSize    = NULL,
                                     binBorderColor = NULL,
                                     
                                     normalizeYMaximum = NULL, normalizeXMaximum = NULL,
                                     
                                     plotTitle = NULL, plotSubtitle = NULL,
                                     plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                                     plotTheme = NULL,
                                     
                                     titleFontSize = NULL,
                                     axysFontSize = NULL,
                                     
                                     generalFontSize = NULL,
                                     
                                     generalMargins = NULL,
                                     titleMargins = NULL,
                                     
        							 overrideImageWidth = NULL,
        							 overrideImageHeight = NULL,

                                     overrideTableName = NULL,
                                     overrideCaption   = NULL){
          
          # Init variables
          {
            
            # Plot type
            myPlotType = "Histogram"
            
            # Table name
            myTableName = deparse(substitute(tableBase))
            
            if(!is.null(overrideTableName)){
              myTableName = overrideTableName
              
            }
            
            # Variable name
            numericalName = colnames(tableBase)[variableIndex]
            
            # Bins variables
            {
              myTotalBins  = -1
              myBinsWidth  = -1
              
              minimumValue = min(as.integer(tableBase[,variableIndex]), na.rm = TRUE)
              maximumValue = max(as.integer(tableBase[,variableIndex]), na.rm = TRUE)
              deltaValue   = maximumValue - minimumValue
              
              upperValue   = max(maximumInteger(tableBase, variableIndex, variableIndex))
              lowerValue   = 0
              delta2Value  = upperValue - lowerValue 
            }
            
            
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
            
            # Correct for NULLs
            if(plotTitle    == "") plotTitle    = NULL
            if(plotSubtitle == "") plotSubtitle = NULL
            if(plotCaption  == "") plotCaption  = NULL
            if(plotXLabel   == "") plotXLabel   = NULL
            if(plotYLabel   == "") plotYLabel   = NULL
            
            # Set the proper border for the bin color
            if(is.null(binBorderColor)) binBorderColor = "grey"
            
            
          }
          
          # What to do with the number of bins, and wide of each bin
          {
            # ---- If neither of then are not initialize
            if(is.null(totalBins) && is.null(binsWidth)){
              
              myTotalBins  = 30
              myBinsWidth  = deltaValue/myTotalBins
              
            }
            
            # ---- If at least one is initialize
            else{
              
              # ---- If BOTH are init, this is a ERROR from the user
              if(!is.null(totalBins) && !is.null(binsWidth)){
                
                print("----")
                print("WARNING!: In the function doing the histogram")
                print("----")
                print("You have choosen to initialize both the number of bins, and how wid each bin is.")
                print("I must choose only one of the two given values.")
                print("I default to the number of bins.")
                print("Please correct this in your code so you don't see this message again.")
                print("----")
                
                myTotalBins  = 30
                myBinsWidth  = deltaValue/myTotalBins
                
              }
              
              else{
                
                # ---- If only total number of bins is init.
                if(!is.null(totalBins)){
                  
                  myTotalBins = totalBins
                  myBinsWidth = deltaValue/myTotalBins
                  
                }
                
                else{
                  
                  # ---- If only bin width is init.
                  
                  myBinsWidth  = binsWidth
                  myTotalBins  = ceiling(deltaValue/myBinsWidth) + 1
                  
                }
                
              }
              
            }
            
          }
          
          # Figure it out special numbers of the plot
          {
            
            # Where to put the breaks
            xBreaksEvery = myBinsWidth
            
            # The coordinates where to pain the plot
            expandXPercent = 0.1
            leftLimit     = minimumValue - (deltaValue * expandXPercent)
            rightLimit    = maximumValue + (deltaValue * expandXPercent)
            
            expandYPercent = 0.1
            upperLimit    = upperValue + (delta2Value * expandYPercent)
            lowerLimit    = lowerValue # - (delta2Value * expandYPercent)
            
            if(!is.null(normalizeXMaximum)) rightLimit    = normalizeXMaximum
            if(!is.null(normalizeYMaximum)) upperLimit    = normalizeYMaximum
            
          }
          
          # Do the plot
          {
            myPlot = ggplot(data=tableBase, aes(tableBase[,variableIndex])) +
              
              # Do the histogram
              geom_histogram(binwidth = binsWidth, fill=colorsVector[1], col=binBorderColor) +

              # Limit the plot to the maximum and minimum
              coord_cartesian(xlim=c(leftLimit,rightLimit), ylim=c(lowerLimit,upperLimit))
              
            # Check if you are a number or a date
            if(isDate(tableBase[,variableIndex])){
              myPlot = myPlot + scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                                             date_labels = "%B")
            }
            else{
              # Add each of the individual breaks
              myPlot = myPlot + scale_x_continuous(breaks = seq(minimumValue , maximumValue , xBreaksEvery))              
            }
            
            myPlot = myPlot + 

              # Create titles and subtitles
              labs(title    = plotTitle,
                   subtitle = plotSubtitle,
                   caption  = plotCaption,
                   x = plotXLabel, y = plotYLabel) +
              
              # Apply the theme
              theme(panel.background   = themeData[[1]],
                    axis.line          = themeData[[2]],
                    panel.grid.major.y = themeData[[3]],
                    panel.grid.major.x = themeData[[4]],
                    
              )
            
            # If you want to write the values on top of each bar (DEFAULT)
            if(writeValues == TRUE){
              
              # With black text on top of each column
              # If you want to specify font size
              if(is.null(binFontSize)){
                myPlot = myPlot + stat_bin(aes(y=..count.., label=..count..), binwidth = binsWidth, geom="text", vjust=-.5)  
              }
              else{
                myPlot = myPlot + stat_bin(aes(y=..count.., label=..count..), binwidth = binsWidth, geom="text", vjust=-.5, size = binFontSize)  
              }
              
            }
            
            # If you want to change the title font size
            if(!is.null(titleFontSize)){
              
              myPlot = myPlot + theme(plot.title = element_text(size = titleFontSize))  
              
            }
            
            # If you want to change the axys font size
            if(!is.null(axysFontSize)){
              
              myPlot = myPlot + theme(axis.text  = element_text(size = axysFontSize)  )  
              
            }
            
            # If you want to override ALL font sizes and declare a general font size
            if(!is.null(generalFontSize)){
              
              myPlot = myPlot + theme(text = element_text(size = generalFontSize))  
              
            }
            
            # If you want to give special margins
            if(!is.null(generalMargins)){
              
              myPlot = myPlot + theme(plot.margin = unit(generalMargins, "cm"))  
              
            }
            
            # If you want different margins around the title
            if(!is.null(titleMargins)){
              
              myPlot = myPlot + theme(plot.title       = element_text(margin = margin(titleMargins) ) )  
              myPlot = myPlot + theme(axis.title.y     = element_text(margin = margin(0,0,0,0)) )   
              myPlot = myPlot + theme(plot.background  = element_rect(fill  = "red", size = 0.1 ) )  
              myPlot = myPlot + theme(panel.background = element_rect(fill = "blue", size = 1 ) )  
              myPlot = myPlot + theme(plot.title.       = element_rect(fill = "yellow", size = 1 ) )  
              #myPlot = myPlot + theme(plot.margin      = margin(0.1, 0.2, 0.5, 2, "cm") )  
              
            }
            
          }
          
          
		  # Override the image dimensions
          imageWidth  = ceiling(myTotalBins/2) * 1.5
          imageHeight = 6
          if(!is.null(overrideImageWidth))  imageWidth  = overrideImageWidth
          if(!is.null(overrideImageHeight)) imageHeight = overrideImageHeight
          
          # ---- Save the image
          ggsave(imageFilePath,                             plot = myPlot, width = imageWidth, height = imageHeight)   
          ggsave(changeFileExtension(imageFilePath, "pdf"), plot = myPlot, width = imageWidth, height = imageHeight)  
          
          
          myReturn = vector("list", length = 2)
          myReturn[[1]] = myPlot
          myReturn[[2]] = imageFilePath
          
          return(myReturn)
          
        }
        
      }