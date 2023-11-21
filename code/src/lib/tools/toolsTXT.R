# ---- GENERIC FUNCTION
{
  # Write any plot into latex
  # All latex images are in the same folder, so you only need the name, but can also give a path
  writePlotImageDataLATEX <- function(imageFilePath){
    
    texFilePath = paste( getFilePathNoExtension(imageFilePath) , ".tex" , sep = "" )
    
    # Get the naming ID that comes automatically with the imageFilePath
    texFileNameNoExtension = getFileNameNoExtension(imageFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = "Generic plot, please fill caption manually"
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    }
    
    # Write into the file
    fileConn = file(texFilePath)
    writeLinesBN(paste(finalImageString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
  # Write into a file a DF in latex format
  writeGenericTableLATEX <- function(tableBase, texFilePath, transposeTable = FALSE, roundMe = Inf,
                                     tableCaption = "Table with a generic name",
                                     overrideTableName = NULL, warningComment = TRUE,
                                     intervalsNumbers = NULL, intervalsColors = NULL, intervalClose = "right",
                                     exactValues = NULL, exactColors = NULL){
    
    # Init
    myTableName = deparse(substitute(tableBase))
    
    # -- If you need to override your table name, then do it
    if(!is.null(overrideTableName)){
      myTableName = overrideTableName
      
    }
    
    # -- Remove the final compiling comment
    finalLatexComment = LATEX_COMPILE_TWICE_COMMENT
    if(warningComment == FALSE){
      
      finalLatexComment = "\n"
      
    }
    
    # Get an automatic name if you don't have a proper one
    {
      genericFilePath = automaticFilePath( texFilePath, tableBase, tableName=myTableName, plotType = "LatexTable")
      latexFilePath   = paste(genericFilePath,".tex",sep="")
    }
    
    # -------------------------------------
    # Build the superstring for the table
    # -------------------------------------
    {
      tableCaptionText = tableCaption
      finalTableString = getTableFromDF(tableBase, captionText = tableCaptionText, transposeTable = transposeTable, roundMe = roundMe,
                                        overrideTableName = overrideTableName,
                                        intervalsNumbers = intervalsNumbers, intervalsColors = intervalsColors, intervalClose = intervalClose,
                                        exactValues = exactValues, exactColors = exactColors)
    }
    
    # Write into the file
    fileConn = file(latexFilePath, 'w')
    writeLinesBN(paste(finalTableString, "\n", finalLatexComment ,sep=""), fileConn) # This close the connection TODO: take that out
    return(latexFilePath)
    
  }
  
}

# ---- BARPLOTS
{
  
  
  
  
  # Absolute Barplot
  writeAbsBarplotData      <- function(tableBase, countingIndex, txtFilePath){
    
    # Get info about different categories
    myCategories = unique(tableBase[,countingIndex])
    myCategories = myCategories[!is.na(myCategories)] # Remove the NA/NULL categories, we deal with them separetly later
    myCategories = as.character(myCategories)
    nCategories  = length(myCategories)
    groupingName = colnames(tableBase)[countingIndex]
    
    # Get the total of each category and numbers
    totalRows = nrow(tableBase)
    totalNA   = sum(is.na(tableBase[,countingIndex]))
    totalNULL = sum(is.null(tableBase[,countingIndex]))
    
    categoryCount = rep(0, nCategories)
    
    # Count each category individually
    for (i in 1:nCategories) {
      
      myCategory = myCategories[i]
      
      onlyThisCategory = subset(tableBase, tableBase[,countingIndex] == myCategory)
      
      categoryCount[i] = nrow(onlyThisCategory)
    }
    
    # Get the alignment spaces for the categories
    myAligments   = getAlignment(c("NA","NULL", myCategories))
    mySpaces      = rep(" ", nCategories + 2)
    
    # ---- For the NA and NULL categories
    mySpaces[1]   = paste(rep(" ",myAligments[1]), collapse="")
    mySpaces[2]   = paste(rep(" ",myAligments[2]), collapse="")
    
    # ---- For the rest of the categories
    for (i in 3:(nCategories+2)) {
      
      mySpaces[i]   = paste(rep(" ",myAligments[i]), collapse="")
    }
    
    # Prepare the text summary:
    intro         = paste("TXT data for the plot saved in\n\n", txtFilePath, "\n\n", sep="")
    absoluteTotal = paste("Total rows: ", totalRows, "\n\n",sep="")
    NATotal       = paste("     NA:",   mySpaces[1],  totalNA,   "\n",sep="")
    NULLTotal     = paste("     NULL:", mySpaces[2],  totalNULL, "\n",sep="")
    
    categoriesString = ""
    
    # ---- Count each category individually
    for (i in 1:nCategories) {
      
      if(!is.na(myCategories[i]) && !is.null(myCategories[i]) ){
        
        newTXT = paste("     ", myCategories[i], ":", mySpaces[i+2], categoryCount[i], "\n", sep="")
        categoriesString = paste(categoriesString, newTXT, sep="")
        
      }
      
    }
    # ---- Build the superstring
    finalString = paste(intro,
                        absoluteTotal,
                        NATotal,
                        NULLTotal,
                        categoriesString,
                        sep="")
    
    # Write into the file
    fileConn = file(txtFilePath)
    
    writeLines(finalString, fileConn)
    
    close(fileConn)
    
  }
  writeAbsBarplotDataLATEX <- function(tableBase, countingIndex, texFilePath, saveTable = TRUE){
    
    # Get the name of the variable
    groupingName = colnames(tableBase)[countingIndex]
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(texFilePath)
    
    
    # Build the superstring for the image
    imageCaptionText  = paste("Barplot with the absolute frequency for ", groupingName, sep='')
    
    imageCaptionText  = "Latex refuse to compile these captions, init to default."
    
    finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    
    # Build the superstring for the table
    tableCaptionText = paste("Table with the absolute and relative frequencies for ", groupingName, sep='')
    finalTableString = getTableAbsCountCategoricalLatexString(tableBase, countingIndex, texFileNameNoExtension, tableCaptionText)
    
    # Put everything together
    finalString = paste(finalImageString, "\n", sep="")
    if(saveTable == TRUE){
      finalString = paste(finalString, finalTableString, "\n", sep="")
    }
    
    # Write into the file
    fileConn = file(texFilePath)
    writeLinesBN(paste(finalString, LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
  # Relative Barplots
  writeBarRelativeCombinePlotDataLATEX <- function(tableBase, countingIndex, groupIndex, texFilePath){
    
    # Get the name of the variable
    countingName = colnames(tableBase)[countingIndex]
    groupingName = colnames(tableBase)[groupIndex]
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(texFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = paste("Relative barplot with the relative frequency for ", countingName," grouped by ", groupingName, sep='')
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    }
    
    # -------------------------------------
    # Build the superstring for the table
    # -------------------------------------
    {
      tableCaptionText = paste("Table with the relative frequency for ", countingName," grouped by ", groupingName, sep='')
      finalTableString = ""
    }
    
    
    # Write into the file
    fileConn = file(texFilePath)
    writeLinesBN(paste(finalImageString, "\n", finalTableString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
  
  # Graphs
  # ---- Write tables:
  # --------- All edges
  # --------- All nodes with selected data
  # ---- Write images
  # --------- One for each layout
  writeGraphplotLATEX <- function(edgesNames, edgeTable, nodesTable, highlightVariableIndex, rimVariableIndex, texFilePath){
    
    # Get the name of the variables
    highlightName = colnames(nodesTable)[highlightVariableIndex]
    rimName       = colnames(nodesTable)[rimVariableIndex]
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(texFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = paste("Graphplot for the network ", edgesNames, sep='')
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText, 
                                              pageWidth = 1, pageHeight = 1)
    }
    
    # Write into the file
    fileConn = file(texFilePath)
    writeLinesBN(paste(finalImageString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
    
  }
  
  # Identity Barplots
  
}

# ---- BOXPLOTS
{
  
  writeBoxPlotPValuesData      <- function(tableBase, groupIndex, variableIndex, pValuesMatrix, plotFilePath){
    
    # INIT VARIABLES
    # -------------------
    # Make the TXT file where we save everything
    dataFilePath = paste(plotFilePath, ".txt", sep="")
    dataFileConn = file(dataFilePath)
    
    # Get info about different categories
    myCategories  = unique(tableBase[,groupIndex])
    nCategories   = length(myCategories)
    groupingName  = colnames(tableBase)[groupIndex]
    numericalName = colnames(tableBase)[variableIndex]
    
    # Here is where the summary is stored
    # Init to "ERROR" in case the p-values thingy didn't work for whatever reason
    dataText = "Status: ERROR \n"
    
    # Check out if we have enought categories to do p-values
    if(nCategories < 2){
      
      print( "WARNING!! Can't create p-values!")
      print( "Only one categorie was found")
      print( "To make a p-value I need at least two categories with two different values in each")
      
    }
    else{
      
      # Everything when well, do the summary
      dataText = "Status: SUSCESS \n"
      
      dataText = paste(dataText, "  Total categories: ",  nCategories ," \n", sep="")
      
      # Create as many substets as there are categories, and put them into this list
      subsetsDF     = rep(list(data.frame(NULL)), nCategories)
      
      for(i in 1:nCategories){
        
        subsetsDF[[i]] = subset(tableBase, tableBase[,groupIndex] == as.character(myCategories[i]))
        
      }
      
      # Now we have all the info ready.
      # ---- Common column headers for each category
      myCols    = "   Min.  1st Qu.   Median     Mean 3rd Qu.      Max.     NA's"
      
      # ---- Write whatever you need into this variable
      dataText =
        dataText = paste(dataText, "Plot data summary \n",           sep="")
      dataText = paste(dataText, "  Summarized by categories: \n", sep="")
      
      # ---- For each category...
      for(i in 1:nCategories){
        
        dataText = paste(dataText , "      ", as.character(myCategories[i]),  "\n" , sep="")
        dataText = paste(dataText , "          Total: ", nrow(subsetsDF[[i]]), "\n" , sep="")
        dataText = paste(dataText , "          Summary: ", "\n" , sep="")
        
        # Create an automatic summary object that we dump into the TXT file
        summaryOb = summary(subsetsDF[[i]][,variableIndex])
        mySummary = toString(summaryOb)
        
        dataText = paste(dataText , "          ", myCols,    "\n" , sep="")
        dataText = paste(dataText , "          ", mySummary, "\n" , sep="")
        dataText = paste(dataText , "          p-values:\n"       , sep="")
        
        for(j in 1:nCategories){
          
          if(i!=j){
            
            if(i>j){  # The p value matrix is only init to be triangular, so this copy the triangle to the other side when writing the file
              dataText = paste(dataText , "             ", as.character(myCategories[j]), " : " , pValuesMatrix[i,j] ,  "\n"       , sep="")
            }
            else{
              dataText = paste(dataText , "             ", as.character(myCategories[j]), " : " , pValuesMatrix[j,i] ,  "\n"       , sep="")
            }
            
          }
          
        }
        
      }
      
      
      
    }
    
    # ---- Finally, write the data
    writeLines(dataText, dataFileConn)
    
    # Close the file
    close(dataFileConn)
  }
  writeBoxPlotPValuesDataLATEX <- function(tableBase, groupIndex, variableIndex, latexFilePath, overrideCaption = NULL){
    
    # Get the name of the variable
    variableName = colnames(tableBase)[variableIndex]
    groupingName = colnames(tableBase)[groupIndex]
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(latexFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = paste("Boxplot for ",variableName," divided by for ", groupingName, sep='')
      
      if(!is.null(overrideCaption)) imageCaptionText  = overrideCaption
      
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    }
    
    # -------------------------------------
    # Build the superstring for the table
    # -------------------------------------
    {
      tableCaptionText = paste("Table with the p-values each combination for ", groupingName, sep='')
      finalTableString  = ""
      #finalTableString = getTableAbsCountCategoricalLatexString(tableBase, countingIndex, texFileNameNoExtension, tableCaptionText)
    }
    
    # Write into the file
    fileConn = file(latexFilePath)
    writeLinesBN(paste(finalImageString, "\n", finalTableString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
}

# ---- PROBABILITY DISTRIBUTIONS
{
  # Histograms
  writeHistogramPlotDataLATEX<- function(tableBase, variableIndex, texFilePath){
    
    # Get the name of the variable
    groupingName = colnames(tableBase)[variableIndex]
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(texFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = paste("Histogram with the absolute frequency for ", groupingName, sep='')
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    }
    
    # -------------------------------------
    # Build the superstring for the table
    # -------------------------------------
    {
      tableCaptionText = paste("Table with the absolute and frequencies for ", groupingName, sep='')
      finalTableString  = ""
      #finalTableString = getTableAbsCountCategoricalLatexString(tableBase, countingIndex, texFileNameNoExtension, tableCaptionText)
    }
    
    # Write into the file
    fileConn = file(texFilePath)
    writeLinesBN(paste(finalImageString, "\n", finalTableString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
  # Density plots
  writeDensityPlotDataLATEX<- function(tableBase, variableIndex, texFilePath){
    
    # Get the name of the variable
    variableName = colnames(tableBase)[variableIndex]
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(texFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = paste("Density plot with the probability distribution of ", variableName, sep='')
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    }
    
    # -------------------------------------
    # Build the superstring for the table
    # -------------------------------------
    {
      tableCaptionText = paste("Table with the probability distribution of ", variableName, sep='')
      finalTableString  = ""
      #finalTableString = getTableAbsCountCategoricalLatexString(tableBase, countingIndex, texFileNameNoExtension, tableCaptionText)
    }
    
    # Write into the file
    fileConn = file(texFilePath)
    writeLinesBN(paste(finalImageString, "\n", finalTableString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
  # QQ plots
  writeQQPlotDataLATEX<- function(tableBase, variableIndex, texFilePath){
    
    # Get the name of the variable
    groupingName = colnames(tableBase)[variableIndex]
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(texFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = paste("QQ plot with a Shapiro test for normality of ", groupingName, sep='')
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    }
    
    # Write into the file
    fileConn = file(texFilePath)
    writeLinesBN(paste(finalImageString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
}

# ---- SPECIAL PLOTS
{
  writeBMIPlotDataLATEX <- function(tableBase, BMIindex, groupIndex, latexFilePath){
    
    # Get the name of the variable
    groupingName = colnames(tableBase)[groupIndex]
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(latexFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = paste("BMI divided by for ", groupingName, sep='')
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    }
    
    # -------------------------------------
    # Build the superstring for the table
    # -------------------------------------
    {
      tableCaptionText = paste("Table with the relative frequencies for each interval", groupingName, sep='')
      finalTableString  = ""
      #finalTableString = getTableAbsCountCategoricalLatexString(tableBase, countingIndex, texFileNameNoExtension, tableCaptionText)
    }
    
    # Write into the file
    fileConn = file(latexFilePath)
    writeLinesBN(paste(finalImageString, "\n", finalTableString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
  writeTablePlotDataLATEX <- function(tableName, latexFilePath){
    
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(latexFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = paste("Table plot for all variables in ", tableName, sep='')
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    }
    
    # Write into the file
    fileConn = file(latexFilePath)
    writeLinesBN(paste(finalImageString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
  # Simulation plots
  writeSimulationPlotDataLATEX <- function(tableBase, texFilePath){
    
    # Get the name of the variable
    #groupingName = colnames(tableBase)[countingIndex]
    
    # Get the naming ID that comes automatically with the texFilePath
    texFileNameNoExtension = getFileNameNoExtension(texFilePath)
    
    # -------------------------------------
    # Build the superstring for the image
    # -------------------------------------
    {
      imageCaptionText  = "Simulation plot "
      finalImageString  = getImageLatexString(texFileNameNoExtension, LATEX_IMAGE_LOCATION, imageCaptionText)
    }
    
    # -------------------------------------
    # Build the superstring for the table
    # -------------------------------------
    {
      # tableCaptionText = paste("Table with the absolute and relative frequencies for ", groupingName, sep='')
      # finalTableString = getTableAbsCountCategoricalLatexString(tableBase, countingIndex, texFileNameNoExtension, tableCaptionText)
    }
    
    
    # Write into the file
    fileConn = file(texFilePath)
    #writeLinesBN(paste(finalImageString, "\n", finalTableString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    writeLinesBN(paste(finalImageString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)
    
  }
  
  
}