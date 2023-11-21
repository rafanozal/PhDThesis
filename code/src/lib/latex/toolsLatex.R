# This tools helps you convert things into latex, mainly making a latex code
# that makes images in latex, and a latex code that makes tables in latex

# Set constant LATEX expressions
# -----------------------------------------------------------------------------
{
  
  # This are the common strings that you need to write in Latex to make images
  # tables, or whatever. The way this work is more or less like this:
  #
  # TABLE HEADER +
  # Specific table info +
  # TABLE ENDING
  #
  # And so on. So these variable are everything that is constant.
  
  # TABLE IN LATEX (all tables)
  LATEX_TABLE_COMMENT = paste("% ------------------------------------- \n",
                              "% TABLE \n",
                              "% ------------------------------------- \n",
                              sep = "")

    
  LATEX_TABLE_BEGIN_STRING  = paste("\\begin{table}[H]\n",
                                    "    \\centering\n",
                                    sep="")
  

  LATEX_TABLE_BEGIN_BOX_STRING_A  = "\\resizebox "
  LATEX_TABLE_BEGIN_BOX_STRING_B  = " \n"
  LATEX_TABLE_END_BOX_STRING    = "    \\end{tabular}\n } \n "
  
  

  
  
  
  #LATEX_TABLE_BEGIN_BOX_STRING  = "\\resizebox{\\textwidth}{!}{ \n"
  #LATEX_TABLE_END_BOX_STRING    = "    \\end{tabular}\n } \n "
  
  
  LATEX_TABLE_END_STRING        = "\\end{table}\n" # This is the real ending, weird naming I know.
  
  
  # TABLE CATEGORICAL DATA
  LATEX_CATEGORICAL_TABLE_HEADER_START_STRING = paste("\\begin{tabular}{lll}\n",
                                                      "        \\hline\n",
                                                      "        \\rowcolor[HTML]{FFFFC7}\n",
                                                      sep="")
  
  
  # IMAGE IN LATEX  (all images)
  LATEX_IMAGE_COMMENT  = paste("% ------------------------------------- \n",
                               "% IMAGE \n",
                               "% ------------------------------------- \n",
                               sep = "")
  
  LATEX_IMAGE_LOCATION = "../img/results/all/"
  
  LATEX_IMAGE_BEGIN_A    = "\\begin{figure}[H"
  
  LATEX_IMAGE_BEGIN_B    = paste("]\n",
                               "{\n",
                               "    \\centering\n",
                               sep="")
  
  
  LATEX_IMAGE_END      = paste("}\n",
                               "\\medskip\n",
                               "\\end{figure}\n",
                               sep="")
  
  
  # LATEX WARNINGS
  LATEX_COMPILE_TWICE_COMMENT  = paste("% -------------------------------------  \n",
                                       "% Notice that you need to compile twice  \n",
                                       "% the latexPDF in order for the index of \n",
                                       "% figures and tables to work properly    \n",
                                       "% -------------------------------------  \n",
                                       sep = "")
  
  
}



# IMAGES
# -----------------------------------------------------------------------------
{

    # Wrapper for the writeImageLATEX2 function. It generates the appropiate
    # string text to render the given image in latex.
    getImageLatexString2 <- function(relativeImageFilePath, finalLabel, captionText,
                                     pageWidth = 0, pageHeight = 0, keepRatio = TRUE,
                                     overrideFloat = FALSE){

        
        # Decide if we measure by width or height
        imageSizeString = ""
        ratioString   = ", keepaspectratio"
        # Do we keep the original ratio?
        # -- YES
        if(keepRatio == TRUE){
        
            # Decide if we measure by width or height
            # -- Nothing given, use default
            if(pageWidth == 0 && pageHeight == 0){
                
                pageWidth  = 0.5
                pageHeight = 0.5
                
                imageSizeString = paste("[width=",pageWidth,"\\textwidth, height=",pageHeight,"\\textheight",ratioString,"]")
            }
            # -- Width win, height follows
            if(pageWidth != 0 && pageHeight == 0){

                imageSizeString = paste("[width=",pageWidth,"\\textwidth",ratioString,"]")
                
            }
            # -- Height win, width follows
            if(pageWidth == 0 && pageHeight != 0){

                imageSizeString = paste("[height=",pageHeight,"\\textheight",ratioString,"]")
                
            }
            # -- Width and height contradict each other, width wins by default
            if(pageWidth != 0 && pageHeight != 0){

                imageSizeString = paste("[width=",pageWidth,"\\textwidth",ratioString,"]")
                
            }
            
            
        }
        # -- NO
        else{
            
            # Decide if we measure by width or height
            # -- Nothing given, use default
            if(pageWidth == 0 && pageHeight == 0){
                
                pageWidth  = 0.5
                pageHeight = 0.5
                
                imageSizeString = paste("[width=",pageWidth,"\\textwidth, height=",pageHeight,"\\textheight]")
            }
            # -- Width win, height follows because we can't have height 0
            if(pageWidth != 0 && pageHeight == 0){

                imageSizeString = paste("[width=",pageWidth,"\\textwidth",ratioString,"]")
                
            }
            # -- Height win, width follows because we can't have width 0
            if(pageWidth == 0 && pageHeight != 0){

                imageSizeString = paste("[height=",pageHeight,"\\textheight",ratioString,"]")
                
            }
            # -- Width and height contradict each other, this is fine, no ration is kept
            if(pageWidth != 0 && pageHeight != 0){

                imageSizeString = paste("[width=",pageWidth,"\\textwidth, height=",pageHeight,"\\textheight]")
                
            }
            
        }
        
        # Label and location
        imageLabelLatex     = paste0("    \\label{", finalLabel  ,"}\n")
        imageLocationString = paste0("    \\includegraphics",imageSizeString,"{",
                                          relativeImageFilePath,"}")
    
        # Is important that no space is in between the {}
        # or else you get a compiling error in Latex
    
        # Caption
        imageCaptionLatex           = paste("    \\caption{",captionText,"}", sep="")
    
        # Override the float position
        LATEX_IMAGE_BEGIN = ""
        if(overrideFloat == TRUE) LATEX_IMAGE_BEGIN = paste0(LATEX_IMAGE_BEGIN_A,     LATEX_IMAGE_BEGIN_B)
        else                      LATEX_IMAGE_BEGIN = paste0(LATEX_IMAGE_BEGIN_A,"!", LATEX_IMAGE_BEGIN_B)
        
        # Final Latex expression
        finalImageString  = paste( LATEX_IMAGE_COMMENT,
                                   LATEX_IMAGE_BEGIN,
                                   imageLocationString,
                                   imageCaptionLatex,
                                   imageLabelLatex,
                                   LATEX_IMAGE_END,
                                   sep="\n")
    
        return (finalImageString)
    
    }  
    
    # Get an image that is already written in disk, and generate a .tex file that
    # you can input{} in your latex file in order to render the image.
    #
    # absoluteImageFilePath   is where the image is located in disk. ie:
    #
    #                             "/home/myImages/analysis.png"
    #
    #                         you can also give a relative path with respect your R
    #                         working directory.
    #
    # relativeImageFileFolder is the folder where the image is located with respect
    #                         the final .tex latex file in here the string needs to
    #                         be written normally, as in:
    #
    #                             "../../my_Images/latex/"
    # 
    #                         keeping the original symbols such as spaces and
    #                         underscores intact.
    #
    # finalTexFilePath        is where the latex file generated by this function will be 
    #                         written. Default is NULL and it will use the same 
    #                         imageFilePath but changing the extension to ".tex" instead
    #                         of .png. You can however overwrite the final destination.
    #
    # captionText             default NULL, optional caption that will be written in the
    #                         latex document
    #
    # overrideLabel           default NULL, you can define the final label here. The
    #                         default is "\label{fig:" + absoluteImageFilePath name without
    #                         extension + "}"
    #
    # pageWidth               how big relative to the page width must the image be, default 0.5
    #
    # pageHeight              how big relative to the page heitht must the image be, default 0.5
    #
    # keepRatio               keep pageWidth and pageHeight proportional to the original ratio.
    #                         pageWidth is the base and pageHeight will be adjusted accordingly.
    #
    #
    # Return the texFilePath
    writeImageLATEX2 <- function(absoluteImageFilePath, relativeImageFileFolder, finalTexFilePath = NULL,
                                 captionText = NULL, overrideLabel = NULL,
                                 pageWidth = 0, pageHeight = 0, keepRatio = TRUE,
                                 overrideFloat = FALSE){

        # First thing, get the filepaths ready
        imageFolder        = getFileFolder(absoluteImageFilePath, includeLastSlash=TRUE)
        imageFileName      = getFileName(absoluteImageFilePath,   includeExtension=FALSE)
        imageFileExtension = getFileExtension(absoluteImageFilePath)
        texFileName        = ""
        texFilePath        = ""
        
        # -- Where are we saving the .tex generated?
        if(is.null(finalTexFilePath)){
    
            texFileName      = paste( imageFileName , ".tex" ,   sep="")
            texFilePath      = paste( imageFolder, texFileName , sep="")
            finalTexFilePath = texFilePath
    
        }
        
        # -- Where is the source of the image respect the latex file?
        finalSourceString = paste0(relativeImageFileFolder,imageFileName,imageFileExtension)
        
        # Now, what label are we going to use?
        if(is.null(overrideLabel)){
            
            overrideLabel = paste0("fig:",imageFileName)
            
        }
        
        # Finally, the caption, do we want any?
        if(is.null(captionText)){
            
            captionText = "Default caption, please fill manually"
            
        }
        
        
    
        # Build the superstring for the image
        finalImageString  = getImageLatexString2( finalSourceString, overrideLabel, captionText,
                                                  pageWidth = pageWidth, pageHeight = pageHeight,
                                                  keepRatio = keepRatio, overrideFloat = overrideFloat)
            
            
            

        # Write into the file
        fileConn = file(texFilePath, "w")
        writeLinesBN(paste(finalImageString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)

        return(texFilePath)
    
    }
    
    
    
    
  # From an image that you have in disk, get latex expression capable of rendering
  # that image in particular from that particular folder.
  #
  # You need to enter the caption manually, otherwise default to "Default Caption, fill manually".
  getImageLatexString <- function(filePath, captionText = NULL,
                                  pageWidth = 0.5, pageHeight = 0.5, keepRation = TRUE){
    
    
    # Get path information
    myExtension           = getFileExtension(filePath)
    myFileName            = getFileName(filePath)
    myFileNameNoExtension = getFileName(filePath, includeExtension=FALSE)
    myFileFolder          = getFileFolder(filePath)
    
    # Set dimensions of the image in latex format
    myPageWidth  = pageWidth
    myPageHeight = pageHeight
    ratioString   = ", keepaspectratio"
    imageSizeSting = paste("[width=",myPageWidth,"\\textwidth, height=",myPageHeight,"\\textheight",ratioString,"]")
    
    # Label and location
    imageLabelLatex     = paste("    \\label{fig:",myFileNameNoExtension,"}\n", sep="")
    imageLocationString = paste("    \\includegraphics",imageSizeSting,"{",
                                filePath,"}", sep='')
    # Is important that no space is in between the {}
    # or else you get a compiling error in Latex
    
    # Caption
    if(is.null(captionText)) captionText = "Default Caption. Please fill manually."
    imageCaptionLatex           = paste("    \\caption{",captionText,"}", sep="")
    
    
    # Final Latex expression
    finalImageString  = paste( LATEX_IMAGE_COMMENT,
                               LATEX_IMAGE_BEGIN,
                               imageLabelLatex,
                               imageLocationString,
                               imageCaptionLatex,
                               LATEX_IMAGE_END,
                               sep="\n")
    
    
    return (finalImageString)
    
  }  
 

  # Write any image into a latex .tex file.
  # You can easily later call this .tex file with input{} in the latex editor.
  #
  # imageFilePath is where the image is located
  #
  # texFilePath is where this latex file will be written. Default is NULL and
  # it will use the same imageFilePath but changing the extension to ".tex" instead
  # of .png for example.
  #
  #
  # Return the texFilePath
  writeImageLATEXOLDVERSION <- function(imageFilePath, captionText = NULL,
                              pageWidth = 0.5, pageHeight = 0.5, keepRation = TRUE,
                              texFilePath = NULL){

    imageFolder   = getFileFolder(imageFilePath, includeLastSlash=TRUE)
    imageFileName = getFileName(imageFilePath, includeExtension=FALSE)
    texFileName   = ""

    if(is.null(texFilePath)){
    
      texFileName   = paste( imageFileName , ".tex" ,   sep="")
      texFilePath   = paste( imageFolder, texFileName , sep="")
    
    }
    
    # Build the superstring for the image
    finalImageString  = getImageLatexString(imageFilePath, captionText = captionText,
                                            pageWidth = pageWidth, pageHeight = pageHeight, keepRation = keepRation)

    # Write into the file
    fileConn = file(texFilePath, "w")
    writeLinesBN(paste(finalImageString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)

    return(texFilePath)
    
  }
  
  # Write a list of images  into a latex .tex file in the form of a grid/matrix.
  # You can easily later call this .tex file with input{} in the latex editor.
  #
  # imageFilePathList is where the images are located. If the element X of the
  #                   list is NULL/NA/0/"0"/FALSE , then that a blank space
  #                   will be place in that particular space in the grid of
  #                   images.
  #
  # texFileName (String) is the file name that the resulting .tex file to make
  #             this grid will have. Do not include the ".tex" extension. If
  #             this name is NULL, the function will gives a random name in the
  #             form of "GRID_<random number 0-999999>.tex".
  #
  #             If BOTH texFileName and texFilePath are NOT NULL, the
  #             texFilePath takes priority and the texFileName will be ignore.
  #
  # texFilePath is where this latex file will be written. Default is NULL and
  #             it will use the same imageFilePathList[1] folder but changing
  #             the name to texFileName if this is not NULL. If this is also
  #             NULL, the default name is "GRID_<random number 0-999999>.tex".
  #
  #             In this case, you need to include the ".tex" extension.
  #
  # rows (int) how many rows do you want.
  #
  # columns (int) how many columns do you want
  #
  #         If the combination of rows x column is bigger than the number of
  #         images, you will still get a grid but with a lot of blank spaces.
  #
  #         If the combination of rows x column is smaller than the number of
  #         images, you will still get the grid, but not all the images will be
  #         included. This however will trigger a warning message.
  #
  # pageWidth is how big the entire grid is with respect the page space. So each
  #           individual image will be a fraction of that. For example, if your
  #           grid has 3 columns, and your pageWidth is 70% of the image, each
  #           individual image will be 70%/3 rounded down = 23%
  #
  #           (int) (0-1]
  #
  # subFloatList is the caption that is given for each individual image. If null
  #              it default to "1", "2", ... "N" until all caption are filled.
  #
  #              List<(String)>
  #
  # captionOverride (String) is the caption for the whole grid. If NULL it will
  #                 default to "".
  #
  # latexLabel (String) the label for the tex image. If NULL (default) it will
  #            default to texFileName. In any case, the function will enforce
  #            that the label don't have ANY special character, and will
  #            subtitute spaces for "_" and remove any extension.
  #
  # Return the texFilePath with the .tex file that you need to input{}
  writeImageGridLATEX <- function(imageFilePathList, rows, columns,
                                  texFileName = NULL, texFilePath = NULL,
                                  pageWidth = 0.5, keepRatio = TRUE,
                                  subFloatList = NULL, latexLabel = NULL,
                                  captionOverride = NULL){

    # Init some variables
    totalCells  = rows * columns
    totalImages = length(imageFilePathList)
    widthString = ""
    latexString = "\\begin{figure}[htp]\n    \\centering\n\n"
    
    if(totalCells < totalImages){
      
      print("WARNING: ")
      print( paste0("I have ", totalImages ," images but only ", totalCells, " cells in the grid."))
      print("I'm doing the grid anyway, but be aware of this please.")
      
    } 
    
    # If you don't have a tex file path, generate one automatically.
    if(is.null(texFilePath)){
      
      # If you don't have a tex file name, generate one automatically.
      if(is.null(texFileName)){
        
        texFileName = paste0("GRID_",round(runif(1, 0, 999999),0))
        
      }
      
      # Make sure that the tex file name has a .tex extension. You don't need
      # to write one as per function definition, but this makes sure that there
      # is none written so it doesn't end with a ".tex.tex" file extension.
      if(getFileExtension(texFileName) != ".tex"){
        
        texFileName   = paste( texFileName , ".tex" ,   sep="")
      
      }
       
      # Make the final filePath 
      imageFolder   = getFileFolder(imageFilePathList[1], includeLastSlash=TRUE)
      texFilePath   = paste( imageFolder, texFileName , sep="")
      
    }
    
    # Prepare the latex label
    {
      # Make equal to tex file name by default
      if(is.null(latexLabel)){
        
        latexLabel = texFileName
        
      }      
      
      # Ensure that no weird character is in the name
      latexLabel = cleanWeirdCharacters(latexLabel)
      
      # Remove the extension if any
      latexLabel = getFilePathNoExtension(latexLabel)
      
    }

    # Prepare the caption
    if(is.null(captionOverride)) captionOverride = ""
    
    # Prepare the subfloat names
    if(is.null(subFloatList)){
      
      subFloatList = c(1:totalImages)
      
    }
    
    # Prepare the width for each image
    {
      pageWidth   = (pageWidth/columns) * 0.9 # 0.9 reduce the overall by 10%, it kinda looks better 
      widthString = paste0("[width = ",pageWidth," \\textwidth ]")
    }
    
    # Finally, make the latex superString
    {
      
      currentSubFloatIndex = 1
      
      # Add the label
      latexString = paste0(latexString, "    \\label{fig:", latexLabel,"}\n\n" )
      # Add the caption
      latexString = paste0(latexString, "    \\caption{",captionOverride,"}\n\n")
      
      # Add each row
      for (i in 1:rows) {
        
        for (j in 1:columns) {

          # Each of the parts
          currentSubFloatString = subFloatList[currentSubFloatIndex]
          currentCellString    = paste0("    \\subfloat[",currentSubFloatString,"]{")
          currentLabelString   = paste0("\\label{figur:", subFloatList[currentSubFloatIndex],"}")
          currentGraphicString = paste0("\\includegraphics", widthString,"{",imageFilePathList[currentSubFloatIndex],"}}\n")
          
          # The whole line
          currentCellString = paste0(currentCellString, currentLabelString, currentGraphicString)
          
          # Include the whole line to the string where we are accumulating everything
          latexString = paste0(latexString, currentCellString)
          
          # Increase indexes
          currentSubFloatIndex = currentSubFloatIndex + 1
          
        }
        
        # Once we are finish with the row, we need to include the special \\ character
        latexString = paste0(latexString, "    \\\\\n")
      }
      
      # At this point we are finish with each row, but we have one "\\\n" too many
      # Delete that
      latexString = substr(latexString, 1, nchar(latexString)-4 )
      
      # Make an extra line so the end string looks more human readable
      latexString = paste0(latexString, "\n\n")
      
      # Add the end of figure string
      endFigureString = "\\end{figure}\n"
      latexString     = paste0(latexString, endFigureString)
      
      
    }
    
    # Write the string into disk
    fileConn = file(texFilePath)
    writeLinesBN(paste0(latexString, "\n", LATEX_COMPILE_TWICE_COMMENT, "\n"), fileConn)
    
    # And finally, return the filepath
    
    return(texFilePath)

  }
  
}



# TABLES
# -----------------------------------------------------------------------------
{
  
  
  # Transform an R Dataframe to a LATEX String expression
  #
  # Intervals numbers need +1 color
  # ( -Inf , N1   ] = Color 1
  # (N1    , N2   ] = Color 2
  # (N2    , N3   ] = Color 3
  # (N3    , +Inf ] = Color 4
  #
  #
  # (bool) autohyperlinks = If your string has an URL in it, it will wrap it
  #                         with an url{} text so you can follow the hyperlink
  #                         after latex compilation.
  #
  # Exact colors numbers are apply after the interval colors
  getTableFromDF <- function(tableBase, captionText = "default caption", transposeTable = FALSE, roundMe = Inf,
                             addBreakingLine = TRUE,
                             widthProportion = 0, heightProportion = 0,
                             overrideTableName = NULL, warningComment = TRUE,
                             intervalsNumbers = NULL, intervalsColors = NULL, intervalClose = "right",
                             exactValues = NULL, exactColors = NULL,
                             rotateColumnHeaders = FALSE, autohyperlinks = TRUE,
                             forcedColumnWidths = NULL, headerColor = NULL){
    
      # R Studio is  a piece of shit sowftware,
      # There are 4 spaces to the left, or one tab.
      # Clicking TAB brings you to 4.
      # Clicking ENTER brings you to 6. instead of 4 or 8
      # Because the getTableFromDF() function is in +2 spaces
      # The +2 is because the stupid developers...
      #
      # FUCK EVERY FUCKING SINGLE PROGRAMMING CONVENTION AND PUT +2 BY DEFAULT!
      #
      # WHO THE FUCK DO YOU THINK YOU ARE!?
      # YOU HAVE NO IDEA OF PROGRAMMING LANGUAGES DEVELOPING AND YOU SWING YOUR
      # DICK WITH THIS SHIT THINKING THAT THIS IS GREATER THING EVER BECAUSE
      # YOU SAY SO!???
      
      
    # Init variables
    myTableName    = deparse(substitute(tableBase))
    
    # Addjust the proportions
    {
        latexTableBeginBoxString = LATEX_TABLE_BEGIN_BOX_STRING_A
          # -- First, let see in which case we are
          
          # ---- Addjust by width, and resize height accordingly
          if(widthProportion != 0 && heightProportion == 0)
          
              latexTableBeginBoxString = paste0(latexTableBeginBoxString, "{",as.character(widthProportion),"\\textwidth}{!}{")
              
          # ----  Addjust by height, and resize width accordingly
          if(widthProportion == 0 && heightProportion != 0)
              
              latexTableBeginBoxString = paste0(latexTableBeginBoxString, "{!}{", as.character(heightProportion),"\\textheight}{")
              
          # ----  Addjust by both
          if(widthProportion != 0 && heightProportion != 0)
              
              latexTableBeginBoxString = paste0(latexTableBeginBoxString, "{",as.character(widthProportion),"\\textwidth}{", as.character(heightProportion),"\\textheight}{")
              
          # ----  Addjust by neither
          if(widthProportion == 0 && heightProportion == 0)
              
              latexTableBeginBoxString = paste0(latexTableBeginBoxString, "{!}{!}{")
          
          
          latexTableBeginBoxString = paste0(latexTableBeginBoxString, LATEX_TABLE_BEGIN_BOX_STRING_B)
      }
      
      # Add the +/- Inf to the numbers
      if(!is.null(intervalsNumbers)) intervalsNumbers = c(intervalsNumbers, +Inf)
          totalIntervals = length(intervalsNumbers)
    
      # If you need to override your table name, then do it now
      if(!is.null(overrideTableName))
          myTableName = overrideTableName
      
    
      initialLatexComment = paste("% ------------------------------------- \n",
                                  "% ", myTableName ," \n",
                                  "% ------------------------------------- \n",
                                  sep = "")

      # If the user say so, transpose the table
      if(transposeTable == TRUE){
      
      # Get the original dimensions and final dimensions
           originalRows    = nrow(tableBase)
           originalColumns = ncol(tableBase)
           finalRows       = originalColumns
           finalColumns    = originalRows
      
           # First save the names of the columns which will be the row names later
           newNames           = c(colnames(tableBase)[1], tableBase[,1])
      
           # Check which rows are all numeric values
           # This will later be transform into columns with all numeric values
           # But beware that the first row will be your column name,
           #  so it doesn't matter whether that is a numeric or not
           # Save where is a numeric value
           myNumerics         = tableBase
           myNumerics[]       = lapply(tableBase, is.numeric)
           myNAs              = tableBase
           myNAs[]            = lapply(tableBase, is.na)
      
           myNumerics = myNumerics | myNAs
      
           numericRows        = rep(FALSE, originalRows)
      
           if(originalRows > 1){
        
               for (i in 1:originalRows) {
          
                   totalTRUE = sum(myNumerics[i,2:originalColumns])
                   if(totalTRUE == (originalColumns - 1)) numericRows[i] = TRUE
          
               }
        
           }
      
           # Transpose the data
           newTable           = as.data.frame(t(tableBase), stringsAsFactors = FALSE)
      
           # Convert the numbers back
      
           # Save the row names which will become the column names
           rowNames           = rownames(newTable)
           # Delete the first column
           rownames(newTable) = NULL
           # Write the new first column with the names you saved before
           newTable           = cbind(rowNames,newTable)
           newTable           = newTable[-c(1),]
      
           colnames(newTable) = newNames
      
           tableBase          = newTable
      
           # Now tableBase contain the final data, but we need to convert the datatypes back to numeric where is needed
           if(originalRows > 1){
        
               for (i in 1:originalRows) {
          
                   if(numericRows[i] == TRUE) tableBase[,i+1] = as.numeric(as.character(tableBase[,i+1])) #+1 because the whole table is displace +1 column to the right
          
               }
        
           }
      
        }
    
        # Get basic info
        totalRows    = nrow(tableBase)
        totalColumns = ncol(tableBase)
        myColnames   = colnames(tableBase)

        # Prepare the special header
        # ---- Prepare the column headers if no forced width
        specialColumnsHeader = paste(rep("l", totalColumns), collapse = "")
        # ---- If forced with (total clusterfuck; R is impossible to work with markup language)
        if(!is.null(forcedColumnWidths)){
        
            # Check that we have the same length of values as the number of colums
            if(length(forcedColumnWidths) == totalColumns){
              
                specialColumnsHeader = ""
                
                for(i in 1:totalColumns){
                    
                    if(forcedColumnWidths[i] == 0)
                        specialColumnsHeader = paste0(specialColumnsHeader, "l")
                    else
                        specialColumnsHeader = paste0(specialColumnsHeader, "p{",forcedColumnWidths[i],"cm} ")
                    
                }
                
            }
            
            else{
            
                print("ERROR: Forced column vector is incompatible, defaulting to no forced")
                    
            }
          
        }
      
      
        
        # I hate R. A string + vector is an error
        # What it was doing here is string + vector = string + vector[1] ¬¬
        # Who make this stupid casting choices!!??
        #
        # AND CHECK THIS:
        # help(paste0) DOESN'T EXIST!!!
        
        currentColor = "FFFFC7"
        if(!is.null(headerColor)) currentColor = headerColor
        
      specialHeaderString = paste(" 	\\renewcommand{\\arraystretch}{1.5} \n    \\begin{tabular}{", specialColumnsHeader, "}\n        \\hline\n        \\rowcolor[HTML]{",currentColor," }\n")
    
      # Override the special header if we want specific column widths
      
      
      # Prepare the label and caption strings
      # Transform weird string characters into proper latex characters
      # -- %
      if (grepl("%", myTableName)) myTableName = gsub("%", "\\\\%", myTableName)
      if (grepl("%", captionText)) captionText = gsub("%", "\\\\%", captionText)
      # -- _  
      #if (grepl("_", myTableName)) myTableName = gsub("_", "$\\\\_$", myTableName)  # Subscripts don't need transformation for the label
      if (grepl("_", captionText)) captionText = gsub("_", "$\\\\_$", captionText)
      
      
      
      tableLabelLatex             = paste("    \\label{table:",myTableName,"}\n", sep="")
      tableCaptionLatex           = paste("    \\caption{",captionText,"}", sep="")
    
      # Prepare the first row with the title for each column
      firstRowString = ""
      for (i in 1:totalColumns) {
      
           # Get the original name
           rawColName = myColnames[i]
           if(is.null(rawColName)) rawColName = " "
           
           # Check that the string is not already in latex form, surrounded by '$ ... $'
           latexForm       = FALSE
           totalCharacters = nchar(rawColName)
           
           if(totalCharacters>=3){
            
               firstCharacter = substr(rawColName,1,1)
               lastCharacter  = substr(rawColName,totalCharacters,totalCharacters)   
               
               if(firstCharacter == '$' && firstCharacter == lastCharacter)
                   latexForm = TRUE    
           }
           
           # Otherwise, convert weird charactes
           if(latexForm == FALSE){

               # Transform weird string characters into proper latex characters
               # -- %
               if (grepl("%", rawColName)) rawColName = gsub("%", "\\\\%", rawColName)
               # -- _  
               if (grepl("_", rawColName)) rawColName = gsub("_", "$\\\\_$", rawColName)
                              
           }
           
           # Rotate the table header if neccesary
           if(i>1 && rotateColumnHeaders == TRUE) rawColName = paste0("\\rotatebox{90}{", rawColName, "}")
      
           # Add it to the header string
           firstRowString = paste(firstRowString, "        \\textbf{", rawColName ,"} &")
      
        }
        
      # Delete the last "&"
      firstRowString = substr(firstRowString,1,nchar(firstRowString)-1) 
      
      # Finish the header line
      firstRowString = paste(firstRowString, "\\\\ \n",
                                             "        \\hline \n",
                             sep="")
    
      # Prepare each of the rows
      rowsString = ""
      for (i in 1:totalRows){
      
            firstCell = tableBase[i,1]
      
            # Check if we need to round this cell, usually it looks like this:
            #
            # -------------------------
            #  Men   |  4.567 | 4.7958
            #  Women |  3.493 | 5.9393
            # -------------------------
            #
            # So, Men and Women, don't need  (can't) rounding.
            
            # If it is a numeric cell, round it.
            if(is.numeric(firstCell)) firstCell = round(firstCell,roundMe)
            # Otherwise is probably the name of a variable.
            else{
        
                # Transform weird string characters into proper latex characters
                # -- %
                if (grepl("%", firstCell)) firstCell = gsub("%", "\\\\%", firstCell)
                # -- _  
                if (grepl("_", firstCell)) firstCell = gsub("_", "$\\\\_$", firstCell)

            }
      
            
            # Prepare the init of the row with the first cell
            rowStart = ""
            # In this string variable we write each one of the cells
            rowMiddle = ""
      
            # If we only have one column we don't neeed the extra | or the &
            if(totalColumns == 1){
                
                rowStart  = paste("        \\multicolumn{1}{l}{", firstCell , "}")
                
            }
            # Otherwise, we do
            else{
                
                if(addBreakingLine == TRUE)
                    rowStart  = paste("        \\multicolumn{1}{l|}{", firstCell , "} &")
                else
                    rowStart  = paste("        \\multicolumn{1}{l}{",  firstCell , "} &")
                
            }
            
            # We only need to add cells is there is more than one column
            if(totalColumns > 1){
        
                # Add each of the cells
                for (j in 2:totalColumns) {

                    cellIsNumeric = FALSE
                    currentCell   = tableBase[i,j]

                    # If it is numeric, round, and color it if needed
                    if(is.numeric(currentCell) && !is.na(currentCell)){  # I don't know why, but NA pops as TRUE numeric now :/

                        cellIsNumeric = TRUE
                    
                        # Round the number
                        # -- If the number is zero, do nothing
                        #    Otherwise, we might need to normalize very small numbers
                        if(tableBase[i,j] != 0){
              
                            # If it is positive...
                            if(tableBase[i,j] > 0){
                          
                                # ...and very, very small, turn it to zero
                                if(tableBase[i,j]< 1/10^24){
                                    currentCell = 0
                                } 
                                # ...but not so small, just round it
                                # If the result is too small, write it into scientific notation
                                else{
                                    currentCell = round(tableBase[i,j],roundMe)  
                                    if(currentCell == 0) currentCell = signif(tableBase[i,j],1)
                                }
                            }
                        
                            # If it is negative...
                            else{
                                # ...and very, very small, turn it to negative zero
                                if( abs(tableBase[i,j] )< 1/10^24){
                                    currentCell = signif(-1/10^-23,1)
                                }
                                # ...but not so small, just round it
                                # If the result is too small, write it into scientific notation
                                else{
                                    currentCell = round(tableBase[i,j],roundMe)
                                    if(currentCell == 0) currentCell = signif(tableBase[i,j],1)
                                }
                        
                            }

                        }
                        else{
                            currentCell = 0
                        }
            
                        # Make it into a string for later processing
                        currentCell = as.character(currentCell)

                        # If the user wanted to add colors do so
                        if(!is.null(intervalsNumbers)){

                            # Find the interval to which this number correspond
                            # ---- Init the variables
                            selectedColor = NA
                            found = FALSE
                            # ---- For each of the numbers in the intervals, find which one you are.
                            for(k in 1:totalIntervals){
                
                                if(found == FALSE){
                                  
                                  currentLeftInterval = intervalsNumbers[k]
                
                                  if(tableBase[i,j] < currentLeftInterval){
                                    
                                    selectedColor = intervalsColors[k]
                                    found = TRUE
                                    k = totalIntervals
                                    
                                  }
                                  
                                }
                
                            }
              
                            # Clean the # from the color name
                            selectedColor = substring(selectedColor, 2)
              
                            # Add the color to the cell
                            currentCell = paste("\\cellcolor[HTML]{",selectedColor,"}", currentCell, sep='')
              
                        }
                        
                        # Finally, convert the extra characters
            
                    }
          
                    # If it is not numeric, then we have text
                    else{
                                            
                        # (If it is not numeric, you might still color if it is an exact match)
              
                        # Transform weird string characters into proper latex characters
                        # notice that in here we don't convert formulas _ into subindexes,
                        # just a regular underscore
                        # -- %
                        if (grepl("%", currentCell)) currentCell = gsub("%", "\\\\%", currentCell)
                        # -- _  
                        if (grepl("_", currentCell)) currentCell = gsub("_", " \\\\textunderscore ", currentCell) # Two \\ because otherwise it get confuse with \t, and sorrounded by spaces because latex say so
                                     
                        # -- Greek letters
                        #
                        # ---- Alpha
                        if (grepl("α", currentCell)) currentCell = gsub("α", "\\alpha",  currentCell)           
                        # ---- Beta
                        if (grepl("β", currentCell)) currentCell = gsub("β", "\\beta",   currentCell)           
                        # ---- Gamma
                        if (grepl("γ", currentCell)) currentCell = gsub("γ", "\\gamma",  currentCell)           
                        # ---- Kappa
                        if (grepl("κ", currentCell)) currentCell = gsub("κ", "\\kappa",  currentCell)           
                        
                        
                        
                        
                        # If it is not numeric, and hyperlink are set to true, check if you have an
                        # http link, and wrap it into an URL latex form. We need to do this after the 
                        # special characters has been converted.
                        if(autohyperlinks == TRUE){
                        
                            # Check whether we have a hyperlink or not
                            
                            httpInside = grep("http", currentCell)
                                
                            # R is a horrible language. Grep, instead of giving you a FALSE,
                            # or a -1, or 0, indicating that substring doesn't exist,
                            # like any other good christian programming language would do, it
                            # gives you a logical vector of size 0. Because fuck all logic.
                            #
                            # So what I'm doing here is checking that the length of the logical
                            # vector is zero or not, so I can tell if this is TRUE or FALSE
                            if(length(httpInside)>=1){
                            
                                currentCell = paste0("\\url{",currentCell,"}")
                                    
                            }

                        }
                        
                        
                        
                    }
                    
                    # In any case, append it to the current row, and do the next cell
                    rowMiddle = paste( rowMiddle , currentCell,"  &")
                    
                }
                
            }
        
            # Add the row ending, we need to do this regardless of whether we have one column or many
            
            rowMiddle = substr(rowMiddle,1,nchar(rowMiddle)-1) # Delete the last "&"
        
            rowMiddle = paste(rowMiddle,
                              "       \\\\ \n",
                              sep="")
            
            # Add it to the previous lines
            rowsString = paste(rowsString, rowStart, rowMiddle, sep="")
        
      }

      # Prepare the rest of the columns
      # ---- Build the superstring for the table
    
      # LATEX_TABLE_COMMENT
    
      finalTableString = paste(initialLatexComment,
                               LATEX_TABLE_BEGIN_STRING,
                               latexTableBeginBoxString,
                               specialHeaderString,
                               firstRowString,
                               rowsString,
                               LATEX_TABLE_END_BOX_STRING,
                               tableCaptionLatex,
                               tableLabelLatex,
                               LATEX_TABLE_END_STRING,
                               sep='\n')
    
      return(finalTableString)
    
  }

  # Write into a file a Dataframe in Latex String format
  #
  # texFilePath can be a folder or a file.
  #             if it is a folder, it will create an automatic filename for you
  #             if it is a actual file, it will use that.
  #
  # addBreakingLine TRUE (Default) After the first column, add a breaking line
  #                      that differienciate that column from the rest
  #                 FALSE No line is added
  #
  # intervalsNumbers (List of Floats) Sorted list of floats, ie:
  #                  c(-0.050, -0.010, -0.001,  0.000,  0.001,  0.010,  0.050)
  #
  # intervalsColors  (List of Colors) Colors that correspond to each interval number, ie:
  #                  c("#ffffff", "#fdd49e", "#fc8d59", "#d7301f", "#0570b0", "#74a9cf", "#d0d1e6", "#ffffff")
  #
  #                  In this case, the interval and colors would be:
  #                  (-Inf  , -0.05]  ffffff
  #                  (-0.05 , -0.01]  fdd49e
  #                  (-0.01 , -0.001] fc8d59
  #                  (-0.001,  0]     d7301f
  #                  (0     ,  0.001] 0570b0
  #                  (0.001 ,  0.01]  74a9cf
  #                  (0.01  ,  0.05]  d0d1e6
  #                  (0.05  ,  +Inf)  ffffff
  #
  #
  # return (String) A String with the path where the .tex file was save
  
  writeTableLATEX <- function(tableBase, texFilePath, tableCaption = "default caption table",
                              addBreakingLine = TRUE,
                              transposeTable = FALSE, roundMe = Inf,
                              widthProportion = 1, heightProportion = 0,
                              overrideTableName = NULL, warningComment = TRUE,
                              intervalsNumbers = NULL, intervalsColors = NULL, intervalClose = "right",
                              exactValues = NULL, exactColors = NULL,
                              rotateColumnHeaders = FALSE,
                              forcedColumnWidths  = NULL,
                              headerColor = NULL){
    
    # Init
    myTableName = deparse(substitute(tableBase))
    
    # -- If you need to override your table name, then do it
    if(!is.null(overrideTableName)){
      myTableName = overrideTableName
      
    }
    
    # -- Remove the final compiling comment if you don't want it
    finalLatexComment = LATEX_COMPILE_TWICE_COMMENT
    if(warningComment == FALSE){
      
      finalLatexComment = "\n"
      
    }
    
    # Get an automatic name if you don't have a proper one
    latexFilePath = automaticFilePath(texFilePath, tableName=myTableName, fileType = "LatexTable")
    
    # Delete the first # of the color code and make sure is a string
    # Because R hate you
    if(!is.null(headerColor)){
        
        headerColor = sub('.', '', headerColor)
        
    }
    
    # -------------------------------------
    # Build the superstring for the table
    # -------------------------------------
    {

      finalTableString = getTableFromDF(tableBase, captionText = tableCaption,
                                        transposeTable = transposeTable, roundMe = roundMe,
                                        addBreakingLine = addBreakingLine,
                                        widthProportion = widthProportion, heightProportion = heightProportion,
                                        overrideTableName = myTableName,
                                        intervalsNumbers = intervalsNumbers, intervalsColors = intervalsColors, intervalClose = intervalClose,
                                        exactValues = exactValues, exactColors = exactColors,
                                        rotateColumnHeaders = rotateColumnHeaders,
                                        forcedColumnWidths = forcedColumnWidths,
                                        headerColor = headerColor)
    }
    
    # Write into the file
    #print(latexFilePath)
    fileConn = file(latexFilePath, 'w')
    writeLinesBN(paste(finalTableString, "\n",
                       finalLatexComment ,sep=""), fileConn) # This close the connection TODO: take that out
    
    toReturn = newList(2)
    toReturn[[1]] = latexFilePath
    toReturn[[2]] = paste0("LatexTable_",myTableName)
    
    return(toReturn)
    
  }

  
}



# IMAGE + TABLE (This is a nightmare)
# -----------------------------------------------------------------------------
{

    writeBoxPlotCompositeLATEX <- function( absoluteImageFilePath, centralitiesDF, pValuesDF, relativeImageFileFolder,
        
                                            finalTexFilePath = NULL,
                                            captionText = NULL, overrideLabel = NULL,
        
                                            minipageLeft = 0.45, minipageRight = 0.45 , 
                                            minipageTop = 0.45,  minipageBottom = 0.45 ,         
                                            pageWidth = 0, pageHeight = 0, keepRatio = TRUE,
                                            topTableWidth    = 0.4, topTableHeight    = 0.05,
                                            bottomTableWidth = 0.4, bottomTableHeight = 0.05,
                                            stretchTop = 2.0, stretchBottom = 2.0,
                                            betweenSpaceMM = 3
                                           ){
    
        # First thing, get the filepaths ready
        imageFolder        = getFileFolder(absoluteImageFilePath, includeLastSlash=TRUE)
        imageFileName      = getFileName(absoluteImageFilePath,   includeExtension=FALSE)
        imageFileExtension = getFileExtension(absoluteImageFilePath)
        texFileName        = ""
        texFilePath        = ""
        
        # -- Where are we saving the .tex generated?
        if(is.null(finalTexFilePath)){
    
            texFileName      = paste( "LatexComposite_",imageFileName , ".tex" ,   sep="")
            texFilePath      = paste( imageFolder, texFileName , sep="")
            finalTexFilePath = texFilePath
    
        }
        
        # -- Where is the source of the image respect the latex file?
        finalSourceString = paste0(relativeImageFileFolder,imageFileName,imageFileExtension)
        
        # Now, what label are we going to use?
        if(is.null(overrideLabel)){
            
            overrideLabel = paste0("fig:",imageFileName)
            
        }
        
        # Finally, the caption, do we want any?
        if(is.null(captionText)){
            
            captionText = "Default caption, please fill manually"
            
        }
             
        
        
        # Define all the variables (This should be constants, and it should be protected by the latex class or whatever, this language is R and is hell
        
        # Beggining and end of all
        beginString = "\\begin{figure}[H]"
        endString   = "\\end{figure}"
        
        # Minipages
        miniPageLeftString   = paste0("   \\begin{minipage}[m]{",minipageLeft, "\\linewidth}")
        miniPageRightString  = paste0("   \\begin{minipage}[m]{",minipageRight,"\\linewidth}")        
        
        miniPageTopString    = paste0("          	\\begin{minipage}[t]{",minipageTop,"\\textheight}")
        miniPageBottomString = paste0("          	\\begin{minipage}[b]{",minipageBottom,"\\textheight}")        
        
        endMiniPageString   = "  \\end{minipage}"
        separatorMiniPage   = "  \\enspace"
        
        # Formatting and aligning
        centeringString  = "  \\centering"
        leftAlignString  = "    \\raggedleft"
        rightAlignString = "    \\raggedright"
        
        betweenTablesString = paste0("        	\\hfill\\vline\\vspace{",betweenSpaceMM,"mm}\\hfill")
        
        # Tables
        horizontalLinesTables     = "			\\hline"
        
        topTableResizeString      = paste0("        \\resizebox {",topTableWidth,"\\textwidth}{",topTableHeight,"\\textheight}{")
        bottomTableResizeString   = paste0("        \\resizebox {",bottomTableWidth,"\\textwidth}{",bottomTableHeight,"\\textheight}{")
        endResizeString           =        "         }"
        
        
        stretchTableTopString     = paste0("        \\renewcommand{\\arraystretch}{",stretchTop,"}")
        stretchTableBottomString  = paste0("        \\renewcommand{\\arraystretch}{",stretchBottom,"}")
        tabularTopTableString     = "		\\begin{tabular}{ lllll }"
        tabularBottomTableString  = "		\\begin{tabular}{ lllll }"
        endTabularString          = "       \\end{tabular}"
        
        colorTopTable             = "			\\rowcolor[HTML]{FFFFC7}"
        colorBottomTable          = "			\\rowcolor[HTML]{DDDDDD}"
        
        			
        
        headerTopTable            = "			\\multicolumn{1}{c}{}  & \\multicolumn{1}{c}{ N }  &    \\multicolumn{1}{c}{ Mean }  &   \\multicolumn{1}{c}{ Median }  &  \\multicolumn{1}{c}{ Sigma }  \\\\ "
        
        
        captionString = paste0("        \\caption{",captionText,"}  ")
        

        
        # Decide if we measure by width or height
        imageSizeString = ""
        ratioString   = ", keepaspectratio"
        # Do we keep the original ratio?
        # -- YES
        if(keepRatio == TRUE){
        
            # Decide if we measure by width or height
            # -- Nothing given, use default
            if(pageWidth == 0 && pageHeight == 0){
                
                pageWidth  = 0.5
                pageHeight = 0.5
                
                imageSizeString = paste("[width=",pageWidth,"\\textwidth, height=",pageHeight,"\\textheight",ratioString,"]")
            }
            # -- Width win, height follows
            if(pageWidth != 0 && pageHeight == 0){

                imageSizeString = paste("[width=",pageWidth,"\\textwidth",ratioString,"]")
                
            }
            # -- Height win, width follows
            if(pageWidth == 0 && pageHeight != 0){

                imageSizeString = paste("[height=",pageHeight,"\\textheight",ratioString,"]")
                
            }
            # -- Width and height contradict each other, width wins by default
            if(pageWidth != 0 && pageHeight != 0){

                imageSizeString = paste("[width=",pageWidth,"\\textwidth",ratioString,"]")
                
            }
            
            
        }
        # -- NO
        else{
            
            # Decide if we measure by width or height
            # -- Nothing given, use default
            if(pageWidth == 0 && pageHeight == 0){
                
                pageWidth  = 0.5
                pageHeight = 0.5
                
                imageSizeString = paste("[width=",pageWidth,"\\textwidth, height=",pageHeight,"\\textheight]")
            }
            # -- Width win, height follows because we can't have height 0
            if(pageWidth != 0 && pageHeight == 0){

                imageSizeString = paste("[width=",pageWidth,"\\textwidth",ratioString,"]")
                
            }
            # -- Height win, width follows because we can't have width 0
            if(pageWidth == 0 && pageHeight != 0){

                imageSizeString = paste("[height=",pageHeight,"\\textheight",ratioString,"]")
                
            }
            # -- Width and height contradict each other, this is fine, no ration is kept
            if(pageWidth != 0 && pageHeight != 0){

                imageSizeString = paste("[width=",pageWidth,"\\textwidth, height=",pageHeight,"\\textheight]")
                
            }
            
        }
        
        graphicsString =paste0(" 	\\includegraphics",imageSizeString,"{",finalSourceString,"}%")
        
        
        # Get the common variables
        totalModalities = nrow(centralitiesDF)-1
        modalitiesNames = centralitiesDF[1:totalModalities,1]
        
        # Make the table for the centralities
        {
        
            centralitiesTableString = paste( horizontalLinesTables,
                                             colorTopTable,
                                             headerTopTable,
                                             horizontalLinesTables,
                                             sep="\n")
        
            
            for(i in 1:totalModalities){
                
                
                rowString = paste0( "\\multicolumn{1}{r|}{ \\cellcolor[HTML]{DDDDDD} ", modalitiesNames[i], "} & ", centralitiesDF[i,2], 
                                                                                                             " & ", centralitiesDF[i,3],
                                                                                                             " & ", centralitiesDF[i,4],
                                                                                                             " & ", centralitiesDF[i,5], "\\\\")
                
                centralitiesTableString = paste(centralitiesTableString, rowString, sep = "\n")
                
            }
            
            centralitiesTableString = paste(centralitiesTableString, horizontalLinesTables, sep = "\n")
            
            
            totalString = paste0( "\\multicolumn{1}{r|}{ \\cellcolor[HTML]{DDDDDD} Total } & ", centralitiesDF[totalModalities+1,2], 
                                                                                         " & ", centralitiesDF[totalModalities+1,3],
                                                                                         " & ", centralitiesDF[totalModalities+1,4],
                                                                                         " & ", centralitiesDF[totalModalities+1,5], "\\\\")

            centralitiesTableString = paste(centralitiesTableString, totalString, sep = "\n")

        }
        
        # Make the table for the p-values
        {
            
            # Count how many modalities we have
            totalPColumns = ncol(pValuesDF)
            totalPRows    = totalPColumns
        
            topRowNames   = colnames(pValuesDF)
            columnsNames  = pValuesDF[,1]
            
            # Add each of the columns
            #headerBottomTable = "\\multicolumn{1}{c}{ }"
            headerBottomTable = ""
            for(j in 1:totalPColumns){
                headerBottomTable = paste0(headerBottomTable, "\\multicolumn{1}{c}{", topRowNames[j], " }  &")
            }
            # Delete last &
            headerBottomTable = substr(headerBottomTable, 1, nchar(headerBottomTable)-1)

            # Add the row separator
            headerBottomTable = paste0(headerBottomTable, "\\\\")
            
            pValuesTableString = paste( horizontalLinesTables,
                                        colorBottomTable,
                                        headerBottomTable,
                                        horizontalLinesTables,
                                        sep="\n")            
            
            
                    
            # Add each of the rows
            for(i in 1:(ncol(pValuesDF)-1)){
                
                # Add the name
                rowString = paste0( "\\multicolumn{1}{r|}{ \\cellcolor[HTML]{DDDDDD} ", pValuesDF[i,1], "} & ")
                    
                # Add the values
                for(j in 2:ncol(pValuesDF)){
                
                    rowString = paste0(rowString,     pValuesDF[i,j], "    &")
                    
                }
                # Delete last &
                rowString = substr(rowString, 1, nchar(rowString)-1)
                # Add line breaker
                rowString = paste0(rowString, "\\\\ \n")

                # Add the row to the whole matrix
                pValuesTableString = paste0(pValuesTableString, rowString)
            }
            
            # Add the horizontal line
            pValuesTableString = paste( pValuesTableString,
                                        horizontalLinesTables,
                                        sep="\n")   
            
            
            # Add the last row with the ANOVA result
            {
                
                lastRowIndex = nrow(pValuesDF)
                
                # Add the name
                rowString = paste0( "\\multicolumn{1}{r|}{ \\cellcolor[HTML]{DDDDFF} ", pValuesDF[lastRowIndex,1], "} & ")
                    
                # Add the values
                for(j in 2:ncol(pValuesDF)){
                
                    rowString = paste0(rowString,     pValuesDF[lastRowIndex,j], "    &")
                    
                }
                # Delete last &
                rowString = substr(rowString, 1, nchar(rowString)-1)
                # Add line breaker
                rowString = paste0(rowString, "\\\\ \n")
                
                # Add the row to the whole matrix
                pValuesTableString = paste0(pValuesTableString, rowString)  
                
            }
         			
        
        
            
            
        }
        
        # Final Latex expression
        finalString  = paste( beginString,
                              centeringString,
            
                              # mini page Left
                              # ----------------------------
                              miniPageLeftString,
                              leftAlignString,
                              graphicsString,
                              endMiniPageString,
                              separatorMiniPage,
                              # ----------------------------
            
                              # mini page Right
                              # ----------------------------
                              miniPageRightString,
                              rightAlignString,
            
                                  # mini page Top
                                  # ----------------------------
                                  miniPageTopString,
            
                                    topTableResizeString,
                                    stretchTableTopString,
                                    tabularTopTableString,
                                    centralitiesTableString,
                                    endTabularString,
                                    endResizeString,
            
                                  endMiniPageString,
                                  # ----------------------------
            
                                  betweenTablesString,
            
                                  # mini page Bottom
                                  # ----------------------------
                                  miniPageBottomString,
            
                                    bottomTableResizeString,
                                    stretchTableBottomString,
                                    tabularTopTableString,
                                    pValuesTableString,
                                    endTabularString,            
                                    endResizeString,
            
                                  endMiniPageString,
                                  # ----------------------------
            
                              endMiniPageString,
                              # ----------------------------
            
                              captionString,
                              endString,
            
            
                               sep="\n")
    
        # Write into the file
        fileConn = file(texFilePath, "w")
        writeLinesBN(paste(finalString, "\n", LATEX_COMPILE_TWICE_COMMENT ,sep=""), fileConn)

        return(texFilePath)        
        
        
        
    }
    
}
