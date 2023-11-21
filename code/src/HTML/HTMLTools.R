# VERY DEPRECATED!!!
# For each menu on the left side, a new menu on the center column appear
# This function generate each of the sub panels in the middle column
# VERY DEPRECATED!!!
generateSidePanel <- function(panelTitle, listOfTopics, listOfLinks){

    startString                     = "				<!-- Independent subcategories -->\n" 
    startString = paste0(startString, "				    <div class=\"sidePanel\">\n")
    startString = paste0(startString, "					    <h2>",panelTitle,"</h2>\n")
    startString = paste0(startString, "\n")
    startString = paste0(startString, "					    <hr>\n")
    startString = paste0(startString, "\n")
    
    for(i in 1:length(listOfTopics)){

            startString = paste0(startString, "			    		<p>", listOfTopics[i], "</p>")        
        
    }
    
    startString = paste0(startString, "\n")    
    startString = paste0(startString, "				    </div>\n")
    
        
}


# Generate a simple HTML table
generateHTMLSimpleTable <- function( myDF, 
                                     tableTitle = "", tableSubtitle = "",
                                     overrideTableName = NULL){
    
    # Prepare the table name if needed
    {
        # Init
        myTableName = deparse(substitute(myDF))
    
        # -- If you need to override your table name, then do it
        if(!is.null(overrideTableName)) myTableName = overrideTableName
    }

    # The final string comes here
    htmlString = ""

    # Get information about the Dataframe
    totalRows     = nrow(myDF)
    totalColumns  = ncol(myDF)
    columnsHeader = colnames(myDF)

    # The table has 3 parts
    # -- The overall container for the 3 parts (open)
    htmlString = paste0(htmlString, "        <div class=\"table-overall\" id=\"",myTableName,"\">")    
    htmlString = paste0(htmlString, "\n")
    # ---- The little box with the table description
    htmlString = paste0(htmlString, "            <div class=\"table-description-box\">")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                <div class=\"table-title\">")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                    <p>",tableTitle,"</p>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                </div>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                <div class=\"table-verbose\">")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                    <p>", tableSubtitle,"</p>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                </div>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "            </div>")    
    htmlString = paste0(htmlString, "\n")
    # ---- The menu with the buttons for saving
    #      (DEPECRATED, but left here so we can have a bunch of options later)
    if(FALSE){
    htmlString = paste0(htmlString, "        <div class=\"table-menu\">")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "            <div class=\"table-menu-option\">")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                <p>CSV</p>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "            </div>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "            <div class=\"table-menu-option\">")
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                <p>TXT</p>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "            </div>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "        </div>")        
    htmlString = paste0(htmlString, "\n")
    }
    
    
    # ---- The table with whatever info

    # The table has a container div above it so we can do flex stuff
    htmlString = paste0(htmlString, "            <div class=\"table-container\">")    
    htmlString = paste0(htmlString, "\n")
    
    # Actual table starts here
    htmlString = paste0(htmlString, "                <table class=\"simpleTable\">")    
    htmlString = paste0(htmlString, "\n")

    # Make the header
    htmlString = paste0(htmlString, "                    <tr>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                        <th class=\"tableHeader\"> <img class=\"iconPic\" src=\"",CSV_ICON_FILEPATH,"\" alt=\"Download as CSV file\"> </th>") # Upper-left corner with the CSV icon
    
    
    
    htmlString = paste0(htmlString, "\n")
    
    for(j in 1:totalColumns){
        htmlString = paste0(htmlString, "                        <th class=\"tableHeader\">",as.character(columnsHeader[j]),"</th>")        
        htmlString = paste0(htmlString, "\n")
    }
    
    htmlString = paste0(htmlString, "                    </tr>")    
    htmlString = paste0(htmlString, "\n")
    
    # Make the content of the table
    ATypeRow = TRUE
    ACounter = 1
    for(i in 1:totalRows){
       
        htmlString = paste0(htmlString, "                    <tr class = \"tableRow\">")
        htmlString = paste0(htmlString, "\n")
        if(ATypeRow == TRUE){
            htmlString = paste0(htmlString, "                        <td class=\"tableRowID tableRowID_A\">",i,"</td>")        
        }
        else{
            htmlString = paste0(htmlString, "                        <td class=\"tableRowID tableRowID_B\">",i,"</td>")                
        }
        
        htmlString = paste0(htmlString, "\n")
        
        # Make the rower (first column)
        if(ATypeRow == TRUE){
            htmlString = paste0(htmlString, "                        <td class=\"tableRower tableRower_A\">",as.character(myDF[i,1]),"</td>")    
        }
        else{
            htmlString = paste0(htmlString, "                        <td class=\"tableRower tableRower_B\">",as.character(myDF[i,1]),"</td>")    
        }
        htmlString = paste0(htmlString, "\n")
        
        # Make the cells after the rower
        for(j in 2:totalColumns){
        
            if(ATypeRow == TRUE){
                htmlString = paste0(htmlString, "                        <td class=\"tableCell tableCell_A\">",as.character(myDF[i,j]),"</td>")                
            }
            else{
                htmlString = paste0(htmlString, "                        <td class=\"tableCell tableCell_B\">",as.character(myDF[i,j]),"</td>")                
            }
            htmlString = paste0(htmlString, "\n")
            
        }
        
        htmlString = paste0(htmlString, "                    </tr>")    
        htmlString = paste0(htmlString, "\n")
        
        # Update the counter for the mod4 rows colors
        ACounter = ACounter + 1
        if(ACounter == 5){
            ACounter = 1
            ATypeRow = !ATypeRow 
        }
      
    }
    
    # Close the opened brackets
    htmlString = paste0(htmlString, "            </table>")    
    htmlString = paste0(htmlString, "\n")    
    htmlString = paste0(htmlString, "        </div>")        
    htmlString = paste0(htmlString, "\n")

    
    # -- The overall container for the 3 parts (close)
    htmlString = paste0(htmlString, "        </div>")    
    htmlString = paste0(htmlString, "\n")
    
    # Write HTML file into disk if the user wants to
    #if(!is.null(HTMLPath)) writeLinesDisk(htmlString, htmlFilePath)
    
    return(htmlString)
    
}

# Generate a simple HTML code with an image inside
generateHTMLSimpleImage <- function( imageFilePath, imageWidthPrt = 50,
                                     imageTitle = "", imageSubtitle = "",
                                     overrideImageName = NULL){
    
    # Prepare the table name if needed
    {
        # Init
        myImageName = getFileName(imageFilePath)
        
        # -- If you need to override your table name, then do it
        if(!is.null(overrideImageName)) myImageName = overrideImageName
    }

    # The final string comes here
    htmlString = ""

    # The image has 2 parts
    # -- The overall container for the 3 parts (open)
    htmlString = paste0(htmlString, "        <div class=\"image-overall\" id=\"",myImageName,"\">")    
    htmlString = paste0(htmlString, "\n")
    # ---- The little box with the table description
    htmlString = paste0(htmlString, "            <div class=\"image-description-box\">")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                <div class=\"image-title\">")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                    <p>",imageTitle,"</p>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                </div>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                <div class=\"image-verbose\">")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                    <p>", imageSubtitle,"</p>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "                </div>")    
    htmlString = paste0(htmlString, "\n")
    htmlString = paste0(htmlString, "            </div>")    
    htmlString = paste0(htmlString, "\n")    
    # ---- The actual image code
    htmlString = paste0(htmlString, "            <img src\"",imageFilePath,"\" width=\"",imageWidthPrt,"%\" />")    
    htmlString = paste0(htmlString, "\n") 
    
        
    # -- The overall container for the 2 parts (close)
    htmlString = paste0(htmlString, "        </div>")    
    htmlString = paste0(htmlString, "\n")
 
    return(htmlString)
       
}












# Write an HTML string into the given folder, with the given filename
writeHTMLReport <- function(reportString, destinationFolder, HTMLFilename){

    finalPath = file.path(paste(destinationFolder, HTMLFilename, sep = ""))        
    
    writeLinesDisk(reportString, finalPath)    
}

# Generate an empty web for results, where the main panel is empty and you
# just need to start filling it with HTML code
generateEmptyResults <- function(){
    
    # Read the basic empty web content
    indexHTMLString = EMPTY_WEB_HTML_CONTENT

    # Change the contents
    {
        # Change the content of the head
            
        #     Basic Head information
        indexHTMLString = str_replace( indexHTMLString, HEAD_KEY_STRING,          HEAD_HTML_CONTENT)
        
        # Change the content of the body
        
        #     Change the key left column for the real left column
        indexHTMLString = str_replace( indexHTMLString, LEFT_COLUMN_KEY_STRING,   LEFT_HTML_CONTENT)
        #         The about code
        indexHTMLString = str_replace( indexHTMLString, ABOUT_KEY_STRING,         ABOUT_HTML_CONTENT)
        #         The topic code
        indexHTMLString = str_replace( indexHTMLString, TOPICS_KEY_STRING,        TOPIC_HTML_CONTENT)
        #     Change the key center column for the real center column
        indexHTMLString = str_replace( indexHTMLString, CENTER_COLUMN_KEY_STRING, EMPTY_CENTER_HTML_CONTENT)
    }

    return(indexHTMLString)

}

# Swap the empty result string with the actual HTML string
# leaveOpen = TRUE will leave the empty content key at the end of the string, so
#             you fill it with more content later.
replaceEmptyResults <- function(emptyString, resultsString, leaveOpen = FALSE){
    
    newString = ""
    
    if(leaveOpen == TRUE){
        openReplacement = paste0(resultsString, "\n", CENTER_CONTENT_KEY_STRING)
        newString       = str_replace( emptyString, CENTER_CONTENT_KEY_STRING, openReplacement)
    }
    else{
        newString = str_replace( emptyString, CENTER_CONTENT_KEY_STRING, resultsString)        
    }
    
    return(newString)
    
}


# Generate an index with internal linking ready