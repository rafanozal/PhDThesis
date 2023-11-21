# This script generate the basic structure of the HTML files
#
#     Create the main index, using the pieces specified in HTMLConstants
#
#     Create the biomarkers subindexes. The content of each of the biomarkers
#     is later generated in the biomarkers scripts
#
#     Create the STAPH subindex. The content is later on generated as well
#

source( paste0(MAIN_CODE_FOLDER, "/HTML/HTMLConstants.R"), encoding="utf-8")

# Generate the basic Index website
{
    
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
        indexHTMLString = str_replace( indexHTMLString, CENTER_COLUMN_KEY_STRING, DEFAULT_CENTER_HTML_CONTENT)
    }
    
    # Write the whole string into disk
    #
    # R is a stupid language, there is no basic way to write /n/t and everything else into disk -_-
    writeLinesDisk(indexHTMLString, MAIN_INDEX_WEB_FILEPATH)
    
    
}

# Generate the biomarkers menu
{
    
}


# Generate the biomarkers menu
if(FALSE)
{
    
    # Read the basic empty web content
    indexHTMLString = EMPTY_WEB_HTML_CONTENT

    # Change the contents
    {
        # Change the content of the head
            
        #     Basic Head information
        indexHTMLString = str_replace( indexHTMLString, HEAD_KEY_STRING,         HEAD_HTML_CONTENT)
        
        # Change the content of the body
        
        # Left column
        #     Change the key left column for the real left column
        indexHTMLString = str_replace( indexHTMLString, LEFT_COLUMN_KEY_STRING,  LEFT_HTML_CONTENT)
        #         The about code
        indexHTMLString = str_replace( indexHTMLString, ABOUT_KEY_STRING,        ABOUT_HTML_CONTENT)
        #         The topic code
        indexHTMLString = str_replace( indexHTMLString, TOPICS_KEY_STRING,       TOPIC_HTML_CONTENT)
        
        # Middle column
        #     Change the key middle column for the real left column
        indexHTMLString = str_replace( indexHTMLString, MIDDLE_COLUMN_KEY_STRING, MIDDLE_HTML_CONTENT)
        #     Change the key where the submenus are, for each submenu
        #         Prepare the titles
        listOfTitles        = c("Social Network", "Blood Serum", "Host Factors", "Medicines and Diseases")
        matrixOfTopics      = newList(length(listOfTitles))
        matrixOfTopics[[1]] = 
        
        MIDDLE_PANEL_KEY_STRING
        
        generateSidePanel <- function(panelTitle, listOfTopics, listOfLinks){
        
    }
    
    # Write the whole string into disk
    #
    # R is a stupid language, there is no basic way to write /n/t and everything else into disk -_-
    writeLinesDisk(indexHTMLString, MAIN_INDEX_WEB_FILEPATH)
    
    
    }
    
}