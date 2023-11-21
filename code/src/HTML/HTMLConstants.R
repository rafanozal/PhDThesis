# This script contain the HTML constants that we use to generate websites
#
# The constants are somewhere in disk, then they are loaded into this variables
# Upon request, the script make the puzzle of variables and make whatever web
# is requiered.
#
# This part of the script only store the variables

# Import the readr library
library(readr) # R is a horrible language, in the base R, there is no way to read a simple file as a STRING -_-

# Import the main constants and add your own HTML constatns on top of that
source( paste0(MAIN_CODE_FOLDER,"constants.R"), encoding="utf-8")

# -----------------------------------------------------------------------------
# KEY STRINGS
# 
# Here we have the key strings that we need to replace with whatever content
# so the web actually works
# -----------------------------------------------------------------------------
{

    # Main web
    #     HEAD
    HEAD_KEY_STRING = "@@@HEADWEB@@@"
    #     BODY
    #         LEFT COLUMN
    LEFT_COLUMN_KEY_STRING  = "@@@LEFTCOLUMN@@@"
    #             ABOUT
    ABOUT_KEY_STRING        = "@@@ABOUT@@@"
    #             TOPICS
    TOPICS_KEY_STRING       = "@@@TOPICS@@@"
    #         MIDDLE COLUMN
    MIDDLE_COLUMN_KEY_STRING  = "@@@MIDDLECOLUMN@@@"    
    #         CENTER COLUMN
    CENTER_COLUMN_KEY_STRING  = "@@@CENTERCOLUMN@@@"    
    
    #             Individual panels
    MIDDLE_PANEL_KEY_STRING = "@@@SIDEPANELS@@@"
    
    #             Replace this with the actual center content
    CENTER_CONTENT_KEY_STRING = "@@@CENTERCONTENT@@@"
    
    
}

# -----------------------------------------------------------------------------
# FILEPATHS
#
# In here, are the files where we can find the content that compose the constants
# variables for the web
#
# You also find here where the webs are suppose to be saved, which is basically
# into the report folders
# -----------------------------------------------------------------------------
{

    # Folder with the PNG icons
    HTML_RESOURCES_ICONS           = file.path(paste(MAIN_PROJECT_FOLDER,"/res/img/icons/",      sep = ""))
    CSV_ICON_FILEPATH              = file.path(paste(HTML_RESOURCES_ICONS,"csv-file.png",        sep = ""))
    
    # Folder where all the HTML resources are contained
    HTML_RESOURCES_FOLDER          = file.path(paste(MAIN_PROJECT_FOLDER,"/res/HTML/",           sep = ""))

    # Empty web where we build things on top
    #
    #     Index and main web
    EMPTY_WEB_HTML_FILEPATH        = file.path(paste(HTML_RESOURCES_FOLDER, "emptyWeb.html",       sep = ""))
    
    #     Submenu
    #EMPTY_MENU_HTML_FILEPATH       = file.path(paste(HTML_RESOURCES_FOLDER, "emptyMenu.html",    sep = ""))
    
    # Constant pieces of the webs
    #     Head
    HEAD_HTML_FILEPATH             = file.path(paste(HTML_RESOURCES_FOLDER, "head.html",           sep = ""))
    #         Left column  
    LEFT_HTML_FILEPATH             = file.path(paste(HTML_RESOURCES_FOLDER, "leftColumn.html",     sep = ""))
    #             About
    ABOUT_HTML_FILEPATH            = file.path(paste(HTML_RESOURCES_FOLDER, "about.html",          sep = ""))
    #             Topics
    TOPIC_HTML_FILEPATH            = file.path(paste(HTML_RESOURCES_FOLDER, "topics.html",         sep = ""))    
    #         Middle column
    #MIDDLE_HTML_FILEPATH           = file.path(paste(HTML_RESOURCES_FOLDER, "emptyMenu.html",    sep = ""))    
    #         Default Center column
    DEFAULT_CENTER_HTML_FILEPATH    = file.path(paste(HTML_RESOURCES_FOLDER, "defaultCenter.html", sep = ""))        
    #         Empty Center column
    EMPTY_CENTER_HTML_FILEPATH      = file.path(paste(HTML_RESOURCES_FOLDER, "emptyCenter.html",   sep = ""))        
    
    # Where the webs reports are going to be saved
    MAIN_INDEX_WEB_FILENAME       = "index.html"
    MAIN_INDEX_WEB_FILEPATH       = paste0(REPORTS_WEB_HTML_FILEPATH,MAIN_INDEX_WEB_FILENAME)
    
}

# -----------------------------------------------------------------------------
# CONTENTS
#
# In here, we read the previous files, and save the content for later. In other
# script, is where we build the webs dynamically
# -----------------------------------------------------------------------------
{
    
    # Empty web where we build things on top
    EMPTY_WEB_HTML_CONTENT        = read_file(EMPTY_WEB_HTML_FILEPATH)
    
    # Constant pieces of the web
    #     Head
    HEAD_HTML_CONTENT             = read_file(HEAD_HTML_FILEPATH)
    #         Left column
    LEFT_HTML_CONTENT             = read_file(LEFT_HTML_FILEPATH)
    #             About
    ABOUT_HTML_CONTENT            = read_file(ABOUT_HTML_FILEPATH)
    #             Topics
    TOPIC_HTML_CONTENT            = read_file(TOPIC_HTML_FILEPATH)
    #         Middle column
    #MIDDLE_HTML_CONTENT           = read_file(MIDDLE_HTML_FILEPATH)
    #         Middle column
    DEFAULT_CENTER_HTML_CONTENT   = read_file(DEFAULT_CENTER_HTML_FILEPATH)        
    EMPTY_CENTER_HTML_CONTENT     = read_file(EMPTY_CENTER_HTML_FILEPATH)        
    
    
}

