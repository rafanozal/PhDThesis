# -----------------------------------------------------------------------------
# Set the global constant for analysis
#
# ie
#
# - How much is the p-value significant?
# - The color of things
# - Folder and files names
# -----------------------------------------------------------------------------

# Load needed libraries
# -----------------------------------------------------------------------------
library(lubridate)     # as_datetime for Sys.time()
library(RColorBrewer)  # brewing color functions

# Set working directory to file location
# -----------------------------------------------------------------------------
{

    # R, and RStudio, absolutely sucks when it comes to handling the getting and
    # setting of the working directory. I tried one million things, and nothing works
    # because it keeps changing with every update.
    #
    # So, this is bad programming, where here you have the absolute folder where
    # the project is located with respect this constants.R script.
    #
    # YOU NEED TO SOURCE THIS SCRIPT AS THE FILE LOCATION MANUALLY ( AND I HATE
    # THAT BECAUSE THERE IS NO WAY TO DO THIS RELIABLE VIA CODING!).
    #
    # Everything else hangs from this folder
    
    # THIS, DOESN'T, WORK, CONSISTENLY:
    
    # Set working directory to file location
    # -----------------------------------------------------------------------------
    # this.dir = dirname(parent.frame(2)$ofile)
    # setwd(this.dir)
    
    # 
    #
    # WRITE HERE WHERE YOUR ROOT FOLDER IS LOCATED
    #
    #
    
    MAIN_PROJECT_FOLDER = file.path("/home/user/Desktop/root")
    setwd(MAIN_PROJECT_FOLDER)
    
    MAIN_CODE_FOLDER    = file.path(paste0(MAIN_PROJECT_FOLDER, "src/"))
    
}

# Change language to english.
# MyOS is completely set on english, but R think it is more clever than me
# and I want things to run in Spanish. I HATE ALL AUTOMATIC THINGS!!
# -----------------------------------------------------------------------------
Sys.setlocale("LC_ALL", "en_GB.UTF-8")

# Timing stuff
# -----------------------------------------------------------------------------
{
  RIGHT_NOW        = gsub("-","",gsub(" ","_",gsub(":","",as_datetime(Sys.time()))))
}


# Set Global Variables
# -----------------------------------------------------------------------------
{

  # Random date for date calculations
  #    ( This is use mostly to calculate the age at time of test
  #    Giving the age of the test is not fully correct, as there
  #    is a significant difference between two 4 year old childs
  #    that are 4 years a 1 day, and 4 years and 360 days. )
  # REFERENCE_DATE <- as.Date("2017-01-01")

  # Networks names
  NETWORK_NAMES  = c("Overall", "Physical", "School", "Sports", "Home", "Other")
  TOTAL_NETWORKS = length(NETWORK_NAMES)
  
  # Colors
  {
    
    # Use this to generate colors autimatically from a given pallette
    # myPalette       = colorRampPalette(brewer.pal(5, "RdYlGn"))
    # colorVectorEdge = myPalette(5)
      
    # General abstract colors
    
    # ---- Conceptual colors
    COLOR_FRIENDSHIP      = "#E8D13A"
      
    # ---- Missing and unknown data
    COLOR_NA              = "#7F7F7F"
    COLOR_UNKNOWN         = "#7F7F7F"

    # ---- General High to None scale, High is better
    COLOR_NONE             = "#D7191C"
    COLOR_LOW              = "#FDAE61"
    COLOR_MEDIUM           = "#FFFFBF"
    COLOR_HIGH             = "#ABDDA4"
    
    # ---- Generic two groups, Group B is better
    colorGroupA           = "#f8766d"
    colorGroupB           = "#00bfc4"
    
    # ---- Specific shades of a given color
    COLOR_WHITE           = "#ffffff"
    COLOR_RED_LOW         = "#fdd49e"
    COLOR_RED_MED         = "#fc8d59"
    COLOR_RED_HIG         = "#d7301f"
    COLOR_BLUE_LOW        = "#d0d1e6"
    COLOR_BLUE_MED        = "#74a9cf"
    COLOR_BLUE_HIG        = "#0570b0"
    
    # Specific colors for specific data
    
    # ---- Sex
    COLOR_MAN             = "#01C0DB"
    COLOR_MAN_LOW         = "#9ED3DB"
    COLOR_WOMAN           = "#EF35AE"
    COLOR_WOMAN_LOW       = "#EFC6E1"
    
    # ---- Smoke status
    COLOR_SMOKE_NEVER     = "#B7C68B"   
    COLOR_SMOKE_SOMETIMES = "#F4F0CB"
    COLOR_SMOKE_DAILY     = "#FF6666"

    # ---- Carrier status
    COLOR_CARRIER         = "#F77FEE"   # (Purple, typical dye for S.Aureus)
    COLOR_NON_CARRIER     = "#C5FAD9"   # (Green, because contrast purple)

    # --- BMI
    COLOR_BMI_UNDERWEIGHT  = "#FFFFBF"  # (Yellow, not so good)
    COLOR_BMI_HEALTHY      = "#ABDDA4"  # (Green, better)
    COLOR_BMI_OVERWEIGHT   = "#FDAE61"  # (Yellow, not so good)
    COLOR_BMI_OBESE        = "#D7191C"  # (Red, bad)

    # --- Pain
    COLOR_PAIN_RESISTANCE   = "#f2b33d"
    COLOR_PAIN_NORESISTANCE = "#9bdf38"
    
    # --- Health
    COLOR_HEALTH_VERY_BAD  = "#D7191C"
    COLOR_HEALTH_BAD       = "#FDAE61"
    COLOR_HEALTH_MEDIUM    = "#FFFFBF"
    COLOR_HEALTH_GOOD      = "#A6D96A"
    COLOR_HEALTH_EXCELLENT = "#1A9641"
    COLOR_HEALTH_UNKNOWN   = COLOR_UNKNOWN
    
    
    # --- Solarium visit
    COLOR_SOLARIUM   = "#FFD266"
    COLOR_NOSOLARIUM = "#5C1700"

    
    # --- Skin Color
    FAIR_SKIN = "#FFC6C0"
    DARK_SKIN = "#764744"
    
    # Generic vectors of colors
    # It puts the previous colors into predifined vectors. Plotting functions
    # needs the vectors as argument, not the individual colors.
    COLOR_INTERVAL_PVALUES = c(COLOR_WHITE, COLOR_RED_LOW, COLOR_RED_MED, COLOR_RED_HIG,
                               COLOR_BLUE_HIG, COLOR_BLUE_MED, COLOR_BLUE_LOW, COLOR_WHITE)
    COLOR_VECTOR_ONE       = c(COLOR_NA)
    COLOR_VECTOR_SEX       = c(COLOR_MAN, COLOR_WOMAN)
    COLOR_VECTOR_SMOKE     = c(COLOR_SMOKE_NEVER, COLOR_SMOKE_SOMETIMES, COLOR_SMOKE_DAILY, COLOR_UNKNOWN)
    COLOR_VECTOR_SNUFF     = c(COLOR_SMOKE_NEVER, COLOR_SMOKE_SOMETIMES, COLOR_SMOKE_DAILY, COLOR_UNKNOWN)
    COLOR_VECTOR_CARRIER   = c(COLOR_CARRIER, COLOR_NON_CARRIER)
    COLOR_VECTOR_SPORTS    = c(COLOR_NONE, COLOR_LOW, COLOR_MEDIUM, COLOR_HIGH, COLOR_UNKNOWN)
    COLOR_VECTOR_BMI       = c(COLOR_BMI_UNDERWEIGHT, COLOR_BMI_HEALTHY, COLOR_BMI_OVERWEIGHT, COLOR_BMI_OBESE, COLOR_UNKNOWN)
    COLOR_VECTOR_HEALTH    = c(COLOR_HEALTH_VERY_BAD, COLOR_HEALTH_BAD, COLOR_HEALTH_MEDIUM, COLOR_HEALTH_GOOD, COLOR_HEALTH_EXCELLENT, COLOR_HEALTH_UNKNOWN)
    COLOR_VECTOR_SOLARIUM  = c(COLOR_SOLARIUM, COLOR_NOSOLARIUM)
    COLOR_VECTOR_SKIN      = c(FAIR_SKIN, DARK_SKIN)
    
    myPalette = colorRampPalette(brewer.pal(8, "Spectral"))
    HIGHSCHOOL_COLOR_VECTOR  = myPalette(8)    
    
    COLOR_VECTOR_SAME_RELATIONSHIP = c(COLOR_RED_HIG,COLOR_UNKNOWN)
    
    myPurplePalette = colorRampPalette(brewer.pal(9, "Purples"))
    COLOR_VECTOR_PURPLE_5  = myPurplePalette(9)[3:7]
    COLOR_VECTOR_PURPLE_3  = myPurplePalette(9)[c(3,5,7)]
    
            
  }

  # Intervals
  INTERVAL_PVALUES = c(-0.05, -0.01, -0.001, 0, +0.001, +0.01, +0.05)

  # How many categories do you want to analyze as maximum for each column
  # If a column have more than this categories, we will skip it in the analysis
  # where we have to break down by categories
  # For example, if you run all the p-values against all the p-values, you will
  # need to do 16! combinations, which is way too much.
  CATEGORY_LIMIT = 12

  # Which layouts do you want to plots
  # There is no good layout by default. You need to run all layout manually
  # until you are satisfy with how it looks like. After some testing, this
  # layout is what it looks best. However if you want to try any other layout
  # you have all of them in ALL_LAYOUT variable (next)
  DO_THIS_LAYOUTS = c("mds")
  
  # Keep track off all layouts that you can do
  ALL_LAYOUTS = c('grid','star','circle','gem', 'dh', 'graphopt', 'mds', 'fr', 'kk', 'drl', 'lgl')
 
}


# Folders to write analysis results and reports
# -----------------------------------------------------------------------------
{
  
    # Folders where we write reports
    {
        # Main folder
        REPORTS_FILEPATH          = file.path(paste(MAIN_PROJECT_FOLDER,  "reports/",      sep = ""))
        #     /Web
        REPORTS_WEB_FILEPATH      = file.path(paste(REPORTS_FILEPATH,     "Web/",          sep = ""))
        #        /HTML
        REPORTS_WEB_HTML_FILEPATH = file.path(paste(REPORTS_WEB_FILEPATH, "HTML/",         sep = ""))        
        
    }

    # Folders where we save main results from the analysis
    {
    
        # Main folders
        RESULT_FOLDER        = file.path(paste(MAIN_PROJECT_FOLDER,"results/",        sep = ""))
    
        GENERAL_FOLDER       = file.path(paste(RESULT_FOLDER, "general/",              sep = ""))
        NETWORK_FOLDER       = file.path(paste(RESULT_FOLDER, "network/",              sep = ""))
        AUREUS_FOLDER        = file.path(paste(RESULT_FOLDER, "aureus/",               sep = ""))
        CONTROL_FOLDER       = file.path(paste(RESULT_FOLDER, "control/",              sep = ""))
        HORMONAL_FOLDER      = file.path(paste(RESULT_FOLDER, "hormonal/",             sep = ""))
        DRUGS_FOLDER         = file.path(paste(RESULT_FOLDER, "drugs/",                sep = ""))
        ANTROPOMETRIC_FOLDER = file.path(paste(RESULT_FOLDER, "antropometry/",         sep = ""))
        AUTOMATIC_FOLDER     = file.path(paste(RESULT_FOLDER, "automatic/",            sep = ""))
        #PAPER_FOLDER         = file.path(paste(RESULT_FOLDER, "paper/",                sep = ""))
        LOGS_FOLDER          = file.path(paste(RESULT_FOLDER, "logs/",                 sep = ""))
        BIOMARKERS_FOLDER    = file.path(paste(RESULT_FOLDER, "biomarkers/",           sep = ""))
        VITAMIND_FOLDER      = file.path(paste(RESULT_FOLDER, "vitaminD/",             sep = ""))
        HIGHSCHOOL_FOLDER    = file.path(paste(RESULT_FOLDER, "highschools/",          sep = ""))
        
        # The control folder has two subfolders, one for the automatic control,
        # and another one for the manual control
    
        AUTOMATIC_CONTROL_FOLDER = file.path(paste(CONTROL_FOLDER, "automatic/",       sep = ""))
        MANUAL_CONTROL_FOLDER    = file.path(paste(CONTROL_FOLDER, "manual/",          sep = ""))
    
        
        # ---- Subfolders
        
        #              /Images goes here
        NETWORK_FOLDER_IMAGES                          = file.path(paste(NETWORK_FOLDER, "images/",                        sep = ""))
        #              /Tables goes here
        NETWORK_FOLDER_TABLES                          = file.path(paste(NETWORK_FOLDER, "tables/",                        sep = ""))
        
        
        
        
        
        #          /VitaminD
        #              /General description of population
        VITAMIND_FOLDER_GENERAL                         = file.path(paste(VITAMIND_FOLDER, "General/",                       sep = ""))
        #              /Images goes here
        VITAMIND_FOLDER_IMAGES                          = file.path(paste(VITAMIND_FOLDER, "images/",                        sep = ""))
        #                  /Everything related to network graphs
        VITAMIND_FOLDER_NETWORK                         = file.path(paste(VITAMIND_FOLDER_IMAGES,  "network/",               sep = ""))
        VITAMIND_GRAPH_GRID                             = file.path(paste(VITAMIND_FOLDER_NETWORK, "allGraphs.png",          sep = ""))
        #                  /Everything related to ethnicities
        VITAMIND_FOLDER_IMAGES_ETHNICITY                = file.path(paste(VITAMIND_FOLDER_IMAGES,  "etchnicity/",            sep = ""))
        #                  /Everything related to diet
        VITAMIND_FOLDER_IMAGES_DIET                     = file.path(paste(VITAMIND_FOLDER_IMAGES,  "diet/",                  sep = ""))        
		VITAMIND_FOLDER_IMAGES_DIET_HS                  = file.path(paste(VITAMIND_FOLDER_IMAGES_DIET,  "DietByHighschool/", sep = ""))                
        #                  /Everything related to highschool biases check
        VITAMIND_FOLDER_IMAGES_HS                       = file.path(paste(VITAMIND_FOLDER_IMAGES,  "highschools/",           sep = ""))                        
        #                  /Everything related to popularity
        VITAMIND_FOLDER_IMAGES_POPULARITY               = file.path(paste(VITAMIND_FOLDER_IMAGES,  "popularity/",            sep = ""))                        
        #                  /Everything related to hormonals
        VITAMIND_FOLDER_IMAGES_HORMONAL                 = file.path(paste(VITAMIND_FOLDER_IMAGES,  "hormonal/",              sep = ""))                                
        #                  /Everything related to time series
        VITAMIND_FOLDER_IMAGES_TIMESERIES               = file.path(paste(VITAMIND_FOLDER_IMAGES,  "timeseries/",            sep = ""))                                        
        #                  /Everything related to scatter plots
        VITAMIND_FOLDER_IMAGES_SCATTERPLOTS             = file.path(paste(VITAMIND_FOLDER_IMAGES,  "scatterplots/",          sep = ""))                                        
        
        
        #              /Tables goes here
        VITAMIND_FOLDER_TABLES                          = file.path(paste(VITAMIND_FOLDER, "tables/",                        sep = ""))
        #                  /Everything related to network tables
        VITAMIND_FOLDER_TABLES_NETWORK                  = file.path(paste(VITAMIND_FOLDER_TABLES,  "network/",               sep = ""))
        #                  /Everything related to diseases
        VITAMIND_FOLDER_TABLES_DISEASE                  = file.path(paste(VITAMIND_FOLDER_TABLES,  "disease/",               sep = ""))        
        #                  /Everything related to medicine
        VITAMIND_FOLDER_TABLES_MEDICINE                 = file.path(paste(VITAMIND_FOLDER_TABLES,  "medicine/",              sep = ""))                
        #                  /Everything related to ethnicities
        VITAMIND_FOLDER_TABLES_ETHNICITY                = file.path(paste(VITAMIND_FOLDER_TABLES,  "etchnicity/",            sep = ""))
        #                  /Everything related to diet
        VITAMIND_FOLDER_TABLES_DIET                     = file.path(paste(VITAMIND_FOLDER_TABLES,  "diet/",                  sep = ""))
        #                  /Everything related to sex
        VITAMIND_FOLDER_TABLES_SEX                      = file.path(paste(VITAMIND_FOLDER_TABLES,  "sex/",                   sep = ""))        
        #                  /Everything related to highschool biases check
        VITAMIND_FOLDER_TABLES_HS                       = file.path(paste(VITAMIND_FOLDER_TABLES,  "highschools/",           sep = ""))                        

        
        
        
        #          /Biomarkers
        #              /tables 
        BIOMARKERS_FOLDER_TABLES                        = file.path(paste(BIOMARKERS_FOLDER, "tables/",                       sep = ""))
        #                  /general                (all summary tables)
        BIOMARKERS_FOLDER_TABLES_GENERAL                = file.path(paste(BIOMARKERS_FOLDER, "tables/general/",                   sep = ""))
        #                  /sex                    (sex differences)
        BIOMARKERS_FOLDER_TABLES_SEX                    = file.path(paste(BIOMARKERS_FOLDER, "tables/sex/",                   sep = ""))
        #                  /network                (everything that has to do with the network)
        BIOMARKERS_FOLDER_TABLES_NETWORK                = file.path(paste(BIOMARKERS_FOLDER, "tables/network/",               sep = ""))
        #                  /isolation              (everything that has to do with the isolation analysis)
        BIOMARKERS_FOLDER_TABLES_ISOLATION              = file.path(paste(BIOMARKERS_FOLDER, "tables/isolation/",             sep = ""))            
        #                  /blood                  (everything that has to do with the blood)
        BIOMARKERS_FOLDER_TABLES_BLOOD                  = file.path(paste(BIOMARKERS_FOLDER, "tables/blood/",                 sep = ""))    
        #                 /diseases
        BIOMARKERS_FOLDER_TABLES_DISEASES               = file.path(paste(BIOMARKERS_FOLDER, "tables/diseases/",              sep = ""))    
        #                 /medicines
        BIOMARKERS_FOLDER_TABLES_MEDICINES              = file.path(paste(BIOMARKERS_FOLDER, "tables/medicines/",             sep = ""))        
        #                 /host factors
        BIOMARKERS_FOLDER_TABLES_HOSTFACTORS            = file.path(paste(BIOMARKERS_FOLDER, "tables/hostfactors/",           sep = ""))            
        #                     /sex
        BIOMARKERS_FOLDER_TABLES_HOSTFACTORS_SEX        = file.path(paste(BIOMARKERS_FOLDER, "tables/hostfactors/sex/",       sep = ""))
        #                     /averages
        BIOMARKERS_FOLDER_TABLES_HOSTFACTORS_SEX        = file.path(paste(BIOMARKERS_FOLDER, "tables/hostfactors/averages/",  sep = ""))
        #                  /antropometry
        BIOMARKERS_FOLDER_TABLES_ANTROPOMETRY           = file.path(paste(BIOMARKERS_FOLDER, "tables/antropometry/",          sep = ""))
    
        #              /images
        #                  /general                 (LOD stats, and so on)
        BIOMARKERS_FOLDER_IMAGES_GENERAL                = file.path(paste(BIOMARKERS_FOLDER, "images/general/",               sep = ""))
        #                  /sex                     (sex differences)
        BIOMARKERS_FOLDER_IMAGES_SEX                    = file.path(paste(BIOMARKERS_FOLDER, "images/sex/",                   sep = ""))
        BIOMARKERS_FOLDER_IMAGES_SEX_BOXPLOTS           = file.path(paste(BIOMARKERS_FOLDER, "images/sex/boxplots/",          sep = ""))
        #                  /isolation
        BIOMARKERS_FOLDER_IMAGES_ISOLATION              = file.path(paste(BIOMARKERS_FOLDER, "images/isolation/",          sep = ""))        
        #                  /antropometry           (all density, boxplot, and BMI plots)
        BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY           = file.path(paste(BIOMARKERS_FOLDER, "images/antropometry/",          sep = ""))
        #                      /nohighschool        (all plots that have no stratification)
        BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY_NOHIGHSCHOOL  = file.path(paste(BIOMARKERS_FOLDER, "images/antropometry/nohighschool/", sep = "")) # I hate R, path not ending in / is not a path. This is a clear error in C++ because IS NOT A PATH 
        #                          /men             (all plots that have no stratification for men only)
        BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY_NOHIGHSCHOOL_MEN  = file.path(paste(BIOMARKERS_FOLDER, "images/antropometry/nohighschool/men/", sep = ""))
        #                          /women           (all plots that have no stratification for women only)
        BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY_NOHIGHSCHOOL_WOMEN  = file.path(paste(BIOMARKERS_FOLDER, "images/antropometry/nohighschool/women/", sep = ""))
        #                      /byhighschool        (all plots that have stratification by highschool)
        BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY_BYHIGHSCHOOL  = file.path(paste(BIOMARKERS_FOLDER, "images/antropometry/byhighschool/", sep = ""))
        #                          /men             (all plots that have no stratification for men only)
        BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY_BYHIGHSCHOOL_MEN  = file.path(paste(BIOMARKERS_FOLDER, "images/antropometry/byhighschool/men/", sep = ""))  # I HATE R, NO FUCKING WARNING FOR DUPLICATED VARIABLES!!
        #                          /women           (all plots that have no stratification for women only)
        BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY_BYHIGHSCHOOL_WOMEN  = file.path(paste(BIOMARKERS_FOLDER, "images/antropometry/byhighschool/women/", sep = ""))
        #                  /blood                  (all blood plots)
        BIOMARKERS_FOLDER_IMAGES_BLOOD                  = file.path(paste(BIOMARKERS_FOLDER, "images/blood/",                 sep = ""))
        #                  /network                (everything that has to do with the network)
        BIOMARKERS_FOLDER_IMAGES_NETWORK                = file.path(paste(BIOMARKERS_FOLDER, "images/network/",               sep = ""))
        BIOMARKERS_GRAPH_GRID                           = file.path(paste(BIOMARKERS_FOLDER_IMAGES_NETWORK, "allGraphs.png",  sep = ""))
        #                  /regression_biofriends  (For when you study biomarkesr againts friends)
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS  = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/", sep = ""))
        #                     /no HS stratificiation
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_NOHS  = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/noHSstratify/", sep = ""))
        #                     /HS stratificiation
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_NOHSYESSEX = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/noHSyesSexStratify/", sep = ""))
        #                     /HS and sex stratification
        #                         /HS1
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS1 = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/allHSStratify/hs1/", sep = ""))
        #                         /HS2
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS2 = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/allHSStratify/hs2/", sep = ""))
        #                         /HS3
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS3 = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/allHSStratify/hs3/", sep = ""))
        #                         /HS4
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS4 = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/allHSStratify/hs4/", sep = ""))
        #                         /HS5
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS5 = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/allHSStratify/hs5/", sep = ""))
        #                         /HS6
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS6 = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/allHSStratify/hs6/", sep = ""))
        #                         /HS7
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS7 = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/allHSStratify/hs7/", sep = ""))
        #                         /HS8
        BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS8 = file.path(paste(BIOMARKERS_FOLDER, "images/regression_biofriends/allHSStratify/hs8/", sep = ""))
        #                 /host factors
        #                     /sex
        BIOMARKERS_FOLDER_IMAGES_HOSTFACTORS_SEX        = file.path(paste(BIOMARKERS_FOLDER, "images/hostfactors/sex/",        sep = ""))
        #                     /levels
        BIOMARKERS_FOLDER_IMAGES_HOSTFACTORS_LEVELS     = file.path(paste(BIOMARKERS_FOLDER, "images/hostfactors/levels/",     sep = ""))
    
        # -------- /Latex
        #          Relative base path to result folder with all tables and images
        #          When generating the latex files, those .tex files need to
        #          refer to the result folder. The .tex files are in another
        #          independent folder, so these are the filepath route from the
        #          latex folder to the result folder.
        LATEX_RELATIVE_BASE_PATH                        = file.path("../../../../results",     sep = "")
        LATEX_RELATIVE_BASE_PATH2                       = file.path("../../../results",        sep = "")
        LATEX_RELATIVE_BASE_PATH3                       = file.path("../../results",           sep = "")
        
        #              /vitamin D 
        LATEX_RELATIVE_VITAMIND                         = file.path(paste(LATEX_RELATIVE_BASE_PATH2,              "vitaminD/",  sep = ""))
        #                  /images
        LATEX_RELATIVE_VITAMIND_FOLDER_IMAGES           = file.path(paste(LATEX_RELATIVE_VITAMIND,                "images/",    sep = ""))
        #                      /network
        LATEX_RELATIVE_VITAMIND_FOLDER_IMAGES_NETWORK   = file.path(paste(LATEX_RELATIVE_VITAMIND_FOLDER_IMAGES,  "network/",   sep = ""))
        
        #              /general 
        LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_GENERAL = file.path(paste(LATEX_RELATIVE_BASE_PATH,  "biomarkers/images/general/", sep = ""))
        #              /antropometry 
        LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY = file.path(paste(LATEX_RELATIVE_BASE_PATH,  "biomarkers/images/antropometry/", sep = ""))
        #              /logs
        LATEX_RELATIVE_FOLDER_LOGS                        = file.path(paste(LATEX_RELATIVE_BASE_PATH2, "logs/",                      sep = ""))
        #              /network
        LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_NETWORK   = file.path(paste(LATEX_RELATIVE_BASE_PATH,  "biomarkers/images/network/", sep = ""))
        #              /summaries
        LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_SUMMARIES = file.path(paste(LATEX_RELATIVE_BASE_PATH,  "biomarkers/tables/summaries/", sep = ""))
    
  }

}


# Prepare logs and results files summaries
#
# These are static plots and tables that we know before hand that we are going
# to generate. For example how many edges were lost in the data cleaning
# process. In contrast, the plots paths for each column for each table is
# generated on the fly depending on the table and column name.
# -----------------------------------------------------------------------------
{
  # Main logs
  #csvPath  = file.path(paste(LOGS_FOLDER, "resultData.csv", sep = "")) # I don't remember what this what for :(
  LOG_PATH = file.path(paste(LOGS_FOLDER, "log.txt", sep = ""))

  # Data cleaning
  CLEANING_LOG_PATH                    = file.path(paste(LOGS_FOLDER,         "cleaningLog.txt",    sep = ""))
  PREANALYSIS_LOG_PATH                 = file.path(paste(LOGS_FOLDER,         "preAnalysisLog.txt", sep = ""))
  
  #CLEANING_LOG_LATEX_TABLE             = file.path(paste(LATEX_FOLDER_TABLES, "cleaningLog.tex", sep = ""))
  #CLEANING_MISSING_FRIENDS_BOXPLOT     = file.path(paste(LOGS_FOLDER,         "Boxplot_Cleaning_MissingFriends.png",     sep = ""))
  #CLEANING_MISSING_NOMINATIONS_BOXPLOT = file.path(paste(LOGS_FOLDER,         "Boxplot_Cleaning_MissingNominations.png", sep = ""))
  
  # Data filtering
  FILTERING_LOG_PATH                    = file.path(paste(LOGS_FOLDER,         "filteringLog.txt", sep = ""))
  #FILTERING_LOG_LATEX_TABLE             = file.path(paste(LATEX_FOLDER_TABLES, "filteringLog.tex", sep = ""))
  #FILTERING_MISSING_FRIENDS_BOXPLOT     = file.path(paste(LOGS_FOLDER,         "Boxplot_Filtering_MissingFriends.png",     sep = ""))
  #FILTERING_MISSING_NOMINATIONS_BOXPLOT = file.path(paste(LOGS_FOLDER,         "Boxplot_Filtering_MissingNominations.png", sep = ""))

  # Log for each analysis
  # (Not use at the moment, but to be use later)
  GENERAL_LOG_PATH = file.path(paste(LOGS_FOLDER, "generalLog.txt", sep = ""))
  NETWORK_LOG_PATH = file.path(paste(LOGS_FOLDER, "networkLog.txt", sep = ""))
  AUREUS_LOG_PATH  = file.path(paste(LOGS_FOLDER, "aureusLog.txt",  sep = ""))
  
  # Control data
  
  #CONTROL_CATEGORICAL_CSV = file.path(paste(AUTOMATIC_CONTROL_FOLDER, "categorical.csv", sep = ""))
  #CONTRO_NUMERICAL_CSV    = file.path(paste(AUTOMATIC_CONTROL_FOLDER, "numerical.csv", sep = ""))

  # Automatic data
  #PVALUESCSV = file.path(paste(AUTOMATIC_FOLDER, "allPValues.csv", sep = ""))
  #PVALUESTXT = file.path(paste(AUTOMATIC_FOLDER, "allPValues.txt", sep = ""))

  # Network data
  # ---- Basic statistics
  #NETWORKSTATSCSV = file.path(paste(NETWORK_FOLDER, "networkStats.csv", sep = ""))

}


# Prepare the data path for where do we find the data
# -----------------------------------------------------------------------------
{
    # ---- Set folder where the data is
    {
        
        # Main data folder
        DATA_FOLDER            = file.path(paste(MAIN_PROJECT_FOLDER, "data/",          sep = ""))
        
        # Original data
        DATA_ORIGINAL          = file.path(paste(DATA_FOLDER,         "originalData/csv/",  sep = ""))

        # Data transformed and clean
        DATA_READY_FOLDER      = file.path(paste(DATA_FOLDER,         "cleanData/", sep = ""))
        DATA_FILTER_FOLDER     = file.path(paste(DATA_FOLDER,         "filtersData/",   sep = ""))
        
        # Metadata with topic information
        #   This cointain things such as blood variables complete names,
        #   biomarkers LOD, biomarkers uniprot website, and so on. If you want
        DATA_META_FOLDER      = file.path(paste(DATA_FOLDER,         "metaData/", sep = ""))
                
        # Fake data
        #   This is autogenerated synthetic data that has absolutely nothing to
        #   do with the original patients
        DATA_FAKE_FOLDER     = file.path(paste(DATA_FOLDER,         "fakeData/",   sep = ""))        
    }
  
    # ---- Set the filenames
    {
        # For the original files
        {
            AUREUS_FILENAME      = "saureus_19022020.csv"
            BIOMARKERS_FILENAME  = "eutro_rafael_paakoblet.csv"
            HORMONAL_FILENAME    = "data_ut_11Juni2019.csv"
            REPORT_FILENAME      = "repor20211005.csv"
            FF2ANTRO_FILENAME    = "Perskey_FF2 antropometri.csv"
        }

    }
  
    # ---- Set the file paths
    {
        # For the original files
        {
            AUREUS_FILEPATH     = file.path(paste(DATA_ORIGINAL, AUREUS_FILENAME,     sep = ""))
            HORMONAL_FILEPATH   = file.path(paste(DATA_ORIGINAL, HORMONAL_FILENAME,   sep = ""))
            BIOMARKERS_FILEPATH = file.path(paste(DATA_ORIGINAL, BIOMARKERS_FILENAME, sep = ""))
            REPORT_FILEPATH     = file.path(paste(DATA_ORIGINAL, REPORT_FILENAME,     sep = ""))
            FF2ANTRO_FILEPATH   = file.path(paste(DATA_ORIGINAL, FF2ANTRO_FILENAME,   sep = ""))
        }

    }
    
    # ---- Special files with metasummaries (variables names, limits, and so on)
    {
        
        BIOMARKERS_METATABLE_FILENAME       = "biomarkers/metabiomarkers.csv"
        BIOMARKERS_METATABLE_FILEPATH       = file.path(paste(DATA_META_FOLDER, BIOMARKERS_METATABLE_FILENAME,       sep = ""))

        BIOMARKERS_REFERENCESTABLE_FILENAME = "biomarkers/references.csv"
        BIOMARKERS_REFERENCESTABLE_FILEPATH = file.path(paste(DATA_META_FOLDER, BIOMARKERS_REFERENCESTABLE_FILENAME, sep = ""))
        
        BLOOD_METATABLE_FILENAME            = "blood/bloodTable.csv"
        BLOOD_METATABLE_FILEPATH            = file.path(paste(DATA_META_FOLDER, BLOOD_METATABLE_FILENAME,            sep = ""))
        
    }
    
}

# Strings that are common to things
# -----------------------------------------------------------------------------
{
    
    UNKNOWN_VARIABLES = c("Unknown", "Didn't Awnser", "Didn't Answered", NA)
    
}
