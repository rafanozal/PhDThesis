# -----------------------------------------------------------------------------
#
#   RUN THE CONSTANTS.R SCRIPT FIRST!
#
#   R absolutely sucks and is impossible to set an automatic setwd() from
#   respect the current script running. The $ofile doesn't work!
#
#   There use to be a way to load the source of the .R file as current directory
#   but after some updates you can use that anymore as you get an empty folder
#   when using the $ofile command. Yet another reason to hate R.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#
# This script is for loading the data only
#
# This include unformated text and converted into Date object, creating the
# proper graph object, creating the factors in the dataframes, and in general
# everything that take clean data and need to be put into R memory.
#
# This make possible using something else that is not R, because your data is
# ready to use by anything. Thus is important to make the two process separated
# The most efficient scenario is where you only need to read the data and don't
# have to do any transformation.
#
# If you want to transform the data, use the dataCleaning.R script
#
# If you want to filter out some data the filter.R script
#
# -----------------------------------------------------------------------------


# Add the needed libraries
library(reshape2) 
library(dplyr)
library(lubridate)
library(stringr)

#library(ggraph)
library(igraph)


# load our proper libraries
source(paste0(MAIN_PROJECT_FOLDER,"src/lib/basic/toolsBasic.R"), encoding="utf-8")



# Get all the CSV files in the folder where the filtered data is and  load it.
allCSVFiles  = list.files(DATA_FILTER_FOLDER, full.names = TRUE, pattern = ".csv$")
allINFOFiles = list.files(DATA_FILTER_FOLDER, full.names = TRUE, pattern = ".info$")

# Read the data into DFs
# -----------------------------------------------------------------------------
{
    # Individual tables and complete table
    basicTable            = read.csv2(allCSVFiles[4],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    networkTechnicalTable = read.csv2(allCSVFiles[20], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    antropometricTableFF1 = read.csv2(allCSVFiles[1],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    antropometricTableFF2 = read.csv2(allCSVFiles[2],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    menstruationTable     = read.csv2(allCSVFiles[18], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    aureusTable           = read.csv2(allCSVFiles[3],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    swabbingTable         = read.csv2(allCSVFiles[36], fileEncoding = "UTF-8", stringsAsFactors = FALSE)    
    highSchoolTable       = read.csv2(allCSVFiles[13], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    bloodTable            = read.csv2(allCSVFiles[6],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    bloodTechnicalTable   = read.csv2(allCSVFiles[7],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sociologyTable        = read.csv2(allCSVFiles[32], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    frienshipTable        = read.csv2(allCSVFiles[12], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    pubertyMenTable       = read.csv2(allCSVFiles[27], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    pubertyWomenTable     = read.csv2(allCSVFiles[28], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    drugsTable            = read.csv2(allCSVFiles[11], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sportsTable           = read.csv2(allCSVFiles[35], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    hygieneTable          = read.csv2(allCSVFiles[16], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    biomarkersTable       = read.csv2(allCSVFiles[5],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    dietTable             = read.csv2(allCSVFiles[9],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sleepTable            = read.csv2(allCSVFiles[31], fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    
    # DBs tables  
    diseasesDBDF          = read.csv2(allCSVFiles[10],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    contraceptivesDBDF    = read.csv2(allCSVFiles[8],   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    medicinesDBDF         = read.csv2(allCSVFiles[17],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    
    # Networks
    overallNetworkDF      = read.csv2(allCSVFiles[23],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    physicalNetworkDF     = read.csv2(allCSVFiles[25],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    homeNetworkDF         = read.csv2(allCSVFiles[14],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    schoolNetworkDF       = read.csv2(allCSVFiles[29],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sportsNetworkDF       = read.csv2(allCSVFiles[33],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    otherNetworkDF        = read.csv2(allCSVFiles[21],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
    overallNetworkFF12DF  = read.csv2(allCSVFiles[24],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    physicalNetworkFF12DF = read.csv2(allCSVFiles[26],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    homeNetworkFF12DF     = read.csv2(allCSVFiles[15],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    schoolNetworkFF12DF   = read.csv2(allCSVFiles[30],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sportsNetworkFF12DF   = read.csv2(allCSVFiles[34],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    otherNetworkFF12DF    = read.csv2(allCSVFiles[22],  fileEncoding = "UTF-8", stringsAsFactors = FALSE)    
    
    # Metadata information
    {
        # Biomarkers    
        biomarkersMetadataDF           = read.csv2(BIOMARKERS_METATABLE_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
        colnames(biomarkersMetadataDF) = c("Acronym", "Protein", "UniProt", "LOD_Batch_20160383", "LOD_Batch_20160977", "Uniprt_Web", "Wiki_Web") # Adjust names because of weird spaces in trailing strings
        
        biomarkersReferencesDF           = read.csv2(BIOMARKERS_REFERENCESTABLE_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
        colnames(biomarkersReferencesDF) = c("Acronym", "Source", "Authors", "Link", "Summary")
        biomarkersReferencesDF           = biomarkersReferencesDF[,c(1, 5, 2, 3, 4)]
        biomarkersReferencesDF$Authors   = NULL
        
        
        # Blood
        bloodMetadataDF                = read.csv2(BLOOD_METATABLE_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
        colnames(bloodMetadataDF)      = c("Original", "Description", "Short", "Unit", "MenLower", "MenUpper", "WomenLower", "WomenUpper") # Adjust names because of weird spaces in trailing strings
        bloodMetadataDF$MenLower       = as.numeric(bloodMetadataDF$MenLower)
        bloodMetadataDF$MenUpper       = as.numeric(bloodMetadataDF$MenUpper)
        bloodMetadataDF$WomenLower     = as.numeric(bloodMetadataDF$WomenLower)
        bloodMetadataDF$WomenUpper     = as.numeric(bloodMetadataDF$WomenUpper)
    }

    # How many people we have
    totalOriginalRows = nrow(basicTable) 
    
}

# Fix the column names in each network table
# ( T 3 T ) whyyyy
# -----------------------------------------------------------------------------
{
    totalRows = nrow(overallNetworkDF)
    
    colnames(overallNetworkDF)      = c(c(1:totalRows),"ID")
    colnames(physicalNetworkDF)     = c(c(1:totalRows),"ID")
    colnames(homeNetworkDF)         = c(c(1:totalRows),"ID")
    colnames(schoolNetworkDF)       = c(c(1:totalRows),"ID")
    colnames(sportsNetworkDF)       = c(c(1:totalRows),"ID")
    colnames(otherNetworkDF)        = c(c(1:totalRows),"ID")
      
    colnames(overallNetworkFF12DF)  = c(c(1:totalRows),"ID")
    colnames(physicalNetworkFF12DF) = c(c(1:totalRows),"ID")
    colnames(homeNetworkFF12DF)     = c(c(1:totalRows),"ID")
    colnames(schoolNetworkFF12DF)   = c(c(1:totalRows),"ID")
    colnames(sportsNetworkFF12DF)   = c(c(1:totalRows),"ID")
    colnames(otherNetworkFF12DF)    = c(c(1:totalRows),"ID")  
      
}

# Set up the strings into factors where necessary and also the proper date format
# -----------------------------------------------------------------------------
# ---- Convert dates strings into proper date variables
{
    
    # Why in hell does SPSS has the start of Gregorian Calendar in 1582 as
    # default date for POSIX origin??? There might be a minor error coercion in
    # dates due the fact that we don't know which timezone, or time origin,
    # shall we take as a reference.
    originSPSSDate = as.Date("1582-10-14" , format = "%Y-%m-%d") 
    
    # Basic Table
    {
    
        basicTable$AttendanceDateFF1  = as.Date(basicTable$AttendanceDateFF1)
        basicTable$AttendanceDateFF12 = as.Date(basicTable$AttendanceDateFF12)
        basicTable$MedicationDateFF1  = as.Date(basicTable$MedicationDateFF1)
        basicTable$QuestionaryDateFF1 = as.Date(basicTable$QuestionaryDateFF1)

    }
    
    # Menstruation Table
    {
    
        menstruationTable$Date             = as.Date(menstruationTable$Date)               # This is the date that the questionary was made
        menstruationTable$MenstruationDate = as.Date(menstruationTable$MenstruationDate)   # This is the date of the first menstruation

    }    
    
    # Aureus table
    {
    
        aureusTable$S1_AttendanceDate    = as.Date(aureusTable$S1_AttendanceDate)
        aureusTable$S1_R2_AttendanceDate = as.Date(aureusTable$S1_R2_AttendanceDate)
        aureusTable$S2_AttendanceDate    = as.Date(aureusTable$S2_AttendanceDate)
        aureusTable$S1_CultureDate       = as.Date(aureusTable$S1_CultureDate)
        aureusTable$S2_CultureDate       = as.Date(aureusTable$S2_CultureDate)
        
    }
    
    # Swabbing table
    {
        
        swabbingTable$S1_Nasal_FreezeDate  = as.Date(swabbingTable$S1_Nasal_FreezeDate)
        swabbingTable$S1_Throat_FreezeDate = as.Date(swabbingTable$S1_Throat_FreezeDate)
    }
    
    # Blood table
    {
        bloodTable$BloodAnalysisDate       = as.Date(bloodTable$BloodAnalysisDate)
        bloodTable$PlasmaAnalysisDate      = as.Date(bloodTable$PlasmaAnalysisDate)
    }
    
    # Friendship Table
    {
        
        # In this case, the date was saved as a string (US format! ¬¬) rather than POSIX
        frienshipTable$Created = as.Date(frienshipTable$Created)
            
    }    
    
}

# Add the manual order for the categorical values, so it coincides with the color palette
{
    
    # ALL OF THESE SHOULDN'T BE DONE MANUALLY HERE!
    # This should be in the metadata, which would make for a great C++ class
    # structure, but since R is a stupid language, this is a total chaos!
    #
    # Ideally this:
    #
    # Category    | Order | Default color | Long name | Short name | Number coded | Ontology concept ID
    # --------------------------------
    # Modality A  | 
    # Modality B
    #    ...
    # Modality N
    #
    # Underweight |   1    | yellow1      | BMI Underweight <18     |  Und  | 0 | Ontology ObesityA.2934
    # Healthy     |   2    | green1       | BMI Healthy 18 < 22     |  Heal | 1 | Ontology ObesityA.2945
    # Overweight  |   3    | orange1      | BMI Overweight 22 < 30  |  Over | 2 | Ontology ObesityA.2996
    # Obese       |   4    | red1         | BMI Obese      30 >     |  Ob   | 3 | Ontology ObesityA.2932
    
    # Yes No questions always default to order "No", "Yes", "Unknown".
    # In general, everything negative to the left, everything positive in the
    # middle, everything unknown to the right.
    
    # Basic Table
    {
      
        basicTable$Sex           = factor(basicTable$Sex,           levels = c("Man",      "Woman"                                                       ))  
        basicTable$GeneralHealth = factor(basicTable$GeneralHealth, levels = c("Very bad", "Bad",  "Neither good nor bad", "Good", "Excellent", "Unknown"))  

    }
    
    # Antropometry
    {
    
        antropometricTableFF1$BMICategorical = factor(antropometricTableFF1$BMICategorical , levels = c("Underweight", "Healthy",   "Overweight", "Obese", "Unknown"))    
        antropometricTableFF2$BMICategorical = factor(antropometricTableFF2$BMICategorical , levels = c("Underweight", "Healthy",   "Overweight", "Obese", "Unknown"))    
        
    }
    
    # Menstruation
    {
        
        menstruationTable$MenstruationStart   = factor(menstruationTable$MenstruationStart,   levels = c( "No", "Yes", "Unknown"))
        menstruationTable$MenstruationRegular = factor(menstruationTable$MenstruationRegular, levels = c( "Irregular", "Usually regular", "Always regular", "Unknown"))
        
    }    
    
    # Aureus Table
    {
        # R is horrible,  since we dont' have objects (proper objects and classes!)
        # we can't do self.table(BacterialGrow, levels = "..."), which makes things
        # much more easy and we don't need to add the same column redundancy in the
        # same line. This causes A LOT of bugs and is very frustrating.
        
        # The experiment grew something in the agar plate
        aureusTable$S1_BacterialNasalGrowth  = factor(aureusTable$S1_BacterialNasalGrowth,   levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_BacterialThroatGrowth = factor(aureusTable$S1_BacterialThroatGrowth,  levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_BacterialNasalGrowth  = factor(aureusTable$S2_BacterialNasalGrowth,   levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_BacterialThroatGrowth = factor(aureusTable$S2_BacterialThroatGrowth,  levels = c( "No", "Non-applicable", "Yes", "Unknown"))

        # The experiment grew SA in the agar plate (Direct Culture)
        aureusTable$S1_SA_Direct_NasalGrowth        = factor(aureusTable$S1_SA_Direct_NasalGrowth,      levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_SA_Direct_ThroatGrowth       = factor(aureusTable$S1_SA_Direct_ThroatGrowth,     levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_SA_Direct_NasalPopulation    = factor(aureusTable$S1_SA_Direct_NasalPopulation,  levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S1_SA_Direct_ThroatPopulation   = factor(aureusTable$S1_SA_Direct_ThroatPopulation, levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S2_SA_Direct_NasalGrowth        = factor(aureusTable$S2_SA_Direct_NasalGrowth,      levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_SA_Direct_ThroatGrowth       = factor(aureusTable$S2_SA_Direct_ThroatGrowth,     levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_SA_Direct_NasalPopulation    = factor(aureusTable$S2_SA_Direct_NasalPopulation,  levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S2_SA_Direct_ThroatPopulation   = factor(aureusTable$S2_SA_Direct_ThroatPopulation, levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))

        # The experiment grew SA, and we try to make it grow even more (Enrichment Broth)
        aureusTable$S1_SA_Enrich_NasalGrowth        = factor(aureusTable$S1_SA_Enrich_NasalGrowth,      levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_SA_Enrich_ThroatGrowth       = factor(aureusTable$S1_SA_Enrich_ThroatGrowth,     levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S1_SA_Enrich_NasalPopulation    = factor(aureusTable$S1_SA_Enrich_NasalPopulation,  levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S1_SA_Enrich_ThroatPopulation   = factor(aureusTable$S1_SA_Enrich_ThroatPopulation, levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S2_SA_Enrich_NasalGrowth        = factor(aureusTable$S2_SA_Enrich_NasalGrowth,      levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_SA_Enrich_ThroatGrowth       = factor(aureusTable$S2_SA_Enrich_ThroatGrowth,     levels = c( "No", "Non-applicable", "Yes", "Unknown"))  
        aureusTable$S2_SA_Enrich_NasalPopulation    = factor(aureusTable$S2_SA_Enrich_NasalPopulation,  levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
        aureusTable$S2_SA_Enrich_ThroatPopulation   = factor(aureusTable$S2_SA_Enrich_ThroatPopulation, levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))

        # Coagulase test to check for the presence of SA (positive) or S.Epidermitis or S.Saprophyticus.
        aureusTable$S1_Direct_CoagulaseNasal        = factor(aureusTable$S1_Direct_CoagulaseNasal,      levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S1_Direct_CoagulaseThroat       = factor(aureusTable$S1_Direct_CoagulaseThroat,     levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S1_Enrich_CoagulaseNasal        = factor(aureusTable$S1_Enrich_CoagulaseNasal,      levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S1_Enrich_CoagulaseThroat       = factor(aureusTable$S1_Enrich_CoagulaseThroat,     levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S2_Direct_CoagulaseNasal        = factor(aureusTable$S2_Direct_CoagulaseNasal,      levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S2_Direct_CoagulaseThroat       = factor(aureusTable$S2_Direct_CoagulaseThroat,     levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S2_Enrich_CoagulaseNasal        = factor(aureusTable$S2_Enrich_CoagulaseNasal,      levels = c("Negative", "Non-applicable", "Positive", "Unknown"))
        aureusTable$S2_Enrich_CoagulaseThroat       = factor(aureusTable$S2_Enrich_CoagulaseThroat,     levels = c("Negative", "Non-applicable", "Positive", "Unknown"))

        # The final variables for positives and negatives      
        # Levels are not initialize to Unknown because the default is negative for everyone
        aureusTable$S1_D_NasalColonize              = factor(aureusTable$S1_D_NasalColonize,            levels = c("Negative", "Positive"))
        aureusTable$S1_D_ThroatColonize             = factor(aureusTable$S1_D_ThroatColonize,           levels = c("Negative", "Positive"))
        aureusTable$S1_D_Colonize                   = factor(aureusTable$S1_D_Colonize,                 levels = c("Negative", "Positive"))
        aureusTable$S1_E_NasalColonize              = factor(aureusTable$S1_E_NasalColonize,            levels = c("Negative", "Positive"))
        aureusTable$S1_E_ThroatColonize             = factor(aureusTable$S1_E_ThroatColonize,           levels = c("Negative", "Positive"))
        aureusTable$S1_E_Colonize                   = factor(aureusTable$S1_E_Colonize,                 levels = c("Negative", "Positive"))
            
        aureusTable$S2_D_NasalColonize              = factor(aureusTable$S2_D_NasalColonize,            levels = c("Negative", "Positive"))
        aureusTable$S2_D_ThroatColonize             = factor(aureusTable$S2_D_ThroatColonize,           levels = c("Negative", "Positive"))
        aureusTable$S2_D_Colonize                   = factor(aureusTable$S2_D_Colonize,                 levels = c("Negative", "Positive"))
        aureusTable$S2_E_NasalColonize              = factor(aureusTable$S2_E_NasalColonize,            levels = c("Negative", "Positive"))
        aureusTable$S2_E_ThroatColonize             = factor(aureusTable$S2_E_ThroatColonize,           levels = c("Negative", "Positive"))
        aureusTable$S2_E_Colonize                   = factor(aureusTable$S2_E_Colonize,                 levels = c("Negative", "Positive"))

        aureusTable$D_NasalCarrier                  = factor(aureusTable$D_NasalCarrier,                levels = c("Negative", "Positive"))
        aureusTable$D_ThroatCarrier                 = factor(aureusTable$D_ThroatCarrier,               levels = c("Negative", "Positive"))
        aureusTable$D_Carrier                       = factor(aureusTable$D_Carrier,                     levels = c("Negative", "Positive"))
            
        aureusTable$E_NasalCarrier                  = factor(aureusTable$E_NasalCarrier,                levels = c("Negative", "Positive"))
        aureusTable$E_ThroatCarrier                 = factor(aureusTable$E_ThroatCarrier,               levels = c("Negative", "Positive"))
        aureusTable$E_Carrier                       = factor(aureusTable$E_Carrier,                     levels = c("Negative", "Positive"))
        
    } 
          
    # Swabbing Table
    {
        
        swabbingTable$S1_NasalOK   = factor(swabbingTable$S1_NasalOK,   levels = c(  "Yes", "No", "Unknown"))
        swabbingTable$S1_ThroatOK  = factor(swabbingTable$S1_ThroatOK,  levels = c(  "Yes", "No", "Unknown"))
        
        
        swabbingTable$S1_Performed = factor(swabbingTable$S1_Performed, levels = c(  "Performed", "Performed with irregularities", "Unknown"))
        swabbingTable$S2_Performed = factor(swabbingTable$S2_Performed, levels = c(  "Performed", "Performed with irregularities", "Unknown"))
        
        
        swabbingTable$S1_Event           = factor(swabbingTable$S1_Event,           levels = c("Yes", "No"))
        swabbingTable$S1_Medical_Event   = factor(swabbingTable$S1_Medical_Event,   levels = c("Yes", "No"))
        swabbingTable$S1_Technical_Event = factor(swabbingTable$S1_Technical_Event, levels = c("Yes", "No"))
        swabbingTable$S1_Abort_Event     = factor(swabbingTable$S1_Abort_Event,     levels = c("Yes", "No"))
        swabbingTable$S1_Other_Event     = factor(swabbingTable$S1_Other_Event,     levels = c("Yes", "No"))
        
        swabbingTable$S2_Event           = factor(swabbingTable$S2_Event,           levels = c("Yes", "No"))
        swabbingTable$S2_Medical_Event   = factor(swabbingTable$S2_Medical_Event,   levels = c("Yes", "No"))
        swabbingTable$S2_Technical_Event = factor(swabbingTable$S2_Technical_Event, levels = c("Yes", "No"))
        swabbingTable$S2_Abort_Event     = factor(swabbingTable$S2_Abort_Event,     levels = c("Yes", "No"))
        swabbingTable$S2_Other_Event     = factor(swabbingTable$S2_Other_Event,     levels = c("Yes", "No"))
                
        
        swabbingTable$S1_Nose_Repeated   = factor(swabbingTable$S1_Nose_Repeated,   levels = c(  "Yes", "No", "Non-applicable", "Unknown"))
        swabbingTable$S1_Throat_Repeated = factor(swabbingTable$S1_Throat_Repeated, levels = c(  "Yes", "No", "Non-applicable", "Unknown"))
        
        swabbingTable$S1_Repeated_Event           = factor(swabbingTable$S1_Repeated_Event,  levels = c("Yes", "No"))
        swabbingTable$S1_Repeated_Medical_Event   = factor(swabbingTable$S1_Medical_Event,   levels = c("Yes", "No"))
        swabbingTable$S1_Repeated_Technical_Event = factor(swabbingTable$S1_Technical_Event, levels = c("Yes", "No"))
        swabbingTable$S1_Repeated_Abort_Event     = factor(swabbingTable$S1_Abort_Event,     levels = c("Yes", "No"))
        swabbingTable$S1_Repeated_Other_Event     = factor(swabbingTable$S1_Other_Event,     levels = c("Yes", "No"))        

    }
    

    # Highschool
    {
    
        # The Highschool ID is not a factor varaible, but we need it sorted
        # so the color scheme is consistent across plots
        highSchoolTable$HighSchool = as.factor(highSchoolTable$HighSchool)    
        
        # Same goes for Class and Programs
        
        # Add the C to the Class ID, it just looks better
        highSchoolTable$Class = as.factor(highSchoolTable$Class)    
        
        # Add the P to the Program ID, it just looks better
        # (There is probably a better description for this, but we don't have it)
        highSchoolTable$Programme = as.factor(highSchoolTable$Programme)            
        
        # Add the order to the main program. In this case the order doesn't
        # make sense, but we do it anyway to keep consistency in between plots
        # and subtables. (In alphabetic order)
        highSchoolTable$MainPrograme = factor(highSchoolTable$MainPrograme , levels = c("General", "Sports", "Vocational"))    
        
    }
    

    # Blood
    # (There is nothing here)    
    
    # Blood Technical Table
    {
     
        bloodTechnicalTable$BloodTestPerformed  = factor(bloodTechnicalTable$BloodTestPerformed,  levels = c(  "Performed", "Irregularities", "Not performed"))
        
        bloodTechnicalTable$BloodEvent          = factor(bloodTechnicalTable$BloodEvent,          levels = c("Yes", "No"))
        bloodTechnicalTable$BloodMedicalEvent   = factor(bloodTechnicalTable$BloodMedicalEvent,   levels = c("Yes", "No"))
        bloodTechnicalTable$BloodTechnicalEvent = factor(bloodTechnicalTable$BloodTechnicalEvent, levels = c("Yes", "No"))
        bloodTechnicalTable$BloodAborted        = factor(bloodTechnicalTable$BloodAborted,        levels = c("Yes", "No"))
        bloodTechnicalTable$BloodOther          = factor(bloodTechnicalTable$BloodOther,          levels = c("Yes", "No"))
        
        bloodTechnicalTable$Blood_S25OH_Event   = factor(bloodTechnicalTable$Blood_S25OH_Event,   levels = c("Yes", "No", "Unknown"))
        bloodTechnicalTable$Blood_Retinol_Event = factor(bloodTechnicalTable$Blood_Retinol_Event, levels = c("Yes", "No", "Unknown"))
           
    }
    
    # Sociology
    {
        
        # Who do you live with
        sociologyTable$Live_With_Mother          = factor(sociologyTable$Live_With_Mother,          levels = c("Yes", "No", "Unknown"))
        sociologyTable$Live_With_Father          = factor(sociologyTable$Live_With_Father,          levels = c("Yes", "No", "Unknown"))
        
        sociologyTable$Live_With_Siblings        = factor(sociologyTable$Live_With_Siblings,        levels = c("Zero", "One or Two", "Three or more", "Unknown"))
        
        sociologyTable$Live_With_Stepfather      = factor(sociologyTable$Live_With_Stepfather,      levels = c("Yes", "No", "Unknown"))
        sociologyTable$Live_With_Stepmother      = factor(sociologyTable$Live_With_Stepmother,      levels = c("Yes", "No", "Unknown"))
        sociologyTable$Live_With_Fosterparents   = factor(sociologyTable$Live_With_Fosterparents,   levels = c("Yes", "No", "Unknown"))
        sociologyTable$Live_With_Adoptiveparents = factor(sociologyTable$Live_With_Adoptiveparents, levels = c("Yes", "No", "Unknown"))
        sociologyTable$Live_With_Grandparents    = factor(sociologyTable$Live_With_Grandparents,    levels = c("Yes", "No", "Unknown"))
        sociologyTable$Live_With_Friends         = factor(sociologyTable$Live_With_Friends,         levels = c("Yes", "No", "Unknown"))
        sociologyTable$Live_With_Nobody          = factor(sociologyTable$Live_With_Nobody,          levels = c("Yes", "No", "Unknown"))
        sociologyTable$Live_With_Institution     = factor(sociologyTable$Live_With_Institution,     levels = c("Yes", "No", "Unknown"))
        sociologyTable$Live_With_Other           = factor(sociologyTable$Live_With_Other,           levels = c("Yes", "No", "Unknown"))
        
        # If you don't live at home, when did you leave
        sociologyTable$When_Left_Home            = factor(sociologyTable$When_Left_Home,            levels = c("Less than 6 months", "6 to 11 months", "1 to 2 years", "More than 2 years", "Unknown"))
            
        # What does your parents do
        sociologyTable$Mother_WorkTime           = factor(sociologyTable$Mother_WorkTime,           levels = c("Full time", "Part time", "Unemployed", "Didn't Awnser", "Pensioned", "Deceased", "Don't know"))
        sociologyTable$Father_WorkTime           = factor(sociologyTable$Father_WorkTime,           levels = c("Full time", "Part time", "Unemployed", "Didn't Awnser", "Pensioned", "Deceased", "Don't know"))
        
        sociologyTable$Mother_Studying           = factor(sociologyTable$Mother_Studying,           levels = c("Yes", "No", "Unknown"))
        sociologyTable$Mother_Domestic           = factor(sociologyTable$Mother_Domestic,           levels = c("Yes", "No", "Unknown"))
        sociologyTable$Mother_Disable            = factor(sociologyTable$Mother_Disable,            levels = c("Yes", "No", "Unknown"))
        sociologyTable$Father_Studying           = factor(sociologyTable$Father_Studying,           levels = c("Yes", "No", "Unknown"))
        sociologyTable$Father_Domestic           = factor(sociologyTable$Father_Domestic,           levels = c("Yes", "No", "Unknown"))
        sociologyTable$Father_Disable            = factor(sociologyTable$Father_Disable,            levels = c("Yes", "No", "Unknown"))
        
        # What is their education
        sociologyTable$Mother_Education          = factor(sociologyTable$Mother_Education,          levels = c("Primary School", "High school", "Occupational High school", "College less than 4 years", "College 4 years or more", "Don't know", "Didn't answer"))
        sociologyTable$Father_Education          = factor(sociologyTable$Father_Education,          levels = c("Primary School", "High school", "Occupational High school", "College less than 4 years", "College 4 years or more", "Don't know", "Didn't answer"))

        # Ethnicity, there is no particular order,
        # but we keep Scandinavia on top, Europe next, and whatever is left for the last part in alphabetical
        # This is very important to keep flag order accordingly.
        sociologyTable$Ethnicity                 = factor(sociologyTable$Ethnicity, 
                                                          levels = c("Norwegian",
                                                                     "Norwegian-Sami",
                                                                     "Norwegian-Kven",
                                                                     "Norwegian-Sami-Kven",
                                                                     "Norwegian-Sami-Swedish",
                                                                     "Norwegian-Sami-Icelandic",
                                                                     "Norwegian-Swedish",
                                                                     "Norwegian-Swedish-Dutch",
                                                                     "Norwegian-Finnish",
                                                                     "Norwegian-Danish",
                                                                     "Norwegian-Russian", 
                                                                     "Norwegian-German",
                                                                     "Norwegian-Belgian",
                                                                     "Norwegian-Italian",
                                                                     "Norwegian-Spanish",
                                                                     "Norwegian-Portuguese",
                                                                     "Norwegian-Turquish",
                                                                     "Norwegian-Chinese",
                                                                     "Norwegian-Thai",
                                                                     "Norwegian-Tamil",    
                                                                     "Norwegian-Philipine",
                                                                     "Norwegian-African",
                                                                     "Norwegian-Gambian",
                                                                     "Norwegian-Ghanaian",
                                                                     "Norwegian-Somalian",
                                                                     "Norwegian-Canadian",
                                                                     "Norwegian-Brasilian",
                                                                     "Norwegian-Colombian",
                                                                     "Norwegian-Other",
                                                                     # Norway
                                                                     # North-Norway
                                                                     "Sami",                           
                                                                     "Kven",   
                                                                     # Scandinavia
                                                                     "Swedish",
                                                                     # Russia
                                                                     "Russian",
                                                                     # Europe
                                                                     # -- Central
                                                                     "Polish",
                                                                     "German",     
                                                                     "Dutch", 
                                                                     "Belgian",
                                                                     # -- Easter
                                                                     "Bulgarian",
                                                                     # -- Mediterranean
                                                                     "Italian",
                                                                     # Asia
                                                                     # -- Central
                                                                     "Palestinian",
                                                                     "Afghan",
                                                                     # -- Oriental
                                                                     "Thai",
                                                                     "Tamil",
                                                                     # Africa
                                                                     "African",
                                                                     # -- Central
                                                                     "Eritrean",
                                                                     # North America
                                                                     "Canadian",
                                                                     # South America
                                                                     # Others
                                                                     "Didn't Answer",
                                                                     "Other"))
        
    }
    
    # Friendship
    {
    
        frienshipTable$YesterdaySMS = factor(frienshipTable$YesterdaySMS , levels = c( "None", "1 to 5", "6 to 10", "11 to 20", "21 to 50", ">50", "Unknown"))

    }    
    
    # Puberty men and women
    {
        
        unique(pubertyWomenTable$Breats)
        
        pubertyMenTable$ChangeHeight = factor(pubertyMenTable$ChangeHeight, levels = c("Haven't Started", "Barely Started", "Definitely Underway", "Completed","Didn't Answered"))
        pubertyMenTable$ChangeVoice  = factor(pubertyMenTable$ChangeVoice,  levels = c("Haven't Started", "Barely Started", "Definitely Underway", "Completed","Didn't Answered"))
        pubertyMenTable$FacialHair   = factor(pubertyMenTable$FacialHair,   levels = c("Haven't Started", "Barely Started", "Definitely Underway", "Completed","Didn't Answered")) 
        pubertyMenTable$MenBodyHair  = factor(pubertyMenTable$MenBodyHair,  levels = c("Haven't Started", "Barely Started", "Definitely Underway", "Completed","Didn't Answered")) 
        pubertyMenTable$MenPubicHair = factor(pubertyMenTable$MenPubicHair, levels = c("Yes", "No", "Unknown"))
        
        pubertyWomenTable$Menarche       = factor(pubertyWomenTable$Menarche,       levels = c("Yes", "No", "Unknown"))
        pubertyWomenTable$WomenPubicHair = factor(pubertyWomenTable$WomenPubicHair, levels = c("Yes", "No", "Unknown"))
        pubertyWomenTable$Breasts        = factor(pubertyWomenTable$Breasts,        levels = c("Yes", "No", "Unknown"))
        
    }
    
    # Drugs
    {

        drugsTable$Smoke         = factor(drugsTable$Smoke        , levels = c("Never", "Sometimes", "Daily", "Unknown"))
        drugsTable$SmokePerWeek  = factor(drugsTable$SmokePerWeek , levels = c("Never", "0 to 1",  "2 to 3", "4 to 6", "7 to 10", "More than 10", "Unknown"))
        drugsTable$SmokePerDay   = factor(drugsTable$SmokePerDay  , levels = c("Never", "0 to 1",  "2 to 3", "4 to 6", "7 to 10", "More than 10", "Unknown"))
        drugsTable$Snuff         = factor(drugsTable$Snuff        , levels = c("Never", "Sometimes", "Daily", "Unknown"))
        drugsTable$SnuffPerWeek  = factor(drugsTable$SnuffPerWeek , levels = c("Never", "0 to 1",  "2 to 3", "4 to 6", "7 to 10", "More than 10", "Unknown"))
        drugsTable$SnuffPerDay   = factor(drugsTable$SnuffPerDay  , levels = c("Never", "0 to 1",  "2 to 3", "4 to 6", "7 to 10", "More than 10", "Unknown"))
        
        # The alcohol variable has an special requirement.
        # There are too few people that drink too much (which is good)
        # So we are going to add all of them to the previous group
        # Otherwise we end up with analysis of data with a categorical that has
        # only 1 sample size and things like that.
        #
        # (This is the original data)
        # Old factoring with the original data
        #
        #     -"Never",
        #     -"Once per month or less"
        #     -"2-4 times per month"
        #     -"2-3 times per week"
        #     -"4 or more times per week",
        #     -"Unknown"
        #
        #  New grouping is
        #
        #     -"Never",
        #     -"Once per month or less"
        #     -"Twice per moth or more"
        #     -"Unknown"        
        
        # R is stupid, it took me 2 hours to find the error that was here
        # where the "Twice of more per month" was wrongly written in the 
        # levels part. Which would had never happens if this language
        # had proper class enumeration like C++ has! >:(
        
        newGroup = "Twice of more per month"
        for (i in 1:nrow(drugsTable)) {
    
            if(drugsTable$Alcohol[i] == "2-4 times per month")      drugsTable$Alcohol[i] = newGroup
            if(drugsTable$Alcohol[i] == "2-3 times per week")       drugsTable$Alcohol[i] = newGroup
            if(drugsTable$Alcohol[i] == "4 or more times per week") drugsTable$Alcohol[i] = newGroup
      
        }
        
        drugsTable$Alcohol = factor(drugsTable$Alcohol , levels = c("Never", "Once per month or less", "Twice of more per month", "Unknown"))
        
        # The following questions are only for people who drink, however that leave
        # empty many data cells that can have useful information.
        #
        # Alcohol units doesn't contemplate the number 0 in the original dataset.
        # So we are going to correct for that too with the help of the previous variable
        drugsTable$AlcoholUnits  = factor(drugsTable$AlcoholUnits  , levels = c( "0",  "1 to 2",  "3 to 4", "5 to 6", "7 to 9", "10 or More", "Unknown"))
        drugsTable$Alcohol6Units = factor(drugsTable$Alcohol6Units , levels = c( "Never",  "Less than monthly", "Monthly", "Weekly", "Daily", "Unknown"))       

        for (i in 1:nrow(drugsTable)) {
        
            if(drugsTable$Alcohol[i] == "Never"){
        
                drugsTable$AlcoholUnits[i]  = "0"        
                drugsTable$Alcohol6Units[i] = "Never"
                
            }
                 
        }       
        
    }
        
    # Sports
    {

        sportsTable$SportsLeisure       = factor(sportsTable$SportsLeisure       , levels = c( "None",   "Light",    "Medium","Hard", "Unknown"))
        sportsTable$SportsOutsideSchool = factor(sportsTable$SportsOutsideSchool , levels = c( "No",  "Yes", "Unknown"))

        sportsTable$SportsFrequency     = factor(sportsTable$SportsFrequency     , levels = c( "Never", "Less than daily", "1 per week", "2-3 per week", "4-6 per week", "Almost every day", "Uknown"))
        sportsTable$SportHours          = factor(sportsTable$SportHours          , levels = c( "None", "About half an hour", "1 to 1.5 hours", "2 to 3 hours", "4 to 6 hours", "7 hours or more", "Uknown"))            
        sportsTable$SportsIntensity     = factor(sportsTable$SportsIntensity     , levels = c( "Not hard at all", "A bit hard", "Quite hard", "Very hard", "Extremely hard", "Uknown"))                        

        sportsTable$SummerTransport     = factor(sportsTable$SummerTransport     , levels = c( "By car or moped", "By bus", "By bike", "On foot", "Uknown"))
        sportsTable$SummerTime          = factor(sportsTable$SummerTime          , levels = c( "<5 minutes", "6-15 minutes", "16-30 minutes", "31-60", ">60 minutes", "Uknown"))                                
        sportsTable$WinterTransport     = factor(sportsTable$WinterTransport     , levels = c( "By car or moped", "By bus", "By bike", "On foot", "Uknown"))
        sportsTable$WinterTime          = factor(sportsTable$WinterTime          , levels = c( "<5 minutes", "6-15 minutes", "16-30 minutes", "31-60", ">60 minutes", "Uknown"))                                            

        sportsTable$ScreenTime          = factor(sportsTable$ScreenTime          , levels = c( "None",  "About half an hour",  "About 1 to 1,5 hours", "About 2 to 3 hours",
                                                                                               "About 4 to 6 hours", "About 7 to 9 hours", "10 hours or more", "Uknown"))                                            
         
        
    }
    
    
    # Hygiene
    {
         
        hygieneTable$ShowerBathFrequency = factor(hygieneTable$ShowerBathFrequency  , levels = c( "Less than once a week", "Once a week", "2-3 times a week", "4-6 times a week", "Once a day",  "2 times a day", "3 or more times a day", "Unknown"))
        hygieneTable$HandwashFrequency   = factor(hygieneTable$HandwashFrequency    , levels = c("0 times", "1-2 times", "3-5 times", "6-10 times", "11-20 times", "More than 20 times", "Unknown"))
        hygieneTable$BodyLotionFrequency = factor(hygieneTable$BodyLotionFrequency  , levels = c("Less than once a week", "Once a week", "2-3 times a week", "4-6 times a week", "Once a day", "2 or more times a day", "Unknown"))        
        hygieneTable$SkinSunbathing      = factor(hygieneTable$SkinSunbathing       , levels = c("Always red, never brown", "Almost always red, sometimes brown", "Almost always brown, sometimes red", "Always brown, never red", "Unknown"))        

        hygieneTable$HolidaySunbathing   = factor(hygieneTable$HolidaySunbathing    , levels = c("Yes", "No", "Unknown"))        
        hygieneTable$SolariumLast4Weeks  = factor(hygieneTable$SolariumLast4Weeks   , levels = c("Yes", "No", "Unknown"))
        
    }
    
    # Biomarkers (flags only)
    {
    
        biomarkersTable$Flagged          = factor(biomarkersTable$Flagged           , levels = c("Yes", "No", "Unknown"))        
            
    }
    
    # Diet
    {
    
        # Time of the day
        dietTable$BreakfastFrequency      = factor(dietTable$BreakfastFrequency      , levels = c( "Rarely/Never", "1-3 times per week", "4-6 times per week", "Every day", "Didn't Answered"))
        # -- Lunch and dinner is missing from data
        # Specific foods
        dietTable$FatFishFrequency        = factor(dietTable$FatFishFrequency        , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Didn't Answered"))                
        dietTable$LeanFishFrequency       = factor(dietTable$LeanFishFrequency       , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Didn't Answered"))
        dietTable$SeagullEggsFrequency    = factor(dietTable$SeagullEggsFrequency    , levels = c( "Rarely/Never", "1-3 times per year", "4-5 times per year", "6-9 times per year", "10 or more times per year", "Didn't Answered"))            
        dietTable$ReindeerFrequency       = factor(dietTable$ReindeerFrequency       , levels = c( "Rarely/Never", "1-3 times per year", "4-5 times per year", "6-9 times per year", "10 or more times per year", "Didn't Answered"))            
        dietTable$CheeseFrequency         = factor(dietTable$CheeseFrequency         , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Didn't Answered"))
        dietTable$ChocolateFrequency      = factor(dietTable$ChocolateFrequency      , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Didn't Answered"))
        dietTable$FruitsFrequency         = factor(dietTable$FruitsFrequency         , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "1-2 times per day", "3-4 times per day", "5 times or more per day", "Didn't Answered"))
        dietTable$VegetablesFrequency     = factor(dietTable$VegetablesFrequency     , levels = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "1-2 times per day", "3-4 times per day", "5 times or more per day", "Didn't Answered"))            
        dietTable$DairyFrequency          = factor(dietTable$DairyFrequency          , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))
        # Specific Drinks
        dietTable$FruitJuiceFrequency     = factor(dietTable$FruitJuiceFrequency     , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))
        dietTable$SugarJuiceFrequency     = factor(dietTable$SugarJuiceFrequency     , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))            
        dietTable$SugarDrinkFrequency     = factor(dietTable$SugarDrinkFrequency     , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))            
        dietTable$SweetenerDrinkFrequency = factor(dietTable$SweetenerDrinkFrequency , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))            
        dietTable$WaterFrequency          = factor(dietTable$WaterFrequency          , levels = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))                        
        # Specific pills
        dietTable$FishOilFrequency        = factor(dietTable$FishOilFrequency        , levels = c( "No",   "Sometimes",  "Yes, daily", "Didn't Answered")) 
        dietTable$VitaminsFrequency       = factor(dietTable$VitaminsFrequency       , levels = c( "No",   "Sometimes",  "Yes, daily", "Didn't Answered")) 

    }
    
    # Sleep
    {

        sleepTable$BedTimeHourCat = factor(sleepTable$BedTimeHourCat , levels = c( "18:00 or earlier", "18:30", "19:00", "19:30",
                                                                                                       "20:00", "20:30", "21:00", "21:30", "22:00",
                                                                                                       "22:30", "23:00", "23:30", "00:00", "00:30",
                                                                                                       "01:00", "01:30", "02:00 or later", "Unknown")) 
        sleepTable$SleepingPills  = factor(sleepTable$SleepingPills , levels = c( "Not used",   "Less frequently than every week",    "Every week, but not daily", "Daily", "Unknown"))            
           
    }

    # Contraceptives
    {
        
        # Sorted by the amount of estradiol and amount of progestin
        contraceptivesDBDF$Hormonal = factor(contraceptivesDBDF$Hormonal , levels = c("Non-hormonal", "Progestin", "Low Estradiol", "High Estradiol", "Unknown"))
        
    }
    

  }

# From here on, we can finally add all tables into a list and do things automatic
#
# We don't do this anymore, we divided the scripts into topics, otherwise
# working with R become unbearable

# We put everything in a list though, so we can check for errors automatically
# This is the same code that in datacleaning, which AGAIN, wouldn't have to make
# this complete mess if we actually had a reference system!
#
# The table info is the same, we just need to update the table list and where
# are you saving the tables, which in this case is the filter folder
{
    
        # Here we store information about each table
        #   [1] - Table computer name, they get transformed into basicTable.csv,
        #         basicTable.txt or whatever. This is very usefull for automatic
        #         things later.
        #
        #         All the metadata for each variable is stored in
        #         basicTable.info, for example.
        #
        #   [2] - Human readable name, great for formating
        #
        #   [3] - Description of what does the table contain
        #
        #   [4] - How many rows  we have
        #
        #   [5] - How many variables (columns) we have        
        #
        #   [6] - Where is the table stored, the name of the file is [1]+".csv"
        #    
        totalTables    = 36
        allTablesList  = newList(totalTables)
        tablesMetaInfo = DF(totalTables, 6)
        colnames(tablesMetaInfo) = c("Table Name","Human Name", "Description", "Folder", "Total Rows", "Total Columns")
    

        # In here we fill the information for each table manually
        {
            
            # In this case, all tables are stored in the same place
            # but you can change that if it makes more sense for your project.
            savedFolder = substr(DATA_FILTER_FOLDER, nchar(MAIN_PROJECT_FOLDER)+1, nchar(DATA_FILTER_FOLDER))
            tablesMetaInfo[,4] = savedFolder
            
            # --  1 -- Basic
            allTablesList[[1]]  = basicTable
            
            # --  2 -- networkTechnicalTable
            allTablesList[[2]]  = networkTechnicalTable
            
            # --  3 -- antropometricFF1Table
            allTablesList[[3]]  = antropometricTableFF1
            
            # --  4 -- antropometricFF2Table
            allTablesList[[4]]  = antropometricTableFF2
            
            # --  5 -- menstruationTable
            allTablesList[[5]]  = menstruationTable
            
            # --  6 -- aureusTable
            allTablesList[[6]]  = aureusTable
            
            # --  7 -- swabbingTable
            allTablesList[[7]]  = swabbingTable
            
            # --  8 -- highSchoolTable
            allTablesList[[8]]  = highSchoolTable
            
            # --  9 -- bloodTable
            allTablesList[[9]]  = bloodTable
            
            # -- 10 -- bloodTechnicalTable
            allTablesList[[10]]  = bloodTechnicalTable
            
            # -- 11 -- sociologyTable
            allTablesList[[11]]  = sociologyTable
            
            # -- 12 -- frienshipTable
            allTablesList[[12]]  = frienshipTable
            
            # -- 13 -- pubertyMenTable
            allTablesList[[13]]  = pubertyMenTable
            
            # -- 14 -- pubertyWomenTable
            allTablesList[[14]]  = pubertyWomenTable
            
            # -- 15 -- drugsTable
            allTablesList[[15]]  = drugsTable
            
            # -- 16 -- sportsTable
            allTablesList[[16]]  = sportsTable
            
            # -- 17 -- hygieneTable
            allTablesList[[17]]  = hygieneTable
            
            # -- 18 -- biomarkersTable
            allTablesList[[18]]  = biomarkersTable
            
            # -- 19 -- dietTable
            allTablesList[[19]]  = dietTable
            
            # -- 20 -- sleepTable
            allTablesList[[20]]  = sleepTable
            
            # -- 21 -- networkTable
            allTablesList[[21]]  = sleepTable # networkTable
            
            # FF1 matrices
            
            # -- 22 -- overallNetworkDF
            allTablesList[[22]]  = overallNetworkDF
            
            # -- 23 -- physicalNetworkDF
            allTablesList[[23]]  = physicalNetworkDF
            
            # -- 24 -- homeNetworkDF
            allTablesList[[24]]  = homeNetworkDF
            
            # -- 25 -- schoolNetworkDF
            allTablesList[[25]]  = schoolNetworkDF
            
            # -- 26 -- sportsNetworkDF
            allTablesList[[26]]  = sportsNetworkDF
            
            # -- 27 -- otherNetworkDF
            allTablesList[[27]]  = otherNetworkDF 
            
            # FF12 Matrices

            # -- 28 -- overallNetworkFF12DF
            allTablesList[[28]]  = overallNetworkFF12DF
            
            # -- 29 -- physicalNetworkFF12DF
            allTablesList[[29]]  = physicalNetworkFF12DF
            
            # -- 30 -- homeNetworkFF12DF
            allTablesList[[30]]  = homeNetworkFF12DF
            
            # -- 31 -- schoolNetworkFF12DF
            allTablesList[[31]]  = schoolNetworkFF12DF
            
            # -- 32 -- sportsNetworkFF12DF
            allTablesList[[32]]  = sportsNetworkFF12DF
            
            # -- 33 -- otherNetworkFF12DF
            allTablesList[[33]]  = otherNetworkFF12DF       
            
            # DBDF tables
            
            # -- 34 -- medicinesDBDF
            allTablesList[[34]]  = medicinesDBDF
            
            # -- 35 -- contraceptivesDBDF
            allTablesList[[35]]  = contraceptivesDBDF
            
            # -- 36 -- diseasesDBDF
            allTablesList[[36]]  = diseasesDBDF      
            
            
            # Keep these for when more tables come
            # -- 37 --
            #tablesMetaInfo[1,1] = ""
            #tablesMetaInfo[1,2] = ""
            #tablesMetaInfo[1,3] = ""         
            #tablesMetaInfo[1,4] = savedFolder
            
            #allTablesList[[1]]  = basicTable      
            
            # -- 38 --
            #tablesMetaInfo[1,1] = ""
            #tablesMetaInfo[1,2] = ""
            #tablesMetaInfo[1,3] = ""         
            #tablesMetaInfo[1,4] = savedFolder
            
            #allTablesList[[1]]  = basicTable                  

        }
        
        # Here we fill the automatic information (table size)
        {
        
            for (i in 1:totalTables){
                
                currentTable = allTablesList[[i]]
                
                tablesMetaInfo[i,5] = nrow(currentTable)
                tablesMetaInfo[i,6] = ncol(currentTable)
            
            }
            
                
            
        }
    
}

# Check that you don't have any NA in any table that might have some hidden meaning
#
# (This actually repeat in the metainfo variables DF, so maybe skip)
# -----------------------------------------------------------------------------
{
 
    NAWarningsDF           = DF(totalTables, 2)
    colnames(NAWarningsDF) = c("Table", "Total Columns with NA")
    
    # R is horrible, it uses [] and [[]] for indexing as if they are totally
    # different concept while both do exactly the same. Is just an arbitrary
    # restriction that [] is use for vectors and [[]] is use for lists.
    #
    # Seriously, R is to programming what a tricycle is to transportation.
    # Yes, is way easier to drive a tricycle than to drive a 6 shafts with a 
    # 12 gearbox truck. But transporting things in tricycles is completely
    # impractical. If people are too scare to learn how to drive a truck we
    # shouldn't be making better tricycles, is never going to work. Learn C++!
    
    for(i in 1:totalTables){
        
        currentTable     = allTablesList[[i]]
        currentTableName = tablesMetaInfo[i,1]
        
        currentTotalVariables = ncol(currentTable)
    
        hasNAvariables     = rep(FALSE, currentTotalVariables)
        howManyNAvariables = rep(0,     currentTotalVariables)    
      
        for(j in 1:currentTotalVariables){
        
            totalNAs = sum(is.na(currentTable[,j]))    
            if(totalNAs > 0){
                
                hasNAvariables[j]     = TRUE
                howManyNAvariables[j] = totalNAs
                
            }
            
        }
        
        
        if(sum(howManyNAvariables) > 0){

            print("")
            print("NAs WARNING!!")
            print(currentTableName)
            print("Please make sure that these columns are clean and this is what you want:")
            print(colnames(currentTable)[hasNAvariables])
            print("")
        
        }
          
    }
       
}


# -----------------------------------------------------------------------------  
# Join tables with proper info together
#
# Again, and AGAIN, this doesn't make any sense from the programming point of
# view. The correct way of doing this is having a pointer to a person object
# and each person has the proper info.
#
# But we NEED to do this so the stupid graphs works as intended in this
# forsaken programming language that God has abandon. Otherwise, we need one
# graph per table, and we can't mix info from different tables in the plots.
# -----------------------------------------------------------------------------
{
    
    # In any case, we need to keep the FF2, and FF3 variables independent, in
    # another completeTable (completeTableFF2, completeTableFF3 if you will)
    ff1InvalidTableIndexes = c(4)
    
    # Sleep table is the last table that we want to join
    # after that, the network table, and the frienship matrixes come.
    finalTableToJoint = 20 
    
    
    completeTable = allTablesList[[1]]     %>% left_join(allTablesList[[2]] , by="ID")
        
    # For each of the tables
    for(i in 3:finalTableToJoint){
    
        # If you are a FF1 table
        if( (i %in% ff1InvalidTableIndexes) == FALSE){
        
            completeTable = completeTable     %>% left_join(allTablesList[[i]] , by="ID")
                
        }
        
    }

}


# -----------------------------------------------------------------------------
# Add the contraceptive information to the complete table
#
# Here it would be best to do a join operation with the contraceptive information
# but R is very bad, and also we don't have a complete 1..n relationship, so many
# people are empty info. So we correct for this in here
# -----------------------------------------------------------------------------
{
    
    completeTable$HormonalType = "Non-hormonal"
    for (i in 1:nrow(completeTable)) {
        
        
        # Get the ID
        currentID = completeTable$ID[i]
        
        # Search in the database for this information
        if(currentID %in% contraceptivesDBDF$ID == TRUE){

            currentHormonalInformation    = as.character(contraceptivesDBDF[contraceptivesDBDF$ID == currentID,]$Hormonal) # FUCK THE R LEVELS!!
            completeTable$HormonalType[i] = currentHormonalInformation
        }
        
    }
    # Add the levels info; AGAIN! This is stupid, this shouldn't be at column level.
    # I hate this language so much you can't even imagine
    completeTable$HormonalType = factor(completeTable$HormonalType , levels = c("Non-hormonal", "Progestin", "Low Estradiol", "High Estradiol", "Unknown"))
    
}


# -----------------------------------------------------------------------------
# Make the proper graph objects
# -----------------------------------------------------------------------------
{

    # Matrixes
    {
        
    
            
    }
    
    # Nodes
    # (note that the nodes are the same for all the networks, only the edges change)
    nodesDF                = completeTable
    nodesDF[]              = lapply(nodesDF[], as.character) # Need to transform integers ID to strings IDs for whatever reason o_O , why??
                                                             #
                                                             # Here is another reason as to why R is total crap. You need to do the previous conversion
                                                             # and whether you do this for all variables, or only ID it doesn't matter. It will do it
                                                             # for all variables. As such, the level-order information is lost inside the graph
                                                             #
                                                             # So you have 3 options:
                                                             #
                                                             # - Use a real programming language with pointers to the table (preferred)
                                                             # - Forget about the levels and correct the results order later
                                                             # - Everytime you require the graph, also give the table. Stupid, but alas, needed.

    # Edges
    {

        # ---- DIRECTED
    
        # -------- Overall
        {
        meltedOverall           = melt(overallNetworkDF, id.vars = "ID")
        colnames(meltedOverall) = c("from", "to", "value")
    
        overallEdgesDF           = meltedOverall
        overallEdgesDF$value     = NULL
        overallEdgesDF[]         = lapply(overallEdgesDF, as.character) # Same as before 0_o
        overallEdgesDF$value     = meltedOverall$value
        overallEdgesDF           = overallEdgesDF[overallEdgesDF$value == 1, ] # Get only those with value = 1
        }
        # -------- Physical
        {
        meltedPhysical           = melt(physicalNetworkDF, id.vars = "ID")
        colnames(meltedPhysical) = c("from", "to", "value")
    
        physicalEdgesDF          = meltedPhysical
        physicalEdgesDF$value    = NULL
        physicalEdgesDF[]        = lapply(physicalEdgesDF, as.character)
        physicalEdgesDF$value    = meltedPhysical$value
        physicalEdgesDF          = physicalEdgesDF[physicalEdgesDF$value == 1, ]
        }
        # -------- Home
        {
        meltedHome               = melt(homeNetworkDF, id.vars = "ID")
        colnames(meltedHome)     = c("from", "to", "value")
    
        homeEdgesDF              = meltedHome
        homeEdgesDF$value        = NULL
        homeEdgesDF[]            = lapply(homeEdgesDF, as.character)
        homeEdgesDF$value        = meltedHome$value
        homeEdgesDF              = homeEdgesDF[homeEdgesDF$value == 1, ]
        }
        # -------- School
        {
        meltedSchool             = melt(schoolNetworkDF, id.vars = "ID")
        colnames(meltedSchool)   = c("from", "to", "value")
    
        schoolEdgesDF            = meltedSchool
        schoolEdgesDF$value      = NULL
        schoolEdgesDF[]          = lapply(schoolEdgesDF, as.character)
        schoolEdgesDF$value      = meltedSchool$value
        schoolEdgesDF            = schoolEdgesDF[schoolEdgesDF$value == 1, ]
        }
        # -------- Sports
        {
        meltedSports             = melt(sportsNetworkDF, id.vars = "ID")
        colnames(meltedSports)   = c("from", "to", "value")
    
        sportsEdgesDF            = meltedSports
        sportsEdgesDF$value      = NULL
        sportsEdgesDF[]          = lapply(sportsEdgesDF, as.character)
        sportsEdgesDF$value      = meltedSports$value
        sportsEdgesDF            = sportsEdgesDF[sportsEdgesDF$value == 1, ]
        }
        # -------- Others
        {
        meltedOther              = melt(otherNetworkDF, id.vars = "ID")
        colnames(meltedOther)    = c("from", "to", "value")
    
        otherEdgesDF             = meltedOther
        otherEdgesDF$value       = NULL
        otherEdgesDF[]           = lapply(otherEdgesDF, as.character)
        otherEdgesDF$value       = meltedOther$value
        otherEdgesDF             = otherEdgesDF[otherEdgesDF$value == 1, ]
        }
        
        # ---- RECIPROCAL
        # Nothing here
    
    
    
    }

    # Create the graph object
    {
    
        # Directed
        overallGraph  = graph_from_data_frame(overallEdgesDF,  vertices = nodesDF, directed = T)
        physicalGraph = graph_from_data_frame(physicalEdgesDF, vertices = nodesDF, directed = T)
        schoolGraph   = graph_from_data_frame(schoolEdgesDF,   vertices = nodesDF, directed = T)
        sportsGraph   = graph_from_data_frame(sportsEdgesDF,   vertices = nodesDF, directed = T)
        homeGraph     = graph_from_data_frame(homeEdgesDF,     vertices = nodesDF, directed = T)
        othersGraph   = graph_from_data_frame(otherEdgesDF,    vertices = nodesDF, directed = T)
    
        # Undirected
        undirectedOverallGraph  = graph_from_data_frame(overallEdgesDF,  vertices = nodesDF, directed = F)
        undirectedPhysicalGraph = graph_from_data_frame(physicalEdgesDF, vertices = nodesDF, directed = F)
        undirectedSchoolGraph   = graph_from_data_frame(schoolEdgesDF,   vertices = nodesDF, directed = F)
        undirectedSportsGraph   = graph_from_data_frame(sportsEdgesDF,   vertices = nodesDF, directed = F)
        undirectedHomeGraph     = graph_from_data_frame(homeEdgesDF,     vertices = nodesDF, directed = F)
        undirectedOthersGraph   = graph_from_data_frame(otherEdgesDF,    vertices = nodesDF, directed = F)
        
        # Reciprocal
        reciprocalOverallGraph  = delete_edges(overallGraph,  E(overallGraph)[!which_mutual(overallGraph)])
        reciprocalPhysicalGraph = delete_edges(physicalGraph, E(physicalGraph)[!which_mutual(physicalGraph)])
        reciprocalSchoolGraph   = delete_edges(schoolGraph,   E(schoolGraph)[!which_mutual(schoolGraph)])
        reciprocalSportsGraph   = delete_edges(sportsGraph,   E(sportsGraph)[!which_mutual(sportsGraph)])
        reciprocalHomeGraph     = delete_edges(homeGraph,     E(homeGraph)[!which_mutual(homeGraph)])
        reciprocalOthersGraph   = delete_edges(othersGraph,   E(othersGraph)[!which_mutual(othersGraph)])
        
        # Reciprocal Edges DF
        reciprocalOverallEdgesDF  = as.data.frame(get.edgelist(reciprocalOverallGraph))
        reciprocalPhysicalEdgesDF = as.data.frame(get.edgelist(reciprocalPhysicalGraph))
        reciprocalSchoolEdgesDF   = as.data.frame(get.edgelist(reciprocalSchoolGraph))
        reciprocalSportsEdgesDF   = as.data.frame(get.edgelist(reciprocalSportsGraph))
        reciprocalHomeEdgesDF     = as.data.frame(get.edgelist(reciprocalHomeGraph))
        reciprocalOthersEdgesDF   = as.data.frame(get.edgelist(reciprocalOthersGraph))
    
        colnames(reciprocalOverallEdgesDF)  = c("from", "to")
        colnames(reciprocalPhysicalEdgesDF) = c("from", "to")
        colnames(reciprocalSchoolEdgesDF)   = c("from", "to")
        colnames(reciprocalSportsEdgesDF)   = c("from", "to")
        colnames(reciprocalHomeEdgesDF)     = c("from", "to")
        colnames(reciprocalOthersEdgesDF)   = c("from", "to")
          
      }

    # Make a vector with all the edges and all the graph
    {
        allEdges  = list(overallEdgesDF, physicalEdgesDF, schoolEdgesDF, sportsEdgesDF, homeEdgesDF, otherEdgesDF)
        allGraphs = list(overallGraph,   physicalGraph,   schoolGraph,   sportsGraph,   homeGraph,   othersGraph)
    }
  
}


# -----------------------------------------------------------------------------
# Prepare the frienship matrix with numbers
# -----------------------------------------------------------------------------
{
    # The friend matrix, as 1 (friend) and 0 (non friend)
    # -- Friends if the original code is defined by rows (each row a maximum of 5)
    tempOverall      = overallNetworkDF
    tempOverall$ID   = NULL
    friendshipMatrix = as.matrix(tempOverall, nrow(completeTable))    
}

# -----------------------------------------------------------------------------
# Find the indexes for all the data
# -----------------------------------------------------------------------------
source(paste0(MAIN_PROJECT_FOLDER,"src/indexesV2.R"), encoding="utf-8")

# Subset tables
# -----------------------------------------------------------------------------
{

    # I UTERLY DESPISE R. This subsets are done when the script is run, but
    # if you change info later, you need to subset again. So if you change
    # something in women, instead of having pointer here LIKE WHAT ANY SERIOUS
    # PROGRAMMING LANGUAGE WOULD DO!!!! , you need to copy the info, and update
    # it every time you are doing something. FUCK ALL THIS SHIT!
    
    # By one variable
    {
    
        # -- By Sex
        menOnlyTable   = subset(completeTable, completeTable[,sexIndex] == "Man")
        womenOnlyTable = subset(completeTable, completeTable[,sexIndex] == "Woman")
        totalMenRows   = nrow(menOnlyTable)
        totalWomenRows = nrow(womenOnlyTable)
        
        # Diseases
        # Medication
        # Contraceptives (nothing here, since only women take contraceptives)
        
    }
    
    # Individual tables
        
}


# Create a dataframe with all the variables meta-information
#     - Type of variable
#     - Number of categories (0 = Numerical)
#     - NA counts
# Analyze which type of variables you have in each column
#
# This is already in the metadata when done correctly
if(FALSE){
    
    # How many columns and rows
    totalColumns = ncol(completeTable)
    totalRows    = nrow(completeTable)

    # The main dataframe
    variablesInfoDF           = DF((totalColumns + totalBasicTables - 1), 8)     # Each table has an ID column that you need to add (+total tables)
                                                                                 # except for the first one that is already included in the complete table (-1)
    colnames(variablesInfoDF) = c("VariableID", "Table", "Type", "TotalCategories", "TotalNAValues", "TableIndex", "LocalIndex", "UniversalIndex")
    
    currentIndex = 1
    
    # For each of the tables that we have    
    for(i in 1:totalBasicTables){
    
        # Get the table and table name
        currentTable     = allBasicTablesList[[i]]
        currentTableName = allBasicTablesNames[i]
        
        # Count how many columns you have
        currentTotalColumns = ncol(currentTable)

        for(j in 1:currentTotalColumns){
    
            # Init some basic variables
            currentVariableName   = colnames(currentTable)[j]
            currentType           = "Date"
            currentCategories     = 0
            currentNAs            = sum(is.na(currentTable[,j]))
            currentUniversalIndex = grep( paste0("^",currentVariableName,"$"),   colnames(completeTable))

            # Figure it out the type and number of categories if any
            myCurrentClass = class(currentTable[,j])

            # -- Categorical
            if(myCurrentClass == "character" || myCurrentClass == "factor"){
                
                currentType       = "Categorical"
                currentCategories = nrow(summarizeCategorical(currentTable, j, sorted = "none", crop = 0))
            
            
            }
            # -- Numerical
            if(myCurrentClass == "integer" || myCurrentClass == "numeric"){
                
                currentType       = "Numerical"
                
            }

            # Write the information into the metasummary
            variablesInfoDF$VariableID[currentIndex]      = currentVariableName
            variablesInfoDF$Table[currentIndex]           = currentTableName
            variablesInfoDF$Type[currentIndex]            = currentType
            variablesInfoDF$TotalCategories[currentIndex] = currentCategories
            variablesInfoDF$TotalNAValues[currentIndex]   = currentNAs
            variablesInfoDF$TableIndex[currentIndex]      = i
            variablesInfoDF$LocalIndex[currentIndex]      = j
            variablesInfoDF$UniversalIndex[currentIndex]  = currentUniversalIndex
                
            # R is horrible, I want to do currentIndex++ so badly
            # Even if you don't provide it in the basic package, you could at
            # least allow for overwrite operators like in C++ where you can
            # program for ++ [] [[]] - * to do whatever you want. But not here.
            currentIndex = currentIndex + 1
            
        }
        
    }
    
}


# Group indexes together. This is a brute force approach, and you should
# really use the manual indexes in the indexes script. In here you have too many
# indexes, such as the SPA-types with 200+ categories.
#
# In order to solve that a little bit in the brute force approach, we also do
# indexes that has less than the category limit we set up in the constants
# script (usually 12)
if(FALSE){

    bruteCategoricalIndexes      = variablesInfoDF[variablesInfoDF$Type == "Categorical",]$UniversalIndex
    totalBruteCategoricalIndexes = length(bruteCategoricalIndexes)
    
    bruteLimitCategoricalIndexes      = variablesInfoDF[variablesInfoDF$Type == "Categorical" & variablesInfoDF$TotalCategories<=CATEGORY_LIMIT ,]$UniversalIndex
    totalBruteLimitCategoricalIndexes = length(bruteLimitCategoricalIndexes)
    
    
    
}