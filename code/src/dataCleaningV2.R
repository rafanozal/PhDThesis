# ----------------------------------------------------------------------
#               / ! \        IMPORTANT       / ! \ 
# ----------------------------------------------------------------------

# Some of the original code is missing from this version due privacy
# reasons.

# The original code keep track of every single change. If the change is
# for example, deleting a name that shouldn't be there, this is not
# shown in this code.

# To get the complete code, apply for data access to the Fit Futures
# directly and refer to this PhD thesis.



# -----------------------------------------------------------------------------
#               / ! \        IMPORTANT       / ! \ 
# -----------------------------------------------------------------------------
#
#         You need to run the constant.R script first manually!!!!
#
# R doesn't allow for automatic file sourcing anymore,
# sorry for the inconvenience!



# -----------------------------------------------------------------------------
#
# This script is for reading RAW data and clean it and prepare it for further
# analysis.
#
# The script does:
#
#   - Read the original files and merge them together into a proper complete
#     table.

#   - Transform weird column name into human readable column names whether
#     applicable. Sometimes is best to keep the original column names for
#     consistency.
#
#   - Change the numerical codes (ie 1 = Man , 0 = Woman) into human readable
#     strings.
#
#   - Replace all IDs for indexes, so a person ID 11223300 goes into 1, which
#     makes easy finding and indexing later. Also anonymize the data further.
#
#   - Replace NA / NULL values for numerical values encoding whatever we need
#     in each case.
#
#   - Add new info base on the RAW info. For example the BMI category from
#     the BMI number, or the SA carrier status based on the results of the
#     tests.
#
#   - Create the 6 friendship network matrices. IT DOES NOT MELT the matrix.
#
# All of this is then saved in different CSVs which are ready to be read and
# run further analysis. You can use these CSVs in any program you want and is
# not R dependent. But if you want to use R, then go to load2.R script to
# load those CSV into memory in R.
#
# Notice that the original data doesn't make sense from a relational database
# perspective. So the data is organized accordingly into a proper relational
# tables.
#
# In the file ../data/Variables.txt you have an extensive and comprehensive
# summary of all the original variables that we have.
#
#   - Complete:  All the patient data in the same table instead of organized
#                by different sub-tables (which are as follow.)

# -----------------------------------------------------------------------------

# Add the needed libraries




library(plyr)      # You need to load dplyr in other libraries for the plot to
                   # work properly, this setup completely mess up everything, 
                   # but dplyr is total shit doing the mutates whether plyr
                   # is plain and simple.

library(dplyr)     # left_join in the complete table and %% mutate
                   #
                   # I don't have enough word to describe how much I hate
                   # dplyr. It doesn't make sense. Is complicated to read.
                   # is not that great in performance either. You need to write
                   # a lot of lines to do something very simple!. I HATE YOU!!!
                   #
                   # Just look at the mutate functions, are a complete mess!
                   # THAT IT! I __*REFUSE*__ to use DPLYR for mutating.
                   # The bug with the graphic libraries stays. YOU CAN'T EVEN
                   # MUTATE NAs , need to do yet another pass through the whole
                   # dataset! WTH?? .default doesn't even mean default(), is
                   # does mean everything that is not NA or NULL. AAAAHHH!!!!!


library(stringr)   # For the substring function, the fact that R doesn't have
                   # a default string library to do these thing is beyond sad.

library(ggpubr)    # For the ggarrange function



source(paste0(MAIN_PROJECT_FOLDER,"src/lib/basic/toolsBasic.R"),               encoding="utf-8")
source(paste0(MAIN_PROJECT_FOLDER,"src/lib/basic/toolsSummarizers.R"),         encoding="utf-8")
source(paste0(MAIN_PROJECT_FOLDER,"src/lib/basic/toolsMedicine.R"),            encoding="utf-8")
source(paste0(MAIN_PROJECT_FOLDER,"src/lib/latex/toolsLatex.R"),               encoding="utf-8")
source(paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingBoxplots.R"), encoding="utf-8")


# Init the log
# -----------------------------------------------------------------------------
{
    # All logs are init to empty, it will delete any previous log information
    
    # Log for the general cleaning process
    logTXTFileConnection = file(CLEANING_LOG_PATH, 'w')
    logLine              = paste0( "CLEANING DATA LOG at: ", RIGHT_NOW)
    write( logLine ,
           file = logTXTFileConnection,
           append = FALSE)
  

    # Log for the pre-analysis 
    logAnalysisFD = file(PREANALYSIS_LOG_PATH, 'w')
    logLine       = paste0( "Preanalysis log start here.")
    write( logLine ,
           file = logAnalysisFD,
           append = FALSE)
  
  
    # The original data has IDs in the friendship network that doesn't exist in any
    # of the patient data. This is ok, but need to keep track of how many of those
    # are and report it later. The cleaning log keep track of that
    
    dataCleaningLogDF            = data.frame(matrix(NA, nrow = 6, ncol =3))
    colnames(dataCleaningLogDF)  = c("Concept", "Total", "Relative")
    dataCleaningLogDF$Concept[1] = "Total IDs:"
    dataCleaningLogDF$Concept[2] = "- Total Deleted IDs:"
    dataCleaningLogDF$Concept[3] = "- Total Remaining IDs:"
    dataCleaningLogDF$Concept[4] = "Total Edges:"
    dataCleaningLogDF$Concept[5] = " - Total Deleted Edges:"
    dataCleaningLogDF$Concept[6] = " - Total Remaining Edges:"

}

# Read the data into DFs
# -----------------------------------------------------------------------------
{
  
    # There are several files to read from, and many of them have redundant
    # information which is a bit of a pain. Here we keep track of what we read,
    # from where we read it, what do we have, and so on:
    #
    # -- The original Aureus file contain the FF1 Friendship, the FF1 Aureus data
    #    Some of the host data, some of the physical activity and some of the
    #    antibiotics data. (And all of this is mixed in the aureus data, you are
    #    starting to understand why this is so mixed and such a pain right?)
    #
    # -- Furthermore, in here there is no variable regarding the status or events
    #    of the samples. This is on the file with the hormonal contraceptives data
    #    with the SWAB_2 data, and FF11 data as well. The FF12 data is in the
    #    biomarkers file, but only the events and dates, not the actual staph 
    #    results which doesn't exist in any of the available files.
    #
    # -- The medical information here is incomplete, as we don't have the
    #    antibiotics 3 data, which is in the biomarker file for example. Some of
    #    the atibiotics 3 for FF12 is missing regardless.
    originalAureusTable      = read.csv(AUREUS_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
    # -- The original biomarkers file contain A LOT OF THINGS that we have missed
    #    during the first year, such as the biomarkers, the FF12 Friendship data,
    #    some of the blood status variables but not all of them (the rest is in
    #    the hormonal contraceptive data. I know, is total madness).
    #
    #    Here you have the complete itchy skin information regarding
    #    chronic diseases. The hormonal file doesn't have the start age or
    #    severity for example. But it has the part regarding psoriasis.
    originalBiomarkersTable  = read.csv(BIOMARKERS_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
    # -- The original hormonal contraceptives table. Aside everything else
    #    mentioned above so far, this file also contain dietary information, such
    #    as how many seagull eggs do you eat. Also for the record, the column
    #    order is kinda funny, you get menarche info, then diabetes, then alcohol,
    #    then pubic hair, then diet, then chronic diseases. What kind of derange
    #    lunatic makes this type of sorting, I can't even begin to imagine.
  
    # R is a horrible language, this code used to work fine, and then suddenly
    # don't. Then it got back working again. Seriously, Reading a CSV shouldn't
    # change over time for given updates or whatever other shenanigans you use
    # to do
    originalHormonalTable    = read.csv(HORMONAL_FILEPATH,  fileEncoding = "ISO-8859-14", stringsAsFactors = FALSE, sep = ";") 
                             # Thank you windows for destroying every standard of
                             # text encoding everywhere ever.
  
  
    # -- Around October 2021 I had to make a report of variables. I got sent a new
    #    dataset with many variables that I had before, and some which I didn't.
    #    So here it is to add to the collection of files we already have.
    originalReportTable      = read.csv(REPORT_FILEPATH,    fileEncoding = "UTF-8",       stringsAsFactors = FALSE)
  
  
    # -- In May 2022 we finally got the first set of FF2 variables, just
    #    basic antropometry.
    originalFF2Antropometry  = read.csv2(FF2ANTRO_FILEPATH, fileEncoding = "UTF-8",       stringsAsFactors = FALSE)
  
  
    # Final note, the date format is mix up across the tables. Sometimes is the
    # format is "dd/mm/yyyy", other time is "yyyy/mm/dd" , and finally, sometimes
    # is UNIX time (as all dates should be!)
  
    # ---- Count how many rows we have in the original data
    originalTotalRows   = nrow(originalAureusTable)
    FF1TotalRows        = nrow(originalAureusTable)
    FF2TotalRows        = nrow(originalFF2Antropometry)
  
    # Notice that we have a smaller set of people in FF2 than in FF1. However, we
    # init all the table of FF2 and beyond to same people, and leave the missing
    # information empty.
    
    # Check how many variables we have, this is pretty much why the data
    # cleaning document is gigantic
    allVariables = c(toupper(colnames(originalAureusTable)),
                     toupper(colnames(originalBiomarkersTable)),
                     toupper(colnames(originalHormonalTable)),
                     toupper(colnames(originalReportTable)),
                     toupper(colnames(originalFF2Antropometry)))
    
    totalUniques = length(unique(allVariables))
    
    
    
}


# ---- Read all the variables regardless of whether they are useful or not from
#      each file and add it to a more user friendly tables
#
#      For each table, we need to create an empty dataframe and start adding
#      columns. Is easy to make a garbage variable this way and delete it later.
#      For some reason, R doesn't like Nx0 DF and transform it to weirdness.
#
#      Furthermore, we don't create the relational tables with proper BCNF here,
#      that will happen in the next step. So the medicineTable is really just
#      a copypaste from all the sources.
{
  
    # Create dataframes
    {
        IDTable               = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        basicTable            = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        networkTechnicalTable = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        antropometricFF1Table = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        antropometricFF2Table = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        menstruationTable     = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        aureusTable           = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        swabbingTable         = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        highSchoolTable       = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        bloodTable            = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        bloodTechnicalTable   = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        sociologyTable        = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        frienshipTable        = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        pubertyMenTable       = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        pubertyWomenTable     = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        drugsTable            = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        sportsTable           = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        hygieneTable          = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        biomarkersTable       = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        dietTable             = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))   
        sleepTable            = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))       
                                
        diseaseTable          = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        medicineTable         = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        contraceptivesTable   = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        
        networkTable          = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        
        #questionariesTable    = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))
        #hospitalizationTable  = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 1))

    }

    # Rename the Auxiliar column
    # This is done because R doesn't know how to handle dataframe of 0 columns,
    # and need this hacking in order to work.
    {
        colnames(IDTable)               = c("Aux1")
        colnames(basicTable)            = c("Aux1")
        colnames(networkTechnicalTable) = c("Aux1")
        colnames(antropometricFF1Table) = c("Aux1")
        colnames(antropometricFF2Table) = c("Aux1")
        colnames(diseaseTable)          = c("Aux1")
        colnames(medicineTable)         = c("Aux1")
        colnames(contraceptivesTable)   = c("Aux1")
        colnames(menstruationTable)     = c("Aux1")
        colnames(networkTable)          = c("Aux1")
        colnames(aureusTable)           = c("Aux1")
        colnames(swabbingTable)         = c("Aux1")
        colnames(highSchoolTable)       = c("Aux1")
        colnames(bloodTable)            = c("Aux1")
        colnames(bloodTechnicalTable)   = c("Aux1")
        #colnames(questionariesTable)    = c("Aux1")
        colnames(sociologyTable)        = c("Aux1")
        colnames(frienshipTable)        = c("Aux1")
        colnames(pubertyMenTable)       = c("Aux1")
        colnames(pubertyWomenTable)     = c("Aux1")
        colnames(drugsTable)            = c("Aux1")
        colnames(sportsTable)           = c("Aux1")
        colnames(hygieneTable)          = c("Aux1")
        #colnames(hospitalizationTable)  = c("Aux1")
        colnames(biomarkersTable)       = c("Aux1") 
        colnames(dietTable)             = c("Aux1") 
        colnames(sleepTable)            = c("Aux1")     
    }


    # Add everything to each table. Remember we have these sources
    # Also, delete the garbage column that we need to create the dataframes     
    {
  
    # IDs (This table is not use anywhere, this piece of code is just to keep the whole code neat)
    IDTable$ID   = originalAureusTable$pers_key_ff1
    IDTable$Aux1 = NULL
    
    # @@Original Names Starts Here@@
    
    # --------------------------------
    # Basic
    #
    #     ID, Attendance dates, Sex, Age, General Health
    # --------------------------------
    {
        basicTable$ID                 = originalAureusTable$pers_key_ff1

        basicTable$AttendanceDateFF1  = originalBiomarkersTable$ATTENDANCE_DATE_FF1  # I have no idea what these variables are.
        basicTable$AttendanceDateFF12 = originalBiomarkersTable$ATTENDANCE_DATE_FF12 # In the documentation are the attendance date for FF1, FF12 literally
                                                                                     # but later on we have attendance date for the swabbing too
        
        basicTable$MedicationDateFF1  = originalBiomarkersTable$MEDICATION_DATE_FF1  # There is a medication time variable
                                                                                     # which is redundant as this is also
                                                                                     # included in the POSIX time
  
        
        basicTable$QuestionaryDateFF1 = originalBiomarkersTable$DATE_QUESTBACK_FF1   # When did we ask the question about the social network
        
        
        basicTable$Sex                = originalAureusTable$SEX_FF1
        basicTable$Age                = originalAureusTable$AGE_FF1
        basicTable$GeneralHealth      = originalReportTable$HEALTH_FF1
        
        
        basicTable$AgeFF12            = originalReportTable$AGE_FF12
        
        basicTable$Aux1               = NULL        
    }

    # --------------------------------
    # Social network technical
    #
    #     Technical comments and signatures about the social network
    # --------------------------------
    {
        
        networkTechnicalTable$ID = originalAureusTable$pers_key_ff1
    
        networkTechnicalTable$Signature = originalAureusTable$NETWORK_SIGNATURE_FF1
        networkTechnicalTable$Comment   = originalAureusTable$NETWORK_COMMENT_FF1
        
        networkTechnicalTable$SignatureFF12 = originalBiomarkersTable$NETWORK_SIGNATURE_FF12
        networkTechnicalTable$CommentFF12   = originalBiomarkersTable$NETWORK_CMNT_FF12
        
        # The comments for FF12 are badly formatted with waaaay too many spaces,
        # I'm going to correct that now.
        networkTechnicalTable$CommentFF12   = trimws(networkTechnicalTable$CommentFF12, which = c("both"))
        
        networkTechnicalTable$Aux1 = NULL    
        
    }

    # --------------------------------
    # Antropometric Tables
    #
    #    Everything that has to do with body sizes and DEXAs data
    #    (Although we don't have the DEXA data yet)
    # --------------------------------
    {
        antropometricFF1Table$ID     = originalAureusTable$pers_key_ff1
        
        antropometricFF1Table$Waist  = (originalHormonalTable$waist1_ff1 + originalHormonalTable$waist2_ff1) / 2
        antropometricFF1Table$Hip    = (originalHormonalTable$hip1_ff1   + originalHormonalTable$hip2_ff1)   / 2
        antropometricFF1Table$Height = originalBiomarkersTable$HEIGHT_FF1
        antropometricFF1Table$Weight = originalBiomarkersTable$WEIGHT_FF1
        antropometricFF1Table$BMI    = originalAureusTable$BMI_FF1
        antropometricFF1Table$HR     = originalReportTable$PULSE2_FF1
        antropometricFF1Table$SYSBP  = originalReportTable$SYSBP2_FF1
        antropometricFF1Table$DIABP  = originalReportTable$DIABP2_FF1
        
        antropometricFF1Table$Aux1   = NULL
        
        # The person ID is constant across tables, the info, might not be there
        # thou for all people. We need to keep track on who exist, who doesn't
        # and fill the table accordingly
        antropometricFF2Table = antropometricFF1Table
        FF2Candidates         = originalFF2Antropometry$PERS_KEY_FF1

        # Find out who is in FF2 or not
        FF2Included           = antropometricFF2Table$ID %in% FF2Candidates
        
        # Init the table to NA
        antropometricFF2Table$Waist  = NA
        antropometricFF2Table$Hip    = NA
        antropometricFF2Table$Height = NA
        antropometricFF2Table$Weight = NA
        antropometricFF2Table$BMI    = NA
        antropometricFF2Table$HR     = NA
        antropometricFF2Table$SYSBP  = NA
        antropometricFF2Table$DIABP  = NA
          
        
        # Fill the FF2 table
        for (i in 1:FF1TotalRows) {
        
            # If this person is on the list of people included in FF2
            if(FF2Included[i] == TRUE){

                # Find the correct index in the data
                candidateIndex = which(grepl(antropometricFF2Table$ID[i], originalFF2Antropometry$PERS_KEY_FF1))
                
                # Fill the data accordingly
                antropometricFF2Table$Waist[i]  = as.numeric((originalFF2Antropometry$WAIST1_FF2[candidateIndex] + originalFF2Antropometry$WAIST2_FF2[candidateIndex]) / 2)
                antropometricFF2Table$Hip[i]    = as.numeric((originalFF2Antropometry$HIP1_FF2[candidateIndex]   + originalFF2Antropometry$HIP2_FF2[candidateIndex])   / 2)
                antropometricFF2Table$Height[i] = as.numeric(originalFF2Antropometry$HEIGHT_FF2[candidateIndex])
                antropometricFF2Table$Weight[i] = as.numeric(originalFF2Antropometry$WEIGHT_FF2[candidateIndex])
                antropometricFF2Table$BMI[i]    = round(antropometricFF2Table$Weight[i]/(antropometricFF2Table$Height[i]/100)^2,2)  # The BMI info is not directly in the table, but we can find it
                #antropometricFF2Table$HR     = originalReportTable$PULSE2_FF1 # We don't have this info yet
                #antropometricFF2Table$SYSBP  = originalReportTable$SYSBP2_FF1
                #antropometricFF2Table$DIABP  = originalReportTable$DIABP2_FF1
                                
            }
            
        }
        
        
        antropometricFF2Table$Aux1   = NULL
        
    }

    # --------------------------------
    # Menstruation Women
    #     - Notice that information about menarche goes into Puberty table
    #
    # This table is important if you want to filter by menstruating women only
    # which is what we use to discriminated upon when talking about women
    # using contraceptives.
    # --------------------------------
    {
        
        menstruationTable$ID                  = originalAureusTable$pers_key_ff1
        menstruationTable$Date                = originalBiomarkersTable$WOMEN_DATE_FF1
        menstruationTable$MenstruationStart   = originalHormonalTable$menses_ff1
        menstruationTable$MenstruationRegular = originalHormonalTable$menses_regularity_ff1
        menstruationTable$MenstruationCycle   = originalHormonalTable$menses_cycle_length_ff1
        # There is also the date certain variable, but we skip that as it is implicitly coded in the menstruation date
        menstruationTable$MenstruationDate    = originalHormonalTable$menses_start_date_ff1
        
        
        menstruationTable$Aux1 = NULL
    }    

    # --------------------------------
    # S.Aureus table
    # 
    #     Here we keep track of the carrier information.
    # --------------------------------
    {

        #  Basic stuff
        aureusTable$ID                       = originalAureusTable$pers_key_ff1
        

        
        
        # Dates
        # -- Dates of attendance
        aureusTable$S1_AttendanceDate        = originalBiomarkersTable$SWAB_DATE_FF1
        aureusTable$S1_R2_AttendanceDate     = originalReportTable$SWAB_DATE_2_FF1
        aureusTable$S2_AttendanceDate        = originalBiomarkersTable$SWAB_DATE_FF12
        # -- Dates of culture
        aureusTable$S1_CultureDate           = originalBiomarkersTable$DATE_CULTURE_DAY0_FF1
        aureusTable$S2_CultureDate           = NA

        
        # The experiment grew something in the agar plate
        aureusTable$S1_BacterialNasalGrowth  = originalAureusTable$CONTROL_NASAL_DAY2_FF1      # Did something grew in the nose
        aureusTable$S1_BacterialThroatGrowth = originalAureusTable$CONTROL_THROAT_DAY2_FF1     # Did something grew in the throat 
        aureusTable$S2_BacterialNasalGrowth  = originalHormonalTable$control_nasal_day2_ff11   
        aureusTable$S2_BacterialThroatGrowth = originalHormonalTable$control_throat_day2_ff11  

        # The experiment grew SA in the agar plate (Direct Culture)
        aureusTable$S1_SA_Direct_NasalGrowth        = originalAureusTable$STAPH_NASAL_DAY2_FF1         # Did SA grew in the nose (yes/no)
        aureusTable$S1_SA_Direct_ThroatGrowth       = originalAureusTable$STAPH_THROAT_DAY2_FF1        # Did SA grew in the throat (yes/no)
        aureusTable$S1_SA_Direct_NasalPopulation    = originalAureusTable$STAPH_GROWTH_NASAL_DAY2_FF1  # How much SA grew in the nose (none, little, much)
        aureusTable$S1_SA_Direct_ThroatPopulation   = originalAureusTable$STAPH_GROWTH_THROAT_DAY2_FF1 # How much SA grew in the throat (none, little, much)
        aureusTable$S2_SA_Direct_NasalGrowth        = originalHormonalTable$staph_nasal_day2_ff11 
        aureusTable$S2_SA_Direct_ThroatGrowth       = originalHormonalTable$staph_throat_day2_ff11
        aureusTable$S2_SA_Direct_NasalPopulation    = originalHormonalTable$staph_growth_nasal_day2_ff11
        aureusTable$S2_SA_Direct_ThroatPopulation   = originalHormonalTable$staph_growth_throat_day2_ff11

        # The experiment grew SA, and we try to make it grow even more (Enrichment Broth)
        aureusTable$S1_SA_Enrich_NasalGrowth        = originalAureusTable$STAPH_NASAL_ENRICH_FF1       # Same as before, but with the enrichment broth
        aureusTable$S1_SA_Enrich_ThroatGrowth       = originalAureusTable$STAPH_THROAT_ENRICH_FF1
        aureusTable$S1_SA_Enrich_NasalPopulation    = originalAureusTable$STAPH_GROWTH_NASAL_ENRICH_FF1
        aureusTable$S1_SA_Enrich_ThroatPopulation   = originalAureusTable$STAPH_GROWTH_THROAT_ENRICH_FF1
        aureusTable$S2_SA_Enrich_NasalGrowth        = originalHormonalTable$staph_nasal_enrich_ff11
        aureusTable$S2_SA_Enrich_ThroatGrowth       = originalHormonalTable$staph_throat_enrich_ff11
        aureusTable$S2_SA_Enrich_NasalPopulation    = originalHormonalTable$staph_growth_nasal_enrich_ff11
        aureusTable$S2_SA_Enrich_ThroatPopulation   = originalHormonalTable$staph_growth_throat_enrich_ff11
        
        # Coagulase test to check for the presence of SA (positive) or S.Epidermitis or S.Saprophyticus.
        aureusTable$S1_Direct_CoagulaseNasal        = originalAureusTable$STAPH_COAGULASE_NASAL_FF1
        aureusTable$S1_Direct_CoagulaseThroat       = originalAureusTable$STAPH_COAGULASE_THROAT_FF1
        aureusTable$S1_Enrich_CoagulaseNasal        = originalAureusTable$STAPH_COAG_NASAL_ENRICH_FF1
        aureusTable$S1_Enrich_CoagulaseThroat       = originalAureusTable$STAPH_COAG_THROAT_ENRICH_FF1
        aureusTable$S2_Direct_CoagulaseNasal        = originalHormonalTable$staph_coagulase_nasal_ff11
        aureusTable$S2_Direct_CoagulaseThroat       = originalHormonalTable$staph_coagulase_throat_ff11
        aureusTable$S2_Enrich_CoagulaseNasal        = originalHormonalTable$staph_coag_nasal_enrich_ff11
        aureusTable$S2_Enrich_CoagulaseThroat       = originalHormonalTable$staph_coag_throat_enrich_ff11
        
        # SPA-Typing variables
        aureusTable$SPANasal1                       = originalAureusTable$SPA_NASAL1_FF1
        aureusTable$SPANasal2                       = originalAureusTable$SPA_NASAL2_FF1
        aureusTable$SPAThroat1                      = originalAureusTable$SPA_THROAT1_FF1
        aureusTable$SPAThroat2                      = originalAureusTable$SPA_THROAT2_FF1
        aureusTable$SPAThroatClonning               = originalAureusTable$CC_THROAT1_FF1
        aureusTable$SPAThroatCount                  = originalAureusTable$CCN_THROAT1_FF1


      
        
        aureusTable$Aux1   = NULL        
    }    
    
    
    # --------------------------------
    # Swabbing technical table
    # 
    #     Here we keep track of whether something happens during the swabbing
    # --------------------------------
    {
        
        # ID
        swabbingTable$ID = originalAureusTable$pers_key_ff1

        # Information about trying to do the swabbing for the first time
        # -- For S1
        swabbingTable$S1_NasalOK                 = originalBiomarkersTable$NASAL_SAMPLE_OK_FF1
        swabbingTable$S1_ThroatOK	             = originalBiomarkersTable$THROAT_SAMPLE_OK_FF1	
        
        # S1 Lab comments for Day 0 and Day 2 are literally just "OK" and coincide with S1 Nasal and Throat
        # Since there is no extra info here, is just skip it.
        
        #swabbingTable$S1_LabComments_Day0	     = originalBiomarkersTable$LAB_COMMENTS_DAY0_FF1					
        #swabbingTable$S1_LabComments_Day2	     = originalBiomarkersTable$LAB_COMMENTS_DAY2_FF1		
        
        # Registering enrich and staph comments, and deleting extra spaces
        swabbingTable$S1_LabComments_Enrich	     = originalBiomarkersTable$LAB_COMMENTS_ENRICH_FF1				
        swabbingTable$S1_LabComments_Staph       = originalBiomarkersTable$COMMENTS_STAPH_FF1
        swabbingTable$S1_LabComments_Enrich      = trimws(swabbingTable$S1_LabComments_Enrich, which = c("both"))
        swabbingTable$S1_LabComments_Staph       = trimws(swabbingTable$S1_LabComments_Staph,  which = c("both"))
        
        
        # These comments are fine, no need for addjustment
        swabbingTable$S1_Performed               = originalHormonalTable$status_swab_ff1
        swabbingTable$S1_Event                   = originalHormonalTable$event_swab_ff1
        
        swabbingTable$S1_Medical_Event           = originalHormonalTable$event_swab_med_ff1
        swabbingTable$S1_Medical_Event_Comment   = originalHormonalTable$event_swab_med_comment_ff1
        
        swabbingTable$S1_Technical_Event         = originalHormonalTable$event_swab_tech_ff1
        swabbingTable$S1_Technical_Event_Comment = originalHormonalTable$event_swab_tech_comment_ff1
        
        swabbingTable$S1_Abort_Event             = originalHormonalTable$event_swab_abort_ff1
        swabbingTable$S1_Abort_Event_Comment     = originalHormonalTable$event_swab_abort_comment_ff1
        
        swabbingTable$S1_Other_Event             = originalHormonalTable$event_swab_other_ff1
        swabbingTable$S1_Other_Event_Comment     = originalHormonalTable$event_swab_other_comment_ff1
        # -- For S2
        
        # These comments are not fine, so deleting spaces
        swabbingTable$S2_Performed               = originalReportTable$STATUS_SWAB_FF12
        swabbingTable$S2_Event                   = originalReportTable$EVENT_SWAB_FF12
        
        swabbingTable$S2_Medical_Event           = originalReportTable$EVENT_SWAB_MED_FF12
        swabbingTable$S2_Medical_Event_Comment   = originalReportTable$EVENT_SWAB_MED_CMNT_FF12
        swabbingTable$S2_Medical_Event_Comment   = trimws(swabbingTable$S2_Medical_Event_Comment,   which = c("both"))
        
        swabbingTable$S2_Technical_Event         = originalReportTable$EVENT_SWAB_TECH_FF12
        swabbingTable$S2_Technical_Event_Comment = originalReportTable$EVENT_SWAB_TECH_CMNT_FF12
        swabbingTable$S2_Technical_Event_Comment = trimws(swabbingTable$S2_Technical_Event_Comment, which = c("both"))
        
        swabbingTable$S2_Abort_Event             = originalReportTable$EVENT_SWAB_ABORT_FF12
        swabbingTable$S2_Abort_Event_Comment     = originalReportTable$EVENT_SWAB_ABORT_CMNT_FF12
        swabbingTable$S2_Abort_Event_Comment     = trimws(swabbingTable$S2_Abort_Event_Comment,     which = c("both"))
        
        swabbingTable$S2_Other_Event             = originalReportTable$EVENT_SWAB_OTHER_FF12
        swabbingTable$S2_Other_Event_Comment     = originalReportTable$EVENT_SWAB_OTHER_CMNT_FF12
        swabbingTable$S2_Other_Event_Comment     = trimws(swabbingTable$S2_Other_Event_Comment,     which = c("both"))
        
        # -- For S3? FF11 and FF12????
        # Sometimes FF11 and FF12 are mixed
        
        # Information about trying to do the swabbing for the second time
        
        # Was the first swabbing, repeated, and if so, what happens
        # -- For S1
        swabbingTable$S1_Repeated_Performed               = originalHormonalTable$status_swab_2_ff1
        swabbingTable$S1_Nose_Repeated		              = originalHormonalTable$swab_nose_2_ff1				
        swabbingTable$S1_Throat_Repeated                  = originalHormonalTable$swab_throat_2_ff1
        
        swabbingTable$S1_Repeated_Event                   = originalReportTable$EVENT_SWAB_2_FF1
        
        swabbingTable$S1_Repeated_Medical_Event           = originalReportTable$EVENT_SWAB_MED_2_FF1
        swabbingTable$S1_Repeated_Medical_Event_Comment   = originalReportTable$EVENT_SWAB_MED_COMMENT_2_FF1
        swabbingTable$S1_Repeated_Medical_Event_Comment   = trimws(swabbingTable$S1_Repeated_Medical_Event_Comment,     which = c("both"))
        
        swabbingTable$S1_Repeated_Technical_Event         = originalReportTable$EVENT_SWAB_TECH_2_FF1
        swabbingTable$S1_Repeated_Technical_Event_Comment = originalReportTable$EVENT_SWAB_TECH_COMMENT_2_FF1
        swabbingTable$S1_Repeated_Technical_Event_Comment = trimws(swabbingTable$S1_Repeated_Technical_Event_Comment,   which = c("both"))
        
        swabbingTable$S1_Repeated_Abort_Event             = originalReportTable$EVENT_SWAB_ABORT_2_FF1
        swabbingTable$S1_Repeated_Abort_Event_Comment     = originalReportTable$EVENT_SWAB_ABORT_COMMENT_2_FF1
        swabbingTable$S1_Repeated_Abort_Event_Comment     = trimws(swabbingTable$S1_Repeated_Abort_Event_Comment,       which = c("both"))
        
        swabbingTable$S1_Repeated_Other_Event             = originalReportTable$EVENT_SWAB_OTHER_2_FF1
        swabbingTable$S1_Repeated_Other_Event_Comment     = originalReportTable$EVENT_SWAB_OTHER_COMMENT_2_FF1
        swabbingTable$S1_Repeated_Other_Event_Comment     = trimws(swabbingTable$S1_Repeated_Other_Event_Comment,       which = c("both"))
        
        # -- For S2
        
        
        # Freezing information
        # ---- Dates of freezing
        swabbingTable$S1_Nasal_FreezeDate  = originalBiomarkersTable$DATE_FREEZE_STAPH_NASAL_FF1
        swabbingTable$S1_Throat_FreezeDate = originalBiomarkersTable$DATE_FREEZE_STAPH_THROAT_FF1
        # ---- Freezer variables (we use these for redundancy later)
        swabbingTable$S1_FreezerNasalID    = originalHormonalTable$freeze_number_staph_nasal_ff1
        swabbingTable$S1_FreezerThroatID   = originalHormonalTable$freeze_number_staph_throat_ff1
        swabbingTable$S2_FreezerNasalID    = originalHormonalTable$freeze_number_staph_nasal_ff11
        swabbingTable$S2_FreezerThroatID   = originalHormonalTable$freeze_number_staph_throat_ff11
        
        	
        
        swabbingTable$Aux1   = NULL        
    }
    
    # --------------------------------
    # Highschool information
    # --------------------------------
    {
        
        highSchoolTable$ID = originalAureusTable$pers_key_ff1
        
        highSchoolTable$HighSchool   = originalAureusTable$HIGH_SCHOOL_NAME_FF1
        highSchoolTable$Class        = originalAureusTable$HIGH_SCHOOL_CLASS_FF1
        highSchoolTable$Programme    = originalAureusTable$HIGH_SCHOOL_PROGRAMME_FF1
        highSchoolTable$MainPrograme = originalAureusTable$HIGH_SCHOOL_MAIN_PROGRAM_FF1
        
        highSchoolTable$Aux1 = NULL

    }
    
    # --------------------------------
    # Blood Table
    #
    #     Everything that has to do with levels of markers in blood that are
    #     not the inflamatory biomarkers.
    # --------------------------------
    {
        # ---- Basic Info
        bloodTable$ID                 = originalAureusTable$pers_key_ff1
        bloodTable$BloodAnalysisDate  = originalBiomarkersTable$BLOOD_ANALYSIS_DATE_FF1  # Date of blood test
        bloodTable$PlasmaAnalysisDate = originalBiomarkersTable$PTH_ANALYSIS_DATE_FF1    # Date of plasma test    
        bloodTable$Time_Since_Eating  = originalBiomarkersTable$TIME_LAST_MEAL_FF1       # When did you eat last time
        


        # ---- Events variables
        #
        #      There are a bunch of columns named EVENT0 to EVENT9, and EVENTA
        #      to EVENTL, which are empty and have no information at all. So I
        #      will skip reading all of those.
        
        # ---- Main blood results
        #
        #      There are a bunch of variables saying for example:
        #
        #      Estradiol Bellow Limit = TRUE / FALSE
        #
        #      These are not register as we can define whatever we want later as
        #      to what is an acceptable limit or not
        bloodTable$`Mean_Corposcular_Hemoglobin_(pg)`                 = originalBiomarkersTable$MCH_FF1
        bloodTable$`Mean_Corposcular_Hemoglobin_Concentration_(g/dL)` = originalBiomarkersTable$MCHC_FF1
        bloodTable$`Mean_Corposcular_Hemoglobin_Volume_(fl)`          = originalBiomarkersTable$MCV_FF1
        bloodTable$`Fe_(µmol/L)`                  = originalBiomarkersTable$FE_FF1
        bloodTable$`Ferritinin_(ug/L)`            = originalBiomarkersTable$FERRITIN_FF1
        bloodTable$`Transferrin_(g/L)`            = originalBiomarkersTable$TRANSFERRIN_FF1
        bloodTable$`Total_cholesterol_(mmol/L)`   = originalBiomarkersTable$CHOLESTEROL_FF1
        bloodTable$`Triglycerides_(mmol/L)`       = originalBiomarkersTable$TRIGLYCERIDES_FF1
        bloodTable$`LDL_cholesterol_(mmol/L)`     = originalBiomarkersTable$LDL_FF1
        bloodTable$`HDL_cholesterol_(mmol/L)`     = originalBiomarkersTable$HDL_FF1
        bloodTable$`Calcium_(mmol/L)`             = originalBiomarkersTable$CALCIUM_FF1
        bloodTable$`High-sensitive_CRP`           = originalBiomarkersTable$S_CRP_S_FF1
        bloodTable$`Apolipoprotein_A1_(g/L)`      = originalBiomarkersTable$APO_A_FF1
        bloodTable$`Apolipoprotein_B_(g/L)`       = originalBiomarkersTable$APO_B_FF1
        bloodTable$`Estradiol_E2_(nmol/L)`        = originalHormonalTable$s_estradiol_ff1
        bloodTable$`Progesterone_(nmol/L)`        = originalHormonalTable$s_progesterone_ff1
        bloodTable$`Testosterone_(nmol/L)`        = originalHormonalTable$s_testosterone_ff1
        bloodTable$`DHEA_SO4_(µmol/L)`            = originalBiomarkersTable$S_DHEA_SO4_FF1
        bloodTable$`SHBG_(nmol/L)`                = originalHormonalTable$s_shbg_ff1
        bloodTable$`LH_(IU/L)`                    = originalHormonalTable$s_lh_ff1
        bloodTable$`FSH_(IU/L)`                   = originalHormonalTable$s_fsh_ff1
        bloodTable$`Glucose_Non-fasting_(mmol/L)` = originalBiomarkersTable$GLUCOSE_FF1
        bloodTable$`HBA1C_(%)`                    = originalHormonalTable$s_hba1c_ff1
        bloodTable$`Haemoglobin_(g/dL)`           = originalBiomarkersTable$HAEMOGLOBIN_FF1
        bloodTable$`Albumin_(g/L)`                = originalHormonalTable$albumin_ff1
        bloodTable$`25(OH)D_(nmol/L)`             = originalHormonalTable$s_25_vitd_ff1
        bloodTable$`Retinol_(µmol/L)`             = originalBiomarkersTable$S_RETINOL_FF1
        bloodTable$`PTH_(pmol/L)`                 = originalBiomarkersTable$PTH_FF1
        
        # ---- Fatty Acids
        bloodTable$`FA_C12:0_(mcg/ml)`       = originalBiomarkersTable$S_FA_C12_0_FF1
        bloodTable$`FA_C14:0_(mcg/ml)`       = originalBiomarkersTable$S_FA_C14_0_FF1
        bloodTable$`FA_C15:0_(mcg/ml)`       = originalBiomarkersTable$S_FA_C15_0_FF1
        bloodTable$`FA_C16:0_(mcg/ml)`       = originalBiomarkersTable$S_FA_C16_0_FF1
        bloodTable$`FA_C16:1_n-7_(mcg/ml)`   = originalBiomarkersTable$S_FA_C16_1_N7_FF1
        bloodTable$`FA_C18:0_(mcg/ml)`       = originalBiomarkersTable$S_FA_C18_0_FF1
        bloodTable$`FA_C18:1_t6-11_(mcg/ml)` = originalBiomarkersTable$S_FA_C18_1_T6_11_FF1
        bloodTable$`FA_C18:1_c-9_(mcg/ml)`   = originalBiomarkersTable$S_FA_C18_1_C9_FF1
        bloodTable$`FA_C18:1_c-11(mcg/ml)`   = originalBiomarkersTable$S_FA_C18_1_C11_FF1
        bloodTable$`FA_C18:2_n-6_(mcg/ml)`   = originalBiomarkersTable$S_FA_C18_2_N6_FF1
        bloodTable$`FA_C20:0_(mcg/ml)`       = originalBiomarkersTable$S_FA_C20_0_FF1
        bloodTable$`FA_C18:3_n-6_(mcg/ml)`   = originalBiomarkersTable$S_FA_C18_3_N6_FF1
        bloodTable$`FA_C18:3_n-3_(mcg/ml)`   = originalBiomarkersTable$S_FA_C18_3_N3_FF1
        bloodTable$`FA_C20:1_n-9_(mcg/ml)`   = originalBiomarkersTable$S_FA_C20_1_N9_FF1
        bloodTable$`FA_C20:2_n-6_(mcg/ml)`   = originalBiomarkersTable$S_FA_C20_2_N6_FF1
        bloodTable$`FA_C22:0_(mcg/ml)`       = originalBiomarkersTable$S_FA_C22_0_FF1
        bloodTable$`FA_C20:3_n-6_(mcg/ml)`   = originalBiomarkersTable$S_FA_C20_3_N6_FF1
        bloodTable$`FA_C20:4_n-6_(mcg/ml)`   = originalBiomarkersTable$S_FA_C20_4_N6_FF1
        bloodTable$`FA_C23:0_(mcg/ml)`       = originalBiomarkersTable$S_FA_C23_0_FF1
        bloodTable$`FA_C20:5_n-3_(mcg/ml)`   = originalBiomarkersTable$S_FA_C20_5_N3_FF1
        bloodTable$`FA_C24:0_(mcg/ml)`       = originalBiomarkersTable$S_FA_C24_0_FF1
        bloodTable$`FA_C24:1_(mcg/ml)`       = originalBiomarkersTable$S_FA_C24_1_FF1
        bloodTable$`FA_C22:5_n-3_(mcg/ml)`   = originalBiomarkersTable$S_FA_C22_5_N3_FF1
        bloodTable$`FA_C22:6_n-3_(mcg/ml)`   = originalBiomarkersTable$S_FA_C22_6_N3_FF1

        bloodTable$`FA_C12:0_(weight%)`       = originalBiomarkersTable$S_FA_C12_0_PERCENT_FF1
        bloodTable$`FA_C14:0_(weight%)`       = originalBiomarkersTable$S_FA_C14_0_PERCENT_FF1
        bloodTable$`FA_C15:0_(weight%)`       = originalBiomarkersTable$S_FA_C15_0_PERCENT_FF1
        bloodTable$`FA_C16:0_(weight%)`       = originalBiomarkersTable$S_FA_C16_0_PERCENT_FF1
        bloodTable$`FA_C16:1_n-7_(weight%)`   = originalBiomarkersTable$S_FA_C16_1_N7_PERCENT_FF1
        bloodTable$`FA_C18:0_(weight%)`       = originalBiomarkersTable$S_FA_C18_0_PERCENT_FF1
        bloodTable$`FA_C18:1_t6-11_(weight%)` = originalBiomarkersTable$S_FA_C18_1_T6_11_PERCENT_FF1
        bloodTable$`FA_C18:1_c-9_(weight%)`   = originalBiomarkersTable$S_FA_C18_1_C9_PERCENT_FF1
        bloodTable$`FA_C18:1_c-11(weight%)`   = originalBiomarkersTable$S_FA_C18_1_C11_PERCENT_FF1
        bloodTable$`FA_C18:2_n-6_(weight%)`   = originalBiomarkersTable$S_FA_C18_2_N6_PERCENT_FF1
        bloodTable$`FA_C20:0_(weight%)`       = originalBiomarkersTable$S_FA_C20_0_PERCENT_FF1
        bloodTable$`FA_C18:3_n-6_(weight%)`   = originalBiomarkersTable$S_FA_C18_3_N6_PERCENT_FF1
        bloodTable$`FA_C18:3_n-3_(weight%)`   = originalBiomarkersTable$S_FA_C18_3_N3_PERCENT_FF1
        bloodTable$`FA_C20:1_n-9_(weight%)`   = originalBiomarkersTable$S_FA_C20_1_N9_PERCENT_FF1
        bloodTable$`FA_C20:2_n-6_(weight%)`   = originalBiomarkersTable$S_FA_C20_2_N6_PERCENT_FF1
        bloodTable$`FA_C22:0_(weight%)`       = originalBiomarkersTable$S_FA_C22_0_PERCENT_FF1
        bloodTable$`FA_C20:3_n-6_(weight%)`   = originalBiomarkersTable$S_FA_C20_3_N6_PERCENT_FF1
        bloodTable$`FA_C20:4_n-6_(weight%)`   = originalBiomarkersTable$S_FA_C20_4_N6_PERCENT_FF1
        bloodTable$`FA_C23:0_(weight%)`       = originalBiomarkersTable$S_FA_C23_0_PERCENT_FF1
        bloodTable$`FA_C20:5_n-3_(weight%)`   = originalBiomarkersTable$S_FA_C20_5_N3_PERCENT_FF1
        bloodTable$`FA_C24:0_(weight%)`       = originalBiomarkersTable$S_FA_C24_0_PERCENT_FF1
        bloodTable$`FA_C24:1_(weight%)`       = originalBiomarkersTable$S_FA_C24_1_PERCENT_FF1
        bloodTable$`FA_C22:5_n-3_(weight%)`   = originalBiomarkersTable$S_FA_C22_5_N3_PERCENT_FF1
        bloodTable$`FA_C22:6_n-3_(weight%)`   = originalBiomarkersTable$S_FA_C22_6_N3_PERCENT_FF1    

        # ---- LCMSMS (Mass spectromety measures). They have the same value as
        #      their original counterpart, as they don't present anything of
        #      interest, they are not included.
     
        bloodTable$Aux1 = NULL
    }
    
    # --------------------------------
    # Blood technical table
    # 
    #     Here we keep track of whether something happens during the blood
    #     extraction (or plama extraction).
    # --------------------------------
    {
        
        bloodTechnicalTable$ID   = originalAureusTable$pers_key_ff1
        
        # ---- Status variables
        bloodTechnicalTable$BloodTestPerformed         = originalHormonalTable$status_blood_ff1           # Did we extract blood? Yes, No, or Something weird happens
        bloodTechnicalTable$BloodEvent                 = originalHormonalTable$event_blood_ff1
        bloodTechnicalTable$BloodMedicalEvent          = originalHormonalTable$event_blood_med_ff1
        bloodTechnicalTable$BloodMedicalComment        = originalHormonalTable$event_blood_med_comment_ff1
        bloodTechnicalTable$BloodTechnicalEvent        = originalHormonalTable$event_blood_tech_ff1
        bloodTechnicalTable$BloodTechnicalComment      = originalHormonalTable$event_blood_tech_comment_ff1
        bloodTechnicalTable$BloodAborted               = originalHormonalTable$event_blood_abort_ff1
        bloodTechnicalTable$BloodAbortedComment        = originalHormonalTable$event_blood_abort_comment_ff1
        bloodTechnicalTable$BloodOther                 = originalHormonalTable$event_blood_other_ff1
        bloodTechnicalTable$BloodOtherComment          = originalHormonalTable$event_blood_other_comment_ff1
        bloodTechnicalTable$Blood_S25OH_Event          = originalBiomarkersTable$EVENT_S_25_VITD_FF1
        bloodTechnicalTable$Blood_S25OH_EventComment   = originalBiomarkersTable$EVENT_S_25_VITD_CMNT_FF1
        bloodTechnicalTable$Blood_Retinol_Event        = originalBiomarkersTable$EVENT_RETINOL_FF1
        bloodTechnicalTable$Blood_Retinol_EventComment = originalBiomarkersTable$EVENT_RETINOL_CMNT_FF1
        			
        # Correct all comments for weird spaces
        bloodTechnicalTable$BloodMedicalComment        = trimws(bloodTechnicalTable$BloodMedicalComment,        which = c("both"))
        bloodTechnicalTable$BloodTechnicalComment      = trimws(bloodTechnicalTable$BloodTechnicalComment,      which = c("both"))
        bloodTechnicalTable$BloodAbortedComment        = trimws(bloodTechnicalTable$BloodAbortedComment,        which = c("both"))
        bloodTechnicalTable$BloodOtherComment          = trimws(bloodTechnicalTable$BloodOtherComment,          which = c("both"))
        bloodTechnicalTable$Blood_S25OH_EventComment   = trimws(bloodTechnicalTable$Blood_S25OH_EventComment,   which = c("both"))
        bloodTechnicalTable$Blood_Retinol_EventComment = trimws(bloodTechnicalTable$Blood_Retinol_EventComment, which = c("both"))
        	
        bloodTechnicalTable$Aux1 = NULL
    }

    # --------------------------------
    # Sociology
    # --------------------------------
    {
     
        # The socioligy questionary is horribly design :(
        
        # The data doesn't account for same sex cohabitants, notify that
        # somewhere. Proper questionary would be something like:
        #
        # How many tutors do you have?
        # If the answer is 2 or less, fill this...
        # If the answer is 3 or more, fill this manually and explain your situation
        #
        # Basically, you could live with both your divorced parents independently
        # and this doesn't account for that.
        #
        # It doesn't account for bizarre poliamoruous hippy families neither
        
        # The data doesn't account (I think) for adoptive and foster parents
        # properly, they have their own category. However, most adopted people
        # would testify that their adoptive parents are their parents regardless
        
        # It also doesn't differienciate between where do you live, and with
        # who do you live. For example,
        #
        #    "Who do you live now: In an institution?"
        #
        # You can still live with somebody, in an institution, even your parents
        # if they are poor and got governmental houses or whatever
        
        # A tutor, need to be full time job, part time job, unemployed, disabled
        # or domestic (housewife). Things is, you can be multiple combinations
        # of those.
        #
        #     Full time job mother           (typical)
        #     Disable father part time job
        #     Unemployed doing housewife
        #     Part time job doing housewife
        #     Unemployed not doing housewife (parasite!)
        #     and so on...
        #
        # Is also not clear whether attending school or not is also a job option
        
        # Very bad design, number of siblings is:
        #
        # Do you live with...?
        # 1 to 2 (yes no)
        # 3 or more (yes no)
        #
        # you could have easily define
        #
        # Number of siblings:         <Actual Number>
        # Number of siblings at home: <Actual Number> 
        
        # Ethnicity, Race, and Nationality are mixed.
        # 
        # The question is:
        #
        #    "Do you consider yourself to be Norwegian?"
        #
        # Where you have answers like Norwegian-Somalian
        #
        # So, your ethnicity can be Irish, your race Black, and your nationality
        # can be Korean or whatever. Those are separated concepts but they are
        # sort of mixed up given the awnsers of the question.
        #
        # There are also nationalistic people who are Sami but not Norwegian
        # even though they are born in Norway. Other are not even Sami, but
        # "Nord-Norsk!" and not norwegian.
        #
        # So once thing is actual, legal, nationality, and other is whatever
        # your heart tells you where you belong.
        
        sociologyTable$ID                        = originalAureusTable$pers_key_ff1
        
        sociologyTable$Live_With_Mother          = originalBiomarkersTable$HOUSHOLD_MOTHER_FF1
        sociologyTable$Live_With_Father          = originalBiomarkersTable$HOUSHOLD_FATHER_FF1
        sociologyTable$Live_With_Zero_S          = "No" # We need to convert this variable later
        sociologyTable$Live_With_12_S            = originalBiomarkersTable$HOUSHOLD_SIBS1TO2_FF1
        sociologyTable$Live_With_3m_S            = originalBiomarkersTable$HOUSHOLD_SIBS3PLUS_FF1
        sociologyTable$Live_With_Stepfather      = originalBiomarkersTable$HOUSHOLD_STEPFATHER_FF1
        sociologyTable$Live_With_Stepmother      = originalBiomarkersTable$HOUSHOLD_STEPMOTHER_FF1
        sociologyTable$Live_With_Fosterparents   = originalBiomarkersTable$HOUSHOLD_FOSTERPARENT_FF1
        sociologyTable$Live_With_Adoptiveparents = originalBiomarkersTable$HOUSHOLD_ADOPTIVEPARENT_FF1
        sociologyTable$Live_With_Grandparents    = originalBiomarkersTable$HOUSHOLD_GRANDPARENTS_FF1
        sociologyTable$Live_With_Friends         = originalBiomarkersTable$HOUSHOLD_FRIENDS_FF1
        sociologyTable$Live_With_Nobody          = originalBiomarkersTable$HOUSHOLD_ALONE_FF1
        sociologyTable$Live_With_Institution     = originalBiomarkersTable$HOUSHOLD_INSTITUTION_FF1
        sociologyTable$Live_With_Other           = originalBiomarkersTable$HOUSHOLD_OTHER_FF1
        
        sociologyTable$When_Left_Home            = originalBiomarkersTable$LEFT_HOME_WHEN_FF1
        
        
        
        # The variables are divided into whatever the questionary said, and the
        # reconstruction of the data that we do.
        sociologyTable$Mother_WorkTime           = "Didn't Awnser"   # {Full, Partial, Pensioned, Unemployed, Deceased, Other, Don't know, Didn't Awnser} you can only be one. If more than one, you get the first from the left
        sociologyTable$Mother_Studying           = "No"              # {Yes, No}
        sociologyTable$Mother_Domestic           = "No"              # {Yes, No}
        sociologyTable$Mother_Disable            = "No"              # {Yes, No}
        
        sociologyTable$Mother_Work_Fulltime      = originalBiomarkersTable$MOTHER_WORK_FULL_TIME_FF1
        sociologyTable$Mother_Work_Parttime      = originalBiomarkersTable$MOTHER_WORK_PART_TIME_FF1
        sociologyTable$Mother_Work_Unemployed    = originalBiomarkersTable$MOTHER_WORK_UNEMPLOYED_FF1
        sociologyTable$Mother_Work_Disabled      = originalBiomarkersTable$MOTHER_WORK_DISABLED_FF1
        sociologyTable$Mother_Work_Domestic      = originalBiomarkersTable$MOTHER_WORK_DOMESTIC_FF1
        sociologyTable$Mother_Work_School        = originalBiomarkersTable$MOTHER_WORK_SCHOOL_FF1
        sociologyTable$Mother_Work_Pensioned     = originalBiomarkersTable$MOTHER_WORK_PENSIONED_FF1
        sociologyTable$Mother_Work_Deceased      = originalBiomarkersTable$MOTHER_WORK_DECEASED_FF1
        sociologyTable$Mother_Work_DontKnown     = originalBiomarkersTable$MOTHER_WORK_DONT_KNOW_FF1
        sociologyTable$Mother_Work_Other         = originalBiomarkersTable$MOTHER_WORK_OTHER_FF1
        
        sociologyTable$Father_WorkTime           = "Didn't Awnser"   
        sociologyTable$Father_Studying           = "No" 
        sociologyTable$Father_Domestic           = "No" 
        sociologyTable$Father_Disable            = "No"
        
        sociologyTable$Father_Work_Fulltime      = originalBiomarkersTable$FATHER_WORK_FULL_TIME_FF1
        sociologyTable$Father_Work_Parttime      = originalBiomarkersTable$FATHER_WORK_PART_TIME_FF1
        sociologyTable$Father_Work_Unemployed    = originalBiomarkersTable$FATHER_WORK_UNEMPLOYED_FF1
        sociologyTable$Father_Work_Disabled      = originalBiomarkersTable$FATHER_WORK_DISABLED_FF1
        sociologyTable$Father_Work_Domestic      = originalBiomarkersTable$FATHER_WORK_DOMESTIC_FF1
        sociologyTable$Father_Work_School        = originalBiomarkersTable$FATHER_WORK_SCHOOL_FF1
        sociologyTable$Father_Work_Pensioned     = originalBiomarkersTable$FATHER_WORK_PENSIONED_FF1
        sociologyTable$Father_Work_Deceased      = originalBiomarkersTable$FATHER_WORK_DECEASED_FF1
        sociologyTable$Father_Work_DontKnown     = originalBiomarkersTable$FATHER_WORK_DONT_KNOW_FF1
        sociologyTable$Father_Work_Other         = originalBiomarkersTable$FATHER_WORK_OTHER_FF1
        
        sociologyTable$Mother_Education          = originalBiomarkersTable$MOTHER_EDUCATION_FF1
        sociologyTable$Father_Education          = originalBiomarkersTable$FATHER_EDUCATION_FF1
        
        sociologyTable$Ethnicity                 = "" # There is only ethnicity which can be a multiple combination (Norwegian-Sami-Kven for example)
                                                      # I keep the rest of the variables to read the table, but drop them later
        
        sociologyTable$EthnicityNorwegian        = originalBiomarkersTable$ETHNICITY_NORWEGIAN_FF1
        sociologyTable$EthnicitySami             = originalBiomarkersTable$ETHNICITY_SAMI_FF1
        sociologyTable$EthnicityKven             = originalBiomarkersTable$ETHNICITY_KVENFINNISH_FF1
        sociologyTable$EthnicityOther            = originalBiomarkersTable$ETHNICITY_OTHER_FF1
        sociologyTable$EthnicityOtherComment     = originalBiomarkersTable$ETHNICITY_OTHER_SPEC_FF1
        
        
        
        sociologyTable$Aux1 = NULL
        
    }
    
    # --------------------------------
    # Friendship
    #
    #     Everything that has to do with friendship statistics
    #
    #     In the data we have so far, we only have how many SMSs each person
    #     receive (thank you boomer for designing this column). However from the
    #     network table we will generate many friendship statistics that are put
    #     in here.
    # --------------------------------
    {
        
        frienshipTable$ID           = originalAureusTable$pers_key_ff1
        frienshipTable$Created      = originalAureusTable$NETWORK_DATE_FF1     # When did we ask the question about the social network
        frienshipTable$Overview     = originalAureusTable$NETWORK_OVERVIEW_FF1
        frienshipTable$YesterdaySMS = originalBiomarkersTable$NUMBER_SMS_FF1
        frienshipTable$Aux1         = NULL
        
    }

    # --------------------------------
    # Puberty Men
    # --------------------------------
    {
     
        pubertyMenTable$ID           = originalAureusTable$pers_key_ff1
        
        pubertyMenTable$ChangeHeight = originalBiomarkersTable$PUBERTY_BOYS_HEIGHT_FF1
        pubertyMenTable$ChangeVoice  = originalBiomarkersTable$PUBERTY_BOYS_VOICE_FF1
        pubertyMenTable$FacialHair   = originalBiomarkersTable$PUBERTY_BOYS_HAIR_FACE_FF1
        pubertyMenTable$MenBodyHair  = originalBiomarkersTable$PUBERTY_BOYS_HAIR_BODY_FF1
        pubertyMenTable$MenPubicHair = originalBiomarkersTable$PUBIC_HAIR_MALE_FF1
        pubertyMenTable$MenPubicAge  = originalBiomarkersTable$PUBIC_HAIR_AGE_MALE_FF1
        
        pubertyMenTable$Aux1 = NULL
    }

    # --------------------------------
    # Puberty Women
    # --------------------------------
    {
     
        pubertyWomenTable$ID             = originalAureusTable$pers_key_ff1
        
        pubertyWomenTable$Menarche       = originalBiomarkersTable$MENARCHE_FF1
        pubertyWomenTable$MenarcheAge    = originalBiomarkersTable$MENARCHE_AGE_YEAR_FF1
        pubertyWomenTable$MenarcheMoths  = originalBiomarkersTable$MENARCHE_AGE_MONTH_FF1 # We read moth, but we change Age for a float number later and drop this column
        pubertyWomenTable$WomenPubicHair = originalBiomarkersTable$PUBIC_HAIR_FEMALE_FF1
        pubertyWomenTable$Breasts        = originalBiomarkersTable$BREASTS_FEMALE_FF1
        
        pubertyWomenTable$Aux1 = NULL
    }
     
    # --------------------------------
    # Recreational Drugs
    # --------------------------------
    {
        
        drugsTable$ID            = originalAureusTable$pers_key_ff1
        drugsTable$Smoke         = originalBiomarkersTable$SMOKE_FF1
        drugsTable$SmokePerWeek  = originalBiomarkersTable$CIGARETTES_PER_WEEK_FF1
        drugsTable$SmokePerDay   = originalBiomarkersTable$CIGARETTES_PER_DAY_FF1
        drugsTable$Snuff         = originalBiomarkersTable$SNUFF_FF1
        drugsTable$SnuffPerWeek  = originalBiomarkersTable$SNUFF_PORTION_PER_WEEK_FF1
        drugsTable$SnuffPerDay   = originalBiomarkersTable$SNUFF_PORTION_PER_DAY_FF1
        drugsTable$Alcohol       = originalBiomarkersTable$ALCOHOL_FREQUENCY_FF1
        drugsTable$AlcoholUnits  = originalBiomarkersTable$ALCOHOL_UNITS_FF1
        drugsTable$Alcohol6Units = originalBiomarkersTable$ALCOHOL_6UNITS_FF1
        
        drugsTable$Aux1 = NULL
        
    }
    
    # --------------------------------
    # Sport habits
    # --------------------------------
    {
        
        sportsTable$ID     = originalAureusTable$pers_key_ff1
        
        sportsTable$SportsLeisure       = originalAureusTable$PHYS_ACT_LEISURE_FF1
        sportsTable$SportsOutsideSchool = originalAureusTable$PHYS_ACT_OUTSIDE_SCHOOL_FF1 # This variable is very weird, it ask the person whether he is active in sports
                                                                                          # But the previous questions is exactly that with more extra information
        sportsTable$SportsFrequency     = originalBiomarkersTable$PHYS_ACT_FREQ_FF1
        sportsTable$SportHours          = originalBiomarkersTable$PHYS_ACT_HOURS_FF1
        sportsTable$SportsIntensity     = originalBiomarkersTable$PHYS_ACT_INTENSITY_FF1

        sportsTable$SummerTransport     = originalBiomarkersTable$TRANSPORT_TYPE_SUMMER_FF1
        sportsTable$SummerTime          = originalBiomarkersTable$TRANSPORT_DURATION_SUMMER_FF1
        sportsTable$WinterTransport     = originalBiomarkersTable$TRANSPORT_TYPE_WINTER_FF1
        sportsTable$WinterTime          = originalBiomarkersTable$TRANSPORT_DURATION_WINTER_FF1
        
        sportsTable$ScreenTime          = originalReportTable$SCREEN_TIME_WEEKDAY_FF1
        
        sportsTable$Aux1 = NULL

    }      
        
    # --------------------------------
    # Hygiene
    # --------------------------------
    {
        
        hygieneTable$ID            = originalAureusTable$pers_key_ff1
        
        hygieneTable$ShowerBathFrequency = originalBiomarkersTable$SHOWER_BATH_FREQUENCY_FF1
        hygieneTable$HandwashFrequency   = originalBiomarkersTable$HANDWASH_FREQUENCY_FF1
        hygieneTable$BodyLotionFrequency = originalBiomarkersTable$BODY_LOTION_FREQUENCY_FF1
        
        hygieneTable$SkinSunbathing      = originalBiomarkersTable$SKIN_COLOUR_SUNBATHING_FF1
        hygieneTable$HolidaySunbathing   = originalBiomarkersTable$HOLIDAY_SUNBATHING_FF1
        hygieneTable$SolariumLast4Weeks  = originalBiomarkersTable$SOLARIUM_LAST_4WEEKS_FF1	
        
        hygieneTable$Aux1   = NULL
        
    }
    
    # --------------------------------
    # Biomarkers
    # --------------------------------
    {

        # ID for the person
        biomarkersTable$ID = originalAureusTable$pers_key_ff1

        # ID for the batch 
        biomarkersTable$BatchNumber = originalBiomarkersTable$OLKINF_BATCH_NUMBER_FF1
        biomarkersTable$Flagged     = originalBiomarkersTable$OLKINF_FLAGGED_FF1
    
        # Everything else. LOD and NDL are the same, but with the names switched
        
        biomarkersTable$`Adenosine Deaminase (LOD - P00813)`                     = originalBiomarkersTable$ADA_LOD_OLKINF_FF1
        biomarkersTable$`Artemin (LOD - Q5T4W7)`                                 = originalBiomarkersTable$ARTN_LOD_OLKINF_FF1
        biomarkersTable$`Axin-1 (LOD - O15169)`                                  = originalBiomarkersTable$AXIN1_LOD_OLKINF_FF1
    
        biomarkersTable$`Brain-derived neurotrophic factor (LOD - P23560)`       = originalBiomarkersTable$BDNF_LOD_OLKINF_FF1
        biomarkersTable$`Beta-nerve growth factor (LOD - P01138)`                = originalBiomarkersTable$BNGF_LOD_OLKINF_FF1
    
        biomarkersTable$`Caspase-8 (LOD - Q14790)`                               = originalBiomarkersTable$CASP8_LOD_OLKINF_FF1
        biomarkersTable$`Eotaxin (LOD - P51671)`                                 = originalBiomarkersTable$CCL11_LOD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 19 (LOD - Q99731)`                  = originalBiomarkersTable$CCL19_LOD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 20 (LOD - P78556)`                  = originalBiomarkersTable$CCL20_LOD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 23 (LOD - P55773)`                  = originalBiomarkersTable$CCL23_LOD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 25 (LOD - O15444)`                  = originalBiomarkersTable$CCL25_LOD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 28 (LOD - Q9NRJ3)`                  = originalBiomarkersTable$CCL28_LOD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 3 (LOD - P10147)`                   = originalBiomarkersTable$CCL3_LOD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 4 (LOD - P13236)`                   = originalBiomarkersTable$CCL4_LOD_OLKINF_FF1
        biomarkersTable$`Natural killer cell receptor 2B4 (LOD - Q9BZW8)`        = originalBiomarkersTable$CD244_LOD_OLKINF_FF1
        biomarkersTable$`CD40L receptor (LOD - P25942)`                          = originalBiomarkersTable$CD40_LOD_OLKINF_FF1
        biomarkersTable$`T-cell surface glycoprotein CD5 (LOD - P06127)`         = originalBiomarkersTable$CD5_LOD_OLKINF_FF1
        biomarkersTable$`T cell surface glycoprotein CD6 isoform (LOD - Q8WWJ7)` = originalBiomarkersTable$CD6_LOD_OLKINF_FF1
        biomarkersTable$`CUB domain-containing protein 1 (LOD - Q9H5V8)`         = originalBiomarkersTable$CDCP1_LOD_OLKINF_FF1
        biomarkersTable$`Macrophage colony-stimulating factor 1 (LOD - P09603)`  = originalBiomarkersTable$CSF1_LOD_OLKINF_FF1
        biomarkersTable$`Cystatin D (LOD - P28325)`                              = originalBiomarkersTable$CST5_LOD_OLKINF_FF1
        biomarkersTable$`Fractalkine (LOD - P78423)`                             = originalBiomarkersTable$CX3CL1_LOD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 1 (LOD - P09341)`                 = originalBiomarkersTable$CXCL1_LOD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 10 (LOD - P02778)`                = originalBiomarkersTable$CXCL10_LOD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 11 (LOD - O14625)`                = originalBiomarkersTable$CXCL11_LOD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 5 (LOD - P42830)`                 = originalBiomarkersTable$CXCL5_LOD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 6 (LOD - P80162)`                 = originalBiomarkersTable$CXCL6_LOD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 9 (LOD - Q07325)`                 = originalBiomarkersTable$CXCL9_LOD_OLKINF_FF1
    
        biomarkersTable$`Delta and Notch-like epidermal growth factor-related receptor (LOD - Q8NFT8)` = originalBiomarkersTable$DNER_LOD_OLKINF_FF1
    
        biomarkersTable$`Eukaryotic translation initiation factor 4E-binding protein 1 (LOD - Q13541)` = originalBiomarkersTable$EIF4EBP1_LOD_OLKINF_FF1
        biomarkersTable$`Protein S100-A12 (LOD - P80511)`                        = originalBiomarkersTable$ENRAGE_LOD_OLKINF_FF1
    
        biomarkersTable$`Fibroblast growth factor 19 (LOD - O95750)`             = originalBiomarkersTable$FGF19_LOD_OLKINF_FF1
        biomarkersTable$`Fibroblast growth factor 21 (LOD - Q9NSA1)`             = originalBiomarkersTable$FGF21_LOD_OLKINF_FF1
        biomarkersTable$`Fibroblast growth factor 23 (LOD - Q9GZV9)`             = originalBiomarkersTable$FGF23_LOD_OLKINF_FF1
        biomarkersTable$`Fibroblast growth factor 5 (LOD - Q8NF90)`              = originalBiomarkersTable$FGF5_LOD_OLKINF_FF1
        biomarkersTable$`Fms-related tyrosine kinase 3 ligand (LOD - P49771)`    = originalBiomarkersTable$FLT3L_LOD_OLKINF_FF1
    
        biomarkersTable$`Glial cell line-derived neurotrophic factor (LOD - P39905)` = originalBiomarkersTable$GDNF_LOD_OLKINF_FF1
    
        biomarkersTable$`Hepatocyte growth factor (LOD - P14210)`                = originalBiomarkersTable$HGF_LOD_OLKINF_FF1
    
        biomarkersTable$`Interferon gamma (LOD - P01579)`                        = originalBiomarkersTable$IFNG_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-10 (LOD - P22301)`                          = originalBiomarkersTable$IL10_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-10 receptor subunit alpha (LOD - Q13651)`   = originalBiomarkersTable$IL10RA_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-10 receptor subunit beta (LOD - Q08334)`    = originalBiomarkersTable$IL10RB_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-12 subunit beta (LOD - P29460)`             = originalBiomarkersTable$IL12B_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-13 (LOD - P35225)`                          = originalBiomarkersTable$IL13_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-15 receptor subunit alpha (LOD - Q13261)`   = originalBiomarkersTable$IL15RA_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-17A (LOD - Q16552)`                         = originalBiomarkersTable$IL17A_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-17C (LOD - Q9P0M4)`                         = originalBiomarkersTable$IL17C_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-18 (LOD - Q14116)`                          = originalBiomarkersTable$IL18_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-18 receptor 1 (LOD - Q13478)`               = originalBiomarkersTable$IL18R1_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-1 alpha (LOD - P01583)`                     = originalBiomarkersTable$IL1A_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-2 (LOD - P60568)`                           = originalBiomarkersTable$IL2_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-20 (LOD - Q9NYY1)`                          = originalBiomarkersTable$IL20_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-20 receptor subunit alpha (LOD - Q9UHF4)`   = originalBiomarkersTable$IL20RA_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-22 receptor subunit alpha-1 (LOD - Q8N6P7)` = originalBiomarkersTable$IL22RA1_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-24 (LOD - Q13007)`                          = originalBiomarkersTable$IL24_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-2 receptor subunit beta (LOD - P14784)`     = originalBiomarkersTable$IL2RB_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-33 (LOD - O95760)`                          = originalBiomarkersTable$IL33_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-4 (LOD - P05112)`                           = originalBiomarkersTable$IL4_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-5 (LOD - P05113)`                           = originalBiomarkersTable$IL5_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-6 (LOD - P05231)`                           = originalBiomarkersTable$IL6_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-7 (LOD - P13232)`                           = originalBiomarkersTable$IL7_LOD_OLKINF_FF1
        biomarkersTable$`Interleukin-8 (LOD - P10145)`                           = originalBiomarkersTable$IL8_LOD_OLKINF_FF1
    
        biomarkersTable$`Leukemia inhibitory factor (LOD - P15018)`              = originalBiomarkersTable$LIF_LOD_OLKINF_FF1
        biomarkersTable$`Leukemia inhibitory factor receptor (LOD - P42702)`     = originalBiomarkersTable$LIFR_LOD_OLKINF_FF1
    
        biomarkersTable$`Monocyte chemotactic protein 1 (LOD - P13500)`          = originalBiomarkersTable$MCP1_LOD_OLKINF_FF1
        biomarkersTable$`Monocyte chemotactic protein 2 (LOD - P80075)`          = originalBiomarkersTable$MCP2_LOD_OLKINF_FF1
        biomarkersTable$`Monocyte chemotactic protein 3 (LOD - P80098)`          = originalBiomarkersTable$MCP3_LOD_OLKINF_FF1
        biomarkersTable$`Monocyte chemotactic protein 4 (LOD - Q99616)`          = originalBiomarkersTable$MCP4_LOD_OLKINF_FF1
        biomarkersTable$`Matrix metalloproteinase-1 (LOD - P03956)`              = originalBiomarkersTable$MMP1_LOD_OLKINF_FF1
        biomarkersTable$`Matrix metalloproteinase-10 (LOD - P09238)`             = originalBiomarkersTable$MMP10_LOD_OLKINF_FF1
    
        biomarkersTable$`Neurturin (LOD - Q99748)`                               = originalBiomarkersTable$NRTN_LOD_OLKINF_FF1
        biomarkersTable$`Neurotrophin-3 (LOD - P20783)`                          = originalBiomarkersTable$NT3_LOD_OLKINF_FF1
    
        biomarkersTable$`Osteoprotegerin (LOD - O00300)`                         = originalBiomarkersTable$OPG_LOD_OLKINF_FF1
        biomarkersTable$`Oncostatin-M (LOD - P13725)`                            = originalBiomarkersTable$OSM_LOD_OLKINF_FF1
    
        biomarkersTable$`Programmed cell death 1 ligand 1 (LOD - Q9NZQ7)`        = originalBiomarkersTable$PDL1_LOD_OLKINF_FF1
    
        biomarkersTable$`Stem cell factor (LOD - P21583)`                        = originalBiomarkersTable$SCF_LOD_OLKINF_FF1
        biomarkersTable$`SIR2-like protein 2 (LOD - Q8IXJ6)`                     = originalBiomarkersTable$SIRT2_LOD_OLKINF_FF1
        biomarkersTable$`Signaling lymphocytic activation molecule (LOD - Q13291)` = originalBiomarkersTable$SLAMF1_LOD_OLKINF_FF1
        biomarkersTable$`Sulfotransferase 1A1 (LOD - P50225)`                    = originalBiomarkersTable$ST1A1_LOD_OLKINF_FF1	
        biomarkersTable$`STAM-binding protein (LOD - O95630)`                    = originalBiomarkersTable$STAMBP_LOD_OLKINF_FF1
    
        biomarkersTable$`Transforming growth factor alpha (LOD - P01135)`        = originalBiomarkersTable$TGFA_LOD_OLKINF_FF1
        biomarkersTable$`Latency-associated peptide transforming growth factor beta-1 (LOD - P01137)` = originalBiomarkersTable$TGFB1_LOD_OLKINF_FF1
        biomarkersTable$`Tumor necrosis factor (LOD - P01375)`                   = originalBiomarkersTable$TNF_LOD_OLKINF_FF1
        biomarkersTable$`TNF-beta (LOD - P01374)`                                = originalBiomarkersTable$TNFB_LOD_OLKINF_FF1
        biomarkersTable$`Tumor necrosis factor receptor superfamily member 9 (LOD - Q07011)` = originalBiomarkersTable$TNFRSF9_LOD_OLKINF_FF1
        biomarkersTable$`Tumor necrosis factor ligand superfamily member 14 (LOD - O43557)` = originalBiomarkersTable$TNFSF14_LOD_OLKINF_FF1
        biomarkersTable$`TNF-related apoptosis-inducing ligand (LOD - P50591)`   = originalBiomarkersTable$TRAIL_LOD_OLKINF_FF1
        biomarkersTable$`TNF-related activation-induced cytokine (LOD - O14788)` = originalBiomarkersTable$TRANCE_LOD_OLKINF_FF1
        biomarkersTable$`Thymic stromal lymphopoietin (LOD - Q969D9)`            = originalBiomarkersTable$TSLP_LOD_OLKINF_FF1
        biomarkersTable$`Tumor necrosis factor (LOD - O43508)`                   = originalBiomarkersTable$TWEAK_LOD_OLKINF_FF1
    
        biomarkersTable$`Urokinase-type plasminogen activator (LOD - P00749)`    = originalBiomarkersTable$UPA_LOD_OLKINF_FF1
    
        biomarkersTable$`Vascular endothelial growth factor A (LOD - P15692)`    = originalBiomarkersTable$VEGFA_LOD_OLKINF_FF1
    
        # NDL
        biomarkersTable$`Adenosine Deaminase (NLD - P00813)`                     = originalBiomarkersTable$ADA_NLD_OLKINF_FF1
        biomarkersTable$`Artemin (NLD - Q5T4W7)`                                 = originalBiomarkersTable$ARTN_NLD_OLKINF_FF1
        biomarkersTable$`Axin-1 (NLD - O15169)`                                  = originalBiomarkersTable$AXIN1_NLD_OLKINF_FF1
    
        biomarkersTable$`Brain-derived neurotrophic factor (NLD - P23560)`       = originalBiomarkersTable$BDNF_NLD_OLKINF_FF1
        biomarkersTable$`Beta-nerve growth factor (NLD - P01138)`                = originalBiomarkersTable$BNGF_NLD_OLKINF_FF1
    
        biomarkersTable$`Caspase-8 (NLD - Q14790)`                               = originalBiomarkersTable$CASP8_NLD_OLKINF_FF1
        biomarkersTable$`Eotaxin (NLD - P51671)`                                 = originalBiomarkersTable$CCL11_NLD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 19 (NLD - Q99731)`                  = originalBiomarkersTable$CCL19_NLD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 20 (NLD - P78556)`                  = originalBiomarkersTable$CCL20_NLD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 23 (NLD - P55773)`                  = originalBiomarkersTable$CCL23_NLD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 25 (NLD - O15444)`                  = originalBiomarkersTable$CCL25_NLD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 28 (NLD - Q9NRJ3)`                  = originalBiomarkersTable$CCL28_NLD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 3 (NLD - P10147)`                   = originalBiomarkersTable$CCL3_NLD_OLKINF_FF1
        biomarkersTable$`C-C motif chemokine 4 (NLD - P13236)`                   = originalBiomarkersTable$CCL4_NLD_OLKINF_FF1
        biomarkersTable$`Natural killer cell receptor 2B4 (NLD - Q9BZW8)`        = originalBiomarkersTable$CD244_NLD_OLKINF_FF1
        biomarkersTable$`CD40L receptor (NLD - P25942)`                          = originalBiomarkersTable$CD40_NLD_OLKINF_FF1
        biomarkersTable$`T-cell surface glycoprotein CD5 (NLD - P06127)`         = originalBiomarkersTable$CD5_NLD_OLKINF_FF1
        biomarkersTable$`T cell surface glycoprotein CD6 isoform (NLD - Q8WWJ7)` = originalBiomarkersTable$CD6_NLD_OLKINF_FF1
        biomarkersTable$`CUB domain-containing protein 1 (NLD - Q9H5V8)`         = originalBiomarkersTable$CDCP1_NLD_OLKINF_FF1
        biomarkersTable$`Macrophage colony-stimulating factor 1 (NLD - P09603)`  = originalBiomarkersTable$CSF1_NLD_OLKINF_FF1
        biomarkersTable$`Cystatin D (NLD - P28325)`                              = originalBiomarkersTable$CST5_NLD_OLKINF_FF1
        biomarkersTable$`Fractalkine (NLD - P78423)`                             = originalBiomarkersTable$CX3CL1_NLD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 1 (NLD - P09341)`                 = originalBiomarkersTable$CXCL1_NLD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 10 (NLD - P02778)`                = originalBiomarkersTable$CXCL10_NLD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 11 (NLD - O14625)`                = originalBiomarkersTable$CXCL11_NLD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 5 (NLD - P42830)`                 = originalBiomarkersTable$CXCL5_NLD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 6 (NLD - P80162)`                 = originalBiomarkersTable$CXCL6_NLD_OLKINF_FF1
        biomarkersTable$`C-X-C motif chemokine 9 (NLD - Q07325)`                 = originalBiomarkersTable$CXCL9_NLD_OLKINF_FF1
    
        biomarkersTable$`Delta and Notch-like epidermal growth factor-related receptor (NLD - Q8NFT8)` = originalBiomarkersTable$DNER_NLD_OLKINF_FF1
    
        biomarkersTable$`Eukaryotic translation initiation factor 4E-binding protein 1 (NLD - Q13541)` = originalBiomarkersTable$EIF4EBP1_NLD_OLKINF_FF1
        biomarkersTable$`Protein S100-A12 (NLD - P80511)`                        = originalBiomarkersTable$ENRAGE_NLD_OLKINF_FF1
    
        biomarkersTable$`Fibroblast growth factor 19 (NLD - O95750)`             = originalBiomarkersTable$FGF19_NLD_OLKINF_FF1
        biomarkersTable$`Fibroblast growth factor 21 (NLD - Q9NSA1)`             = originalBiomarkersTable$FGF21_NLD_OLKINF_FF1
        biomarkersTable$`Fibroblast growth factor 23 (NLD - Q9GZV9)`             = originalBiomarkersTable$FGF23_NLD_OLKINF_FF1
        biomarkersTable$`Fibroblast growth factor 5 (NLD - Q8NF90)`              = originalBiomarkersTable$FGF5_NLD_OLKINF_FF1
        biomarkersTable$`Fms-related tyrosine kinase 3 ligand (NLD - P49771)`    = originalBiomarkersTable$FLT3L_NLD_OLKINF_FF1
    
        biomarkersTable$`Glial cell line-derived neurotrophic factor (NLD - P39905)` = originalBiomarkersTable$GDNF_NLD_OLKINF_FF1
    
        biomarkersTable$`Hepatocyte growth factor (NLD - P14210)`                = originalBiomarkersTable$HGF_NLD_OLKINF_FF1
    
        biomarkersTable$`Interferon gamma (NLD - P01579)`                        = originalBiomarkersTable$IFNG_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-10 (NLD - P22301)`                          = originalBiomarkersTable$IL10_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-10 receptor subunit alpha (NLD - Q13651)`   = originalBiomarkersTable$IL10RA_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-10 receptor subunit beta (NLD - Q08334)`    = originalBiomarkersTable$IL10RB_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-12 subunit beta (NLD - P29460)`             = originalBiomarkersTable$IL12B_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-13 (NLD - P35225)`                          = originalBiomarkersTable$IL13_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-15 receptor subunit alpha (NLD - Q13261)`   = originalBiomarkersTable$IL15RA_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-17A (NLD - Q16552)`                         = originalBiomarkersTable$IL17A_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-17C (NLD - Q9P0M4)`                         = originalBiomarkersTable$IL17C_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-18 (NLD - Q14116)`                          = originalBiomarkersTable$IL18_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-18 receptor 1 (NLD - Q13478)`               = originalBiomarkersTable$IL18R1_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-1 alpha (NLD - P01583)`                     = originalBiomarkersTable$IL1A_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-2 (NLD - P60568)`                           = originalBiomarkersTable$IL2_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-20 (NLD - Q9NYY1)`                          = originalBiomarkersTable$IL20_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-20 receptor subunit alpha (NLD - Q9UHF4)`   = originalBiomarkersTable$IL20RA_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-22 receptor subunit alpha-1 (NLD - Q8N6P7)` = originalBiomarkersTable$IL22RA1_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-24 (NLD - Q13007)`                          = originalBiomarkersTable$IL24_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-2 receptor subunit beta (NLD - P14784)`     = originalBiomarkersTable$IL2RB_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-33 (NLD - O95760)`                          = originalBiomarkersTable$IL33_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-4 (NLD - P05112)`                           = originalBiomarkersTable$IL4_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-5 (NLD - P05113)`                           = originalBiomarkersTable$IL5_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-6 (NLD - P05231)`                           = originalBiomarkersTable$IL6_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-7 (NLD - P13232)`                           = originalBiomarkersTable$IL7_NLD_OLKINF_FF1
        biomarkersTable$`Interleukin-8 (NLD - P10145)`                           = originalBiomarkersTable$IL8_NLD_OLKINF_FF1
    
        biomarkersTable$`Leukemia inhibitory factor (NLD - P15018)`              = originalBiomarkersTable$LIF_NLD_OLKINF_FF1
        biomarkersTable$`Leukemia inhibitory factor receptor (NLD - P42702)`     = originalBiomarkersTable$LIFR_NLD_OLKINF_FF1
    
        biomarkersTable$`Monocyte chemotactic protein 1 (NLD - P13500)`          = originalBiomarkersTable$MCP1_NLD_OLKINF_FF1
        biomarkersTable$`Monocyte chemotactic protein 2 (NLD - P80075)`          = originalBiomarkersTable$MCP2_NLD_OLKINF_FF1
        biomarkersTable$`Monocyte chemotactic protein 3 (NLD - P80098)`          = originalBiomarkersTable$MCP3_NLD_OLKINF_FF1
        biomarkersTable$`Monocyte chemotactic protein 4 (NLD - Q99616)`          = originalBiomarkersTable$MCP4_NLD_OLKINF_FF1
        biomarkersTable$`Matrix metalloproteinase-1 (NLD - P03956)`              = originalBiomarkersTable$MMP1_NLD_OLKINF_FF1
        biomarkersTable$`Matrix metalloproteinase-10 (NLD - P09238)`             = originalBiomarkersTable$MMP10_NLD_OLKINF_FF1
    
        biomarkersTable$`Neurturin (NLD - Q99748)`                               = originalBiomarkersTable$NRTN_NLD_OLKINF_FF1
        biomarkersTable$`Neurotrophin-3 (NLD - P20783)`                          = originalBiomarkersTable$NT3_NLD_OLKINF_FF1
    
        biomarkersTable$`Osteoprotegerin (NLD - O00300)`                         = originalBiomarkersTable$OPG_NLD_OLKINF_FF1
        biomarkersTable$`Oncostatin-M (NLD - P13725)`                            = originalBiomarkersTable$OSM_NLD_OLKINF_FF1
    
        biomarkersTable$`Programmed cell death 1 ligand 1 (NLD - Q9NZQ7)`        = originalBiomarkersTable$PDL1_NLD_OLKINF_FF1
    
        biomarkersTable$`Stem cell factor (NLD - P21583)`                        = originalBiomarkersTable$SCF_NLD_OLKINF_FF1
        biomarkersTable$`SIR2-like protein 2 (NLD - Q8IXJ6)`                     = originalBiomarkersTable$SIRT2_NLD_OLKINF_FF1
        biomarkersTable$`Signaling lymphocytic activation molecule (NLD - Q13291)` = originalBiomarkersTable$SLAMF1_NLD_OLKINF_FF1
        biomarkersTable$`Sulfotransferase 1A1 (NLD - P50225)`                    = originalBiomarkersTable$ST1A1_NLD_OLKINF_FF1	
        biomarkersTable$`STAM-binding protein (NLD - O95630)`                    = originalBiomarkersTable$STAMBP_NLD_OLKINF_FF1
    
        biomarkersTable$`Transforming growth factor alpha (NLD - P01135)`        = originalBiomarkersTable$TGFA_NLD_OLKINF_FF1
        biomarkersTable$`Latency-associated peptide transforming growth factor beta-1 (NLD - P01137)` = originalBiomarkersTable$TGFB1_NLD_OLKINF_FF1
        biomarkersTable$`Tumor necrosis factor (NLD - P01375)`                   = originalBiomarkersTable$TNF_NLD_OLKINF_FF1
        biomarkersTable$`TNF-beta (NLD - P01374)`                                = originalBiomarkersTable$TNFB_NLD_OLKINF_FF1
        biomarkersTable$`Tumor necrosis factor receptor superfamily member 9 (NLD - Q07011)` = originalBiomarkersTable$TNFRSF9_NLD_OLKINF_FF1
        biomarkersTable$`Tumor necrosis factor ligand superfamily member 14 (NLD - O43557)` = originalBiomarkersTable$TNFSF14_NLD_OLKINF_FF1
        biomarkersTable$`TNF-related apoptosis-inducing ligand (NLD - P50591)`   = originalBiomarkersTable$TRAIL_NLD_OLKINF_FF1
        biomarkersTable$`TNF-related activation-induced cytokine (NLD - O14788)` = originalBiomarkersTable$TRANCE_NLD_OLKINF_FF1
        biomarkersTable$`Thymic stromal lymphopoietin (NLD - Q969D9)`            = originalBiomarkersTable$TSLP_NLD_OLKINF_FF1
        biomarkersTable$`Tumor necrosis factor (NLD - O43508)`                   = originalBiomarkersTable$TWEAK_NLD_OLKINF_FF1
    
        biomarkersTable$`Urokinase-type plasminogen activator (NLD - P00749)`    = originalBiomarkersTable$UPA_NLD_OLKINF_FF1
    
        biomarkersTable$`Vascular endothelial growth factor A (NLD - P15692)`    = originalBiomarkersTable$VEGFA_NLD_OLKINF_FF1
                
        
        biomarkersTable$Aux1 = NULL
        
                
    }    
       
    # --------------------------------
    # Diet
    # --------------------------------
    {

        dietTable$ID     = originalAureusTable$pers_key_ff1
    
        # Time of the day
        dietTable$BreakfastFrequency      = originalReportTable$BREAKFAST_FF1    
        # -- Lunch and dinner is missing from data
        # Specific foods
        dietTable$FatFishFrequency        = originalReportTable$FAT_FISH_FF1
        dietTable$LeanFishFrequency       = originalHormonalTable$lean_fish_ff1
        dietTable$SeagullEggsFrequency    = originalHormonalTable$seagull_eggs_ff1
        dietTable$ReindeerFrequency       = originalHormonalTable$reindeer_ff1
        dietTable$CheeseFrequency         = originalReportTable$CHEESE_FF1
        dietTable$ChocolateFrequency      = originalReportTable$CHOCOLATE_SWEETS_FF1
        dietTable$FruitsFrequency         = originalReportTable$FRUITS_FF1
        dietTable$VegetablesFrequency     = originalReportTable$VEGETABLES_FF1
        dietTable$DairyFrequency          = originalReportTable$FULL_FAT_DAIRY_DRINKS_FF1
        # Specific Drinks
        dietTable$FruitJuiceFrequency     = originalReportTable$FRUIT_JUICE_FF1
        dietTable$SugarJuiceFrequency     = originalReportTable$JUICE_SUGAR_FF1
        dietTable$SugarDrinkFrequency     = originalReportTable$SOFT_DRINKS_SUGAR_FF1
        dietTable$SweetenerDrinkFrequency = originalReportTable$SOFT_DRINKS_SWEETENER_FF1
        dietTable$WaterFrequency          = originalReportTable$WATER_FF1
        # Specific pills
        dietTable$FishOilFrequency        = originalReportTable$COD_LIVER_OIL_FF1
        dietTable$VitaminsFrequency       = originalReportTable$VITAMINS_MINERALS_FF1
        
        dietTable$Aux1 = NULL
                
    }
    
    # --------------------------------
    # Sleep habits
    #
    #     The original variables are categorical variables (1,2,3,4,...) for
    #     modeling 18:30, 19:00, 19:30... This is not a good way of encoding
    #     this, so in here I read the original numbers and they will be
    #     transform later.
    # --------------------------------
    {
        
        sleepTable$ID   = originalAureusTable$pers_key_ff1        
        
        sleepTable$HoursSleeping    = originalReportTable$SLEEP_HOURS_GET_FF1
        sleepTable$BedTimeHourCat   = originalReportTable$SLEEP_BED_WEEK_FF1
        sleepTable$BedTimeHourFloat = 0
        sleepTable$SleepingPills    = originalBiomarkersTable$SLEEPING_PILLS_4WEEKS_FF1  # This variable is repeated in the medication DB, I will leave a copy here though
        
        sleepTable$Aux1 = NULL        
        
    }
    
    # --------------------------------
    # Social network table
    #
    #     Everything related to the social network structure. This will be
    #     transformed later into a proper melted table. And we will have one
    #     for each network, that's why we keep the friendships statistics
    #     separated into the frienship table.
    # --------------------------------
    {
        
        # For FF1
        {
            
            # ID
            networkTable$ID        = originalAureusTable$pers_key_ff1
        
            # Basic stuff
            networkTable$NetworkDateFF1 = originalAureusTable$NETWORK_DATE_FF1
            networkTable$Overwiew       = originalAureusTable$NETWORK_OVERVIEW_FF1 # 0 to 10, how good this describe your life        
        
            # Your 5 friends, if any
            networkTable$Friend1   = originalAureusTable$FRIEND_1_FF1
            networkTable$Friend2   = originalAureusTable$FRIEND_2_FF1
            networkTable$Friend3   = originalAureusTable$FRIEND_3_FF1
            networkTable$Friend4   = originalAureusTable$FRIEND_4_FF1
            networkTable$Friend5   = originalAureusTable$FRIEND_5_FF1

            # The contact with your friends

            # ---- Friend 1
            networkTable$Friend1Physical = originalAureusTable$FRIEND1_PHYSICAL_CONTACT_FF1
            networkTable$Friend1School   = originalAureusTable$FRIEND1_CONTACT_SCHOOL_FF1
            networkTable$Friend1Sport    = originalAureusTable$FRIEND1_CONTACT_SPORT_FF1
            networkTable$Friend1Home     = originalAureusTable$FRIEND1_CONTACT_HOME_FF1
            networkTable$Friend1Other    = originalAureusTable$FRIEND1_CONTACT_OTHER_FF1

            # ----Friend 2
            networkTable$Friend2Physical = originalAureusTable$FRIEND2_PHYSICAL_CONTACT_FF1
            networkTable$Friend2School   = originalAureusTable$FRIEND2_CONTACT_SCHOOL_FF1
            networkTable$Friend2Sport    = originalAureusTable$FRIEND2_CONTACT_SPORT_FF1
            networkTable$Friend2Home     = originalAureusTable$FRIEND2_CONTACT_HOME_FF1
            networkTable$Friend2Other    = originalAureusTable$FRIEND2_CONTACT_OTHER_FF1

            # ----Friend 3
            networkTable$Friend3Physical = originalAureusTable$FRIEND3_PHYSICAL_CONTACT_FF1
            networkTable$Friend3School   = originalAureusTable$FRIEND3_CONTACT_SCHOOL_FF1
            networkTable$Friend3Sport    = originalAureusTable$FRIEND3_CONTACT_SPORT_FF1
            networkTable$Friend3Home     = originalAureusTable$FRIEND3_CONTACT_HOME_FF1
            networkTable$Friend3Other    = originalAureusTable$FRIEND3_CONTACT_OTHER_FF1

            # ----Friend 4
            networkTable$Friend4Physical = originalAureusTable$FRIEND4_PHYSICAL_CONTACT_FF1
            networkTable$Friend4School   = originalAureusTable$FRIEND4_CONTACT_SCHOOL_FF1
            networkTable$Friend4Sport    = originalAureusTable$FRIEND4_CONTACT_SPORT_FF1
            networkTable$Friend4Home     = originalAureusTable$FRIEND4_CONTACT_HOME_FF1
            networkTable$Friend4Other    = originalAureusTable$FRIEND4_CONTACT_OTHER_FF1

            # ----Friend 5
            networkTable$Friend5Physical = originalAureusTable$FRIEND5_PHYSICAL_CONTACT_FF1
            networkTable$Friend5School   = originalAureusTable$FRIEND5_CONTACT_SCHOOL_FF1
            networkTable$Friend5Sport    = originalAureusTable$FRIEND5_CONTACT_SPORT_FF1
            networkTable$Friend5Home     = originalAureusTable$FRIEND5_CONTACT_HOME_FF1
            networkTable$Friend5Other    = originalAureusTable$FRIEND5_CONTACT_OTHER_FF1
            
        }
      
        # For FF12
        {
            # Basic stuff
            networkTable$NetworkDateFF12 = originalReportTable$NETWORK_DATE_FF12   
            networkTable$OverwiewFF12    = originalReportTable$NETWORK_OVERVIEW_FF12 # 0 to 10, how good this describe your life        
        
            # Your 5 friends, if any
            networkTable$Friend1FF12     = originalReportTable$FRIEND_1_FF12
            networkTable$Friend2FF12     = originalReportTable$FRIEND_2_FF12
            networkTable$Friend3FF12     = originalReportTable$FRIEND_3_FF12
            networkTable$Friend4FF12     = originalReportTable$FRIEND_4_FF12
            networkTable$Friend5FF12     = originalReportTable$FRIEND_5_FF12

            # The contact with your friends

            # ---- Friend 1
            networkTable$Friend1PhysicalFF12 = originalReportTable$FRIEND1_PHYSICAL_CONTACT_FF12
            networkTable$Friend1SchoolFF12   = originalReportTable$FRIEND1_CONTACT_SCHOOL_FF12
            networkTable$Friend1SportFF12    = originalReportTable$FRIEND1_CONTACT_SPORT_FF12
            networkTable$Friend1HomeFF12     = originalReportTable$FRIEND1_CONTACT_HOME_FF12
            networkTable$Friend1OtherFF12    = originalReportTable$FRIEND1_CONTACT_OTHER_FF12

            # ----Friend 2
            networkTable$Friend2PhysicalFF12 = originalReportTable$FRIEND2_PHYSICAL_CONTACT_FF12
            networkTable$Friend2SchoolFF12   = originalReportTable$FRIEND2_CONTACT_SCHOOL_FF12
            networkTable$Friend2SportFF12    = originalReportTable$FRIEND2_CONTACT_SPORT_FF12
            networkTable$Friend2HomeFF12     = originalReportTable$FRIEND2_CONTACT_HOME_FF12
            networkTable$Friend2OtherFF12    = originalReportTable$FRIEND2_CONTACT_OTHER_FF12

            # ----Friend 3
            networkTable$Friend3PhysicalFF12 = originalReportTable$FRIEND3_PHYSICAL_CONTACT_FF12
            networkTable$Friend3SchoolFF12   = originalReportTable$FRIEND3_CONTACT_SCHOOL_FF12
            networkTable$Friend3SportFF12    = originalReportTable$FRIEND3_CONTACT_SPORT_FF12
            networkTable$Friend3HomeFF12     = originalReportTable$FRIEND3_CONTACT_HOME_FF12
            networkTable$Friend3OtherFF12    = originalReportTable$FRIEND3_CONTACT_OTHER_FF12

            # ----Friend 4
            networkTable$Friend4PhysicalFF12 = originalReportTable$FRIEND4_PHYSICAL_CONTACT_FF12
            networkTable$Friend4SchoolFF12   = originalReportTable$FRIEND4_CONTACT_SCHOOL_FF12
            networkTable$Friend4SportFF12    = originalReportTable$FRIEND4_CONTACT_SPORT_FF12
            networkTable$Friend4HomeFF12     = originalReportTable$FRIEND4_CONTACT_HOME_FF12
            networkTable$Friend4OtherFF12    = originalReportTable$FRIEND4_CONTACT_OTHER_FF12

            # ----Friend 5
            networkTable$Friend5PhysicalFF12 = originalReportTable$FRIEND5_PHYSICAL_CONTACT_FF12
            networkTable$Friend5SchoolFF12   = originalReportTable$FRIEND5_CONTACT_SCHOOL_FF12
            networkTable$Friend5SportFF12    = originalReportTable$FRIEND5_CONTACT_SPORT_FF12
            networkTable$Friend5HomeFF12     = originalReportTable$FRIEND5_CONTACT_HOME_FF12
            networkTable$Friend5OtherFF12    = originalReportTable$FRIEND5_CONTACT_OTHER_FF12
             
        }
        
        networkTable$Aux1               = NULL        
        
    }

    # --------------------------------
    # Questionaries (anything here?)
    # --------------------------------
    {
        
    }
    
    # --------------------------------
    # Hospitalization (anything here?)
    # --------------------------------
    {
        
    }    

    # --------------------------------
    # Diseases
    #
    #     This just reads the data from the original file. Later on we transform
    #     All of this into a proper relational database.
    # --------------------------------
    {
      diseaseTable$ID                             = originalAureusTable$pers_key_ff1
      diseaseTable$TotalDiseases                  = 0                                              # Save this spot to link the count from the DB
      diseaseTable$Health                         = originalBiomarkersTable$HEALTH_FF1
      diseaseTable$Diabetes                       = originalBiomarkersTable$DIABETES_FF1
      diseaseTable$IchySkin                       = originalBiomarkersTable$ICHY_SKIN_FF1
      diseaseTable$IchySkinLocation               = originalBiomarkersTable$ICHY_SKIN_LOCATION_FF1
      diseaseTable$IchySkinStartAge               = originalBiomarkersTable$ICHY_SKIN_AGE_FF1
      diseaseTable$IchySkinSeverity               = originalBiomarkersTable$ICHY_SKIN_SEVERITY_FF1
      diseaseTable$HandEczema                     = originalBiomarkersTable$HAND_ECZEMA_FF1         # There is a hand eczema severity variable as well, but we don't have it.
      diseaseTable$Rhinitis                       = originalBiomarkersTable$ALLERGIC_RHINITIS_FF1
      diseaseTable$Asthma                         = originalBiomarkersTable$ASTHMA_FF1
      diseaseTable$AtopicEczema                   = originalBiomarkersTable$ATOPIC_ECZEMA_FF1
      diseaseTable$Psoriasis                      = originalHormonalTable$psoriasis_lifetime_ff1
      diseaseTable$PsoriasisSeverity              = originalHormonalTable$psoriasis_severity_ff1
      diseaseTable$ChronicDisease                 = originalHormonalTable$chronic_disease_ff1
      diseaseTable$ChronicDisease1Name            = originalHormonalTable$diagnosis_chronic_disease1_ff1
      diseaseTable$ChronicDisease1ICD10           = originalHormonalTable$icd10_chronic_disease1_ff1
      diseaseTable$ChronicDisease2Name            = originalHormonalTable$diagnosis_chronic_disease2_ff1
      diseaseTable$ChronicDisease2ICD10           = originalHormonalTable$icd10_chronic_disease2_ff1
      diseaseTable$ChronicDisease3Name            = originalHormonalTable$diagnosis_chronic_disease3_ff1
      diseaseTable$ChronicDisease3ICD10           = originalHormonalTable$icd10_chronic_disease3_ff1
      diseaseTable$ChronicDisease4Name            = originalHormonalTable$diagnosis_chronic_disease4_ff1
      diseaseTable$ChronicDisease4ICD10           = originalHormonalTable$icd10_chronic_disease4_ff1
      diseaseTable$ChronicDisease5Name            = originalHormonalTable$diagnosis_chronic_disease5_ff1
      diseaseTable$ChronicDisease5ICD10           = originalHormonalTable$icd10_chronic_disease5_ff1
      diseaseTable$ChronicDiseaseOtherName        = originalHormonalTable$chronic_disease_other_ff1
      diseaseTable$ChronicDiseaseOtherDescription = originalHormonalTable$chronic_disease_other_desc_ff1
      
      diseaseTable$ChronicDisease1AgeDiagnose     = originalReportTable$AGE_DIAGN_CHRONIC_DISEASE1_FF1
      diseaseTable$ChronicDisease2AgeDiagnose     = originalReportTable$AGE_DIAGN_CHRONIC_DISEASE2_FF1
      diseaseTable$ChronicDisease3AgeDiagnose     = originalReportTable$AGE_DIAGN_CHRONIC_DISEASE3_FF1
      diseaseTable$ChronicDisease4AgeDiagnose     = originalReportTable$AGE_DIAGN_CHRONIC_DISEASE4_FF1
      diseaseTable$ChronicDisease5AgeDiagnose     = originalReportTable$AGE_DIAGN_CHRONIC_DISEASE5_FF1
      
      diseaseTable$ChronicDiseaseDebut            = originalReportTable$AGE_CHRONIC_SYMPTOM_DEBUT_FF1
      
      diseaseTable$Aux1                           = NULL
      
    }
   
    
    # @@Original Names Ends Here@@
    
  }
}

# Create the relational tables
# ---- Diseases
# ---- Medication
# ---- Contraceptives
{
  
    # Diseases
    {
        # The disease table is a complete pain in the ass because some people report
        # diabetes in the diabetes column, and others do in the chronic disease
        # column. And worse, some say no diabetes in the column, and yes diabetes in
        # the chronic.
        #
        # This is true, not just for diabetes, but for all diseases.
        #
        # Plus there is the "Other" Column, where people also don't report anything
        # to later say "Yes I have diabetes". Or people that don't know they have
        # Ichy hands but called it Dyshidrosis (which is the same thing)
        #
        # So it needs some extra manual curation to fix this.
        #
        # Furthermore, the name of the chronic diseases are a mix of norwegian and
        # english and non-standarized names
        #
        # ie: "Rhinitis", "Høysnuve", "Høysnuve (allergisk rinitt)"
        #
        # So all of this needs to be unify further so you don't get weird statistics
        # later on.
    
        # Anyway, lets create the relational table for diseases
        {
      
            # We could create an empty DF and add rows to it, but R does this
            # EXTREMELLY INNEFICIENT (In C you could just allocate 1000 rows each time
            # and join them later for example, so yes, R is stupid)
            #
            # So to solve that, we count approximately how many rows do we need,
            # multiply by 2 to be sure, and fill everything. Then we just delete the
            # empty rows that we don't need.
      
            # Count how many diseases we have
            {
       
                totalDiseases = 0
                for(i in 1:originalTotalRows){
        
                    if(diseaseTable$ChronicDisease1ICD10[i]           != "") totalDiseases = totalDiseases + 1
                    if(diseaseTable$ChronicDisease2ICD10[i]           != "") totalDiseases = totalDiseases + 1
                    if(diseaseTable$ChronicDisease3ICD10[i]           != "") totalDiseases = totalDiseases + 1
                    if(diseaseTable$ChronicDisease4ICD10[i]           != "") totalDiseases = totalDiseases + 1
                    if(diseaseTable$ChronicDisease5ICD10[i]           != "") totalDiseases = totalDiseases + 1
                    if(diseaseTable$ChronicDiseaseOtherDescription[i] != "") totalDiseases = totalDiseases + 1
                
                }
      
                totalDiseases = totalDiseases + sum(diseaseTable$Diabetes,     na.rm = TRUE)
                totalDiseases = totalDiseases + sum(diseaseTable$IchySkin,     na.rm = TRUE)
                totalDiseases = totalDiseases + sum(diseaseTable$HandEczema,   na.rm = TRUE)
                totalDiseases = totalDiseases + sum(diseaseTable$Rhinitis,     na.rm = TRUE)
                totalDiseases = totalDiseases + sum(diseaseTable$Asthma,       na.rm = TRUE)
                totalDiseases = totalDiseases + sum(diseaseTable$AtopicEczema, na.rm = TRUE)
                totalDiseases = totalDiseases + sum(diseaseTable$Psoriasis,    na.rm = TRUE)
      
                totalDiseases = totalDiseases * 2 # Add a bunch of extra files now because R sucks handling memory
      
                # -- Create the final new dataframe (we still need to delete the extra rows)
                diseasesDBDF = data.frame(matrix(NA, nrow = totalDiseases, ncol = 6))
                colnames(diseasesDBDF) = c("ID", "Diagnostic", "ICD10", "Title", "Age Diagnostic", "Age Debut")
                currentCell     = 1
                icdIndexes      = c(17,19,21,23,25)
                diagnoseIndexes = c(28,29,30,31,32)
                debutIndexes    = 33
            
                # -- Most of the time, we don't have the age of diagnostic, or age of debut
                #    (or severity, if we ever got that variable)
                #    So initialize all to unknown, and change later
                diseasesDBDF$`Age Diagnostic` = NA
                diseasesDBDF$`Age Debut`      = NA
            
            
                # -- For each row of the disease table
                #    Process the normal ICD10 diseases first
                for(i in 1:originalTotalRows){
        
                    # For each icd code
                    for(j in 1:5){
          
                        # get the icd code index
                        currentIndex         = icdIndexes[j]
                        currentDiagnoseIndex = diagnoseIndexes[j]
                        currentDebutIndex    = debutIndexes
          
                        # For each ICD which is not empty
                        if( !is.na(diseaseTable[i,currentIndex]))
                            if(diseaseTable[i,currentIndex] != "" ){
            
                                # Add the info to the final DF
                                # ---- ID, Diagnosis and ICD10
                                diseasesDBDF[currentCell,1] = diseaseTable$ID[i]
                                diseasesDBDF[currentCell,2] = diseaseTable[i , currentIndex - 1]
                                diseasesDBDF[currentCell,3] = diseaseTable[i , currentIndex]
                                # ---- Age of diagnosis, and age of debut if they exist
                                diseasesDBDF[currentCell,5] = diseaseTable[i , currentDiagnoseIndex]
                                diseasesDBDF[currentCell,6] = diseaseTable[i , currentDebutIndex]
                            
                                currentCell = currentCell + 1
            
                            }

                        }
        
                    }
      
                # -- For each row of the temporal table
                #    Register the weird OTHER icd next
                currentIndex = 27
                for(i in 1:originalTotalRows){
        
                        # For each ATC which is not empty
                        if( !is.na(diseaseTable[i,currentIndex])) if(diseaseTable[i,currentIndex] != "" ){
          
                            # Add the info to the final DF
                            diseasesDBDF[currentCell,1] = diseaseTable$ID[i]
                            diseasesDBDF[currentCell,2] = diseaseTable[i , currentIndex]
                            diseasesDBDF[currentCell,3] = "Ñ99"
          
                            currentCell = currentCell + 1
          
                        }
        
                    }
      
                # Now we add the info about diabetes, ichy skin, hand eczema, rhinitis
                # asthma, atopic eczema, and psorisis. This will probably duplicate some
                # records already. It doesn't matter, we will correct that later.
                for(i in 1:originalTotalRows){
        
                        # R, I hate your NA conditionals, really. You need to check that is not
                        # NA first, then go into the IF, then check that is whatever value.
                        # NA == 1 should be FALSE!! not an ERROR! AAAAAG!!!
        
                        # Diabetes (it is not specify if it is Type I or Type II)
                        if(!is.na(diseaseTable$Diabetes[i])) if(diseaseTable$Diabetes[i] == 1 ){
          
                            diseasesDBDF[currentCell,1] = diseaseTable$ID[i]
                            diseasesDBDF[currentCell,2] = "Diabetes"
                            diseasesDBDF[currentCell,3] = "E13"       # E13 Other specified diabetes mellitus 
                            diseasesDBDF[currentCell,4] = "Endocrine, nutritional and metabolic"
          
                            currentCell = currentCell + 1          
          
                        }
        
                        # Ichy Skin
                        if(!is.na(diseaseTable$IchySkin[i])) if(diseaseTable$IchySkin[i] == 1 ){
          
                            diseasesDBDF[currentCell,1] = diseaseTable$ID[i]
                            diseasesDBDF[currentCell,2] = "Pruritus, unspecified"
                            diseasesDBDF[currentCell,3] = "L29.9"       
                            diseasesDBDF[currentCell,4] = "Skin and subcutaneous tissue"
          
                            currentCell = currentCell + 1          
          
                        }    
        
                        # Hand Eczema
                        if(!is.na(diseaseTable$HandEczema[i])) if(diseaseTable$HandEczema[i] == 1 ){
          
                            diseasesDBDF[currentCell,1] = diseaseTable$ID[i]
                            diseasesDBDF[currentCell,2] = "Dyshidrosis"
                            diseasesDBDF[currentCell,3] = "L30.1"       
                            diseasesDBDF[currentCell,4] = "Skin and subcutaneous tissue"
          
                            currentCell = currentCell + 1          
          
                        }    
        
                        # Rhinitis
                        if(!is.na(diseaseTable$Rhinitis[i])) if(diseaseTable$Rhinitis[i] == 1 ){
          
                            diseasesDBDF[currentCell,1] = diseaseTable$ID[i]
                            diseasesDBDF[currentCell,2] = "Allergic rhinitis, unspecified"
                            diseasesDBDF[currentCell,3] = "J30.9" # J30.9 Allergic rhinitis, unspecified       
                            diseasesDBDF[currentCell,4] = "Respiratory system"
          
                            currentCell = currentCell + 1          
          
                        }    
        
                        # Asthma
                        if(!is.na(diseaseTable$Asthma[i])) if(diseaseTable$Asthma[i] == 1 ){
          
                            diseasesDBDF[currentCell,1] = diseaseTable$ID[i]
                            diseasesDBDF[currentCell,2] = "Asthma"
                            diseasesDBDF[currentCell,3] = "J45.9" # J45 Generic Asthma, there are more subcodes if you want to specify
                            diseasesDBDF[currentCell,4] = "Respiratory system"
          
                            currentCell = currentCell + 1          
          
                        }    
        
                        # Atopic Eczema
                        if(!is.na(diseaseTable$AtopicEczema[i])) if(diseaseTable$AtopicEczema[i] == 1 ){
          
                            diseasesDBDF[currentCell,1] = diseaseTable$ID[i]
                            diseasesDBDF[currentCell,2] = "Atopic dermatitis, unspecified"
                            diseasesDBDF[currentCell,3] = "L20.9" 
                            diseasesDBDF[currentCell,4] = "Skin and subcutaneous tissue"
          
                            currentCell = currentCell + 1          

                        }    
        
                        # Psoriasis
                        if(!is.na(diseaseTable$Psoriasis[i])) if(diseaseTable$Psoriasis[i] == 1 ){
          
                            diseasesDBDF[currentCell,1] = diseaseTable$ID[i]
                            diseasesDBDF[currentCell,2] = "Psoriasis"
                            diseasesDBDF[currentCell,3] = "L40.9" # Psoriasis, unspecified
                            diseasesDBDF[currentCell,4] = "Skin and subcutaneous tissue"
          
                            currentCell = currentCell + 1          
          
                        }    
        
                    }
      
                # Now we delete the extra rows that we don't need.
                # Just find the first ID equal to NA and delete from there.
                # Which coincidentally is the current Cell
                diseasesDBDF = diseasesDBDF[-c(currentCell:totalDiseases),] # Column are deleted with NULL, rows are deleted with -c(), R is horrible an inconsistent 
       
            }
      
            # Now we curate the table manually
            # -- Change the common diseases names
            {
                diseasesDBDF[diseasesDBDF$Diagnostic == "Høysnuve (allergisk rinitt)",]$Diagnostic       = "Allergic rhinitis, unspecified"
                diseasesDBDF[diseasesDBDF$Diagnostic == "ADHD (hyperkinetisk forstyrrelse)",]$Diagnostic = "ADHD"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Cøliaki/glutenintoleranse",]$Diagnostic         = "Celiac disease"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Eksem",]$Diagnostic                             = "Eczema"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Matvareallergi",]$Diagnostic                    = "Food allergy"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Migrene",]$Diagnostic                           = "Migraine"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Laktoseintoleranse",]$Diagnostic                = "Lactose intolerance"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Depressiv lidelse",]$Diagnostic                 = "Depression"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Blodmangel/anemi",]$Diagnostic                  = "Anemia, unspecified"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Søvnlidelse (innsovningsvansker o/el tidlig oppvåkning)",]$Diagnostic = "Imnsonia"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Diabetes mellitus type 1",]$Diagnostic          = "Diabetes Type 1"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Angst",]$Diagnostic                             = "Anxiety"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Spenningshodepine",]$Diagnostic                 = "Tension headache (TTH)"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Gastritt",]$Diagnostic                          = "Gastritis"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Uspesifisert artritt",]$Diagnostic              = "Artritis"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Hypotyreose",]$Diagnostic                       = "Hypothyroidism"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Astma",]$Diagnostic                             = "Asthma"
                diseasesDBDF[diseasesDBDF$Diagnostic == "Spiseforstyrrelse",]$Diagnostic                 = "Eating Disorder"        
            }
        
            # -- Change all the "Others" text to something that makes sense (There is about 145, and you need to look for the ICD10 too T_T)
            {
        
				# CODE MISSING IN THIS PART; APPLY FOR FULL CODE ACCESS!
        
        
                # Some people report two or more diseases in the same comment. So here in the code, is grouped by whatever the first disease is,
                # plus then the second disease
                #
                # This is a broad classification that I pulled from nowhere in no particular order
                #
                # ------------------------------------------
                # Brain and general nervous system
                # ------------------------------------------
                

        

                
                # ------------------------------------------
                # Psychological / Psychiatric
                # ------------------------------------------
                
                # ------------------------------------------
                # Heart and blood
                # ------------------------------------------        
                
                # ------------------------------------------
                # Lungs
                # ------------------------------------------
                # ---- Asthma cases
                # ---- For whatever reason, we classified all animal allergies as simple rhinitis

                
                # ---- Added the rhinitis plus the plant allergy
                
                # ---- Added the double combo rhinitis
                
                
                # ---- Assuming respiratory allergy since nothing else is indicated
                
                # ------------------------------------------
                # Bones diseases and Physical traumas
                # ------------------------------------------
                # ---- Other disorders
                # ---- Rheumatism covers arthritis, so nothing else added
                
                # -------- Only Arthritis, possible diagnostics are left out
                # ---- Head
                # ---- Neck
                
                # ---- Spine and back
                # -------- Adding back pain as independent from scoliosis
                # -------- Adding, knee, back, and shoulder pain as indepedent from scoliosis
                # This one is unespecified in the sense that no C,D or L vertebrae is specified, although the reason for the pain is actually specified
                
                # ---- Hips
                # ---- Legs
                # ---- Arms
                
                # ---- Torso and abdomen
                  
                # ------------------------------------------
                # Skin diseases
                # ------------------------------------------
                # ---- Add the dermatitis and the bone disease again later with the same ID
                # ---- Dina's specific diagnosed this one as pruritus, so it stays in skin diseases
                
                # ------------------------------------------
                # Digestive track
                # ------------------------------------------
                
                # ---- Not sure if this should even be register since is now solved.
                # ---- As it is not specified the reason for pancreatitis, is listed as others
                
                # ------------------------------------------
                # Kidneys to genitals
                # ------------------------------------------
                                 
                        
                # ------------------------------------------
                # General infectious diseases
                # ------------------------------------------
                
                # ------------------------------------------
                # Others
                # ------------------------------------------
                
                # ---- Can't find the specific area of a cyst unless it is a neoplasm in the icd10 codes.
                
                                
                # ------------------------------------------
                # Other Autoimmune not listed before
                # ------------------------------------------
                
                # ------------------------------------------
                # No clear diagnosis that are just deleted from the list of diseases
                # ------------------------------------------
                
         
            }
      
            # -- Delete the Diabetes entries that also have a more specific diabetes (type 1 or 2)
            #    You need to open the table and look these manually
            #    But it happens that everyone who has "diabetes" is also "Diabetes type 1",
            #    So we just delete those entries
            diseasesDBDF = diseasesDBDF[diseasesDBDF$Diagnostic != "Diabetes",]
        
            # -- Delete everyone who doesn't have a diagnostic now
            diseasesDBDF = diseasesDBDF[diseasesDBDF$Diagnostic != "",]
        
            # -- Finally, looks for unique combinations of ID + Diagnostic that repeat in the table
            dupeDF = diseasesDBDF[,c(1,2)] # Select ID and Diagnostic
            diseasesDBDF = diseasesDBDF[!duplicated(dupeDF),]
      
            # Now we have all the data ready, let finish by giving more sense to the
            # ICD10 Codes
            totalDiseases = nrow(diseasesDBDF)
            for(i in 1:totalDiseases){
        
                # Add the ICD10 information
                diseasesDBDF$Title[i] = getICD10Info(diseasesDBDF$ICD10[i])  
        
            }
      
        }  
    }
  

    # Contraceptives
    {
        # Create a new temporal table
        tempTable    = originalHormonalTable
        # Add the IDs
        tempTable$ID = originalAureusTable$pers_key_ff1
        # Delete the columns that we don't need
        tempTable[,c(1:71)]                   = NULL
        tempTable[,c(14:(ncol(tempTable))-1)] = NULL

        # Take away people who don't use any contraceptives
        tempTable = tempTable[!is.na(tempTable$contraceptives_type_ff1),]
        totalRows = nrow(tempTable)
        
        # Nobody uses vaginal contraceptives, so delete those columns
        tempTable$vaginal_contracept_atc_ff1  = NULL
        tempTable$vaginal_contracept_name_ff1 = NULL
        
        # Now we have all the data that we need, now let melt by contraceptives
        # -- Create the final new dataframe 
        contraceptivesDBDF = data.frame(matrix(NA, nrow = totalRows, ncol = 5))
        colnames(contraceptivesDBDF) = c("ID", "Brand", "ATC", "Type", "Hormonal")
        
        
        # The rules for the hormonal column goes as follows
        #
        # Non-Hormonal:        Condons
        # Progestin-only:      Cerazette, Nexplanon, Depo-provera
        # Progestin-Estradiol: Mercilon, Yasminelle, Loette 28, Nuvaring,
        #                      Marvelon, Yasmin, Microgynon, Oralcon, Diane,
        #                      Synfase, Evra)
        # Unknown:             Any other brand/type 
        
        # -- For each row of the temporal table
        for(i in 1:totalRows){
          
          # Init the basic variables
          currentID       = tempTable$ID[i]
          currentBrand    = "Unknown"
          currentATC      = "Unknown"
          currentType     = tempTable$contraceptives_type_ff1[i]
          currentHormonal = "Unknown"
          
          # Pills
          if(currentType == 1){
          
              # Fix the type
              currentType     = "Oral"
              # Get the name
              currentBrand    = tempTable$oral_contracept_name_ff1[i]
            
              # If the name is "Annet", this is not a contraceptive, this is the
              # norwegian word for "Other", so this is still unknown. Same for
              # an empty string
              if(currentBrand != "Annet" && currentBrand != ""){
              
                  # Get the ATC
                  currentATC      = tempTable$oral_contracept_atc_ff1[i]
                  if(currentATC == "") currentATC = "Unknown"
              
                      # Get the type of hormonal
                      currentHormonal = getHormonalType(currentBrand)
                  }
              else{
                  currentBrand    = "Unknown"
              }
            
          }

          # Injected
          # (all cases follows the same structure as in == 1)
          if(currentType == 2){
            
              currentType     = "Injected"
              currentBrand    = tempTable$injected_contracept_name_ff1[i]
            
              if(currentBrand != "Annet" && currentBrand != ""){
              
                  currentATC      = tempTable$injected_contracept_atc_ff1[i]
                  if(currentATC == "") currentATC = "Unknown"
                  currentHormonal = getHormonalType(currentBrand)
                  
              }
              else{
                  currentBrand    = "Unknown"
              }
            
          }
          
          # Subdermal
          if(currentType == 3){
            
              currentType     = "Subdermal"
              currentBrand    = tempTable$subdermal_contracept_name_ff1[i]

              if(currentBrand != "Annet" && currentBrand != ""){

                  currentATC      = tempTable$oral_contracept_atc_ff1[i]
                  if(currentATC == "") currentATC = "Unknown"
                  currentHormonal = getHormonalType(currentBrand)
                  
              }
              else{
                  currentBrand    = "Unknown"
              }
            
          }
          
          # Condons
          if(currentType == 4){
            
            currentType     = "Condons"
            currentHormonal = "Non-hormonal"

          }
            
          # Skin
          if(currentType == 5){
            
              currentType     = "Skin"
              currentBrand    = tempTable$contracep_skin_patch_name_ff1[i]

              if(currentBrand != "Annet" && currentBrand != ""){
              
                  currentATC      = tempTable$contracep_skin_patch_atc_ff1[i]
                  if(currentATC == "") currentATC = "Unknown"
                  currentHormonal = getHormonalType(currentBrand)
                  
              }
              else{
                  currentBrand    = "Unknown"
              }
            
          }
          
          # This is vaginal, and never happens
          #if(currentType == 6)
          
          # Others (pretty much no idea of what's going on here)
          if(currentType == 7){
            
              currentType     = "Other"
              currentHormonal = "Unknown"
            
          }
          
          # Now that you have all the info properly set, put it into the
          # contraceptives dataframe
          contraceptivesDBDF$ID[i]       = currentID
          contraceptivesDBDF$Brand[i]    = currentBrand
          contraceptivesDBDF$ATC[i]      = currentATC
          contraceptivesDBDF$Type[i]     = currentType
          contraceptivesDBDF$Hormonal[i] = currentHormonal
          
          # Also, update the the medicineTable with the contraceptive type
          # medicineTable[phenotypeTable$ID == currentID,]$Contraceptives = currentType
          # Update only if the type is not condons
          
        }
        
      }
    
    
    # Medicine
    # This table includes multivitamins and sleeping pills (maybe I should call it xenotable).
    {
        
        # Variables
        # MEDICATION_DAILY_FF1	MEDICATION_BRAND1_FF1	MEDICF	MEDICATION_ATC1_FF1
        # MEDICATION_REGULAR1_FF1	MEDICATION_BRAND2_FF1	MEDICE	MEDICATION_ATC2_FF1
        # MEDICATION_REGULAR2_FF1	MEDICATION_BRAND3_FF1	MEDICD	MEDICATION_ATC3_FF1
        # MEDICATION_REGULAR3_FF1	MEDICATION_BRAND4_FF1	MEDICC	MEDICATION_ATC4_FF1
        # MEDICATION_REGULAR4_FF1	MEDICATION_BRAND5_FF1	MEDICB	MEDICATION_ATC5_FF1
        # MEDICATION_REGULAR5_FF1	MEDICATION_OTHER_FF1	MEDICATION_OTHER_DESC_FF1
        # MEDIC0	MEDIC1	MEDIC2	MEDIC3	MEDIC4	MEDIC5	MEDIC6	MEDIC7	MEDIC8	MEDIC9	MEDICA
        # ANALGETICS_FF1	ANALGETICS_BRAND1_FF1	ANALG3	ANALGETICS_ATC1_FF1	ANALGETICS_HOURS1_FF1
        # ANALGETICS_LAST_NUMBER1_FF1	ANALGETICS_BRAND2_FF1	ANALG2	ANALGETICS_ATC2_FF1	ANALGETICS_HOURS2_FF1
        # ANALGETICS_LAST_NUMBER2_FF1	ANALGETICS_BRAND3_FF1	ANALG1	ANALGETICS_ATC3_FF1	ANALGETICS_HOURS3_FF1
        # ANALGETICS_LAST_NUMBER3_FF1	ANALGETICS_BRAND4_FF1	ANALG0	ANALGETICS_ATC4_FF1	ANALGETICS_HOURS4_FF1
        # ANALGETICS_LAST_NUMBER4_FF1	ANTIBIOTICS_FF1	ANTIBIOTICS_BRAND1_FF1	ANTIB5	ANTIBIOTICS_ATC1_FF1
        # ANTIBIOTICS_BRAND2_FF1	ANTIB4	ANTIBIOTICS_ATC2_FF1	ANTIBIOTICS_BRAND3_FF1	ANTIB3	ANTIBIOTICS_ATC3_FF1
        # MEDICATION_DATE_FF1	MEDICATION_TIME_FF1	MEDICATION_SIGNATURE_FF1	PAINKILLERS_PRESC_4WEEKS_FF1
        # PAINKILLERS_NOPRESC_4WEEKS_FF1	SLEEPING_PILLS_4WEEKS_FF1	ANTIDEPRESSANTS_4WEEKS_FF1	ADHD_MEDICATION_4WEEKS_FF1
        # TRANQUILIZERS_4WEEKS_FF1

        # If a person report taking in the last 4 week:
        #
        # -- Painkillers
        # -- Sleeping pills
        # -- Antidepressant
        # -- ADHD
        # -- Tranquilizers
        #
        # Check that the medication is register, otherwise add it and label it as unknown
        
        
        # Count how many people take medicines
        
        
        # Create a new temporal table
        tempTable    = originalHormonalTable
        # Add the IDs
        tempTable$ID = basicTable$ID
        # Delete the columns that we don't need
        tempTable[,c(1:41)]                   = NULL
        tempTable[,c(18:(ncol(tempTable))-1)] = NULL
        # Take away people who don't do drugs
        tempTable = tempTable[tempTable$medication_daily_ff1 == 1,]
        # Take away people who do OTHERS drugs but are still registered as drug users
        totalRows = nrow(tempTable)
        keepRows  = rep(FALSE, totalRows)
        for(i in 1:totalRows){
          
            if(!is.na(tempTable$medication_atc1_ff1[i])) #R is a horrible language, and NA != "" is not FALSE, is NA, so you need to check for NA independently.
                if( tempTable$medication_atc1_ff1[i] != "" ) keepRows[i] = TRUE
            
        }
        tempTable = tempTable[keepRows,]
        totalRows = sum(keepRows)
        # Now we have all the data that we need, now let melt it by medication
        # -- We need to know, how many rows we need in the new dataframe. Count how
        #    many values are missing in columsn 2,3,4 and 5. We garanteed now that
        #    1 is not empty.
        na2 = sum(tempTable$medication_atc2_ff1 != "") # R is a horrible language, you can't call your variable 2na, but you can na2
        na3 = sum(tempTable$medication_atc3_ff1 != "")
        na4 = sum(tempTable$medication_atc4_ff1 != "")
        na5 = sum(tempTable$medication_atc5_ff1 != "")
        totalCells = totalRows + na2 + na3 + na4 + na5
        
        # -- Create the final new dataframe 
        drugUseDF = data.frame(matrix(NA, nrow = totalCells, ncol = 6))
        colnames(drugUseDF) = c("ID", "Type", "Brand", "ATC", "Regularity", "Content")
        cellID     = 1
        atcIndexes = c(3,6,9,12,15)
        
        # -- For each row of the temporal table
        for(i in 1:totalRows){
          
            # For each atc code
            for(j in 1:5){
            
                # get the atc code index
                currentIndex = atcIndexes[j]
            
                # For each ATC which is not empty
                if(tempTable[i,currentIndex]!=""){
              
                    # Add the info to the final DF
                    # -- ID
                    drugUseDF[cellID,1] = tempTable$ID[i]
                    # -- Type
                    drugUseDF[cellID,2] = ""
                    # -- Brand
                    drugUseDF[cellID,3] = tempTable[i , currentIndex - 1]
                    # -- ATC
                    drugUseDF[cellID,4] = tempTable[i , currentIndex]
                    # -- Regularity
                    drugUseDF[cellID,5] = tempTable[i , currentIndex + 1]
                    # -- Content
                    drugUseDF[cellID,6] = ""
              
                    cellID = cellID + 1
              
                }
            
            }
          
        }
        
        # This conclude all the medicine table, however there are a few more
        # instances to include here.
        # -- Manual review of analgesics
        {
         
            # ANALGETICS_FF1	ANALGETICS_BRAND1_FF1	ANALG3	ANALGETICS_ATC1_FF1	ANALGETICS_HOURS1_FF1
            # ANALGETICS_LAST_NUMBER1_FF1	ANALGETICS_BRAND2_FF1	ANALG2	ANALGETICS_ATC2_FF1	ANALGETICS_HOURS2_FF1
            # ANALGETICS_LAST_NUMBER2_FF1	ANALGETICS_BRAND3_FF1	ANALG1	ANALGETICS_ATC3_FF1	ANALGETICS_HOURS3_FF1
            # ANALGETICS_LAST_NUMBER3_FF1	ANALGETICS_BRAND4_FF1	ANALG0	ANALGETICS_ATC4_FF1	ANALGETICS_HOURS4_FF1
            # ANALGETICS_LAST_NUMBER4_FF1	
            
            # Create a new temporal table
            tempTable    = originalBiomarkersTable
            # Add the IDs
            tempTable$ID = basicTable$ID
            
            # Delete the columns that we don't need
            tempTable[,c(1:45)]                   = NULL
            tempTable[,c(23:(ncol(tempTable))-1)] = NULL
            
            # Clean the spaces that are in the ATC1 and ATC2 codes
            tempTable$ANALGETICS_ATC1_FF1 = gsub(" ","",tempTable$ANALGETICS_ATC1_FF1)
            tempTable$ANALGETICS_ATC2_FF1 = gsub(" ","",tempTable$ANALGETICS_ATC2_FF1)
            
            # Take away people who don't take analgetics
            tempTable = tempTable[!tempTable$ANALGETICS_FF1 == 0,]
            tempTable = tempTable[!tempTable$ANALGETICS_FF1 == "",]
            tempTable = tempTable[!is.na(tempTable$ANALGETICS_FF1),]
            totalRows = nrow(tempTable)
            
            # There is nobody taking 3 analgesics, so we check only for brand 1 and 2
            na2 = sum(tempTable$ANALGETICS_ATC2_FF1 != "")
            totalCells = totalRows + na2
         
            # -- Create the final new dataframe 
            analgesicUseDF = data.frame(matrix(NA, nrow = totalCells, ncol = 6))
            colnames(analgesicUseDF) = c("ID", "Type", "Brand", "ATC", "Regularity", "Content")
            cellID     = 1
            atcIndexes = c(4,9)
               
            # -- For each row of the temporal table
            for(i in 1:totalRows){
          
                # For each atc code
                for(j in 1:2){
            
                    # get the atc code index
                    currentIndex = atcIndexes[j]

                    # For each ATC which is not empty
                    if(tempTable[i,currentIndex]!=""){
              
                        # Add the info to the final DF
                        # -- ID
                        analgesicUseDF[cellID,1] = tempTable$ID[i]
                        # -- Type
                        analgesicUseDF[cellID,2] = ""
                        # -- Brand
                        analgesicUseDF[cellID,3] = tempTable[i , currentIndex - 2]
                        # -- ATC
                        analgesicUseDF[cellID,4] = tempTable[i , currentIndex]
                        # -- Regularity
                        analgesicUseDF[cellID,5] = ""
                        # -- Content
                        analgesicUseDF[cellID,6] = ""
              
                        cellID = cellID + 1
              
                    }
            
                }
          
            }
            
            # Clean the trailing spaces from brand names and delete NA rows
            analgesicUseDF$Brand = trimws(analgesicUseDF$Brand)
            analgesicUseDF       = analgesicUseDF[!is.na(analgesicUseDF[,1]), ]
            
            
            
        }
        
        
        # -- Manual review of antibiotics
        {
         
            # ANTIBIOTICS_FF1
            # ANTIBIOTICS_BRAND1_FF1
            # ANTIBIOTICS_ATC1_FF1
            # (Any other variables were empty columns)
            
            # Create a new temporal table
            tempTable    = originalBiomarkersTable
            # Add the IDs
            tempTable$ID = basicTable$ID
            
            # Delete the columns that we don't need
            # Nobody takes more than 1 antibiotics, so the table is simplified significantly
            tempTable[,c(1:67)]                   = NULL
            tempTable[,c(5:(ncol(tempTable))-1)] = NULL
            
            # Clean the spaces that are in the ATC1 and trailling/leading spaces in brand
            tempTable$ANTIBIOTICS_ATC1_FF1   = gsub(" ","",tempTable$ANTIBIOTICS_ATC1_FF1)
            tempTable$ANTIBIOTICS_BRAND1_FF1 = trimws(tempTable$ANTIBIOTICS_BRAND1_FF1)
            
            # Take away people who don't take analgetics
            tempTable = tempTable[!tempTable$ANTIBIOTICS_BRAND1_FF1 == 0,]
            tempTable = tempTable[!tempTable$ANTIBIOTICS_BRAND1_FF1 == "",]
            tempTable = tempTable[!is.na(tempTable$ANTIBIOTICS_BRAND1_FF1),]
            totalRows = nrow(tempTable)
            
            # -- Create the final new dataframe 
            antibioticsUseDF = data.frame(matrix(NA, nrow = totalRows, ncol = 6))
            colnames(antibioticsUseDF) = c("ID", "Type", "Brand", "ATC", "Regularity", "Content")
            cellID     = 1
            
            # -- For each row of the temporal table
            for(i in 1:totalRows){
          
            
                    # get the atc code index
                    currentIndex = 3

                    # For each ATC which is not empty
                    if(tempTable[i,currentIndex]!=""){
              
                        # Add the info to the final DF
                        # -- ID
                        antibioticsUseDF[cellID,1] = tempTable$ID[i]
                        # -- Type
                        antibioticsUseDF[cellID,2] = ""
                        # -- Brand
                        antibioticsUseDF[cellID,3] = tempTable[i , currentIndex - 2]
                        # -- ATC
                        antibioticsUseDF[cellID,4] = tempTable[i , currentIndex]
                        # -- Regularity
                        antibioticsUseDF[cellID,5] = ""
                        # -- Content
                        antibioticsUseDF[cellID,6] = ""
              
                        cellID = cellID + 1
              
                    }
            
                }
          
            

            
        }
        
        
        # -- Manual review of painkillers
        # -- Manual review of sleeping pills
        # -- Manual review of antidepressant
        # -- Manual review of ADHD medication
        # -- Manual review of Tranquilizars
        # There is a single column that keep track of these variables and is a 
        # yes/no field, that goes back to 4 week. So the question is:
        #
        #       " Have you taken sleeping pills in the last 4 weeks? "
        #
        # Since the time is so long, and the ATC is unknown, we are going to
        # skip all of these and hope that they are included in the general
        # medication table, IF they are using medication regurarly
        
        # -- Add hormonal contraceptives data
        tempTable = contraceptivesDBDF
        # Delete the non hormonal and unknowns
        tempTable = tempTable[as.character(tempTable$Hormonal) != "Non-hormonal",]
        tempTable = tempTable[as.character(tempTable$Hormonal) != "Unknown",]
        tempTable = tempTable[as.character(tempTable$ATC)      != "Unknown",]
        
        # -- Create the final new dataframe 
        hormonalUseDF = data.frame(matrix(NA, nrow = nrow(tempTable), ncol = 6))
        colnames(hormonalUseDF) = c("ID", "Type", "Brand", "ATC", "Regularity", "Content")
        
        hormonalUseDF$ID    = tempTable$ID
        hormonalUseDF$Brand = tempTable$Brand
        hormonalUseDF$ATC   = tempTable$ATC
        
        # -- Add multivitamim data
        # (do we have too?)
        
        
        # Finally, we have ALL the data ready, merge all DFs into one
        medicinesDBDF = rbind(drugUseDF,     analgesicUseDF)
        medicinesDBDF = rbind(medicinesDBDF, antibioticsUseDF)
        medicinesDBDF = rbind(medicinesDBDF, hormonalUseDF)
        
        
        # Now we have all the data ready, let finish by giving more sense to the
        # numbers, and adding some extra info about the medicine used
        for(i in 1:nrow(medicinesDBDF)){
          
          # Change the regularity string
          if(is.na(medicinesDBDF$Regularity[i])) medicinesDBDF$Regularity[i] = "Unknown"
          else{
            if(medicinesDBDF$Regularity[i] == 1) medicinesDBDF$Regularity[i] = "Regularly"
            if(medicinesDBDF$Regularity[i] == 2) medicinesDBDF$Regularity[i] = "Occasionally"
          }
          
          # Add the ATC information
          medicinesDBDF$Type [i] = getATCInfo(medicinesDBDF$ATC[i])  
          
        }
        
        
        # Everything is finish by now, but there might be repeated rows from
        # people that report the same medicine in the recurrent medicine table
        # and specific medicine table. So solve that.
        
        
        # -- Finally, looks for unique combinations of ID + Diagnostic that repeat in the table
        dupeDF = medicinesDBDF[,c(1,3)] # Select ID and Brand
        medicinesDBDF = medicinesDBDF[!duplicated(dupeDF),]
        
        
        
      }
    
}



# -----------------------------------------------------------------------------
# Count how many edges we have before we change anything yet
# -----------------------------------------------------------------------------
{

    absoluteTotalEdges = 
    sum(!is.na(networkTable$Friend1)) +
    sum(!is.na(networkTable$Friend2)) +
    sum(!is.na(networkTable$Friend3)) +
    sum(!is.na(networkTable$Friend4)) +
    sum(!is.na(networkTable$Friend5))
    
}


# -----------------------------------------------------------------------------
# Transform and clean data
#      Only original data, later you can convert original data into something new.
# -----------------------------------------------------------------------------
{

    # @@Transform Starts Here@@
    
    # Transform dates
    # ---- Convert dates strings into proper date string variables.
    {
        
        # Why in hell does SPSS has the start of Gregorian Calendar in 1582 as
        # default date for POSIX origin??? There might be a minor error coercion in
        # dates due the fact that we don't know which timezone, or time origin,
        # shall we take as a reference.
        #
        # So we delete all timezones from UTC to simply a date, which we assume
        # is GMT +1/+2 depending of time of the year. Since the time difference
        # is barelly one to two hours, and we have days resolution level, it
        # doesn't really matter.
        originSPSSDate  = as.Date("1582-10-14" , format = "%Y-%m-%d") 
        #originPOSIXDate = as.Date("1970-01-01" , format = "%Y-%m-%d") 
        #originWEIRDDate = as.Date("2000-01-01" , format = "%Y-%m-%d")
        
        # Basic Table
        {
    
            basicTable$AttendanceDateFF1  = as.Date(as_datetime(basicTable$AttendanceDateFF1,  origin = originSPSSDate))
            basicTable$AttendanceDateFF12 = as.Date(as_datetime(basicTable$AttendanceDateFF12, origin = originSPSSDate))
            basicTable$MedicationDateFF1  = as.Date(as_datetime(basicTable$MedicationDateFF1,  origin = originSPSSDate))
            basicTable$QuestionaryDateFF1 = as.Date(as_datetime(basicTable$QuestionaryDateFF1, origin = originSPSSDate))
               
        }
    
        # Social Network Table
        {

            # In this case, the date was saved as a string (US format! ¬¬) rather than POSIX
            networkTable$NetworkDateFF1  = as.Date(networkTable$NetworkDateFF1,       format = "%m/%d/%Y")    
            networkTable$NetworkDateFF12 = as.Date(as_datetime(networkTable$NetworkDateFF12,  origin = originSPSSDate))
            
        }
        
        # Aureus table
        {
    
            aureusTable$S1_AttendanceDate     = as.Date(as_datetime(aureusTable$S1_AttendanceDate,    origin = originSPSSDate))
            aureusTable$S1_R2_AttendanceDate  = as.Date(as_datetime(aureusTable$S1_R2_AttendanceDate, origin = originSPSSDate))
            aureusTable$S2_AttendanceDate     = as.Date(as_datetime(aureusTable$S2_AttendanceDate,    origin = originSPSSDate))
            aureusTable$S1_CultureDate        = as.Date(as_datetime(aureusTable$S1_CultureDate,       origin = originSPSSDate))
            #aureusTable$S2_CultureDate       = as_datetime(aureusTable$S2_CultureDate,       origin = originSPSSDate) We don't have this data, all values are NA

        }
        
        # Swabbing technical table
        {
        
            swabbingTable$S1_Nasal_FreezeDate  = as.Date(as_datetime(swabbingTable$S1_Nasal_FreezeDate,  origin = originSPSSDate))
            swabbingTable$S1_Throat_FreezeDate = as.Date(as_datetime(swabbingTable$S1_Throat_FreezeDate, origin = originSPSSDate))
        
        }
        
        # Blood table
        {
            bloodTable$BloodAnalysisDate    = as.Date(as_datetime(bloodTable$BloodAnalysisDate,    origin = originSPSSDate))
            bloodTable$PlasmaAnalysisDate   = as.Date(as_datetime(bloodTable$PlasmaAnalysisDate,   origin = originSPSSDate))
        }
    
        # Menstruation Table
        {
    
            # The first date is when the questionary was filled
            # The second date is when did you start menstruating
            # (which is not a very good variable because we don't have the
            #  exact birthdate, so we can't transform to float, and we have a
            #  better approximation in the puberty table for women)
            
            menstruationTable$Date             = as.Date(as_datetime(menstruationTable$Date, origin = originSPSSDate))
            
            menstruationTable$MenstruationDate = as.Date(menstruationTable$MenstruationDate, format = "%Y-%m-%d")

        }            
        
        # Friendship Table
        {
        
            frienshipTable$Created    = as.Date(frienshipTable$Created, format = "%d/%m/%Y")
                
        }
        
        
        
        
        
        
    }

    # ---- Convert numbers into categories
    {

        # R is horrible, all of this could be made much more legible and
        # efficient using references, which R don't have by default. You could
        # emulate it using environment variables, which make the legible part
        # even more horrible ¬¬
        
        # CSV also have the limitation that doesn't allow for sorted
        # enumerations. Or ontology anotations. However we adverted adding some
        # extra .info later to each .csv file.
        
        # In here we also add the factor levels. This is useless here because
        # we are not going to do any analysis yet, however this is use to give
        # order to the categorical where needed, which later goes into the .info
        # which later is read accordingly for whatever program.
        
        # Basics
        {
    
            # -- Sex 1 = Man 0 = Woman
            basicTable$Sex    = mapvalues( basicTable$Sex,
                                           from = c( 1,    0      ),
                                           to   = c("Man", "Woman"))
    
            basicTable$GeneralHealth = mapvalues( basicTable$GeneralHealth,
                                                  from = c( 1,          2,     3,                      4,      5,           NA),
                                                  to   = c("Very bad", "Bad", "Neither good nor bad", "Good", "Excellent", "Didn't Answer"))

            basicTable$Sex           = factor(basicTable$Sex,           levels = c("Man",      "Woman"                                                             ))  
            basicTable$GeneralHealth = factor(basicTable$GeneralHealth, levels = c("Very bad", "Bad",  "Neither good nor bad", "Good", "Excellent", "Didn't Answer"))  
            
        }
        
        # Network technical details (nothing to do here)
             
        # Network (nothing here)
        {
    
            # Technically, there are a bunch of 1s and 0s that represent yes or no, but I'm going to leave then
            # as they are because is faster to do math with that rather than "yes", "no", types.

            # I can change them to TRUE or FALSE, but then we lost the NAs information
            # Later on we change NAs to -1 to represent to friend in here, rather than FALSE which would mean no contact with that friend

        }        
        
        # Antropometry
        # -- Everything is a float number, so no changes
        
        # S.Aureus table (Everything repeat for swap 1 and swap 2 (S1, S2)
        {
         
            # Nothing to do with the dates
            
            # The experiment grew something in the agar plate
            # ----  (1 = Yes, 0 = No, 9 = Non-applicable)
            aureusTable$S1_BacterialNasalGrowth  = mapvalues( aureusTable$S1_BacterialNasalGrowth,
                                                              from = c( "1",  "0",  "9",               NA),
                                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))

            aureusTable$S1_BacterialThroatGrowth  = mapvalues( aureusTable$S1_BacterialThroatGrowth,
                                                              from = c( "1",  "0",  "9",               NA),
                                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))       
            
            aureusTable$S2_BacterialNasalGrowth  = mapvalues( aureusTable$S2_BacterialNasalGrowth,
                                                              from = c( "1",  "0",  "9",               NA),
                                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))
            aureusTable$S2_BacterialThroatGrowth  = mapvalues( aureusTable$S2_BacterialThroatGrowth,
                                                              from = c( "1",  "0",  "9",               NA),
                                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))                    
            
            
            
            # The experiment grew SA in the agar plate (Direct Culture)
            #
            # According to the Metadata description, a 9 shouldn't be possible  in the total grow
            # Yet it exist, this is true for all Light, Moderate, Rich variables
            aureusTable$S1_SA_Direct_NasalGrowth = mapvalues( aureusTable$S1_SA_Direct_NasalGrowth,
                                                              from = c( "1",  "0",  "9",               NA),
                                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))

            aureusTable$S1_SA_Direct_ThroatGrowth = mapvalues( aureusTable$S1_SA_Direct_ThroatGrowth,
                                                               from = c( "1",  "0",  "9",               NA),
                                                               to   = c("Yes", "No", "Non-applicable",  "Unknown"))
            
            aureusTable$S1_SA_Direct_NasalPopulation = mapvalues( aureusTable$S1_SA_Direct_NasalPopulation,
                                                                  from = c( "0",     "1",        "2",     NA,           "9"),
                                                                  to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))
            
            aureusTable$S1_SA_Direct_ThroatPopulation = mapvalues( aureusTable$S1_SA_Direct_ThroatPopulation,
                                                                   from = c( "0",     "1",        "2",     NA,           "9"),
                                                                   to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))

            aureusTable$S2_SA_Direct_NasalGrowth = mapvalues( aureusTable$S2_SA_Direct_NasalGrowth,
                                                              from = c( "1",  "0",  "9",               NA),
                                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))
            aureusTable$S2_SA_Direct_ThroatGrowth = mapvalues( aureusTable$S2_SA_Direct_ThroatGrowth,
                                                               from = c( "1",  "0",  "9",               NA),
                                                               to   = c("Yes", "No", "Non-applicable",  "Unknown"))

            aureusTable$S2_SA_Direct_NasalPopulation = mapvalues( aureusTable$S2_SA_Direct_NasalPopulation,
                                                                  from = c( "0",     "1",        "2",     NA,           "9"),
                                                                  to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))
            aureusTable$S2_SA_Direct_ThroatPopulation = mapvalues( aureusTable$S2_SA_Direct_ThroatPopulation,
                                                                   from = c( "0",     "1",        "2",     NA,           "9"),
                                                                   to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))
            
            
            # The experiment grew SA, and we try to make it grow even more (Enrichment Broth)
            aureusTable$S1_SA_Enrich_NasalGrowth = mapvalues( aureusTable$S1_SA_Enrich_NasalGrowth,
                                                              from = c( "1",  "0",  "9",               NA),
                                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))
    
            aureusTable$S1_SA_Enrich_ThroatGrowth = mapvalues( aureusTable$S1_SA_Enrich_ThroatGrowth,
                                                               from = c( "1",  "0",  "9",               NA),
                                                               to   = c("Yes", "No", "Non-applicable",  "Unknown"))            
            
            aureusTable$S1_SA_Enrich_NasalPopulation = mapvalues( aureusTable$S1_SA_Enrich_NasalPopulation,
                                                                  from = c( "0",     "1",        "2",     NA,           "9"),
                                                                  to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))
            
            aureusTable$S1_SA_Enrich_ThroatPopulation = mapvalues( aureusTable$S1_SA_Enrich_ThroatPopulation,
                                                                   from = c( "0",     "1",        "2",     NA,           "9"),
                                                                   to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))
            
            aureusTable$S2_SA_Enrich_NasalGrowth = mapvalues( aureusTable$S2_SA_Enrich_NasalGrowth,
                                                              from = c( "1",  "0",  "9",               NA),
                                                              to   = c("Yes", "No", "Non-applicable",  "Unknown"))
            aureusTable$S2_SA_Enrich_ThroatGrowth = mapvalues( aureusTable$S2_SA_Enrich_ThroatGrowth,
                                                               from = c( "1",  "0",  "9",               NA),
                                                               to   = c("Yes", "No", "Non-applicable",  "Unknown"))
            
            aureusTable$S2_SA_Enrich_NasalPopulation = mapvalues( aureusTable$S2_SA_Enrich_NasalPopulation,
                                                                  from = c( "0",     "1",        "2",     NA,           "9"),
                                                                  to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))
            aureusTable$S2_SA_Enrich_ThroatPopulation = mapvalues( aureusTable$S2_SA_Enrich_ThroatPopulation,
                                                                   from = c( "0",     "1",        "2",     NA,           "9"),
                                                                   to   = c( "Light", "Moderate", "Rich",  "None",       "Non-applicable"))                        
            
            
            # Coagulase test to check for the presence of SA (positive) or S.Epidermitis or S.Saprophyticus.
            aureusTable$S1_Direct_CoagulaseNasal  = mapvalues( aureusTable$S1_Direct_CoagulaseNasal,
                                                               from = c( "1",       "0",        "9",              NA),
                                                               to   = c("Positive", "Negative", "Non-applicable", "Unknown"))
    
            aureusTable$S1_Direct_CoagulaseThroat = mapvalues( aureusTable$S1_Direct_CoagulaseThroat,
                                                               from = c( "1",       "0",        "9",               NA),
                                                               to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
    
            aureusTable$S1_Enrich_CoagulaseNasal  = mapvalues( aureusTable$S1_Enrich_CoagulaseNasal,
                                                               from = c( "1",       "0",        "9",               NA),
                                                               to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
    
            aureusTable$S1_Enrich_CoagulaseThroat = mapvalues( aureusTable$S1_Enrich_CoagulaseThroat,
                                                               from = c( "1",       "0",        "9",               NA),
                                                               to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))

            aureusTable$S2_Direct_CoagulaseNasal  = mapvalues( aureusTable$S2_Direct_CoagulaseNasal,
                                                               from = c( "1",       "0",        "9",              NA),
                                                               to   = c("Positive", "Negative", "Non-applicable", "Unknown"))
    
            aureusTable$S2_Direct_CoagulaseThroat = mapvalues( aureusTable$S2_Direct_CoagulaseThroat,
                                                               from = c( "1",       "0",        "9",               NA),
                                                               to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
    
            aureusTable$S2_Enrich_CoagulaseNasal  = mapvalues( aureusTable$S2_Enrich_CoagulaseNasal,
                                                               from = c( "1",       "0",        "9",               NA),
                                                               to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
    
            aureusTable$S2_Enrich_CoagulaseThroat = mapvalues( aureusTable$S2_Enrich_CoagulaseThroat,
                                                               from = c( "1",       "0",        "9",               NA),
                                                               to   = c("Positive", "Negative", "Non-applicable",  "Unknown"))
            
            
            # SPA-Typing variables
            # (nothing to do here, they are string with bacterias IDs)
            
         
        
      }
        
        # Swabbing technical table
        {
            
         
            # This information is nowhere to be found actually, it looks like
            # 1 is yes, and 0 is no, by the amounts of 1s and 0s that there are
            #
            # (not like they have a normalization of 0/1 anyway ¬¬)
            swabbingTable$S1_NasalOK    = mapvalues( swabbingTable$S1_NasalOK,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown")) 
            
            swabbingTable$S1_ThroatOK   = mapvalues( swabbingTable$S1_ThroatOK,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown")) 
            
            
            # Lab comments, nothing to do with that, day 0 and day 2 are not
            # registered anyway and enrich and staph are verbose
            
            swabbingTable$S1_Performed   = mapvalues( swabbingTable$S1_Performed,
                                           from = c( 1,           0,               2,                               NA),
                                           to   = c("Performed", "Not performed", "Performed with irregularities", "Unknown")) 
            
            swabbingTable$S1_Event       = mapvalues( swabbingTable$S1_Event,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown")) 
            
            swabbingTable$S1_Medical_Event       = mapvalues( swabbingTable$S1_Medical_Event,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown")) 
        
            swabbingTable$S1_Technical_Event       = mapvalues( swabbingTable$S1_Technical_Event,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown"))             
            
            swabbingTable$S1_Abort_Event       = mapvalues( swabbingTable$S1_Abort_Event,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown"))             
            
            swabbingTable$S1_Other_Event       = mapvalues( swabbingTable$S1_Other_Event,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown")) 

            # Same thing for S2
            
            swabbingTable$S2_NasalOK    = mapvalues( swabbingTable$S2_NasalOK,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown")) 
            
            swabbingTable$S2_ThroatOK   = mapvalues( swabbingTable$S2_ThroatOK,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown")) 
            
            swabbingTable$S2_Performed   = mapvalues( swabbingTable$S2_Performed,
                                           from = c( 1,           0,               2,                               NA),
                                           to   = c("Performed", "Not performed", "Performed with irregularities", "Unknown")) 
            
            swabbingTable$S2_Event       = mapvalues( swabbingTable$S2_Event,
                                           from = c( 1,    0,     NA),
                                           to   = c("Yes", "No", "Unknown")) 
            
            swabbingTable$S2_Medical_Event  = mapvalues( swabbingTable$S2_Medical_Event,
                                              from = c( 1,    0,     NA),
                                              to   = c("Yes", "No", "Unknown")) 
        
            swabbingTable$S2_Technical_Event = mapvalues( swabbingTable$S2_Technical_Event,
                                               from = c( 1,    0,     NA),
                                               to   = c("Yes", "No", "Unknown"))             
            
            swabbingTable$S2_Abort_Event     = mapvalues( swabbingTable$S2_Abort_Event,
                                               from = c( 1,    0,     NA),
                                               to   = c("Yes", "No", "Unknown"))             
            
            swabbingTable$S2_Other_Event     = mapvalues( swabbingTable$S2_Other_Event,
                                               from = c( 1,    0,     NA),
                                               to   = c("Yes", "No", "Unknown")) 

            # Information about trying to do the swabbing for the second time
            # -- For S1
            
            # IMPORTANT!!
            # The 9 here is not defined in the metadata as a possible awnser
            # I'm making it up that it means Non-Applicable because it is
            # in line with the rest of the transformation.
            
            swabbingTable$S1_Repeated_Performed   = mapvalues( swabbingTable$S1_Repeated_Performed,
                                                    from = c( 1,           0,               2,                               NA),
                                                    to   = c("Performed", "Not performed", "Performed with irregularities", "Unknown")) 
            
            swabbingTable$S1_Nose_Repeated       = mapvalues( swabbingTable$S1_Nose_Repeated,
                                                   from = c( 1,    0,     9,                NA),
                                                   to   = c("Yes", "No", "Non-applicable", "Unknown"))             
            
            swabbingTable$S1_Throat_Repeated     = mapvalues( swabbingTable$S1_Throat_Repeated,
                                                   from = c( 1,    0,     9,                NA),
                                                   to   = c("Yes", "No", "Non-applicable", "Unknown"))             


            swabbingTable$S1_Repeated_Event     = mapvalues( swabbingTable$S1_Repeated_Event,
                                                   from = c( 1,    0,     NA),
                                                   to   = c("Yes", "No", "Unknown")) 
            
            swabbingTable$S1_Repeated_Medical_Event     = mapvalues( swabbingTable$S1_Repeated_Medical_Event,
                                                   from = c( 1,    0,     NA),
                                                   to   = c("Yes", "No", "Unknown")) 
            
            swabbingTable$S1_Repeated_Technical_Event     = mapvalues( swabbingTable$S1_Repeated_Technical_Event,
                                                   from = c( 1,    0,     NA),
                                                   to   = c("Yes", "No", "Unknown")) 
            
            swabbingTable$S1_Repeated_Abort_Event     = mapvalues( swabbingTable$S1_Repeated_Abort_Event,
                                                   from = c( 1,    0,     NA),
                                                   to   = c("Yes", "No", "Unknown")) 
            
            swabbingTable$S1_Repeated_Other_Event     = mapvalues( swabbingTable$S1_Repeated_Other_Event,
                                                   from = c( 1,    0,     NA),
                                                   to   = c("Yes", "No", "Unknown"))             
        
            # -- For S2 (we don't have it)
            
               
        }
        
        # Highschool
        {
            
            # Highschools are divided into 1, 2, 3,... and so on. Same goes
            # for Programme and classes. Each represent a different, place or
            # concept, but the numerical information is totally irrelevant.
            #
            # So all of those are converted from numerical to categorical.
            
            for(i in 1:originalTotalRows){
                
                currentHighschool = highSchoolTable$HighSchool[i]
                currentClass      = highSchoolTable$Class[i]
                currentProgramme  = highSchoolTable$Programme[i]
                
                currentHighschool = paste0("H",currentHighschool)
                currentClass      = paste0("C",currentClass)
                currentProgramme  = paste0("P",currentProgramme)
                
                highSchoolTable$HighSchool[i] = currentHighschool
                highSchoolTable$Class[i]      = currentClass
                highSchoolTable$Programme[i]  = currentProgramme
                    
            }
            
            
            
            
            # The only variable that has a meaning is the Main programme, which
            # is also converted here.
            highSchoolTable$MainPrograme = mapvalues( highSchoolTable$MainPrograme,
                                                      from = c( "1",       "2",      "3",           NA),
                                                      to   = c( "General", "Sports", "Vocational",  "Unknown"))
            
        }
        
        # Blood (we only change the date, so nothing else)
        
        # Blood Technical table
        {

            bloodTechnicalTable$BloodTestPerformed    = mapvalues( bloodTechnicalTable$BloodTestPerformed,
                                                        from = c( 1,          0,               2,                " ",        "",        NA),
                                                        to   = c("Performed", "Not performed", "Irregularities", "Unknown", "Unknown",  "Unknown"))
            
            bloodTechnicalTable$BloodEvent            = mapvalues( bloodTechnicalTable$BloodEvent,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))
            
            bloodTechnicalTable$BloodMedicalEvent     = mapvalues( bloodTechnicalTable$BloodMedicalEvent,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))
                        
            bloodTechnicalTable$BloodTechnicalEvent   = mapvalues( bloodTechnicalTable$BloodTechnicalEvent,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))
            
            bloodTechnicalTable$BloodAborted          = mapvalues( bloodTechnicalTable$BloodAborted,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))
            
            bloodTechnicalTable$BloodOther            = mapvalues( bloodTechnicalTable$BloodOther,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))            
        
            bloodTechnicalTable$Blood_S25OH_Event     = mapvalues( bloodTechnicalTable$Blood_S25OH_Event,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))            
            
            bloodTechnicalTable$Blood_Retinol_Event   = mapvalues( bloodTechnicalTable$Blood_Retinol_Event,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))                        
            
            
                        
        }
    
        # Sociology
        # (this one is long and weird)
        {
            
            sociologyTable$Live_With_Mother            = mapvalues( sociologyTable$Live_With_Mother,
                                                         from = c( 1,     0,    NA),
                                                         to   = c("Yes", "No", "Unknown"))
            
            sociologyTable$Live_With_Father            = mapvalues( sociologyTable$Live_With_Father,
                                                         from = c( 1,     0,    NA),
                                                         to   = c("Yes", "No", "Unknown"))            
            
            sociologyTable$Live_With_12_S            = mapvalues( sociologyTable$Live_With_12_S,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))            

            sociologyTable$Live_With_3m_S            = mapvalues( sociologyTable$Live_With_3m_S,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))
            
            # This column is new, but I think is worth it to add it here
            sociologyTable$Live_With_Zero_S          = "No" # We need to convert this variable later
            
            # One of the things I hate about R is that comparing NAs gives you NAs, so the code
            # become dirty very quickly. If I want to say x == 4, and x = NA, the awnser is NA
            # instead of FALSE. So you need an extra step to ensure that you get a FALSE >:(.
            #
            # And course, on top of that, you don't have the damn switch, so is a messy if/else
            # structure all the way down.
            for (i in 1:originalTotalRows) {

                # 12s = NA
                if(is.na(sociologyTable$Live_With_12_S[i]) == TRUE){
                    
                    # NA + NA = NA
                    if(is.na(sociologyTable$Live_With_3m_S[i] == TRUE)){
                        sociologyTable$Live_With_Zero_S[i] = "Unknown"
                    }
                    else{
                    
                        # NA + 0 = NA
                        if(sociologyTable$Live_With_3m_S[i] == "No"){
                            sociologyTable$Live_With_Zero_S[i] = "Unknown"    
                        }   
                        # NA + 1 = 0
                        # Already Init to 0
                        
                    }
                    
                }
                else{
                    
                    # 12s = 0
                    if(sociologyTable$Live_With_12_S[i] == "No"){

                        # 0 + NA = NA
                        if(is.na(sociologyTable$Live_With_3m_S[i] == TRUE)){
                    
                            sociologyTable$Live_With_Zero_S[i] = "Unknown"    
                    
                        }
                        else{
                         
                            # 0 + 0 = 1
                            if(sociologyTable$Live_With_3m_S[i] == "No"){
                    
                                sociologyTable$Live_With_Zero_S[i] = "Yes"    
                                
                            }
                            # 0 + 1 = 0
                            # Already init to 0
                            
                    
                        }
                               
                            
                    }
                    # 12s = 1
                    # In this case we don't do anything because is already
                    # init to "No"
                    # 1 + NA = 0
                    # 1 + 0  = 0
                    # 1 + 1  = Impossible case, 0

                }
                
            }
            
            # Finally, we can convert the three variables to a single variable
            # with the following options:
            #
            # Live with siblings:
            #     - Unknown
            #     - Zero
            #     - One or Two
            #     - Three or more
            sociologyTable$Live_With_Siblings = "Unknown"
            for (i in 1:originalTotalRows) {
                
                if(sociologyTable$Live_With_Zero_S[i] == "Yes") sociologyTable$Live_With_Siblings[i] = "Zero"
                if(sociologyTable$Live_With_12_S[i]   == "Yes") sociologyTable$Live_With_Siblings[i] = "One or Two"
                if(sociologyTable$Live_With_3m_S[i]   == "Yes") sociologyTable$Live_With_Siblings[i] = "Three of more"
                
            }
            sociologyTable$Live_With_Zero_S = NULL
            sociologyTable$Live_With_12_S   = NULL
            sociologyTable$Live_With_3m_S   = NULL
            
            
            sociologyTable$Live_With_Stepfather       = mapvalues( sociologyTable$Live_With_Stepfather,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))
            
            sociologyTable$Live_With_Stepmother       = mapvalues( sociologyTable$Live_With_Stepmother,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))
            
            sociologyTable$Live_With_Fosterparents    = mapvalues( sociologyTable$Live_With_Fosterparents,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))                  
            
            sociologyTable$Live_With_Adoptiveparents  = mapvalues( sociologyTable$Live_With_Adoptiveparents,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))                  
            
            sociologyTable$Live_With_Grandparents     = mapvalues( sociologyTable$Live_With_Grandparents,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))                  
            
            sociologyTable$Live_With_Friends          = mapvalues( sociologyTable$Live_With_Friends,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))                              
            
            sociologyTable$Live_With_Nobody           = mapvalues( sociologyTable$Live_With_Nobody,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))                  
            
            sociologyTable$Live_With_Institution      = mapvalues( sociologyTable$Live_With_Institution,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))      
            
            sociologyTable$Live_With_Other            = mapvalues( sociologyTable$Live_With_Other,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown"))                              
            
            
            sociologyTable$When_Left_Home             = mapvalues( sociologyTable$When_Left_Home,
                                                        from = c( 1,                    2,                3,              4,                   NA),
                                                        to   = c("Less than 6 months", "6 to 11 months", "1 to 2 years", "More than 2 years", "Unknown"))            
            
            
            # Mother and father working variables
            # -- Let do the easiest one first
            # ---- Mother
            sociologyTable$Mother_Studying            = mapvalues( sociologyTable$Mother_Work_School,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown")) 
            
                        
            sociologyTable$Mother_Domestic            = mapvalues( sociologyTable$Mother_Work_Domestic,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown")) 
            
                        
            sociologyTable$Mother_Disable            = mapvalues( sociologyTable$Mother_Work_Disabled,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown")) 
            
            # -- Now let figure it out the complicated one
            #    Again, R does not allow for proper flow structures :(
            for (i in 1:originalTotalRows){
                
                # All the data is NA, in this case the default is Didn't answer
                # This is already init, so we go to the other case in which at
                # least one option is not NA
                #
                # Problem is, you could still have 0s and NA, so we need at
                # least one 1, to answer this question
                Mother_Work_FulltimeNA      = is.na(sociologyTable$Mother_Work_Fulltime[i])
                Mother_Work_ParttimeNA      = is.na(sociologyTable$Mother_Work_Parttime[i])
                Mother_Work_UnemployedNA    = is.na(sociologyTable$Mother_Work_Unemployed[i])
                Mother_Work_PensionedNA     = is.na(sociologyTable$Mother_Work_Pensioned[i])
                Mother_Work_DeceasedNA      = is.na(sociologyTable$Mother_Work_Deceased[i])
                Mother_Work_DontKnownNA     = is.na(sociologyTable$Mother_Work_DontKnown[i])
                Mother_Work_OtherNA         = is.na(sociologyTable$Mother_Work_Other[i])
                
                totalNAs                    = sum(Mother_Work_FulltimeNA + Mother_Work_ParttimeNA + Mother_Work_UnemployedNA +
                                                  Mother_Work_PensionedNA + Mother_Work_DeceasedNA + Mother_Work_DontKnownNA +
                                                  Mother_Work_OtherNA)
                
                Mother_Work_Fulltime0      = FALSE
                Mother_Work_Parttime0      = FALSE
                Mother_Work_Unemployed0    = FALSE
                Mother_Work_Pensioned0     = FALSE
                Mother_Work_Deceased0      = FALSE
                Mother_Work_DontKnown0     = FALSE
                Mother_Work_Other0         = FALSE
                
                Mother_Work_Fulltime0      = if(!is.na(sociologyTable$Mother_Work_Fulltime[i]))   sociologyTable$Mother_Work_Fulltime[i]   == 0
                Mother_Work_Parttime0      = if(!is.na(sociologyTable$Mother_Work_Parttime[i]))   sociologyTable$Mother_Work_Parttime[i]   == 0
                Mother_Work_Unemployed0    = if(!is.na(sociologyTable$Mother_Work_Unemployed[i])) sociologyTable$Mother_Work_Unemployed[i] == 0
                Mother_Work_Pensioned0     = if(!is.na(sociologyTable$Mother_Work_Pensioned[i]))  sociologyTable$Mother_Work_Pensioned[i]  == 0
                Mother_Work_Deceased0      = if(!is.na(sociologyTable$Mother_Work_Deceased[i]))   sociologyTable$Mother_Work_Deceased[i]   == 0
                Mother_Work_DontKnown0     = if(!is.na(sociologyTable$Mother_Work_DontKnown[i]))  sociologyTable$Mother_Work_DontKnown[i]  == 0
                Mother_Work_Other0         = if(!is.na(sociologyTable$Mother_Work_Other[i]))      sociologyTable$Mother_Work_Other[i]      == 0
                
                total0s                    = sum(Mother_Work_Fulltime0  + Mother_Work_Parttime0 + Mother_Work_Unemployed0 +
                                                 Mother_Work_Pensioned0 + Mother_Work_Deceased0 + Mother_Work_DontKnown0  +
                                                 Mother_Work_Other0)
                
                Mother_Work_Fulltime1      = FALSE
                Mother_Work_Parttime1      = FALSE
                Mother_Work_Unemployed1    = FALSE
                Mother_Work_Pensioned1     = FALSE
                Mother_Work_Deceased1      = FALSE
                Mother_Work_DontKnown1     = FALSE
                Mother_Work_Other1         = FALSE
                
                Mother_Work_Fulltime1      = if(!is.na(sociologyTable$Mother_Work_Fulltime[i]))   sociologyTable$Mother_Work_Fulltime[i]   == 1
                Mother_Work_Parttime1      = if(!is.na(sociologyTable$Mother_Work_Parttime[i]))   sociologyTable$Mother_Work_Parttime[i]   == 1
                Mother_Work_Unemployed1    = if(!is.na(sociologyTable$Mother_Work_Unemployed[i])) sociologyTable$Mother_Work_Unemployed[i] == 1
                Mother_Work_Pensioned1     = if(!is.na(sociologyTable$Mother_Work_Pensioned[i]))  sociologyTable$Mother_Work_Pensioned[i]  == 1
                Mother_Work_Deceased1      = if(!is.na(sociologyTable$Mother_Work_Deceased[i]))   sociologyTable$Mother_Work_Deceased[i]   == 1
                Mother_Work_DontKnown1     = if(!is.na(sociologyTable$Mother_Work_DontKnown[i]))  sociologyTable$Mother_Work_DontKnown[i]  == 1
                Mother_Work_Other1         = if(!is.na(sociologyTable$Mother_Work_Other[i]))      sociologyTable$Mother_Work_Other[i]      == 1
                
                total1s                    = sum(Mother_Work_Fulltime1  + Mother_Work_Parttime1 + Mother_Work_Unemployed1 +
                                                 Mother_Work_Pensioned1 + Mother_Work_Deceased1 + Mother_Work_DontKnown1  +
                                                 Mother_Work_Other1)                
                
                # Find out if there is at least one 1 answer
                # and if there is, find the first valid answer.
                if( total1s > 0 ){
                    
                    # AGAIN; no switch. R bad. :(
                    if(Mother_Work_Fulltime1 == TRUE){
                        sociologyTable$Mother_WorkTime[i] = "Full time"
                    }
                    else{
                        if(Mother_Work_Parttime1 == TRUE){
                            sociologyTable$Mother_WorkTime[i] = "Part time"    
                        }
                        else{
                            if(Mother_Work_Unemployed1 == TRUE){
                                sociologyTable$Mother_WorkTime[i] = "Unemployed"        
                            }
                            else{
                                if(Mother_Work_Pensioned1 == TRUE){
                                    sociologyTable$Mother_WorkTime[i] = "Pensioned"        
                                }
                                else{
                                    if(Mother_Work_Deceased1 == TRUE){
                                        sociologyTable$Mother_WorkTime[i] = "Deceased"        
                                    }
                                    else{
                                        if(Mother_Work_DontKnown1 == TRUE){
                                            sociologyTable$Mother_WorkTime[i] = "Don't know" 
                                        }
                                        else{
                                            if(Mother_Work_Other1 == TRUE){
                                                sociologyTable$Mother_WorkTime[i] = "Other" 
                                            }
                                            # Default is Didn't answer, so no need to go deeper    
                                        }
                                    }
                                }
                            }
                        }
                    }
                    
                    
                    
                    
                    
                    
                    
                }
                
            }
            
            
            # ---- Father
            sociologyTable$Father_Studying            = mapvalues( sociologyTable$Father_Work_School,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown")) 
            
                        
            sociologyTable$Father_Domestic            = mapvalues( sociologyTable$Father_Work_Domestic,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown")) 
            
                        
            sociologyTable$Father_Disable            = mapvalues( sociologyTable$Father_Work_Disabled,
                                                        from = c( 1,     0,    NA),
                                                        to   = c("Yes", "No", "Unknown")) 
            
            # -- Now let figure it out the complicated one
            #    Again, R does not allow for proper flow structures :(
            for (i in 1:originalTotalRows){
                
                # All the data is NA, in this case the default is Didn't answer
                # This is already init, so we go to the other case in which at
                # least one option is not NA
                #
                # Problem is, you could still have 0s and NA, so we need at
                # least one 1, to answer this question
                Father_Work_FulltimeNA      = is.na(sociologyTable$Father_Work_Fulltime[i])
                Father_Work_ParttimeNA      = is.na(sociologyTable$Father_Work_Parttime[i])
                Father_Work_UnemployedNA    = is.na(sociologyTable$Father_Work_Unemployed[i])
                Father_Work_PensionedNA     = is.na(sociologyTable$Father_Work_Pensioned[i])
                Father_Work_DeceasedNA      = is.na(sociologyTable$Father_Work_Deceased[i])
                Father_Work_DontKnownNA     = is.na(sociologyTable$Father_Work_DontKnown[i])
                Father_Work_OtherNA         = is.na(sociologyTable$Father_Work_Other[i])
                
                totalNAs                    = sum(Father_Work_FulltimeNA + Father_Work_ParttimeNA + Father_Work_UnemployedNA +
                                                  Father_Work_PensionedNA + Father_Work_DeceasedNA + Father_Work_DontKnownNA +
                                                  Father_Work_OtherNA)
                
                Father_Work_Fulltime0      = FALSE
                Father_Work_Parttime0      = FALSE
                Father_Work_Unemployed0    = FALSE
                Father_Work_Pensioned0     = FALSE
                Father_Work_Deceased0      = FALSE
                Father_Work_DontKnown0     = FALSE
                Father_Work_Other0         = FALSE
                
                Father_Work_Fulltime0      = if(!is.na(sociologyTable$Father_Work_Fulltime[i]))   sociologyTable$Father_Work_Fulltime[i]   == 0
                Father_Work_Parttime0      = if(!is.na(sociologyTable$Father_Work_Parttime[i]))   sociologyTable$Father_Work_Parttime[i]   == 0
                Father_Work_Unemployed0    = if(!is.na(sociologyTable$Father_Work_Unemployed[i])) sociologyTable$Father_Work_Unemployed[i] == 0
                Father_Work_Pensioned0     = if(!is.na(sociologyTable$Father_Work_Pensioned[i]))  sociologyTable$Father_Work_Pensioned[i]  == 0
                Father_Work_Deceased0      = if(!is.na(sociologyTable$Father_Work_Deceased[i]))   sociologyTable$Father_Work_Deceased[i]   == 0
                Father_Work_DontKnown0     = if(!is.na(sociologyTable$Father_Work_DontKnown[i]))  sociologyTable$Father_Work_DontKnown[i]  == 0
                Father_Work_Other0         = if(!is.na(sociologyTable$Father_Work_Other[i]))      sociologyTable$Father_Work_Other[i]      == 0
                
                total0s                    = sum(Father_Work_Fulltime0  + Father_Work_Parttime0 + Father_Work_Unemployed0 +
                                                 Father_Work_Pensioned0 + Father_Work_Deceased0 + Father_Work_DontKnown0  +
                                                 Father_Work_Other0)
                
                Father_Work_Fulltime1      = FALSE
                Father_Work_Parttime1      = FALSE
                Father_Work_Unemployed1    = FALSE
                Father_Work_Pensioned1     = FALSE
                Father_Work_Deceased1      = FALSE
                Father_Work_DontKnown1     = FALSE
                Father_Work_Other1         = FALSE
                
                Father_Work_Fulltime1      = if(!is.na(sociologyTable$Father_Work_Fulltime[i]))   sociologyTable$Father_Work_Fulltime[i]   == 1
                Father_Work_Parttime1      = if(!is.na(sociologyTable$Father_Work_Parttime[i]))   sociologyTable$Father_Work_Parttime[i]   == 1
                Father_Work_Unemployed1    = if(!is.na(sociologyTable$Father_Work_Unemployed[i])) sociologyTable$Father_Work_Unemployed[i] == 1
                Father_Work_Pensioned1     = if(!is.na(sociologyTable$Father_Work_Pensioned[i]))  sociologyTable$Father_Work_Pensioned[i]  == 1
                Father_Work_Deceased1      = if(!is.na(sociologyTable$Father_Work_Deceased[i]))   sociologyTable$Father_Work_Deceased[i]   == 1
                Father_Work_DontKnown1     = if(!is.na(sociologyTable$Father_Work_DontKnown[i]))  sociologyTable$Father_Work_DontKnown[i]  == 1
                Father_Work_Other1         = if(!is.na(sociologyTable$Father_Work_Other[i]))      sociologyTable$Father_Work_Other[i]      == 1
                
                total1s                    = sum(Father_Work_Fulltime1  + Father_Work_Parttime1 + Father_Work_Unemployed1 +
                                                 Father_Work_Pensioned1 + Father_Work_Deceased1 + Father_Work_DontKnown1  +
                                                 Father_Work_Other1)                
                
                # Find out if there is at least one 1 answer
                # and if there is, find the first valid answer.
                if( total1s > 0 ){
                    
                    # AGAIN; no switch. R bad. :(
                    if(Father_Work_Fulltime1 == TRUE){
                        sociologyTable$Father_WorkTime[i] = "Full time"
                    }
                    else{
                        if(Father_Work_Parttime1 == TRUE){
                            sociologyTable$Father_WorkTime[i] = "Part time"    
                        }
                        else{
                            if(Father_Work_Unemployed1 == TRUE){
                                sociologyTable$Father_WorkTime[i] = "Unemployed"        
                            }
                            else{
                                if(Father_Work_Pensioned1 == TRUE){
                                    sociologyTable$Father_WorkTime[i] = "Pensioned"        
                                }
                                else{
                                    if(Father_Work_Deceased1 == TRUE){
                                        sociologyTable$Father_WorkTime[i] = "Deceased"        
                                    }
                                    else{
                                        if(Father_Work_DontKnown1 == TRUE){
                                            sociologyTable$Father_WorkTime[i] = "Don't know" 
                                        }
                                        else{
                                            if(Father_Work_Other1 == TRUE){
                                                sociologyTable$Father_WorkTime[i] = "Other" 
                                            }
                                            # Default is Didn't answer, so no need to go deeper    
                                        }
                                    }
                                }
                            }
                        }
                    }
                    
                    
                    
                    
                    
                    
                    
                }
                
            }
            

            
            # We are finish with the working variables, we can drop the extra info
            sociologyTable$Mother_Work_Fulltime      = NULL
            sociologyTable$Mother_Work_Parttime      = NULL
            sociologyTable$Mother_Work_Unemployed    = NULL
            sociologyTable$Mother_Work_Disabled      = NULL
            sociologyTable$Mother_Work_Domestic      = NULL
            sociologyTable$Mother_Work_School        = NULL
            sociologyTable$Mother_Work_Pensioned     = NULL
            sociologyTable$Mother_Work_Deceased      = NULL
            sociologyTable$Mother_Work_DontKnown     = NULL
            sociologyTable$Mother_Work_Other         = NULL
            
            sociologyTable$Father_Work_Fulltime      = NULL
            sociologyTable$Father_Work_Parttime      = NULL
            sociologyTable$Father_Work_Unemployed    = NULL
            sociologyTable$Father_Work_Disabled      = NULL
            sociologyTable$Father_Work_Domestic      = NULL
            sociologyTable$Father_Work_School        = NULL
            sociologyTable$Father_Work_Pensioned     = NULL
            sociologyTable$Father_Work_Deceased      = NULL
            sociologyTable$Father_Work_DontKnown     = NULL
            sociologyTable$Father_Work_Other         = NULL

            
            
            # Education for mother and father
            sociologyTable$Mother_Education    = mapvalues( sociologyTable$Mother_Education,
                                                        from = c( 1,                2,                          3,             4,                           5,                            0,            NA),
                                                        to   = c("Primary School", "Occupational High school", "High school", "College less than 4 years", "College 4 years or more ",   "Don't know", "Didn't answer"))

            sociologyTable$Father_Education    = mapvalues( sociologyTable$Father_Education,
                                                        from = c( 1,                2,                          3,             4,                           5,                            0,            NA),
                                                        to   = c("Primary School", "Occupational High school", "High school", "College less than 4 years", "College 4 years or more ",   "Don't know", "Didn't answer"))
            
                        
            # Ethnicity

            # Basically, the ethnicity question is mixed up with country of
            # origin and race. So here what I do is just put everything
            # together in a long string.
            #
            # So you can be
            #
            # "Norwegian" , "Norwegian - Sami", "Sami - Belgium", or whatever
            # combination.
            #
            # There is some data cleaning from people who are trolling, saying
            # "awesome!" and similar.
            #
            # There are also some people that awnser "Nord Norwegian", if that
            # is the case, is default to "Norwegian".
            #
            # We also correct for typos, empty field, "Write here if other" and
            # all of those cases

            for (i in 1:originalTotalRows){
            
                EthnicityNorwegian = ""    
                EthnicitySami      = ""
                EthnicityKven      = ""
                EthnicityOther     = ""
                
        
                if(!is.na(sociologyTable$EthnicityNorwegian[i])){
                    if(sociologyTable$EthnicityNorwegian[i] == 1)    EthnicityNorwegian = "Norwegian"
                }
                
                if(!is.na(sociologyTable$EthnicitySami[i])){
                    if(sociologyTable$EthnicitySami[i] == 1)      EthnicitySami      = "Sami"
                }
                
                if(!is.na(sociologyTable$EthnicityKven[i])){
                    if(sociologyTable$EthnicityKven[i] == 1)      EthnicityKven      = "Kven"
                }
                
                if(!is.na(sociologyTable$EthnicityOther[i])){
                    if(sociologyTable$EthnicityOther[i] == 1){
                        
                        EthnicComment   = tolower(trimws(sociologyTable$EthnicityOtherComment[i], which = c("both")))
                        
                        # CODE MISSING HERE, APPLY FOR THE FULL CODE
                        
                    }     
                }                
                
                
                # Now we need to put everything together, with or without "-"
                # symbol, depending on whether we have one or more ethnicities
                
                        
                totalEthnicities = 0
                if(EthnicityNorwegian != "") totalEthnicities = totalEthnicities + 1
                if(EthnicitySami      != "") totalEthnicities = totalEthnicities + 1
                if(EthnicityKven      != "") totalEthnicities = totalEthnicities + 1
                if(EthnicityOther     != "") totalEthnicities = totalEthnicities + 1
                
                
                if(totalEthnicities == 0){

                    sociologyTable$Ethnicity[i] = "Didn't Answer"
                    
                }
                else{
                
                    # Cases where we only have one ethnicity
                    # In here are included people who ansewered incorrectly and had
                    # to add the additional -, so don't worry about it
                    if(totalEthnicities == 1){
                
                        if(EthnicityNorwegian != "") sociologyTable$Ethnicity[i] = EthnicityNorwegian
                        if(EthnicitySami      != "") sociologyTable$Ethnicity[i] = EthnicitySami
                        if(EthnicityKven      != "") sociologyTable$Ethnicity[i] = EthnicityKven
                        if(EthnicityOther     != "") sociologyTable$Ethnicity[i] = EthnicityOther
                        
                    }
                    # Cases where we have more than one
                    else{
                    
                        finalEthnicity = ""
                        # We cannot paste everything toguether directly because it
                        # might be blank, and thus introducing extra -
                        if(EthnicityNorwegian != "") finalEthnicity = paste0(finalEthnicity, EthnicityNorwegian, "-")
                        if(EthnicitySami      != "") finalEthnicity = paste0(finalEthnicity, EthnicitySami,      "-")
                        if(EthnicityKven      != "") finalEthnicity = paste0(finalEthnicity, EthnicityKven,      "-")
                        if(EthnicityOther     != "") finalEthnicity = paste0(finalEthnicity, EthnicityOther,     "-")
                        # Delete the last extra -
                        finalEthnicity = str_sub(finalEthnicity,1,nchar(finalEthnicity)-1)
                        # And finally, add the info to the table!
                        sociologyTable$Ethnicity[i] = finalEthnicity

                    }
                    
                }
                

                
            }
            
            # Finally the extra info can be dropped
            sociologyTable$EthnicityNorwegian        = NULL
            sociologyTable$EthnicitySami             = NULL
            sociologyTable$EthnicityKven             = NULL
            sociologyTable$EthnicityOther            = NULL
            sociologyTable$EthnicityOtherComment     = NULL
            

            # There are some extra questions on where do you live, or have you
            # lived in the few years, born, and parents and so on which we don't
            # have.
            
            
        }
        
        # Puberty men    
        {
            
            
            pubertyMenTable$ChangeHeight = mapvalues( pubertyMenTable$ChangeHeight,
                                           from = c(  1,               2,                3,           4,           NA),
                                           to   = c( "Haven't Started", "Barely Started", "Underway",  "Completed", "Didn't Answered"))
            
            pubertyMenTable$ChangeVoice  = mapvalues( pubertyMenTable$ChangeVoice,
                                           from = c(  1,                 2,                3,                      4,           NA),
                                           to   = c( "Haven't Started", "Barely Started", "Underway",  "Completed", "Didn't Answered"))
            
            pubertyMenTable$FacialHair   = mapvalues( pubertyMenTable$FacialHair,
                                           from = c(  1,                 2,                3,                      4,           NA),
                                           to   = c( "Haven't Started", "Barely Started", "Underway",  "Completed", "Didn't Answered"))
            
            pubertyMenTable$MenBodyHair  = mapvalues( pubertyMenTable$MenBodyHair,
                                           from = c(  1,                 2,                3,                      4,           NA),
                                           to   = c( "Haven't Started", "Barely Started", "Underway",  "Completed", "Didn't Answered"))            
            
            pubertyMenTable$MenPubicHair = mapvalues( pubertyMenTable$MenPubicHair,
                                           from = c( 1,     0,    NA),
                                           to   = c("Yes", "No", "Unknown"))            
            
            # This variable is categorical that is changed to numerical censored left and right
            pubertyMenTable$MenPubicAge  = mapvalues( pubertyMenTable$MenPubicAge,
                                           from = c( 1,  2,  3,  4,  5,  6,  7, NA),
                                           to   = c( 9, 10, 11, 12, 13, 14, 15, NA))   
            
        }
        
        # Puberty women
        {
            
            pubertyWomenTable$Menarche       = mapvalues( pubertyWomenTable$Menarche,
                                               from = c( 1,     0,    NA),
                                               to   = c("Yes", "No", "Unknown"))            
            
            pubertyWomenTable$WomenPubicHair = mapvalues( pubertyWomenTable$WomenPubicHair,
                                               from = c( 1,     0,    NA),
                                               to   = c("Yes", "No", "Unknown"))            
                        
            pubertyWomenTable$Breasts        = mapvalues( pubertyWomenTable$Breasts,
                                               from = c( 1,     0,    NA),
                                               to   = c("Yes", "No", "Unknown"))            
            
        }
        
        # Menstruation
        {

            menstruationTable$MenstruationStart = mapvalues( menstruationTable$MenstruationStart,
                                                      from = c( "1",   "0",  NA),
                                                      to   = c( "Yes", "No", "Didn't Answered"))
        
            # There are 6 cases of '4' values that is are register in the metavariables information table and shouldn't be here
            # I transformed them to Unknown since is literally unknown what it means
            
            menstruationTable$MenstruationRegular = mapvalues( menstruationTable$MenstruationRegular,
                                                      from = c( "1",              "2",               "3",         "4",        NA),
                                                      to   = c( "Always regular", "Usually regular", "Irregular", "Unknown",  "Didn't Answered"))
            
        }     
        
        # Drugs
        {
        
            
            # -- Smoke
            drugsTable$Smoke  = mapvalues( drugsTable$Smoke,
                                           from = c( "1",      "2",        "3",     NA),
                                           to   = c( "Never", "Sometimes", "Daily", "Unknown"))
            
            # ---- Smoke per week/day is not register for never users, correct that
            drugsTable[drugsTable$Smoke == "Never",3] = 9
            drugsTable[drugsTable$Smoke == "Never",4] = 9
            
            drugsTable$SmokePerWeek  = mapvalues( drugsTable$SmokePerWeek,
                                           from = c( "1",      "2",      "3",      "4",       "5",            "9",     NA),
                                           to   = c( "0 to 1", "2 to 3", "4 to 6", "7 to 10", "More than 10", "Never", "Unknown"))        
            
            drugsTable$SmokePerDay  = mapvalues( drugsTable$SmokePerDay,
                                           from = c( "1",      "2",      "3",      "4",       "5",            "9",     NA),
                                           to   = c( "0 to 1", "2 to 3", "4 to 6", "7 to 10", "More than 10", "Never", "Unknown"))        
            
            # -- Snuff

            drugsTable$Snuff  = mapvalues( drugsTable$Snuff,
                                           from = c( "1",      "2",        "3",     NA),
                                           to   = c( "Never", "Sometimes", "Daily", "Unknown"))

            # ---- Snuff per week/day is not register for never users, correct that
            drugsTable[drugsTable$Snuff == "Never",6] = 9
            drugsTable[drugsTable$Snuff == "Never",7] = 9
            
            
            drugsTable$SnuffPerWeek  = mapvalues( drugsTable$SnuffPerWeek,
                                           from = c( "1",      "2",      "3",      "4",       "5",            "9",     NA),
                                           to   = c( "0 to 1", "2 to 3", "4 to 6", "7 to 10", "More than 10", "Never", "Unknown"))        
            
            drugsTable$SnuffPerDay  = mapvalues( drugsTable$SnuffPerDay,
                                           from = c( "1",      "2",      "3",      "4",       "5",            "9",     NA),
                                           to   = c( "0 to 1", "2 to 3", "4 to 6", "7 to 10", "More than 10", "Never", "Unknown"))        

            # -- Alcohol
            
            drugsTable$Alcohol  = mapvalues( drugsTable$Alcohol,
                                             from = c( "1",     "2",                      "3",                   "4",                  "5",                         NA),
                                             to   = c( "Never", "Once per month or less", "2-4 times per month", "2-3 times per week", "4 or more times per week",  "Unknown"))        

            
            # ---- Alcohol per week/day is not register for never users, correct that
            drugsTable[drugsTable$Alcohol == "Never",9]  = 9
            drugsTable[drugsTable$Alcohol == "Never",10] = 9
            
            drugsTable$AlcoholUnits  = mapvalues( drugsTable$AlcoholUnits,
                                                  from = c( "1",      "2",      "3",      "4",      "5",           "9",     NA),
                                                  to   = c( "1 to 2", "3 to 4", "5 to 6", "7 to 9", "10 or More",  "Never", "Unknown"))        
            
            drugsTable$Alcohol6Units  = mapvalues( drugsTable$Alcohol6Units,
                                                   from = c( "1",      "2",                 "3",       "4",      "5",      "9",     NA),
                                                   to   = c( "Never",  "Less than monthly", "Monthly", "Weekly", "Daily",  "Never", "Unknown"))        

        }

        # Sports
        {
        
            # Sport original description is:
            #     1 Reading, watching TV, or other sedentary activity?
            #     2 Walking, cycling, or other forms of exercise at least 4 hours a week? (including walking or cycling to place of school, shopping, Sunday-walking, etc.)
            #     3 Participation in recreational sports, heavy outdoor activities, snow clearing etc? (note: duration of activity at least 4 hours a week).
            #     4 Participation in hard training or sports competitions, regularly several times a week?
            #
            # I transform this into 1 = None, 2 = Light, 3 = Medium, 4 = Hard
            #
            # ( Notice that I personally don't consider snow cleaning to be
            #   Medium, that should be merged with Light. Hard should be the
            #   medium and professional should be hard; but whatever )
            
            # R is horrible self.mapvalues() doesn't exist, and you can't even
            # make it yourself T-T. And it doesn't tell you of the most basic
            # variables errors, I had "sportsTable$sportsTable$SportHours" which
            # doesn't make any sense, and can't even warning you about that.
            # And what it worse, it execute the code with no problem!!!!
            
            sportsTable$SportsLeisure  = mapvalues( sportsTable$SportsLeisure,
                                                    from = c( "1",      "2",        "3",     "4",    NA),
                                                    to   = c( "None",   "Light",    "Medium","Hard", "Unknown"))

            sportsTable$SportsOutsideSchool  = mapvalues( sportsTable$SportsOutsideSchool,
                                                          from = c( "1",    "0",  NA),
                                                          to   = c( "Yes",  "No", "Unknown"))
            
            sportsTable$SportsFrequency     = mapvalues( sportsTable$SportsFrequency,
                                                         from = c( "1",     "2",               "3",          "4",            "5",            "6",                 NA),
                                                         to   = c( "Never", "Less than daily", "1 per week", "2-3 per week", "4-6 per week", "Almost every day", "Unknown"))

            sportsTable$SportHours          = mapvalues( sportsTable$SportHours,
                                                         from = c( "1",     "2",                 "3",              "4",            "5",            "6",               NA),
                                                         to   = c( "None", "About half an hour", "1 to 1.5 hours", "2 to 3 hours", "4 to 6 hours", "7 hours or more", "Unknown"))            

            sportsTable$SportsIntensity     = mapvalues( sportsTable$SportsIntensity,
                                                         from = c( "1",               "2",          "3",          "4",         "5",              NA),
                                                         to   = c( "Not hard at all", "A bit hard", "Quite hard", "Very hard", "Extremely hard", "Unknown"))                        
        
            sportsTable$SummerTransport     = mapvalues( sportsTable$SummerTransport,
                                                         from = c( "1",               "2",      "3",       "4",       NA),
                                                         to   = c( "By car or moped", "By bus", "By bike", "On foot", "Unknown"))
            
            sportsTable$SummerTime          = mapvalues( sportsTable$SummerTime,
                                                         from = c( "1",          "2",            "3",             "4",     "5",           NA),
                                                         to   = c( "<5 minutes", "6-15 minutes", "16-30 minutes", "31-60", ">60 minutes", "Unknown"))                                

            sportsTable$WinterTransport     = mapvalues( sportsTable$WinterTransport,
                                                         from = c( "1",               "2",      "3",       "4",       NA),
                                                         to   = c( "By car or moped", "By bus", "By bike", "On foot", "Unknown"))
            
            sportsTable$WinterTime          = mapvalues( sportsTable$WinterTime,
                                                         from = c( "1",          "2",            "3",             "4",     "5",           NA),
                                                         to   = c( "<5 minutes", "6-15 minutes", "16-30 minutes", "31-60", ">60 minutes", "Unknown"))                                            

            sportsTable$ScreenTime          = mapvalues( sportsTable$ScreenTime,
                                                         from = c( "1",    "2",                    "3",                   "4", 
                                                                   "5",                  "6",                  "7",                 NA),
                                                         to   = c( "None",  "About half an hour",  "About 1 to 1,5 hours", "About 2 to 3 hours",
                                                                   "About 4 to 6 hours", "About 7 to 9 hours", "10 hours or more", "Unknown"))                                            

            
        }
  
        # Hygiene
        {

            
                        
            hygieneTable$ShowerBathFrequency  = mapvalues( hygieneTable$ShowerBathFrequency,
                                                from = c(  1,                       2,             3,                  4,                 5,            6,               7,                       NA),
                                                to   = c( "Less than once a week", "Once a week", "2-3 times a week", "4-6 times a week","Once a day", "2 times a day", "3 or more times a day", "Didn't Answered"))
                     
                        
            hygieneTable$HandwashFrequency    = mapvalues(  hygieneTable$HandwashFrequency,
                                                from = c(  1,         2,           3,           4,             5,             6,                    NA),
                                                to   = c( "0 times", "1-2 times", "3-5 times", "6-10 times",  "11-20 times", "More than 20 times", "Didn't Answered"))
            
            hygieneTable$BodyLotionFrequency  = mapvalues(  hygieneTable$BodyLotionFrequency,
                                                from = c(  1,                       2,             3,                  4,                   5,            6,                       NA),
                                                to   = c( "Less than once a week", "Once a week", "2-3 times a week", "4-6 times a week",  "Once a day", "2 or more times a day", "Didn't Answered"))

                        
            hygieneTable$SkinSunbathing       = mapvalues( hygieneTable$SkinSunbathing,
                                                from = c(  1,                         2,                                    3,                                   4 ,                        NA),
                                                to   = c( "Always red, never brown", "Almost always red, sometimes brown", "Almost always brown, sometimes red","Always brown, never red", "Didn't Answered"))

            hygieneTable$HolidaySunbathing    = mapvalues( hygieneTable$HolidaySunbathing,
                                                from = c(  1,      0,    NA),
                                                to   = c( "Yes",  "No", "Didn't Answered"))
            
            hygieneTable$SolariumLast4Weeks   = mapvalues( hygieneTable$SolariumLast4Weeks,
                                                from = c(  1,      0,    NA),
                                                to   = c( "Yes",  "No", "Didn't Answered"))
        
               
        }
        
        # Hospitalization (nothing here)
        
        # Biomarkers (flag only)
        {
        
            biomarkersTable$Flagged  = mapvalues( biomarkersTable$Flagged,
                                                  from = c( "1",    "0",  NA),
                                                  to   = c( "Yes",  "No", "Unknown"))
            
        }
        
        # Diet ( this is a nightmare T_T )
        {
        
            # Notice how the numbers and frequencies are not consistent in between
            # different concepts. Also, different concepts are not mutually exclusive
            # such as 1-3 week, could also be true for 1-3 month.
            
            # Time of the day
            dietTable$BreakfastFrequency     = mapvalues( dietTable$BreakfastFrequency,
                                                         from = c( "1",            "2",                  "3",                  "4",          NA),
                                                         to   = c( "Rarely/Never", "1-3 times per week", "4-6 times per week", "Every day",  "Didn't Answered"))
            
    
        
            # Specific foods (sorted by max frequency)
            
            # ---- Fruits 5 per day
            dietTable$FruitsFrequency     = mapvalues( dietTable$FruitsFrequency,
                                                         from = c( "1",            "2",                   "3",                  "4",                  "5",                 "6",                 "7",                       NA),
                                                         to   = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "1-2 times per day", "3-4 times per day", "5 times or more per day", "Didn't Answered"))

            
            dietTable$VegetablesFrequency     = mapvalues( dietTable$VegetablesFrequency,
                                                         from = c( "1",            "2",                   "3",                  "4",                  "5",                 "6",                 "7",                       NA),
                                                         to   = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "1-2 times per day", "3-4 times per day", "5 times or more per day", "Didn't Answered"))            

            # ---- Drinks 4 glasses per day
            dietTable$DairyFrequency     = mapvalues( dietTable$DairyFrequency,
                                                         from = c( "1",            "2",                    "3",               "4",                   "5",                         NA),
                                                         to   = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))

            dietTable$FruitJuiceFrequency     = mapvalues( dietTable$FruitJuiceFrequency,
                                                         from = c( "1",            "2",                    "3",               "4",                   "5",                         NA),
                                                         to   = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))
            
            dietTable$SugarJuiceFrequency     = mapvalues( dietTable$SugarJuiceFrequency,
                                                         from = c( "1",            "2",                    "3",               "4",                   "5",                         NA),
                                                         to   = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))            
            
            dietTable$SugarDrinkFrequency     = mapvalues( dietTable$SugarDrinkFrequency,
                                                         from = c( "1",            "2",                    "3",               "4",                   "5",                         NA),
                                                         to   = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))            
            
            dietTable$SweetenerDrinkFrequency     = mapvalues( dietTable$SweetenerDrinkFrequency,
                                                         from = c( "1",            "2",                    "3",               "4",                   "5",                         NA),
                                                         to   = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))            
            
            dietTable$WaterFrequency     = mapvalues( dietTable$WaterFrequency,
                                                         from = c( "1",            "2",                    "3",               "4",                   "5",                         NA),
                                                         to   = c( "Rarely/Never", "1-6 glasses per week", "1 glass per day", "2-3 glasses per day", "4 or more glasses per day", "Didn't Answered"))                        
            
            # ---- General Food Daily
            dietTable$CheeseFrequency     = mapvalues( dietTable$CheeseFrequency,
                                                         from = c( "1",            "2",                   "3",                  "4",                  "5",         NA),
                                                         to   = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Didn't Answered"))
                        
            dietTable$FatFishFrequency     = mapvalues( dietTable$FatFishFrequency,
                                                         from = c( "1",            "2",                   "3",                  "4",                  "5",         NA),
                                                         to   = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Didn't Answered"))                
            
            dietTable$LeanFishFrequency     = mapvalues( dietTable$LeanFishFrequency,
                                                         from = c( "1",            "2",                   "3",                  "4",                  "5",         NA),
                                                         to   = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Didn't Answered"))

            dietTable$ChocolateFrequency     = mapvalues( dietTable$ChocolateFrequency,
                                                         from = c( "1",            "2",                   "3",                  "4",                  "5",         NA),
                                                         to   = c( "Rarely/Never", "1-3 times per month", "1-3 times per week", "4-6 times per week", "Every day", "Didn't Answered"))
                        
            # ---- Weird food (why is reindeer considered weird?, I eat it more than once per month here in Norway)
            dietTable$SeagullEggsFrequency     = mapvalues( dietTable$SeagullEggsFrequency,
                                                         from = c( "1",            "2",                  "3",                  "4",                  "5",                         NA),
                                                         to   = c( "Rarely/Never", "1-3 times per year", "4-5 times per year", "6-9 times per year", "10 or more times per year", "Didn't Answered"))            
            
            dietTable$ReindeerFrequency     = mapvalues( dietTable$ReindeerFrequency,
                                                         from = c( "1",            "2",                  "3",                  "4",                  "5",                         NA),
                                                         to   = c( "Rarely/Never", "1-3 times per year", "4-5 times per year", "6-9 times per year", "10 or more times per year", "Didn't Answered"))            

            # ---- PILLS HERE!
            
            dietTable$FishOilFrequency  = mapvalues( dietTable$FishOilFrequency,
                                                    from = c( "1",    "2",          "3",          NA),
                                                    to   = c( "No",   "Sometimes",  "Yes, daily", "Didn't Answered"))
            
            dietTable$VitaminsFrequency  = mapvalues( dietTable$VitaminsFrequency,
                                                    from = c( "1",    "2",          "3",          NA),
                                                    to   = c( "No",   "Sometimes",  "Yes, daily", "Didn't Answered"))            

        }
        
        # Sleeping
        {
        
            sleepTable$SleepingPills   = mapvalues( sleepTable$SleepingPills,
                                                    from = c( "1",          "2",                                  "3",                         "4",     NA),
                                                    to   = c( "Not used",   "Less frequently than every week",    "Every week, but not daily", "Daily", "Didn't Answered"))            
            
            sleepTable$BedTimeHourCat  = mapvalues( sleepTable$BedTimeHourCat,
                                                    from = c( "1",  "2",  "3",  "4",  "5",  "6",
                                                              "7",  "8",  "9",  "10", "11", "12",
                                                              "13", "14", "15", "16", "17",  NA),
                
                                                    to   = c( "18:00 or earlier", "18:30", "19:00", "19:30",
                                                              "20:00", "20:30", "21:00", "21:30", "22:00",
                                                              "22:30", "23:00", "23:30", "00:00", "00:30",
                                                              "01:00", "01:30", "02:00 or later", "Didn't Answered"))

        }
        
        # Diseases (We don't need to change anything, as we are not going to use
        #           anything from this table that isn't health or total diseases.
        #           Everything else is used via the DB table) 
        
        # Friendship
        {
            
            frienshipTable$YesterdaySMS = mapvalues( frienshipTable$YesterdaySMS,
                                          from = c(  1,      2,        3,         4,          5,          6,     NA),
                                          to   = c( "None", "1 to 5", "6 to 10", "11 to 20", "21 to 50", ">50", "Didn't Answered"))

        }
        
        # @@Transform Ends Here@@

    }
    
    # ----Convert numbers that are strings into proper number variables
    #     (nothing to do here)
    
    # Replace the IDs for indexes
    #     it makes looking and writing something much more easy in general
    #      And you need to do this in all the general tables
    {
    
        # Keep track of all key changes
        replacementKeysDF           = data.frame(matrix(NA, nrow = originalTotalRows, ncol = 2))
        colnames(replacementKeysDF) = c("Old_Key", "New_Key")
    
        # Prepare the log for writing the keys replacements
        logLine = paste("The original keys were replaced like this: ")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
        logLine = paste("    ----    ")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
    
        totalIDs   = length(basicTable$ID)
        
        # For each ID, replace the ID for a consecutive number
        for(i in 1:totalIDs){

            # Save the current key
            replacementKey = basicTable$ID[i]
    
            # Set the new key in every table
            #
            #    (take into account that different tables might be sorted differently)
            #    (default sorting is the phenotype table, whatever that is)
            #
            # -- Basic Table get the "i" index as default
            basicTable$ID[i] = i
            
            # -- Everything else change based on this i value
            networkTechnicalTable$ID   = replace(networkTechnicalTable$ID,   networkTechnicalTable$ID   == replacementKey,   i)
            antropometricFF1Table$ID   = replace(antropometricFF1Table$ID,   antropometricFF1Table$ID   == replacementKey,   i)
            antropometricFF2Table$ID   = replace(antropometricFF2Table$ID,   antropometricFF2Table$ID   == replacementKey,   i)
            menstruationTable$ID       = replace(menstruationTable$ID,       menstruationTable$ID       == replacementKey,   i)
            aureusTable$ID             = replace(aureusTable$ID,             aureusTable$ID             == replacementKey,   i)
            swabbingTable$ID           = replace(swabbingTable$ID,           swabbingTable$ID           == replacementKey,   i)
            highSchoolTable$ID         = replace(highSchoolTable$ID,         highSchoolTable$ID         == replacementKey,   i)
            bloodTable$ID              = replace(bloodTable$ID,              bloodTable$ID              == replacementKey,   i)
            bloodTechnicalTable$ID     = replace(bloodTechnicalTable$ID,     bloodTechnicalTable$ID     == replacementKey,   i)
            #questionariesTable$ID     = replace(questionariesTable$ID,      questionariesTable$ID      == replacementKey,   i) # This is empty
            sociologyTable$ID          = replace(sociologyTable$ID,          sociologyTable$ID          == replacementKey,   i)
            pubertyMenTable$ID         = replace(pubertyMenTable$ID,         pubertyMenTable$ID         == replacementKey,   i)
            pubertyWomenTable$ID       = replace(pubertyWomenTable$ID,       pubertyWomenTable$ID       == replacementKey,   i)
            drugsTable$ID              = replace(drugsTable$ID,              drugsTable$ID              == replacementKey,   i)
            sportsTable$ID             = replace(sportsTable$ID,             sportsTable$ID             == replacementKey,   i)
            hygieneTable$ID            = replace(hygieneTable$ID,            hygieneTable$ID            == replacementKey,   i)
            #hospitalizationTable$ID = replace(hospitalizationTable$ID, hospitalizationTable$ID == replacementKey,   i) # Also empty
            biomarkersTable$ID         = replace(biomarkersTable$ID,         biomarkersTable$ID         == replacementKey,   i)
            dietTable$ID               = replace(dietTable$ID,               dietTable$ID               == replacementKey,   i)
            sleepTable$ID              = replace(sleepTable$ID,              sleepTable$ID              == replacementKey,   i)
            frienshipTable$ID          = replace(frienshipTable$ID,          frienshipTable$ID          == replacementKey,   i) 
      
            # -- Network table
            #    This is a special case since we need to change every reference
            #    R is horrible, this would be trivial with pointers, in fact you
            #    wouldn't need to do anything at all. I hate this.
            # ---- IDs
            networkTable$ID = replace(networkTable$ID, networkTable$ID == replacementKey, i)
            # ---- Each one of the friends
            networkTable$Friend1     = replace(networkTable$Friend1,     networkTable$Friend1     == replacementKey, i)
            networkTable$Friend2     = replace(networkTable$Friend2,     networkTable$Friend2     == replacementKey, i)
            networkTable$Friend3     = replace(networkTable$Friend3,     networkTable$Friend3     == replacementKey, i)
            networkTable$Friend4     = replace(networkTable$Friend4,     networkTable$Friend4     == replacementKey, i)
            networkTable$Friend5     = replace(networkTable$Friend5,     networkTable$Friend5     == replacementKey, i)
            
            networkTable$Friend1FF12 = replace(networkTable$Friend1FF12, networkTable$Friend1FF12 == replacementKey, i)
            networkTable$Friend2FF12 = replace(networkTable$Friend2FF12, networkTable$Friend2FF12 == replacementKey, i)
            networkTable$Friend3FF12 = replace(networkTable$Friend3FF12, networkTable$Friend3FF12 == replacementKey, i)
            networkTable$Friend4FF12 = replace(networkTable$Friend4FF12, networkTable$Friend4FF12 == replacementKey, i)
            networkTable$Friend5FF12 = replace(networkTable$Friend5FF12, networkTable$Friend5FF12 == replacementKey, i)
            
            
            # Each of the relational tables
            # ---- The disease table
            diseasesDBDF$ID        = replace(diseasesDBDF$ID,       diseasesDBDF$ID       == replacementKey, i)
            # ---- The medicine table
            medicinesDBDF$ID       = replace(medicinesDBDF$ID,      medicinesDBDF$ID      == replacementKey, i)
            # ---- The contraceptive table
            contraceptivesDBDF$ID  = replace(contraceptivesDBDF$ID, contraceptivesDBDF$ID == replacementKey, i)
            
            # Write that in the log
            # ---- txt
            logLine = paste(replacementKey, " -> ", i)
            write( logLine ,
                   file   = logTXTFileConnection,
                   append = TRUE )
                    
            # ---- dataframe
            replacementKeysDF$Old_Key[i] = replacementKey
            replacementKeysDF$New_Key[i] = i
    
        }
    
        # Empty line for the txt log
        logLine = paste("    ----    ")
        write( logLine , file   = logTXTFileConnection, append = TRUE )
    
        # Log the IDs that hasn't change yet.
        #
        # Up to here, the basic table ID column has all the IDs changed from
        # 1 to 1000ish. The only things is left unchanged are friends that has
        # a nomination, but are not in the list of IDs
        {
            validKeys = unique(basicTable$ID)
            selectedFriendsKeys = unique(c(networkTable$Friend1,
                                           networkTable$Friend2,
                                           networkTable$Friend3,
                                           networkTable$Friend4,
                                           networkTable$Friend5,
                                           networkTable$Friend1FF12,
                                           networkTable$Friend2FF12,
                                           networkTable$Friend3FF12,
                                           networkTable$Friend4FF12,
                                           networkTable$Friend5FF12))
            selectedFriendsKeys  = setdiff(selectedFriendsKeys, NA) # Remove the NA key if any
            missingKeys          = setdiff(selectedFriendsKeys, validKeys)
            totalMissing         = length(missingKeys)
            totalRemaining       = length(validKeys)
    
            # These are how many unique keys were in the study
            totalOriginalIDs     = totalMissing + totalRemaining
            
            # Log the results in the data frame
            {
              dataCleaningLogDF$Total[1]    = totalOriginalIDs
              dataCleaningLogDF$Relative[1] = "100 %"
              dataCleaningLogDF$Total[2]    = totalMissing
              dataCleaningLogDF$Relative[2] = paste(round(totalMissing/totalOriginalIDs,4)   * 100, " %", sep="")
              dataCleaningLogDF$Total[3]    = totalRemaining
              dataCleaningLogDF$Relative[3] = paste(round(totalRemaining/totalOriginalIDs,4) * 100, " %", sep="")          
            }
         }
          
        # Keep track of all edges that are lost
        {
        
            # First, we initiatlize the DFs where we are going to safe the info.
            
            # In validKeys we have the keys that remain
            # In missingKeys we have the keys that are going to be throw away
            #
            # In here we keep a list of the keys that remain, and for each key
            # a list of friends that are lost.
            #
            # People who remain, nominate people that are lost, always.
            # (A)
            #
            #
            #
            # COMMENT MISSING; APPLY FOR THE FULL CODE
            #
            #
            # ...
            remainingToDeletedFriendsLostDF           = DF(totalRemaining, 3)
            colnames(remainingToDeletedFriendsLostDF) = c("Key", "Total", "List")
            remainingToDeletedFriendsLostDF$Key       = validKeys
            remainingToDeletedFriendsLostDF$Total     = 0
            for(i in 1:totalRemaining){
                remainingToDeletedFriendsLostDF$List[i] = vector(mode = "list", length = 1)
            }
            
            # In here we keep a list of the keys that are lost, and for each
            # key, a list of friends that remain
            #
            # People who are lost, are nominated by people that remain, always
            # (B)
            #
            #
            # COMMENT MISSING; APPLY FOR THE FULL CODE
            #
            #
            # ...
            deletedByRemainingFriendsLostDF           = DF(totalMissing, 3)
            colnames(deletedByRemainingFriendsLostDF) = c("Key", "Total", "List")
            deletedByRemainingFriendsLostDF$Key       = missingKeys
            deletedByRemainingFriendsLostDF$Total     = 0
            for(i in 1:totalMissing){
                deletedByRemainingFriendsLostDF$List[i] = vector(mode = "list", length = 1)
            }
            
            # For each missing key, record who is nominating it and update the Totals
            print("Keeping track of lost connections, please wait...")
            for (i in 1:totalMissing) {
              
                  # Find, who has nominated this person, it doesn't matter if you
                  # nominate him as friend 1 or friend 5.
                  {
                      F1 = networkTable$Friend1 == missingKeys[i] & (!is.na(networkTable$Friend1))
                      F2 = networkTable$Friend2 == missingKeys[i] & (!is.na(networkTable$Friend2))
                      F3 = networkTable$Friend3 == missingKeys[i] & (!is.na(networkTable$Friend3))
                      F4 = networkTable$Friend4 == missingKeys[i] & (!is.na(networkTable$Friend4))
                      F5 = networkTable$Friend5 == missingKeys[i] & (!is.na(networkTable$Friend5))
                
                      F1FF12 = networkTable$Friend1FF12 == missingKeys[i] & (!is.na(networkTable$Friend1FF12))
                      F2FF12 = networkTable$Friend2FF12 == missingKeys[i] & (!is.na(networkTable$Friend2FF12))
                      F3FF12 = networkTable$Friend3FF12 == missingKeys[i] & (!is.na(networkTable$Friend3FF12))
                      F4FF12 = networkTable$Friend4FF12 == missingKeys[i] & (!is.na(networkTable$Friend4FF12))
                      F5FF12 = networkTable$Friend5FF12 == missingKeys[i] & (!is.na(networkTable$Friend5FF12))                      
                      
                      anyFriend = (F1     | F2     | F3     | F4     | F5     |
                                   F1FF12 | F2FF12 | F3FF12 | F4FF12 | F5FF12 )
                      nominateAnUnknown = networkTable$ID[ anyFriend ] # Here you have the list of people who has nominated an this particular unknown ID
                      totalNominated    = length(nominateAnUnknown)
                  }
              
                  # Go though that list of people that are nominating this ID that is
                  # going to be deleted, and add the missing friend to their respective
                  # list.
                  for (j in 1:totalNominated) {
                
                      nominatingKey = nominateAnUnknown[j]
                
                      remainingToDeletedFriendsLostDF$List[[nominatingKey]] = c(remainingToDeletedFriendsLostDF$List[[nominatingKey]], missingKeys[i])
                
                  }
              
                  # Go to that particular key, and add the people that are nominating
                  # this person that is going to be deleted later.
                  #
                  # This dataframe is not sorted by index
                  # so you need to find the proper row first
                  myIndex = grep(TRUE, (deletedByRemainingFriendsLostDF$Key == missingKeys[i]))
                  for (j in 1:totalNominated) {
                
                      nominatingKey = nominateAnUnknown[j]
                
                      deletedByRemainingFriendsLostDF$List[[myIndex]] = c(deletedByRemainingFriendsLostDF$List[[myIndex]],nominatingKey)  
                
                  }
              
            }
            
            # ---- Up to here, we have all the lists we need, just count the length
            #      and update the totals of each DF
            #
            # I can't do:
            #
            # remainingToDeletedFriendsLostDF$Total = length(remainingToDeletedFriendsLostDF$List)
            # deletedByRemainingFriendsLostDF$Total = length(deletedByRemainingFriendsLostDF$List)
            #
            # Really R, I hate you and your lack of consistency in vector operation, among other many things ¬¬
            for (i in 1:totalRemaining) remainingToDeletedFriendsLostDF$Total[i] = length(remainingToDeletedFriendsLostDF$List[[i]])
            for (i in 1:totalMissing)   deletedByRemainingFriendsLostDF$Total[i] = length(deletedByRemainingFriendsLostDF$List[[i]])
            
            # ---- Finally, update the log DF with the total lost edges
            #         How many we have at starting

            percentLost  = round((sum(remainingToDeletedFriendsLostDF$Total) / absoluteTotalEdges)*100,2)
            percentLost  = paste0(percentLost," %")
            percentStill = round(( (absoluteTotalEdges - sum(remainingToDeletedFriendsLostDF$Total))  / absoluteTotalEdges)*100,2)
            percentStill = paste0(percentStill," %")
            
                                    
            dataCleaningLogDF$Total [4]   = absoluteTotalEdges
            dataCleaningLogDF$Relative[4] = "100 %"
            #         How many we have deleted
            dataCleaningLogDF$Total[5]    = sum(remainingToDeletedFriendsLostDF$Total)
            dataCleaningLogDF$Relative[5] = percentLost
            #         How many we have now
            dataCleaningLogDF$Total [6]   = absoluteTotalEdges - sum(remainingToDeletedFriendsLostDF$Total)
            dataCleaningLogDF$Relative[6] = percentStill
            
            
            # ---- And update the TXT log also
            {
              
                  # From remaining to deleted:
    
                  logLine = "People remaining that are missing nomination:"
                  write( logLine , file = logTXTFileConnection, append = TRUE )
                  logLine = paste("    ----    ")
                  write( logLine , file   = logTXTFileConnection, append = TRUE )
              
                  for (i in 1:totalRemaining){
                
                      currentKey   = remainingToDeletedFriendsLostDF$Key[i]
                      currentTotal = remainingToDeletedFriendsLostDF$Total[i]
                      currentList  = remainingToDeletedFriendsLostDF$List[[i]]
                
                      if(currentTotal > 0){
                          for (j in 1:currentTotal){
    
                              logLine = paste(currentKey, " -> ", currentList[j])
                              write( logLine , file   = logTXTFileConnection, append = TRUE )
                                    
                          }              
                      }
                   }
              
                   # To deleted By remaining:
              
                   logLine = paste("    ----    ")
                   write( logLine , file   = logTXTFileConnection, append = TRUE )          
                   logLine = "People deleted that were nominated by someone remaining:"
                   write( logLine , file = logTXTFileConnection, append = TRUE )
                   logLine = paste("    ----    ")
                   write( logLine , file   = logTXTFileConnection, append = TRUE )
              
                   for (i in 1:totalMissing){
                
                       currentKey   = deletedByRemainingFriendsLostDF$Key[i]
                       currentTotal = deletedByRemainingFriendsLostDF$Total[i]
                       currentList  = deletedByRemainingFriendsLostDF$List[[i]]
                
                       if(currentTotal > 0){
                           for (j in 1:currentTotal){
                    
                               logLine = paste(currentKey, " -> ", currentList[j])
                               write( logLine , file   = logTXTFileConnection, append = TRUE )
                    
                           }              
                       }
                    }
              
              }
            
            
        }
          
        # There are many NAs everywhere for the people who have less than 5 friends. We are going
        # to set those to ID = -1, meaning nobody.
        {
              networkTable$Friend1[is.na(networkTable$Friend1)] = -1
              networkTable$Friend2[is.na(networkTable$Friend2)] = -1
              networkTable$Friend3[is.na(networkTable$Friend3)] = -1
              networkTable$Friend4[is.na(networkTable$Friend4)] = -1
              networkTable$Friend5[is.na(networkTable$Friend5)] = -1

              networkTable$Friend1FF12[is.na(networkTable$Friend1FF12)] = -1
              networkTable$Friend2FF12[is.na(networkTable$Friend2FF12)] = -1
              networkTable$Friend3FF12[is.na(networkTable$Friend3FF12)] = -1
              networkTable$Friend4FF12[is.na(networkTable$Friend4FF12)] = -1
              networkTable$Friend5FF12[is.na(networkTable$Friend5FF12)] = -1
        }
    
        # Finally, there might be still keys present that don't match with anyone in the ID list
        # We set a special ID = 0, which mean an anonymous person that is not in our data
        {
              networkTable$Friend1[networkTable$Friend1 > totalIDs] = 0
              networkTable$Friend2[networkTable$Friend2 > totalIDs] = 0
              networkTable$Friend3[networkTable$Friend3 > totalIDs] = 0
              networkTable$Friend4[networkTable$Friend4 > totalIDs] = 0
              networkTable$Friend5[networkTable$Friend5 > totalIDs] = 0
              
              networkTable$Friend1FF12[networkTable$Friend1FF12 > totalIDs] = 0
              networkTable$Friend2FF12[networkTable$Friend2FF12 > totalIDs] = 0
              networkTable$Friend3FF12[networkTable$Friend3FF12 > totalIDs] = 0
              networkTable$Friend4FF12[networkTable$Friend4FF12 > totalIDs] = 0
              networkTable$Friend5FF12[networkTable$Friend5FF12 > totalIDs] = 0              
        }
        
    }
        
    # NA/Null Cleaning
    {
    
        # ---- Replace the NAs in the network table for -1s , meaning that there is no friend in here
        #      Later use this for data validation in the control script
        {
    
            # FF1
            
            networkTable$Friend1Physical[is.na(networkTable$Friend1Physical)] = -1
            networkTable$Friend2Physical[is.na(networkTable$Friend2Physical)] = -1
            networkTable$Friend3Physical[is.na(networkTable$Friend3Physical)] = -1
            networkTable$Friend4Physical[is.na(networkTable$Friend4Physical)] = -1
            networkTable$Friend5Physical[is.na(networkTable$Friend5Physical)] = -1
    
            networkTable$Friend1Home[is.na(networkTable$Friend1Home)] = -1
            networkTable$Friend2Home[is.na(networkTable$Friend2Home)] = -1
            networkTable$Friend3Home[is.na(networkTable$Friend3Home)] = -1
            networkTable$Friend4Home[is.na(networkTable$Friend4Home)] = -1
            networkTable$Friend5Home[is.na(networkTable$Friend5Home)] = -1
    
            networkTable$Friend1School[is.na(networkTable$Friend1School)] = -1
            networkTable$Friend2School[is.na(networkTable$Friend2School)] = -1
            networkTable$Friend3School[is.na(networkTable$Friend3School)] = -1
            networkTable$Friend4School[is.na(networkTable$Friend4School)] = -1
            networkTable$Friend5School[is.na(networkTable$Friend5School)] = -1
    
            networkTable$Friend1Sport[is.na(networkTable$Friend1Sport)] = -1
            networkTable$Friend2Sport[is.na(networkTable$Friend2Sport)] = -1
            networkTable$Friend3Sport[is.na(networkTable$Friend3Sport)] = -1
            networkTable$Friend4Sport[is.na(networkTable$Friend4Sport)] = -1
            networkTable$Friend5Sport[is.na(networkTable$Friend5Sport)] = -1
    
            networkTable$Friend1Other[is.na(networkTable$Friend1Other)] = -1
            networkTable$Friend2Other[is.na(networkTable$Friend2Other)] = -1
            networkTable$Friend3Other[is.na(networkTable$Friend3Other)] = -1
            networkTable$Friend4Other[is.na(networkTable$Friend4Other)] = -1
            networkTable$Friend5Other[is.na(networkTable$Friend5Other)] = -1
    
            # FF12
            
            networkTable$Friend1PhysicalFF12[is.na(networkTable$Friend1PhysicalFF12)] = -1
            networkTable$Friend2PhysicalFF12[is.na(networkTable$Friend2PhysicalFF12)] = -1
            networkTable$Friend3PhysicalFF12[is.na(networkTable$Friend3PhysicalFF12)] = -1
            networkTable$Friend4PhysicalFF12[is.na(networkTable$Friend4PhysicalFF12)] = -1
            networkTable$Friend5PhysicalFF12[is.na(networkTable$Friend5PhysicalFF12)] = -1
    
            networkTable$Friend1HomeFF12[is.na(networkTable$Friend1HomeFF12)] = -1
            networkTable$Friend2HomeFF12[is.na(networkTable$Friend2HomeFF12)] = -1
            networkTable$Friend3HomeFF12[is.na(networkTable$Friend3HomeFF12)] = -1
            networkTable$Friend4HomeFF12[is.na(networkTable$Friend4HomeFF12)] = -1
            networkTable$Friend5HomeFF12[is.na(networkTable$Friend5HomeFF12)] = -1
    
            networkTable$Friend1SchoolFF12[is.na(networkTable$Friend1SchoolFF12)] = -1
            networkTable$Friend2SchoolFF12[is.na(networkTable$Friend2SchoolFF12)] = -1
            networkTable$Friend3SchoolFF12[is.na(networkTable$Friend3SchoolFF12)] = -1
            networkTable$Friend4SchoolFF12[is.na(networkTable$Friend4SchoolFF12)] = -1
            networkTable$Friend5SchoolFF12[is.na(networkTable$Friend5SchoolFF12)] = -1
    
            networkTable$Friend1SportFF12[is.na(networkTable$Friend1SportFF12)] = -1
            networkTable$Friend2SportFF12[is.na(networkTable$Friend2SportFF12)] = -1
            networkTable$Friend3SportFF12[is.na(networkTable$Friend3SportFF12)] = -1
            networkTable$Friend4SportFF12[is.na(networkTable$Friend4SportFF12)] = -1
            networkTable$Friend5SportFF12[is.na(networkTable$Friend5SportFF12)] = -1
    
            networkTable$Friend1OtherFF12[is.na(networkTable$Friend1OtherFF12)] = -1
            networkTable$Friend2OtherFF12[is.na(networkTable$Friend2OtherFF12)] = -1
            networkTable$Friend3OtherFF12[is.na(networkTable$Friend3OtherFF12)] = -1
            networkTable$Friend4OtherFF12[is.na(networkTable$Friend4OtherFF12)] = -1
            networkTable$Friend5OtherFF12[is.na(networkTable$Friend5OtherFF12)] = -1            
            
        }
    
    }    
    
}


# -----------------------------------------------------------------------------
# Add new data
#
#     Adding new columns based on previous info
#     In here the original data is ready and clean,
#     so *now* you can add whathever you want.
# -----------------------------------------------------------------------------
{

    # Basic (nothing here)
    
    # Network technical details (nothing here)
    
    # Network (nothing here)
    
    # Antropometric
    {
        # -- BMI value to BMI categorical
        {
          antropometricFF1Table$BMICategorical = "Unknown"
          antropometricFF1Table[(0    <   antropometricFF1Table$BMI) & (antropometricFF1Table$BMI < 18.5) & !is.na(antropometricFF1Table$BMI), ]$BMICategorical = "Underweight"
          antropometricFF1Table[(18.5 <=  antropometricFF1Table$BMI) & (antropometricFF1Table$BMI < 25)   & !is.na(antropometricFF1Table$BMI), ]$BMICategorical = "Healthy"
          antropometricFF1Table[(25   <=  antropometricFF1Table$BMI) & (antropometricFF1Table$BMI < 30)   & !is.na(antropometricFF1Table$BMI), ]$BMICategorical = "Overweight"
          antropometricFF1Table[(30   <=  antropometricFF1Table$BMI) & (antropometricFF1Table$BMI < Inf)  & !is.na(antropometricFF1Table$BMI), ]$BMICategorical = "Obese"
          
          antropometricFF2Table$BMICategorical = "Unknown"
          antropometricFF2Table[(0    <   antropometricFF2Table$BMI) & (antropometricFF2Table$BMI < 18.5) & !is.na(antropometricFF2Table$BMI), ]$BMICategorical = "Underweight"
          antropometricFF2Table[(18.5 <=  antropometricFF2Table$BMI) & (antropometricFF2Table$BMI < 25)   & !is.na(antropometricFF2Table$BMI), ]$BMICategorical = "Healthy"
          antropometricFF2Table[(25   <=  antropometricFF2Table$BMI) & (antropometricFF2Table$BMI < 30)   & !is.na(antropometricFF2Table$BMI), ]$BMICategorical = "Overweight"
          antropometricFF2Table[(30   <=  antropometricFF2Table$BMI) & (antropometricFF2Table$BMI < Inf)  & !is.na(antropometricFF2Table$BMI), ]$BMICategorical = "Obese"          
          
        }        
    }

    # S.Aureus
    {

        # You can be COLONIZE or CARRIER
        #
        # Colonize: means that only one swab was positive
        # Carrier:  means that both swabs were positive
        #
        # Sometimes, people call these synonyms:
        # 
        # Carrier:            means that only one swab was positive
        # Persistent carrier: means that both swabs were positive
        #
        # And this is confusing as hell as you can imagine.
        #
        # In this code we are going to use the first definition because it makes
        # the code more legible. Later on, specially when we make the plots
        # you can see that we change the names of the variables some times.
        
        
        # (!) Default is "Negative" by the definition of colonization / carrier.
        #     There are a lot of Unknowns values but those default to negative
        #     always because that's how it was design to work.
        #
        #     So, init the default to negative everything
        {
            # -- Extra information for the AUREUS definition of CARRIER / COLONIZED / NON CARRIER
            # ---- COLONIZATION
            # ------- S1 by the direct culture
            aureusTable$S1_D_NasalColonize      = "Negative"
            aureusTable$S1_D_ThroatColonize     = "Negative"
            aureusTable$S1_D_Colonize           = "Negative" # (Nasal or Throat)
            # ------- S1 by the Enrichment broth
            aureusTable$S1_E_NasalColonize      = "Negative"
            aureusTable$S1_E_ThroatColonize     = "Negative"
            aureusTable$S1_E_Colonize           = "Negative"
            # ------- S2 (same as before)
            aureusTable$S2_D_NasalColonize      = "Negative"
            aureusTable$S2_D_ThroatColonize     = "Negative"
            aureusTable$S2_D_Colonize           = "Negative"
            aureusTable$S2_E_NasalColonize      = "Negative"
            aureusTable$S2_E_ThroatColonize     = "Negative"
            aureusTable$S2_E_Colonize           = "Negative"

            # ---- PERSISTANT CARRIER
            # ----- by the direct culture
            aureusTable$D_NasalCarrier          = "Negative"
            aureusTable$D_ThroatCarrier         = "Negative"
            aureusTable$D_Carrier               = "Negative" # (Nasal or Throat)
            # ----- by the Enrichment broth
            aureusTable$E_NasalCarrier          = "Negative"
            aureusTable$E_ThroatCarrier         = "Negative"
            aureusTable$E_Carrier               = "Negative"     

            # ---- PROBABILITY SUMMARY
            aureusTable$P_Nasal   = 0
            aureusTable$P_Throat  = 0
            aureusTable$P_Carrier = 0
        }
    
        # For each of the rows decide if patient is nasal/throat colonizer
        for(i in 1:originalTotalRows){

            # INDIVIDUAL VARIABLES FOR COLONIZATION
            # (The same repeat for each variable in nasal/throat and direct culture / enrichment broth
      
            # Nose variables
            {
                # Swab 1 Direct Culture in the nose
                # ---- If some bacterias grew, and those bacterias are SA, then you are candidate to positive
                if(aureusTable$S1_BacterialNasalGrowth[i] == "Yes" || aureusTable$S1_SA_Direct_NasalGrowth[i] == "Yes"){
                    # If the coagulase test for the direct culture is positive, then you are colonized for nasal in the swab 1
                    if(aureusTable$S1_Direct_CoagulaseNasal[i] == "Positive") 
                        aureusTable$S1_D_NasalColonize[i] = "Positive" }   
                    
                # Swab 1 Enrichment in the nose
                if(aureusTable$S1_BacterialNasalGrowth[i] == "Yes" || aureusTable$S1_SA_Enrich_NasalGrowth[i] == "Yes"){
                    if(aureusTable$S1_Enrich_CoagulaseNasal[i] == "Positive") 
                        aureusTable$S1_E_NasalColonize[i] = "Positive" }                           
                    
                # Swab 2 Coagulase in the nose
                if(aureusTable$S2_BacterialNasalGrowth[i] == "Yes" || aureusTable$S2_SA_Direct_NasalGrowth[i] == "Yes"){
                    if(aureusTable$S2_Direct_CoagulaseNasal[i] == "Positive") 
                        aureusTable$S2_D_NasalColonize[i] = "Positive" }      
                    
                # Swab 2 Enrichment in the nose
                if(aureusTable$S2_BacterialNasalGrowth[i] == "Yes" || aureusTable$S2_SA_Enrich_NasalGrowth[i] == "Yes"){
                    if(aureusTable$S2_Enrich_CoagulaseNasal[i] == "Positive") 
                        aureusTable$S2_E_NasalColonize[i] = "Positive" }                        
            }
        
            # Throat variables
            {
                # Swab 1 Direct Culture in the throat
                if(aureusTable$S1_BacterialThroatGrowth[i] == "Yes" || aureusTable$S1_SA_Direct_ThroatGrowth[i] == "Yes"){
                    if(aureusTable$S1_Direct_CoagulaseThroat[i] == "Positive") 
                        aureusTable$S1_D_ThroatColonize[i] = "Positive" }   
                    
                # Swab 1 Enrichment in the throat
                if(aureusTable$S1_BacterialThroatGrowth[i] == "Yes" || aureusTable$S1_SA_Enrich_ThroatGrowth[i] == "Yes"){
                    if(aureusTable$S1_Enrich_CoagulaseThroat[i] == "Positive") 
                        aureusTable$S1_E_ThroatColonize[i] = "Positive" }                           
                    
                # Swab 2 Coagulase in the throat
                if(aureusTable$S2_BacterialThroatGrowth[i] == "Yes" || aureusTable$S2_SA_Direct_ThroatGrowth[i] == "Yes"){
                    if(aureusTable$S2_Direct_CoagulaseThroat[i] == "Positive") 
                        aureusTable$S2_D_ThroatColonize[i] = "Positive" }      
                    
                # Swab 2 Enrichment in the throat
                if(aureusTable$S2_BacterialThroatGrowth[i] == "Yes" || aureusTable$S2_SA_Enrich_ThroatGrowth[i] == "Yes"){
                    if(aureusTable$S2_Enrich_CoagulaseThroat[i] == "Positive") 
                        aureusTable$S2_E_ThroatColonize[i] = "Positive" }                        
            }            
            
        }

        
        # For each of the rows decide if patient probability of having a bacteria
        # 
        # There are 4 definitions for nose (s1 direct, s1 enrich, s2 direct and s2 enrich)
        # and another 4 definitions for throat. Your P(Nasal) is equal to how many of those
        # variables are positive. Same for throat.
        #
        # Finally, the P(Carrier) definition is the average of P(Nasal) and P(Throat)
        # 
        # So in this definition, it doesn't matter if you are colonize or carrier
        # It only matters a unique standard definition
        for(i in 1:originalTotalRows){
           
            # Probability variables
            aureusTable$P_Nasal[i]  = sum(aureusTable$S1_D_NasalColonize[i] == "Positive",
                                          aureusTable$S1_E_NasalColonize[i] == "Positive",
                                          aureusTable$S2_D_NasalColonize[i] == "Positive",
                                          aureusTable$S2_E_NasalColonize[i] == "Positive")/4
              
            aureusTable$P_Throat[i] = sum(aureusTable$S1_D_ThroatColonize[i] == "Positive",
                                          aureusTable$S1_E_ThroatColonize[i] == "Positive",
                                          aureusTable$S2_D_ThroatColonize[i] == "Positive",
                                          aureusTable$S2_E_ThroatColonize[i] == "Positive")/4
              
            aureusTable$P_Carrier[i] = (aureusTable$P_Nasal[i] + aureusTable$P_Throat[i])/2       
            
        }
        

        # COMBINATION VARIABLES FOR COLONIZATION
        #
        # - If you are colonize in throat or nose, you are colonize
        # - If you are negative in both, you are not colonize
        # - If you are negative and unknown, you are unknown
        for(i in 1:originalTotalRows){
       
            # S1 Nasal + S1 Throat Coagulase (original numbers from when we decided what was positive or negative)
            if(aureusTable$S1_D_NasalColonize[i] == "Positive" || aureusTable$S1_D_ThroatColonize[i] == "Positive") aureusTable$S1_D_Colonize[i] = "Positive"
            # S1 Nasal + S1 Throat Enrichment
            if(aureusTable$S1_E_NasalColonize[i] == "Positive" || aureusTable$S1_E_ThroatColonize[i] == "Positive") aureusTable$S1_E_Colonize[i] = "Positive"
            # S2 Nasal + S2 Throat Coagulase
            if(aureusTable$S2_D_NasalColonize[i] == "Positive" || aureusTable$S2_D_ThroatColonize[i] == "Positive") aureusTable$S2_D_Colonize[i] = "Positive"
            # S2 Nasal + S2 Throat Enrichment
            if(aureusTable$S2_E_NasalColonize[i] == "Positive" || aureusTable$S2_E_ThroatColonize[i] == "Positive") aureusTable$S2_E_Colonize[i] = "Positive"

        }
      
        # COMBINATION VARIABLES FOR PERSISTENT CARRIER
        #
        # - If you are colonized in both S1 and S2 you are positive carrier
        #
        # - If either of S1 or S2 is negative, you are negative carrier
        #
        # - If either of nasal or throat is positive carrier, you are carrier.
        #   This might sound confusing because is not an AND, but OR. But this
        #   is how it is. If you are a persistent carrier in the nose, or a
        #   persistent carrier in the throat, then you are a persistent carrier
        #   even if the bacteria is active in one part but not the other.
        #
        # - If you are negative carrier in both nasal and throat, you are not carrier
        #
        # - If you are negative carrier and unknown carrier, you are unknown
        for(i in 1:originalTotalRows){

            # -- S1+S2 Nasal for Direct and Enrichment
            if(aureusTable$S1_D_NasalColonize[i]  == "Positive" && aureusTable$S2_D_NasalColonize[i]  == "Positive") aureusTable$D_NasalCarrier[i]  = "Positive"
            if(aureusTable$S1_E_NasalColonize[i]  == "Positive" && aureusTable$S2_E_NasalColonize[i]  == "Positive") aureusTable$E_NasalCarrier[i]  = "Positive"
            # -- S1+S2 Throat for Direct and Enrichment        
            if(aureusTable$S1_D_ThroatColonize[i] == "Positive" && aureusTable$S2_D_ThroatColonize[i] == "Positive") aureusTable$D_ThroatCarrier[i] = "Positive"
            if(aureusTable$S1_E_ThroatColonize[i] == "Positive" && aureusTable$S2_E_ThroatColonize[i] == "Positive") aureusTable$E_ThroatCarrier[i] = "Positive"
            # -- (S1+S2 Nasal AND S1+S2 Throat) Carrier status for Direct and Enrichment
            if(aureusTable$D_NasalCarrier[i]      == "Positive" || aureusTable$D_ThroatCarrier[i]     == "Positive") aureusTable$D_Carrier[i]       = "Positive"
            if(aureusTable$E_NasalCarrier[i]      == "Positive" || aureusTable$E_ThroatCarrier[i]     == "Positive") aureusTable$E_Carrier[i]       = "Positive"
        
        }
      
    }

    # Swabbing technical (nothing here)
    
    # Highschool (nothing here)
    
    # Blood (nothing here)
    
    # Blood technical (nothing here)
    
    # Sociology, there are multiple variables that are binary, which we combine
    # together and transform it into a new categorical variable while dropping
    # the previous one.
    #
    # This is done in the "numbers to categories" section because it was
    # more efficient and convinient to read it there and transform directly,
    # rather to bring it here.
    
    # Puberty men, some categorical transformed to numerical already, but
    # nothing new
    
    # Puberty women
    {

        # Transform age + months to proper age float variable    
        pubertyWomenTable$MenarcheAge = round(pubertyWomenTable$MenarcheAge + pubertyWomenTable$MenarcheMoths/12, 2)
        pubertyWomenTable$MenarcheMoths = NULL

    }
    
    # Menstruation
    #
    # We added how long are you in your cicle (0 to 1) with respect the blood
    # sample date.
    #
    {
        # Count how many days have pass since your last menstruation and the
        # date in which the blood test was taken.
        daysSinceLastMenstruation = as.Date(bloodTable$BloodAnalysisDate) - as.Date(menstruationTable$MenstruationDate)
     
        # Some women have too many days in between (200+), I have no idea
        # why that is, but we are going to prune those with 50+ days of
        # difference and set it to NA
        #
        # Again, R is horrible and NA comparisons are too much complicated
        for(i in 1:originalTotalRows){
        
            if(!is.na(daysSinceLastMenstruation[i])){
                if(daysSinceLastMenstruation[i]>50){
                    daysSinceLastMenstruation[i] = NA
                }
            }
        }
        
        # Finally, find out how far away are you in your cycle normalized
        # from 0 to 1. If you have 1+, just addjust to -1
        menstruationAdvance = daysSinceLastMenstruation/menstruationTable$MenstruationCycle
        for(i in 1:originalTotalRows){
        
            if(!is.na(menstruationAdvance[i])){
                if(menstruationAdvance[i]>1){
                    menstruationAdvance[i] = menstruationAdvance[i] - 1
                }
            }
        }
        
        # Add that info into the menstruation table, you need to combine this
        # with the blood table though.
        menstruationTable$CycleAdvance = menstruationAdvance
          
    }
    
    # Drugs (nothing here)
    
    # Sports (nothing here)
    
    # Hygiene (nothing here)
    
    # Hospitalization (nothing here)
    
    # Biomarkers (nothing here, although we could add the LOD info here)
    
    # Diet (nothing)
    
    # Sleeping
    {
    
        for(i in 1:originalTotalRows){
        
            currentTime = sleepTable$HoursSleeping[i]
            
            # R is horrible, why don't you have switch! ¬¬
            if(is.na(currentTime) == TRUE)  sleepTable$HoursSleeping[i] = NA
            else{
                
                if(currentTime == 1)  sleepTable$HoursSleeping[i] = 4
                if(currentTime == 2)  sleepTable$HoursSleeping[i] = 4.5
                if(currentTime == 3)  sleepTable$HoursSleeping[i] = 5
                if(currentTime == 4)  sleepTable$HoursSleeping[i] = 5.5
                if(currentTime == 5)  sleepTable$HoursSleeping[i] = 6
                if(currentTime == 6)  sleepTable$HoursSleeping[i] = 6.5
                if(currentTime == 7)  sleepTable$HoursSleeping[i] = 7
                if(currentTime == 8)  sleepTable$HoursSleeping[i] = 7.5
                if(currentTime == 9)  sleepTable$HoursSleeping[i] = 8
                if(currentTime == 10) sleepTable$HoursSleeping[i] = 8.5
                if(currentTime == 11) sleepTable$HoursSleeping[i] = 9
                if(currentTime == 12) sleepTable$HoursSleeping[i] = 9.5
                if(currentTime == 13) sleepTable$HoursSleeping[i] = 10
                if(currentTime == 14) sleepTable$HoursSleeping[i] = 10.5
                if(currentTime == 15) sleepTable$HoursSleeping[i] = 11
                if(currentTime == 16) sleepTable$HoursSleeping[i] = 11.5
                if(currentTime == 17) sleepTable$HoursSleeping[i] = 12                
            
            }

        }
        
    }
    
    # Diseases (nothing that haven't been changed already in the DB)

    # Frienship
    {
        # -- General friendship statistics for each network    
        # (notice that we can't initialize this numbers yet but we need to create the columns)
        
        # Also, this script only calculate friendship statistics such as,
        # popularity. I could include every combination of friendship statistics
        # here, such as how many of your friends have SA, how many use the same
        # contraceptives as you do, how many have the same biomarker profile as
        # you do, and so on. That would make it very combinient for saving and
        # loading, but would also make this script very complicated. So all of
        # those are left for each of the individual script that deals with those
        # data.

        # FF1
        # -- Overall network

        # Total connections
        frienshipTable$OverallConnections = 0
        # Popularity (how many people likes you, incoming edges)
        frienshipTable$OverallPopularity  = 0
        # Following  (how many people you like, outgoing edges)
        frienshipTable$OverallFollowing   = 0
        # Reciprocity (how many people you like, likes you back)
        frienshipTable$OverallReciprocity = 0

        # -- Physical

        frienshipTable$PhysicalConnections = 0
        frienshipTable$PhysicalPopularity  = 0
        frienshipTable$PhysicalFollowing   = 0
        frienshipTable$PhysicalReciprocity = 0

        # -- School

        frienshipTable$SchoolConnections = 0
        frienshipTable$SchoolPopularity  = 0
        frienshipTable$SchoolFollowing   = 0
        frienshipTable$SchoolReciprocity = 0

        # -- Sports

        frienshipTable$SportsConnections = 0
        frienshipTable$SportsPopularity  = 0
        frienshipTable$SportsFollowing   = 0
        frienshipTable$SportsReciprocity = 0

        # -- Home

        frienshipTable$HomeConnections = 0
        frienshipTable$HomePopularity  = 0
        frienshipTable$HomeFollowing   = 0
        frienshipTable$HomeReciprocity = 0

        # -- Other

        frienshipTable$OtherConnections = 0
        frienshipTable$OtherPopularity  = 0
        frienshipTable$OtherFollowing   = 0
        frienshipTable$OtherReciprocity = 0
        
        
        # FF12
        # -- Overall network
        frienshipTable$OverallConnectionsFF12 = 0
        frienshipTable$OverallPopularityFF12  = 0
        frienshipTable$OverallFollowingFF12   = 0
        frienshipTable$OverallReciprocityFF12 = 0

        # -- Physical

        frienshipTable$PhysicalConnectionsFF12 = 0
        frienshipTable$PhysicalPopularityFF12  = 0
        frienshipTable$PhysicalFollowingFF12   = 0
        frienshipTable$PhysicalReciprocityFF12 = 0

        # -- School

        frienshipTable$SchoolConnectionsFF12 = 0
        frienshipTable$SchoolPopularityFF12  = 0
        frienshipTable$SchoolFollowingFF12   = 0
        frienshipTable$SchoolReciprocityFF12 = 0

        # -- Sports

        frienshipTable$SportsConnectionsFF12 = 0
        frienshipTable$SportsPopularityFF12  = 0
        frienshipTable$SportsFollowingFF12   = 0
        frienshipTable$SportsReciprocityFF12 = 0

        # -- Home

        frienshipTable$HomeConnectionsFF12 = 0
        frienshipTable$HomePopularityFF12  = 0
        frienshipTable$HomeFollowingFF12   = 0
        frienshipTable$HomeReciprocityFF12 = 0

        # -- Other

        frienshipTable$OtherConnectionsFF12 = 0
        frienshipTable$OtherPopularityFF12  = 0
        frienshipTable$OtherFollowingFF12   = 0
        frienshipTable$OtherReciprocityFF12 = 0        
        
    }

}


# ---- Transform the network table into proper frienship matrix
#      There are 6 graphs, all of them directed.
#
#      Notice that we are saving the complete friendship matrix, and not the
# l    list of edges. Sometimes, methods use the friendship matrix, and some
#      other times, they use the edge list. Transforming from matrix to edges is
#      very easy, so we are keeping the matrix, and converting to edge later.
#      That way we can read the matrix, convert to edges, and keep both
#      variables and use them as we go.
#
#      Also, for what I can tell, other programs that are not R, Python or C,
#      tend to use matrices rather than edges too. So we also keep it like this
#      to maintain language independent data.
#
#      -- Overall friendship
#      -- Physical
#      -- Home
#      -- School
#      -- Sports
#      -- Others
{

    # Create the skeleton dataframe for each network
    {
        # Check all the IDs
        networkIDs = basicTable$ID
        totalIDs   = length(networkIDs)
        
        # Create the basic DF from where later we will create the edges data frames
        overallNetworkDF    = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        physicalNetworkDF   = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        homeNetworkDF       = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        schoolNetworkDF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        sportsNetworkDF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        otherNetworkDF      = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))

        overallNetworkFF12DF    = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        physicalNetworkFF12DF   = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        homeNetworkFF12DF       = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        schoolNetworkFF12DF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        sportsNetworkFF12DF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
        otherNetworkFF12DF      = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))        
                
        # Ensure that the rows and the columns refers to the same ID; for each of the networks we have
        # FF1
        # -- Overall
        overallNetworkDF$ID                     = networkIDs
        colnames(overallNetworkDF)[1:totalIDs]  = networkIDs
        # -- Physical
        physicalNetworkDF$ID                    = networkIDs
        colnames(physicalNetworkDF)[1:totalIDs] = networkIDs
        # -- Home
        homeNetworkDF$ID                        = networkIDs
        colnames(homeNetworkDF)[1:totalIDs]     = networkIDs
        # -- School
        schoolNetworkDF$ID                      = networkIDs
        colnames(schoolNetworkDF)[1:totalIDs]   = networkIDs
        # -- Sports
        sportsNetworkDF$ID                      = networkIDs
        colnames(sportsNetworkDF)[1:totalIDs]   = networkIDs
        # -- Others
        otherNetworkDF$ID                       = networkIDs
        colnames(otherNetworkDF)[1:totalIDs]    = networkIDs

        # (out of context complain, R is horrible because it doesn't give
        #  you warnings regarding unused variables)
        
        # FF12
        # -- Overall
        overallNetworkFF12DF$ID                     = networkIDs
        colnames(overallNetworkFF12DF)[1:totalIDs]  = networkIDs
        # -- Physical
        physicalNetworkFF12DF$ID                    = networkIDs
        colnames(physicalNetworkFF12DF)[1:totalIDs] = networkIDs
        # -- Home
        homeNetworkFF12DF$ID                        = networkIDs
        colnames(homeNetworkFF12DF)[1:totalIDs]     = networkIDs
        # -- School
        schoolNetworkFF12DF$ID                      = networkIDs
        colnames(schoolNetworkFF12DF)[1:totalIDs]   = networkIDs
        # -- Sports
        sportsNetworkFF12DF$ID                      = networkIDs
        colnames(sportsNetworkFF12DF)[1:totalIDs]   = networkIDs
        # -- Others
        otherNetworkFF12DF$ID                       = networkIDs
        colnames(otherNetworkFF12DF)[1:totalIDs]    = networkIDs
        
                
    }
        
    # For each of the people in the network
    # Fill all their edges information
    # FF1
    for(i in 1:totalIDs){
        
        # Identify each of your friends
        FriendA = networkTable$Friend1[i]
        FriendB = networkTable$Friend2[i]
        FriendC = networkTable$Friend3[i]
        FriendD = networkTable$Friend4[i]
        FriendE = networkTable$Friend5[i]
        
        # For each of the network, add that frienship link if it exist
        # -- The overall network doesn't care about anything, everyone comes here
        if(FriendA > 0) overallNetworkDF[i, FriendA] = 1
        if(FriendB > 0) overallNetworkDF[i, FriendB] = 1
        if(FriendC > 0) overallNetworkDF[i, FriendC] = 1
        if(FriendD > 0) overallNetworkDF[i, FriendD] = 1
        if(FriendE > 0) overallNetworkDF[i, FriendE] = 1
        # -- Physical
        if(FriendA > 0 && networkTable$Friend1Physical[i] == 1) physicalNetworkDF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2Physical[i] == 1) physicalNetworkDF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3Physical[i] == 1) physicalNetworkDF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4Physical[i] == 1) physicalNetworkDF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5Physical[i] == 1) physicalNetworkDF[i, FriendE] = 1
        # -- Home
        if(FriendA > 0 && networkTable$Friend1Home[i] == 1) homeNetworkDF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2Home[i] == 1) homeNetworkDF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3Home[i] == 1) homeNetworkDF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4Home[i] == 1) homeNetworkDF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5Home[i] == 1) homeNetworkDF[i, FriendE] = 1
        # -- School
        if(FriendA > 0 && networkTable$Friend1School[i] == 1) schoolNetworkDF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2School[i] == 1) schoolNetworkDF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3School[i] == 1) schoolNetworkDF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4School[i] == 1) schoolNetworkDF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5School[i] == 1) schoolNetworkDF[i, FriendE] = 1
        # -- Sports
        if(FriendA > 0 && networkTable$Friend1Sport[i] == 1) sportsNetworkDF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2Sport[i] == 1) sportsNetworkDF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3Sport[i] == 1) sportsNetworkDF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4Sport[i] == 1) sportsNetworkDF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5Sport[i] == 1) sportsNetworkDF[i, FriendE] = 1
        # -- Others
        if(FriendA > 0 && networkTable$Friend1Other[i] == 1) otherNetworkDF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2Other[i] == 1) otherNetworkDF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3Other[i] == 1) otherNetworkDF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4Other[i] == 1) otherNetworkDF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5Other[i] == 1) otherNetworkDF[i, FriendE] = 1
        
    }
    # FF12
    for(i in 1:totalIDs){
        
        # Identify each of your friends
        FriendA = networkTable$Friend1FF12[i]
        FriendB = networkTable$Friend2FF12[i]
        FriendC = networkTable$Friend3FF12[i]
        FriendD = networkTable$Friend4FF12[i]
        FriendE = networkTable$Friend5FF12[i]
        
        # For each of the network, add that frienship link if it exist
        # -- The overall network doesn't care about anything, everyone comes here
        if(FriendA > 0) overallNetworkFF12DF[i, FriendA] = 1
        if(FriendB > 0) overallNetworkFF12DF[i, FriendB] = 1
        if(FriendC > 0) overallNetworkFF12DF[i, FriendC] = 1
        if(FriendD > 0) overallNetworkFF12DF[i, FriendD] = 1
        if(FriendE > 0) overallNetworkFF12DF[i, FriendE] = 1
        # -- Physical
        if(FriendA > 0 && networkTable$Friend1PhysicalFF12[i] == 1) physicalNetworkFF12DF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2PhysicalFF12[i] == 1) physicalNetworkFF12DF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3PhysicalFF12[i] == 1) physicalNetworkFF12DF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4PhysicalFF12[i] == 1) physicalNetworkFF12DF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5PhysicalFF12[i] == 1) physicalNetworkFF12DF[i, FriendE] = 1
        # -- Home
        if(FriendA > 0 && networkTable$Friend1HomeFF12[i] == 1) homeNetworkFF12DF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2HomeFF12[i] == 1) homeNetworkFF12DF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3HomeFF12[i] == 1) homeNetworkFF12DF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4HomeFF12[i] == 1) homeNetworkFF12DF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5HomeFF12[i] == 1) homeNetworkFF12DF[i, FriendE] = 1
        # -- School
        if(FriendA > 0 && networkTable$Friend1SchoolFF12[i] == 1) schoolNetworkFF12DF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2SchoolFF12[i] == 1) schoolNetworkFF12DF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3SchoolFF12[i] == 1) schoolNetworkFF12DF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4SchoolFF12[i] == 1) schoolNetworkFF12DF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5SchoolFF12[i] == 1) schoolNetworkFF12DF[i, FriendE] = 1
        # -- Sports
        if(FriendA > 0 && networkTable$Friend1SportFF12[i] == 1) sportsNetworkFF12DF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2SportFF12[i] == 1) sportsNetworkFF12DF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3SportFF12[i] == 1) sportsNetworkFF12DF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4SportFF12[i] == 1) sportsNetworkFF12DF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5SportFF12[i] == 1) sportsNetworkFF12DF[i, FriendE] = 1
        # -- Others
        if(FriendA > 0 && networkTable$Friend1OtherFF12[i] == 1) otherNetworkFF12DF[i, FriendA] = 1
        if(FriendB > 0 && networkTable$Friend2OtherFF12[i] == 1) otherNetworkFF12DF[i, FriendB] = 1
        if(FriendC > 0 && networkTable$Friend3OtherFF12[i] == 1) otherNetworkFF12DF[i, FriendC] = 1
        if(FriendD > 0 && networkTable$Friend4OtherFF12[i] == 1) otherNetworkFF12DF[i, FriendD] = 1
        if(FriendE > 0 && networkTable$Friend5OtherFF12[i] == 1) otherNetworkFF12DF[i, FriendE] = 1
        
    }    
        
    # Now that you have the edges, you can initialize the edge counting you left blank before
    # FF1
    for(i in 1:totalIDs){
        
        # Print the progress bar
        print(getProgressCharacters((100*i)/totalIDs))
        print("")
        print("Initializing networks, please wait...")
        
        # Popularity by out degree, in degree, either, and reciprocal
        # -- For the overall
        {
        
            # Get the list of everyone that likes me
            popularThisIDsOverall                = overallNetworkDF[ (overallNetworkDF[,i] == 1), ]$ID
            frienshipTable$OverallPopularity[i]  = length(popularThisIDsOverall)
        
            # Get the list of everyone I like
            followingThisIDsOverall              = overallNetworkDF[overallNetworkDF[i,1:totalIDs] == 1,]$ID
            frienshipTable$OverallFollowing [i]  = length(followingThisIDsOverall)
        
            # Check the union of both sets for all relationship
            allRelationshipsOverall              = union( popularThisIDsOverall, followingThisIDsOverall )
            totalAllRelationshipsOverall         = length(allRelationshipsOverall)
            frienshipTable$OverallConnections[i] = totalAllRelationshipsOverall
        
            # Check the intersection of both sets for reciprocity
            realciprocallRelationshipsOverall    = intersect( popularThisIDsOverall, followingThisIDsOverall )
            frienshipTable$OverallReciprocity[i] = length(realciprocallRelationshipsOverall)
        
        }
        
        # -- Physical
        {
        
            popularThisIDs                        = physicalNetworkDF[    (physicalNetworkDF[,i] == 1), ]$ID
            frienshipTable$PhysicalPopularity[i]  = length(popularThisIDs)
            followingThisIDs                      = physicalNetworkDF[physicalNetworkDF[i,1:totalIDs] == 1,]$ID
            frienshipTable$PhysicalFollowing [i]  = length(followingThisIDs)
            allRelationshipsPhysical              = union( popularThisIDs, followingThisIDs )
            totalAllRelationshipsPhysical         = length(allRelationshipsPhysical)
            frienshipTable$PhysicalConnections[i] = totalAllRelationshipsPhysical
            realciprocallRelationships            = intersect( popularThisIDs, followingThisIDs )
            frienshipTable$PhysicalReciprocity[i] = length(realciprocallRelationships)
        
        }
        
        # -- Home
        {
        
            popularThisIDs                    = homeNetworkDF[    (homeNetworkDF[,i] == 1), ]$ID
            frienshipTable$HomePopularity[i]  = length(popularThisIDs)
            followingThisIDs                  = homeNetworkDF[homeNetworkDF[i,1:totalIDs] == 1,]$ID
            frienshipTable$HomeFollowing [i]  = length(followingThisIDs)
            allRelationshipsHome              = union( popularThisIDs, followingThisIDs )
            totalAllRelationshipsHome         = length(allRelationshipsHome)
            frienshipTable$HomeConnections[i] = totalAllRelationshipsHome
            realciprocallRelationships        = intersect( popularThisIDs, followingThisIDs )
            frienshipTable$HomeReciprocity[i] = length(realciprocallRelationships)
        
        }
        
        # -- School
        {
        
            popularThisIDs                      = schoolNetworkDF[  (schoolNetworkDF[,i] == 1), ]$ID
            frienshipTable$SchoolPopularity[i]  = length(popularThisIDs)
            followingThisIDs                    = schoolNetworkDF[schoolNetworkDF[i,1:totalIDs] == 1,]$ID
            frienshipTable$SchoolFollowing [i]  = length(followingThisIDs)
            allRelationshipsSchool              = union( popularThisIDs, followingThisIDs )
            totalAllRelationshipsSchool         = length(allRelationshipsSchool)
            frienshipTable$SchoolConnections[i] = totalAllRelationshipsSchool
            realciprocallRelationships          = intersect( popularThisIDs, followingThisIDs )
            frienshipTable$SchoolReciprocity[i] = length(realciprocallRelationships)
        
        }
        
        # -- Sport
        {
        
            popularThisIDs                      = sportsNetworkDF[    (sportsNetworkDF[,i] == 1), ]$ID
            frienshipTable$SportsPopularity[i]  = length(popularThisIDs)
            followingThisIDs                    = sportsNetworkDF[sportsNetworkDF[i,1:totalIDs] == 1,]$ID
            frienshipTable$SportsFollowing [i]  = length(followingThisIDs)
            allRelationshipsSports              = union( popularThisIDs, followingThisIDs )
            totalAllRelationshipsSports         = length(allRelationshipsSports)
            frienshipTable$SportsConnections[i] = totalAllRelationshipsSports
            realciprocallRelationships          = intersect( popularThisIDs, followingThisIDs )
            frienshipTable$SportsReciprocity[i] = length(realciprocallRelationships)
        
        }
        
        # -- Others
        {
        
            popularThisIDs                     = otherNetworkDF[    (otherNetworkDF[,i] == 1), ]$ID
            frienshipTable$OtherPopularity[i]  = length(popularThisIDs)
            followingThisIDs                   = otherNetworkDF[otherNetworkDF[i,1:totalIDs] == 1,]$ID
            frienshipTable$OtherFollowing [i]  = length(followingThisIDs)
            allRelationshipsOthers             = union( popularThisIDs, followingThisIDs )
            totalAllRelationshipsOthers        = length(allRelationshipsOthers)
            frienshipTable$OtherConnections[i] = totalAllRelationshipsOthers
            realciprocallRelationships         = intersect( popularThisIDs, followingThisIDs )
            frienshipTable$OtherReciprocity[i] = length(realciprocallRelationships)
        
        }
        

        
    }
    # FF12
    for(i in 1:totalIDs){
        
        # Print the progress bar
        print(getProgressCharacters((100*i)/totalIDs))
        print("")
        print("Initializing FF12 networks, please wait...")
        
        # Popularity by out degree, in degree, either, and reciprocal
        # -- For the overall
        {
        
            # Get the list of everyone that likes me
            popularThisIDsOverall                    = overallNetworkFF12DF[ (overallNetworkFF12DF[,i] == 1), ]$ID
            frienshipTable$OverallPopularityFF12[i]  = length(popularThisIDsOverall)
        
            # Get the list of everyone I like
            followingThisIDsOverall                  = overallNetworkFF12DF[overallNetworkFF12DF[i,1:totalIDs] == 1,]$ID
            frienshipTable$OverallFollowingFF12[i]   = length(followingThisIDsOverall)
        
            # Check the union of both sets for all relationship
            allRelationshipsOverall                  = union( popularThisIDsOverall, followingThisIDsOverall )
            totalAllRelationshipsOverall             = length(allRelationshipsOverall)
            frienshipTable$OverallConnectionsFF12[i] = totalAllRelationshipsOverall
        
            # Check the intersection of both sets for reciprocity
            realciprocallRelationshipsOverall        = intersect( popularThisIDsOverall, followingThisIDsOverall )
            frienshipTable$OverallReciprocityFF12[i] = length(realciprocallRelationshipsOverall)
        
        }
        
        # -- Physical
        {
        
            popularThisIDs                            = physicalNetworkFF12DF[    (physicalNetworkFF12DF[,i] == 1), ]$ID
            frienshipTable$PhysicalPopularityFF12[i]  = length(popularThisIDs)
            followingThisIDs                          = physicalNetworkFF12DF[physicalNetworkFF12DF[i,1:totalIDs] == 1,]$ID
            frienshipTable$PhysicalFollowingFF12 [i]  = length(followingThisIDs)
            allRelationshipsPhysical                  = union( popularThisIDs, followingThisIDs )
            totalAllRelationshipsPhysical             = length(allRelationshipsPhysical)
            frienshipTable$PhysicalConnectionsFF12[i] = totalAllRelationshipsPhysical
            realciprocallRelationships                = intersect( popularThisIDs, followingThisIDs )
            frienshipTable$PhysicalReciprocityFF12[i] = length(realciprocallRelationships)
        
        }
        
        # -- Home
        {
        
             popularThisIDs                        = homeNetworkFF12DF[    (homeNetworkFF12DF[,i] == 1), ]$ID
             frienshipTable$HomePopularityFF12[i]  = length(popularThisIDs)
             followingThisIDs                      = homeNetworkFF12DF[homeNetworkFF12DF[i,1:totalIDs] == 1,]$ID
             frienshipTable$HomeFollowingFF12[i]   = length(followingThisIDs)
             allRelationshipsHome                  = union( popularThisIDs, followingThisIDs )
             totalAllRelationshipsHome             = length(allRelationshipsHome)
             frienshipTable$HomeConnectionsFF12[i] = totalAllRelationshipsHome
             realciprocallRelationships            = intersect( popularThisIDs, followingThisIDs )
             frienshipTable$HomeReciprocityFF12[i] = length(realciprocallRelationships)
        
        }
        
        # -- School
        {
        
            popularThisIDs                          = schoolNetworkFF12DF[  (schoolNetworkFF12DF[,i] == 1), ]$ID
            frienshipTable$SchoolPopularityFF12[i]  = length(popularThisIDs)
            followingThisIDs                        = schoolNetworkFF12DF[schoolNetworkFF12DF[i,1:totalIDs] == 1,]$ID
            frienshipTable$SchoolFollowingFF12 [i]  = length(followingThisIDs)
            allRelationshipsSchool                  = union( popularThisIDs, followingThisIDs )
            totalAllRelationshipsSchool             = length(allRelationshipsSchool)
            frienshipTable$SchoolConnectionsFF12[i] = totalAllRelationshipsSchool
            realciprocallRelationships              = intersect( popularThisIDs, followingThisIDs )
            frienshipTable$SchoolReciprocityFF12[i] = length(realciprocallRelationships)
        
        }
        
        # -- Sport
        {
        
            popularThisIDs                          = sportsNetworkFF12DF[    (sportsNetworkFF12DF[,i] == 1), ]$ID
            frienshipTable$SportsPopularityFF12[i]  = length(popularThisIDs)
            followingThisIDs                        = sportsNetworkFF12DF[sportsNetworkFF12DF[i,1:totalIDs] == 1,]$ID
            frienshipTable$SportsFollowingFF12 [i]  = length(followingThisIDs)
            allRelationshipsSports                  = union( popularThisIDs, followingThisIDs )
            totalAllRelationshipsSports             = length(allRelationshipsSports)
            frienshipTable$SportsConnectionsFF12[i] = totalAllRelationshipsSports
            realciprocallRelationships              = intersect( popularThisIDs, followingThisIDs )
            frienshipTable$SportsReciprocityFF12[i] = length(realciprocallRelationships)
        
        }
        
        # -- Others
        {
        
            popularThisIDs                         = otherNetworkFF12DF[    (otherNetworkFF12DF[,i] == 1), ]$ID
            frienshipTable$OtherPopularityFF12[i]  = length(popularThisIDs)
            followingThisIDs                       = otherNetworkFF12DF[otherNetworkFF12DF[i,1:totalIDs] == 1,]$ID
            frienshipTable$OtherFollowingFF12 [i]  = length(followingThisIDs)
            allRelationshipsOthers                 = union( popularThisIDs, followingThisIDs )
            totalAllRelationshipsOthers            = length(allRelationshipsOthers)
            frienshipTable$OtherConnectionsFF12[i] = totalAllRelationshipsOthers
            realciprocallRelationships             = intersect( popularThisIDs, followingThisIDs )
            frienshipTable$OtherReciprocityFF12[i] = length(realciprocallRelationships)
        
        }
        

        
    }

    
    
}


# Variables pre analysis.
# For each table, count how many variables we have, and what types they are
# 
# The idea is to get a file that looks like this, for each table
#
# Variable Name | Type | Total Categories | Order
# -----------------------------------------------------------------------------
{
    
    # Put all tables that goes into a final complete table, into a list
    # Same for DB tables.
    #
    # Notice that, AGAIN, R doesn't have a proper reference system and
    # instead of doing this at the beginning and passing through memory
    # everything nicely, this is at the end to avoid copies not referenced
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
        
        # R is horrible, I can't reference the original table name
        # and use deparse to get the variable name. You need to
        # keep the variables for the files in a different vector.
        
        # In here we fill the information for each table manually
        {
            
            # In this case, all tables are stored in the same place
            # but you can change that if it makes more sense for your project.
            savedFolder = substr(DATA_READY_FOLDER, nchar(MAIN_PROJECT_FOLDER)+1, nchar(DATA_READY_FOLDER))
            
            # --  1 -- Basic
            tablesMetaInfo[1,1] = "basicTable"
            tablesMetaInfo[1,2] = "Basic Table"
            tablesMetaInfo[1,3] = "Basic information for each person; age, sex, ID, and so on. Also contain extra info that is not suitable for any other table."
            tablesMetaInfo[1,4] = savedFolder
            
            allTablesList[[1]]  = basicTable
            
            # --  2 -- networkTechnicalTable
            tablesMetaInfo[2,1] = "networkTechnicalTable"
            tablesMetaInfo[2,2] = "Network Technical Information Table"
            tablesMetaInfo[2,3] = "Information that has to do on how did we made the social network. The actual information about the network is stored in each of the network tables."
            tablesMetaInfo[2,4] = savedFolder
            
            allTablesList[[2]]  = networkTechnicalTable
            
            # --  3 -- antropometricFF1Table
            tablesMetaInfo[3,1] = "antropometricFF1Table"
            tablesMetaInfo[3,2] = "Antropometric Table for FF1"
            tablesMetaInfo[3,3] = "Antropometric variables for the FF1 timepoint."
            tablesMetaInfo[3,4] = savedFolder
            
            allTablesList[[3]]  = antropometricFF1Table
            
            # --  4 -- antropometricFF2Table
            tablesMetaInfo[4,1] = "antropometricFF2Table"
            tablesMetaInfo[4,2] = "Antropometric Table for FF2"
            tablesMetaInfo[4,3] = "Antropometric variables for the FF2 timepoint."
            tablesMetaInfo[4,4] = savedFolder
            
            allTablesList[[4]]  = antropometricFF2Table
            
            # --  5 -- menstruationTable
            tablesMetaInfo[5,1] = "menstruationTable"
            tablesMetaInfo[5,2] = "Menstruation Table"
            tablesMetaInfo[5,3] = "Information regarding menstrual parameters for women. Men IDs are keept in this table to maintain indexing consistency across tables."
            tablesMetaInfo[5,4] = savedFolder
            
            allTablesList[[5]]  = menstruationTable
            
            # --  6 -- aureusTable
            tablesMetaInfo[6,1] = "aureusTable"
            tablesMetaInfo[6,2] = "Staphylococcus Aureus related information."
            tablesMetaInfo[6,3] = "Here we store useful information regarding the grow of samples or whether a person is carrier or not. All other technical information is stored in another table."
            tablesMetaInfo[6,4] = savedFolder
            
            allTablesList[[6]]  = aureusTable
            
            # --  7 -- swabbingTable
            tablesMetaInfo[7,1] = "swabbingTable"
            tablesMetaInfo[7,2] = "Swabbing Technical Information Table"
            tablesMetaInfo[7,3] = "Information regarding how did the swabbing of patients when. Include medical comments as to why it didn't happen, errors, and all of that."
            tablesMetaInfo[7,4] = savedFolder
            
            allTablesList[[7]]  = swabbingTable
            
            # --  8 -- highSchoolTable
            tablesMetaInfo[8,1] = "highSchoolTable"
            tablesMetaInfo[8,2] = "High School Table"
            tablesMetaInfo[8,3] = "What is studying each person, and where."
            tablesMetaInfo[8,4] = savedFolder
            
            allTablesList[[8]]  = highSchoolTable
            
            # --  9 -- bloodTable
            tablesMetaInfo[9,1] = "bloodTable"
            tablesMetaInfo[9,2] = "Blood Test Table"
            tablesMetaInfo[9,3] = "All useful information regarding blood analysis. Technical information is stored in another table."
            tablesMetaInfo[9,4] = savedFolder
            
            allTablesList[[9]]  = bloodTable
            
            # -- 10 -- bloodTechnicalTable
            tablesMetaInfo[10,1] = "bloodTechnicalTable"
            tablesMetaInfo[10,2] = "Blood Test Technical Information"
            tablesMetaInfo[10,3] = "All technical information regarding the blood analysis. Include comments as if something happened during the blood extraction."
            tablesMetaInfo[10,4] = savedFolder
            
            allTablesList[[10]]  = bloodTechnicalTable
            
            # -- 11 -- sociologyTable
            tablesMetaInfo[11,1] = "sociologyTable"
            tablesMetaInfo[11,2] = "Sociology Table"
            tablesMetaInfo[11,3] = "Information regarding houshold cohabitants, parents educational background, and etchnicity"
            tablesMetaInfo[11,4] = savedFolder
            
            allTablesList[[11]]  = sociologyTable
            
            # -- 12 -- frienshipTable
            tablesMetaInfo[12,1] = "frienshipTable"
            tablesMetaInfo[12,2] = "Friendship Statistics Table"
            tablesMetaInfo[12,3] = "Table that store statistical data regarding frienship. All this info can be generated from the social network, but this serve as a quick pre-calculation to all that information, so you don't need to waste time calculating it for each script."
            tablesMetaInfo[12,4] = savedFolder
            
            allTablesList[[12]]  = frienshipTable
            
            # -- 13 -- pubertyMenTable
            tablesMetaInfo[13,1] = "pubertyMenTable"
            tablesMetaInfo[13,2] = "Puberty Information for Males"
            tablesMetaInfo[13,3] = "Puberty related information for makes. Women IDs are keept in this table to maintain indexing consistency across tables."
            tablesMetaInfo[13,4] = savedFolder
            
            allTablesList[[13]]  = pubertyMenTable
            
            # -- 14 -- pubertyWomenTable
            tablesMetaInfo[14,1] = "pubertyWomenTable"
            tablesMetaInfo[14,2] = "Puberty Information for Males"
            tablesMetaInfo[14,3] = "Puberty related information for makes. Women IDs are keept in this table to maintain indexing consistency across tables."
            tablesMetaInfo[14,4] = savedFolder
            
            allTablesList[[14]]  = pubertyWomenTable
            
            # -- 15 -- drugsTable
            tablesMetaInfo[15,1] = "drugsTable"
            tablesMetaInfo[15,2] = "Recreational Drugs Table"
            tablesMetaInfo[15,3] = "Information about the usage of recreational drugs. Only contain soft and legal drugs."
            tablesMetaInfo[15,4] = savedFolder
            
            allTablesList[[15]]  = drugsTable
            
            # -- 16 -- sportsTable
            tablesMetaInfo[16,1] = "sportsTable"
            tablesMetaInfo[16,2] = "Sport Table"
            tablesMetaInfo[16,3] = "Statistics about how much time people spend doing sports or other physical activities."
            tablesMetaInfo[16,4] = savedFolder
            
            allTablesList[[16]]  = sportsTable
            
            # -- 17 -- hygieneTable
            tablesMetaInfo[17,1] = "hygieneTable"
            tablesMetaInfo[17,2] = "Hygiene Table"
            tablesMetaInfo[17,3] = "How much people wash themselves, and how often do they go to a solarium"            
            tablesMetaInfo[17,4] = savedFolder
            
            allTablesList[[17]]  = hygieneTable
            
            # -- 18 -- biomarkersTable
            tablesMetaInfo[18,1] = "biomarkersTable"
            tablesMetaInfo[18,2] = "Biomarkers Table"
            tablesMetaInfo[18,3] = "All the info regarding the inflammatory biomarkers. There is no extra table with technical details, everything is important here."            
            tablesMetaInfo[18,4] = savedFolder
            
            allTablesList[[18]]  = biomarkersTable
            
            # -- 19 -- dietTable
            tablesMetaInfo[19,1] = "dietTable"
            tablesMetaInfo[19,2] = "Diet Table"
            tablesMetaInfo[19,3] = "Information regarding the nutriotional habits of each person."            
            tablesMetaInfo[19,4] = savedFolder
            
            allTablesList[[19]]  = dietTable
            
            # -- 20 -- sleepTable
            tablesMetaInfo[20,1] = "sleepTable"
            tablesMetaInfo[20,2] = "Sleep Table"
            tablesMetaInfo[20,3] = "Information about sleeping habits for each person."            
            tablesMetaInfo[20,4] = savedFolder
            
            allTablesList[[20]]  = sleepTable
            
            # -- 21 -- networkTable
            # Notice that we don't really need the network table anymore and we could
            # skip saving it. However, is very combinient to have it as it is for the
            # filtering process rather than having to addjust every other networkDF
            # one by one. So we are going to keep it anyway.            
            tablesMetaInfo[21,1] = "networkTable"
            tablesMetaInfo[21,2] = "Network Table for FF1"
            tablesMetaInfo[21,3] = "Complete information regarding social networks for the FF1 period. This same information can be found in several files properly divided by network type and FF1 or FF12."         
            tablesMetaInfo[21,4] = savedFolder
            
            allTablesList[[21]]  = networkTable
            
            # FF1 matrices
            
            # -- 22 -- overallNetworkDF
            tablesMetaInfo[22,1] = "overallNetworkDF"
            tablesMetaInfo[22,2] = "FF1 Overall Network Matrix"
            tablesMetaInfo[22,3] = "Matrix with all directed relationships combined for FF1."         
            tablesMetaInfo[22,4] = savedFolder
            
            allTablesList[[22]]  = overallNetworkDF
            
            # -- 23 -- physicalNetworkDF
            tablesMetaInfo[23,1] = "physicalNetworkDF"
            tablesMetaInfo[23,2] = "FF1 Physical Network Matrix"
            tablesMetaInfo[23,3] = "Matrix with the physical directed relationships for FF1."         
            tablesMetaInfo[23,4] = savedFolder
            
            allTablesList[[23]]  = physicalNetworkDF
            
            # -- 24 -- homeNetworkDF
            tablesMetaInfo[24,1] = "homeNetworkDF"
            tablesMetaInfo[24,2] = "FF1 Home Network Matrix"
            tablesMetaInfo[24,3] = "Matrix with the home directed relationships for FF1."         
            tablesMetaInfo[24,4] = savedFolder
            
            allTablesList[[24]]  = homeNetworkDF
            
            # -- 25 -- schoolNetworkDF
            tablesMetaInfo[25,1] = "schoolNetworkDF"
            tablesMetaInfo[25,2] = "FF1 School Network Matrix"
            tablesMetaInfo[25,3] = "Matrix with the school directed relationships for FF1."         
            tablesMetaInfo[25,4] = savedFolder
            
            allTablesList[[25]]  = schoolNetworkDF
            
            # -- 26 -- sportsNetworkDF
            tablesMetaInfo[26,1] = "sportsNetworkDF"
            tablesMetaInfo[26,2] = "FF1 Sport Network Matrix"
            tablesMetaInfo[26,3] = "Matrix with the sport directed relationships for FF1."              
            tablesMetaInfo[26,4] = savedFolder
            
            allTablesList[[26]]  = sportsNetworkDF
            
            # -- 27 -- otherNetworkDF
            tablesMetaInfo[27,1] = "otherNetworkDF"
            tablesMetaInfo[27,2] = "FF1 Sport Other Matrix"
            tablesMetaInfo[27,3] = "Matrix with the other directed relationships for FF1."                 
            tablesMetaInfo[27,4] = savedFolder
            
            allTablesList[[27]]  = otherNetworkDF 
            
            # FF12 Matrices

            # -- 28 -- overallNetworkFF12DF
            tablesMetaInfo[28,1] = "overallNetworkFF12DF"
            tablesMetaInfo[28,2] = "FF12 Overall Network Matrix"
            tablesMetaInfo[28,3] = "Matrix with all directed relationships combined for FF12."         
            tablesMetaInfo[28,4] = savedFolder
            
            allTablesList[[28]]  = overallNetworkFF12DF
            
            # -- 29 -- physicalNetworkFF12DF
            tablesMetaInfo[29,1] = "physicalNetworkFF12DF"
            tablesMetaInfo[29,2] = "FF12 Physical Network Matrix"
            tablesMetaInfo[29,3] = "Matrix with the physical directed relationships for FF12."         
            tablesMetaInfo[29,4] = savedFolder
            
            allTablesList[[29]]  = physicalNetworkFF12DF
            
            # -- 30 -- homeNetworkFF12DF
            tablesMetaInfo[30,1] = "homeNetworkFF12DF"
            tablesMetaInfo[30,2] = "FF12 Home Network Matrix"
            tablesMetaInfo[30,3] = "Matrix with the home directed relationships for FF12."         
            tablesMetaInfo[30,4] = savedFolder
            
            allTablesList[[30]]  = homeNetworkFF12DF
            
            # -- 31 -- schoolNetworkFF12DF
            tablesMetaInfo[31,1] = "schoolNetworkFF12DF"
            tablesMetaInfo[31,2] = "FF12 School Network Matrix"
            tablesMetaInfo[31,3] = "Matrix with the school directed relationships for FF12."         
            tablesMetaInfo[31,4] = savedFolder
            
            allTablesList[[31]]  = schoolNetworkFF12DF
            
            # -- 32 -- sportsNetworkFF12DF
            tablesMetaInfo[32,1] = "sportsNetworkFF12DF"
            tablesMetaInfo[32,2] = "FF12 Sport Network Matrix"
            tablesMetaInfo[32,3] = "Matrix with the sport directed relationships for FF12."              
            tablesMetaInfo[32,4] = savedFolder
            
            allTablesList[[32]]  = sportsNetworkFF12DF
            
            # -- 33 -- otherNetworkFF12DF
            tablesMetaInfo[33,1] = "otherNetworkFF12DF"
            tablesMetaInfo[33,2] = "FF12 Sport Other Matrix"
            tablesMetaInfo[33,3] = "Matrix with the other directed relationships for FF12."                 
            tablesMetaInfo[33,4] = savedFolder
            
            allTablesList[[33]]  = otherNetworkFF12DF       
            
            # DBDF tables
            
            # -- 34 -- medicinesDBDF
            tablesMetaInfo[34,1] = "medicinesDBDF"
            tablesMetaInfo[34,2] = "Medicine usage database"
            tablesMetaInfo[34,3] = "List of people and the medicine that they use. Notice that the hormonal contraceptive information is included here, but not other contraceptives such as condons that don't change your blood composition."         
            tablesMetaInfo[34,4] = savedFolder
            
            allTablesList[[34]]  = medicinesDBDF
            
            # -- 35 -- contraceptivesDBDF
            tablesMetaInfo[35,1] = "contraceptivesDBDF"
            tablesMetaInfo[35,2] = "Contraceptive usage database"
            tablesMetaInfo[35,3] = "List of women and the contraceptives that they use. All types of contraceptives are included."         
            tablesMetaInfo[35,4] = savedFolder
            
            allTablesList[[35]]  = contraceptivesDBDF
            
            # -- 36 -- diseasesDBDF
            tablesMetaInfo[36,1] = "diseasesDBDF"
            tablesMetaInfo[36,2] = "Chronic diseases database"
            tablesMetaInfo[36,3] = "List of people and the chronic diseases that are afflicting them."         
            tablesMetaInfo[36,4] = savedFolder
            
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
        
        # Save the info to disk
        
    }

}

# Join tables with proper info together
# -----------------------------------------------------------------------------
if(FALSE){
    # Check that no column name is repeated in any other table
    # Except for ID, is is fine as long as there is no other, ie: "ID", "id", "Id"
    print("Checking that all columns have indepedent names")
    {
        
        allNames = c(colnames(basicTable),
                     colnames(networkTechnicalTable),
                     colnames(antropometricFF1Table),
                     colnames(menstruationTable),
                     colnames(aureusTable),
                     colnames(swabbingTable),
                     colnames(highSchoolTable),
                     colnames(bloodTable),
                     colnames(bloodTechnicalTable),
                     # colnames(questionariesTable),            
                     colnames(sociologyTable),            
                     colnames(frienshipTable),
                     colnames(pubertyMenTable),            
                     colnames(pubertyWomenTable),            
                     colnames(drugsTable),
                     colnames(sportsTable),
                     colnames(hygieneTable),
                     #colnames(hospitalizationTable),
                     colnames(biomarkersTable),
                     colnames(dietTable),
                     colnames(sleepTable))
    
        uniquesNames  = unique(allNames)
        repeatedNames = setdiff(allNames, uniquesNames)
        print("Duplicated column names")
        print(unique(allNames[duplicated(allNames)]))
                
    }
    
    # Join everything into a complete table for each FF group
    {

        # FF1
      
        # We don't use the network table, as it is only use to generate the
        # friendships matrices
        #
        # Neither the DB tables of disease, medicine, and contraceptives
      
        completeTable          = basicTable     %>% left_join(antropometricFF1Table, by="ID")
        completeTable          = completeTable  %>% left_join(networkTechnicalTable, by="ID")
        completeTable          = completeTable  %>% left_join(aureusTable ,          by="ID")
        completeTable          = completeTable  %>% left_join(swabbingTable ,        by="ID")
        completeTable          = completeTable  %>% left_join(menstruationTable,     by="ID")
        completeTable          = completeTable  %>% left_join(highSchoolTable ,      by="ID")
        completeTable          = completeTable  %>% left_join(bloodTable ,           by="ID")
        completeTable          = completeTable  %>% left_join(bloodTechnicalTable ,  by="ID")
        #completeTable          = completeTable  %>% left_join(questionariesTable ,   by="ID")
        completeTable          = completeTable  %>% left_join(sociologyTable ,       by="ID")
        completeTable          = completeTable  %>% left_join(frienshipTable ,       by="ID")
        completeTable          = completeTable  %>% left_join(pubertyMenTable ,      by="ID")
        completeTable          = completeTable  %>% left_join(pubertyWomenTable ,    by="ID")
        completeTable          = completeTable  %>% left_join(drugsTable ,           by="ID")
        completeTable          = completeTable  %>% left_join(sportsTable ,          by="ID")
        completeTable          = completeTable  %>% left_join(hygieneTable ,         by="ID")
        #completeTable          = completeTable  %>% left_join(hospitalizationTable , by="ID")
        completeTable          = completeTable  %>% left_join(biomarkersTable ,      by="ID")
        completeTable          = completeTable  %>% left_join(dietTable ,            by="ID")
        completeTable          = completeTable  %>% left_join(sleepTable ,           by="ID")        
                
    }
}

# Everything is finish now, we just need to write the CSVs into disk so we don't
# have to do this over and over in each analysis. We just read them.
{

    # Write all tables into disk
    for (i in 1:totalTables){
            
            currentTable    = allTablesList[[i]]    
            currentFilePath = paste0(MAIN_PROJECT_FOLDER, tablesMetaInfo[i,4], tablesMetaInfo[i,1], ".csv")
    
            write.csv2( currentTable, file = currentFilePath, row.names = FALSE)
                    
    }
    
    # The disease, drug and contraceptives tables
    #write.csv2( medicinesDBDF,         file = READY_DATA_MEDICINES_FILEPATH,       row.names = FALSE)
    #write.csv2( contraceptivesDBDF,    file = READY_DATA_CONTRACEPTIVES_FILEPATH,  row.names = FALSE)
    #write.csv2( diseasesDBDF,          file = READY_DATA_DISEASES_FILEPATH,        row.names = FALSE)
    
    
    # The complete table with everything
    # FF1
    #write.csv2( completeTable        , file = READY_DATA_COMPLETE_FILEPATH,        row.names = FALSE)
      
    # Each table individually
    #
    # R is horrible, there is no precompiler that tells you a warning about unused
    # variables. So when you have in this case "sleepTable" that wasn't properly asigned 
    # it takes a long time to track the bug down.
    if(FALSE){
        write.csv2( basicTable            , file = READY_DATA_BASIC_FILEPATH,               row.names = FALSE)
        write.csv2( networkTechnicalTable , file = READY_DATA_NETWORK_TECHNICAL_FILEPATH,   row.names = FALSE)
        write.csv2( antropometricFF1Table , file = READY_DATA_FF1_ANTROPOMETRIC_FILEPATH,   row.names = FALSE)
        write.csv2( antropometricFF2Table , file = READY_DATA_FF2_ANTROPOMETRIC_FILEPATH,   row.names = FALSE)
        write.csv2( menstruationTable     , file = READY_DATA_MENSTRUATION_FILEPATH,        row.names = FALSE)
        write.csv2( aureusTable           , file = READY_DATA_AUREUS_FILEPATH,              row.names = FALSE)
        write.csv2( swabbingTable         , file = READY_DATA_SWABBING_FILEPATH,            row.names = FALSE)
        write.csv2( highSchoolTable       , file = READY_DATA_HIGHSCHOOL_FILEPATH,          row.names = FALSE)
        write.csv2( bloodTable            , file = READY_DATA_BLOOD_FILEPATH,               row.names = FALSE)
        write.csv2( bloodTechnicalTable   , file = READY_DATA_BLOOD_TECHNICAL_FILEPATH,     row.names = FALSE)
        #write.csv2( questionariesTable    , file = READY_DATA_QUESTIONARIES_FILEPATH,       row.names = FALSE)
        write.csv2( sociologyTable        , file = READY_DATA_SOCIOLOGY_FILEPATH,           row.names = FALSE)
        write.csv2( frienshipTable        , file = READY_DATA_FRIENSHIP_FILEPATH,           row.names = FALSE)
        write.csv2( pubertyMenTable       , file = READY_DATA_PUBERTYMEN_FILEPATH,          row.names = FALSE)
        write.csv2( pubertyWomenTable     , file = READY_DATA_PUBERTYWOMEN_FILEPATH,        row.names = FALSE)
        write.csv2( drugsTable            , file = READY_DATA_DRUGS_FILEPATH,               row.names = FALSE)
        write.csv2( sportsTable           , file = READY_DATA_SPORTS_FILEPATH,              row.names = FALSE)
        write.csv2( hygieneTable          , file = READY_DATA_HYGIENE_FILEPATH,             row.names = FALSE)
        #write.csv2( hospitalizationTable  , file = READY_DATA_HOSPITALIZATION_FILEPATH,     row.names = FALSE)
        write.csv2( biomarkersTable       , file = READY_DATA_BIOMARKERS_FILEPATH,          row.names = FALSE)
        write.csv2( dietTable             , file = READY_DATA_DIET_FILEPATH,                row.names = FALSE)
        write.csv2( sleepTable            , file = READY_DATA_SLEEP_FILEPATH,               row.names = FALSE)

        # Notice that we don't really need the network table anymore and we could
        # skip saving it. However, is very combinient to have it as it is for the
        # filtering process rather than having to addjust every other networkDF
        # one by one. So we are going to keep it anyway.
        write.csv2( networkTable         , file = READY_DATA_NETWORK_FILEPATH,       row.names = FALSE)
    
        # The overall  friendship matrix
        # The physical friendship matrix
        # The home     friendship matrix
        # The school   friendship matrix
        # The sport    friendship matrix
        # The other    friendship matrix
        
        write.csv2(overallNetworkDF,  file = READY_DATA_OVERALL_NETWORK_FILEPATH   , row.names = FALSE)
        write.csv2(physicalNetworkDF, file = READY_DATA_PHYSICAL_NETWORK_FILEPATH  , row.names = FALSE)
        write.csv2(homeNetworkDF,     file = READY_DATA_HOME_NETWORK_FILEPATH      , row.names = FALSE)
        write.csv2(schoolNetworkDF,   file = READY_DATA_SCHOOL_NETWORK_FILEPATH    , row.names = FALSE)
        write.csv2(sportsNetworkDF,   file = READY_DATA_SPORTS_NETWORK_FILEPATH    , row.names = FALSE)
        write.csv2(otherNetworkDF,    file = READY_DATA_OTHERS_NETWORK_FILEPATH    , row.names = FALSE)
        
        write.csv2(overallNetworkFF12DF,  file = READY_DATA_FF12_OVERALL_NETWORK_FILEPATH   , row.names = FALSE)
        write.csv2(physicalNetworkFF12DF, file = READY_DATA_FF12_PHYSICAL_NETWORK_FILEPATH  , row.names = FALSE)
        write.csv2(homeNetworkFF12DF,     file = READY_DATA_FF12_HOME_NETWORK_FILEPATH      , row.names = FALSE)
        write.csv2(schoolNetworkFF12DF,   file = READY_DATA_FF12_SCHOOL_NETWORK_FILEPATH    , row.names = FALSE)
        write.csv2(sportsNetworkFF12DF,   file = READY_DATA_FF12_SPORTS_NETWORK_FILEPATH    , row.names = FALSE)
        write.csv2(otherNetworkFF12DF,    file = READY_DATA_FF12_OTHERS_NETWORK_FILEPATH    , row.names = FALSE)    
    }

}

# Fill the extra info for each variable in each table
{

    # Each table have an associated DF with info about the variables that we
    # fill on the fly. The DF looks like this.
    #
    #     Variable Original Name | Variable New Name | Variable Human Name | Description         | Color Vector | Tags
    #
    #     AGE_FF1                  Age                 Age                   How old is a person   NA             NA            
    #     ...
    #
    # First, we initialize this information for each variable
    tableExtraInfoColumnsNames = c( "Name",  "Human Name", "Description", "Color Vector", "Ontology ID")    
    
    allExtraInfoList  = newList(totalTables)
    
    for(i in 1:totalTables){
    
        totalVariables = tablesMetaInfo[i,6]
        
        allExtraInfoList[[i]]           = DF(totalVariables,5)
        colnames(allExtraInfoList[[i]]) = tableExtraInfoColumnsNames
            
        for (j in 1:totalVariables){
        
            allExtraInfoList[[i]][j,1] = colnames(allTablesList[[i]])[j]
            allExtraInfoList[[i]][j,2] = colnames(allTablesList[[i]])[j] # Human Name is the same as the computer name by default, in many cases you don't need to change anything
                
        }

    }
    # Now we fill this table one by one as needed
    #
    # Basic table
    {
        # ID
        allExtraInfoList[[1]][1,3] = "Unique Identifier for each person, integer from 1 to 1038"
        allExtraInfoList[[1]][1,5] = "ID, Key, Identifier"
        
        # Dates
        allExtraInfoList[[1]][2,2] = "Attendance Date FF1"
        allExtraInfoList[[1]][3,2] = "Attendance Date FF12"
        allExtraInfoList[[1]][4,2] = "Medication Date FF1"
        allExtraInfoList[[1]][5,2] = "Questionary Date FF1"
        
        allExtraInfoList[[1]][2,3] = "Dates in which the swabbing was colected for the FF1 group, so first sample of FF1"
        allExtraInfoList[[1]][3,3] = "Dates in which the swabbing was colected for the FF12 group, so second sample of FF1"
        allExtraInfoList[[1]][4,3] = "Dates in which the medication questionaries where filled for FF1"
        allExtraInfoList[[1]][5,3] = "Dates in which several questionaries independently from medication and diseases where filled for FF1"        
        
        # Sex
        allExtraInfoList[[1]][6,3] = "What is the biological sex of each person."
        allExtraInfoList[[1]][6,4] = paste0(COLOR_VECTOR_SEX, collapse = " ")
        allExtraInfoList[[1]][6,5] = "Sex, men, women, man, woman"
                
        # Age
        allExtraInfoList[[1]][7,3] = "What is the age of each person at the time of the FF1 registration."
        allExtraInfoList[[1]][7,5] = "time, age"        
        
        # General Health
        allExtraInfoList[[1]][8,3] = "The subjective feeling that each person has regarding health."
        allExtraInfoList[[1]][8,4] = paste0(COLOR_VECTOR_HEALTH, collapse = " ")
        allExtraInfoList[[1]][8,5] = "Sex, men, women, man, woman"        
        
        # Age FF12
        allExtraInfoList[[1]][9,3] = "What is the age of each person at the time of the FF12 registration."
        allExtraInfoList[[1]][9,5] = "time, age"                
        
    }
        
    
}


# Finally, we are going to write the summaries of everything we have done in
# this script. This consist of:
#
#     1.- Writting the .info file associated with each table
#             A) Table information
#             B) Table head
#             C) Each variable information and summary
#
#     2.- Finding out which where the original names, and what are the new names
#
#         ( This is not possible to do automatically completely. You can extract
#           most of the tables, but there are complex things such as the DB
#           relationships, antropometric where we use several columns to create
#           a new one, and many more , so after long time, I ended up ditching
#           trying to do this automatically. )
#
#     3.- Find out all the mapping of values

# Get info of everything
#
# THere are some bugs here, skip this process for now, but come back to this later
if(FALSE){
    
    # First you get the general info of the table
    for (i in 1:totalTables){
            
        # Get the basic info
        currentTable          = allTablesList[[i]]
        currentTableName      = tablesMetaInfo[i,1]
        currentTotalVariables = tablesMetaInfo[i,6]
        currentFilePath       = paste0(MAIN_PROJECT_FOLDER, tablesMetaInfo[i,4], currentTableName, ".info")            
            
        # Get the head summary into proper string form
        headString    = getStringFromDF(head(allTablesList[[i]]), extraOffset = 4)
            
        # Init the .info string
        currentString = ""
            
        # Basic Info
        currentString = paste0(currentString, "--------------------------------------------------------------------------------\n")            
        currentString = paste0(currentString, currentTableName, "\n")
        currentString = paste0(currentString, tablesMetaInfo[i,2], "\n")
        currentString = paste0(currentString, tablesMetaInfo[i,3], "\n")            
        currentString = paste0(currentString, tablesMetaInfo[i,5], " x ", tablesMetaInfo[i,6], "\n") 
        currentString = paste0(currentString, "--------------------------------------------------------------------------------\n")            
        # Head info
        currentString = paste0(currentString, "\n")
        currentString = paste0(currentString, "\n")
        currentString = paste0(currentString, headString)
        currentString = paste0(currentString, "\n")
        currentString = paste0(currentString, "\n")
        currentString = paste0(currentString, "--------------------------------------------------------------------------------\n")            
        currentString = paste0(currentString, "\n")
        # Each variable info
        for(j in 1:currentTotalVariables){
                
            print("----")
            print(j)
            
            currentVariableType = getVariableType(currentTable, j)
            
            print(currentVariableType)
                
            currentString = paste0(currentString, "    ----------------------------------------------------------------------------\n")                
            currentString = paste0(currentString, "    ", allExtraInfoList[[i]][j,1], "\n")
            currentString = paste0(currentString, "    ", currentVariableType, "\n")
            currentString = paste0(currentString, "    ", allExtraInfoList[[i]][j,2], "\n")    
            currentString = paste0(currentString, "    ", allExtraInfoList[[i]][j,3], "\n")
            currentString = paste0(currentString, "    ", allExtraInfoList[[i]][j,5], "\n")    
            currentString = paste0(currentString, "    ----------------------------------------------------------------------------\n")                
            currentString = paste0(currentString, "\n")
            
            print("--ssss--")
            
            # Here it goes the variable summary, which depends of the type
            # of variable that we have.
            if(currentVariableType == "Date"){
                
                currentMinimumDate = min(currentTable[,j], na.rm = TRUE)
                currentMaximumDate = max(currentTable[,j], na.rm = TRUE)
                                    
                currentString = paste0(currentString, "\n")
                currentString = paste0(currentString, "        ", currentMinimumDate, "\n")
                currentString = paste0(currentString, "        ", currentMaximumDate, "\n")
                    
            }
                
            if(currentVariableType == "Discrete" || currentVariableType == "Continuous"){
                
                    currentString = paste0(currentString, "\n")
                        
                    summaryString = getStringFromNumerical(currentTable, j, extraOffset = 8)
                    currentString = paste0(currentString, summaryString)
                    
            }                
            
            if(currentVariableType == "Categorical"){
            
                print("--9999--")
                
                summaryDF       = summarizeCategorical(currentTable,j, sorted = "none")
                totalCategories = nrow(summaryDF)
                    
                print("--3333--")
                
                currentString = paste0(currentString, "\n")
                currentString = paste0(currentString, "        Total Categories: ", as.character(totalCategories), "\n")
                currentString = paste0(currentString, "\n")
                    
                # If we have too many categories, write the top 10 and skip the rest
                if(totalCategories>10){
                
                    summaryDF       = summarizeCategorical(currentTable,j, crop = 10, sorted = "top" )
                    totalCategories = 10
                        
                }
                # If there is an acceptable amount, write them in factor order
                for(k in 1:totalCategories){
                    
                    print("--")
                    print(i)
                    print(j)
                    print(k)
                    print("--")
                    
                    
                    currentModalityName  = as.character(summaryDF[k,1])
                    print(currentModalityName)
                    currentModalityTotal = as.character(summaryDF[k,2])
                    print(currentModalityTotal)
                    currentModalityColor = NA
                    if(!is.na(allExtraInfoList[[i]][j,4])){
                        currentModalityColor = strsplit(allExtraInfoList[[i]][j,4], " ", fixed = TRUE)[[1]][k]    
                    }
                    print(currentModalityColor)
                    currentModalityTags  = " no ontology reference "
                    print(currentModalityTags)
                    
                    currentString = paste0(currentString, "        ", currentModalityName, " | ", currentModalityTotal, " | ", currentModalityColor, " | ", currentModalityTags, "\n")

                    print("aa")
                    
                }
                    
                currentString = paste0(currentString, "\n")
                
                    
            }
            
            currentString = paste0(currentString, "\n")
            currentString = paste0(currentString, "\n")
            currentString = paste0(currentString, "\n")
            currentString = paste0(currentString, "\n")
                
        }
            
            
            #allExtraInfoList
            

            
            
        writeLinesDisk(currentString, currentFilePath)
            
    }
        
}
   

if(FALSE){
 
        # Then for each table, you have a bunch of variables
        # For each variable, you get which type of variables you have.
        #
        # If the variable is a date, you get the minimum and maximum
        # If the variable is numerical, you get the numeric statistics
        # If the variable is categorical, you get each modality, and how many
        # are of each.
        
        
        # The final result looks similar to this:
        #
        #
        # --------------------------------------------------
        # myTableName
        # My Table has a Name
        # This table control that tables have indeed a name and is very important information
        # 433 x 40
        # --------------------------------------------------
        #
        #     (blank line)
        #     (blank line)
        #     (Head example)
        #     (blank line)
        #     (blank line)
        #
        # --------------------------------------------------
        #

        #
        #
        #     --------------------------------------------------
        #     secondVariable
        #     variable type 3
        #     Second Variable Name in human form
        #     Second variable description
        #     Second variable tags
        #     --------------------------------------------------
        #     (blank line)
        #          First Modality  | 34 | #AA4457 | indentity tags
        #          Second Modality | 56 | #999999 | indentity tags
        #          Third Modality  |  3 | #494949 | indentity tags
        #     (blank line)
        #     (blank line)
        #     (blank line)        
    
        
        currentTotalRows    = tablesMetaInfo[1,5]
        currentTotalColumns = tablesMetaInfo[1,6]
        
        for(j in 1:currentTotalColumns){
        
            print(getVariableType(allTablesList[[1]], j))
                
        }
        
        print(getVariableType(allTablesList[[1]], 1))
        print(getVariableType(basicTable, 1))
        
        head(allTablesList[[1]])    
        
        class(basicTable[,1])
        
        # Is this significantly different from a distribution...
        #
        # Normal distribution?
        #shapiro.test(basicTable[,1])$p.value
        # Uniform distribution?
        #ks.test(basicTable[,1], "punif", min(basicTable[,1], na.rm = TRUE), max(basicTable[,1], na.rm = TRUE))
        
        # Do this
        # descdist(basicTable[,7], boot = 500)
        # With a density plot
        # A boxplot
        # And with the statistics printed
        
        
}


# Close the TXT connections
close(logTXTFileConnection)

# Write the Latex tables with respect the data that we missed
writeTableLATEX(dataCleaningLogDF, LOGS_FOLDER, 
                tableCaption      = "Summary of lost connections at data cleaning.",
                overrideTableName = "SummaryLost", widthProportion = 0.5, heightProportion = 0.1)

# Plot the boxplots for missing data and show the data on screen
{
    
    myYMaximum = max( max(remainingToDeletedFriendsLostDF$Total) , max(deletedByRemainingFriendsLostDF$Total) )
    
    # In this list, we keep the image file path for each of the plots
    myListOfPlotsObjects = newList(2)    
    myListOfPlotsPaths   = newList(2) 
    
    nominationPlot = doBoxPlot(remainingToDeletedFriendsLostDF, 2, LOGS_FOLDER, 
              colorsVector = COLOR_RED_MED,
              plotTitle    = "Nominations lost; from remaining to unknown",
              plotSubtitle = "",
              plotXLabel   = "Missing Friends", plotYLabel = "Total",
              plotTheme    = "simple",
              ymin = 0, ymax = myYMaximum,
              overrideCaption = "Boxplot for number of missing edges from existing keys to deleted keys.")
    
    popularityPlot = doBoxPlot(deletedByRemainingFriendsLostDF, 2, LOGS_FOLDER, 
              colorsVector = COLOR_BLUE_MED,
              plotTitle    = "Popularity of people who are unknown.",
              plotSubtitle = "",
              plotXLabel   = "Nominations", plotYLabel = "", # No Y-label, looks better in composition
              plotTheme    = "simple",
              ymin = 0, ymax = myYMaximum,
              overrideCaption = "Boxplot for number of missing edges by existing keys to deleted keys.")    
    
    myListOfPlotsObjects[[1]] = nominationPlot[[1]]
    myListOfPlotsObjects[[2]] = popularityPlot[[1]]
    myListOfPlotsPaths[[1]]   = nominationPlot[[3]]
    myListOfPlotsPaths[[2]]   = popularityPlot[[3]]    
    
    # .. For PNG
    ggarrange(plotlist =  myListOfPlotsObjects , ncol = 2, nrow = 1)
    nominationsPath    = file.path(paste(LOGS_FOLDER, "NominationsBoxplots.png", sep = ""))
    ggsave(nominationsPath, width = 8, height = 16)
    
    # .. For LATEX
    writeImageLATEX2(nominationsPath, LATEX_RELATIVE_FOLDER_LOGS, 
                     captionText   = "Summary of all nominations lost.",
                     overrideLabel = "fig:allnominationsboxplots", 
                     pageHeight    = 0.3, overrideFloat = TRUE)      


    #print("List of people who nominated somone that doesn't exist in the dataset")
    #print(remainingToDeletedFriendsLostDF)
    
    #print("List of people who doesn't exist and which existing person nominate them")
    #print(deletedByRemainingFriendsLostDF)
}


# In here you could do
#
# read ..\whatev\dataCleaningV2.R
#
# Find all the map values
#
# Find the variable to -> from conversion
#
# Write a latex table with the info
# Write all latex talbes into the log so you just copypaste that into latex

# You could do something similar with the original variables and renaming
# but looking into lines 1000 to 2000 for example



print("Data cleaning completed!")

# This goes into the Aureus script, make a proper DF and output it into CSV
if(FALSE){
print("Showing some Aureus Statistics")
print("Total people: ")
print(totalOriginalIDs)
print("IDs that are missing: ")
print(totalMissing)
print("Total valid IDs:")
print(originalTotalRows)
print(" ---- SWAB 1 ---- ")
print(" ---- Nose valid")
print(" -------- Yes ")
print(sum(aureusTable$S1_BacterialNasalGrowth == "Yes"))
print(" -------- No, Unknown, Non-applicable ")
print(sum(aureusTable$S1_BacterialNasalGrowth != "Yes"))
print(" ------------ Enrichment broth ")
print(" ---------------- Positive: ")
print(sum(aureusTable$S1_Enrich_CoagulaseNasal == "Positive"))
print(" ---------------- Negative, Non-applicable, Unknown: ")
print(sum(aureusTable$S1_Enrich_CoagulaseNasal != "Positive"))    
}

# HEATMAP CODE BRING IT SOMEWHERE ELSE
if(FALSE){
        # Add the heatmaps that shows if there is inconsistencies with respect the
    # coagulase/enrichment test and the population grow
    
    # ---- Give a factor order so the heatmaps shows nicely
    aureusTable$NasalGrowth           = factor(aureusTable$NasalGrowth          , levels = c("Non-applicable", "Yes", "No", "Unknown"))
    aureusTable$ThroatGrowth          = factor(aureusTable$ThroatGrowth         , levels = c("Non-applicable", "Yes", "No", "Unknown"))
    aureusTable$NasalPopulation       = factor(aureusTable$NasalPopulation      , levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
    aureusTable$ThroatPopulation      = factor(aureusTable$ThroatPopulation     , levels = c("None", "Light", "Moderate", "Rich", "Non-applicable"))
    aureusTable$CoagulaseNasal        = factor(aureusTable$CoagulaseNasal       , levels = c("Non-applicable", "Negative", "Positive", "Unknown"))
    aureusTable$CoagulaseThroat       = factor(aureusTable$CoagulaseThroat      , levels = c("Non-applicable", "Negative", "Positive", "Unknown"))
    aureusTable$CoagulaseEnrichNasal  = factor(aureusTable$CoagulaseEnrichNasal , levels = c("Non-applicable", "Negative", "Positive", "Unknown"))
    aureusTable$CoagulaseEnrichThroat = factor(aureusTable$CoagulaseEnrichNasal , levels = c("Non-applicable", "Negative", "Positive", "Unknown"))
    
    # 
    # ---- Do the actual heatmaps
    {
      # Coagulase
      {
        doCategoricalHeatmap(aureusTable, 7, 13, LOGS_FOLDER,
                             plotTitle    = "(NASAL) Comparing SA population with respect the coagulase test",
                             plotXLabel   = "S.A. Population grow in the nose", plotYLabel = "Coagulase test in the nose",
                             overrideCaption = "Absolute frequency of population grow vs coagulase test in the nose. This image shows
                                            that both tests are consistence with each other. We only consider positive nasal carrier
                                            those with positive coagulase test."  )
        
        doCategoricalHeatmap(aureusTable, 8, 14, LOGS_FOLDER,
                             plotTitle    = "(THROAT) Comparing SA population with respect the coagulase test",
                             plotXLabel   = "S.A. Population grow in the throat", plotYLabel = "Coagulase test in the throat",
                             overrideCaption = "Absolute frequency of population grow vs coagulase test in the throat. This image shows
                                            that both tests are consistence with each other. We only consider positive throat carrier
                                            those with positive coagulase test.")
        
        doCategoricalHeatmap(aureusTable, 3, 13, LOGS_FOLDER,
                             plotTitle    = "(NASAL) Comparing SA swab with respect the coagulase test",
                             plotXLabel   = "S.A. swab collected in the nose.", plotYLabel = "Coagulase test in the nose",
                             overrideCaption = "Absolute frequency of swabs vs coagulase test in the nose. We consider carriers those
                                            which have nasal positive coagulase and with a nasal swab collected")
        
        doCategoricalHeatmap(aureusTable, 4, 14, LOGS_FOLDER,
                             plotTitle    = "(THROAT) Comparing SA swab with respect the coagulase test",
                             plotXLabel   = "S.A. swab collected in the throat.", plotYLabel = "Coagulase test in the throat",
                             overrideCaption = "Absolute frequency of swabs vs coagulase test in the nose. We consider carriers those
                                            which have throat positive coagulase and with a throat swab collected")
        
      }
      
      # Enrichment
      {
        doCategoricalHeatmap(aureusTable, 7, 15, LOGS_FOLDER,
                             plotTitle    = "(NASAL) Comparing SA population with respect the enrichment test",
                             plotXLabel   = "S.A. Population grow in the nose", plotYLabel = "Enrichment test in the nose",
                             overrideCaption = "Absolute frequency of population grow vs enrichment test in the nose. This image shows
                                            that both tests are consistence with each other. We only consider positive nasal carrier
                                            those with positive enrichment test."  )
        
        doCategoricalHeatmap(aureusTable, 8, 16, LOGS_FOLDER,
                             plotTitle    = "(THROAT) Comparing SA population with respect the enrichment test",
                             plotXLabel   = "S.A. Population grow in the throat", plotYLabel = "Enrichment test in the throat",
                             overrideCaption = "Absolute frequency of population grow vs enrichment test in the throat. This image shows
                                            that both tests are consistence with each other. We only consider positive throat carrier
                                            those with positive enrichment test.")
        
        doCategoricalHeatmap(aureusTable, 3, 15, LOGS_FOLDER,
                             plotTitle    = "(NASAL) Comparing SA swab with respect the enrichment test",
                             plotXLabel   = "S.A. swab collected in the nose.", plotYLabel = "Enrichment test in the nose",
                             overrideCaption = "Absolute frequency of swabs vs enrichment test in the nose. We consider carriers those
                                            which have nasal positive enrichment and with a nasal swab collected")
        
        doCategoricalHeatmap(aureusTable, 4, 16, LOGS_FOLDER,
                             plotTitle    = "(THROAT) Comparing SA swab with respect the enrichment test",
                             plotXLabel   = "S.A. swab collected in the throat.", plotYLabel = "Enrichment test in the throat",
                             overrideCaption = "Absolute frequency of swabs vs enrichment test in the nose. We consider carriers those
                                            which have throat positive enrichment and with a throat swab collected")
        
      }
      
    }
}
