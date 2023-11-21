#############################################################################

#                                      !

# We need to rewrite this completely using the metadata info instead of each
# table one by one.

# For now, since there is no filtering, just copypaste the files from one
# folder to another

#                                      !

#############################################################################



# In this script we take the ready to read .csv data and create another ready
# to read data which is filtered by whatever standards we deem necessary. Later
# on, we load this filtered data instead.


# Set working directory to file location and load libraries
# -----------------------------------------------------------------------------
{
  #this.dir = dirname(parent.frame(2)$ofile)
  #setwd(this.dir)
  
    #source(paste0(MAIN_PROJECT_FOLDER,"src/constants.R"),   encoding="utf-8")
    source(paste0(MAIN_PROJECT_FOLDER,"src/lib/analysis.R"), encoding="utf-8")    
  
}

# Init the log
# -----------------------------------------------------------------------------
{
  logTXTFileConnection = file(FILTERING_LOG_PATH, 'w')
  logLine              = paste( "FILTERING DATA LOG at: ", RIGHT_NOW, sep = "")
  write( logLine ,
         file = logTXTFileConnection,
         append = FALSE)
  
  
  dataFilteringLogDF            = data.frame(matrix(NA, nrow = 2, ncol = 2))
  colnames(dataFilteringLogDF)  = c("Concept", "Total")
  dataFilteringLogDF$Concept[1] = "By age restrictions:"
  dataFilteringLogDF$Concept[2] = "By missing information:"
}

# Select the filtering variables
# All numeric filters are apply to equal or smaller, or equal or greater where applicable
{

    # By age
    ageMinimum = 0
    ageMaximum = 17
        
}


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
    
    
    if(FALSE){
    # Individual tables and complete table
    basicTable           = read.csv2(READY_DATA_BASIC_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    antropometricTable   = read.csv2(READY_DATA_ANTROPOMETRIC_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    menstruationTable    = read.csv2(READY_DATA_MENSTRUATION_FILEPATH,    fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    aureusTable          = read.csv2(READY_DATA_AUREUS_FILEPATH,          fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    highSchoolTable      = read.csv2(READY_DATA_HIGHSCHOOL_FILEPATH,      fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    bloodTable           = read.csv2(READY_DATA_BLOOD_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    questionariesTable   = read.csv2(READY_DATA_QUESTIONARIES_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sociologyTable       = read.csv2(READY_DATA_SOCIOLOGY_FILEPATH,       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    frienshipTable       = read.csv2(READY_DATA_FRIENSHIP_FILEPATH,       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    pubertyMenTable      = read.csv2(READY_DATA_PUBERTYMEN_FILEPATH,      fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    pubertyWomenTable    = read.csv2(READY_DATA_PUBERTYWOMEN_FILEPATH,    fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    drugsTable           = read.csv2(READY_DATA_DRUGS_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sportsTable          = read.csv2(READY_DATA_SPORTS_FILEPATH,          fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    hygieneTable         = read.csv2(READY_DATA_HYGIENE_FILEPATH,         fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    hospitalizationTable = read.csv2(READY_DATA_HOSPITALIZATION_FILEPATH, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    biomarkersTable      = read.csv2(READY_DATA_BIOMARKERS_FILEPATH,      fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    dietTable            = read.csv2(READY_DATA_DIET_FILEPATH,            fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sleepTable           = read.csv2(READY_DATA_SLEEP_FILEPATH,           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    
    # DBs tables  
    diseasesDBDF         = read.csv2(READY_DATA_DISEASES_FILEPATH,        fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    contraceptivesDBDF   = read.csv2(READY_DATA_CONTRACEPTIVES_FILEPATH,  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    medicinesDBDF        = read.csv2(READY_DATA_MEDICINES_FILEPATH,       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    
    # Networks
    overallNetworkDF  = read.csv2(READY_DATA_OVERALL_NETWORK_FILEPATH,    fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    physicalNetworkDF = read.csv2(READY_DATA_PHYSICAL_NETWORK_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    homeNetworkDF     = read.csv2(READY_DATA_HOME_NETWORK_FILEPATH,       fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    schoolNetworkDF   = read.csv2(READY_DATA_SCHOOL_NETWORK_FILEPATH,     fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    sportsNetworkDF   = read.csv2(READY_DATA_SPORTS_NETWORK_FILEPATH,     fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    otherNetworkDF    = read.csv2(READY_DATA_OTHERS_NETWORK_FILEPATH,     fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
    # Notice that technically we don't need the network table anymore, but it is
    # easier to do the filtering with this table, rather than each of those
    # individual networkDFs, specially when it comes to counting how many
    # friends each person has for the friendship table.
    networkTable      = read.csv2(READY_DATA_NETWORK_FILEPATH,   fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    }
    
    
    totalOriginalRows = nrow(basicTable)
}

# Fix the column names in each network table
# So it goes from "X851" to "851"
# ( T 3 T ) R, whyyyy are you doing this... C++ I miss you!
# -----------------------------------------------------------------------------
{
  totalRows = nrow(overallNetworkDF)

  colnames(overallNetworkDF)  = c(c(1:totalRows),"ID")
  colnames(physicalNetworkDF) = c(c(1:totalRows),"ID")
  colnames(homeNetworkDF)     = c(c(1:totalRows),"ID")
  colnames(schoolNetworkDF)   = c(c(1:totalRows),"ID")
  colnames(sportsNetworkDF)   = c(c(1:totalRows),"ID")
  colnames(otherNetworkDF)    = c(c(1:totalRows),"ID")
}


# -----------------------------------------------------------------------------
# ALL FILTERS ARE APPLY HERE
# -----------------------------------------------------------------------------
{
    # Filter by age
    {
    
        ageMinimum = 0
        ageMaximum = 99
      
        # R is horrible, seriously, why do you even have & and && operators to confuse people
        # When if ever are you going to use the && to reduce a vector to a bool, without explicitly saying that?
        # And why in the name of everything that is holly you screw up with the previous standard and change && to &?
        rowsByAge  = (basicTable$Age >= ageMinimum) & (basicTable$Age <= ageMaximum)
        
        totalLostByAge = sum(rowsByAge)
    
        dataFilteringLogDF$Total[1] = totalLostByAge
    
    }
}


# -----------------------------------------------------------------------------
# RECONSTRUCT THE TABLES BASED ON THE INFO WE LOST
# -----------------------------------------------------------------------------
{
    # IDs that are going to be deleted.
    # If no ID is going to be deleted, that's fine and we will save a lot of 
    # time. If there is at least one ID to be deleted, we need to reconstruct
    # the entire network information as we need to rebuild all the indexes so
    # they stay consecutive.
    {
        keepTheseRows   = rowsByAge        # a|b
        invalidIDs      = basicTable[!keepTheseRows,]$ID
        totalInvalidIDs = length(invalidIDs)  
    } 
 
    # Remake all the basic tables. It doesn't matter if any ID is deleted or not
    # This code will just adjust the tables accordingly for both cases.
    {

        NEWbasicTable           = basicTable[keepTheseRows,]
        NEWantropometricTable   = antropometricTable[keepTheseRows,]
        NEWmenstruationTable    = menstruationTable[keepTheseRows,]
        NEWaureusTable          = aureusTable[keepTheseRows,]
        NEWhighSchoolTable      = highSchoolTable[keepTheseRows,]
        NEWbloodTable           = bloodTable[keepTheseRows,]
        NEWquestionariesTable   = questionariesTable[keepTheseRows,]
        NEWsociologyTable       = sociologyTable[keepTheseRows,]
        NEWfrienshipTable       = frienshipTable[keepTheseRows,]
        NEWpubertyMenTable      = pubertyMenTable[keepTheseRows,]
        NEWpubertyWomenTable    = pubertyWomenTable[keepTheseRows,]
        NEWdrugsTable           = drugsTable[keepTheseRows,]
        NEWsportsTable          = sportsTable[keepTheseRows,]
        NEWhygieneTable         = hygieneTable[keepTheseRows,]
        NEWhospitalizationTable = hospitalizationTable[keepTheseRows,]
        NEWbiomarkersTable      = biomarkersTable[keepTheseRows,]
        NEWdietTable            = dietTable[keepTheseRows,]
        NEWsleepTable           = sleepTable[keepTheseRows,]
        
    }
       
    # Everything else stay the same as default. We will delete the invalid rows
    # later on.
    
    # Network tables
    {
        # Again, we don't need this table, but it get things done easier.
        NEWnetworkTable      =  networkTable[keepTheseRows,]
        
        NEWoverallNetworkDF  = overallNetworkDF
        NEWphysicalNetworkDF = physicalNetworkDF
        NEWhomeNetworkDF     = homeNetworkDF
        NEWschoolNetworkDF   = schoolNetworkDF
        NEWsportsNetworkDF   = sportsNetworkDF
        NEWotherNetworkDF    = otherNetworkDF    
    }
    
    # Relational tables
    NEWmedicinesDBDF         = medicinesDBDF
    NEWcontraceptivesDBDF    = contraceptivesDBDF
    NEWdiseasesDBDF          = diseasesDBDF
  
    
    totalIDs = nrow(NEWbasicTable)
    
    # Finally, check if we miss any data, and if so, rebuild all tables and all
    # IDs accordingly. Otherwise, we just skip all this hassle as if nothing had
    # happen.
    if(totalInvalidIDs > 0){
        
        
        # ----------------------------------------------------------------------------
        # From here we do the same as in data cleaning, only now we have a "new" table
        # ----------------------------------------------------------------------------
        {
            
            # (This {} section should be indented one level down)

            # Keep track of all key changes
            replacementKeysDF           = data.frame(matrix(NA, nrow = totalIDs, ncol = 2))
            colnames(replacementKeysDF) = c("Old_Key", "New_Key")
    
            # For each existing ID, keep track of how many friends are missing
            missingFriendsDF            = data.frame(matrix(NA, nrow = totalInvalidIDs, ncol = 7))
            colnames(missingFriendsDF)  = c("Key", "Popularity", "Nominations", "Reciprocal", "Internal Popularity", "Internal Nominations", "Internal Reciprocal")            
            
            # We start by the invalid IDs that will change to 0
            # Later on, we replace the old keys to new keys, that coincidentally might get
            # the same ID as what we call invalid ID now, but that is just a coincidence,
            # and by the time that happens, it will be no possible confusion anymore.
    
            # Prepare the log for writing the keys deletions
            {
                logLine = paste("These keys were deleted in the filtering process: ")
                write( logLine , file   = logTXTFileConnection, append = TRUE )
                logLine = paste("    ----    ")
                write( logLine , file   = logTXTFileConnection, append = TRUE )
            }            
            
            # For each invalid ID, delete that ID from the friendship matrix, and keep
            # track of everything that was deleted for logging purposes.
            for(i in 1:totalInvalidIDs){

                # Save the current key
                replacementKey = invalidIDs[i]
      
                # Write the current key in the log
                logLine = paste("    ", replacementKey)
                write( logLine , file   = logTXTFileConnection, append = TRUE )
      
                # ---- Each one of the friends that now are loosing a friend
                #      For each of the IDs (i) replace it in the entire row for 0
                #
                #      Again, we have no idea in which order this might be written, so we
                #      need to do it not very efficiently and replace the entire column.
                #      (I hate you R, and your lack of pointers)
                {
                    NEWnetworkTable$Friend1 = replace(NEWnetworkTable$Friend1, NEWnetworkTable$Friend1 == replacementKey, 0)
                    NEWnetworkTable$Friend2 = replace(NEWnetworkTable$Friend2, NEWnetworkTable$Friend2 == replacementKey, 0)
                    NEWnetworkTable$Friend3 = replace(NEWnetworkTable$Friend3, NEWnetworkTable$Friend3 == replacementKey, 0)
                    NEWnetworkTable$Friend4 = replace(NEWnetworkTable$Friend4, NEWnetworkTable$Friend4 == replacementKey, 0)
                    NEWnetworkTable$Friend5 = replace(NEWnetworkTable$Friend5, NEWnetworkTable$Friend5 == replacementKey, 0)      
                }
      
                # Update the rest of the information if needed in the networks
                {
                    # -- Physical
                    NEWnetworkTable$Friend1Physical = replace(NEWnetworkTable$Friend1Physical, NEWnetworkTable$Friend1 == 0, 0)
                    NEWnetworkTable$Friend2Physical = replace(NEWnetworkTable$Friend2Physical, NEWnetworkTable$Friend2 == 0, 0)
                    NEWnetworkTable$Friend3Physical = replace(NEWnetworkTable$Friend3Physical, NEWnetworkTable$Friend3 == 0, 0)
                    NEWnetworkTable$Friend4Physical = replace(NEWnetworkTable$Friend4Physical, NEWnetworkTable$Friend4 == 0, 0)
                    NEWnetworkTable$Friend5Physical = replace(NEWnetworkTable$Friend5Physical, NEWnetworkTable$Friend5 == 0, 0)
                    # -- School
                    NEWnetworkTable$Friend1School   = replace(NEWnetworkTable$Friend1School, NEWnetworkTable$Friend1 == 0, 0)
                    NEWnetworkTable$Friend2School   = replace(NEWnetworkTable$Friend2School, NEWnetworkTable$Friend2 == 0, 0)
                    NEWnetworkTable$Friend3School   = replace(NEWnetworkTable$Friend3School, NEWnetworkTable$Friend3 == 0, 0)
                    NEWnetworkTable$Friend4School   = replace(NEWnetworkTable$Friend4School, NEWnetworkTable$Friend4 == 0, 0)
                    NEWnetworkTable$Friend5School   = replace(NEWnetworkTable$Friend5School, NEWnetworkTable$Friend5 == 0, 0)
                    # -- Sport
                    NEWnetworkTable$Friend1Sport    = replace(NEWnetworkTable$Friend1Sport, NEWnetworkTable$Friend1 == 0, 0)
                    NEWnetworkTable$Friend2Sport    = replace(NEWnetworkTable$Friend2Sport, NEWnetworkTable$Friend2 == 0, 0)
                    NEWnetworkTable$Friend3Sport    = replace(NEWnetworkTable$Friend3Sport, NEWnetworkTable$Friend3 == 0, 0)
                    NEWnetworkTable$Friend4Sport    = replace(NEWnetworkTable$Friend4Sport, NEWnetworkTable$Friend4 == 0, 0)
                    NEWnetworkTable$Friend5Sport    = replace(NEWnetworkTable$Friend5Sport, NEWnetworkTable$Friend5 == 0, 0)
                    # -- Home
                    NEWnetworkTable$Friend1Home     = replace(NEWnetworkTable$Friend1Home, NEWnetworkTable$Friend1 == 0, 0)
                    NEWnetworkTable$Friend2Home     = replace(NEWnetworkTable$Friend2Home, NEWnetworkTable$Friend2 == 0, 0)
                    NEWnetworkTable$Friend3Home     = replace(NEWnetworkTable$Friend3Home, NEWnetworkTable$Friend3 == 0, 0)
                    NEWnetworkTable$Friend4Home     = replace(NEWnetworkTable$Friend4Home, NEWnetworkTable$Friend4 == 0, 0)
                    NEWnetworkTable$Friend5Home     = replace(NEWnetworkTable$Friend5Home, NEWnetworkTable$Friend5 == 0, 0)
                    # -- Other
                    NEWnetworkTable$Friend1Other    = replace(NEWnetworkTable$Friend1Other, NEWnetworkTable$Friend1 == 0, 0)
                    NEWnetworkTable$Friend2Other    = replace(NEWnetworkTable$Friend2Other, NEWnetworkTable$Friend2 == 0, 0)
                    NEWnetworkTable$Friend3Other    = replace(NEWnetworkTable$Friend3Other, NEWnetworkTable$Friend3 == 0, 0)
                    NEWnetworkTable$Friend4Other    = replace(NEWnetworkTable$Friend4Other, NEWnetworkTable$Friend4 == 0, 0)
                    NEWnetworkTable$Friend5Other    = replace(NEWnetworkTable$Friend5Other, NEWnetworkTable$Friend5 == 0, 0)    
                }
      
                # Finally, we track down how many connection we lost in this way.
                {
                    myResults        = getFrienshipTypes(replacementKey, overallNetworkDF)
                    myPopularity     = myResults[[1]]
                    myNominations    = myResults[[2]]
                    myReciprocals    = myResults[[3]]
                    myPopularityIDs  = myResults[[4]]
                    myNominationsIDs = myResults[[5]]
                    myReciprocalsIDs = myResults[[6]]      
                }
      
                # Write it in the log the lost connections
                {
                    logLine = paste("        Was popular with: ")
                    write( logLine , file   = logTXTFileConnection, append = TRUE )
                    logLine = paste("        ", myPopularityIDs)
                    write( logLine , file   = logTXTFileConnection, append = TRUE )
                    logLine = paste("        Was nominating: ")
                    write( logLine , file   = logTXTFileConnection, append = TRUE )
                    logLine = paste("        ", myNominationsIDs)
                    write( logLine , file   = logTXTFileConnection, append = TRUE )
                    logLine = paste("        Reciprocals: ")
                    write( logLine , file   = logTXTFileConnection, append = TRUE )
                    logLine = paste("        ", myReciprocalsIDs)
                    write( logLine , file   = logTXTFileConnection, append = TRUE )    
                }
      
                # Write it in the dataframe log the new info
                # This is what we use later to plot the number of missing friends
                missingFriendsDF[i,1] = replacementKey
                missingFriendsDF[i,2] = myPopularity
                missingFriendsDF[i,3] = myNominations
                missingFriendsDF[i,4] = myReciprocals
                missingFriendsDF[i,5] = sum(invalidIDs %in% myPopularityIDs)
                missingFriendsDF[i,6] = sum(invalidIDs %in% myNominationsIDs)
                missingFriendsDF[i,7] = sum(invalidIDs %in% myReciprocalsIDs)
      
                # R is horrible. There is no particular piece of code here to
                # reference, is just that when you are debugging, it doesn't
                # even tell you the line of code where it fails and you need
                # to trace it manually.
                
                
                
                # Update the diseases, drugs, and contraceptive table
                # ---- Drug use
                # counterVariable     = nrow(NEWdrugUseDF)
                # NEWdrugUseDF        = NEWdrugUseDF[NEWdrugUseDF$ID != replacementKey,]
                # counterVariable     = counterVariable - nrow(NEWdrugUseDF)
                # if(counterVariable>0){
                # 
                #     logLine = paste0("        Was using ", counterVariable, " drugs.")
                #     write( logLine , file   = logTXTFileConnection, append = TRUE )
                # 
                # }
      
      
                # # ---- Contraceptive use
                counterVariable       = nrow(NEWcontraceptivesDBDF)
                NEWcontraceptivesDBDF = NEWcontraceptivesDBDF[NEWcontraceptivesDBDF$ID != replacementKey,]
                counterVariable       = counterVariable - nrow(NEWcontraceptivesDBDF)
                if(counterVariable>0){
                   
                   logLine = paste0("        Was using ", counterVariable, " contraceptives.")
                   write( logLine , file   = logTXTFileConnection, append = TRUE )
                   
                }
      
                # ---- Disesases
                counterVariable     = nrow(NEWdiseasesDBDF)
                NEWdiseasesDBDF     = NEWdiseasesDBDF[NEWdiseasesDF$ID != replacementKey,]
                counterVariable     = counterVariable - nrow(NEWdiseasesDBDF)
                if(counterVariable>0){
                
                    logLine = paste0("        Was having ", counterVariable, " diseases.")
                    write( logLine , file   = logTXTFileConnection, append = TRUE )
                
                } 

                
                
            }
            
            # Delete from the databases (WRONG?!?)
            {
                
                #NEWdiseasesDBDF = NEWdiseasesDBDF[NEWdiseasesDBDF$ID %in% invalidIDs,]
                
            }
            
            # At this point, the new tables don't contain any invalid IDs anymore. We
            # just need to replace everything to consecutive IDs again. And recount how
            # many friends and popularity each person have.            
            
         
            # Init the log for writing the keys replacements
            {
                logLine = paste("These keys replaced, from post cleaning ID to their new post filter ID: ")
                write( logLine , file   = logTXTFileConnection, append = TRUE )
                logLine = paste("    ----    ")
                write( logLine , file   = logTXTFileConnection, append = TRUE )
            }
    
            # For each valid ID...
            for(i in 1:totalIDs){

                # Save the current key
                replacementKey               = NEWbasicTable$ID[i]

                # Set the new key in every basic table
                #
                #    (take into account that different tables might be sorted differently)
                #    (the default sorting is the basic table, we assign consecutive IDs there)
                #
                {
                                      
                    # R is horrible. It doesn't precompile to tell you that you
                    # are assigning the wrong value type to the an specific
                    # variable type, so you ned to waste 20min tracking a bug
                    # that doesn't shouldn't even exist in the first place.
                    
                    NEWbasicTable$ID[i]        = i
                    NEWantropometricTable$ID   = replace(NEWantropometricTable$ID,   NEWantropometricTable$ID    == replacementKey, i)
                    NEWmenstruationTable$ID    = replace(NEWmenstruationTable$ID,    NEWmenstruationTable$ID     == replacementKey, i)
                    NEWaureusTable$ID          = replace(NEWaureusTable$ID,          NEWaureusTable$ID           == replacementKey, i)
                    NEWhighSchoolTable$ID      = replace(NEWhighSchoolTable$ID,      NEWhighSchoolTable$ID       == replacementKey, i)
                    NEWbloodTable$ID           = replace(NEWbloodTable$ID,           NEWbloodTable$ID            == replacementKey, i)
                    #NEWquestionariesTable$ID   = replace(NEWquestionariesTable$ID,   NEWquestionariesTable$ID    == replacementKey, i)
                    #NEWsociologyTable$ID       = replace(NEWsociologyTable$ID,       NEWsociologyTable$ID        == replacementKey, i)
                    NEWfrienshipTable$ID       = replace(NEWfrienshipTable$ID,       NEWfrienshipTable$ID        == replacementKey, i)
                    #NEWpubertyMenTable$ID      = replace(NEWpubertyMenTable$ID,      NEWpubertyMenTable$ID       == replacementKey, i)
                    #NEWpubertyWomenTable$ID    = replace(NEWpubertyWomenTable$ID,    NEWpubertyWomenTable$ID     == replacementKey, i)
                    NEWdrugsTable$ID           = replace(NEWdrugsTable$ID,           NEWdrugsTable$ID            == replacementKey, i)
                    NEWsportsTable$ID          = replace(NEWsportsTable$ID,          NEWsportsTable$ID           == replacementKey, i)
                    #NEWhygieneTable$ID         = replace(NEWhygieneTable$ID,         NEWhygieneTable$ID          == replacementKey, i)
                    #NEWhospitalizationTable$ID = replace(NEWhospitalizationTable$ID, NEWhospitalizationTable$ID  == replacementKey, i)
                    NEWbiomarkersTable$ID      = replace(NEWbiomarkersTable$ID,      NEWbiomarkersTable$ID       == replacementKey, i)
                    NEWdietTable$ID            = replace(NEWdietTable$ID,            NEWdietTable$ID             == replacementKey, i)
                    NEWsleepTable$ID           = replace(NEWsleepTable$ID,           NEWsleepTable$ID            == replacementKey, i)
                    
                }

                # Set the new key in every network table
      
                # -- Network table
                # ---- IDs
                NEWnetworkTable$ID = replace(NEWnetworkTable$ID, NEWnetworkTable$ID == replacementKey, i)
                # ---- Each one of the friends
                NEWnetworkTable$Friend1 = replace(NEWnetworkTable$Friend1, NEWnetworkTable$Friend1 == replacementKey, i)
                NEWnetworkTable$Friend2 = replace(NEWnetworkTable$Friend2, NEWnetworkTable$Friend2 == replacementKey, i)
                NEWnetworkTable$Friend3 = replace(NEWnetworkTable$Friend3, NEWnetworkTable$Friend3 == replacementKey, i)
                NEWnetworkTable$Friend4 = replace(NEWnetworkTable$Friend4, NEWnetworkTable$Friend4 == replacementKey, i)
                NEWnetworkTable$Friend5 = replace(NEWnetworkTable$Friend5, NEWnetworkTable$Friend5 == replacementKey, i)

                # Each of the relational tables
                # ---- The disease table
                NEWdiseasesDBDF$ID        = replace(NEWdiseasesDBDF$ID, NEWdiseasesDBDF$ID == replacementKey, i)
                # ---- The medicine table
                #NEWdrugUseDF$ID         = replace(NEWdrugUseDF$ID,        NEWdrugUseDF$ID        == replacementKey, i)
                # ---- The contraceptive table
                NEWcontraceptivesDBDF$ID  = replace(NEWcontraceptivesDBDF$ID, NEWcontraceptivesDBDF$ID == replacementKey, i)
      
      
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
               
            
            # ----------------------------------------------------------------------------
            # We need to create the 6 network from scratch again, same as in data cleaning
            # ----------------------------------------------------------------------------
            {
    
                # At this point we have the NEWnetworkTable ready.
                #
                # Remember that:
                #
                # In Friend1, Friend2,...Friend 5 we have an ID
                # In Friend1Phyisical, Friend2Physical... we have a boolean.
    
    
                # ---- Transform the network table into proper frienship matrix
                #      There are 6 graphs, all of them directed.
                #      -- Overall friendship
                #      -- Physical
                #      -- Home
                #      -- School
                #      -- Sports
                #      -- Others
    
                # Create the skeleton dataframe for each network
                {
                    # Check all the IDs
                    NEWnetworkIDs = NEWbasicTable$ID
                    totalIDs      = length(NEWnetworkIDs)
      
                    # Create the basic DF from where later we will create the edges data frames
                    NEWoverallNetworkDF    = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
                    NEWphysicalNetworkDF   = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
                    NEWhomeNetworkDF       = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
                    NEWschoolNetworkDF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
                    NEWsportsNetworkDF     = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
                    NEWotherNetworkDF      = data.frame(matrix(0, nrow = totalIDs, ncol = totalIDs))
      
                    # Ensure that the rows and the columns refers to the same ID; for each of the networks we have
                    # -- Overall
                    NEWoverallNetworkDF$ID                     = NEWnetworkIDs
                    colnames(NEWoverallNetworkDF)[1:totalIDs]  = NEWnetworkIDs
                    # -- Physical
                    NEWphysicalNetworkDF$ID                    = NEWnetworkIDs
                    colnames(NEWphysicalNetworkDF)[1:totalIDs] = NEWnetworkIDs
                    # -- Home
                    NEWhomeNetworkDF$ID                        = NEWnetworkIDs
                    colnames(NEWhomeNetworkDF)[1:totalIDs]     = NEWnetworkIDs
                    # -- School
                    NEWschoolNetworkDF$ID                      = NEWnetworkIDs
                    colnames(NEWschoolNetworkDF)[1:totalIDs]   = NEWnetworkIDs
                    # -- Sports
                    NEWsportsNetworkDF$ID                      = NEWnetworkIDs
                    colnames(NEWsportsNetworkDF)[1:totalIDs]   = NEWnetworkIDs
                    # -- Others
                    NEWotherNetworkDF$ID                       = NEWnetworkIDs
                    colnames(NEWotherNetworkDF)[1:totalIDs]    = NEWnetworkIDs
      
                }
                
                # For each of the people in the network
                # Fill all their edges information
                for(i in 1:totalIDs){
      
                    # Identify each of your friends
                    FriendA = NEWnetworkTable$Friend1[i]
                    FriendB = NEWnetworkTable$Friend2[i]
                    FriendC = NEWnetworkTable$Friend3[i]
                    FriendD = NEWnetworkTable$Friend4[i]
                    FriendE = NEWnetworkTable$Friend5[i]
      
                    # For each of the network, add that friendship link if it exist
                    # -- The overall network doesn't care about anything, everyone comes here
                    if(FriendA > 0) NEWoverallNetworkDF[i, FriendA] = 1
                    if(FriendB > 0) NEWoverallNetworkDF[i, FriendB] = 1
                    if(FriendC > 0) NEWoverallNetworkDF[i, FriendC] = 1
                    if(FriendD > 0) NEWoverallNetworkDF[i, FriendD] = 1
                    if(FriendE > 0) NEWoverallNetworkDF[i, FriendE] = 1
                    # -- Physical
                    if(FriendA > 0 && NEWnetworkTable$Friend1Physical[i] == 1) NEWphysicalNetworkDF[i, FriendA] = 1
                    if(FriendB > 0 && NEWnetworkTable$Friend2Physical[i] == 1) NEWphysicalNetworkDF[i, FriendB] = 1
                    if(FriendC > 0 && NEWnetworkTable$Friend3Physical[i] == 1) NEWphysicalNetworkDF[i, FriendC] = 1
                    if(FriendD > 0 && NEWnetworkTable$Friend4Physical[i] == 1) NEWphysicalNetworkDF[i, FriendD] = 1
                    if(FriendE > 0 && NEWnetworkTable$Friend5Physical[i] == 1) NEWphysicalNetworkDF[i, FriendE] = 1
                    # -- Home
                    if(FriendA > 0 && NEWnetworkTable$Friend1Home[i] == 1) NEWhomeNetworkDF[i, FriendA] = 1
                    if(FriendB > 0 && NEWnetworkTable$Friend2Home[i] == 1) NEWhomeNetworkDF[i, FriendB] = 1
                    if(FriendC > 0 && NEWnetworkTable$Friend3Home[i] == 1) NEWhomeNetworkDF[i, FriendC] = 1
                    if(FriendD > 0 && NEWnetworkTable$Friend4Home[i] == 1) NEWhomeNetworkDF[i, FriendD] = 1
                    if(FriendE > 0 && NEWnetworkTable$Friend5Home[i] == 1) NEWhomeNetworkDF[i, FriendE] = 1
                    # -- School
                    if(FriendA > 0 && NEWnetworkTable$Friend1School[i] == 1) NEWschoolNetworkDF[i, FriendA] = 1
                    if(FriendB > 0 && NEWnetworkTable$Friend2School[i] == 1) NEWschoolNetworkDF[i, FriendB] = 1
                    if(FriendC > 0 && NEWnetworkTable$Friend3School[i] == 1) NEWschoolNetworkDF[i, FriendC] = 1
                    if(FriendD > 0 && NEWnetworkTable$Friend4School[i] == 1) NEWschoolNetworkDF[i, FriendD] = 1
                    if(FriendE > 0 && NEWnetworkTable$Friend5School[i] == 1) NEWschoolNetworkDF[i, FriendE] = 1
                    # -- Sports
                    if(FriendA > 0 && NEWnetworkTable$Friend1Sport[i] == 1) NEWsportsNetworkDF[i, FriendA] = 1
                    if(FriendB > 0 && NEWnetworkTable$Friend2Sport[i] == 1) NEWsportsNetworkDF[i, FriendB] = 1
                    if(FriendC > 0 && NEWnetworkTable$Friend3Sport[i] == 1) NEWsportsNetworkDF[i, FriendC] = 1
                    if(FriendD > 0 && NEWnetworkTable$Friend4Sport[i] == 1) NEWsportsNetworkDF[i, FriendD] = 1
                    if(FriendE > 0 && NEWnetworkTable$Friend5Sport[i] == 1) NEWsportsNetworkDF[i, FriendE] = 1
                    # -- Others
                    if(FriendA > 0 && NEWnetworkTable$Friend1Other[i] == 1) NEWotherNetworkDF[i, FriendA] = 1
                    if(FriendB > 0 && NEWnetworkTable$Friend2Other[i] == 1) NEWotherNetworkDF[i, FriendB] = 1
                    if(FriendC > 0 && NEWnetworkTable$Friend3Other[i] == 1) NEWotherNetworkDF[i, FriendC] = 1
                    if(FriendD > 0 && NEWnetworkTable$Friend4Other[i] == 1) NEWotherNetworkDF[i, FriendD] = 1
                    if(FriendE > 0 && NEWnetworkTable$Friend5Other[i] == 1) NEWotherNetworkDF[i, FriendE] = 1
      
                }
    
                # Now that you have the edges, you can initialize the edge
                # counting in the frienship table
                for(i in 1:totalIDs){
      
                    # Print the progress bar
                    print(getProgressCharacters((100*i)/totalIDs))
                    print("")
                    print("Initializing networks, please wait...")
      
                    # -- For the overall
                    {
                        
                        # Get the list of everyone that likes me
                        popularThisIDsOverall                   = NEWoverallNetworkDF[ (NEWoverallNetworkDF[,i] == 1), ]$ID
                        NEWfrienshipTable$OverallPopularity[i]  = length(popularThisIDsOverall)
                        
                        # Get the list of everyone I like
                        followingThisIDsOverall                 = NEWoverallNetworkDF[NEWoverallNetworkDF[i,1:totalIDs] == 1,]$ID
                        NEWfrienshipTable$OverallFollowing [i]  = length(followingThisIDsOverall)
                        
                        # Check the union of both sets for all relationship
                        allRelationshipsOverall                 = union( popularThisIDsOverall, followingThisIDsOverall )
                        totalAllRelationshipsOverall            = length(allRelationshipsOverall)
                        NEWfrienshipTable$OverallConnections[i] = totalAllRelationshipsOverall
                        
                        # Check the intersection of both sets for reciprocity
                        realciprocallRelationshipsOverall       = intersect( popularThisIDsOverall, followingThisIDsOverall )
                        NEWfrienshipTable$OverallReciprocity[i] = length(realciprocallRelationshipsOverall)
                        
                    }
      
                    # -- Physical
                    {
                        
                        popularThisIDs                           = NEWphysicalNetworkDF[    (NEWphysicalNetworkDF[,i] == 1), ]$ID
                        NEWfrienshipTable$PhysicalPopularity[i]  = length(popularThisIDs)
                        followingThisIDs                         = NEWphysicalNetworkDF[NEWphysicalNetworkDF[i,1:totalIDs] == 1,]$ID
                        NEWfrienshipTable$PhysicalFollowing [i]  = length(followingThisIDs)
                        allRelationshipsPhysical                 = union( popularThisIDs, followingThisIDs )
                        totalAllRelationshipsPhysical            = length(allRelationshipsPhysical)
                        NEWfrienshipTable$PhysicalConnections[i] = totalAllRelationshipsPhysical
                        realciprocallRelationships               = intersect( popularThisIDs, followingThisIDs )
                        NEWfrienshipTable$PhysicalReciprocity[i] = length(realciprocallRelationships)
                        
                    }
      
                    # -- Home
                    {
                        
                        popularThisIDs                       = NEWhomeNetworkDF[    (NEWhomeNetworkDF[,i] == 1), ]$ID
                        NEWfrienshipTable$HomePopularity[i]  = length(popularThisIDs)
                        followingThisIDs                     = NEWhomeNetworkDF[NEWhomeNetworkDF[i,1:totalIDs] == 1,]$ID
                        NEWfrienshipTable$HomeFollowing [i]  = length(followingThisIDs)
                        allRelationshipsHome                 = union( popularThisIDs, followingThisIDs )
                        totalAllRelationshipsHome            = length(allRelationshipsHome)
                        NEWfrienshipTable$HomeConnections[i] = totalAllRelationshipsHome
                        realciprocallRelationships           = intersect( popularThisIDs, followingThisIDs )
                        NEWfrienshipTable$HomeReciprocity[i] = length(realciprocallRelationships)
                        
                    }
      
                    # -- School
                    {
                        
                        popularThisIDs                         = NEWschoolNetworkDF[  (NEWschoolNetworkDF[,i] == 1), ]$ID
                        NEWfrienshipTable$SchoolPopularity[i]  = length(popularThisIDs)
                        followingThisIDs                       = NEWschoolNetworkDF[NEWschoolNetworkDF[i,1:totalIDs] == 1,]$ID
                        NEWfrienshipTable$SchoolFollowing [i]  = length(followingThisIDs)
                        allRelationshipsSchool                 = union( popularThisIDs, followingThisIDs )
                        totalAllRelationshipsSchool            = length(allRelationshipsSchool)
                        NEWfrienshipTable$SchoolConnections[i] = totalAllRelationshipsSchool
                        realciprocallRelationships             = intersect( popularThisIDs, followingThisIDs )
                        NEWfrienshipTable$SchoolReciprocity[i] = length(realciprocallRelationships)
                        
                    }
      
                    # -- Sport
                    {
        
                        popularThisIDs                         = NEWsportsNetworkDF[    (NEWsportsNetworkDF[,i] == 1), ]$ID
                        NEWfrienshipTable$SportsPopularity[i]  = length(popularThisIDs)
                        followingThisIDs                       = NEWsportsNetworkDF[NEWsportsNetworkDF[i,1:totalIDs] == 1,]$ID
                        NEWfrienshipTable$SportsFollowing [i]  = length(followingThisIDs)
                        allRelationshipsSports                 = union( popularThisIDs, followingThisIDs )
                        totalAllRelationshipsSports            = length(allRelationshipsSports)
                        NEWfrienshipTable$SportsConnections[i] = totalAllRelationshipsSports
                        realciprocallRelationships             = intersect( popularThisIDs, followingThisIDs )
                        NEWfrienshipTable$SportsReciprocity[i] = length(realciprocallRelationships)
        
                    }
      
                    # -- Others
                    {
                        
                        popularThisIDs                        = NEWotherNetworkDF[    (NEWotherNetworkDF[,i] == 1), ]$ID
                        NEWfrienshipTable$OtherPopularity[i]  = length(popularThisIDs)
                        followingThisIDs                      = NEWotherNetworkDF[NEWotherNetworkDF[i,1:totalIDs] == 1,]$ID
                        NEWfrienshipTable$OtherFollowing [i]  = length(followingThisIDs)
                        allRelationshipsOthers                = union( popularThisIDs, followingThisIDs )
                        totalAllRelationshipsOthers           = length(allRelationshipsOthers)
                        NEWfrienshipTable$OtherConnections[i] = totalAllRelationshipsOthers
                        realciprocallRelationships            = intersect( popularThisIDs, followingThisIDs )
                        NEWfrienshipTable$OtherReciprocity[i] = length(realciprocallRelationships)
                        
                    }
      
                }
    
            }
            
        }
        
    }
    
    # -----------------------------------------------------------------------------  
    # Join tables with proper info together
    # -----------------------------------------------------------------------------
    {
        
        # In the data cleaning script we check that the column names doesn't
        # repeat so there is no conflict in the complete table.
        #
        # Here is not necessary anymore as we haven't add anything new.
        
        
        # Join everything
        NEWcompleteTable = NEWbasicTable     %>% left_join(NEWantropometricTable ,  by="ID")
        NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWmenstruationTable ,   by="ID")
        NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWaureusTable ,         by="ID")
        NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWhighSchoolTable ,     by="ID")
        NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWbloodTable ,          by="ID")
        #NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWquestionariesTable , by="ID")
        #NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWsociologyTable ,     by="ID")
        NEWcompleteTable  = NEWcompleteTable  %>% left_join(NEWfrienshipTable ,     by="ID")
        #NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWpubertyMenTable ,    by="ID")
        #NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWpubertyWomenTable ,  by="ID")
        NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWdrugsTable ,          by="ID")
        NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWsportsTable ,         by="ID")
        #NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWhygieneTable ,         by="ID")
        #NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWhospitalizationTable , by="ID")
        NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWbiomarkersTable ,     by="ID")
        NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWdietTable ,           by="ID")
        NEWcompleteTable = NEWcompleteTable  %>% left_join(NEWsleepTable ,          by="ID")        
                
    }
     
}


# -----------------------------------------------------------------------------
# WRITE THE NEW INFO INTO THE PROPER FILTER FILES
#
#     Write them into the filter place
#     Now the update is finish. Write the CSVs into disk so we don't have to
#     do this over and over again everytime we run the scripts
# -----------------------------------------------------------------------------
{

    # The disease, drug and contraceptives tables
    #write.csv2(medicineDF,       file = READY_DATA_MEDICINES_FILEPATH,           row.names = FALSE)
    write.csv2( NEWcontraceptivesDBDF,    file = FILTER_DATA_CONTRACEPTIVES_FILEPATH,  row.names = FALSE)
    write.csv2( NEWdiseasesDBDF,          file = FILTER_DATA_DISEASES_FILEPATH,        row.names = FALSE)
    write.csv2( NEWmedicinesDBDF,         file = FILTER_DATA_MEDICINES_FILEPATH,       row.names = FALSE)
    
    # The complete table with everything
    write.csv2( NEWcompleteTable        , file = FILTER_DATA_COMPLETE_FILEPATH,        row.names = FALSE)
    
    # Each table individually
    write.csv2( NEWbasicTable           , file = FILTER_DATA_BASIC_FILEPATH,           row.names = FALSE)
    write.csv2( NEWantropometricTable   , file = FILTER_DATA_ANTROPOMETRIC_FILEPATH,   row.names = FALSE)
    write.csv2( NEWmenstruationTable    , file = FILTER_DATA_MENSTRUATION_FILEPATH,    row.names = FALSE)
    write.csv2( NEWaureusTable          , file = FILTER_DATA_AUREUS_FILEPATH,          row.names = FALSE)
    write.csv2( NEWhighSchoolTable      , file = FILTER_DATA_HIGHSCHOOL_FILEPATH,      row.names = FALSE)
    write.csv2( NEWbloodTable           , file = FILTER_DATA_BLOOD_FILEPATH,           row.names = FALSE)
    write.csv2( NEWquestionariesTable   , file = FILTER_DATA_QUESTIONARIES_FILEPATH,   row.names = FALSE)
    write.csv2( NEWsociologyTable       , file = FILTER_DATA_SOCIOLOGY_FILEPATH,       row.names = FALSE)
    write.csv2( NEWfrienshipTable       , file = FILTER_DATA_FRIENDSHIP_FILEPATH,      row.names = FALSE)
    write.csv2( NEWpubertyMenTable      , file = FILTER_DATA_PUBERTYMEN_FILEPATH,      row.names = FALSE)
    write.csv2( NEWpubertyWomenTable    , file = FILTER_DATA_PUBERTYWOMEN_FILEPATH,    row.names = FALSE)
    write.csv2( NEWdrugsTable           , file = FILTER_DATA_DRUGS_FILEPATH,           row.names = FALSE)
    write.csv2( NEWsportsTable          , file = FILTER_DATA_SPORTS_FILEPATH,          row.names = FALSE)
    write.csv2( NEWhygieneTable         , file = FILTER_DATA_HYGIENE_FILEPATH,         row.names = FALSE)
    write.csv2( NEWhospitalizationTable , file = FILTER_DATA_HOSPITALIZATION_FILEPATH, row.names = FALSE)
    write.csv2( NEWbiomarkersTable      , file = FILTER_DATA_BIOMARKERS_FILEPATH,      row.names = FALSE)
    write.csv2( NEWdietTable            , file = FILTER_DATA_DIET_FILEPATH,            row.names = FALSE)
    write.csv2( NEWsleepTable           , file = FILTER_DATA_SLEEP_FILEPATH,           row.names = FALSE)

    # Notice that we don't really need the network table anymore and we could
    # skip saving it. However, is very combinient to have it as it is for the
    # filtering process rather than having to addjust every other networkDF
    # one by one. So we are going to keep it anyway.
    # write.csv2( NEWnetworkTable         , file = FILTER_DATA_NETWORK_FILEPATH,       row.names = FALSE)

    # The overall  friendship matrix
    # The physical friendship matrix
    # The home     friendship matrix
    # The school   friendship matrix
    # The sport    friendship matrix
    # The other    friendship matrix
    
    write.csv2(NEWoverallNetworkDF,  file = FILTER_DATA_OVERALL_NETWORK_FILEPATH   , row.names = FALSE)
    write.csv2(NEWphysicalNetworkDF, file = FILTER_DATA_PHYSICAL_NETWORK_FILEPATH  , row.names = FALSE)
    write.csv2(NEWhomeNetworkDF,     file = FILTER_DATA_HOME_NETWORK_FILEPATH      , row.names = FALSE)
    write.csv2(NEWschoolNetworkDF,   file = FILTER_DATA_SCHOOL_NETWORK_FILEPATH    , row.names = FALSE)
    write.csv2(NEWsportsNetworkDF,   file = FILTER_DATA_SPORTS_NETWORK_FILEPATH    , row.names = FALSE)
    write.csv2(NEWotherNetworkDF,    file = FILTER_DATA_OTHERS_NETWORK_FILEPATH    , row.names = FALSE)

}

# Close the TXT connections
close(logTXTFileConnection)

# Write the Latex tables with respect the data that we missed
writeTableLATEX(dataFilteringLogDF, LOGS_FOLDER, tableCaption = "Summary of lost connections at filtering.")

# Plot the boxplots for missing data
if(totalInvalidIDs != 0){
  myYMaximum = max( max(missingFriendsDF$Popularity) , max(missingFriendsDF$Nominations) )
  
  doBoxPlot(missingFriendsDF, 3, LOGS_FOLDER, 
            colorsVector = COLOR_RED_MED,
            plotTitle    = "Nominations lost during filtering; from people remaining, to dissapeared people.",
            plotSubtitle = "",
            plotXLabel   = "Missing Friends", plotYLabel = "Total",
            plotTheme    = "simple",
            ymin = 0, ymax = myYMaximum,
            overrideCaption = "Boxplot for number of missing edges from existing keys to deleted keys during filtering.")
  
  doBoxPlot(missingFriendsDF, 2, LOGS_FOLDER, 
            colorsVector = COLOR_BLUE_MED,
            plotTitle    = "Popularity of people that have dissapear during filtering.",
            plotSubtitle = "",
            plotXLabel   = "Missing Friends", plotYLabel = "Total",
            plotTheme    = "simple",
            ymin = 0, ymax = myYMaximum,
            overrideCaption = "Boxplot for number of missing edges by existing keys to deleted keys during filtering.")
  
  
}

print("Data filtering completed!")

if(totalInvalidIDs == 0) print("No filter was necessary!")




