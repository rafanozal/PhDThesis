# This script prepares the basic pre-requisites about data and tables for the
# rest of the papers.
library(ggpubr)

# ---- Plotting libs
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingBoxplots.R"),   encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingBarplots.R"),   encoding="utf-8")
# ---- Plotting latex
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/latex/toolsLatex.R"),                 encoding="utf-8")





#---------------------------------
# Init constants and indexes
#---------------------------------
{
 
    # How many simulations in the network analysis
    TOTAL_SIMULATIONS = 10
    
    # Get how many patients we have
    TOTAL_PEOPLE = nrow(completeTable)
    
    # Local filepaths
    # -- LOD vs NDL
    LOD_STATISTICS_FILEPATH          = paste0(BIOMARKERS_FOLDER_TABLES,"LODStats.csv")
    # -- Prevalences
    PREVALENCES_TABLE_FILEPATH       = paste0(BIOMARKERS_FOLDER_TABLES,"prevalences.csv")
    # -- Averages
    AVERAGES_NDL_TABLE_FILEPATH      = paste0(BIOMARKERS_FOLDER_TABLES,"averagesNDL.csv")
    AVERAGES_BMI_NDL_TABLE_FILEPATH  = paste0(BIOMARKERS_FOLDER_TABLES,"averagesBMINDL.csv")
    # -- Scatter plots summaries
    # ---- Simple
    SCATTER_SIMPLE_FILEPATH          = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK,"simple_myVSfriendBiomarkersRounded.csv")
    SCATTER_SIMPLE_ROUNDED_FILEPATH  = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK,"simple_myVSfriendBiomarkersRAW.csv")
    # ---- HS
    # ---- HS + SEX
    SCATTER_STRATOS_FILEPATH         = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK,"complex_myVSfriendBiomarkers.csv")
    SCATTER_STRATOS_ROUNDED_FILEPATH = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK,"complex_myVSfriendBiomarkersRounded.csv")
    SCATTER_BMI_FILEPATH             = paste0(BIOMARKERS_FOLDER,"BMIResults.csv")
    
    # -- Boxplots summaries
    BOXPLOT_BMI_FILEPATH             = paste0(BIOMARKERS_FOLDER,"BMICatResults.csv")
    # -- Distances summary
    DISTANCES_FILEPATH               = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK,"distancesBioSex.csv")
    
    # -- Categorical summary
    MEN_P_SIMPLE                     = paste0(BIOMARKERS_FOLDER_TABLES,"menPSimple.csv")
    MEN_P_BONFERRONI                 = paste0(BIOMARKERS_FOLDER_TABLES,"menPBonferroni.csv")
    MEN_P_BENJAMINI                  = paste0(BIOMARKERS_FOLDER_TABLES,"menPBenjamini.csv")
    WOMEN_P_SIMPLE                   = paste0(BIOMARKERS_FOLDER_TABLES,"womenPSimple.csv")
    WOMEN_P_BONFERRONI               = paste0(BIOMARKERS_FOLDER_TABLES,"womenPBonferroni.csv")
    WOMEN_P_BENJAMINI                = paste0(BIOMARKERS_FOLDER_TABLES,"womenPBenjamini.csv")
    BOTH_P_SIMPLE                    = paste0(BIOMARKERS_FOLDER_TABLES,"bothPSimple.csv")
    BOTH_P_BONFERRONI                = paste0(BIOMARKERS_FOLDER_TABLES,"bothPBonferroni.csv")
    BOTH_P_BENJAMINI                 = paste0(BIOMARKERS_FOLDER_TABLES,"bothPBenjamini.csv")
    BOTH_EXPAND_P_SIMPLE             = paste0(BIOMARKERS_FOLDER_TABLES,"bothExpandedPSimple.csv")
    BOTH_EXPAND_P_BONFERRONI         = paste0(BIOMARKERS_FOLDER_TABLES,"bothExpandedPBonferroni.csv")
    BOTH_EXPAND_P_BENJAMINI          = paste0(BIOMARKERS_FOLDER_TABLES,"bothExpandedPBenjamini.csv")    
    
    # -- Drugs summary
    # ---- Hormonal summary (deprecated, now in medicines)
    # -- Diseases summary
    
    # Get how many biomarkers you have.
    totalBiomarkersColumns = ncol(biomarkersTable) - 3
    TOTAL_BIOMARKERS       = totalBiomarkersColumns/2  # LOD and NDL are the same
  
    # Get the appropriate indexes
    LODIndex = firstBiomarkerIndex
    NDLIndex = TOTAL_BIOMARKERS + firstBiomarkerIndex  
  
    # Collection of all LOD and NDL
    allNDLIndex = c(NDLIndex:(NDLIndex+TOTAL_BIOMARKERS-1))
    allLODIndex = c(LODIndex:(LODIndex+TOTAL_BIOMARKERS-1))
}

# -----------------------------------------------------------------------------
# Prepare the proper datasets and what variables are we going to analyze automatically
#
#     - Filter women menstruating only.
# -----------------------------------------------------------------------------
{
    
    womenMenstruatingTable = womenOnlyTable[womenOnlyTable$MenstruationStart == "Yes",]
    
    
}

#---------------------------------------------------------------------------
# Check how many proteins are bellow LOD and make the proper plot and table
#---------------------------------------------------------------------------
{
  
    # Before doing anything, there is something really confusing, that is that
    # not everything was done in the same batch. And each batch has different
    # LODs.
    #
    # So, step zero, figure it out which batch the people have and fill the LOD
    # accordingly
    {
    
      batchIDs      = c("20160383", "20160977")
      LOD386Values  = biomarkersMetadataDF$LOD_Batch_20160383
      LOD977Values  = biomarkersMetadataDF$LOD_Batch_20160977
      batchIDsColor = c("#ed0000", "#00ad0b")
    
      batchInfoList = biomarkersTable$BatchNumber    
    
    }
  
    # -- First we do a general pass to see how many Proteins are under LOD
    generalLODStatisticsDF           = DF(TOTAL_BIOMARKERS, 7)
    colnames(generalLODStatisticsDF) = c("Protein", "N_Under_LOD", "N_Above_LOD", "N_NA", "Prt_Under_LOD", "Prt_Above_LOD", "Prt_NA")
    for(i in 1:TOTAL_BIOMARKERS){
    
        # First count the missing NA and 99999999 values in the batch file.
        # Since the 999 value is especial and common for each person then do nothing
        # and count NA only
        totalMissing = sum(is.na(completeTable[,NDLIndex+i-1]))
    
        # Prepare the LOD value vector
        LODValueVector = rep(9999, TOTAL_PEOPLE)
        for(j in 1:TOTAL_PEOPLE){
      
            # Which valid batch are you?
            myBatch = batchInfoList[j]
            if(!is.na(myBatch)){
        
              if(myBatch == "20160977") LODValueVector[j] = as.numeric(LOD977Values[i])
              if(myBatch == "20160383") LODValueVector[j] = as.numeric(LOD386Values[i])
              # This batch means that we perform both batches for this person
              # So we are going to take the minimum of both LOD
              if(myBatch == "99999999"){
                  LODValueVector[j] = min(as.numeric(LOD386Values[i]), as.numeric(LOD977Values[i]))
              }
          }
        }
      
        # Count how many are bellow and above LOD
        totalUnderLOD = sum(completeTable[,NDLIndex+i-1] < LODValueVector , na.rm=TRUE)
        totalAboveLOD = TOTAL_PEOPLE - (totalMissing + totalUnderLOD)
        
        
        # Write the LOD statistics
        generalLODStatisticsDF[i,1] = biomarkersMetadataDF$Protein[i]
        generalLODStatisticsDF[i,2] = totalUnderLOD 
        generalLODStatisticsDF[i,3] = totalAboveLOD
        generalLODStatisticsDF[i,4] = totalMissing
        generalLODStatisticsDF[i,5] = totalUnderLOD/TOTAL_PEOPLE
        generalLODStatisticsDF[i,6] = totalAboveLOD/TOTAL_PEOPLE
        generalLODStatisticsDF[i,7] = totalMissing/TOTAL_PEOPLE
    }
    # -- Write the RAW results into a CSV
    write.csv2(generalLODStatisticsDF,  LOD_STATISTICS_FILEPATH)
    
    # Now, we are going to make the plot to take a quick look at each biomarker status
    {
    
      # Prepare the data into long format so ggplot can do it things  
      longLODStats = DF(TOTAL_BIOMARKERS*10000,2)
      colnames(longLODStats) = c("Protein","Type")
      
      currentIndex = 1
      
      # For each protein in the summary
      print("Filling LOD Data")
      for(i in 1:TOTAL_BIOMARKERS){
        
        # Get the current protein and fill a bunch of rows with its name
        currentProtein = generalLODStatisticsDF$Protein[i]
        longLODStats[currentIndex:(currentIndex+10000),1] = currentProtein
        
        # Add as many "Under", "Above", or "Missing" as necessary
        totalUnder =  round(10000 * generalLODStatisticsDF$Prt_Under_LOD[i],0)
        totalAbove =  round(10000 * generalLODStatisticsDF$Prt_Above_LOD[i],0)
        totalNA    =  round(10000 * generalLODStatisticsDF$Prt_NA[i],0)
        
        if(totalUnder > 0){
            
            longLODStats[currentIndex:(currentIndex+totalUnder),2] = "Under LOD"
            currentIndex = currentIndex + totalUnder
          
        }
        
        if(totalAbove > 0){
          
            longLODStats[currentIndex:(currentIndex+totalAbove),2] = "Above LOD"
            currentIndex = currentIndex + totalAbove
          
        }
        
        if(totalNA>0){
          
            longLODStats[currentIndex:(currentIndex+totalNA),2] = "Missing Data"
            currentIndex = currentIndex + totalNA
          
        }
        
        # Give the factor order
        longLODStats$Type = factor(longLODStats$Type, levels = c("Under LOD", "Missing Data",  "Above LOD"))  
        
        
      }
      
      
      if(FALSE){
      doBarPlotV2 (longLODStats, 1, BIOMARKERS_FOLDER_IMAGES_GENERAL,
                   #groupIndex = NULL,
                   colorsVector = c("red","grey","blue"),
    
                        #countingType = "absolute",   # relative
                        rotation     = TRUE,        # TRUE
                        sort         = "none", # none, alphabetical, ascending
                        #minValue     = 0,
                        #maxValue     = Inf,
    
                        # cropNumbers   = 0, NOT IMPLEMENTED
                        #labelsAlignment = "above",   # under, none
                        #barsFontSize    = 2,
                        #colorCounting   = TRUE,     # TRUE
                                                         
                        plotTitle    = "Proportion of values, above LOD (blue), under LOD (red), and missing (grey)",
                        plotSubtitle = "",
                        plotXLabel   = "Biomarkers", plotYLabel = "Percentage",

                        overrideImageWidth  = 10,
                        overrideImageHeight = 30)
      }
      
      
      # Image with the horizontal bars in red, grey, and blue, telling how many values are above/bellow LOD
      filePath = doLongBarRelativeCombinePlot(longLODStats, 2, 1, BIOMARKERS_FOLDER_IMAGES_GENERAL,
                                              barsFontSize = 2,
                                              colorsVector = c("red","grey","blue"),
                                              plotTitle    = "Proportion of values, above LOD (blue), under LOD (red), and missing (grey)",
                                              plotSubtitle = "",
                                              plotXLabel   = "Biomarkers", plotYLabel = "Percentage of students in each category",
                                              sort = "none",
                                              imageWidth = 10,
                                              imageHeight = 70)
    
       writeImageLATEX2(filePath[[2]], LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_GENERAL, 
                        captionText   = "Overview of all subject (n=1038) biomarkers values with respect to LOD levels. The figure shows that using the NLD levels are ",
                        overrideLabel = "fig:LODLevelsOverview", 
                        pageHeight = 0.7, overrideFloat = TRUE)
      
       
      
    }
      
}



#---------------------------------------------------------------------------
# Check differences between men and women
#---------------------------------------------------------------------------
{
    
    # We are going to analyze each biomarker for men and women, the summary goes here
    sexInflamatorySummmaryDF = DF(TOTAL_BIOMARKERS,7)
    colnames(sexInflamatorySummmaryDF) = c("Protein", "PValue", "Significance","DeltaSigma","AbsSigma","Type", "AvgPro")
    
    # Get some general statistics about biomarkers
    biomarkersSummary = DF(TOTAL_BIOMARKERS, 5)
    colnames(biomarkersSummary) = c("Variable", "LOD Average", "NDL Average", "LOD SD", "NDL SD")
  
    # -- Init the names
    for(i in 1:TOTAL_BIOMARKERS){
        biomarkersSummary[i,1]        = biomarkersMetadataDF$Protein[i]
        sexInflamatorySummmaryDF[i,1] = biomarkersMetadataDF$Protein[i]
    }
   
    # We are going to stratify by sex, so divide in men and women
    biomarkersSummaryMen   = biomarkersSummary
    biomarkersSummaryWomen = biomarkersSummary
  
    # For each biomarker and for men and women, fill the table with the averages
    # and SDs, also do the significant difference between and boxplots
    print("Analyzing sex differnces") 
    for(i in 1:TOTAL_BIOMARKERS){
    
        print(   round(100*i/TOTAL_BIOMARKERS,2)   )  
          
        # Men
        # LOD
        biomarkersSummaryMen[i,2] = mean(menOnlyTable[,LODIndex + i - 1] , na.rm = TRUE )
        biomarkersSummaryMen[i,4] = sd(menOnlyTable[,LODIndex + i - 1] ,   na.rm = TRUE )
        # NDL
        biomarkersSummaryMen[i,3] = mean(menOnlyTable[,NDLIndex + i - 1] , na.rm = TRUE )
        biomarkersSummaryMen[i,5] = sd(menOnlyTable[,NDLIndex + i - 1] ,   na.rm = TRUE )
        
        # Women
        # LOD
        biomarkersSummaryWomen[i,2] = mean(womenOnlyTable[,LODIndex + i - 1] , na.rm = TRUE )
        biomarkersSummaryWomen[i,4] = sd(womenOnlyTable[,LODIndex + i - 1] ,   na.rm = TRUE )
        # NDL
        biomarkersSummaryWomen[i,3] = mean(womenOnlyTable[,NDLIndex + i - 1] , na.rm = TRUE )
        biomarkersSummaryWomen[i,5] = sd(womenOnlyTable[,NDLIndex + i - 1] ,   na.rm = TRUE )
        
        # Get the LOD cutoff values
        cutOffList      = newList(2)
        cutOffList[[1]] = as.numeric(c(LOD386Values[i], LOD977Values[i]))
        cutOffList[[2]] = batchIDs
        cutOffList[[3]] = batchIDsColor
        
        # Make a nice title
        currentBoxplotTitle    = paste0(biomarkersMetadataDF$Protein[i], " NDL")
        currentBoxplotSubtitle = paste0("Different batches have different LOD (horizontal lines)")
        
        # Do the plot
        myBoxplotResults =  doBoxPlotV2 (completeTable, (NDLIndex+i-1),
                                         BIOMARKERS_FOLDER_IMAGES_SEX_BOXPLOTS,
                                         groupIndex = sexIndex,
                                         showPValues = TRUE,
                                         significantPValue = 0.05,
                                         cutOffLine   = cutOffList,
        
                                         colorsVector = COLOR_VECTOR_SEX,
                                         plotTitle    = currentBoxplotTitle,
                                         plotSubtitle = currentBoxplotSubtitle,
                                         plotYLabel   = currentBoxplotTitle,
                                         overrideImageWidth = 7)  
        
        # Get the p-value
        currentPValue = myBoxplotResults[[3]][[2]]
        
        # Write everything into the sex summary result table
        sexInflamatorySummmaryDF[i,2] = currentPValue
        sexInflamatorySummmaryDF[i,3] = getAsterkisPValue(currentPValue)
        sexInflamatorySummmaryDF[i,4] = (biomarkersSummaryMen[i,3] - biomarkersSummaryWomen[i,3]) / biomarkersSummaryWomen[i,5]
        sexInflamatorySummmaryDF[i,5] = abs(sexInflamatorySummmaryDF[i,4])
        
        # Finally, check out the pvalue, and the sex average difference
        # Fill the type accordingly
        
        # If we have significant values
        if(currentPValue < 0.05){
            
            # If men average is greater than women average
            if( biomarkersSummaryMen[i,3] > biomarkersSummaryWomen[i,3]){
                
                sexInflamatorySummmaryDF[i,6] = "Men low p-value"
                sexInflamatorySummmaryDF[i,7] = (-100) * ((biomarkersSummaryMen[i,3] / biomarkersSummaryWomen[i,3]) -  1) 
                
            }
            # If women average is greater than men average
            else{
                
                sexInflamatorySummmaryDF[i,6] = "Women low p-value"
                sexInflamatorySummmaryDF[i,7] = 100 * ((biomarkersSummaryWomen[i,3] / biomarkersSummaryMen[i,3]) - 1)
            }
            
        }
        else{
            
            # If men average is greater than women average
            if(biomarkersSummaryMen[i,3] > biomarkersSummaryWomen[i,3]){
                
                sexInflamatorySummmaryDF[i,6] = "Men"
                
                # Correction for Artemin that has negative values
                if(biomarkersSummaryMen[i,3] / biomarkersSummaryWomen[i,3] < 1)
                    sexInflamatorySummmaryDF[i,7] = 0
                else
                    sexInflamatorySummmaryDF[i,7] = (-100) * ((biomarkersSummaryMen[i,3] / biomarkersSummaryWomen[i,3]) -  1) 
            }
            # If women average is greater than men average
            else{
                
                sexInflamatorySummmaryDF[i,6] = "Women"
                sexInflamatorySummmaryDF[i,7] = 100 * ((biomarkersSummaryWomen[i,3] / biomarkersSummaryMen[i,3]) - 1)
            }
            
        }
        
    }

    # Give the factor order
    sexInflamatorySummmaryDF$Type = factor(sexInflamatorySummmaryDF$Type, levels = c("Men",  "Men low p-value",
                                                                                     "Women","Women low p-value"))  
    
    
    # Do the plot
    filePath = doLongBarAbsoluteCombinePlot(sexInflamatorySummmaryDF, 1, 7, 6, BIOMARKERS_FOLDER_IMAGES_GENERAL,
                                            colorsVector = rev(c(COLOR_MAN_LOW,  COLOR_MAN,
                                                                 COLOR_WOMAN_LOW,COLOR_WOMAN)),
                                            plotTitle    = "Average NDL levels differences with respect sex",
                                            plotSubtitle = "",
                                            plotYLabel   = "% increase", 
                                            sort = "none", imageHeight = 20, imageWidth = 10)
        
    writeImageLATEX2(filePath[[2]], LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_GENERAL, 
                        captionText   = "Overview of all biomarkers diferences with respect sex. In many cases there is a significant difference between men and women (p<0.05). 
                                         Due biological reasons.",
                        overrideLabel = "fig:BiomarkersBySexDifference",
                        pageHeight = 0.3, overrideFloat = TRUE)
    
    
    write.csv2( sexInflamatorySummmaryDF , file = paste0(BIOMARKERS_FOLDER_IMAGES_GENERAL,"NDL Levels by sex.csv"))
    
    
    
    
    
    # Prepare a table with the significance differences for the article
    paperSexDifferencesSummmaryDF              = sexInflamatorySummmaryDF
    paperSexDifferencesSummmaryDF$Acronym      = biomarkersMetadataDF$Acronym
    paperSexDifferencesSummmaryDF$Men          = biomarkersSummaryMen$`NDL Average`
    paperSexDifferencesSummmaryDF$Women        = biomarkersSummaryWomen$`NDL Average`
    
    paperSexDifferencesSummmaryDF$PValue       = NULL
    paperSexDifferencesSummmaryDF$DeltaSigma   = NULL
    paperSexDifferencesSummmaryDF$AbsSigma     = NULL
    paperSexDifferencesSummmaryDF$Type         = NULL
    paperSexDifferencesSummmaryDF$AvgPro       = NULL
    
    paperSexDifferencesSummmaryDF$Men   = round(paperSexDifferencesSummmaryDF$Men,2)
    paperSexDifferencesSummmaryDF$Women = round(paperSexDifferencesSummmaryDF$Women,2)
    
    paperSexDifferencesSummmaryDF = paperSexDifferencesSummmaryDF[, c("Acronym", "Protein", "Significance","Men","Women")]
    colnames(paperSexDifferencesSummmaryDF) = c("Acronym", "Protein", "Significance","$\\overline{x}_{men}$", "$\\overline{x}_{women}$")
    
    writeTableLATEX(paperSexDifferencesSummmaryDF, BIOMARKERS_FOLDER_TABLES_SEX, tableCaption = "Sex differences for each biomarker",
                    overrideTableName = "SexDifferencesBiomakers", widthProportion = 0, heightProportion = 0.5)
    
}

#---------------------------------------------------------------------------
# Separate tables into men and women due high differences levels
#---------------------------------------------------------------------------
{
    
    # we need to do the stratification for both sexes, and for the LOD and NDL
    menBiomarkersLODTable   = menOnlyTable[,LODIndex:(LODIndex+TOTAL_BIOMARKERS-1)]
    menBiomarkersNDLTable   = menOnlyTable[,NDLIndex:(NDLIndex+TOTAL_BIOMARKERS-1)]
    womenBiomarkersLODTable = womenOnlyTable[,LODIndex:(LODIndex+TOTAL_BIOMARKERS-1)]
    womenBiomarkersNDLTable = womenOnlyTable[,NDLIndex:(NDLIndex+TOTAL_BIOMARKERS-1)]
    
    # Not everyone has biomarkers analysis done, log who hasn't and mark them for deletion in the tables
    # We just need to check column 1 as the rest of the columns just follow along
    keepTheseMen   = !is.na(menBiomarkersLODTable[,1])
    keepTheseWomen = !is.na(womenBiomarkersLODTable[,1])
    
    menBiomarkersLODTable   = menBiomarkersLODTable[keepTheseMen,]
    menBiomarkersNDLTable   = menBiomarkersNDLTable[keepTheseMen,]
    womenBiomarkersLODTable = womenBiomarkersLODTable[keepTheseWomen,]
    womenBiomarkersNDLTable = womenBiomarkersNDLTable[keepTheseWomen,]
    
    # Later on, we need to check who are friend with people with valid LOD/NDL
    # values, so keep track of the IDs
    deletedMenIDs   = menOnlyTable[!keepTheseMen,]$ID
    deletedWomenIDs = womenOnlyTable[!keepTheseWomen,]$ID
    keepMenIDs      = menOnlyTable[keepTheseMen,]$ID
    keepWomenIDs    = womenOnlyTable[keepTheseWomen,]$ID
    
    # Finally, we need a table that has every other variable, but only valid
    # men and women
    biomenOnlyTable   = menOnlyTable[keepTheseMen,]
    biowomenOnlyTable = womenOnlyTable[keepTheseWomen,]
    
    # From here on, we have tables clean of NAs. So check how big is each table
    totalMen   = nrow(menBiomarkersLODTable)
    totalWomen = nrow(womenBiomarkersLODTable)
    
}

#--------------------------------------------------------------------------------
# Convert supplementary tables that we are going to use in Latex or GITHUB
# This is information common for every article
#--------------------------------------------------------------------------------
{

    writeTableLATEX(biomarkersMetadataDF, BIOMARKERS_FOLDER_TABLES_GENERAL,
                    tableCaption = "Summary of all biomarkers. From left to right, short acronym with the protein ID, protein name, UniProt ID, LOD value for each of the two run batches, UniProt web with the protein, Wikipedia link with the protein.",
                    overrideTableName = "SuplementaryAllBiomarkers")
    
}

#---------------------------------------------------------------------------
# Prepare the references table
#---------------------------------------------------------------------------
{
    
    forcedColumnLengthVector = c(0,10,10,0)
    
    #writeTableLATEX(biomarkersReferencesDF, BIOMARKERS_FOLDER_TABLES_GENERAL,
     #               tableCaption = "Summary of all biomarkers literature with respect obesity.",
      #              forcedColumnWidths = forcedColumnLengthVector)
    
    
    biomarkersReferences1of2DF = biomarkersReferencesDF[1:46,]
    biomarkersReferences2of2DF = biomarkersReferencesDF[47:nrow(biomarkersReferencesDF),]
    
    writeTableLATEX(biomarkersReferences1of2DF, BIOMARKERS_FOLDER_TABLES_GENERAL,
                    tableCaption = "Summary of all biomarkers literature with respect obesity (1 of 2).",
                    forcedColumnWidths = forcedColumnLengthVector)
    
    writeTableLATEX(biomarkersReferences2of2DF, BIOMARKERS_FOLDER_TABLES_GENERAL,
                    tableCaption = "Summary of all biomarkers literature with respect obesity (2 of 2).",
                    forcedColumnWidths = forcedColumnLengthVector)
    
    
    
    
}