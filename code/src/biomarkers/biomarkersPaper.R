# Final script for the final biomarkers paper.

# String library
library(stringr) 

# All the graph libraries, because R refuses to give me help about where the
# create_layout() function is suppose to be, not that it makes any sense that
# the script works without these because the stupid eviroment that is saved
# forever 
library(ggraph)
library(igraph)
library(statnet)
library(ggpubr) # ggarrange()
library(ggalluvial) # Allovial plot
library(ggrepel)


# Prepare the tables for men and women, as well as all the NDL and LOD tables
source( paste0(MAIN_PROJECT_FOLDER,"src/biomarkers/preparingData.R"),  encoding="utf-8")

# Load tools
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/basic/toolsNetworks.R"),   encoding="utf-8")

# Load plotting
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingNetwork.R"),      encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingHistograms.R"),   encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingRegression.R"),   encoding="utf-8")


#---------------------------------------------------------------------------
# Check how much RAW data we have
# -- This is use mostly to write the summary in the ABSTRACT
#---------------------------------------------------------------------------
{
    
    # BIOMARKERS
    {
        # Get some general statistics about biomarkers
        biomarkersTotal = DF(TOTAL_BIOMARKERS, 3)
        colnames(biomarkersTotal) = c("Variable", "LOD_NonNA", "NDL_NonNA")
        
        # -- Init the names and count
        for(i in 1:TOTAL_BIOMARKERS){
            biomarkersTotal[i,1] = biomarkersMetadataDF$Protein[i]
            biomarkersTotal[i,2] =  sum(!is.na(completeTable[,LODIndex + i - 1]))
            biomarkersTotal[i,3] =  sum(!is.na(completeTable[,NDLIndex + i - 1]))
        }
        
        # Percentage wise
        percentageTotal = biomarkersTotal[2,2]/TOTAL_PEOPLE        
    }
    
    # BLOOD
    {
        bloodTotal           = DF(totalBloodColumns, 3)
        colnames(bloodTotal) = c("Variable", "NonNA", "Prc")
        
        # -- Init the names and count
        offset = 17
        for(i in 1:totalBloodColumns){
            
            bloodTotal[i,1] = bloodMetadataDF$Short[i + offset]
            bloodTotal[i,2] = sum(!is.na(completeTable[,firstBloodIndex + i - 1]))
            bloodTotal[i,3] = sum(!is.na(completeTable[,firstBloodIndex + i - 1]))/TOTAL_PEOPLE
            
        }
        
        
    }
    
    # ANTROPOMETRY
    {
        
        antropometryTotal           = DF(ncol(antropometricTableFF1), 3)
        colnames(antropometryTotal) = c("Variable", "NonNA", "Prc")
        
        # -- Init the names and count
        for(i in 1:ncol(antropometricTableFF1)){
            
            antropometryTotal[i,1] = colnames(antropometricTableFF1)[i]
            antropometryTotal[i,2] = sum(!is.na(antropometricTableFF1[i]))
            antropometryTotal[i,3] = sum(!is.na(antropometricTableFF1[i]))/TOTAL_PEOPLE
            
        }
        
    }
    
	# HIGHSCHOOL COUNT IN FF1 AND FF2
	{
	
		tempA = summarizeCategorical(completeTable, highSchoolIndex, sorted = "none")
		
		ff2IDs = antropometricTableFF2[!is.na(antropometricTableFF2$BMI),]$ID
		
		ff2OnlyTable = completeTable[completeTable$ID %in% ff2IDs,]
		
		tempB = summarizeCategorical(ff2OnlyTable, highSchoolIndex, sorted = "none")
		
		highschoolSummaryDF = DF(8,5)
		colnames(highschoolSummaryDF) = c("Highschool", "FF1", "FF2", "Dif", "Rel")
		
		highschoolSummaryDF[,1]	= tempA$Modality
		highschoolSummaryDF[,2]	= tempA$Count
		highschoolSummaryDF[,3]	= tempB$Count
		highschoolSummaryDF[,4] = highschoolSummaryDF[,3] - highschoolSummaryDF[,2]
		highschoolSummaryDF[,5] = highschoolSummaryDF[,4]/highschoolSummaryDF[,2]
		
		temp3 = completeTable[,c(highSchoolIndex, hsProgrameIndex)]
		
	}
	
	
}

# ------------------------------------------------------------------------------
# Do all the network plots
# ------------------------------------------------------------------------------
{

    # Make a constant layout for consistence between plots
    myGraph           = graph_from_data_frame(overallEdgesDF, vertices = completeTable, directed = FALSE)
    myConstantLayoutA = create_layout(graph = myGraph, layout = 'mds')
    
    # Figure in annex where it shows all the graph at the same time with
    # no highlights, all using MDS layout.
    {

        # In this list, we keep the image file path for each of the plots
        myListOfPlots        = rep(NA, TOTAL_NETWORKS)
        myListOfPlotsObjects = newList(TOTAL_NETWORKS)
        
        # Each plot for each network.
        for(i in 1:TOTAL_NETWORKS){

            currentOverridedPlotName = paste(NETWORK_NAMES[i],"_with_no_highlight", sep='')
            currentPlotTitle         = paste(NETWORK_NAMES[i], sep='')
            currentOverrideCaption   = "Size based on number of undirected relationships"

            plotResults = doGraphPlot(allEdges[[i]],  completeTable, PAPER_FOLDER,
                                      sizeVariableIndex = overallConnectionsIndex,
                                      selectedLayouts   = DO_THIS_LAYOUTS,
                                      plotTitle         = currentPlotTitle,
                                      overrideTableName = currentOverridedPlotName) 
    
            # Save the current plot for the grid image
            myListOfPlots[i]          = plotResults[[2]]
            myListOfPlotsObjects[[i]] = plotResults[[1]]

        }
  
        # Make the grid image for all the networks
        # ---- By default, the grid is divided into two columns
        #      you can change this into sqrt of images so it more squarish
        #
        # (remember that in the network.R script you have the same code with
        #  the option to export to latex)
        totalGridColumns = 2
        totalGridRows    = ceiling(TOTAL_NETWORKS/totalGridColumns)

        # ---- PNG / PDF
        ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows)
        ggsave(BIOMARKERS_GRAPH_GRID, width = 11, height = 16)
        ggsave(changeFileExtension(BIOMARKERS_GRAPH_GRID, "pdf"), width = 22, height = 32) # Number for image size are different on porpuse. The pdf saved is not the same, border for the nodes are thicker and it looks weird
        
        writeImageLATEX2(BIOMARKERS_GRAPH_GRID, LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_NETWORK, 
                         captionText   = "Overview of all friendship networks.",
                         overrideLabel = "fig:allGraphsOverview", 
                         pageHeight    = 0.7, overrideFloat = TRUE)
        
    }
    
}

# ------------------------------------------------------------------------------
# Check if the friendship is good enough
# ------------------------------------------------------------------------------
{
    
    # -- Histogram with How well is this friendship a representation of real life
    {
    
        plotObject = doHistogramPlot2(completeTable, overviewIndex, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                      colorsVector   = COLOR_FRIENDSHIP,
                                      binsWidth      = 1,
                                      binBorderColor = "#333333",
                                      plotTitle      = " Does these friends give a good overview of your social network? ",
                                      plotSubtitle   = " 0 = Low, 10 = High",
                                      plotXLabel     = "Points", plotYLabel = "Total")       
        
        
        
        writeImageLATEX2(plotObject[[2]], LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_NETWORK, 
                         captionText   = "The histogram tells us that the majority of students evaluate their reported friendship network as representative of their real life social network.",
                         overrideLabel = "fig:histogramFriendship", 
                         pageHeight    = 0.4, overrideFloat = TRUE)        
        
    }
    
}

#---------------------------------------------------------------------------
# Create the antropometry table and figures for men and women
#---------------------------------------------------------------------------
{

    # How many antropomotric variables do you have
    totalAntropometries = lastAntropometryIndex - fistAntropometryIndex
    
    # Save the boxplots in here
    myListOfPlotsObjects = newList(totalAntropometries)
    
    # Save the antropometric values here so we print this table
    antropometricSummaryDF = DF(totalAntropometries, 6)
    colnames(antropometricSummaryDF) = c("Concept","$\\overline{x}_{men}$", "$\\overline{x}_{women}$", "$SD_{men}$", "$SD_{women}$", "Significance")
    
    # For each antropometric variable do your thing
    for(i in 1:totalAntropometries){
        
        currentPlotTitle  = ""
        
        # Adjust the labels for each variable
        if(i == 1) currentPlotTitle  = "Waist circurference (cm)"
        if(i == 2) currentPlotTitle  = "Hip circurference (cm)"
        if(i == 3) currentPlotTitle  = "Height (cm)"
        if(i == 4) currentPlotTitle  = "Weight (kg)"
        if(i == 5) currentPlotTitle  = "BMI (kg/m²)"
        if(i == 6) currentPlotTitle  = "Heart Rate (bpm)"
        if(i == 7) currentPlotTitle  = "Systolic BP (mmHg)"
        if(i == 8) currentPlotTitle  = "Diastolic BP (mmHg)"
        
        # (density plot doesn't look good, boxplots simplest but looks much more nicer)
        #doCategoricalDensityPlot(completeTable, (fistBloodIndex + i - 1), sexIndex, plotFilePath = BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
        #                        colorsVector = COLOR_VECTOR_SEX,
        #                       imageWidth = 8, imageHeight = 8)    
                
        
        currentBoxPlot = doBoxPlotV2 (completeTable, (fistAntropometryIndex + i - 1),
                                         BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
                                         groupIndex = sexIndex,
                                         colorsVector = COLOR_VECTOR_SEX,
                                         showPValues = TRUE,
                                         significantPValue = 0.05,
                                         
                                         plotTitle = currentPlotTitle,
                                         plotSubtitle = "",
                                         plotXLabel = "",
                                         plotYLabel = "",
        
                                         overrideImageWidth = 7) 
        
        myListOfPlotsObjects[[i]] = currentBoxPlot[[1]]
        
        # Fill the table
        antropometricSummaryDF[i,1] = colnames(completeTable)[(fistAntropometryIndex + i - 1)]
        antropometricSummaryDF[i,2] = round(mean(menOnlyTable[,(fistAntropometryIndex + i - 1)],   na.rm = TRUE),1)
        antropometricSummaryDF[i,3] = round(mean(womenOnlyTable[,(fistAntropometryIndex + i - 1)], na.rm = TRUE),1)
        antropometricSummaryDF[i,4] = round(sd(menOnlyTable[,(fistAntropometryIndex + i - 1)],     na.rm = TRUE),1)
        antropometricSummaryDF[i,5] = round(sd(womenOnlyTable[,(fistAntropometryIndex + i - 1)],   na.rm = TRUE),1)
        antropometricSummaryDF[i,6] = getAsterkisPValue(currentBoxPlot[[3]][[2]])

    }

    # Make the grid image
    totalGridColumns = 2
    totalGridRows    = ceiling(totalAntropometries/totalGridColumns)

    # Save as PNG
    # ---- This image is too big, is simpler to put this on a table
    if(FALSE){
        ALL_ANTROPOMETRY_FILEPATH = file.path(paste(BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY, "BloodGrid.png", sep = ""))
        ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows,
                  common.legend = TRUE, legend = "bottom")
        ggsave(ALL_ANTROPOMETRY_FILEPATH, width = 10, height = 16)
     
        writeImageLATEX2(ALL_ANTROPOMETRY_FILEPATH, "../../../../results/biomarkers/images/antropometry/", 
                         captionText   = "Overview of all antropometric variables diferences with respect sex.",
                         overrideLabel = "fig:BloodBySexDifference",
                         pageWidth = 1, overrideFloat = TRUE)
    }

    # Save the table
    writeTableLATEX(antropometricSummaryDF, BIOMARKERS_FOLDER_TABLES_ANTROPOMETRY, tableCaption = "Sex differences for antropometry variables",
                    overrideTableName = "SexDifferencesAntropometryTable", widthProportion = 0, heightProportion = 0.1)
    
}


#---------------------------------------------------------------------------
# Do biomarkers levels for every numerical (Antropometrics and Blood)
#---------------------------------------------------------------------------
{
    
    GITHUB_MEN_PREFIX   = 'https://github.com/uit-hdl/mimisbrunnr/tree/main/results/biomarkers/images/antropometry/nohighschool/men'
    GITHUB_WOMEN_PREFIX = 'https://github.com/uit-hdl/mimisbrunnr/tree/main/results/biomarkers/images/antropometry/nohighschool/women'
    
    
    # Let start with the antropometry table
    menResultsAntropometryDF           = DF(TOTAL_BIOMARKERS, (totalAntropometries*2+1)) # +1 for Protein
    colnames(menResultsAntropometryDF) = c("Protein",c(antropometricSummaryDF$Concept, antropometricSummaryDF$Concept))
    menResultsAntropometryDF$Protein   = biomarkersMetadataDF$Protein
    womenResultsAntropometryDF         = menResultsAntropometryDF
    
    # For both men and women
    for(i in 1:2){
        
        # Select which table are we using
        {
            currentSubtable = menOnlyTable
            currentSex      = "Man"
            if(i == 2){
                currentSex  = "Woman"
                currentSubtable = womenOnlyTable
            }
        }
        
        # For each of the biomarkers
        for(k in 1:TOTAL_BIOMARKERS){
            print(   round(100*k/TOTAL_BIOMARKERS,2)   )  
            currentBiomarkerIndex = allNDLIndex[k]
            currentBiomarkerName  = biomarkersMetadataDF$Protein[k]
     
            # For each of the antropometric variables
            for(j in 1:totalAntropometries){
                
                currentAntropometricIndex = firstAntropometricIndex + j - 1
                currentAntropometricName  = antropometricSummaryDF$Concept[j]
                
                # Clean the data from NA data
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentBiomarkerIndex]),]
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentAntropometricIndex]),]
                
                # Run the model
                # Plot the graph
                currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " and ", currentAntropometricName)
                currentTableOverride = paste0(currentBiomarkerName,"_",    currentSex, "_",     currentAntropometricName)
                currentSavingFolder  = BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY_NOHIGHSCHOOL_MEN
                if(i == 2) currentSavingFolder  = BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY_NOHIGHSCHOOL_WOMEN
                
                myAntropomodel = doRegressionPlot(currentSubtable, currentAntropometricIndex, currentBiomarkerIndex, currentSavingFolder,
                                                  plotTitle = currentPlotTitle,
                                                  overrideTableName = currentTableOverride,
                                                  plotTheme = "regression",
                                                  overrideImageWidth = 12)
                
                
                minimumPValueIndex = which.max(myAntropomodel[[5]]$`p-value`) 
                maximumR2Index     = which.max(myAntropomodel[[5]]$R2) 
                
                if(i == 1){
                  menResultsAntropometryDF[k,(j+1)]                       = myAntropomodel[[5]][minimumPValueIndex,3]  
                  menResultsAntropometryDF[k,(totalAntropometries+j+1)]   = myAntropomodel[[5]][minimumPValueIndex,2]  
                    
                } 
                else{
                  womenResultsAntropometryDF[k,(j+1)]                       = myAntropomodel[[5]][minimumPValueIndex,3]  
                  womenResultsAntropometryDF[k,(totalAntropometries+j+1)]   = myAntropomodel[[5]][minimumPValueIndex,2]                      
                }       
                
                
            }
                   
        }
        
    }
            
    # All results are finish, addjust by Benjamini and Bonferroni
    # And create the easy to read asterisk matrix for men and women
    {
     
        menResultsAntropometryDFCopy = menResultsAntropometryDF
        #menResultsAntropometryDFCopy[,totalAntropometries+2:ncol(menResultsAntropometryDFCopy)] = NULL
        womenResultsAntropometryDFCopy = womenResultsAntropometryDF
        #womenResultsAntropometryDFCopy[,totalAntropometries+2:ncol(womenResultsAntropometryDFCopy)] = NULL        
        
        
        menResultsAntropometryBonferroniDF         = menResultsAntropometryDFCopy
        menResultsAntropometryBenjaminiDF          = menResultsAntropometryDFCopy
        menAsteriskResultsAntropometryDF           = menResultsAntropometryDFCopy
        menAsteriskResultsAntropometryBonferroniDF = menResultsAntropometryDFCopy
        menAsteriskResultsAntropometryBenjaminiDF  = menResultsAntropometryDFCopy
        
        womenResultsAntropometryBonferroniDF         = womenResultsAntropometryDFCopy
        womenResultsAntropometryBenjaminiDF          = womenResultsAntropometryDFCopy
        womenAsteriskResultsAntropometryDF           = womenResultsAntropometryDFCopy
        womenAsteriskResultsAntropometryBonferroniDF = womenResultsAntropometryDFCopy
        womenAsteriskResultsAntropometryBenjaminiDF  = womenResultsAntropometryDFCopy
    
        for(j in 1:totalAntropometries){
    
            menResultsAntropometryBenjaminiDF[,j+1]  = p.adjust(menResultsAntropometryBenjaminiDF[,j+1],  method = "fdr")  
            menResultsAntropometryBonferroniDF[,j+1] = p.adjust(menResultsAntropometryBonferroniDF[,j+1], method = "bonferroni") 
        
            menAsteriskResultsAntropometryDF[,j+1]           = getAsterkisPValue(menResultsAntropometryDF[,j+1])
            menAsteriskResultsAntropometryBenjaminiDF[,j+1]  = getAsterkisPValue(menResultsAntropometryBenjaminiDF[,j+1])
            menAsteriskResultsAntropometryBonferroniDF[,j+1] = getAsterkisPValue(menResultsAntropometryBonferroniDF[,j+1])
                    
            womenResultsAntropometryBenjaminiDF[,j+1]  = p.adjust(womenResultsAntropometryBenjaminiDF[,j+1],  method = "fdr")  
            womenResultsAntropometryBonferroniDF[,j+1] = p.adjust(womenResultsAntropometryBonferroniDF[,j+1], method = "bonferroni") 
        
            womenAsteriskResultsAntropometryDF[,j+1]           = getAsterkisPValue(womenResultsAntropometryDF[,j+1])
            womenAsteriskResultsAntropometryBenjaminiDF[,j+1]  = getAsterkisPValue(womenResultsAntropometryBenjaminiDF[,j+1])
            womenAsteriskResultsAntropometryBonferroniDF[,j+1] = getAsterkisPValue(womenResultsAntropometryBonferroniDF[,j+1])
            
        }

        # Clean the matrixes from ns for all rows (no column
        
        # For each row
        deleteTheseSimpleRows     = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBenjaminiRows  = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBonferroniRows = rep(FALSE,TOTAL_BIOMARKERS)
        for(i in 1:TOTAL_BIOMARKERS){
            
            if(sum(menAsteriskResultsAntropometryDF[i,1:(totalAntropometries+1)] == 'ns')           == 8) deleteTheseSimpleRows[i]     = TRUE
            if(sum(menAsteriskResultsAntropometryBenjaminiDF[i,1:(totalAntropometries+1)]  == 'ns') == 8) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(menAsteriskResultsAntropometryBonferroniDF[i,1:(totalAntropometries+1)] == 'ns') == 8) deleteTheseBonferroniRows[i] = TRUE
            
        }
                
        menAsteriskResultsAntropometryDF           = menAsteriskResultsAntropometryDF[!deleteTheseSimpleRows,]
        menAsteriskResultsAntropometryBenjaminiDF  = menAsteriskResultsAntropometryBenjaminiDF[!deleteTheseBenjaminiRows,]
        menAsteriskResultsAntropometryBonferroniDF = menAsteriskResultsAntropometryBonferroniDF[!deleteTheseBonferroniRows,]
        
        deleteTheseSimpleRows     = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBenjaminiRows  = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBonferroniRows = rep(FALSE,TOTAL_BIOMARKERS)
        for(i in 1:TOTAL_BIOMARKERS){
            
            if(sum(womenAsteriskResultsAntropometryDF[i,1:(totalAntropometries+1)] == 'ns')           == 8) deleteTheseSimpleRows[i]     = TRUE
            if(sum(womenAsteriskResultsAntropometryBenjaminiDF[i,1:(totalAntropometries+1)]  == 'ns') == 8) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(womenAsteriskResultsAntropometryBonferroniDF[i,1:(totalAntropometries+1)] == 'ns') == 8) deleteTheseBonferroniRows[i] = TRUE
            
        }
                
        womenAsteriskResultsAntropometryDF           = womenAsteriskResultsAntropometryDF[!deleteTheseSimpleRows,]
        womenAsteriskResultsAntropometryBenjaminiDF  = womenAsteriskResultsAntropometryBenjaminiDF[!deleteTheseBenjaminiRows,]
        womenAsteriskResultsAntropometryBonferroniDF = womenAsteriskResultsAntropometryBonferroniDF[!deleteTheseBonferroniRows,]
       

        # Delete the R2 information (optional, but the link is in the table anyway and we don't care if the model is good or not, only that there's relationship)
        menAsteriskResultsAntropometryDF[,c(totalAntropometries+2:ncol(menAsteriskResultsAntropometryDF))]             = NULL
        menAsteriskResultsAntropometryBonferroniDF[,c(totalAntropometries+2:ncol(menAsteriskResultsAntropometryBonferroniDF))]   = NULL
        menAsteriskResultsAntropometryBenjaminiDF[,c(totalAntropometries+2:ncol(menAsteriskResultsAntropometryBenjaminiDF))]    = NULL
        womenAsteriskResultsAntropometryDF[,c(totalAntropometries+2:ncol(womenAsteriskResultsAntropometryDF))]           = NULL
        womenAsteriskResultsAntropometryBonferroniDF[,c(totalAntropometries+2:ncol(womenAsteriskResultsAntropometryBonferroniDF))] = NULL
        womenAsteriskResultsAntropometryBenjaminiDF[,c(totalAntropometries+2:ncol(womenAsteriskResultsAntropometryBenjaminiDF))]  = NULL
                    
    }
    
    # Write to latex
    {

        writeTableLATEX(menAsteriskResultsAntropometryDF,          BIOMARKERS_FOLDER_TABLES_ANTROPOMETRY,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in men",
                        overrideTableName = "BiomarkersBloodMen",          heightProportion = 0.2, headerColor = COLOR_MAN)        
        
        writeTableLATEX(menAsteriskResultsAntropometryBenjaminiDF, BIOMARKERS_FOLDER_TABLES_ANTROPOMETRY,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in men, after applying Benjamini correction",
                        overrideTableName = "BiomarkersBloodBenjaminiMen",  heightProportion = 0.2, headerColor = COLOR_MAN)        
        
        writeTableLATEX(menAsteriskResultsAntropometryBonferroniDF,       BIOMARKERS_FOLDER_TABLES_ANTROPOMETRY,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in men, after applying Bonferroni correction",
                        overrideTableName = "BiomarkersBloodBonferroniMen", heightProportion = 0.2, headerColor = COLOR_MAN)
        
        
        
        
        writeTableLATEX(womenAsteriskResultsAntropometryDF,          BIOMARKERS_FOLDER_TABLES_ANTROPOMETRY,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in women",
                        overrideTableName = "BiomarkersBloodWomen",          heightProportion = 0.2, headerColor = COLOR_WOMAN)        
        
        writeTableLATEX(womenAsteriskResultsAntropometryBenjaminiDF, BIOMARKERS_FOLDER_TABLES_ANTROPOMETRY,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in women, after applying Benjamini correction",
                        overrideTableName = "BiomarkersBloodBenjaminiWomen",  heightProportion = 0.2, headerColor = COLOR_WOMAN)
        
        writeTableLATEX(womenAsteriskResultsAntropometryBonferroniDF,       BIOMARKERS_FOLDER_TABLES_ANTROPOMETRY,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in women, after applying Bonferroni correction",
                        overrideTableName = "BiomarkersBloodBonferroniWomen", heightProportion = 0.2, headerColor = COLOR_WOMAN)
        
        
    }

            
  
    # Let's not do the blood for now...
    if(FALSE){
    
    # Now let do the blood, which is the same but with more variables
    menResultsBloodDF           = DF(TOTAL_BIOMARKERS, (totalBloodColumns+1))
    colnames(menResultsBloodDF) = c("Protein",bloodMetadataLatexDF$Short)
    menResultsBloodDF$Protein   = biomarkersMetadataDF$Protein
    womenResultsBloodDF         = menResultsBloodDF 
    
    # For men and women
    for(i in 1:2){
        
        # Select which table are we using
        {
            currentSubtable = menOnlyTable
            currentSex      = "Man"
            if(i == 2){
                currentSex  = "Woman"
                currentSubtable = womenOnlyTable
            }
        }
        
        # For each of the biomarkers
        for(k in 1:TOTAL_BIOMARKERS){
            
            currentBiomarkerIndex = allNDLIndex[k]
            currentBiomarkerName  = biomarkersMetadataDF$Protein[k]
     
            # For each of the antropometric variables
            for(j in 1:totalBloodColumns){
                
                currentAntropometricIndex = firstBloodIndex + j - 1
                currentAntropometricName  = bloodMetadataLatexDF$Short[j]
                
                # Clean the data from NA data
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentBiomarkerIndex]),]
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentAntropometricIndex]),]
                
                # Run the model
                # Plot the graph
                currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " and ", currentAntropometricName)
                currentTableOverride = paste0(currentBiomarkerName,"_",    currentSex, "_",     currentAntropometricName)
                
                myAntropomode        = doSimpleRegression(currentSubtable, currentAntropometricIndex, currentBiomarkerIndex, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                                          plotTitle = currentPlotTitle,
                                                          overrideTableName = currentTableOverride)

                if(i == 1) menResultsBloodDF[k,(j+1)]   = glance(myAntropomode)$p.value[[1]]
                else       womenResultsBloodDF[k,(j+1)] = glance(myAntropomode)$p.value[[1]]
                
                
            }
                   
        }
        
    }
    
    
    
    
    # All results are finish, addjust by Benjamini and Bonferroni
    # And create the easy to read asterisk matrix for men and women
    {
     
        menResultsBloodBonferroniDF         = menResultsBloodDF
        menResultsBloodBenjaminiDF          = menResultsBloodDF
        menAsteriskResultsBloodDF           = menResultsBloodDF
        menAsteriskResultsBloodBonferroniDF = menResultsBloodDF
        menAsteriskResultsBloodBenjaminiDF  = menResultsBloodDF
        
        womenResultsBloodBonferroniDF         = womenResultsBloodDF
        womenResultsBloodBenjaminiDF          = womenResultsBloodDF
        womenAsteriskResultsBloodDF           = womenResultsBloodDF
        womenAsteriskResultsBloodBonferroniDF = womenResultsBloodDF
        womenAsteriskResultsBloodBenjaminiDF  = womenResultsBloodDF    
    
        for(j in 1:totalBloodColumns){
    
            menResultsBloodBenjaminiDF[,j+1]  = p.adjust(menResultsBloodBenjaminiDF[,j+1],  method = "fdr")  
            menResultsBloodBonferroniDF[,j+1] = p.adjust(menResultsBloodBonferroniDF[,j+1], method = "bonferroni") 
        
            menAsteriskResultsBloodDF[,j+1]           = getAsterkisPValue(menResultsBloodDF[,j+1])
            menAsteriskResultsBloodBenjaminiDF[,j+1]  = getAsterkisPValue(menResultsBloodBenjaminiDF[,j+1])
            menAsteriskResultsBloodBonferroniDF[,j+1] = getAsterkisPValue(menResultsBloodBonferroniDF[,j+1])
                    
            womenResultsBloodBenjaminiDF[,j+1]  = p.adjust(womenResultsBloodBenjaminiDF[,j+1],  method = "fdr")  
            womenResultsBloodBonferroniDF[,j+1] = p.adjust(womenResultsBloodBonferroniDF[,j+1], method = "bonferroni") 
        
            womenAsteriskResultsBloodDF[,j+1]           = getAsterkisPValue(womenResultsBloodDF[,j+1])
            womenAsteriskResultsBloodBenjaminiDF[,j+1]  = getAsterkisPValue(womenResultsBloodBenjaminiDF[,j+1])
            womenAsteriskResultsBloodBonferroniDF[,j+1] = getAsterkisPValue(womenResultsBloodBonferroniDF[,j+1])
            
        }

        # Clean the matrixes from ns for all rows (no column
        
        # For each row
        deleteTheseSimpleRows     = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBenjaminiRows  = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBonferroniRows = rep(FALSE,TOTAL_BIOMARKERS)
        for(i in 1:TOTAL_BIOMARKERS){
            
            if(sum(menAsteriskResultsBloodDF[i,] == 'ns')           == totalBloodColumns) deleteTheseSimpleRows[i]     = TRUE
            if(sum(menAsteriskResultsBloodBenjaminiDF[i,]  == 'ns') == totalBloodColumns) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(menAsteriskResultsBloodBonferroniDF[i,] == 'ns') == totalBloodColumns) deleteTheseBonferroniRows[i] = TRUE
            
        }
                
        menAsteriskResultsBloodDF           = menAsteriskResultsBloodDF[!deleteTheseSimpleRows,]
        menAsteriskResultsBloodBenjaminiDF  = menAsteriskResultsBloodBenjaminiDF[!deleteTheseBenjaminiRows,]
        menAsteriskResultsBloodBonferroniDF = menAsteriskResultsBloodBonferroniDF[!deleteTheseBonferroniRows,]
        
        deleteTheseSimpleRows     = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBenjaminiRows  = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBonferroniRows = rep(FALSE,TOTAL_BIOMARKERS)
        for(i in 1:TOTAL_BIOMARKERS){
            
            if(sum(womenAsteriskResultsBloodDF[i,] == 'ns')           == totalBloodColumns) deleteTheseSimpleRows[i]     = TRUE
            if(sum(womenAsteriskResultsBloodBenjaminiDF[i,]  == 'ns') == totalBloodColumns) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(womenAsteriskResultsBloodBonferroniDF[i,] == 'ns') == totalBloodColumns) deleteTheseBonferroniRows[i] = TRUE
            
        }
        
        womenAsteriskResultsBloodDF           = womenAsteriskResultsBloodDF[!deleteTheseSimpleRows,]
        womenAsteriskResultsBloodBenjaminiDF  = womenAsteriskResultsBloodBenjaminiDF[!deleteTheseBenjaminiRows,]
        womenAsteriskResultsBloodBonferroniDF = womenAsteriskResultsBloodBonferroniDF[!deleteTheseBonferroniRows,]
        
        
        # For each column
        deleteTheseSimpleColumns     = rep(FALSE,totalBloodColumns+1)
        deleteTheseBenjaminiColumns  = rep(FALSE,totalBloodColumns+1)
        deleteTheseBonferroniColumns = rep(FALSE,totalBloodColumns+1)
        for(i in 1:totalBloodColumns){
            
            if(sum(menAsteriskResultsBloodDF[,(i+1)] == 'ns')           == nrow(menAsteriskResultsBloodDF))           deleteTheseSimpleColumns[i+1]     = TRUE
            if(sum(menAsteriskResultsBloodBenjaminiDF[,(i+1)]  == 'ns') == nrow(menAsteriskResultsBloodBenjaminiDF))  deleteTheseBenjaminiColumns[i+1]  = TRUE
            if(sum(menAsteriskResultsBloodBonferroniDF[,(i+1)] == 'ns') == nrow(menAsteriskResultsBloodBonferroniDF)) deleteTheseBonferroniColumns[i+1] = TRUE
            
        }
                
        menAsteriskResultsBloodDF           = menAsteriskResultsBloodDF[,!deleteTheseSimpleColumns]
        menAsteriskResultsBloodBenjaminiDF  = menAsteriskResultsBloodBenjaminiDF[,!deleteTheseBenjaminiColumns]
        menAsteriskResultsBloodBonferroniDF = menAsteriskResultsBloodBonferroniDF[,!deleteTheseBonferroniColumns]
        
        deleteTheseSimpleColumns     = rep(FALSE,totalBloodColumns+1)
        deleteTheseBenjaminiColumns  = rep(FALSE,totalBloodColumns+1)
        deleteTheseBonferroniColumns = rep(FALSE,totalBloodColumns+1)
        for(i in 1:totalBloodColumns){
            
            if(sum(womenAsteriskResultsBloodDF[,(i+1)] == 'ns')           == nrow(womenAsteriskResultsBloodDF))           deleteTheseSimpleColumns[i+1]     = TRUE
            if(sum(womenAsteriskResultsBloodBenjaminiDF[,(i+1)]  == 'ns') == nrow(womenAsteriskResultsBloodBenjaminiDF))  deleteTheseBenjaminiColumns[i+1]  = TRUE
            if(sum(womenAsteriskResultsBloodBonferroniDF[,(i+1)] == 'ns') == nrow(womenAsteriskResultsBloodBonferroniDF)) deleteTheseBonferroniColumns[i+1] = TRUE
            
        }
                
        womenAsteriskResultsBloodDF           = womenAsteriskResultsBloodDF[,!deleteTheseSimpleColumns]
        womenAsteriskResultsBloodBenjaminiDF  = womenAsteriskResultsBloodBenjaminiDF[,!deleteTheseBenjaminiColumns]
        womenAsteriskResultsBloodBonferroniDF = womenAsteriskResultsBloodBonferroniDF[,!deleteTheseBonferroniColumns]
                

           
    }
    
    
    # Change all ns to nothing for easy reading
    menAsteriskResultsBloodBonferroniDF[menAsteriskResultsBloodBonferroniDF     == "ns"] = ""
    womenAsteriskResultsBloodBonferroniDF[womenAsteriskResultsBloodBonferroniDF == "ns"] = ""
    
    # Write to latex
    writeTableLATEX(menAsteriskResultsBloodBonferroniDF, BIOMARKERS_FOLDER_TABLES_BLOOD,  tableCaption = "Biomarkers that are statistically significant with respect the blood variables in men, after applying Bonferroni correction. Non-significant values appears as a white space for easy reading.",
                    overrideTableName = "BiomarkersBloodBonferroniMen", heightProportion = 0.1, rotateColumnHeaders = TRUE)
    
    writeTableLATEX(womenAsteriskResultsBloodBonferroniDF, BIOMARKERS_FOLDER_TABLES_BLOOD,  tableCaption = "Biomarkers that are statistically significant with respect the blood variables in women, after applying Bonferroni correction. Non-significant values appears as a white space for easy reading.",
                    overrideTableName = "BiomarkersBloodBonferroniWomen", heightProportion = 0.1, rotateColumnHeaders = TRUE)  
    
    }
    
    
}



# ------------------------------------------------------------------------------
# Check homophily by variable, so we can justify stratifying by sex and school
# in the next step.
# ------------------------------------------------------------------------------
{
    # Not necessary, linked reference to the STAPH paper	
}

#---------------------------------------------------------------------------
# Check my friends average biomarker (stratify by sex and highschool) againts
# my own biomarkers levels. Check for R2 and p-value  (FIX THIS!)
#---------------------------------------------------------------------------
{
    # I need to create the friendship matrix for later
    myOverallFriendshipMatrix = getFriendshipMatrix(overallEdgesDF, TOTAL_PEOPLE)
    
    # First, let do it without stratification
    # There is nothing relevant in here
    {
    
        # Prepare the DF where we accumulate the results
        resultsSimpleDF           = DF(TOTAL_BIOMARKERS,5)
        colnames(resultsSimpleDF) = c("Protein", "Sex", "Highschool", "R2","Pvalue")
        
        # Get how many people have friends
        peopleWithFriendsDF    = completeTable[completeTable$OverallConnections > 0,]
        totalPeopleWithFriends = nrow(peopleWithFriendsDF)
        
        # Init the counter for the DF where we write results
        myCounter = 1
        
        # For each biomarker
        for(i in 1:TOTAL_BIOMARKERS){
        
            # Prepare the blank DF where we put the dataset
            # Some of these can be NA since not everyone friend has the biomarkers
            # analysis done (or could even be under LOD)
            regressionAllDF = DF(totalPeopleWithFriends,2)
            colnames(regressionAllDF) = c("MyFriendsAverage", "MyLevel")
            
            currentBiomarkerName = biomarkersMetadataDF$Acronym[i]
            
            
            # For each person
            for(j in 1:totalPeopleWithFriends){
            
                # Get the person relevant information
                currentPersonID       = peopleWithFriendsDF[j, IDIndex     ]
                currentPersonBioLevel = peopleWithFriendsDF[j, NDLIndex+i-1]
                
                # Find the friends surrounding you (you nominate or nominate you)
                currentPersonFriends  = unique(c(getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[4]],
                                                 getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[5]]))
                    
                currentTotalFriends   = length(currentPersonFriends) 
                
                currentPersonFriendsAverageBioLevel = 0
                currentPersonFriendsNonNAFriends    = 0
                
                # Find the friends average by accumulating into the variable
                # but check that you are not adding NA values
                for(k in 1:currentTotalFriends){
                    
                    # Get the biomarker level
                    currentFriendID       = currentPersonFriends[k]
                    currentFriendBioLevel = peopleWithFriendsDF[k, NDLIndex+i-1]
                    
                    # If the biomarker is non-NA, added to the average
                    if(!is.na(currentFriendBioLevel)){
                        
                        currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel + currentFriendBioLevel
                        currentPersonFriendsNonNAFriends    = currentPersonFriendsNonNAFriends    + 1
                    }
                    
                }
                    
                # If we have friends with non NA values, add everything to the
                # proper DF, otherwise skip and leave a NA line, that is fine
                if(currentPersonFriendsNonNAFriends>0){
                    
                    currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel / currentPersonFriendsNonNAFriends
                    
                    regressionAllDF[j,1] = currentPersonFriendsAverageBioLevel
                    regressionAllDF[j,2] = currentPersonBioLevel
                }
                    
                 
                    
            }
                
            # Clean the data from NA data
            regressionAllDF = regressionAllDF[   !is.na(regressionAllDF[,1]),  ]
                    
            # Run the model
            # Plot the graph
                    
            currentPlotTitle     = paste0(currentBiomarkerName," for men and women, all highschools")
            currentTableOverride = paste0(currentBiomarkerName,"_men_women_allHS")
            
            myBioModel = doRegressionPlot(regressionAllDF, 1, 2, BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_NOHS,
                                          plotTitle = currentPlotTitle,
                                          overrideTableName = currentTableOverride,
                                          plotTheme = "regression",
                                          overrideImageWidth = 12)            
            
            
            bestModel          = myBioModel[[4]]
            minimumPValue      = myBioModel[[5]]$`p-value`[bestModel]
            maximumR2          = myBioModel[[5]]$R2[bestModel]
                    
            # Add results to dataframe        
            resultsSimpleDF[myCounter,1] = currentBiomarkerName
            resultsSimpleDF[myCounter,2] = "Both"
            resultsSimpleDF[myCounter,3] = "All"
            resultsSimpleDF[myCounter,4] = maximumR2
            resultsSimpleDF[myCounter,5] = minimumPValue
                    
            myCounter = myCounter + 1
            
            
        }
                
    }
    
    # Now we do the same but stratifying by highschool and sex
    {
        
        # Get the highschools info
        myHighschools    = levels(completeTable[,highSchoolIndex])
        totalHighschools = length(myHighschools)
        
        # Sex info without highschool stratification
        resultsMenWomenDF           = DF(TOTAL_BIOMARKERS*2,5)
        colnames(resultsMenWomenDF) = c("Protein", "Sex", "Highschool","R2","Pvalue")
        mySexCounter                = 1
        
        # Prepare the DF where we accumulate the results
        resultsComplexDF           = DF(TOTAL_BIOMARKERS*2*totalHighschools,5)
        colnames(resultsComplexDF) = c("Protein", "Sex", "Highschool","R2","Pvalue")
        myCounter                  = 1
        
        # For men and women
        for(i in 1:2){
        
            currentSex = "Man"
            if(i == 2) currentSex = "Woman"

            # We are also going to see the result for no high school stratification
            {
            
                # Get the table of people within the same sex and highschool
                currentSubtable        = completeTable[ completeTable[,sexIndex] == currentSex, ]
                peopleWithFriendsDF    = currentSubtable[currentSubtable[ , overallConnectionsIndex] > 0,]
                totalPeopleWithFriends = nrow(peopleWithFriendsDF)            
                
                # For each biomarker
                for(k in 1:TOTAL_BIOMARKERS){
                    
                    # Prepare the blank DF where we put the dataset
                    # Some of these can be NA since not everyone friend has the biomarkers
                    # analysis done (or could even be under LOD)
                    regressionAllDF = DF(totalPeopleWithFriends,2, defaultValue = NA)
                    colnames(regressionAllDF) = c("MyFriendsAverage", "MyLevel")
                    
                    currentBiomarkerName = biomarkersMetadataDF$Protein[k]
                    
                    # For each person, find their friends averages
                    for(x in 1:totalPeopleWithFriends){
                        
                        # Get the person relevant information
                        currentPersonID       = peopleWithFriendsDF[x, IDIndex     ]
                        currentPersonBioLevel = completeTable[currentPersonID, NDLIndex + k - 1]
                        
                        # If your biomarker is not NA, do everything
                        # otherwise skip to the next person
                        if(!is.na(currentPersonBioLevel)){
                        
                            # Find the friends surrounding you (you nominate or nominate you)
                            currentPersonFriends  = unique(c(getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[4]],
                                                             getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[5]]))
                            
                            currentTotalFriends   = length(currentPersonFriends) 
                            
                            currentPersonFriendsAverageBioLevel = 0
                            currentPersonFriendsNonNAFriends    = 0
                            
                            # Find the friends average by accumulating into the variable
                            # but check that you is a valid friend.
                            #     Same sex
                            #     Same highschool
                            #     No NA values
                            for(y in 1:currentTotalFriends){
                                
                                # Get the biomarker level
                                currentFriendID       = currentPersonFriends[y]
                                currentFriendBioLevel = completeTable[currentFriendID, NDLIndex + k - 1]
                                currentFriendSex      = as.character(completeTable[currentFriendID,sexIndex])
                                currentFriendHS       = as.character(completeTable[currentFriendID,highSchoolIndex])
                                
                                # If the biomarker is non-NA, added to the average
                                if(!is.na(currentFriendBioLevel)  & 
                                   currentFriendSex == currentSex ){
                                    
                                    currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel + currentFriendBioLevel
                                    currentPersonFriendsNonNAFriends    = currentPersonFriendsNonNAFriends    + 1
                                }
                                
                            }
                            
                            # If we have friends with non NA values, add everything to the
                            # proper DF, otherwise skip and leave a NA line, that is fine
                            if(currentPersonFriendsNonNAFriends>0){
                                
                                currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel / currentPersonFriendsNonNAFriends
                                
                                regressionAllDF[x,1] = currentPersonFriendsAverageBioLevel
                                regressionAllDF[x,2] = currentPersonBioLevel
                            }
                            
                                
                        }
                    
                    }
                
                    # Clean the data from NA data
                    regressionAllDF = regressionAllDF[   !is.na(regressionAllDF[,1]),  ]
                    
                    # Run the model
                    # Plot the graph
                    
                    currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " in all HS")
                    currentTableOverride = paste0(currentBiomarkerName,"_",currentSex,"_allHS")
                    
                    
                    myBioModel = doRegressionPlot(regressionAllDF, 1, 2, BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_NOHSYESSEX,
                                                  plotTitle = currentPlotTitle,
                                                  overrideTableName = currentTableOverride,
                                                  plotTheme = "regression",
                                                  overrideImageWidth = 12)            
                    
                    bestModel          = myBioModel[[4]]
                    minimumPValue      = myBioModel[[5]]$`p-value`[bestModel]
                    maximumR2          = myBioModel[[5]]$R2[bestModel]
                            
                    # Add results to dataframe        
                    resultsMenWomenDF[myCounter,1] = currentBiomarkerName
                    resultsMenWomenDF[myCounter,2] = currentSex
                    resultsMenWomenDF[myCounter,3] = "All"
                    resultsMenWomenDF[myCounter,4] = maximumR2
                    resultsMenWomenDF[myCounter,5] = minimumPValue                    
                    
                    mySexCounter = mySexCounter + 1
                    
        }    
                
            }
            
            # For high school stratification
            for(j in 1:totalHighschools){
                
                currentHighschool = myHighschools[j]
                
                # Get the table of people within the same sex and highschool
                currentSubtable = completeTable[ completeTable[,sexIndex]        == currentSex &
                                                 completeTable[,highSchoolIndex] == currentHighschool, ]
                
                peopleWithFriendsDF    = currentSubtable[currentSubtable[ , overallConnectionsIndex] > 0,]
                totalPeopleWithFriends = nrow(peopleWithFriendsDF)
                
                # For each biomarker
                for(k in 1:TOTAL_BIOMARKERS){
                    
                    # Prepare the blank DF where we put the dataset
                    # Some of these can be NA since not everyone friend has the biomarkers
                    # analysis done (or could even be under LOD)
                    regressionAllDF = DF(totalPeopleWithFriends,2, defaultValue = NA)
                    colnames(regressionAllDF) = c("MyFriendsAverage", "MyLevel")
                    
                    currentBiomarkerName = biomarkersMetadataDF$Protein[k]
                    
                    # For each person, find their friends averages
                    for(x in 1:totalPeopleWithFriends){
                        
                        # Get the person relevant information
                        currentPersonID       = peopleWithFriendsDF[x, IDIndex     ]
                        currentPersonBioLevel = completeTable[currentPersonID, NDLIndex + k - 1]
                        
                        # If your biomarker is not NA, do everything
                        # otherwise skip to the next person
                        if(!is.na(currentPersonBioLevel)){
                        
                            # Find the friends surrounding you (you nominate or nominate you)
                            currentPersonFriends  = unique(c(getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[4]],
                                                             getFrienshipTypes(currentPersonID, myOverallFriendshipMatrix)[[5]]))
                            
                            currentTotalFriends   = length(currentPersonFriends) 
                            
                            currentPersonFriendsAverageBioLevel = 0
                            currentPersonFriendsNonNAFriends    = 0
                            
                            # Find the friends average by accumulating into the variable
                            # but check that you is a valid friend.
                            #     Same sex
                            #     Same highschool
                            #     No NA values
                            for(y in 1:currentTotalFriends){
                                
                                # Get the biomarker level
                                currentFriendID       = currentPersonFriends[y]
                                currentFriendBioLevel = completeTable[currentFriendID, NDLIndex + k - 1]
                                currentFriendSex      = as.character(completeTable[currentFriendID,sexIndex])
                                currentFriendHS       = as.character(completeTable[currentFriendID,highSchoolIndex])
                                
                                # If the biomarker is non-NA, added to the average
                                if(!is.na(currentFriendBioLevel)  & 
                                   currentFriendSex == currentSex &
                                   currentFriendHS  == currentHighschool){
                                    
                                    currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel + currentFriendBioLevel
                                    currentPersonFriendsNonNAFriends    = currentPersonFriendsNonNAFriends    + 1
                                }
                                
                            }
                            
                            # If we have friends with non NA values, add everything to the
                            # proper DF, otherwise skip and leave a NA line, that is fine
                            if(currentPersonFriendsNonNAFriends>0){
                                
                                currentPersonFriendsAverageBioLevel = currentPersonFriendsAverageBioLevel / currentPersonFriendsNonNAFriends
                                
                                regressionAllDF[x,1] = currentPersonFriendsAverageBioLevel
                                regressionAllDF[x,2] = currentPersonBioLevel
                            }
                            
                                
                        }
                    
                    }
                
                    # Clean the data from NA data
                    regressionAllDF = regressionAllDF[   !is.na(regressionAllDF[,1]),  ]
                    
                    # Run the model
                    # Plot the graph
                    
                    currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " in ", currentHighschool)
                    currentTableOverride = paste0(currentBiomarkerName,"_",currentSex,"_",currentHighschool)
                    
                    
                    # Find the right folder
                    currentSubFolder = BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS1
                    if(currentHighschool == "H2") currentSubFolder = BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS2
                    if(currentHighschool == "H3") currentSubFolder = BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS3
                    if(currentHighschool == "H4") currentSubFolder = BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS4
                    if(currentHighschool == "H5") currentSubFolder = BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS5
                    if(currentHighschool == "H6") currentSubFolder = BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS6
                    if(currentHighschool == "H7") currentSubFolder = BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS7
                    if(currentHighschool == "H8") currentSubFolder = BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS_HS8
                    
                    myBioModel = doRegressionPlot(regressionAllDF, 1, 2, currentSubFolder,
                                                  plotTitle = currentPlotTitle,
                                                  overrideTableName = currentTableOverride,
                                                  plotTheme = "regression",
                                                  overrideImageWidth = 12)            
                    
                    bestModel          = myBioModel[[4]]
                    minimumPValue      = myBioModel[[5]]$`p-value`[bestModel]
                    maximumR2          = myBioModel[[5]]$R2[bestModel]
                            
                    # Add results to dataframe        
                    resultsComplexDF[myCounter,1] = currentBiomarkerName
                    resultsComplexDF[myCounter,2] = currentSex
                    resultsComplexDF[myCounter,3] = currentHighschool
                    resultsComplexDF[myCounter,4] = maximumR2
                    resultsComplexDF[myCounter,5] = minimumPValue                      
                    
                    myCounter = myCounter + 1
                    
        }
        
    }
    
    
		}
    
    }

    
    ##############################################################################
    # You are here, the program works, but is not being saved into the DF
    ##############################################################################
    
    # Write to disk
    # (but round numbers first)
    
    
    # ---- In the no stratification table there is nothing interesting
    resultsSimpleRoundedDF    = resultsSimpleDF
    resultsSimpleRoundedDF$R2 = round(resultsSimpleRoundedDF$R2, 2)
    resultsSimpleRoundedDF$P2 = "A"
    for( i in 1:nrow(resultsSimpleRoundedDF)){

        resultsSimpleRoundedDF$P2[i] = getAsterkisPValue(resultsSimpleRoundedDF$Pvalue[i])

    }

    resultsSimpleRoundedDF$Pvalue = NULL    
    colnames(resultsSimpleRoundedDF)[5] = "p-value"
    write.csv2(resultsSimpleRoundedDF, SCATTER_SIMPLE_ROUNDED_FILEPATH)
    write.csv2(resultsSimpleDF,        SCATTER_SIMPLE_FILEPATH)
        
    
    # ---- In the stratification by sex only (fix this)
    
    resultsMenWomenDF
    
    
    # ---- In the stratification by sex and high school
    {
	    	
    	
    	# ---- No correction
    	
	    resultsComplexRoundedDF    = resultsComplexDF
	    resultsComplexRoundedDF$R2 = round(resultsComplexRoundedDF$R2, 2)
	    resultsComplexRoundedDF$P2 = "A"
	    for( i in 1:nrow(resultsComplexRoundedDF)){
	
	        resultsComplexRoundedDF$P2[i] = getAsterkisPValue(resultsComplexRoundedDF$Pvalue[i])
	
	    }
	
	    resultsComplexRoundedDF$Pvalue = NULL    
	    colnames(resultsComplexRoundedDF)[5] = "Pvalue"
	    write.csv2(resultsComplexRoundedDF, SCATTER_STRATOS_ROUNDED_FILEPATH)
	    write.csv2(resultsComplexDF,        SCATTER_STRATOS_FILEPATH)    
        	
	    
	    
    
	    # We have waaaay too many significances, let delete those that are ns 
	    # and include that in the paper, the rest of the raw result can be found
	    # at the github page
	    
	    # -- Delete those that are ns
	    resultsMyVSFriendBioPaper = resultsComplexRoundedDF
	    resultsMyVSFriendBioPaper = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$Pvalue != "ns",]
	    # -- Delete those that R2 is smaller than 0.1
	    resultsMyVSFriendBioPaper = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$R2 > 0.2,]	    
	    # -- Sort by Sex, Highschool, and Protein in that order
	    resultsMyVSFriendBioPaper = resultsMyVSFriendBioPaper[order(resultsMyVSFriendBioPaper$Sex,resultsMyVSFriendBioPaper$Highschool, resultsMyVSFriendBioPaper$Protein),]
	    # -- The table is too big, divide into two tables, for men and women
	    resultsMyVSFriendBioPaperMen    = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$Sex == "Man",]
	    resultsMyVSFriendBioPaperWomen  = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$Sex == "Woman",]    	
	    

    	# Write to disk
	    writeTableLATEX(resultsMyVSFriendBioPaperMen, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Protein for men, which are similar to your male friend, stratify by highschool, with R2 > 0.2",
	                    overrideTableName = "biofriendsMalesHighschool", widthProportion = 0.5, heightProportion = 0.4, headerColor = COLOR_MAN)
    
	    writeTableLATEX(resultsMyVSFriendBioPaperWomen, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Protein for women, which are similar to your female friend, stratify by highschool, with R2 > 0.2",
	                    overrideTableName = "biofriendsFemalesHighschool", widthProportion = 0.5, heightProportion = 0.4, headerColor = COLOR_WOMAN)
	    
    }

    
    

    
    
    
}


#---------------------------------------------------------------------------
# Check friends distances
#---------------------------------------------------------------------------
{
    # This variables are for the final results
    menFriendDistances  = NA
    menFriendsTotal     = NA
    menEnemiesDistances = NA
    menEnemiesTotal     = NA
    menRatios           = NA
    
    womenFriendDistances  = NA
    womenFriendsTotal     = NA
    womenEnemiesDistances = NA
    womenEnemiesTotal     = NA
    womenRatios           = NA
    
    # Prepare the tables with the NDL values for men and women
    # Include people with non NA values only
    {
        menNDLOnlyTable   = menOnlyTable[,  c(1,NDLIndex:(NDLIndex+TOTAL_BIOMARKERS-1))]
        womenNDLOnlyTable = womenOnlyTable[,c(1,NDLIndex:(NDLIndex+TOTAL_BIOMARKERS-1))]
        
        keepTheseMen            = !is.na(menBiomarkersNDLTable[,2])
        keepTheseWomen          = !is.na(womenBiomarkersNDLTable[,2])
        
        menBiomarkersNDLTable   = menBiomarkersNDLTable[keepTheseMen,]
        keepMenIDs              = menBiomarkersNDLTable[keepTheseMen,]$ID
        totalMen                = length(keepMenIDs)
        
        womenBiomarkersNDLTable = womenBiomarkersNDLTable[keepTheseWomen,]
        keepWomenIDs            = womenBiomarkersNDLTable[keepTheseWomen,]$ID
        totalWomen              = length(keepWomenIDs)
        
        sexNDLTablesList      = newList(2)
        sexNDLTablesList[[1]] = menOnlyTable
        sexNDLTablesList[[2]] = womenOnlyTable
    }
    
    # We want to continue with only people who has
    #     - 2 friends of the same sex, who don't have NA values
    {
    
        # Repeat for men and women
        for(z in 1:2){
            
            currentSex = "Man"
            if(z == 2) currentSex = "Woman"
            
            currentTable   = sexNDLTablesList[[z]]
            totalTableRows = nrow(currentTable)
            
            currentTableIDs = currentTable$ID
            
            keepTheseRows  = rep(TRUE, totalTableRows)
            
            # For each person in this table
            for(j in 1:totalTableRows){
                
                currentID = currentTableIDs[j]
                
                # Find the friends for this ID with same sex
                currentSameSexFriends = unique(c(getFrienshipTypes(currentID, myOverallFriendshipMatrix)[[4]],
                                                 getFrienshipTypes(currentID, myOverallFriendshipMatrix)[[5]]))
                currentSameSexFriends = currentSameSexFriends[currentSameSexFriends %in% currentTableIDs]
                totalCurrentFriends   = length(currentSameSexFriends)
                
                # If you have 2 friends or more you stay, otherwise, marked
                # for deletion
                if(totalCurrentFriends < 2){
                    
                    keepTheseRows[j] = FALSE
                    
                }
                
            }
            
            # Update the table
            currentTable          = currentTable[keepTheseRows,]
            sexNDLTablesList[[z]] = currentTable
            totalTableRows        = nrow(currentTable)
            currentTableIDs       = currentTable$ID
            
            # Make the list of friends for each person
            # This is superannoying to do in R since it doesn't have pointers. This is
            # completely unnaceptable performance wise, and this really need to be done in
            # C++. I need to start using Rcpp and we done with all this crap.
            {
                
                popularityTable           = DF(totalTableRows,2)
                colnames(popularityTable) = c("ID", "Total Friends")
                popularityLists           = newList(totalTableRows)
                
                for(k in 1:totalTableRows){
                    
                    currentID = currentTable$ID[k]
                    
                    # Get the friends of this person
                    myCurrentFriends     = unique(c(getFrienshipTypes(currentID, myOverallFriendshipMatrix)[[4]],
                                                    getFrienshipTypes(currentID, myOverallFriendshipMatrix)[[5]]))
                    
                    # Filter out people of different sex and people who are same sex
                    # but with NA values
                    myCurrentFriends = myCurrentFriends[myCurrentFriends %in% currentTableIDs]
                    
                    # Get the total
                    totalCurrentFriends  = length(myCurrentFriends)
                    
                    # Make the list of friends, added to the big list, and register the total
                    popularityTable[k,1] = currentID
                    popularityTable[k,2] = totalCurrentFriends
                    
                    if(totalCurrentFriends > 0){
                        
                        popularityLists[[k]] = myCurrentFriends
                        
                    }
                    
                }
                
            }
            
            # Now, for each person, we are going to measure the distance to 
            # friends biomarkers, and compare that to the distance of people
            # who are not your friends biomarkers.
            #
            # If friendhship has anyhthing to do with this, the distance should
            # be smaller in avarage for the friend group
            {
                yesFriendsAllDistancesVector = rep(0, TOTAL_BIOMARKERS)
                yesFriendsAllDistancesIndex  = rep(1, TOTAL_BIOMARKERS)
                
                nonFriendsAllDistancesVector = rep(0, TOTAL_BIOMARKERS)
                nonFriendsAllDistancesIndex  = rep(1, TOTAL_BIOMARKERS)
                
                # For each person, and for each biomarkers
                for(i in 1:totalTableRows){
                    
                    print(   round(100*i/totalTableRows,2)   )
                    
                    # Get ID
                    myCurrentID = currentTable$ID[i]
                    
                    # Get the friends of this person
                    listIndex           = row.names(popularityTable[popularityTable$ID == myCurrentID,])
                    myCurrentFriends    = popularityLists[[as.numeric(listIndex)]]
                    totalCurrentFriends = popularityTable[popularityTable$ID == myCurrentID,2]
                    
                    # If you have more than one friend, find the total distance, otherwise, set it to 0
                    if(totalCurrentFriends > 0){
                        
                        # Let's find the average distance for each biomarker for each friend
                        for(j in 1:TOTAL_BIOMARKERS){
                            
                            # In here we can set up the limit for valid biomarkers,
                            # under LOD or not. Until that is debated, keep going.
                            myBiomarker     = completeTable[myCurrentID,(NDLIndex+j-1)]
                            currentDistance = 0
                            
                            # If I don't have any value, skip this person
                            if(!is.na(myBiomarker)){
                                
                                # For each of my friends
                                for(k in 1:totalCurrentFriends){
                                    
                                # Get the ID
                                myFriendID        = myCurrentFriends[k]
                                    
                                # IDs are consecutive, and we only need to measure the edges once.
                                # So if your friend ID is smaller than you ID, skip it.
                                if(myFriendID < myCurrentID){
                                        
                                    # Get your friend biomarker
                                    myFriendBiomarker = completeTable[myFriendID,(NDLIndex+j-1)]    
                                    
                                    # of course some values are NA,
                                    # and the stupid R default is to make X+NA = NA,
                                    # which makes this code more unreadable
                                    if(!is.na(myFriendBiomarker)){
                                            
                                        # We don't standarize the distance with 
                                        # respect my biomarker here
                                        currentBioDistance = (myBiomarker - myFriendBiomarker)^2
                                        
                                        yesFriendsAllDistancesVector[j] = yesFriendsAllDistancesVector[j] + currentBioDistance
                                        yesFriendsAllDistancesIndex[j]  = yesFriendsAllDistancesIndex[j]  + 1
                                            
                                    }
                                        
                                }
                                    
                            }
                            
                                # For everybody else how is not my friend
                                for(k in 1:totalTableRows){
                                
                                # Get the ID
                                myNonFriendID = currentTable$ID[k]
                                
                                # If this is friend, skip it
                                if(myNonFriendID %in% myCurrentFriends == FALSE){
                                    
                                    # IDs are consecutive, and we only need to measure the edges once.
                                    # So if your friend ID is smaller than you ID, skip it.
                                    if(myNonFriendID < myCurrentID){
                                        
                                        # Get your friend biomarker
                                        myNonFriendBiomarker = completeTable[myNonFriendID,(NDLIndex+j-1)]    
                                        
                                        # of course some values are NA, and the stupid R default is to make X+NA = NA,
                                        # which makes this code more unreadable
                                        if(!is.na(myNonFriendBiomarker)){
                                            
                                            # We don't standarize the distance with respect my biomarker here
                                            currentBioDistance = (myBiomarker - myNonFriendBiomarker)^2
                                            
                                            nonFriendsAllDistancesVector[j] = nonFriendsAllDistancesVector[j] + currentBioDistance
                                            nonFriendsAllDistancesIndex[j]  = nonFriendsAllDistancesIndex[j]  + 1
                                            
                                        }
                                        
                                    }
                                    
                                     
                                }
                                    
                            }
                                
                            }
                                
                                
                        }
                            
                            
                    }            
                    
                }
                
                # Finally get the total Score
                yesFriendsAverageDistanceVector = yesFriendsAllDistancesVector/yesFriendsAllDistancesIndex
                nonFriendsAverageDistanceVector = nonFriendsAllDistancesVector/nonFriendsAllDistancesIndex
               
                if(currentSex == "Man"){
                    
                    menFriendDistances  = yesFriendsAllDistancesVector
                    menFriendsTotal     = yesFriendsAllDistancesIndex
                    menEnemiesDistances = nonFriendsAllDistancesVector
                    menEnemiesTotal     = nonFriendsAllDistancesIndex
                    menRatios           = nonFriendsAverageDistanceVector/yesFriendsAverageDistanceVector
                    
                }
                else{
                    
                    womenFriendDistances  = yesFriendsAllDistancesVector
                    womenFriendsTotal     = yesFriendsAllDistancesIndex
                    womenEnemiesDistances = nonFriendsAllDistancesVector
                    womenEnemiesTotal     = nonFriendsAllDistancesIndex
                    womenRatios           = nonFriendsAverageDistanceVector/yesFriendsAverageDistanceVector
                }
                
            }
        }
            
    }
    
    
    distancesDF = DF(TOTAL_BIOMARKERS,3)
    colnames(distancesDF) = c("Protein", "Men", "Women")
    for(i in 1:TOTAL_BIOMARKERS){
        
        distancesDF[i,1] = biomarkersMetadataDF$Protein[i]
        distancesDF[i,2] = round(menRatios[i],2)
        distancesDF[i,3] = round(womenRatios[i],2)
    }
    
    write.csv2(distancesDF, DISTANCES_FILEPATH)
    
    writeTableLATEX(distancesDF, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Ratio of square distances between each person friend and each person non-friends, stratify by sex. Values greater than 1 indicate that friends have a smaller difference in biomarkers levels compared with their non-friends counterpart.",
                    overrideTableName = "distancesBiomarkers", widthProportion = 0.4, heightProportion = 0.5)
 
    # How many have a distance greater than 1.1 or lower than 0.9
    sum(distancesDF$Men > 1.1)
    sum(distancesDF$Men < 0.9)
    sum(distancesDF$Women > 1.1)
    sum(distancesDF$Women < 0.9)
}

#---------------------------------------------------------------------------
# Make the summary table of tables
#---------------------------------------------------------------------------
{

        writeImageLATEX2("/home/gromenawer/Desktop/Amalgamlab/mimisbrunnr/results/biomarkers/tables/summaries/ReducedResults.png", LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_SUMMARIES, 
                         captionText   = "Summary of significant biomarkers corrected for Bonferroni, with R2 > 0.2, and distance not in the range (0.9 , 1.1)",
                         overrideLabel = "fig:allSummary", 
                         pageHeight    = 1, overrideFloat = TRUE)	
	
		
}

#---------------------------------------------------------------------------
# Spread of BMI in the network
#---------------------------------------------------------------------------
{
    
    # BMI bias analysis by simulations
    {
        
        # Get all edges into a list
        edgeList      = newList(TOTAL_NETWORKS)
        edgeList[[1]] = overallEdgesDF
        edgeList[[2]] = physicalEdgesDF
        edgeList[[3]] = schoolEdgesDF
        edgeList[[4]] = sportsEdgesDF
        edgeList[[5]] = homeEdgesDF
        edgeList[[6]] = otherEdgesDF     
        
        # Run the bias analysis with respect the carrier variables
        CarrierBiasDF = doCategoricalBiasAnalysis(completeTable,
                                                  edgeList,
                                                  BMICatIndex,
                                                  TOTAL_SIMULATIONS,
                                                  listOfNetworksNames = NETWORK_NAMES)

        # Add the significances asterisk and save it into disk
        CarrierBiasDF2 = CarrierBiasDF[[1]]
        CarrierBiasDF3 = CarrierBiasDF[[1]]
        
        colnames(CarrierBiasDF2)[11] = "Pvalue"
        colnames(CarrierBiasDF3)[11] = "Pvalue"
        
        CarrierBiasDF2$Pvalue = getAsterkisPValue(CarrierBiasDF2$Pvalue)
        CarrierBiasDF2$SD     = round(CarrierBiasDF2$SD,2) 
        
        CarrierBiasDF3$Pvalue = round(CarrierBiasDF3$Pvalue,3) 
        CarrierBiasDF3$SD     = round(CarrierBiasDF3$SD,2) 
        
        current_filepath = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK, "BMI_simulation,csv")
        write.csv2(CarrierBiasDF2,  current_filepath)
        
        
        writeTableLATEX(CarrierBiasDF2, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Simulated networks (n=1000) same-to-same relationships againts the real network same-to-same relationships. All simulated network shows bias of BMI spread in the real network.",
                       overrideTableName = "bmiSimulated", widthProportion = 0.9, heightProportion = 0.06)
        
        writeTableLATEX(CarrierBiasDF3, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Simulated networks (n=1000) same-to-same relationships againts the real network same-to-same relationships. All simulated network shows bias of BMI spread in the real network.",
                       overrideTableName = "bmiSimulatedNumbers", widthProportion = 0.9, heightProportion = 0.06)
        
        
    }
    
    # Do the loggistic model
	{
		
	    # First we need to count how many rows are we going to have in our model
	    # So how many friends each person has is the total rows, excluding people
	    # with no friends
	    {
	    
	        totalFriends = rep(0, TOTAL_PEOPLE)
	     
	        for(i in 1:TOTAL_PEOPLE){
	    
	            # Current ID
	            currentID = i
	    
	            # Find the friends surrounding you (you nominate or nominate you)
	            currentFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
	                                      getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
	            currentTotalFriends = length(currentFriends) 
	            
	            # Write that into the total friends vector for later
	            totalFriends[i]     = currentTotalFriends
	
	        }
	               
	    }
	    
	    # Prepare the data
	    #
	    # - Count how much BMI I have and how much of BMI my friends have
	    #
	    # - Count how many people of the opposite sex you have
	    # Do the model of X: Number of people of opposite sex Y: Your BMI
	    {
	    
	        totalRelationships      = sum(totalFriends)
	        BMIRelationDF           = DF(totalRelationships, 6, 0)
	        colnames(BMIRelationDF) = c("MyID", "MyFriendID", "MyBMI", "MyFriendBMI", "MySex", "MyFriendSex")
	        currentRow              = 1
	    
	        sameSexBMIDF            = DF(TOTAL_PEOPLE, 8, 0)
	        colnames(sameSexBMIDF)  = c("ID", "Sex", "Total Friends", "Total opposite friends", "BMI", "MyObesity", "TotalFriendsObesity", "MyCatObesity")
	            
	        for(i in 1:TOTAL_PEOPLE){
	    
	            # Current ID and variables
	            currentID  = i
	            currentSex = as.character(completeTable$Sex[currentID])
	            currentBMI = completeTable$BMI[currentID]
	            
	                
	            # Find the friends surrounding you (you nominate or nominate you)
	            currentFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
	                                      getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
	            currentTotalFriends = length(currentFriends) 
	
	            currentTotalOpposite = 0
	            currentTotalObese    = 0
	            
	            # If you have one or more friends
	            if(currentTotalFriends >= 1){
	                
	                # For each of the friends
	                for(j in 1:currentTotalFriends){
	
	                    # Get the friend ID
	                    currentFriendID  = currentFriends[j]
	                    currentFriendSex = as.character(completeTable$Sex[currentFriendID])
	                    currentFriendBMI = completeTable$BMI[currentFriendID]
	                    
	                    # Write the info into the relations DF
	                    BMIRelationDF[currentRow,1] = currentID
	                    BMIRelationDF[currentRow,2] = currentFriendID
	                    BMIRelationDF[currentRow,3] = currentBMI
	                    BMIRelationDF[currentRow,4] = currentFriendBMI
	                    BMIRelationDF[currentRow,5] = currentSex
	                    BMIRelationDF[currentRow,6] = currentFriendSex
	
	                    currentRow = currentRow + 1
	                    
	                    if(currentFriendSex != currentSex) currentTotalOpposite = currentTotalOpposite + 1
	                    if(!is.na(currentFriendBMI))
	                        if(currentFriendBMI >     25 ) currentTotalObese    = currentTotalObese    + 1
	
	                }
	
	            }
	
	            sameSexBMIDF[i,1] = currentID
	            sameSexBMIDF[i,2] = as.character(completeTable$Sex[currentID])
	            sameSexBMIDF[i,3] = currentTotalFriends
	            sameSexBMIDF[i,4] = currentTotalOpposite
	            sameSexBMIDF[i,5] = currentBMI
	            sameSexBMIDF[i,6] = NA
	            if(!is.na(currentBMI)){
	                sameSexBMIDF[i,6] = 0
	                sameSexBMIDF[i,8] = "Non-Obese"
	                if(currentBMI > 30){
	                    sameSexBMIDF[i,6] = 1      
	                    sameSexBMIDF[i,8] = "Obese"
	                } 
	            }
	            sameSexBMIDF[i,7] = currentTotalObese
	        }
	        
	    }
	    
	    # Clean the data a little bit
	    # - Take away all rows with NA in the BMI
	    BMIRelationDF = BMIRelationDF[!is.na(BMIRelationDF$MyBMI),]
	    BMIRelationDF = BMIRelationDF[!is.na(BMIRelationDF$MyFriendBMI),]
	    sameSexBMIDF  = sameSexBMIDF[!is.na(sameSexBMIDF$BMI),]
	    # - Take away people with no friends at all
	    #   (not to be confuse with people with zero obese friends)
	    sameSexBMIDF  = sameSexBMIDF[sameSexBMIDF$`Total Friends`>0,]
	    
	    # Do the logistic model
	    {
	    
	        ObeseityModel = glm(formula = sameSexBMIDF$MyObesity ~ sameSexBMIDF$TotalFriendsObesity, family=binomial(link="logit"))
	        
	        # Prepare the dataframes for the plots
	        logisticsPlotsDF           = DF(nrow(sameSexBMIDF), 3)
	        colnames(logisticsPlotsDF) = c("Obesity", "TotalObeseFriends", "O_Numerical")
	        logisticsPlotsDF[,1]       = sameSexBMIDF$MyCatObesity
	        logisticsPlotsDF[,2]       = sameSexBMIDF$TotalFriendsObesity
	        logisticsPlotsDF[,3]       = sameSexBMIDF$MyObesity

	        # Do the boxplots and get the averages and significant (or not) p-value
	        myBoxplotResults = doBoxPlotV2(logisticsPlotsDF, 2, BIOMARKERS_FOLDER_IMAGES_NETWORK,
	                                       groupIndex = 1, showPValues = TRUE)
	        
	        OPositiveFriendsPvalue  = myBoxplotResults[[5]][2,2]
	        ONegativeFriendsAverage = myBoxplotResults[[4]][1,2]
	        OPositiveFriendsAverage = myBoxplotResults[[4]][2,2]
	     
	        # Prepare the function for the plot
	        myObesefunction = function(x){
	            return(1/(1 + exp(-(0.27*x - 2.98) ) ) )
	        }
	        
	        
	        averageObesityIncrease = (myObesefunction(0) + myObesefunction(1) + myObesefunction(2) + myObesefunction(3) + myObesefunction(4))/5
	        
	        
	        # Now lets do the actual plot.
	        {
	        
	            # Version A) Both variables
	            myAPlot = ggplot() +
	                      # Jitter points
	                      geom_jitter(data = logisticsPlotsDF, aes(x=TotalObeseFriends,y=O_Numerical),  width = 0.25, height = 0.025, color = "red",   alpha = 0.2) +
	                      # Horizontal lines
	                      geom_hline(yintercept=0) +
	                      geom_hline(yintercept=1) +
	                      # Exponential functions
	                      geom_function(fun = myObesefunction, colour = "red",   lwd = 1, linetype = 1) +
	                      
	                      # Manual boxplots
	                      
	                      # Break the axys in integers numbers only
	                      scale_x_continuous( breaks = seq(0,4,1),   limits=c(-0.5, 4.5))    +
	                      scale_y_continuous( breaks = seq(0,1,0.2), limits=c(-0.17, 1.17))  +
	                      # Legends and axys names
	                      scale_colour_manual(name="Legend",
	                                          labels=c("Obese Legend"),
	                                          values=c("red")) +
	                      labs(x = "Total friends with BMI > 25",
	                           y = "Probability of obesity",
	                           title = "Logistic Regression between total friends who are obese and probability of obesity status",
	                           color = "Legend") +
	                      # Apply a theme to the whole plot that looks nice
	                      theme_linedraw()
	            
	                      AplotFilePath = paste0(BIOMARKERS_FOLDER_IMAGES_NETWORK, "logisticObesity.png")
	                      ggsave(AplotFilePath, plot = myAPlot, width = 8, height = 8)
	        
	        }
	        
	        # Save it to latex
	        writeImageLATEX2(AplotFilePath,  LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_NETWORK, 
	                         captionText   = "Logistic regression between number of obese friends and obesity status. The figure shows
	        	                              the increase in risk of Obesity with respect to the total number of friends with BMI > 25.
	        	                              Each dot represents a person in the data. In the X axis, we display the total number of friends with BMI > 30 each person has, and in the Y-axis whether the person with these total friends is obese (1), or non-obese (0). The logistic regression line is displayed in red, showing approximately an increase of 10% for each additional obese friend.",
	                        overrideLabel  = "fig:EgoObeseFriendsObese",
	                        pageHeight = 0.3, overrideFloat = TRUE)
	        
	           
	    }
    
	}
    
    # Check for influence of BMI with respect opposite sex
	# ---- Nothing found here
    {
     
        # Checking correlation my BMI and friends BMI
    	# ---- R² is 0, nothing here
        myBMIIncrease = doRegressionPlot(BMIRelationDF, 3, 4, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                         plotTitle = "Relationship of my BMI with my friends BMI",
                                         plotXLabel = "My BMI",
                                         plotYLabel = "My Friend BMI",
                                         plotTheme = "regression",
                                         overrideImageWidth = 12)

        
        # Checking correlation between how many opposite friends I have and my BMI
    	# ---- R² is 0 and p-v 0.6, nothing here    	
        mySexIncrease = doRegressionPlot(sameSexBMIDF, 4, 5, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                         plotTitle = "Relationship between my BMI and opposite friends sex (ALL)",
                                         plotXLabel = "Total friends of the opposite sex",
                                         plotYLabel = "My BMI",
                                         plotTheme = "regression",
                                         overrideImageWidth = 12)    	
    	
    	# Do the same for men and women individually
        # ---- Also nothing here
        sameSexBMIDFMen    = sameSexBMIDF[sameSexBMIDF$Sex == "Man",]
    	sameSexBMIDFWomen  = sameSexBMIDF[sameSexBMIDF$Sex == "Woman",]
    	
        mySexIncreaseMen = doRegressionPlot(sameSexBMIDFMen, 4, 5, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                         plotTitle = "Relationship between male total female friends and BMI",
                                         plotXLabel = "Total friends of the opposite sex",
                                         plotYLabel = "My BMI",
                                         plotTheme = "regression",
                                         overrideImageWidth = 12)
        
        mySexIncreaseWomen = doRegressionPlot(sameSexBMIDFWomen, 4, 5, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                              plotTitle = "Relationship between female total male friends and BMI",
                                              plotXLabel = "Total friends of the opposite sex",
                                              plotYLabel = "My BMI",
                                              plotTheme = "regression",
                                              overrideImageWidth = 12)    	    	    
        
        
    
    
    	# Find the tables for BMI
    	summaryDF           = DF(10,4,0)
    	colnames(summaryDF) = c("Total Opposite Sex Friends", "$\\overline{BMI}_{men}$", "$\\overline{BMI}_{women}$", "$\\overline{BMI}_{all}$")
    
    	for(i in 1:10){
        
        	summaryDF[i,1] = (i-1)
        	summaryDF[i,2] = mean(  sameSexBMIDFMen[sameSexBMIDFMen$`Total opposite friends`     == (i-1),5]  )
        	summaryDF[i,3] = mean(  sameSexBMIDFWomen[sameSexBMIDFWomen$`Total opposite friends` == (i-1),5]  )
        	summaryDF[i,4] = mean(  sameSexBMIDF[sameSexBMIDF$`Total opposite friends`           == (i-1),5]  )
        
    	}
    
    	# Round the columns, or else the latex tables look very weird
    	summaryDF[,2] = round(summaryDF[,2],2)
    	summaryDF[,3] = round(summaryDF[,3],2)
    	summaryDF[,4] = round(summaryDF[,4],2)
    
    	writeTableLATEX(summaryDF, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "In the cross-sectiona data from FF1, BMI doesn't seem to be associated with number of opposite sex friends.",
        	            overrideTableName = "OppositeSexBMI", widthProportion = 0.8, heightProportion = 0.2)
       
        
    }
    
	# Check BMI with average BMI
	{
		BMIRelationDF
		
		# Get all the people who has friends
		totalWithFriends  = length(unique(BMIRelationDF$MyID))
		BMIAveragesDF     = DF(totalWithFriends, 4)
		colnames(BMIAveragesDF) = c("ID", "MyBMI","FriendsBMIAverage", "Sex")
		BMIAveragesDF[,1] = unique(BMIRelationDF$MyID)
		
		# For each person
		for(i in 1:totalWithFriends){
			
			# Get the ID
			currentID = BMIAveragesDF$ID[i]
			
			# Find the BMI
			auxiliarDF = subset(BMIRelationDF, BMIRelationDF$MyID == currentID)
			currentBMI = auxiliarDF[1,3]
			BMIAveragesDF[i,2] = currentBMI
			
			# Find my friends averages
			currentBMIAverage = mean(auxiliarDF[,4])
			BMIAveragesDF[i,3] = currentBMIAverage
			
			# Find the sex
			currentSex = auxiliarDF[1,5]
			BMIAveragesDF[i,4] = currentSex
		}
		
		# Do the scatterplot
		# ---- R² = 0.04 , similar for both sexes, not very interesting
        myBMIIncrease = doRegressionPlot(BMIAveragesDF, 2, 3, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                         plotTitle = "Relationship of my BMI with my friends average BMI (ALL)",
                                         plotXLabel = "My BMI",
                                         plotYLabel = "My Friend average BMI",
                                         plotTheme = "regression",
                                         overrideImageWidth = 12)		
        
        BMIAveragesDFMen    = BMIAveragesDF[BMIAveragesDF$Sex == "Man",]
    	BMIAveragesDFWomen  = BMIAveragesDF[BMIAveragesDF$Sex == "Woman",]
        
        
        myBMIIncrease = doRegressionPlot(BMIAveragesDFMen, 2, 3, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                         plotTitle = "Relationship of my BMI with my friends average BMI (Men only)",
                                         plotXLabel = "My BMI",
                                         plotYLabel = "My Friend average BMI",
                                         plotTheme = "regression",
                                         overrideImageWidth = 12)		        
        
        myBMIIncrease = doRegressionPlot(BMIAveragesDFWomen, 2, 3, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                         plotTitle = "Relationship of my BMI with my friends average BMI (Women only)",
                                         plotXLabel = "My BMI",
                                         plotYLabel = "My Friend average BMI",
                                         plotTheme = "regression",
                                         overrideImageWidth = 12)		                
		
	}
	
	# Check BMI average increase over time
	{
		
		# Overall mean
		mean(antropometricTableFF1$BMI, na.rm = TRUE)
		sd(antropometricTableFF1$BMI, na.rm = TRUE)
		mean(antropometricTableFF2$BMI, na.rm = TRUE)
		sd(antropometricTableFF2$BMI, na.rm = TRUE)
		
		# For men
		mean(antropometricTableFF1[antropometricTableFF1$ID %in% menOnlyTable$ID,]$BMI, na.rm = TRUE)
		sd(antropometricTableFF1[antropometricTableFF1$ID %in% menOnlyTable$ID,]$BMI, na.rm = TRUE)
		mean(antropometricTableFF2[antropometricTableFF2$ID %in% menOnlyTable$ID,]$BMI, na.rm = TRUE)
		sd(antropometricTableFF2[antropometricTableFF2$ID %in% menOnlyTable$ID,]$BMI, na.rm = TRUE)
		
		# For women
		mean(antropometricTableFF1[antropometricTableFF1$ID %in% womenOnlyTable$ID,]$BMI, na.rm = TRUE)
		sd(antropometricTableFF1[antropometricTableFF1$ID %in% womenOnlyTable$ID,]$BMI, na.rm = TRUE)
		mean(antropometricTableFF2[antropometricTableFF2$ID %in% womenOnlyTable$ID,]$BMI, na.rm = TRUE)
		sd(antropometricTableFF2[antropometricTableFF2$ID %in% womenOnlyTable$ID,]$BMI, na.rm = TRUE)
		
	}
	
	# Xi² table with respect number of relationships
	{
		
		# Delete the people with unknown BMI (this is insane, I hate R, it shouldn't take this much code to do something as simple as this. And this is already with an optimized reduction code that I made in tools)
		tempTable  = completeTable
		# ---- Get the IDs
		badIDs = tempTable[tempTable$BMICategorical == "Unknown",]$ID
		# ---- Delete the IDs from the overall relationship table
		badRelationships = deleteConnections(overallEdgesDF, badIDs, badIDs)[[1]]
		newRelationshipsDF = overallEdgesDF[!badRelationships,]
		# ---- Delete the Unknown category
		tempTable  = deleteCategory(tempTable, BMICatIndex, "Unknown")
		
		XiDataDF = meltedRelationships(tempTable, BMICatIndex, newRelationshipsDF)
		
		xiResults = categoricalXiV2(XiDataDF, 1 ,2)
		
		writeTableLATEX(xiResults[[7]], BIOMARKERS_FOLDER_TABLES_NETWORK,
                        tableCaption = "Frienship Bias with respect BMI",
                        overrideTableName = "friendshipBiasBMI",
                        widthProportion = 0.8, heightProportion = 0.09) 
		
	}


	
	
}	
    

#---------------------------------------------------------------------------
# Check isolation group with respect the rest
#---------------------------------------------------------------------------
{

	# Prepare the results dataframe, we need BIOMARKERS x SEX table
	# Both for men and women
	isolationResultsDF = DF(TOTAL_BIOMARKERS, 3)
	colnames(isolationResultsDF) = c("Name", "Men", "Women")
	isolationResultsDF[,1] = biomarkersMetadataDF$Protein
	
    # Count how many friends per person
    myOverallFriendshipMatrix    = getFriendshipMatrix(overallEdgesDF, TOTAL_PEOPLE)
    friendsPerPersonDF           = DF(TOTAL_PEOPLE, 3)
    colnames(friendsPerPersonDF) = c("ID", "Total", "Isolation")
    friendsPerPersonDF[,1]       = completeTable$ID
    friendsPerPersonDF[,2]       = getTotalFriends(myOverallFriendshipMatrix)
    friendsPerPersonDF[,3]       = "Non-Isolated"
    for(i in 1:TOTAL_PEOPLE) if(friendsPerPersonDF[i,2] == 0) friendsPerPersonDF[i,3] = "Isolated"
    friendsPerPersonDF[,3]       = factor(friendsPerPersonDF[,3], levels = c("Isolated", "Non-Isolated"))  
    
    # Transform the data into string because R sucks as a language and can't cast int to strings directly, let alone declare as a typeof variable! :(
    # AND I HATE THIS LANGUAGE; I DONT WANT TO CHANGE THE ORDER; I DEFINE THE ORDER ALREADY
    # WHAT'S WRONG WITH YOU AND THE STUPID NUMBERS!!????    JESUS!!
    #
    # Seriously, why after so many years, there is no BASIC function in R that
    # convert integers to categorical data, and you need to do all this manually?
    #
    # I tell you why, because there are no classes here. This is stupid beyond believe
    # I wasted 4 years programming all of this when I could have done the perfect
    # library in C++ to handle abstract dataframes
    #currentLevels          = levels(as.factor(friendsPerPersonDF[,2]))
    #currentLevels          = as.character(currentLevels)
    #friendsPerPersonDF[,2] = as.character(friendsPerPersonDF[,2])
    #friendsPerPersonDF[,2] = factor(friendsPerPersonDF[,2], levels = currentLevels)  
     
    
               	
	# For men and women
    for(i in 1:2){
        
    	# Select which table are we using
        {
            currentSubtable = menOnlyTable
            currentSex      = "Men"
            if(i == 2){
                currentSex  = "Women"
                currentSubtable = womenOnlyTable
            }
        }

    	# Get the ID that you want, by sex
    	currentIDs = currentSubtable[,1]    	
    	
    	# Get the friend rows that you need
    	currentFriendLinesDF = friendsPerPersonDF[friendsPerPersonDF[,1] %in% currentIDs,]
    	
    	
		# For each biomarker, check how levels change with respect number of friends
    	# There are no difference when you look into friends from 1 to 12, but there are changes for 0
    	# So instead of looking at the whole spectrum, we divide into isolation or not
    	if(FALSE){for(k in 1:TOTAL_BIOMARKERS){
    	
    		# Add the biomarkers levels
    		currentBioData     = currentFriendLinesDF
			currentBioData[,3] = menOnlyTable[ ,(NDLIndex + k - 1)]
			
			# Get the biomarker information
			currentBiomarkerShort = biomarkersMetadataDF$Acronym[k]
			currentBiomarkerName  = biomarkersMetadataDF$Protein[k]
			currentPlotTitle      = paste0("Level of ",currentBiomarkerName, " for ", currentSex)
			
			# Set the tableTame
			currentTableName     = paste0("Isolation",currentBiomarkerShort, currentSex)
     
        	currentBoxPlot = doBoxPlotV2 (currentBioData, 3,
                                          BIOMARKERS_FOLDER_IMAGES_ISOLATION,
                                          groupIndex = 2,
                                          #colorsVector = COLOR_VECTOR_SEX,
                                          showPValues = TRUE,
                                          significantPValue = 0.05,
                                         
        								  overrideTableName = currentTableName,
        		
                                          plotTitle = currentPlotTitle,
                                          plotSubtitle = "",
                                          plotXLabel = "Number of friends",
                                          plotYLabel = "Level",
        
                                          overrideImageWidth = 7) 			
			
    			
    	}}
    	
    	# We are not breaking down by high-school because there are too little people isolated
    	for(k in 1:TOTAL_BIOMARKERS){
    	
    		# Add the biomarkers levels
    		currentBioData     = currentFriendLinesDF
			currentBioData[,4] = currentSubtable[ ,(NDLIndex + k - 1)]
			
			# Get the biomarker information
			currentBiomarkerShort = biomarkersMetadataDF$Acronym[k]
			currentBiomarkerName  = biomarkersMetadataDF$Protein[k]
			currentPlotTitle      = paste0("Level of ",currentBiomarkerName, " for ", currentSex)
			
			# Set the tableTame
			currentTableName     = paste0("Isolation",currentBiomarkerShort, currentSex)
     
        	currentBoxPlot = doBoxPlotV2 (currentBioData, 4,
                                          BIOMARKERS_FOLDER_IMAGES_ISOLATION,
                                          groupIndex = 3,
                                          #colorsVector = COLOR_VECTOR_SEX,
                                          showPValues = TRUE,
                                          significantPValue = 0.05,
                                         
        								  overrideTableName = currentTableName,
        		
                                          plotTitle    = currentPlotTitle,
                                          plotSubtitle = "",
                                          plotXLabel   = "Isolation group",
                                          plotYLabel   = "Level",
        
                                          overrideImageWidth = 7)
        	
        	# Get the p-value and put it into the table of results
        	currentPValue = as.numeric(currentBoxPlot[[5]][1,2])
        	isolationResultsDF[k,i+1] = currentPValue
    			
    	}
    	
    	
    }		
    		
    	
    # Transform into Bonferroni p-values for both sexes
    bonferroniIsolationResultsDF = isolationResultsDF
    benjaminiIsolationResultsDF  = isolationResultsDF
    
    bonferroniIsolationResultsDF[,2] = p.adjust(bonferroniIsolationResultsDF[,2],  method = "bonferroni")  
    bonferroniIsolationResultsDF[,3] = p.adjust(bonferroniIsolationResultsDF[,3],  method = "bonferroni")
    benjaminiIsolationResultsDF[,2]  = p.adjust(benjaminiIsolationResultsDF[,2],   method = "fdr")  
    benjaminiIsolationResultsDF[,3]  = p.adjust(benjaminiIsolationResultsDF[,3],   method = "fdr")
    
    
    asteriskBonferroniIsolationResultsDF = bonferroniIsolationResultsDF
    
    
    
    asteriskBonferroniIsolationResultsDF
    
    getAsterkisPValue(bonferroniIsolationResultsDF[,2])
    getAsterkisPValue(bonferroniIsolationResultsDF[,3])
    
    getAsterkisPValue(benjaminiIsolationResultsDF[,2])
    getAsterkisPValue(benjaminiIsolationResultsDF[,3])
    
    
    getAsterkisPValue(isolationResultsDF[,2])
    getAsterkisPValue(isolationResultsDF[,3])
    
    
}



# Check evolution in time
{
    
    # Create the dataframe with the relative obese information
    {
        # Get the Friendship matrix (again, because the variable is lost somewhere; I really hate R)
        ovMatrix = getFriendshipMatrix(overallEdgesDF, 1038)
        
        # Check for people that are healthy by % of friends that are obese
        # Where do they end up in FF2
        
        dataTableDF = DF(nrow(completeTable), 6)
        colnames(dataTableDF) = c("ID", "FF1Status", "FF2Status", "TotalFriends", "TotalObeseFriends", "PercentageObeseFriends")
        
        # Get the friends statistics
        currentResults = meltedCountFriendsByCategory(completeTable, BMICatIndex, ovMatrix)
    
        # Fill the DF
        dataTableDF[,1] = completeTable$ID
        dataTableDF[,2] = antropometricTableFF1$BMICategorical
        dataTableDF[,3] = antropometricTableFF2$BMICategorical    
        dataTableDF[,4] = currentResults$TotalFriends
        dataTableDF[,5] = currentResults$Overweight + currentResults$Obese
        dataTableDF[,6] = -1
        
        for(i in 1:nrow(dataTableDF)){
        
            if( dataTableDF[i,4] > 0 ){
                
                  dataTableDF[i,6] = dataTableDF[i,5] / dataTableDF[i,4]  
                
            }
                
            
        }
        
        #  Fill the categorical data
        dataTableDF$CatTotalObese = factor(dataTableDF$TotalObeseFriends)
        dataTableDF$Combination   = paste0(as.character(dataTableDF$FF1Status)," ",as.character(dataTableDF$CatTotalObese))
        
        # Transform this into alluvial format
        # ---- We don't care about those with unknown category, delete thos first
        cleanDataTableDF = dataTableDF
        cleanDataTableDF = deleteCategory(cleanDataTableDF, 2, "Unknown")
        cleanDataTableDF = deleteCategory(cleanDataTableDF, 3, "Unknown")
        # ---- Make a DF where we count each combination
        totalCombinations             = length(unique(cleanDataTableDF$Combination))
        alluvialByFriendsDF           = DF(totalCombinations*4, 3)
        colnames(alluvialByFriendsDF) = c("FF1", "FF2", "Freq")
        # ---- Fill the DF
        # (Another stupid R thing, I want to iterate on the enum of BMI. Can't, you don't have enums either iterators)
        myFF1Levels = sort(unique(cleanDataTableDF$Combination))
        myFF2Levels = levels(antropometricTableFF2$BMICategorical)[1:4]
        currentIndex = 1
        for(i in 1: length(myFF1Levels)){
            FF1Level = myFF1Levels[i]
            for(j in 1:4){
                FF2Level = myFF2Levels[j]
            
                alluvialByFriendsDF[currentIndex, 1] = FF1Level
                alluvialByFriendsDF[currentIndex, 2] = FF2Level
            
                tempDF1 = keepCategory(cleanDataTableDF, 8, FF1Level)
                tempDF2 = keepCategory(cleanDataTableDF, 3, FF2Level)
                
                currentIntersect = intersect(tempDF1$ID,tempDF2$ID)
            
                alluvialByFriendsDF[currentIndex, 3] = length(currentIntersect)
            
                currentIndex = currentIndex + 1

            }
        }   
        
        # ---- Transform the alluvial DF so we can make a proper plot
        alluvialLodesBig = to_lodes_form(alluvialByFriendsDF,
                           axes = 1:2,
                           id = "Cohort")        
        
        #           Add an metacategory, colors are weird with all combinations
        alluvialLodesBig$Category = "Healthy"
        for(i in 1:nrow(alluvialLodesBig)){
        
            if(grepl("Underweight", alluvialLodesBig$stratum[i], fixed = TRUE)) alluvialLodesBig$Category[i] = "Underweight"
            if(grepl("Overweight",  alluvialLodesBig$stratum[i], fixed = TRUE)) alluvialLodesBig$Category[i] = "Overweight"
            if(grepl("Obese",       alluvialLodesBig$stratum[i], fixed = TRUE)) alluvialLodesBig$Category[i] = "Obese"
                
        }
        
        # This is the normal sorting
        alluvialLodesBig$stratum  = factor(alluvialLodesBig$stratum,    levels = c("Underweight",
                                                                                   "Underweight 0", "Underweight 1", "Underweight 2", "Underweight 3", "Underweight 4",
                                                                                   "Healthy",
                                                                                   "Healthy 0", "Healthy 1", "Healthy 2", "Healthy 3", "Healthy 4", "Healthy 5",
                                                                                   "Overweight",
                                                                                   "Overweight 0", "Overweight 1", "Overweight 2", "Overweight 3", "Overweight 4", "Overweight 5",
                                                                                   "Obese",
                                                                                   "Obese 0", "Obese 1", "Obese 2", "Obese 3", "Obese 4", "Obese 5"))
        alluvialLodesBig$Category = factor(alluvialLodesBig$Category,   levels = c("Underweight", "Healthy",   "Overweight", "Obese"))

        
        # Finally, let make a table that is more readable from the inverse point of view
        
        reverseObeseFriendsDF = DF(6, 6, defaultValue = 0)
        colnames(reverseObeseFriendsDF) = c("High BMI Friends", "Underweight", "Healthy", "Overweight", "Obese", "Total")
        reverseObeseFriendsDF[,1] = c(0:5)
        
        alluvialLodesBigFF1 = keepCategory(alluvialLodesBig, 3, "FF1")
        
        for(i in 1:nrow(alluvialLodesBigFF1)){
            
            # Get the total of friends
            currentFriends = 0
            if(grepl("1", alluvialLodesBigFF1$stratum[i], fixed = TRUE)) currentFriends = 1
            if(grepl("2", alluvialLodesBigFF1$stratum[i], fixed = TRUE)) currentFriends = 2
            if(grepl("3", alluvialLodesBigFF1$stratum[i], fixed = TRUE)) currentFriends = 3
            if(grepl("4", alluvialLodesBigFF1$stratum[i], fixed = TRUE)) currentFriends = 4
            if(grepl("5", alluvialLodesBigFF1$stratum[i], fixed = TRUE)) currentFriends = 5
            currentFriends = currentFriends + 1
            
            # Get the category
            currentCategory = 1
            if( as.character(alluvialLodesBigFF1$Category[i]) == "Healthy" )    currentCategory = 2
            if( as.character(alluvialLodesBigFF1$Category[i]) == "Overweight" ) currentCategory = 3
            if( as.character(alluvialLodesBigFF1$Category[i]) == "Obese" )      currentCategory = 4            
            currentCategory = currentCategory + 1
            
            # Get the frequency
            currentFrequency = alluvialLodesBig$Freq[i]
            
            # Update
            reverseObeseFriendsDF[currentFriends, currentCategory] = currentFrequency + reverseObeseFriendsDF[currentFriends, currentCategory]
        }
        
        reverseObeseFriendsDF$Total[1] = sum(reverseObeseFriendsDF[1,])
        reverseObeseFriendsDF$Total[2] = sum(reverseObeseFriendsDF[2,]) - 1
        reverseObeseFriendsDF$Total[3] = sum(reverseObeseFriendsDF[3,]) - 2
        reverseObeseFriendsDF$Total[4] = sum(reverseObeseFriendsDF[4,]) - 3
        reverseObeseFriendsDF$Total[5] = sum(reverseObeseFriendsDF[5,]) - 4
        reverseObeseFriendsDF$Total[6] = sum(reverseObeseFriendsDF[6,]) - 5
        
        reverseObeseFriendsPercentageDF = reverseObeseFriendsDF
        reverseObeseFriendsPercentageDF$Underweight = round(reverseObeseFriendsPercentageDF$Underweight / reverseObeseFriendsPercentageDF$Total,4) * 100
        reverseObeseFriendsPercentageDF$Healthy = round(reverseObeseFriendsPercentageDF$Healthy / reverseObeseFriendsPercentageDF$Total,4) * 100
        reverseObeseFriendsPercentageDF$Overweight = round(reverseObeseFriendsPercentageDF$Overweight / reverseObeseFriendsPercentageDF$Total,4) * 100
        reverseObeseFriendsPercentageDF$Obese = round(reverseObeseFriendsPercentageDF$Obese / reverseObeseFriendsPercentageDF$Total,4) * 100
        
    
        writeTableLATEX(reverseObeseFriendsPercentageDF, BIOMARKERS_FOLDER_TABLES_NETWORK,
                        tableCaption = "Percentage of students that end up in each BMI category at FF2, divided by the number of high BMI friends that they had in FF1",
                        overrideTableName = "reverseObeseFriends", widthProportion = 0.7)        
        
    }
    
    # Create the alluvial DFs
    {
    
		# Get only people that appear in both tables
        antroFF1 = antropometricTableFF1
        antroFF1 = deleteCategory(antroFF1, 10, "Unknown")
        antroFF2 = antropometricTableFF2
        antroFF2 = deleteCategory(antroFF2, 10, "Unknown")    

        commonIDs = intersect(antroFF1$ID, antroFF2$ID)
            
        antroFF1 = antroFF1[(antroFF1$ID %in% commonIDs),]
        antroFF2 = antroFF2[(antroFF2$ID %in% commonIDs),]
    	
    	
        # For the general case
        {
            
            # Make a DF with all possibilities without the unknown
            alluvialDF = DF(4*4, 3)
            colnames(alluvialDF) = c("FF1", "FF2", "Freq")
    
            # Another stupid R thing, I want to iterate on the enum of BMI. Can't, you don't have enums either iterators
            myCurrentLevels = levels(antropometricTableFF1$BMICategorical)[1:4]
    
            currentIndex = 1
            for(i in 1:4){
                FF1Level = myCurrentLevels[i]
                for(j in 1:4){
                    FF2Level = myCurrentLevels[j]
            
                    alluvialDF[currentIndex, 1] = FF1Level
                    alluvialDF[currentIndex, 2] = FF2Level
            
                    tempDF1 = keepCategory(antroFF1, 10, FF1Level)
                    tempDF2 = keepCategory(antroFF2, 10, FF2Level)
                
                    currentIntersect = intersect(tempDF1$ID,tempDF2$ID)
            
                    alluvialDF[currentIndex, 3] = length(currentIntersect)
            
                    currentIndex = currentIndex + 1

                }
            }    
            
            
            alluvialLodes = to_lodes_form(alluvialDF,
                           axes = 1:2,
                           id = "Cohort")

            alluvialLodes$stratum = factor(alluvialLodes$stratum,   levels = c("Underweight", "Healthy",   "Overweight", "Obese"))
            
        }
        
        # For the general case number 2
        {
        
            alluvialDF2 = DF(8, 4)    
            colnames(alluvialDF2) = c("Survey", "Status", "Freq", "Subject")
            
            # FF1
            for(i in 1:4){
                
                # Write the category and survey
                alluvialDF2[i,1] = "FF1"
                alluvialDF2[i,2] = myCurrentLevels[i]
                alluvialDF2[i,4] = i
                
                # Count how many are in this status
                alluvialDF2[i,3] = countCategories(antroFF1, 10, myCurrentLevels[i])
                
            }
            
            # FF2
            for(i in 5:8){
                
                # Write the category and survey
                alluvialDF2[i,1] = "FF2"
                alluvialDF2[i,2] = myCurrentLevels[i-4]
                alluvialDF2[i,4] = i-4
                
                # Count how many are in this status
                alluvialDF2[i,3] = countCategories(antroFF2, 10, myCurrentLevels[i-4])
                
            }            
            
        }
        
    }
    
    
    # Everything is done now, let do the plots
    {
    
        # ---- General overview of how people shift
        ggplot(data = alluvialDF, aes(axis1 = FF1, axis2 = FF2, y = Freq)) +
                geom_flow() +
                geom_stratum(alpha = .5) +

                geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
                scale_x_discrete(limits = c("Survey", "Response"),
                               expand = c(0.15, 0.05)) +
                theme_void()    
     
        # ---- General overview 2
        generalEvolutionPlot = ggplot(alluvialLodes,
               aes(x = x, y = Freq, stratum = stratum, alluvium = Cohort,
                   fill = stratum, label = stratum)) +
               scale_x_discrete(expand = c(.1, .1)) +
               geom_flow() +
               geom_stratum(alpha = .5) +
               geom_text(stat = "stratum", size = 3) +
               theme(legend.position = "none") +
               ggtitle("BMI Evolution from academic years 2010 to 2013")
        
        imageWidth    = 8
        imageHeight   = 8
        imageFilePath = paste0(BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY, "generalEvolution.png")
        ggsave(imageFilePath, plot = generalEvolutionPlot, width = imageWidth, height = imageHeight)  
        myCaption = "Evolution of BMI from FF1 (2010-2011) to FF2 (2013-2014)"
        myLabel   = "fig:EvolutionFF1toFF2"
        writeImageLATEX2(imageFilePath, LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY, 
                         captionText   = myCaption,
                         overrideLabel = myLabel,
                         pageHeight    = 0.5, overrideFloat = TRUE)        
        

    }
    
    
    
    
    # ---- Shift with respect percentage of obese friends
    # ---- Shift with respect total of obese friends
    {
    
        # ---- General overview 2
        preciseEvolutionPlot = ggplot(alluvialLodesBig,
               aes(x = x, y = Freq, stratum = stratum, alluvium = Cohort,
                   fill = Category, label = stratum)) +
               scale_x_discrete(expand = c(.1, .1)) +
               geom_flow() +
               geom_stratum(alpha = .5) +
               geom_text(stat = "stratum") +
               theme(legend.position = "none") +
               ggtitle("BMI Evolution from FF1 by total number of obese + overweight friends to FF2")
        
        
    }
    
    # The previous shift is too chaotic in the image, reproduce for each final FF2 category
    
    
    # ---- For healthy
    {
        # Delete the rows that are FF2 and not healthy
        alluvialLodesBigHealthy = alluvialLodesBig[!(alluvialLodesBig$x == "FF2" & alluvialLodesBig$stratum != "Healthy"),]
        alluvialLodesBigHealthy = alluvialLodesBigHealthy[alluvialLodesBigHealthy$Freq != 0,]
        alluvialLodesBigHealthy = alluvialLodesBigHealthy[alluvialLodesBigHealthy$Cohort %in% alluvialLodesBigHealthy[alluvialLodesBigHealthy$x == "FF2",]$Cohort,]
        alluvialLodesBigHealthy$Category = factor(alluvialLodesBigHealthy$Category,   levels = c("Underweight", "Healthy",   "Overweight", "Obese"))
        
        # ---- General overview 2
        healthyEvolutionPlot = ggplot(alluvialLodesBigHealthy,
               aes(x = x, y = Freq, stratum = stratum, alluvium = Cohort,
                   fill = Category, label = stratum)) +
               scale_x_discrete(expand = c(.1, .1)) +
               scale_fill_discrete(drop = FALSE) +
               geom_flow() +
               geom_stratum(alpha = .5) +
               geom_text(stat = "stratum") +
               theme(legend.position = "none",
                     axis.title.x=element_blank(), axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())
        
        alluvialLodesBigHealthy = keepCategory(alluvialLodesBigHealthy, 3, "FF1")
        alluvialLodesBigHealthy$Frac = alluvialLodesBigHealthy$Freq / sum(alluvialLodesBigHealthy$Freq)
        alluvialLodesBigHealthy$ymax = cumsum(alluvialLodesBigHealthy$Frac)
        alluvialLodesBigHealthy$ymin = c(0, head(alluvialLodesBigHealthy$ymax, n=-1))
        alluvialLodesBigHealthy$labelPosition = (alluvialLodesBigHealthy$ymax + alluvialLodesBigHealthy$ymin) / 2
        alluvialLodesBigHealthy$HumanFrac = paste0(round(alluvialLodesBigHealthy$Frac * 100),"%")
        alluvialLodesBigHealthy$label =paste0(alluvialLodesBigHealthy$stratum, "\n", alluvialLodesBigHealthy$HumanFrac)

        # Make the plot
        healthyDonutPlot = ggplot(alluvialLodesBigHealthy, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
            geom_rect(colour="grey10") +
            scale_fill_discrete(drop = FALSE) +
            geom_label_repel( x=3.5, aes(y=labelPosition, label=label), max.overlaps = 20) +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "none") +
            ggtitle("FF2 Healthy source")       
        
        
        # Make the bar plot
        
        # ---- Keep the proper category
        tempTable = dataTableDF
        tempTable = tempTable[tempTable$FF2Status == "Healthy",]
        tempTable = tempTable[tempTable$FF1Status != "Unknown",]
        
        
        tempTable$Combination = factor(tempTable$Combination , levels = c("Underweight 0", "Underweight 1", "Underweight 2", "Underweight 3", "Underweight 4", "Underweight 5",
        	                                                              "Healthy 0",     "Healthy 1",     "Healthy 2",     "Healthy 3",     "Healthy 4",     "Healthy 5",
        	                                                              "Overweight 0",  "Overweight 1",  "Overweight 2",  "Overweight 3",  "Overweight 4",  "Overweight 5",        	
        	                                                              "Obese 0",       "Obese 1",       "Obese 2",       "Obese 3",       "Obese 4",       "Obese 5"))

        currentColorVector = c("#F8766D", "#F8766D", "#F8766D", "#F8766D", "#F8766D",
        	                   "#7CAE00", "#7CAE00", "#7CAE00", "#7CAE00", "#7CAE00", "#7CAE00",
							   "#00BFC4", "#00BFC4", "#00BFC4", "#00BFC4", "#00BFC4")
        
        
        doBarPlotV2(tempTable, 8, BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
        	        colorCounting = TRUE,
        	        colorsVector = currentColorVector,
                    givenAngle = 90,
        	        plotTitle = "Healthy BMI at FF2",
        			plotXLabel = "FF1 BMI + Total Friends BMI > 25",
        	        plotYLabel = "",
        	        plotTheme = "simple",
                    overrideImageWidth  = 10,
                    overrideImageHeight = 10)

        
        # Line plot
        
        lineDF = DF(4, 7)
        colnames(lineDF) = c("Group", "0", "1", "2", "3", "4", "5")
        lineDF[1,1] = "Underweight"
        lineDF[2,1] = "Healthy"
        lineDF[3,1] = "Overweight"
        lineDF[4,1] = "Obese"
        
        for (i in 0:5) {

        	tempTable = dataTableDF
        	tempTable = tempTable[tempTable$FF2Status != "Unknown",]
        	tempTable = tempTable[tempTable$TotalObeseFriends == i,]                	
        	
        	totalPeople = nrow(tempTable)
        	
        	# Count how many...
        	# underweight
        	totalUnder  = nrow(tempTable[tempTable$FF2Status == "Underweight",])
        	# healthy
        	totalHealth = nrow(tempTable[tempTable$FF2Status == "Healthy",])
        	# Overweight
        	totalOver   = nrow(tempTable[tempTable$FF2Status == "Overweight",])
        	# Obese
        	totalObese  = nrow(tempTable[tempTable$FF2Status == "Obese",])
        	
        	# Put it into the DF
        	lineDF[1,i+2] = totalUnder  / totalPeople
        	lineDF[2,i+2] = totalHealth / totalPeople
        	lineDF[3,i+2] = totalOver   / totalPeople
        	lineDF[4,i+2] = totalObese  / totalPeople
        	        	
        }
        
        meltedLineDF = melt(lineDF)
        
        meltedLineDF$Group = factor(meltedLineDF$Group , levels = c("Underweight",
                                                                    "Healthy",
                                                                    "Overweight",        	
                                                                    "Obese"))
        
        
        colorsVector = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
        meltedLineDF$value = meltedLineDF$value * 100
        
        # Line plot with multiple groups
		myLinePlot = ggplot(data=meltedLineDF, aes(x=variable, y=value, group=Group, color = Group)) +
			                scale_fill_manual(values = rev(colorsVector)) +
			                geom_line(size = 1.2) +
			                geom_point(size=3) +
		
							# Create titles and subtitles
        					labs(title    = "Frequency of each student in FF2 with respect how many friends with BMI > 25 had in FF1",
			                     fill     = "FF2 BMI",
                                 x = "Number of friends with BMI > 25",
        						 y = "%") +
          
        					# Apply the theme
        					theme(panel.background   = element_blank(),
                				  axis.line          = element_line(colour = "black"),
                				  panel.grid.major.y = element_line(colour = "grey70"),
                				  panel.grid.major.x = element_line(colour = "grey70", linetype="dashed"),
                				  legend.position    = "right")
        
        
        tempTable = dataTableDF
        tempTable = tempTable[tempTable$FF2Status != "Unknown",]
        tempTable = tempTable[tempTable$TotalObeseFriends == 0,]        
        
        
                
        
        tempTable = dataTableDF
        tempTable = tempTable[tempTable$FF2Status != "Unknown",]
        tempTable = tempTable[tempTable$TotalObeseFriends == 0,]
        
        doBarPlotV2(tempTable, 3, BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
        	        colorCounting = TRUE,
        	        colorsVector = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                    givenAngle = 90,
        	        plotTitle  = "FF2 BMI Category and 0 friends with BMI > 25",
        			plotXLabel = "",
        	        plotYLabel = "",
        	        plotTheme = "simple",
                    overrideImageWidth  = 10,
                    overrideImageHeight = 10)        
        

        tempTable = dataTableDF
        tempTable = tempTable[tempTable$FF2Status != "Unknown",]
        tempTable = tempTable[tempTable$TotalObeseFriends == 1,]
        
        doBarPlotV2(tempTable, 3, BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
        	        colorCounting = TRUE,
        	        colorsVector = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                    givenAngle = 90,
        	        plotTitle  = "FF2 BMI Category and 1 friends with BMI > 25",
        			plotXLabel = "",
        	        plotYLabel = "",
        	        plotTheme = "simple",
                    overrideImageWidth  = 10,
                    overrideImageHeight = 10)                
        

        tempTable = dataTableDF
        tempTable = tempTable[tempTable$FF2Status != "Unknown",]
        tempTable = tempTable[tempTable$TotalObeseFriends == 2,]
        
        doBarPlotV2(tempTable, 3, BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
        	        colorCounting = TRUE,
        	        colorsVector = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                    givenAngle = 90,
        	        plotTitle  = "FF2 BMI Category and 2 friends with BMI > 25",
        			plotXLabel = "",
        	        plotYLabel = "",
        	        plotTheme = "simple",
                    overrideImageWidth  = 10,
                    overrideImageHeight = 10)        
        
        tempTable = dataTableDF
        tempTable = tempTable[tempTable$FF2Status != "Unknown",]
        tempTable = tempTable[tempTable$TotalObeseFriends == 3,]
        
        doBarPlotV2(tempTable, 3, BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
        	        colorCounting = TRUE,
        	        colorsVector = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                    givenAngle = 90,
        	        plotTitle  = "FF2 BMI Category and 3 friends with BMI > 25",
        			plotXLabel = "",
        	        plotYLabel = "",
        	        plotTheme = "simple",
                    overrideImageWidth  = 10,
                    overrideImageHeight = 10)         
                		
        tempTable = dataTableDF
        tempTable = tempTable[tempTable$FF2Status != "Unknown",]
        tempTable = tempTable[tempTable$TotalObeseFriends == 4,]
        
        doBarPlotV2(tempTable, 3, BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
        	        colorCounting = TRUE,
        	        colorsVector = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                    givenAngle = 90,
        	        plotTitle  = "FF2 BMI Category and 4 friends with BMI > 25",
        			plotXLabel = "",
        	        plotYLabel = "",
        	        plotTheme = "simple",
                    overrideImageWidth  = 10,
                    overrideImageHeight = 10) 		
        
        tempTable = dataTableDF
        tempTable = tempTable[tempTable$FF2Status != "Unknown",]
        tempTable = tempTable[tempTable$TotalObeseFriends == 5,]
        
        tempTable$Combination = factor(tempTable$FF2Status , levels = c("Underweight",
        	                                                            "Healthy",
        	                                                            "Overweight",        	
        	                                                            "Obese")         
        
        doBarPlotV2(tempTable, 3, BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
        	        colorCounting = TRUE,
        	        colorsVector = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                    givenAngle = 90,
        	        plotTitle  = "FF2 BMI Category and 5 friends with BMI > 25",
        			plotXLabel = "",
        	        plotYLabel = "",
        	        plotTheme = "simple",
                    overrideImageWidth  = 10,
                    overrideImageHeight = 10)         

        
        
    }
    
    # ---- For Overweight
    {
        # Delete the rows that are FF2 and not healthy
        alluvialLodesBigHealthy = alluvialLodesBig[!(alluvialLodesBig$x == "FF2" & alluvialLodesBig$stratum != "Overweight"),]
        alluvialLodesBigHealthy = alluvialLodesBigHealthy[alluvialLodesBigHealthy$Freq != 0,]
        alluvialLodesBigHealthy = alluvialLodesBigHealthy[alluvialLodesBigHealthy$Cohort %in% alluvialLodesBigHealthy[alluvialLodesBigHealthy$x == "FF2",]$Cohort,]
        alluvialLodesBigHealthy$Category = factor(alluvialLodesBigHealthy$Category,   levels = c("Underweight", "Healthy",   "Overweight", "Obese"))
        
        # ---- General overview 2
        overweightEvolutionPlot = ggplot(alluvialLodesBigHealthy,
               aes(x = x, y = Freq, stratum = stratum, alluvium = Cohort,
                   fill = Category, label = stratum)) +
               scale_x_discrete(expand = c(.1, .1)) +
               scale_fill_discrete(drop = FALSE) +
               geom_flow() +
               geom_stratum(alpha = .5) +
               geom_text(stat = "stratum") +
               theme(legend.position = "none",
                     axis.title.x=element_blank(), axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())
        
        alluvialLodesBigHealthy = keepCategory(alluvialLodesBigHealthy, 3, "FF1")
        alluvialLodesBigHealthy$Frac = alluvialLodesBigHealthy$Freq / sum(alluvialLodesBigHealthy$Freq)
        alluvialLodesBigHealthy$ymax = cumsum(alluvialLodesBigHealthy$Frac)
        alluvialLodesBigHealthy$ymin = c(0, head(alluvialLodesBigHealthy$ymax, n=-1))
        alluvialLodesBigHealthy$labelPosition = (alluvialLodesBigHealthy$ymax + alluvialLodesBigHealthy$ymin) / 2
        alluvialLodesBigHealthy$HumanFrac = paste0(round(alluvialLodesBigHealthy$Frac * 100),"%")
        alluvialLodesBigHealthy$label =paste0(alluvialLodesBigHealthy$stratum, "\n", alluvialLodesBigHealthy$HumanFrac)

        # Make the plot
        overweightDonutPlot = ggplot(alluvialLodesBigHealthy, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
            geom_rect(colour="grey10") +
            scale_fill_discrete(drop = FALSE) +
            geom_label_repel( x=3.5, aes(y=labelPosition, label=label), max.overlaps = 20) +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "none") +
            ggtitle("FF2 Overweight source")         
        
    }    
    
    # ---- For Obese
    {
        # Delete the rows that are FF2 and not healthy
        alluvialLodesBigHealthy = alluvialLodesBig[!(alluvialLodesBig$x == "FF2" & alluvialLodesBig$stratum != "Obese"),]
        alluvialLodesBigHealthy = alluvialLodesBigHealthy[alluvialLodesBigHealthy$Freq != 0,]
        alluvialLodesBigHealthy = alluvialLodesBigHealthy[alluvialLodesBigHealthy$Cohort %in% alluvialLodesBigHealthy[alluvialLodesBigHealthy$x == "FF2",]$Cohort,]
        alluvialLodesBigHealthy$Category = factor(alluvialLodesBigHealthy$Category,   levels = c("Underweight", "Healthy",   "Overweight", "Obese"))
        
        # ---- General overview 2
        obeseEvolutionPlot = ggplot(alluvialLodesBigHealthy,
               aes(x = x, y = Freq, stratum = stratum, alluvium = Cohort,
                   fill = Category, label = stratum)) +
               scale_x_discrete(expand = c(.1, .1)) +
               scale_fill_discrete(drop = FALSE) +
               geom_flow() +
               geom_stratum(alpha = .5) +
               geom_text(stat = "stratum") +
               theme(legend.position = "none",
                     axis.title.x=element_blank(), axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())
        
        alluvialLodesBigHealthy = keepCategory(alluvialLodesBigHealthy, 3, "FF1")
        alluvialLodesBigHealthy$Frac = alluvialLodesBigHealthy$Freq / sum(alluvialLodesBigHealthy$Freq)
        alluvialLodesBigHealthy$ymax = cumsum(alluvialLodesBigHealthy$Frac)
        alluvialLodesBigHealthy$ymin = c(0, head(alluvialLodesBigHealthy$ymax, n=-1))
        alluvialLodesBigHealthy$labelPosition = (alluvialLodesBigHealthy$ymax + alluvialLodesBigHealthy$ymin) / 2
        alluvialLodesBigHealthy$HumanFrac = paste0(round(alluvialLodesBigHealthy$Frac * 100),"%")
        alluvialLodesBigHealthy$label =paste0(alluvialLodesBigHealthy$stratum, "\n", alluvialLodesBigHealthy$HumanFrac)

        # Make the plot
        obeseDonutPlot = ggplot(alluvialLodesBigHealthy, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
            geom_rect(colour="grey10") +
            scale_fill_discrete(drop = FALSE) +
            geom_label_repel( x=3.5, aes(y=labelPosition, label=label), max.overlaps = 20) +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "none") +
            ggtitle("FF2 Obese source")        
        
    }
    
   
    # ---- For Underweight
    {
        # Delete the rows that are FF2 and not healthy
        alluvialLodesBigHealthy = alluvialLodesBig[!(alluvialLodesBig$x == "FF2" & alluvialLodesBig$stratum != "Underweight"),]
        alluvialLodesBigHealthy = alluvialLodesBigHealthy[alluvialLodesBigHealthy$Freq != 0,]
        alluvialLodesBigHealthy = alluvialLodesBigHealthy[alluvialLodesBigHealthy$Cohort %in% alluvialLodesBigHealthy[alluvialLodesBigHealthy$x == "FF2",]$Cohort,]
        alluvialLodesBigHealthy$Category = factor(alluvialLodesBigHealthy$Category,   levels = c("Underweight", "Healthy",   "Overweight", "Obese"))
        
        # ---- General overview 2
        underweightEvolutionPlot = ggplot(alluvialLodesBigHealthy,
               aes(x = x, y = Freq, stratum = stratum, alluvium = Cohort,
                   fill = Category, label = stratum)) +
               scale_x_discrete(expand = c(.1, .1)) +
               scale_fill_discrete(drop = FALSE) +
               geom_flow() +
               geom_stratum(alpha = .5) +
               #geom_label(stat = "stratum") +
               geom_text(stat = "stratum") +
               theme(legend.position = "none",
                     axis.title.x=element_blank(), axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())
        
        
        alluvialLodesBigHealthy = keepCategory(alluvialLodesBigHealthy, 3, "FF1")
        alluvialLodesBigHealthy$Frac = alluvialLodesBigHealthy$Freq / sum(alluvialLodesBigHealthy$Freq)
        alluvialLodesBigHealthy$ymax = cumsum(alluvialLodesBigHealthy$Frac)
        alluvialLodesBigHealthy$ymin = c(0, head(alluvialLodesBigHealthy$ymax, n=-1))
        alluvialLodesBigHealthy$labelPosition = (alluvialLodesBigHealthy$ymax + alluvialLodesBigHealthy$ymin) / 2
        alluvialLodesBigHealthy$HumanFrac = paste0(round(alluvialLodesBigHealthy$Frac * 100),"%")
        alluvialLodesBigHealthy$label =paste0(alluvialLodesBigHealthy$stratum, "\n", alluvialLodesBigHealthy$HumanFrac)

        # Make the plot
        underweightDonutPlot = ggplot(alluvialLodesBigHealthy, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
            geom_rect(colour="grey10") +
            scale_fill_discrete(drop = FALSE) +
            geom_label_repel( x=3.5, aes(y=labelPosition, label=label), max.overlaps = 20) +
            coord_polar(theta="y") +
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "none") +
            ggtitle("FF2 Underweights source")
        
        
        
    }
    
    # Put the precise and all the rest into one single plot
    {
        
        smallPlotList = newList(4)
        smallPlotList[[1]] = underweightEvolutionPlot
        smallPlotList[[2]] = healthyEvolutionPlot
        smallPlotList[[3]] = overweightEvolutionPlot
        smallPlotList[[4]] = obeseEvolutionPlot        
        
        smallPlotList = newList(4)
        smallPlotList[[1]] = underweightDonutPlot
        smallPlotList[[2]] = healthyDonutPlot
        smallPlotList[[3]] = overweightDonutPlot
        smallPlotList[[4]] = obeseDonutPlot        
        
        subPlot = ggarrange(plotlist = smallPlotList, nrow = 2, ncol = 2)

        megaPlot = ggarrange(
            preciseEvolutionPlot,
            subPlot, 
            nrow = 1, ncol = 2
        )         
        

        imageWidth    = 30
        imageHeight   = 30
        imageFilePath = paste0(BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY, "specificEvolution.png")
        ggsave(imageFilePath, plot = megaPlot, width = imageWidth, height = imageHeight)  
        myCaption = "Evolution of BMI from FF1 (2010-2011) to FF2 (2013-2014) with respect total friends with high BMI"
        myLabel   = "fig:PreciseEvolutionFF1toFF2"
        writeImageLATEX2(imageFilePath, LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY, 
                         captionText   = myCaption,
                         overrideLabel = myLabel,
                         pageHeight    = 0.7, overrideFloat = TRUE)           
        
        
    }
    
    

    # Tables and heatmaps Where does you end up in FF2 depending of your FF1
	# status by number of BMICat friends
	{
		
	}
	
	    
    
    
    # R is shit, the function "install.package" doesn't exist but
    # "install.packages". Show lack of care for cohesive syntaxis
    
    
    

    
    

    
  
  
     # Check stimated probability againts actual results
        # For each person in FF1
                # Check how many obese friends do you have and how many are obese
                    # Find the stimate and roll a dice
                        # Assign to that person the future
  
        # For each person in FF2
            # Check assigned vs prediction
}