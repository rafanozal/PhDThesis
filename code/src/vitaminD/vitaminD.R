# Set up the hypercalcimia limits and hypervitaminosis limits
# TODO: Set up a table in the constant declaration with all these limits
#https://www.testing.com/tests/calcium/

# String library
library(stringr) 

# All the graph libraries, because R refuses to give me help about where the
# create_layout() function is suppose to be, not that it makes any sense that
# the script works without these because the stupid eviroment that is saved
# forever 
library(ggraph)
library(igraph)
library(statnet)
library(ggpubr)     # ggarrange()
library(ggalluvial) # Allovial plot
library(ggrepel)


# Analysis
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/analysis/analysisNetworks.R"),          encoding="utf-8")

# Plotting
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingBarplots.R"),     encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingBoxplots.R"),     encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingRegression.R"),   encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingNetwork.R"),      encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingHistograms.R"),   encoding="utf-8")

# Latex
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/latex/toolsLatex.R"),                   encoding="utf-8")

# Set the maximum and minimum ranges for Ca, VitD and PTH
# Count how many of each we have
{
  
    # ---- For BMI
    BMI_UNDERWEIGHT_LIMIT = 18.5
    BMI_OVERWEIGHT_LIMIT  = 25
    BMI_OBESE_LIMIT       = 30
    
    # ---- For Blood
    # Calcium
    CALCIUM_MAX_LIMIT  = 2.55
    CALCIUM_MIN_LIMIT  = 2.15
    # Vitamin D
    VITAMIND_MAX_LIMIT = 150
    VITAMIND_MIN_LIMIT = 50
    # PTH
    PTH_MAX_LIMIT      = 6.89
    PTH_MIN_LIMIT      = 1.59
    # Apolipo B
    APOB_MAX_LIMIT      = 1.25
    APOB_MIN_LIMIT      = 0.4
    # Retinol
    RETINOL_MAX_LIMIT   = 2.9
    RETINOL_MIN_LIMIT   = 0.9
    
    # Create a dataframe to export directly to Latex
    healthyRangesTable = DF(3,6)
    colnames(healthyRangesTable) = c("Variable", "Units", "Lower", "Upper", "Pct Under", "Pct Over")
    # -- Set up values here, because R is stupid and does weird NA logic
    underD   = sum(completeTable[!is.na(completeTable[,vitamimDIndex]),vitamimDIndex] < VITAMIND_MIN_LIMIT)
    overD    = sum(completeTable[!is.na(completeTable[,vitamimDIndex]),vitamimDIndex] > VITAMIND_MAX_LIMIT)
    underCa  = sum(completeTable[!is.na(completeTable[,calciumIndex]),calciumIndex] < CALCIUM_MIN_LIMIT)
    overCa   = sum(completeTable[!is.na(completeTable[,calciumIndex]),calciumIndex] > CALCIUM_MAX_LIMIT)
    underPTH = sum(completeTable[!is.na(completeTable[,pthIndex]),pthIndex] < PTH_MIN_LIMIT)
    overPTH  = sum(completeTable[!is.na(completeTable[,pthIndex]),pthIndex] > PTH_MAX_LIMIT)

    # -- Variable
    healthyRangesTable[1,1] = "25OHD"
    healthyRangesTable[2,1] = "Calcium"
    healthyRangesTable[3,1] = "PTH"
    # -- Units
    healthyRangesTable[1,2] = "nmol/L"
    healthyRangesTable[2,2] = "mmol/L"
    healthyRangesTable[3,2] = "pmol/L"
    # -- Lower
    healthyRangesTable[1,3] = VITAMIND_MIN_LIMIT
    healthyRangesTable[2,3] = CALCIUM_MIN_LIMIT
    healthyRangesTable[3,3] = PTH_MIN_LIMIT
    # -- Upper
    healthyRangesTable[1,4] = VITAMIND_MAX_LIMIT
    healthyRangesTable[2,4] = CALCIUM_MAX_LIMIT
    healthyRangesTable[3,4] = PTH_MAX_LIMIT
    # -- Total rows
    totalD   = sum(!is.na(completeTable[,vitamimDIndex]))
    totalCa  = sum(!is.na(completeTable[,calciumIndex]))
    totalPTH = sum(!is.na(completeTable[,pthIndex])) 
    # -- Under
    healthyRangesTable[1,5] = paste0(    round(    100 * underD   / totalD    ,2),"%")
    healthyRangesTable[2,5] = paste0(    round(    100 * underCa  / totalCa   ,2),"%")
    healthyRangesTable[3,5] = paste0(    round(    100 * underPTH / totalPTH  ,2),"%")
    # -- Over
    healthyRangesTable[1,6] = paste0(round(100*overD/totalD,2),"%")
    healthyRangesTable[2,6] = paste0(round(100*overCa/totalCa,2),"%")
    healthyRangesTable[3,6] = paste0(round(100*overPTH/totalPTH,2),"%")


    #writeTableLATEX(healthyRangesTable, VITAMIND_FOLDER,
     #               tableCaption = "Healthy range for variable and percentage of people outside.",
      #              widthProportion = 0.8, heightProportion = 0.05)
    
    
    # -- Add cut off values for the plots
    
    

    # For the BMI
    cutOffBMI      = newList(3)
    cutOffBMI[[1]] = as.numeric(c(BMI_UNDERWEIGHT_LIMIT, BMI_OVERWEIGHT_LIMIT, BMI_OBESE_LIMIT))
    cutOffBMI[[2]] = c("Underweight", "Overweight", "Obese")
    cutOffBMI[[3]] = c("#00AA00","#00AA00", "#00AA00")
        
    # For the blood
    cutOffCalcium      = newList(3)
    cutOffCalcium[[1]] = as.numeric(c(CALCIUM_MIN_LIMIT, CALCIUM_MAX_LIMIT))
    cutOffCalcium[[2]] = c("Lower Ca", "Upper Ca")
    cutOffCalcium[[3]] = c("#FF0000","#FF0000")
    
    cutOffVitD      = newList(3)
    cutOffVitD[[1]] = as.numeric(c(VITAMIND_MIN_LIMIT, VITAMIND_MAX_LIMIT))
    cutOffVitD[[2]] = c("Lower 25OHD", "Upper 25OHD")
    cutOffVitD[[3]] = c("#FF0000","#FF0000")
    
    cutOffPTH      = newList(3)
    cutOffPTH[[1]] = as.numeric(c(PTH_MIN_LIMIT, PTH_MAX_LIMIT))
    cutOffPTH[[2]] = c("Lower PTH", "Upper PTH")
    cutOffPTH[[3]] = c("#FF0000","#FF0000")

    cutOffAPOB      = newList(3)
    cutOffAPOB[[1]] = as.numeric(c(APOB_MIN_LIMIT, APOB_MAX_LIMIT))
    cutOffAPOB[[2]] = c("Lower APOB", "Upper APOB")
    cutOffAPOB[[3]] = c("#FF0000","#FF0000")    
    
    cutOffRetinol      = newList(3)
    cutOffRetinol[[1]] = as.numeric(c(RETINOL_MIN_LIMIT, RETINOL_MAX_LIMIT))
    cutOffRetinol[[2]] = c("Lower Retinol", "Upper Retinol")
    cutOffRetinol[[3]] = c("#FF0000","#FF0000")        
    
    
    
    
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

            plotResults = doGraphPlot(allEdges[[i]],  completeTable, VITAMIND_FOLDER_NETWORK,
                                      sizeVariableIndex = overallConnectionsIndex,
                                      selectedLayouts   = DO_THIS_LAYOUTS,
                                      plotTitle         = currentPlotTitle,
            	                      plotTheme         = "blank",
                                      overrideTableName = currentOverridedPlotName) 
    
            # Save the current plot for the grid image
            myListOfPlots[i]          = plotResults[[2]]
            myListOfPlotsObjects[[i]] = plotResults[[1]]

        }
  
        # Make the grid image for all the networks
        totalGridColumns = 2
        totalGridRows    = ceiling(TOTAL_NETWORKS/totalGridColumns)

        # ---- PNG / PDF
        ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows)
        ggsave(BIOMARKERS_GRAPH_GRID, width = 11, height = 16)
        ggsave(changeFileExtension(VITAMIND_GRAPH_GRID, "pdf"), width = 22, height = 32) # Number for image size are different on porpuse. The pdf saved is not the same, border for the nodes are thicker and it looks weird
        
        writeImageLATEX2(VITAMIND_GRAPH_GRID, LATEX_RELATIVE_VITAMIND_FOLDER_IMAGES_NETWORK, 
                         captionText   = "Overview of all friendship networks. The figure is reproduced from Social network analysis of Staphylococcus aureus
                                          carriage in a general youth population with permissions. DOI: 10.1016/j.ijid.2022.08.018",
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
    
        plotObject = doHistogramPlot2(completeTable, overviewIndex, VITAMIND_FOLDER_NETWORK,
                                      colorsVector   = COLOR_FRIENDSHIP,
                                      binsWidth      = 1,
                                      binBorderColor = "#333333",
                                      plotTitle      = " Does these friends give a good overview of your social network? ",
                                      plotSubtitle   = " 0 = Low, 10 = High",
                                      plotXLabel     = "Points", plotYLabel = "Total")       
        
        
        
        writeImageLATEX2(plotObject[[2]], LATEX_RELATIVE_VITAMIND_FOLDER_IMAGES_NETWORK, 
                         captionText   = "Histogram with how good is the self-reported network (0 - 10, x-axis) by each student (y-axis).The figure is reproduced
                                          from Social network analysis of Staphylococcus aureus carriage in a general youth population with permissions. DOI: 10.1016/j.ijid.2022.08.018",
                         overrideLabel = "fig:histogramFriendship", 
                         pageHeight    = 0.4, overrideFloat = TRUE)        
        
    }
	
	# -- Same, but for each high school individually
	{
	    myHighschools        = getCategories(completeTable, highSchoolIndex)
	    myListOfPlotsObjects = newList( length(myHighschools))
	    
	    for(i in 1:length(myHighschools)){
	    
	            currentHighschool = myHighschools[i]
	            currentSubtable   = keepCategory(completeTable, highSchoolIndex, currentHighschool)
	        
	            currentPlotTitle      = paste(currentHighschool, " network overview")
	            currentTableOverride  = paste(currentHighschool, "_overview")
	            
	            myListOfPlotsObjects[[i]] = doHistogramPlot2(currentSubtable, overviewIndex, VITAMIND_FOLDER_NETWORK,
	                                                         colorsVector   = HIGHSCHOOL_COLOR_VECTOR[i],
	                                                         binsWidth      = 1,
	                                                         binBorderColor = "#333333",
	                                                         plotTitle      = currentPlotTitle,
	                                                         plotSubtitle   = " 0 = Low, 10 = High",
	                                                         plotXLabel     = "Points", plotYLabel = "Total",
	                                                         overrideTableName = currentTableOverride)[[1]]
	            
	    }
	        
	    ggarrange(plotlist =  myListOfPlotsObjects , nrow = 4, ncol = 2)
	        
	    allGraphPath  = file.path(paste(VITAMIND_FOLDER_NETWORK, "4x2HistoFriends.png", sep = ""))
	    ggsave(allGraphPath, width = 8, height = 16)
	
	    latexPlot = writeImageLATEX2(allGraphPath, VITAMIND_FOLDER_NETWORK, 
	    	                         pageWidth = 0.5,
	                                 captionText   = "For each of the highschools (H1 to H8), histogram with how good is the self-reported network (0 - 10, x-axis) by each student (y-axis)",
	                                 overrideLabel = "fig:allOverviewAllHS",
	                                 pageHeight = 1.0, overrideFloat = TRUE)   	
	}
	
	# -- Homophily for each highschool 
	{

    	# Homophily for all
    	homoResults = completeHomophilyV3(overallGraph, completeTable, highSchoolIndex)
    	# ---- Add a column for the overall grade
    	# ---- Delete the Delta, sign, difference, and p-value as there are not needed
    	homoResults[,c(4,5,6,7)]       = NULL
    	homoResults$OverviewAverage    = 0
    	homoResults$OverviewAverage[1] = mean(completeTable$Overview,   na.rm = TRUE)
        homoResults$OverviewMedian     = 0
        homoResults$OverviewMedian     = median(completeTable$Overview, na.rm = TRUE)
    	
    	# For each HS
    	myHighschools        = getCategories(completeTable, highSchoolIndex)
    	myListOfPlotsObjects = newList( length(myHighschools))
    
    	for(i in 1:length(myHighschools)){
    		
            currentHighschool                = myHighschools[i]
            currentSubtable                  = keepCategory(completeTable, highSchoolIndex, currentHighschool)
            homoResults$OverviewAverage[1+i] = mean(currentSubtable[,overviewIndex],   na.rm = TRUE)
            homoResults$OverviewMedian[1+i]  = median(currentSubtable[,overviewIndex], na.rm = TRUE)
            
    	}
    
	    # Round the numbers in the homophily table
    	homoResults[,2]  = round(homoResults[,2],2)
    	homoResults[,3]  = round(homoResults[,3],2)
    	homoResults[,5]  = round(homoResults[,5],2)
    	# Delete the weird NAs
    	homoResults[1,3] = ""
    	homoResults[1,4] = ""
    	
    	# Change the column names
    	colnames(homoResults) = c("High School", "Homophily", "Frequency", "Significance", "Average", "Median")
    
    	# Change the first row to ALL
    	homoResults[1,1] = "All"
    	
    	writeTableLATEX(homoResults, VITAMIND_FOLDER_TABLES_NETWORK,
        	            tableCaption = "Highschools and friendship overview. The first row represent all high schools combined. H1 to H8 rows represent each high school separetly.
    		                            Homophily represent how many students of this school form friendship with a student of the same school. Frequency is the relative frequency
    		                            of students in each high school. Significance is the p-value of a two sided binomial test of relationships with-in same high school, total
    		                            relationships, and relative frequency of students in each high-school. Average and Median are the values of how good is the self-reported
    		                            network (0 - 10) by each student in each high school.",
            	        overrideTableName = "HSHomophily",
                	    widthProportion = 0.7, heightProportion = 0.09)          
    
}	
	
}

# ------------------------------------------------------------------------------
# Diseases and drugs count
# ------------------------------------------------------------------------------
{

	
	# Diseases
	{
	
		# Total Count
		{

			# Make the diseases summary table
			myDiseases    = unique(diseasesDBDF$ICD10)
			totalDiseases = length(myDiseases)
			diseasesSummaryDF = DF(totalDiseases, 4)
			colnames(diseasesSummaryDF) = c("Diagnostic", "ICD10", "Group", "Total")
			
			# Fill and init the table
			diseasesSummaryDF$Total = 0
			diseasesSummaryDF$ICD10 = myDiseases
			
			for(i in 1:totalDiseases){
				
				currentICD = myDiseases[i]
					
				tempTable  = diseasesDBDF[diseasesDBDF$ICD10 == currentICD,]
				diseasesSummaryDF$Diagnostic[i] = tempTable[1,2]
				diseasesSummaryDF$Group[i]      = tempTable[1,4]
				diseasesSummaryDF$Total[i]      = nrow(tempTable)
				
			}
			
			# Sort by ICD
			diseasesSummaryDF = diseasesSummaryDF[order(diseasesSummaryDF$ICD10), ]
			
		    writeTableLATEX(diseasesSummaryDF, VITAMIND_FOLDER_TABLES_DISEASE,
		        	            tableCaption      = "Summary of all diseases in the population. From left to right, diagnostic of the disease, ICD10, group to which this disease bellongs, and total cases.",
		            	        overrideTableName = "diseasesTable",
		                	    widthProportion   = 0.7, heightProportion = 0.5)  			
						
		}
		
		# Relevant diseases by high-school
		{
			
			importantDiseases = c("K90.0", "T78.4", "E73.9", "K29", "F50.9", "M13", "M92.40", "K50.90", "K25", "M41", "M45.9", "M08", "M13", "M86.9", "N18.2")
			
			idWithDisease = diseasesDBDF[diseasesDBDF$ICD10 %in% importantDiseases,]$ID
			
			tempTable = completeTable[completeTable$ID %in% idWithDisease,]
			
			summarizeCategorical(tempTable, highSchoolIndex, sorted = "None")
			summarizeCategorical(completeTable, highSchoolIndex, sorted = "None")
			
		}
			
		
	}
	
	
    # Medication
	{
	
		# Total Count
		{
			
		    # Make the medicine summary table
			myDrugs    = unique(medicinesDBDF$ATC)
			totalDrugs = length(myDrugs)
			drugsSummaryDF = DF(totalDrugs, 4)
			colnames(drugsSummaryDF) = c("Brand", "ATC", "Type", "Total")
			
			# Fill and init the table
			drugsSummaryDF$Total = 0
			drugsSummaryDF$ATC = myDrugs
			
			for(i in 1:totalDrugs){
				
				currentATC = myDrugs[i]
					
				tempTable  = medicinesDBDF[medicinesDBDF$ATC == currentATC,]
				drugsSummaryDF$Brand[i] = tempTable[1,3]
				drugsSummaryDF$Type[i]  = tempTable[1,2]
				drugsSummaryDF$Total[i] = nrow(tempTable)
				
			}
			
			# Sort by ICD
			drugsSummaryDF = drugsSummaryDF[order(drugsSummaryDF$ATC), ]
			
		    writeTableLATEX(drugsSummaryDF, VITAMIND_FOLDER_TABLES_MEDICINE,
		        	        tableCaption      = "Summary of all medicine comsuption in the population. From left to right,
		    	                                 brand of the medicine, ATC, type of medicine, and total consumption.",
		            	    overrideTableName = "drugTable",
		                	widthProportion   = 0.7, heightProportion = 0.3)  				
			
		}
		
		# Relevant
		{
		
			
			tempTable = completeTable[completeTable$ID %in% medicinesDBDF[medicinesDBDF$ATC == "M01A",1],]
			summarizeCategorical(tempTable, highSchoolIndex, sorted = "None")
			summarizeCategorical(completeTable, highSchoolIndex, sorted = "None")
			
		}
			
	}
 
}

# ------------------------------------------------------------------------------
# Create the general overview of the population (Table 0)
# ------------------------------------------------------------------------------
{

	
	variablesOfInterest = c(sexIndex, BMICatIndex, healthIndex, smokeIndex, snuffIndex, alcoholIndex, sportsIndex, screenIndex, highSchoolIndex, solariumIndex)
	targetVariables     = c(vitamimDIndex, pthIndex, calciumIndex)
	
	totalDF   = readyDFModalities(completeTable, variablesOfInterest, targetVariables)
	freqDF    = readyDFModalities(completeTable, variablesOfInterest, targetVariables)
	underDF   = readyDFModalities(completeTable, variablesOfInterest, targetVariables)
	overDF    = readyDFModalities(completeTable, variablesOfInterest, targetVariables)
	
	pValuesDF = readyDFVariables(completeTable, variablesOfInterest, targetVariables)
	
	
	modalitiesPerVariable = getTotalModalitiesV2(completeTable, variablesOfInterest, FALSE)
	
	generalCounter     = 1
	
	# For each of the columns
	if(FALSE)for (j in 1:length(targetVariables)) {
	
		# Reset the row index
		currentModalityRow = 1	
		# For each of the variables
		for (i in 1:length(variablesOfInterest)) {	
	
			# Skip the variable name row
			currentModalityRow = currentModalityRow + 1

			# Do the categorical analysis
			
			myCurrentBoxplotResults = doBoxPlotV2(completeTable, targetVariables[j], VITAMIND_FOLDER_GENERAL,
                                                  groupIndex = variablesOfInterest[i],
                                                  showPValues = TRUE,
				                                  overrideTableName = paste0(j,"_",i))
			
			# For each modality of each variable
			currentModalities = modalitiesPerVariable[i]
			for (k in 1:currentModalities) {
			
				# Get the modality name
				currentModalityName = totalDF[currentRow,1]
					
				totalDF[currentModalityRow,j+1] = generalCounter
				
				currentModalityRow = currentModalityRow + 1
				generalCounter = generalCounter + 1
				
			}
		}
	}
		
	
	# Do solarium 
	myNewTable = completeTable
	summarizeCategorical(myNewTable, solariumIndex)
	myNewTable = deleteNA(myNewTable, solariumIndex)
	doBoxPlotV2(myNewTable, vitamimDIndex, VITAMIND_FOLDER_GENERAL,
                                          groupIndex = solariumIndex,
                                          showPValues = TRUE)
	myNewTable = deleteNA(myNewTable, vitamimDIndex)
	sum(myNewTable[myNewTable[,solariumIndex] == "Yes",vitamimDIndex] < 50)
	sum(myNewTable[myNewTable[,solariumIndex] == "No", vitamimDIndex] < 50)
	
	
	# I'm just going to do this manually; since R sucks calling functions on objects this is a nightmare to program
	{
	
		# Change the guy with no screen time to the next category
		completeTable[573, screenIndex] = "About half an hour"
		
		myNewTable = completeTable
		myNewTable = keepCategory(myNewTable, solariumIndex, "No")
		
		# For the general population
		for (i in 1:length(variablesOfInterest)) {
			currentVariableName = colnames(myNewTable)[variablesOfInterest[i]]
			print("")
			print(currentVariableName)
			print("")
			print(summarizeCategorical(myNewTable, variablesOfInterest[i], sorted = "none"))
			print("----")
		}
		
		# For the 25OHD levels
		# For each of the columns
		for (i in 1:length(variablesOfInterest)) {
			currentVariableName = colnames(myNewTable)[variablesOfInterest[i]]
			print("")
			print(currentVariableName)
			print("")
			
			# Delete the unknown for the analysis
			temporalTable = myNewTable
			temporalTable = deleteCategory(temporalTable, variablesOfInterest[i], "Unknown")
			temporalTable = deleteNA(temporalTable, variablesOfInterest[i])
			
			myCurrentBoxplotResults = doBoxPlotV2(temporalTable, targetVariables[1], VITAMIND_FOLDER_GENERAL,
                                                  groupIndex = variablesOfInterest[i],
                                                  showPValues = TRUE,
		                                          overrideTableName = paste0(j,"_",i))
					
			print(myCurrentBoxplotResults)
					
					
			print("----")
		}

		
		# Check how many are in the lower/high levels
		modalitiesPerVariable = getTotalModalitiesV2(myNewTable, variablesOfInterest, FALSE)
		for (i in 1:length(variablesOfInterest)){
			
			currentVariableName = colnames(myNewTable)[variablesOfInterest[i]]
			print("")
			print(currentVariableName)
			print("")			
			
			# For each modality of each variable
			currentModalities = modalitiesPerVariable[i]
			namesModalities   = getModalities(myNewTable, variablesOfInterest[i])
			for (k in 1:currentModalities) {
			
				# Get the modality name
				currentModalityName = namesModalities[k]
				
				tempTable = keepCategory(myNewTable, variablesOfInterest[i], currentModalityName)
				
				# R is a stupid language, you can't compare NA > Value because is NA -_-	
				tempList  = tempTable[,vitamimDIndex]
				tempList  = tempList[!is.na(tempList)]
				
				print(currentModalityName)
				
				print(  sum( tempList < VITAMIND_MIN_LIMIT  )  )
				
				
			}
			
		}
		
		
	}
	
	# Sport is weird, check again
	myCurrentBoxplotResults = doBoxPlotV2(completeTable, vitamimDIndex, VITAMIND_FOLDER_GENERAL,
                                          groupIndex = sportsIndex,
                                          showPValues = TRUE)
	
	
	# Screen time results are inconsistence, check again
	tempTable  = deleteCategory(nonSolariumOnlyTable, screenIndex, "Unknown")
	tempTable  = deleteNA(tempTable, screenIndex)
	myCurrentBoxplotResults = doBoxPlotV2(tempTable, vitamimDIndex, VITAMIND_FOLDER_GENERAL,
                                          groupIndex = screenIndex,
                                          showPValues = TRUE)	
	
	
	
	# Do sunbathing
	myNewTable = completeTable
	summarizeCategorical(myNewTable, sunbathingIndex)
	myNewTable = deleteNA(myNewTable, sunbathingIndex)
	myBoxPlot  = doBoxPlotV2(myNewTable, vitamimDIndex, VITAMIND_FOLDER_GENERAL,
                                          groupIndex = sunbathingIndex,
                                          showPValues = TRUE)
	myNewTable = deleteNA(myNewTable, vitamimDIndex)
	sum(myNewTable[myNewTable[,sunbathingIndex] == "Yes",vitamimDIndex] < 50)
	sum(myNewTable[myNewTable[,sunbathingIndex] == "No", vitamimDIndex] < 50)
	
	myNewTable = nonSolariumOnlyTable
	summarizeCategorical(myNewTable, sunbathingIndex)
	myNewTable = deleteNA(myNewTable, sunbathingIndex)
	myBoxPlot  = doBoxPlotV2(myNewTable, vitamimDIndex, VITAMIND_FOLDER_GENERAL,
                                          groupIndex = sunbathingIndex,
                                          showPValues = TRUE)
	myNewTable = deleteNA(myNewTable, vitamimDIndex)
	sum(myNewTable[myNewTable[,sunbathingIndex] == "Yes",vitamimDIndex] < 50)
	sum(myNewTable[myNewTable[,sunbathingIndex] == "No", vitamimDIndex] < 50)		
	
	
	
	
}


# ------------------------------------------------------------------------------
# Check again bias by high school for non-solariums
# ------------------------------------------------------------------------------
{

	
	myHighschools       = getCategories(completeTable, highSchoolIndex)
	variablesOfInterest = c(sexIndex, BMICatIndex, healthIndex, smokeIndex, snuffIndex, alcoholIndex, sportsIndex, screenIndex, sunbathingIndex)
	targetVariables     = c(vitamimDIndex, pthIndex, calciumIndex)
	
	# Create the dataframe where we put the results
	resultsDF             = DF(length(variablesOfInterest), length(myHighschools) + 1)
	colnames(resultsDF)   = c("Variable", myHighschools)
	resultsDF[,1]         = colnames(completeTable)[variablesOfInterest]
	
	# Get the non solarium, and keep those with valid 25OHD only
	nonSolariumOnlyTable = completeTable[completeTable[,solariumIndex] == "No",]
	nonSolariumOnlyTable = nonSolariumOnlyTable[!is.na(nonSolariumOnlyTable$X25.OH.D_.nmol.L.),]
	
	# For each highschol
	for(i in 1:length(myHighschools)){
		
		currentHighschool = myHighschools[i]
	
		print(currentHighschool)
		print("-----------------------")
		
		
		tempTableA = keepCategory(nonSolariumOnlyTable, highSchoolIndex, currentHighschool)
		
		# Gor each variable of interest
		for(j in 1:length(variablesOfInterest)){
		
			currentVariable = variablesOfInterest[j]
			
			
			print(colnames(completeTable)[currentVariable])
			print(".........")	
			
			tempTableB  = deleteCategory(tempTableA, currentVariable, "Unknown")
			tempTableB  = deleteNA(tempTableB, currentVariable)
			tempTableB  = deleteNA(tempTableB, vitamimDIndex)
			
			overrideTableName = paste0(currentHighschool,"_",currentVariable,"_bias")
			
			myCurrentBoxplotResults = doBoxPlotV2(tempTableB, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HS,
            		                              groupIndex = currentVariable,
				                                  overrideTableName = overrideTableName,
                    		                      showPValues = TRUE)	

			resultsDF[j,i+1] = myCurrentBoxplotResults[[3]][[2]]

		}	
		
		
	}
	
	
	# Seriously, R is complete garbage, there is no immediate way to convert a dataframe of p-values
	# Bonferroni / Benjamini. You need to convert them to a vector, which there is no way to do
	# easily either!, then convert, then put it back. IT HAS BEEN LIKE 50 YEARS SINCE THIS LANGUAGE
	# WAS RELEASED, THIS IS UNNACEPTABLE!!!!
	pValuesVector = rep(0, length(variablesOfInterest) * length(myHighschools))
	for(i in 1:length(variablesOfInterest)){
		
		for(j in 1:length(myHighschools)){
	
			# If there is not enought data you get a -1 as error, so addjust for that and turn it into a 1 for future conversion
			if(resultsDF[i,j+1] < 0 )
			
				pValuesVector[(i-1) * length(myHighschools) + j] = 1
			
			else
			
				pValuesVector[(i-1) * length(myHighschools) + j] = resultsDF[i,j+1]

			
		}
		
	}
	
	# Addjust this many p-values
	pValuesVectorBenjamini = p.adjust(pValuesVector,  method = "bonferroni")  
	
	# Put it back into DF form
	resultsBenjaminiDF  = resultsDF
	resultsBonferroniDF = resultsDF
	
	for(i in 1:length(variablesOfInterest)){
		
		for(j in 1:length(myHighschools)){

			resultsBenjaminiDF[i,j+1] = pValuesVectorBenjamini[(i-1) * length(myHighschools) + j]

		}
		
	}
	
	# Make it start form for easy reading
	
	resultsAsteriskDF = resultsDF
	
	for(j in 1:length(myHighschools)){
	
		resultsAsteriskDF[,j+1] = getAsterkisPValue(resultsDF[,j+1])
		
	}
	
	
	resultsBenjaminiAsteriskDF = resultsBenjaminiDF
	
	for(j in 1:length(myHighschools)){
	
		resultsBenjaminiAsteriskDF[,j+1] = getAsterkisPValue(resultsBenjaminiDF[,j+1])
		
	}
	
	# Save the raw p-values and benjamini p-values in CSV form
	noCorrectionPath = paste0(VITAMIND_FOLDER_TABLES_HS,"noCorrection.csv")
	benjaminiPath    = paste0(VITAMIND_FOLDER_TABLES_HS,"benjamini.csv")
	write.csv2(resultsDF,          noCorrectionPath)
	write.csv2(resultsBenjaminiDF, benjaminiPath)
	
	
	# Check women in H8, they have way too much vitD
    tempTableA = keepCategory(nonSolariumOnlyTable, highSchoolIndex, "H8")
	tempTableB = deleteCategory(tempTableA, sexIndex, "Unknown")
	tempTableB = deleteNA(tempTableB, sexIndex)
	tempTableB = deleteNA(tempTableB, vitamimDIndex)
	
	myCurrentBoxplotResults = doBoxPlotV2(tempTableB, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HS,
					                      groupIndex = sexIndex,
		                                  overrideTableName = "tempPlot",
		                                  showPValues = TRUE)	
	
	xiResultsA = categoricalXiV2(tempTableB, sportsIndex , sexIndex)
	tempTableB$HolidaySunbathing
	
	myCurrentBoxplotResults = doBoxPlotV2(tempTableB, BMIIndex, VITAMIND_FOLDER_IMAGES_HS,
					                      groupIndex = sexIndex,
		                                  overrideTableName = "tempPlot",
		                                  showPValues = TRUE)		
	
		
	# Check sports in H3, they have way too much vitD
    tempTableA = keepCategory(nonSolariumOnlyTable, highSchoolIndex, "H3")
	tempTableB = deleteCategory(tempTableA, sportsIndex, "Unknown")
	tempTableB = deleteNA(tempTableB, sportsIndex)
	tempTableB = deleteNA(tempTableB, vitamimDIndex)
	
	myCurrentBoxplotResults = doBoxPlotV2(tempTableB, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HS,
					                      groupIndex = sportsIndex,
		                                  overrideTableName = "tempPlot",
		                                  showPValues = TRUE)	
	
	myCurrentBoxplotResults = doBoxPlotV2(tempTableB, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HS,
					                      groupIndex = transportWinterIndex,
		                                  overrideTableName = "tempPlot",
		                                  showPValues = TRUE)	
	
	
	xiResultsA = categoricalXiV2(tempTableB, sportsIndex , transportWinterIndex)
	tempTableB$HolidaySunbathing
	
	myCurrentBoxplotResults = doBoxPlotV2( nonSolariumOnlyTable, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HS,
					                      groupIndex = transportWinterIndex,
		                                  overrideTableName = "tempPlot",
		                                  showPValues = TRUE)		
	
	
	tempTableB = deleteNA(completeTable, sportsIndex)
	
	completeTable$HolidaySunbathing 
	
	sunbathingIndex
	
	summarizeCategorical(completeTable, sunbathingIndex)
	
	xiResultsA = categoricalXiV2(nonSolariumOnlyTable, sportsIndex , sunbathingIndex)
	
	tempTableA$WinterTransport
	
	
	# Check sports in H6, with travelling
    tempTableA = keepCategory(nonSolariumOnlyTable, highSchoolIndex, "H6")
	tempTableB = deleteCategory(tempTableA, sunbathingIndex, "Unknown")
	tempTableB = deleteNA(tempTableB, sunbathingIndex)
	tempTableB = deleteNA(tempTableB, vitamimDIndex)
	
	myCurrentBoxplotResults = doBoxPlotV2(tempTableB, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HS,
					                      groupIndex = sunbathingIndex,
		                                  overrideTableName = "tempPlot",
		                                  showPValues = TRUE)	
	
	# Check sports in H1, with travelling
    tempTableA = keepCategory(nonSolariumOnlyTable, highSchoolIndex, "H1")
	tempTableB = deleteCategory(tempTableA, sunbathingIndex, "Unknown")
	tempTableB = deleteNA(tempTableB, sunbathingIndex)
	tempTableB = deleteNA(tempTableB, vitamimDIndex)
	
	myCurrentBoxplotResults = doBoxPlotV2(tempTableB, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HS,
					                      groupIndex = sunbathingIndex,
		                                  overrideTableName = "tempPlot",
		                                  showPValues = TRUE)	
	
	# Questionaries
	tempTableA = keepCategory(nonSolariumOnlyTable, highSchoolIndex, "H1")
	min(tempTableA$QuestionaryDateFF1, na.rm = TRUE)
	max(tempTableA$QuestionaryDateFF1, na.rm = TRUE)
	
	
	# Sunbathing for non solariums
	tempTableB = deleteCategory(nonSolariumOnlyTable, sunbathingIndex, "Unknown")
	tempTableB = deleteNA(tempTableB, sunbathingIndex)
	tempTableB = deleteNA(tempTableB, vitamimDIndex)
	
	myCurrentBoxplotResults = doBoxPlotV2(tempTableB, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HS,
					                      groupIndex = sunbathingIndex,
		                                  overrideTableName = "tempPlot",
		                                  showPValues = TRUE)
	
	
	xiResultsA = categoricalXiV2(tempTableB, highSchoolIndex , sunbathingIndex)
	
	writeTableLATEX(xiResultsA[[7]], VITAMIND_FOLDER_TABLES_HS,
					tableCaption      = "Xi-square test for sunbathing and highschool for non-solarium users.",
		            overrideTableName = "sunbathingHSTable",
		            widthProportion   = 0.7, heightProportion = 0.15)			
	
	
	
	
	summarizeBicategoricalV3(tempTableB, highSchoolIndex, sunbathingIndex)
	
	
	summarizeCategorical(tempTableB, sunbathingIndex)
	
	nonSolariumOnlyTable
	
	
	
	# Let check social dynamics for H8
	myHighschoolTable = keepCategory(nonSolariumOnlyTable, highSchoolIndex, "H8")
	
	# Reduce the graph to only people in H8
	myIDs = myHighschoolTable$ID
	h8OverallEdgesDF = overallEdgesDF
	h8OverallEdgesDF = h8OverallEdgesDF[h8OverallEdgesDF$from %in% myIDs,]
	h8OverallEdgesDF = h8OverallEdgesDF[h8OverallEdgesDF$to   %in% myIDs,]
	
	h8Graph = graph_from_data_frame(h8OverallEdgesDF,  vertices = myHighschoolTable, directed = T)
	
	
	#
# (String) selectedLayout: Which layout do you want to use. The default is
#                         'mds'. The possible layouts are:
# 
#                         'manual' - Mark this if you want to use your own
#                                    layout (see next)
#
#                         'gem',
#                         'dh',
#                         'graphopt',
#                         'mds' = multidimensional scaling, it tries to keep a balance in between everything.
#                         'fr' = Fruchterman - Reingold , it keeps related vertices toguether
#                         'kk' = Kawai - Emphases distance as information
#                         'drl'
#                         'lgl'
	
	
	# Correct vitD to graph visualization
	myHighschoolTable[,vitamimDIndex] = myHighschoolTable[,vitamimDIndex] * 0.3
	        
    myGraphPlot = doGraphPlot(h8OverallEdgesDF,  myHighschoolTable, VITAMIND_FOLDER_NETWORK,
                              highlightVariable = sexIndex,
                              colorVectorHighlight = COLOR_VECTOR_SEX,
				              sizeVariableIndex = vitamimDIndex,
    	                      selectedLayouts = "fr",
                              plotTitle    = "Non solarium relationships in Highschool 8",
                              plotSubtitle = "Fruchterman - Reingold layout with node size proportional to 25OHD levels",
        		              plotTheme    = "blank",
				              overrideTableName  = "H8SubGraphNonSolariums",
                              overrideCaption    = "H8 School only",
                              overrideLegendSize = 5,
                              overrideLegendPosition = c(0.9, 0.1)) 
	
    writeImageLATEX2(myGraphPlot[[2]][1], LATEX_RELATIVE_VITAMIND_FOLDER_IMAGES_NETWORK, 
                     captionText   = "Relationships in H8 for non solarium users highlighted by sex. Node size is proportional to 25OHD level. Layout of the nodes using Fruchterman - Reingold.",
                     overrideLabel = "fig:H8NonSolRelationships", 
                     pageHeight    = 0.7, overrideFloat = TRUE)
    
    
    myHighschoolTableWomen = keepCategory(myHighschoolTable, sexIndex, "Woman")
    myHighschoolTableMen   = keepCategory(myHighschoolTable, sexIndex, "Man")
    
    
	myCurrentBoxplotResults = doBoxPlotV2(myHighschoolTableMen, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HS,
					                      groupIndex = sportsIndex,
		                                  overrideTableName = "tempPlot",
		                                  showPValues = TRUE)  
	
	
	summarizeGraph(h8OverallEdgesDF, myHighschoolTable)
    
	xiResultsA = categoricalXiV2(myHighschoolTable, sportsIndex  , sexIndex)	
	xiResultsA = categoricalXiV2(myHighschoolTable, BMICatIndex  , sexIndex)	
	xiResultsA = categoricalXiV2(myHighschoolTable, snuffIndex   , sexIndex)		
	xiResultsA = categoricalXiV2(myHighschoolTable, alcoholIndex , sexIndex)		
    xiResultsA = categoricalXiV2(myHighschoolTable, smokeIndex   , sexIndex)			
	xiResultsA = categoricalXiV2(myHighschoolTable, sunbathingIndex   , sexIndex)			    
	
	
	xiResultsA = categoricalXiV2(completeTable, highSchoolIndex   , sexIndex)			    
	as.numeric(xiResultsA[[9]][1:9,2]) / as.numeric(xiResultsA[[9]][1:9,4])
	
	doBarPlotV2(myHighschoolTable, sportsIndex, VITAMIND_FOLDER_IMAGES_HS,
                groupIndex = sexIndex,
                colorsVector = NULL,
                countingType = "relative",   # absolute, relative
                overrideTableName = "tempPlot")
	
	doBarPlotV2(myHighschoolTable, BMICatIndex, VITAMIND_FOLDER_IMAGES_HS,
                groupIndex = sexIndex,
                colorsVector = NULL,
                countingType = "relative",   # absolute, relative
                overrideTableName = "tempPlot")	
	
	
	variablesOfInterest = c(sexIndex, BMICatIndex, healthIndex, smokeIndex, snuffIndex, alcoholIndex, sportsIndex)
	print(completeHomophilyV3(h8Graph, myHighschoolTable, variablesOfInterest[2]))
	
	
	
	# How is homophyly by sex in each high school
	myHighschools
	for (i in 1:length(myHighschools)){
		
		currentHighschool = myHighschools[i]
		
		# Let check social dynamics for H8
		currentHighschoolTable = keepCategory(nonSolariumOnlyTable, highSchoolIndex, currentHighschool)
	
		# Reduce the graph to only people in H8
		myIDs = currentHighschoolTable$ID
		currentOverallEdgesDF = overallEdgesDF
		currentOverallEdgesDF = currentOverallEdgesDF[currentOverallEdgesDF$from %in% myIDs,]
		currentOverallEdgesDF = currentOverallEdgesDF[currentOverallEdgesDF$to   %in% myIDs,]
		
		currentGraph = graph_from_data_frame(currentOverallEdgesDF,  vertices = currentHighschoolTable, directed = T)		
		
		print(completeHomophilyV3(currentGraph, currentHighschoolTable, sexIndex))
		
	}
	
	
}


# ------------------------------------------------------------------------------
# Get the homophily values for the variables
# ------------------------------------------------------------------------------
{

	variablesOfInterest = c(sexIndex, BMICatIndex, healthIndex, smokeIndex, snuffIndex, alcoholIndex, sportsIndex, highSchoolIndex, solariumIndex)

	for(i in 1:length(variablesOfInterest)){
	
		tempTable = completeTable
		tempTable = transformNA(tempTable, variablesOfInterest[i])
		
		# I hate R, this would be trivial with pointers, AGAIN. Please die already.
		tempNodesDF   = tempTable
    	tempNodesDF[] = lapply(tempNodesDF[], as.character)
		tempGraph     = graph_from_data_frame(overallEdgesDF,  vertices = tempNodesDF, directed = T)
		
		#V(tempGraph)$name <-vertex_attr(tempGraph,"GeneralHealth")
		

    	# From the previous table, get only the Sex From, Sex To, and Value
    	#ee<-get.data.frame(tempGraph)

    
    	#ee$from		
		
		print(completeHomophilyV3(tempGraph, tempTable, variablesOfInterest[i]))
		print("   ")
	}
	
}

# ------------------------------------------------------------------------------
# Check popularity again for variables
# ------------------------------------------------------------------------------
{

	# Get the non solarium, and keep those with valid 25OHD only
	nonSolariumOnlyTable = completeTable[completeTable[,solariumIndex] == "No",]
	
	menNonSolarium   = keepCategory(nonSolariumOnlyTable, sexIndex, "Man")
	womenNonSolarium = keepCategory(nonSolariumOnlyTable, sexIndex, "Woman")
	
	mean(nonSolariumOnlyTable$OverallPopularity, na.rm = TRUE)
	mean(menNonSolarium$OverallPopularity)
	mean(womenNonSolarium$OverallPopularity, na.rm = TRUE)
	
	myCurrentBoxplotResults = doBoxPlotV2(nonSolariumOnlyTable, overallPopularityIndex, VITAMIND_FOLDER_IMAGES_POPULARITY,
                                          groupIndex = sexIndex,
		                                  colorsVector = COLOR_VECTOR_SEX,
                                          showPValues = TRUE)	
		
}

# ------------------------------------------------------------------------------
# Table 0 discussion and break down of the population
# ------------------------------------------------------------------------------
{
	# Get the non solarium, and keep those with valid 25OHD only
	nonSolariumOnlyTable = completeTable[completeTable[,solariumIndex] == "No",]
	nonSolariumOnlyTable = nonSolariumOnlyTable[!is.na(nonSolariumOnlyTable$X25.OH.D_.nmol.L.),]
	
	# Sex
	summarizeCategorical(nonSolariumOnlyTable, sexIndex)
	tempTable  = nonSolariumOnlyTable
    xiResultsA = categoricalXiV2(tempTable, highSchoolIndex , sexIndex)
	
    writeTableLATEX(xiResultsA[[7]], VITAMIND_FOLDER_TABLES_SEX,
                    tableCaption = "Xi2 table with respect sex and highschool for non-solarium users",
                    overrideTableName = "xi2sexhs",
                    widthProportion = 0.7, heightProportion = 0.1)      
    
	
	# BMI values
	summarizeBicategoricalV3(nonSolariumOnlyTable, sexIndex, BMICatIndex)
	menNonSolarium   = keepCategory(nonSolariumOnlyTable, sexIndex, "Man")
	womenNonSolarium = keepCategory(nonSolariumOnlyTable, sexIndex, "Woman")
	
	mean(menNonSolarium$BMI,   na.rm = TRUE)
	sd(menNonSolarium$BMI,     na.rm = TRUE)
	mean(womenNonSolarium$BMI, na.rm = TRUE)
	sd(womenNonSolarium$BMI,   na.rm = TRUE)

	# Smoking
	summarizeCategorical(completeTable, smokeIndex)
	summarizeCategorical(nonSolariumOnlyTable, smokeIndex)
	summarizeBicategoricalV3(completeTable, sexIndex, smokeIndex)
	summarizeBicategoricalV3(nonSolariumOnlyTable, sexIndex, smokeIndex)
	
	# Xi2 analysis for smoke and solarium
	tempTable  = deleteCategory(nonSolariumOnlyTable, smokeIndex, "Unknown")
    xiResultsA = categoricalXiV2(tempTable, smokeIndex ,sexIndex)
    xiResultsB = categoricalXiV2(tempTable, highSchoolIndex, smokeIndex)
    
    writeTableLATEX(xiResultsB[[7]], VITAMIND_FOLDER_TABLES_SEX,
                    tableCaption = "Xi2 table with respect smoke and highschool for non-solarium users",
                    overrideTableName = "xi2smokehs",
                    widthProportion = 0.7, heightProportion = 0.1)    
	
    # Xi2 analysis for snuff and solarium
	tempTable  = deleteCategory(nonSolariumOnlyTable, snuffIndex, "Unknown")
    xiResultsA = categoricalXiV2(tempTable, snuffIndex , sexIndex)
    xiResultsB = categoricalXiV2(tempTable, highSchoolIndex, snuffIndex )    
    
    writeTableLATEX(xiResultsB[[7]], VITAMIND_FOLDER_TABLES_SEX,
                    tableCaption = "Xi2 table with respect snuff and highschool for non-solarium users",
                    overrideTableName = "xi2snuffhs",
                    widthProportion = 0.7, heightProportion = 0.1)        
    
    # Xi2 analysis for BMI and solarium
	tempTable  = deleteCategory(nonSolariumOnlyTable, BMICatIndex, "Unknown")
    xiResultsA = categoricalXiV2(tempTable, BMICatIndex , sexIndex)
    xiResultsB = categoricalXiV2(tempTable, BMICatIndex , highSchoolIndex)  
    
    writeTableLATEX(xiResultsB[[7]], VITAMIND_FOLDER_TABLES_SEX,
                    tableCaption = "Xi2 table with respect BMI and highschool for non-solarium users",
                    overrideTableName = "xi2bmihs",
                    widthProportion = 0.7, heightProportion = 0.1)      
    
    # Xi2 analysis for alcohol and solarium
	tempTable  = deleteCategory(nonSolariumOnlyTable, alcoholIndex, "Unknown")
    xiResultsA = categoricalXiV2(tempTable, alcoholIndex , sexIndex)
    xiResultsB = categoricalXiV2(tempTable, highSchoolIndex, alcoholIndex)    
    
    
    writeTableLATEX(xiResultsB[[7]], VITAMIND_FOLDER_TABLES_SEX,
                    tableCaption = "Xi2 table with respect alcohol and highschool for non-solarium users",
                    overrideTableName = "xi2alcoholhs",
                    widthProportion = 0.7, heightProportion = 0.1)      
    
    # Xi2 analysis for sport and solarium
	tempTable  = deleteCategory(nonSolariumOnlyTable, sportsIndex, "Unknown")
    xiResultsA = categoricalXiV2(tempTable, sportsIndex , sexIndex)
    xiResultsB = categoricalXiV2(tempTable, highSchoolIndex, sportsIndex)        
    
    
	writeTableLATEX(xiResultsB[[7]], VITAMIND_FOLDER_TABLES_SEX,
                    tableCaption = "Xi2 table with respect sport and highschool for non-solarium users",
                    overrideTableName = "xi2sporths",
                    widthProportion = 0.7, heightProportion = 0.1)     
    
    
}

# ------------------------------------------------------------------------------
# Race and skin type
# ------------------------------------------------------------------------------
{

    # I hate R, the weird things you need to do to create a clean DF based on other columns
    ethnicDF            = DF(nrow(completeTable), 2)
    ethnicDF$Original   = sociologyTable$Ethnicity
    ethnicDF$VitD       = bloodTable$X25.OH.D_.nmol.L.
    ethnicDF$Solariun   = hygieneTable$SolariumLast4Weeks
    ethnicDF            = ethnicDF[,3:5]
    ethnicDF$Skin       = "Fair"
    ethnicDF$Highschool = completeTable$HighSchool
    
    # Change a lonely NA to Didn't Answer
    ethnicDF[is.na(ethnicDF$Original),1] = "Didn't Answer"

    myPlot = doBarPlotV2(ethnicDF, 1, VITAMIND_FOLDER_IMAGES_ETHNICITY,
                         rotation     = TRUE,        
                         sort         = "ascending",
                         plotTitle    = "Ethnicity background, absolute frequency",
    	                 plotXLabel = "", plotYLabel = "",
    	                 overrideImageWidth = 10,
    	                 overrideImageHeigh = 10)
    
    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER_IMAGES_ETHNICITY, 
                     captionText   = "Absolute frequency for each ethnicity sorted by frequency",
                     overrideLabel = "fig:EthnicityCounts",
                     pageHeight = 0.5, overrideFloat = TRUE)  
    
    
    summaryEthnit     = summarizeCategorical(ethnicDF, 1)
    summaryEthnit[,3] = round(summaryEthnit[,3],3)
    summaryEthnit[,4] = round(summaryEthnit[,4],3)
    
    writeTableLATEX(summaryEthnit, VITAMIND_FOLDER_TABLES_ETHNICITY,
                    tableCaption = "Absolute frequency for each ethnicity sorted by frequency",
                    overrideTableName = "allEthnicities",
                    widthProportion = 0.7, heightProportion = 0.5)      
    
    # Delete people who didn't awnser from the ethnic
    # Find skin color
    ethnicDF = deleteCategory(ethnicDF, 1, "Didn't Answer")
    
    for(i in 1:nrow(ethnicDF)){
        
        currentEthnicity = ethnicDF[i,1]
        
        if(currentEthnicity == "Norwegian-Somalian")  ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Afghan")              ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "African")             ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Thai")                ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Erithrean")           ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Turquish")  ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Thai")      ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Thamil")    ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Philipine") ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-African")   ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Gambian")   ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Ghanaian")  ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Brasilian") ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Colombian") ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Other")     ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Palestinian")         ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Tamil")               ethnicDF[i,4] = "Dark"

    }
    
    # Check spread in highschools
    xiResults      = categoricalXiV2(ethnicDF, 5 ,4)
    summaryEthnit  = summarizeCategorical(ethnicDF, 4)

    writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER_TABLES_ETHNICITY,
                    tableCaption = "Xi-square test skin group and high-school",
                    overrideTableName = "XiTestSkinHS",
                    widthProportion = 0.5, heightProportion = 0.15)  
    
    
    
        # Everythin that is not "No" from solarium
    # NA values for vitD
    
    
    
    
    ethnicDF = keepCategory(ethnicDF,   3, "No")
    ethnicDF = deleteNA(ethnicDF, 2)

    
    myPlot = doBoxPlotV2 (ethnicDF, 2, VITAMIND_FOLDER,
                          groupIndex = 4,
                          colorsVector = rev(COLOR_VECTOR_SKIN),
                          showPValues = TRUE,
                          pValuesFormat = "number2",
                          cutOffLine = cutOffVitD,
                          ymax = 155,
                          plotTitle = "25OHD levels with respect skin group",
                          plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
                          plotYLabel = "25OHD (nmol/L)",
                          overrideImageWidth = 7)    
    
    writeBoxPlotCompositeLATEX ( myPlot[[2]], myPlot[[4]], myPlot[[5]], VITAMIND_FOLDER,
                                 captionText = "Relationship between vitamin D and skin group",
                                 minipageTop = 0.4,  minipageBottom = 0.5 ,         
                                 pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                                 topTableWidth    = 0.4, topTableHeight    = 0.05,
                                 bottomTableWidth = 0.4, bottomTableHeight = 0.05
                                )    
    
    
    ethnicDFAA = keepCategory(ethnicDF,   4, "Fair")
    sum(ethnicDFAA$VitD < 50)
    
}

# ------------------------------------------------------------------------------
# By diet
# ------------------------------------------------------------------------------
{
    
    # As we stablish before, solarium is a very big influence. So we are going
    # to check people who don't go to the solarium only
    nonSolariumOnlyTable = completeTable[completeTable[,solariumIndex] == "No",]
    
	# Create a table where we are going to summarize the t-test/oANOVA results
	summaryDF           = DF(6,4)
	colnames(summaryDF) = c("Food group", "25OHD", "Calcium", "PTH")
	summaryDF[1,1]      = "Lean Fish"
	summaryDF[2,1]      = "Fat Fish"
	summaryDF[3,1]      = "Diary"
	summaryDF[4,1]      = "Cheese"
	summaryDF[5,1]      = "Fish oil"
	summaryDF[6,1]      = "Vitamins"
	
	summaryDF$`25OHD`   = 0
	summaryDF$Calcium   = 0
	summaryDF$PTH       = 0
	
	
    # Compare by Lean Fish
	{
		# ---- Vit D
    	noUnknownDiet = subset(nonSolariumOnlyTable, nonSolariumOnlyTable[,leanFishIndex] != "Didn't Answered")
    	myPlotA = doBoxPlotV2 (noUnknownDiet, vitamimDIndex, VITAMIND_FOLDER_IMAGES_DIET,
        	                   groupIndex = leanFishIndex,
            	               colorsVector = COLOR_VECTOR_PURPLE_5,
                  	           showPValues = TRUE,
                      	       significantPValue = 0.05,
                               cutOffLine = cutOffVitD,
                          	   ymax = 155,
	                           plotTitle = "25OHD levels with respect Lean Fish consumption",
    	                       angleXLabels = 45,
        	                   plotSubtitle = "Only analyzing people not going to the solarium in the last 4 week",
            	               plotYLabel = "25OHD (nmol/L)",
                	           overrideImageWidth = 7)
    
    	writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
        	             captionText   = "Relationship between vitamin D and lean fish consumption",
            	         overrideLabel = "fig:RelationshipVITDLeanFish",
                	     pageHeight = 0.3, overrideFloat = TRUE)		
    	
    	summaryDF[1,2] = myPlot[[3]][[2]]
    	
    	writeBoxPlotCompositeLATEX ( myPlotA[[2]], myPlotA[[4]], myPlotA[[5]], VITAMIND_FOLDER_IMAGES_DIET,
                                     captionText = "Relationship between 25OHD and fat fish consumption",
                                     minipageTop = 0.4,  minipageBottom = 0.5 ,         
                                     pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                                     topTableWidth    = 0.4, topTableHeight    = 0.05,
                                     bottomTableWidth = 0.4, bottomTableHeight = 0.03 )    	
    	
	}
  
    # Compare by Fat Fish
    {
    	
    	# ---- Vit D
	    noUnknownDiet = subset(nonSolariumOnlyTable, nonSolariumOnlyTable[,fatFishIndex] != "Didn't Answered")
	    myPlotA = doBoxPlotV2 (noUnknownDiet, vitamimDIndex, VITAMIND_FOLDER_IMAGES_DIET,
	                          groupIndex = fatFishIndex,
	                          colorsVector = COLOR_VECTOR_PURPLE_5,
	                          showPValues = FALSE,
	                          cutOffLine = cutOffVitD,
	                          ymax = 155,
	                          plotTitle = "25OHD levels with respect Fat Fish consumption",
	    	                  plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
	                          angleXLabels = 45,
	                          plotYLabel = "25OHD (nmol/L)",
	                          overrideImageWidth = 7)    
	    
	    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER_IMAGES_DIET, 
	                     captionText   = "Relationship between 25OHD and fat fish consumption",
	                     overrideLabel = "fig:RelationshipVITDFatFish",
	                     pageHeight = 0.3, overrideFloat = TRUE)    
    
	    summaryDF[2,2] = myPlotA[[3]][[2]]
	    
   
    	writeBoxPlotCompositeLATEX ( myPlotA[[2]], myPlotA[[4]], myPlotA[[5]], VITAMIND_FOLDER_IMAGES_DIET,
                                     captionText = "Relationship between 25OHD and fat fish consumption",
                                     minipageTop = 0.4,  minipageBottom = 0.5 ,         
                                     pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                                     topTableWidth    = 0.4, topTableHeight    = 0.05,
                                     bottomTableWidth = 0.4, bottomTableHeight = 0.03 )   	    
	    
    	# ---- Calcium
	    myPlotB = doBoxPlotV2 (noUnknownDiet, calciumIndex, VITAMIND_FOLDER_IMAGES_DIET,
	                          groupIndex = fatFishIndex,
	                          colorsVector = COLOR_VECTOR_PURPLE_5,
	                          showPValues = FALSE,
	                          cutOffLine = cutOffCalcium,
	                          plotTitle = "Calcium levels with respect fat fish consumption",
	    	                  plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
	                          angleXLabels = 45,
	                          plotYLabel = "Calcium (mmol/L)",
	                          overrideImageWidth = 7)      
    
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER_IMAGES_DIET, 
                         captionText   = "Relationship between Calcium and fat fish consumption",
                         overrideLabel = "fig:RelationshipCaFatFish",
                         pageHeight = 0.3, overrideFloat = TRUE)    
    
    
        summaryDF[2,3] = myPlotB[[3]][[2]]
        
        # ---- PTH
        myPlotC = doBoxPlotV2 (noUnknownDiet, pthIndex, VITAMIND_FOLDER_IMAGES_DIET,
                              groupIndex = fatFishIndex,
                              colorsVector = COLOR_VECTOR_PURPLE_5,
                              showPValues = FALSE,
                              cutOffLine = cutOffPTH,
                              plotTitle = "PTH levels with respect fat fish consumption",
        	                  plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
                              angleXLabels = 45,
                              plotYLabel = "PTH (mmol/L)",
                              overrideImageWidth = 7)      
        
         writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER_IMAGES_DIET, 
                          captionText   = "Relationship between PTH and Dairy comsumption for people not going to the solarium",
                          overrideLabel = "fig:RelationshipPTHFatFish",
                          pageHeight = 0.3, overrideFloat = TRUE)   
       
         summaryDF[2,4] = myPlotC[[3]][[2]]
        
       
    
    	 writeBoxPlotCompositeLATEX ( myPlotC[[2]], myPlotC[[4]], myPlotC[[5]], VITAMIND_FOLDER_IMAGES_DIET,
                                      captionText = "Relationship between PTH and Dairy comsumption for people not going to the solarium",
                                      minipageTop = 0.4,  minipageBottom = 0.5 ,         
                                      pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                                      topTableWidth    = 0.4, topTableHeight    = 0.05,
                                      bottomTableWidth = 0.4, bottomTableHeight = 0.03 )      	
    }

    
    # Compare by diary
	{
	
		# ---- VitD
    	noUnknownDiet = subset(nonSolariumOnlyTable, nonSolariumOnlyTable[,dairyIndex] != "Didn't Answered")
    	myPlotA = doBoxPlotV2 (noUnknownDiet, vitamimDIndex, VITAMIND_FOLDER_IMAGES_DIET,
        	      	           groupIndex = dairyIndex,
                    		   colorsVector = COLOR_VECTOR_PURPLE_5,
                			   showPValues = TRUE,
    		                   significantPValue = 0.05,
                			   cutOffLine = cutOffVitD,
                               ymax = 155,
                			   plotTitle    = "25OHD levels with respect Dairy consumption",
    		                   plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
                               angleXLabels = 45,
                			   plotYLabel   = "25OHD (nmol/L)",
                			   overrideImageWidth = 7)      
    
    	writeBoxPlotCompositeLATEX ( myPlotA[[2]], myPlotA[[4]], myPlotA[[5]], VITAMIND_FOLDER_IMAGES_DIET,
                                     captionText = "Relationship between 25OHD and diary consumption",
                                     minipageTop = 0.4,  minipageBottom = 0.5 ,         
                                     pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                                     topTableWidth    = 0.4, topTableHeight    = 0.05,
                                     bottomTableWidth = 0.4, bottomTableHeight = 0.03 ) 
		    	
    	summaryDF[3,2] = myPlotA[[3]][[2]]

    	
	    # ---- Calcium
	    noUnknownDiet = subset(nonSolariumOnlyTable, nonSolariumOnlyTable[,dairyIndex] != "Didn't Answered")
	    myPlot = doBoxPlotV2 (noUnknownDiet, calciumIndex, VITAMIND_FOLDER_IMAGES_DIET,
	                 groupIndex = dairyIndex,
	                 colorsVector = COLOR_VECTOR_PURPLE_5,
	                 showPValues = FALSE,
	                 cutOffLine = cutOffCalcium,
	                 plotTitle = "Calcium levels with respect Dairy consumption",
	                 angleXLabels = 45,
	                 plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
	                 plotYLabel = "Calcium (mmol/L)",
	                 overrideImageWidth = 7)      
	    
	    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER_IMAGES_DIET, 
	                     captionText   = "Relationship between Calcium and Dairy consumption",
	                     overrideLabel = "fig:RelationshipCaDairy",
	                     pageHeight = 0.3, overrideFloat = TRUE)     
	    
	    # ---- PTH
	    noUnknownDiet = subset(nonSolariumOnlyTable, nonSolariumOnlyTable[,dairyIndex] != "Didn't Answered")
	    myPlot = doBoxPlotV2 (noUnknownDiet, pthIndex, VITAMIND_FOLDER_IMAGES_DIET,
	                 groupIndex = dairyIndex,
	                 colorsVector = COLOR_VECTOR_PURPLE_5,
	                 showPValues = FALSE,
	                 cutOffLine = cutOffPTH,
	                 plotTitle = "PTH levels with respect Dairy consumption",
	                 angleXLabels = 45,
	                 plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
	                 plotYLabel = "PTH (mmol/L)",
	                 overrideImageWidth = 7)      
	        
	    
	    writeBoxPlotCompositeLATEX ( myPlot[[2]], myPlot[[4]], myPlot[[5]], VITAMIND_FOLDER_IMAGES_DIET,
	                                 captionText = "Relationship between PTH and Dairy comsumption for people not going to the solarium",
	                                 minipageTop = 0.4,  minipageBottom = 0.5 ,         
	                                 pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
	                                 topTableWidth    = 0.4, topTableHeight    = 0.05,
	                                 bottomTableWidth = 0.4, bottomTableHeight = 0.03
	                               )	    
    	
	}

    # Compare by cheese
	{
		
	    noUnknownDiet = subset(nonSolariumOnlyTable, nonSolariumOnlyTable[,cheeseIndex] != "Didn't Answered")
	    myPlotA = doBoxPlotV2 (noUnknownDiet, vitamimDIndex, VITAMIND_FOLDER_IMAGES_DIET,
	                           groupIndex = cheeseIndex,
	                           colorsVector = COLOR_VECTOR_PURPLE_5,
	                           showPValues = TRUE,
	    	                   significantPValue = 0.05,
	                           cutOffLine = cutOffVitD,
	                           ymax = 155,
	                           plotTitle = "25OHD levels with respect Cheese consumption",
	    	                   plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
	                           angleXLabels = 45,
	                           plotYLabel = "25OHD (nmol/L)",
	                           overrideImageWidth = 7)    
	    
    
    	writeBoxPlotCompositeLATEX ( myPlotA[[2]], myPlotA[[4]], myPlotA[[5]], VITAMIND_FOLDER_IMAGES_DIET,
                                     captionText = "Relationship between 25OHD and cheese consumption",
                                     minipageTop = 0.4,  minipageBottom = 0.5 ,         
                                     pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                                     topTableWidth    = 0.4, topTableHeight    = 0.05,
                                     bottomTableWidth = 0.4, bottomTableHeight = 0.03 ) 
		    	
    	summaryDF[4,2] = myPlotA[[3]][[2]]
		
	}

    # Compare by fish oil
	{

		noUnknownDiet = subset(nonSolariumOnlyTable, nonSolariumOnlyTable[,fishOilIndex] != "Didn't Answered")
    	myPlotA = doBoxPlotV2 (noUnknownDiet, vitamimDIndex, VITAMIND_FOLDER_IMAGES_DIET,
                 			   groupIndex = fishOilIndex,
                			   colorsVector = COLOR_VECTOR_PURPLE_3,
	                           showPValues = TRUE,
	    	                   significantPValue = 0.05,
                			   cutOffLine = cutOffVitD,
                               ymax = 155,
                               plotTitle = "25OHD levels with respect Fish Oil consumption",
                               plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
                               plotYLabel = "25OHD (nmol/L)",
                               overrideImageWidth = 7)    

        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER_IMAGES_DIET, 
                         captionText   = "Relationship between vitamin D and fish oil consumption",
                         overrideLabel = "fig:RelationshipVitDFishOil",
                         pageHeight = 0.3, overrideFloat = TRUE)     	
    	
        writeBoxPlotCompositeLATEX ( myPlotA[[2]], myPlotA[[4]], myPlotA[[5]], VITAMIND_FOLDER_IMAGES_DIET,
                                     captionText = "Relationship between 25OHD and fish oil consumption",
                                     minipageTop = 0.4,  minipageBottom = 0.5 ,         
                                     pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                                     topTableWidth    = 0.4, topTableHeight    = 0.05,
                                     bottomTableWidth = 0.4, bottomTableHeight = 0.03 ) 

        summaryDF[5,2] = myPlotA[[3]][[2]]
		
	}

    # Compare by vitamins
	{
		
	    noUnknownDiet = subset(nonSolariumOnlyTable, nonSolariumOnlyTable[,vitaminsIndex] != "Didn't Answered")
	    myPlotA = doBoxPlotV2 (noUnknownDiet, vitamimDIndex, VITAMIND_FOLDER_IMAGES_DIET,
	                           groupIndex = vitaminsIndex,
	                           colorsVector = COLOR_VECTOR_PURPLE_3,
	                           showPValues = TRUE,
	    	                   significantPValue = 0.05,
	                           cutOffLine = cutOffVitD,
	                           ymax = 155,
	                           plotTitle = "25OHD levels with respect Vitamins consumption",
	                           plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
	                           plotYLabel = "25OHD (nmol/L)",
	                           overrideImageWidth = 7)    
	    
	    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER_IMAGES_DIET, 
	                     captionText   = "Relationship between vitamin D and vitamins consumption",
	                     overrideLabel = "fig:RelationshipVitDVitamins",
	                     pageHeight = 0.3, overrideFloat = TRUE)  
	    
        writeBoxPlotCompositeLATEX ( myPlotA[[2]], myPlotA[[4]], myPlotA[[5]], VITAMIND_FOLDER_IMAGES_DIET,
                                     captionText = "Relationship between 25OHD and vitamins consumption",
                                     minipageTop = 0.4,  minipageBottom = 0.5 ,         
                                     pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                                     topTableWidth    = 0.4, topTableHeight    = 0.05,
                                     bottomTableWidth = 0.4, bottomTableHeight = 0.03 ) 
        
		summaryDF[6,2] = myPlotA[[3]][[2]]
		
	}
  
    # Lets try to put all of that into a single multidimensional logistic regression model (nothing work)
    {
    
        # Make a subset for easy reading
        myDietDF = nonSolariumOnlyTable[,c(vitaminsIndex, fishOilIndex, leanFishIndex, fatFishIndex, dairyIndex, cheeseIndex, vitamimDIndex )]
        
        # Delete everyone that didn't answered one or more of the columns
        {
            myDietDF = deleteCategory(myDietDF, 1, "Didn't Answered")
            myDietDF = deleteCategory(myDietDF, 2, "Didn't Answered")
            myDietDF = deleteCategory(myDietDF, 3, "Didn't Answered")
            myDietDF = deleteCategory(myDietDF, 4, "Didn't Answered")
            myDietDF = deleteCategory(myDietDF, 5, "Didn't Answered")
            myDietDF = deleteCategory(myDietDF, 6, "Didn't Answered")            
        }
        
        # Transform categorical into numerical
        {
            myDietDF[,1] = as.numeric(myDietDF[,1])
            myDietDF[,2] = as.numeric(myDietDF[,2])
            myDietDF[,3] = as.numeric(myDietDF[,3])
            myDietDF[,4] = as.numeric(myDietDF[,4])
            myDietDF[,5] = as.numeric(myDietDF[,5])
            myDietDF[,6] = as.numeric(myDietDF[,6])            
        }

        # Do the megachart
        {
            library(PerformanceAnalytics)

            chartFilename = paste0(VITAMIND_FOLDER, "CorrelationChart.png")
            png(chartFilename)
            chart.Correlation(myDietDF,
                              method="spearman",
                              histogram=TRUE,
                              pch=16)
            dev.off()
            
            writeImageLATEX2(chartFilename, VITAMIND_FOLDER, 
                             captionText   = "Relationship between all dietary variables and 25OHD level for non-solarium users",
                             overrideLabel = "fig:SpearmanChartDietary",
                             pageHeight = 0.3, overrideFloat = TRUE)    
        }
 
        
        # Transform the 25OHD levels into 0/1 and do logistic
        {
            # Delete those that have an NA value
            myDietDF = deleteNA(myDietDF, 7)
            myDietDF$Cat25OHD = 0
            myDietDF[myDietDF[,7] > 50, 8] = 1
            
            # Delete the numerical column
            myDietDF[,7] = NULL
            
        }
        
        myModel = glm(Cat25OHD ~ VitaminsFrequency + FishOilFrequency + LeanFishFrequency + FatFishFrequency + DairyFrequency + CheeseFrequency,
                         data=myDietDF,
                         family = binomial(link="logit"))
        
        summary(myModel)
     
        # Its appear that only vitamins, fish oil and lean fish are relevant, so update the model
        
        myModelUpdate = glm(Cat25OHD ~ VitaminsFrequency + FishOilFrequency + LeanFishFrequency,
                            data=myDietDF,
                            family = binomial(link="logit"))
        
        summary(myModelUpdate)        
        
        library(car)

        Anova(myModelUpdate, type="II", test="Wald")
        
        library(rcompanion)
           
        nagelkerke(myModelUpdate)
        
    }


	# Stratify by highschool for xi2 test only
    {
            
		# Prepare a new folder
        currentFolder = paste0(VITAMIND_FOLDER_IMAGES_DIET, "DietByHighschool/")

        # For each dietary data source 
        for(i in 1:length(vitaminDIndexes)){

                # Get the index and name of the diet concept
                currentDietIndex = vitaminDIndexes[i]
                currentDietName  = colnames(completeTable)[currentDietIndex]

                # Filter people who didn't awnserd the question
                currentDietDataDF = deleteCategory(nonSolariumOnlyTable, currentDietIndex, "Didn't Answered")

                # Count how many categories are left, delete those that have only 1 if any
                currentSummary = summarizeCategorical(currentDietDataDF, currentDietIndex)
                for(k in 1:nrow(currentSummary)){
                    
                    if(currentSummary[k,2] == 1){
                    
                        badCategoryName   = as.character(currentSummary[k,1])           # FUCKING FACTORS! I Hate you, ALL THE FUCKING TIME THE SAME SHIT!
                        currentDietDataDF = deleteCategory(currentDietDataDF, currentDietIndex, badCategoryName)
                        
                    }
                    
                }

                xiResults = categoricalXiV2(currentDietDataDF, highSchoolIndex ,currentDietIndex)
                
                currentOverrideCaption = paste0("Xi-square test between ", currentDietName, " and high-school")
                currentOverrideName    = paste0("XiTestHS", currentDietName)
                
                myLatexTable = writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER_TABLES_DIET,
                                               tableCaption      = currentOverrideCaption,
                                               overrideTableName = currentOverrideName,
                                               widthProportion   = 0.5, heightProportion = 0.15)  
                
            }
            
    }	
	   
	# Stratify by highschool all diets
	{
		
		
		myHighschools       = getCategories(completeTable, highSchoolIndex)
		variablesOfInterest = vitDdietIndexes
	
		# Create the dataframe where we put the results
		resultsDF             = DF(length(variablesOfInterest), length(myHighschools) + 1)
		colnames(resultsDF)   = c("Variable", myHighschools)
		resultsDF[,1]         = colnames(completeTable)[variablesOfInterest]
	
		# Get the non solarium, and keep those with valid 25OHD only
		nonSolariumOnlyTable = completeTable[completeTable[,solariumIndex] == "No",]
		nonSolariumOnlyTable = nonSolariumOnlyTable[!is.na(nonSolariumOnlyTable$X25.OH.D_.nmol.L.),]
	
		# For each highschol
		for(i in 1:length(myHighschools)){
		
			currentHighschool = myHighschools[i]
	
			print(currentHighschool)
			print("-----------------------")
		
			tempTableA = keepCategory(nonSolariumOnlyTable, highSchoolIndex, currentHighschool)
		
			# Gor each variable of interest
			for(j in 1:length(variablesOfInterest)){
		
				currentVariable = variablesOfInterest[j]
			
			
				print(colnames(completeTable)[currentVariable])
				print(".........")	
			
				tempTableB  = deleteCategory(tempTableA, currentVariable, "Unknown")
				tempTableB  = deleteNA(tempTableB, currentVariable)
				tempTableB  = deleteNA(tempTableB, vitamimDIndex)
			
				overrideTableName = paste0(currentHighschool,"_",currentVariable,"_bias")
			
				myCurrentBoxplotResults = doBoxPlotV2(tempTableB, vitamimDIndex, VITAMIND_FOLDER_IMAGES_DIET_HS,
            			                              groupIndex = currentVariable,
				                                	  overrideTableName = overrideTableName,
                    		                          showPValues = TRUE)	

				resultsDF[j,i+1] = myCurrentBoxplotResults[[3]][[2]]

			}	
		
		}		
		
		# This needs a post-hoc correction too
		
	}
    
}


# ------------------------------------------------------------------------------
# PCR with blood non FA variables
# ------------------------------------------------------------------------------
{
	
	# Luckily, the PTH is the last one, because is impossible to take a number
	# from a list in R without doing a million things too >:(
	bloodVariablesIndexes = c(firstBloodIndex : (firstBloodIndex+26))
	
	myModel = doPCRanalysis(completeTable, pthIndex, bloodVariablesIndexes, ncomp = 22)
	
	#summary(myModel)
	validationplot(myModel[[1]] ) # Seems 4 is ok

	
	myModel = doPCRanalysis(menOnlyTable,   pthIndex, bloodVariablesIndexes, ncomp = 20)
	validationplot(myModel[[1]], val.type="R2")
	myModel = doPCRanalysis(womenOnlyTable, pthIndex, bloodVariablesIndexes, ncomp = 26)
	validationplot(myModel[[1]], val.type="R2")
	
		
}


# ------------------------------------------------------------------------------
# Let check hormonal contraceptives by school and levels
# ------------------------------------------------------------------------------
{

	# Hormonal types
	myHormonalTypes = c("Non-hormonal", "Progestin", "Low Estradiol", "High Estradiol")
	
	# Declare the dataframes
	hormonalSummary = DF(8,5)
	colnames(hormonalSummary) = c("Highschool", myHormonalTypes)
	hormonalVitDAvg = hormonalSummary
	hormonalVitDSig = hormonalSummary		
	
    nonNASolariumOnlyTable    = keepCategory(completeTable, solariumIndex, "No")
    nonSolariumsMen           = keepCategory(nonNASolariumOnlyTable, sexIndex, "Man")
    nonSolariumsWomen         = keepCategory(nonNASolariumOnlyTable, sexIndex, "Woman")
    nonSolariumsWomenHormonal = deleteCategory(nonSolariumsWomen, hormonalTypeIndex, "Unknown")
    
	
    nonUnknownWomenOnlyTable = deleteCategory(womenOnlyTable, hormonalTypeIndex, "Unknown")
    nonUnknownWomenOnlyTable = deleteCategory(nonUnknownWomenOnlyTable, solariumIndex,     "Yes")
     
    xiResults = categoricalXiV2(nonUnknownWomenOnlyTable, highSchoolIndex ,hormonalTypeIndex)
    
    summarizeCategorical(nonNASolariumOnlyTable, hormonalTypeIndex, sorted = "None")
    
    myHighschools = levels(completeTable[,highSchoolIndex])
    
    # For each highschool
    for(i in 1:length(myHighschools)){
    	
    	# Get the HS and women in that school
    	currentHighschool = myHighschools[i]
    	womenInThisSchool = keepCategory(nonSolariumsWomenHormonal, highSchoolIndex, currentHighschool)
    	
    	
    	womenInThisSchool = keepCategory(nonSolariumsWomenHormonal, highSchoolIndex, currentHighschool)
    	
    	print("--- T ----")
    	print(currentHighschool)
    	print(mean(womenInThisSchool[,testosteroneIndex],na.rm = TRUE))
    	
    	
    	# Fill the row of the table
    	#print(    	summarizeCategorical(womenInThisSchool, hormonalTypeIndex, sorted = "None") )
    	
    	hormonalSummary[i,1] = currentHighschool
    	hormonalSummary[i,2] = countCategories(womenInThisSchool, hormonalTypeIndex, "Non-hormonal")
    	hormonalSummary[i,3] = countCategories(womenInThisSchool, hormonalTypeIndex, "Progestin")
    	hormonalSummary[i,4] = countCategories(womenInThisSchool, hormonalTypeIndex, "Low Estradiol")
    	hormonalSummary[i,5] = countCategories(womenInThisSchool, hormonalTypeIndex, "High Estradiol")
    	
    	# Do the boxplot with the SHBG and sex
    	currentSubtitle  = paste0(currentHighschool, " non-solarium users")
    	currentTableName = paste0(currentHighschool,"_sexAndSHBG")
    	myPlot = doBoxPlotV2 (nonNASolariumOnlyTable, shbgIndex, VITAMIND_FOLDER_IMAGES_HORMONAL,
        			          groupIndex = sexIndex,
                			  showPValues = TRUE,
                			  significantPValue = 0.1,
                			  cutOffLine = cutOffVitD,
                			  colorsVector = COLOR_VECTOR_SEX,
                			  plotTitle = "SHBG levels with respect sex",
                			  plotSubtitle = currentSubtitle,
                		      plotYLabel = "SHBG (nmol/L)",
    						  overrideTableName = currentTableName,
                			  overrideImageWidth = 7)      	
    	
    	
    	# Do the boxplot with the hormonal information
    	currentSubtitle  = paste0(currentHighschool, " non-solarium women")
    	currentTableName = paste0(currentHighschool,"_nonSolariumHoromnalVitD")
    	myPlot = doBoxPlotV2 (womenInThisSchool, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HORMONAL,
        			          groupIndex = hormonalTypeIndex,
                			  showPValues = TRUE,
                			  significantPValue = 0.1,
                			  pValuesFormat = "number3",
                			  cutOffLine = cutOffVitD,
                			  #colorsVector = COLOR_VECTOR_HEALTH,
                			  ymax = 155,
                			  plotTitle = "25OHD levels with respect contraceptives use",
                			  plotSubtitle = currentSubtitle,
                		      plotYLabel = "25OHD (nmol/L)",
    						  overrideTableName = currentTableName,
                			  overrideImageWidth = 7)  
    	
    	
    	#print("---------------------")
    	#print(currentHighschool)
    	#print(myPlot[[3]][[4]])
    	#print(" ")
    	
    	# Do the boxplot with the SHBG and hormonal type
    	currentSubtitle  = paste0(currentHighschool, " non-solarium women")
    	currentTableName = paste0(currentHighschool,"_nonSolariumHoromnalSHBG")
    	myPlot = doBoxPlotV2 (womenInThisSchool, shbgIndex, VITAMIND_FOLDER_IMAGES_HORMONAL,
        			          groupIndex = hormonalTypeIndex,
                			  showPValues = TRUE,
                			  significantPValue = 0.1,
                			  pValuesFormat = "number3",
                			  cutOffLine = cutOffVitD,
                			  #colorsVector = COLOR_VECTOR_HEALTH,
                			  plotTitle = "SHBG levels with respect contraceptives use",
                			  plotSubtitle = currentSubtitle,
                		      plotYLabel = "SHBG (nmol/L)",
    						  overrideTableName = currentTableName,
                			  overrideImageWidth = 7)      	
    	
    	# print("---------------------")
    	# print(currentHighschool)
    	# print(myPlot[[3]][[4]])
    	# print(" ")
    	
    	
    	# Do the regression with the hormonal information for
    	# - Estrogen
    	currentOverridedPlotName = paste0(currentHighschool,"_nonSolariumEstradiolVitD")
		myPlot = doRegressionPlot(womenInThisSchool, estradiolIndex, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HORMONAL,
		                          horizontalLinesCuts   = cutOffVitD,
		                          plotTitle    = "Relationship between 25OHD and Estradiol",
		                          plotSubtitle = currentSubtitle,
		                          plotXLabel = "Estradiol (nmol/L)",
		                          plotYLabel = "25OHD (nmol/L)",
		                          plotTheme = "regression",
								  ymax = 160,
								  overrideTableName = currentOverridedPlotName,
		                          overrideImageWidth = 12)      	
		
		# - Progesteron
    	currentOverridedPlotName = paste0(currentHighschool,"_nonSolariumProgeteronVitD")
		myPlot = doRegressionPlot(womenInThisSchool, progesteronIndex, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HORMONAL,
		                          horizontalLinesCuts   = cutOffVitD,
		                          plotTitle    = "Relationship between 25OHD and Progesterone",
		                          plotSubtitle = currentSubtitle,
		                          plotXLabel = "Progesterone (nmol/L)",
		                          plotYLabel = "25OHD (nmol/L)",
		                          plotTheme = "regression",
			                      ymax = 160,
								  overrideTableName = currentOverridedPlotName,
		                          overrideImageWidth = 12)      	
		            
		
		# - SHBG
    	currentOverridedPlotName = paste0(currentHighschool,"_nonSolariumSHBGVitD")
		myPlot = doRegressionPlot(womenInThisSchool, shbgIndex, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HORMONAL,
		                          horizontalLinesCuts   = cutOffVitD,
		                          plotTitle    = "Relationship between 25OHD and SHBG",
		                          plotSubtitle = currentSubtitle,
		                          plotXLabel = "SHBG (nmol/L)",
		                          plotYLabel = "25OHD (nmol/L)",
		                          plotTheme = "regression",
			                      ymax = 160,
								  overrideTableName = currentOverridedPlotName,
		                          overrideImageWidth = 12)    
    	
    	
    	
    	
    	# For each hormonal, fill the averages and sigma information
    	for(j in 1:length(myHormonalTypes)){
    	
    		currentHormonal = myHormonalTypes[j]
    		currentSubtable = keepCategory(womenInThisSchool, hormonalTypeIndex, currentHormonal)
	
	    	if(nrow(currentSubtable)>0){
	    	
	    		hormonalVitDAvg[i,(j+1)] = mean(currentSubtable[,vitamimDIndex], na.rm = TRUE)
	    		hormonalVitDSig[i,(j+1)] = sd(currentSubtable[,vitamimDIndex],   na.rm = TRUE)
	    			
	    	}
    		
    	}
    	
    }
    hormonalVitDAvg$Highschool = hormonalSummary$Highschool
    hormonalVitDSig$Highschool = hormonalSummary$Highschool
    
    
    # For H8 only, HE and NH only
    {
    	
    	h8SpecialTable = keepCategory(womenOnlyTable, highSchoolIndex, "H8")
    	h8SpecialTable = deleteCategory(h8SpecialTable, hormonalTypeIndex, "Unknown")
    	h8SpecialTable = deleteCategory(h8SpecialTable, hormonalTypeIndex, "Progestin")
    	h8SpecialTable = deleteCategory(h8SpecialTable, hormonalTypeIndex, "Low Estradiol")
    	h8SpecialTable = deleteCategory(h8SpecialTable, solariumIndex,     "Yes")
    	
    	myPlot = doBoxPlotV2 (h8SpecialTable, vitamimDIndex, VITAMIND_FOLDER_IMAGES_HORMONAL,
        			          groupIndex = hormonalTypeIndex,
                			  showPValues = TRUE,
                			  significantPValue = 0.1,
                			  cutOffLine = cutOffVitD,
                			  plotTitle = "H8 differences in contraceptives",
                		      plotYLabel = "25OHD (nmol/L)",
    						  overrideTableName = "specialH8case",
                			  overrideImageWidth = 7)       	
    	
    }
    
}




# Lets compare the vitamin D for men and women
{
    
	
	
	
    # Lets see if there is a difference between, there should be nothing
    # but actually we got a very significant difference
    myBoxplotResults = doCategoricalBoxPlot (completeTable,
                                             sexIndex,
                                             vitamimDIndex,
                                             VITAMIND_FOLDER,
                                             colorsVector = COLOR_VECTOR_SEX,
                                             showPValues = TRUE,
                                             cutOffLine = cutOffVitD,
                                             ymax = 155,
                                             plotTitle = "25OHD for men and women",
                                             plotSubtitle = "",
                                             plotYLabel = "25OHD (nmol/L)",
                                             overrideImageWidth = 5)

	
	myBoxplotResults = doBoxPlotV2(completeTable, vitamimDIndex, VITAMIND_FOLDER,
                                   groupIndex = sexIndex,
								   colorsVector = COLOR_VECTOR_SEX,
                                   showPValues = TRUE,
                                   cutOffLine = cutOffVitD,
                                   ymax = 155,
                                   plotTitle = "25OHD for men and women",
                                   plotSubtitle = "",
                                   plotYLabel = "25OHD (nmol/L)",
                                   overrideImageWidth = 5)	
	
	

    writeImageLATEX2(myBoxplotResults[[4]], VITAMIND_FOLDER, 
                     captionText   = "Differences between men and women in 25OHD",
                     overrideLabel = "fig:Relationship25OHDMenWomen",
                     pageHeight = 0.4, overrideFloat = TRUE)    
    
    
    # Lets compare diets
    # The results tell us that men and women eat more or less the same, with no signicicant xi2 test
    {
    
        myXiSummaries = readyXiSummary(completeTable, vitaminDIndexes, sexIndex, skipUnknowns = UNKNOWN_VARIABLES, fillWith = "none")
    
        writeTableLATEX(myXiSummaries[[1]], VITAMIND_FOLDER,
                        tableCaption = "Xi-square test for dietary habits of men and women",
                        overrideTableName = "vitDhabitsSimpleSex",
                        widthProportion = 0.3, heightProportion = 0.1)    
    
        writeTableLATEX(myXiSummaries[[2]], VITAMIND_FOLDER,
                        tableCaption = "Full count of dietary habits for men and women",
                        overrideTableName = "vitDhabitsCompleteSex",
                        widthProportion = 0.5, heightProportion = 0.4)        
    
    }
    
    # Lets compare sun exposure
    # There is a correlation between date and vitD (low p-value), but the model
    # is not good at all making predictions (R2 close to 0). There is a lot of
    # unexplained variance that we didn't put inside the model.
    {

        # We need to transform dates to numbers in order to do regression (or time series)
        datesTransformation = transformToNumeric(completeTable, bloodExtractionDateIndex)
        completeTableTemp   = completeTable
        completeTableTemp[,bloodExtractionDateIndex] = datesTransformation
    
        # Checking correlation between VD and date (low R2 and p-value)
        myPlot = doRegressionPlot(completeTableTemp, bloodExtractionDateIndex, vitamimDIndex, VITAMIND_FOLDER,
                                  horizontalLinesCuts = cutOffVitD,
                                  plotTitle = "Relationship between 25OHD and time of blood extraction",
                                  plotXLabel = "Date (October 2010 - May 2011)",
                                  plotYLabel = "25OHD (nmol/L)",
                                  plotTheme = "regression",
                                  ymax = 160,
                                  overrideImageWidth = 12)

        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Correlation between vitamin D and date of the extraction",
                         overrideLabel = "fig:RelationshipDATENUMVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)            
        
        
        # Same thing without regression analysis, since we don't have numbers anymore
        myPlot = doTimeSeriesPlot(completeTable, bloodExtractionDateIndex, vitamimDIndex, VITAMIND_FOLDER,
                                  horizontalLinesCuts = cutOffVitD,
                                  plotTitle = "Relationship between 25OHD and time of blood extraction",
                                  plotXLabel = "Date",
                                  plotYLabel = "25OHD (nmol/L)",
                                  plotTheme = "regression",
                                  ymax = 160,
                                  overrideImageWidth = 12)        
        
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between vitamin D and date of the extraction",
                         overrideLabel = "fig:RelationshipDATEVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)           
        
        # Same thing but divided by men and women
        myPlot = doTimeSeriesPlot(completeTable, bloodExtractionDateIndex, vitamimDIndex, VITAMIND_FOLDER_IMAGES_TIMESERIES,
                                  groupingIndex = sexIndex, colorsVector = COLOR_VECTOR_SEX,
                                  horizontalLinesCuts = cutOffVitD,
                                  plotTitle    = "LOESS regression between 25OHD and time of blood extraction",
                                  plotSubtitle = "Grouped by sex",
                                  plotXLabel   = "Date",
                                  plotYLabel   = "25OHD (nmol/L)",
                                  plotTheme    = "regression",
                                  ymax         = 160,
                                  overrideImageWidth = 12)        
        
        
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER_IMAGES_TIMESERIES, 
                         captionText   = "Relationship between vitamin D and date of the extraction",
                         overrideLabel = "fig:RelationshipSEXDATEVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)
        
        # Same thing but for solarium visit in the last 4 weeks
        # Clean the data a little bit first
        completeTableTemp = completeTable
        completeTableTemp = completeTableTemp[!is.na(completeTableTemp[,solariumIndex]),]
        myPlot = doTimeSeriesPlot(completeTableTemp, bloodExtractionDateIndex, vitamimDIndex, VITAMIND_FOLDER_IMAGES_TIMESERIES,
                                  groupingIndex = solariumIndex, colorsVector = COLOR_VECTOR_SOLARIUM, 
                                  horizontalLinesCuts = cutOffVitD,
                                  plotTitle = "Relationship between 25OHD and time of blood extraction",
                                  plotSubtitle = "Grouped by a visit to the solarium in the last 4 weeks",
                                  plotXLabel = "Date",
                                  plotYLabel = "25OHD (nmol/L)",
                                  plotTheme = "regression",
                                  ymax = 160,
                                  overrideImageWidth = 12)
        
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER_IMAGES_TIMESERIES, 
                         captionText   = "Relationship between vitamin D and date of the extraction, grouped by recent visit to the solarium.",
                         overrideLabel = "fig:RelationshipSOLDATEVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)             
        
        # Xi2 analysis for sex and solarium
        xiResults = categoricalXi(completeTable, solariumIndex ,sexIndex)
        
        writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                        tableCaption = "Xi-square test for solarium habits of men and women",
                        overrideTableName = "solariumHabitsSex",
                        widthProportion = 0.5, heightProportion = 0.07)        
        
        # Let see the difference between people that visit the solarium for men and women
        # ---- WOMEN
        womenOnlyTableTemp = womenOnlyTable
        womenOnlyTableTemp = womenOnlyTableTemp[!is.na(womenOnlyTableTemp[,solariumIndex]),]
        myBoxplotResultsW  = doCategoricalBoxPlot (womenOnlyTableTemp,
                                                 solariumIndex,
                                                 vitamimDIndex,
                                                 VITAMIND_FOLDER,
                                                 colorsVector = COLOR_VECTOR_SOLARIUM,
                                                 showPValues = TRUE,
                                                 cutOffLine = cutOffVitD,
                                                 ymax = 155,
                                                 plotTitle = "25OHD for WOMEN only",
                                                 plotSubtitle = "Grouped by recent solarium visit",
                                                 plotYLabel = "25OHD (nmol/L)",
                                                 overrideImageWidth = 5)


        writeImageLATEX2(myBoxplotResults[[4]], VITAMIND_FOLDER, 
                         captionText   = "Differences in 25OHD levels for women visiting the solarium",
                         overrideLabel = "fig:Relationship25OHDWomen",
                         pageHeight = 0.4, overrideFloat = TRUE)
        
        
        menOnlyTableTemp  = menOnlyTable
        menOnlyTableTemp  = menOnlyTableTemp[!is.na(menOnlyTableTemp[,solariumIndex]),]
        myBoxplotResultsM = doCategoricalBoxPlot (menOnlyTableTemp,
                                                 solariumIndex,
                                                 vitamimDIndex,
                                                 VITAMIND_FOLDER,
                                                 colorsVector = COLOR_VECTOR_SOLARIUM,
                                                 showPValues = TRUE,
                                                 cutOffLine = cutOffVitD,
                                                 ymax = 155,
                                                 plotTitle = "25OHD for MEN only",
                                                 plotSubtitle = "Grouped by recent solarium visit",
                                                 plotYLabel = "25OHD (nmol/L)",
                                                 overrideImageWidth = 5)


        writeImageLATEX2(myBoxplotResults[[4]], VITAMIND_FOLDER, 
                         captionText   = "Differences in 25OHD levels for men visiting the solarium",
                         overrideLabel = "fig:Relationship25OHDMen",
                         pageHeight = 0.4, overrideFloat = TRUE)
        
        
        myListOfPlotsObjects      = newList(2)
        myListOfPlotsObjects[[1]] = myBoxplotResultsW[[1]]
        myListOfPlotsObjects[[2]] = myBoxplotResultsM[[1]]
        ggarrange(plotlist =  myListOfPlotsObjects , ncol = 2, nrow = 1)
        
        allGraphPath  = file.path(paste(VITAMIND_FOLDER, "2x1VitDSolarium.png", sep = ""))
        ggsave(allGraphPath, width = 16, height = 8)
        
        writeImageLATEX2(allGraphPath, VITAMIND_FOLDER, 
                         captionText   = "Differences in 25OHD levels for men and women visiting the solarium",
                         overrideLabel = "fig:Relationship25OHDMenWomen",
                         pageHeight = 0.4, overrideFloat = TRUE)
        
    }
    


    
    
}

# Let see for health
{
    
    nonNASolariumOnlyTable = completeTable
    nonNASolariumOnlyTable = deleteNA(nonSolariumOnlyTable, solariumIndex)
    nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, solariumIndex, "Yes")
    nonNASolariumOnlyTable = deleteNA(nonNASolariumOnlyTable, healthIndex)
    nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, healthIndex,   "Unknown")
    
    myPlot = doBoxPlotV2 (nonNASolariumOnlyTable, vitamimDIndex, VITAMIND_FOLDER,
                 groupIndex = healthIndex,
                 showPValues = TRUE,
                 significantPValue = 0.05,
                 
                 
                 cutOffLine = cutOffVitD,
                 colorsVector = COLOR_VECTOR_HEALTH,
                 ymax = 155,
                 plotTitle = "25OHD levels with respect self reported health",
                 plotSubtitle = "Only analyzing people not going to the solarium in the last 4 week",
                 plotYLabel = "25OHD (nmol/L)",
                 overrideImageWidth = 7)  
    
    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                     captionText   = "Relationship between vitamin D and self reported health",
                     overrideLabel = "fig:RelationshipVITDHealthSelf",
                     pageHeight = 0.3, overrideFloat = TRUE)         
    
}

# By use of hormonal contraceptives
{

    nonNASolariumOnlyTable = womenOnlyTable
    nonNASolariumOnlyTable = deleteNA(nonNASolariumOnlyTable, solariumIndex)
    nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, solariumIndex, "Yes")
    nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, hormonalTypeIndex, "Unknown")
    
    
    
    
    myPlot = doBoxPlotV2 (nonNASolariumOnlyTable, vitamimDIndex, VITAMIND_FOLDER,
                 groupIndex = hormonalTypeIndex,
                 showPValues = TRUE,
                 significantPValue = 0.1,
                 pValuesFormat = "number3",
                 cutOffLine = cutOffVitD,
                 #colorsVector = COLOR_VECTOR_HEALTH,
                 ymax = 155,
                 plotTitle = "25OHD levels with respect contraceptives use",
                 plotSubtitle = "Only analyzing people not going to the solarium in the last 4 week",
                 plotYLabel = "25OHD (nmol/L)",
                 overrideImageWidth = 7)  
    
    
    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                     captionText   = "Relationship between vitamin D and use of contraceptives (women only)",
                     overrideLabel = "fig:RelationshipVITDContraceptives",
                     pageHeight = 0.3, overrideFloat = TRUE)             

    writeTableLATEX(myPlot[[4]], VITAMIND_FOLDER,
                    tableCaption = "Experimental",
                    overrideTableName = "vitDExperimental",
                    widthProportion = 0.5, heightProportion = 0.05)    
    
    
    
       writeBoxPlotCompositeLATEX ( myPlot[[2]], myPlot[[4]], myPlot[[5]], VITAMIND_FOLDER,
                                    
                                    captionText = "Relationship between vitamin D and use of contraceptives for women not going to the solarium",
           
                                    
                                    minipageTop = 0.4,  minipageBottom = 0.5 ,         
           
                                            pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
           
                                            topTableWidth    = 0.4, topTableHeight    = 0.05,
                                            bottomTableWidth = 0.4, bottomTableHeight = 0.05
           
           
                                   )
    

        # Analyze women contraceptive usage by highschool
        # -- Usage is very biased with respect school so maybe vitamin D levels is school related
        nonNASolariumOnlyTable = womenOnlyTable
        nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, hormonalTypeIndex, "Unknown")
                                           
        xiResults = categoricalXiV2(nonNASolariumOnlyTable, highSchoolIndex ,hormonalTypeIndex)
       
        writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                        tableCaption = "HC usage bias with respect high-school",
                        overrideTableName = "vitDWamenBiasHSHC",
                        widthProportion = 0.8, heightProportion = 0.09) 
       
        # Analyze women contraceptive usage with obesity
        nonNASolariumOnlyTable = womenOnlyTable
        nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, hormonalTypeIndex, "Unknown")
        nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, BMICatIndex,       "Unknown")
        
        xiResults = categoricalXiV2(nonNASolariumOnlyTable, BMICatIndex ,hormonalTypeIndex)
}



# Lets make our own weird vitD predictor
{
    
}

# Show association between BMI and vitD for all combination
{
 
    # For the categorical data
    nonNASolariumOnlyTable = completeTable
    nonNASolariumOnlyTable = deleteNA(nonSolariumOnlyTable, solariumIndex)
    nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, solariumIndex, "Yes")
    nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, BMICatIndex,   "Unknown")
    nonNASolariumOnlyTable = deleteNA(nonNASolariumOnlyTable, BMICatIndex)
    
    
    myPlot = doBoxPlotV2 (nonNASolariumOnlyTable, vitamimDIndex, VITAMIND_FOLDER,
                 groupIndex = BMICatIndex,
                 
                 showPValues = TRUE,
                 significantPValue = 0.05,
                 cutOffLine = cutOffVitD,
                 colorsVector = COLOR_VECTOR_BMI,
                 ymax = 155,
                 plotTitle = "25OHD levels with respect BMI",
                 plotSubtitle = "Only analyzing people not going to the solarium in the last 4 week",
                 plotYLabel = "25OHD (nmol/L)",
                 overrideImageWidth = 7)  
    
    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                     captionText   = "Relationship between vitamin D and BMI categorical data",
                     overrideLabel = "fig:RelationshipVITDBMICat",
                     pageHeight = 0.3, overrideFloat = TRUE)     
    
    
    # For the numerical data
    nonNASolariumOnlyTable = completeTable
    nonNASolariumOnlyTable = deleteNA(nonSolariumOnlyTable, solariumIndex)
    nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, solariumIndex, "Yes")
    nonNASolariumOnlyTable = deleteNA(nonNASolariumOnlyTable, BMIIndex)
    
    
    myPlot = doRegressionPlot(nonNASolariumOnlyTable, BMIIndex, vitamimDIndex, VITAMIND_FOLDER,
                              verticalLinesCuts   = cutOffBMI,
                              horizontalLinesCuts = cutOffVitD,
                              plotTitle = "Relationship between BMI and 25OHD levels",
                              plotSubtitle = "Only analyzing people not going to the solarium in the last 4 week",
                              plotXLabel = "BMI (kg/m2)",
                              plotYLabel = "25OHD (nmol/L)",
                              ymax = 155,
                              plotTheme = "regression",
                              overrideImageWidth = 12)
    
    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between BMI and vitamin D in blood.",
                         overrideLabel = "fig:RelationshipBMINumVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)  
    
    
    # Check for the fatty accids
    {
        
    
        nonNASolariumOnlyTable  = completeTable
        nonNASolariumOnlyTable  = deleteNA(nonSolariumOnlyTable, solariumIndex)
        nonNASolariumOnlyTable  = deleteCategory(nonNASolariumOnlyTable, solariumIndex, "Yes")
        nonNASolariumOnlyTable  = deleteNA(nonNASolariumOnlyTable, vitamimDIndex)
    
        fattyAccidsDF           = DF(totalFAColumns, 3)
        colnames(fattyAccidsDF) = c("Fatty accid", "p-value", "R2", "benjamini", "bonferroni")
    
        for(i in 1:totalFAColumns){

        currentIndex = firstFAIndex + i - 1
        currentTable = nonNASolariumOnlyTable
        currentTable = currentTable[,c(vitamimDIndex, currentIndex)]
        
        currentTable = deleteNA(currentTable, 2)
        currentName  = colnames(completeTable)[currentIndex]

        myPlot = doRegressionPlot(currentTable, 2, 1, VITAMIND_FOLDER,
                              horizontalLinesCuts = cutOffVitD,
                              plotTitle           = paste0("Relationship between BMI and ", currentName),
                              plotSubtitle        = "Only analyzing peoplpe not going to the solarium in the last 4 week",
                              plotXLabel          = " units ",
                              plotYLabel          = "25OHD (nmol/L)",
                              ymax                = 155,
                              plotTheme           = "regression",
                              overrideImageWidth = 12)   
    
        # FIll the DF
        # ---- Which is the minimum p-value?
        minimumPvalue = min(myPlot[[5]][,3])
        minimumRow    = getMinimumRow(myPlot[[5]], 3)
        r2value       = myPlot[[5]][minimumRow,2]
        
        fattyAccidsDF[i,1] = currentName
        fattyAccidsDF[i,2] = minimumPvalue
        fattyAccidsDF[i,3] = r2value
        
        
    }

        # Do the p-values correction
        fattyAccidsDF[,4] = p.adjust(fattyAccidsDF[,2], method = "fdr")  
        fattyAccidsDF[,5] = p.adjust(fattyAccidsDF[,2], method = "bonferroni") 
        
        fattyAccidsDFCopy = fattyAccidsDF
        fattyAccidsDFCopy[,2] = getAsterkisPValue(fattyAccidsDFCopy[,2])
        fattyAccidsDFCopy[,4] = getAsterkisPValue(fattyAccidsDFCopy[,4])
        fattyAccidsDFCopy[,5] = getAsterkisPValue(fattyAccidsDFCopy[,5])
        fattyAccidsDFCopy[,3] = round(fattyAccidsDFCopy[,3],2)
        
        writeTableLATEX(fattyAccidsDFCopy, VITAMIND_FOLDER,
                        tableCaption = "Significance for fatty accids in blood serum and 25OHD levels",
                        overrideTableName = "vitDFAcomplete",
                        widthProportion = 0.5, heightProportion = 0.5)                
        
        
        # We can try to check out from where do the fatty accids come in the diet
        {
            
            
            # prepare the dataframe with the results
            fattyAccidsOriginDF = DF(totalFA2Columns, totalDietIndexes+1)
            colnames(fattyAccidsOriginDF) = c("FA","Lean Fish", "Fat Fish", "Cheese",
                                              "Chocolate", "Fruits", "Vegetables",
                                              "Dairy", "Juice Fruits", "Juice Sugar",
                                              "Sugar Drink", "SweetenerDrink", "Water",
                                              "Fish Oil", "Vitamins")
                
                
            
            for(i in 1:totalFA2Columns){
                
                currentIndex = firstFAIndex + i - 1
                
                # Delete the NAs for this particular FA
                currentTable = completeTable
                currentTable = deleteNA(currentTable, currentIndex)
                currentName  = colnames(completeTable)[currentIndex]
                
                fattyAccidsOriginDF[i,1] = currentName
                
                for(j in 1:totalDietIndexes){
                    
                    print("---------")
                    print(i)
                    print(j)
                    print(((i-1) * totalFA2Columns + j) / (totalFA2Columns * totalDietIndexes))
                    
                    # Delete people who didn't respond for this particular food panel
                    currentDietIndex = dietIndexes[j]
                    currentTableB    = deleteCategory(currentTable, currentDietIndex, "Didn't Answered")
                    
                    currentPlotTitle = paste(currentName," with ", colnames(fattyAccidsOriginDF)[j+1])
                    
                    analysisResults  = doBoxPlotV2 (currentTableB, currentIndex, VITAMIND_FOLDER,
                                                    groupIndex = currentDietIndex,
                 
                                                    showPValues = TRUE,
                                                    significantPValue = 0.05,
                 
                                                    plotTitle = currentPlotTitle,
                                                    plotSubtitle = "Only analyzing people not going to the solarium in the last 4 week",
                 
                                                    overrideImageWidth = 7)    
                    
                    fattyAccidsOriginDF[i,j+1] = analysisResults[[3]][[2]]
                    
                }
                
            }
            
            # Correct for bonferroni
            fattyAccidsOriginDF2 = fattyAccidsOriginDF
            for(j in 1:totalDietIndexes){
                
                
                fattyAccidsOriginDF2[,j+1] = p.adjust(fattyAccidsOriginDF2[,j+1], method = "bonferroni") 
                
            }
            
            # Transform into something readable
            for(i in 1:totalFA2Columns){
                
                for(j in 1:totalDietIndexes){
                    
                    # I hate R, Why, in the fuck, are you converting numbers to strings now bithc!?!?!?

                    fattyAccidsOriginDF2[i,j+1] = getAsterkisPValue( as.numeric(fattyAccidsOriginDF2[i,j+1]))
                    
                }
                
            }
            
            writeTableLATEX(fattyAccidsOriginDF2, VITAMIND_FOLDER,
                        tableCaption = "What changes Fatty Acids levels?",
                        overrideTableName = "FACompositionbyDiet",
                        widthProportion = 0.9, heightProportion = 0.2)     
            
        }
        
        
    }
    
}

        
        
# Let see absortion levels
{

    completeTableTemp  = completeTable
    completeTableTemp  = deleteCategory(completeTableTemp, solariumIndex, "Yes")
    completeTableTemp  = deleteNA(completeTableTemp, solariumIndex)
    womenOnlyTableTemp = deleteCategory(completeTableTemp, sexIndex, "Man")
    
    # Delete a woman that has way too much estradiol
    # Fuck R and it shitty NA handling!
    womenOnlyTableTemp = deleteGreaterThan(womenOnlyTableTemp, estradiolIndex, 2,5)
    
    
    # Checking correlation between VD and Estradiol and Progesteron in women only, regardless of HC status, for non solarium users
    {
        
        
        # LOESS Regression doesn't show anything convinient, just ignore them
        {
    
        
            myPlot = doLOESSRegressionPlot(womenOnlyTableTemp, estradiolIndex, vitamimDIndex, VITAMIND_FOLDER,
                                      horizontalLinesCuts   = cutOffVitD,
                                      plotTitle = "Relationship between 25OHD and Estradiol",
                                      plotXLabel = "Estradiol (nmol/L)",
                                      plotYLabel = "25OHD (nmol/L)",
                                      plotTheme = "regression",
                                      ymax = 160,
                                      overrideImageWidth = 12)        
            
            myPlot = doLOESSRegressionPlot(womenOnlyTableTemp, progesteronIndex, vitamimDIndex, VITAMIND_FOLDER,
                                      horizontalLinesCuts   = cutOffVitD,
                                      plotTitle = "Relationship between 25OHD and Progesterone",
                                      plotXLabel = "Progesterone (nmol/L)",
                                      plotYLabel = "25OHD (nmol/L)",
                                      plotTheme = "regression",
                                      ymax = 160,
                                      overrideImageWidth = 12)              
                    
        }
        
        # Regression models
        {
            
            
            myPlot = doRegressionPlot(womenOnlyTableTemp, estradiolIndex, vitamimDIndex, VITAMIND_FOLDER,
                                      horizontalLinesCuts   = cutOffVitD,
                                      plotTitle    = "Relationship between 25OHD and Estradiol",
                                      plotSubtitle = "Only women who haven't gone to the solarium in the last 4 weeks",
                                      plotXLabel = "Estradiol (nmol/L)",
                                      plotYLabel = "25OHD (nmol/L)",
                                      plotTheme = "regression",
                                      overrideImageWidth = 12)       
            
            writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between estradiol and vitamin D in blood.",
                         overrideLabel = "fig:RelationshipESTRVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)    
            
            myPlot = doRegressionPlot(womenOnlyTableTemp, progesteronIndex, vitamimDIndex, VITAMIND_FOLDER,
                                      horizontalLinesCuts   = cutOffVitD,
                                      plotTitle = "Relationship between 25OHD and Progesterone",
                                      plotSubtitle = "Only women who haven't gone to the solarium in the last 4 weeks",
                                      plotXLabel = "Progesteron (nmol/L)",
                                      plotYLabel = "25OHD (nmol/L)",
                                      plotTheme = "regression",
                                      overrideImageWidth = 12)      
            
            writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between progesterone and vitamin D in blood.",
                         overrideLabel = "fig:RelationshipPROGVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)                
            
            
        }
        
        # Simplify by boxplots
        {
            
            estradiolHormoneThrelshold    = 0.4
            progesteroneHormoneThrelshold = 10
            womenOnlyTableTemp$EstradiolLevel    = "Low"
            womenOnlyTableTemp$ProgesteroneLevel = "Low"
            
            womenOnlyTableTemp = deleteNA(womenOnlyTableTemp, estradiolIndex    )
            womenOnlyTableTemp = deleteNA(womenOnlyTableTemp, progesteronIndex  )
            
            for(i in 1:nrow(womenOnlyTableTemp)){
                
                if( womenOnlyTableTemp[i, estradiolIndex ]   > estradiolHormoneThrelshold   )  womenOnlyTableTemp$EstradiolLevel[i]    = "High"
                
                if( womenOnlyTableTemp[i, progesteronIndex ] > progesteroneHormoneThrelshold)  womenOnlyTableTemp$ProgesteroneLevel[i] = "High"
                
            }
                    
            myPlot = doBoxPlotV2 (womenOnlyTableTemp, vitamimDIndex, VITAMIND_FOLDER,
                                  groupIndex = (ncol(womenOnlyTableTemp)-1),
                         
                                  showPValues = TRUE,
                                  significantPValue = 0.05,
                                  cutOffLine = cutOffVitD,
                                 
                                  ymax = 155,
                                  plotTitle = "25OHD levels with respect Estradiol",
                                  plotSubtitle = "Only analyzing women not going to the solarium in the last 4 week",
                                  plotYLabel = "25OHD (nmol/L)",
                                  overrideImageWidth = 7)
                    
             writeBoxPlotCompositeLATEX ( myPlot[[2]], myPlot[[4]], myPlot[[5]], VITAMIND_FOLDER,
                                            
                                          captionText = "Relationship between vitamin D and estradiol (>0.4 nmol/l) in women not going to the solarium",
                   
                                            
                                          minipageTop = 0.4,  minipageBottom = 0.5 ,         
                   
                                          pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                   
                                          topTableWidth    = 0.4, topTableHeight    = 0.05,
                                          bottomTableWidth = 0.4, bottomTableHeight = 0.05
                   
                                          )
            
            myPlot = doBoxPlotV2 (womenOnlyTableTemp, vitamimDIndex, VITAMIND_FOLDER,
                                  groupIndex = (ncol(womenOnlyTableTemp)),
                         
                                  showPValues = TRUE,
                                  significantPValue = 0.05,
                                  cutOffLine = cutOffVitD,
                                 
                                  ymax = 155,
                                  plotTitle = "25OHD levels with respect Progesterone",
                                  plotSubtitle = "Only analyzing women not going to the solarium in the last 4 week",
                                  plotYLabel = "25OHD (nmol/L)",
                                  overrideImageWidth = 7)            
            
             writeBoxPlotCompositeLATEX ( myPlot[[2]], myPlot[[4]], myPlot[[5]], VITAMIND_FOLDER,
                                            
                                          captionText = "Relationship between vitamin D and progesterone (>10 nmol/l) in women not going to the solarium",
                   
                                          minipageTop = 0.4,  minipageBottom = 0.5 ,         
                   
                                          pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                   
                                          topTableWidth    = 0.4, topTableHeight    = 0.05,
                                          bottomTableWidth = 0.4, bottomTableHeight = 0.05
                   
                                          )            
            
            
        }
        
    
 
    
    }    

    # Checking correlation between VD and Ca
    {
        
        myPlot = doRegressionPlot(completeTableTemp, vitamimDIndex, calciumIndex, VITAMIND_FOLDER,
                                  horizontalLinesCuts = cutOffCalcium,
                                  verticalLinesCuts   = cutOffVitD,
                                  plotTitle = "Relationship between 25OHD and Calcium",
                                  plotXLabel = "25OHD (nmol/L)",
                                  plotYLabel = "Calcium (mmol/L)",
                                  plotTheme = "regression",
                                  overrideImageWidth = 12)
    
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between calcium and vitamin D in blood. The central square shows people within healthy limits",
                         overrideLabel = "fig:RelationshipCAVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)     
    
    }
    
    # Checking correlation for PTH
    {
        
        # -- PTH should correlate with Calcium, more PTH = more calcium abortions from the bones
        myPlot = doRegressionPlot(completeTableTemp, pthIndex, calciumIndex, VITAMIND_FOLDER,
                                  horizontalLinesCuts = cutOffCalcium,
                                  #verticalLinesCuts   = cutOffPTH,
                                  plotTitle = "Relationship between PTH and Calcium",
                                  plotXLabel = "PTH (pmol/L)",
                                  plotYLabel = "Calcium (mmol/L)",
                                  plotTheme = "regression",
                                  overrideImageWidth = 12)
    
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between calcium and PTH in blood.",
                         overrideLabel = "fig:RelationshipCAPTH",
                         pageHeight = 0.3, overrideFloat = TRUE)    
        
        
        # -- PTH should correlate with VitD, low vitamin D should stimulate PTH production
        myPlot = doRegressionPlot(completeTableTemp, pthIndex, vitamimDIndex, VITAMIND_FOLDER,
                                  horizontalLinesCuts = cutOffVitD,
                                  #verticalLinesCuts   = cutOffPTH,
                                  plotTitle = "Relationship between PTH and Vitamin D",
                                  plotXLabel = "PTH (pmol/L)",
                                  plotYLabel = "25OHD (nmol/L)",
                                  ymax = 160,
                                  plotTheme = "regression",
                                  overrideImageWidth = 12)    
        
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between PTH and 25OHD in blood.",
                         overrideLabel = "fig:RelationshipVitDPTH",
                         pageHeight = 0.3, overrideFloat = TRUE)    

 
        # -- PTH should correlate with BMI according to other papers
        myPlot = doRegressionPlot(completeTable, pthIndex, BMIIndex, VITAMIND_FOLDER,
                                  horizontalLinesCuts = cutOffBMI,
                                  verticalLinesCuts   = cutOffPTH,
                                  plotTitle    = "Relationship between PTH and BMI",
        	                      plotSubtitle = "All 1038 students are included in this analysis",
                                  plotXLabel   = "PTH (pmol/L)",
                                  plotYLabel   = "BMI (kg/m²)",
                                  plotTheme    = "regression",
                                  overrideImageWidth = 12)    
        
        myPlot = doRegressionPlot(completeTable, BMIIndex, pthIndex, VITAMIND_FOLDER,
                                  horizontalLinesCuts = cutOffPTH,
                                  verticalLinesCuts   = cutOffBMI,
        						  myRounding   = 3,
                                  plotTitle    = "Relationship between PTH and BMI",
        	                      plotSubtitle = "All 1038 students are included in this analysis",
                                  plotYLabel   = "PTH (pmol/L)",
                                  plotXLabel   = "BMI (kg/m²)",
                                  plotTheme    = "regression",
        	                      ymin         = 0,
                                  overrideImageWidth = 12)    
                
        
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between PTH and BMI. All the students are included in this analysis. 
        	                              Vertical lines represent the threeshold between BMI categories. Horizontal lines represent
        	                              the healthy boundaries of PTH levels.",
                         overrideLabel = "fig:RelationshipBMIPTH",
                         pageHeight = 0.3, overrideFloat = TRUE)          
        


        totalMales = nrow( menOnlyTable[ !is.na(menOnlyTable[,testosteroneIndex]) & !is.na(menOnlyTable[,pthIndex]) , ] )
        
        myPlot = doRegressionPlot(menOnlyTable, testosteroneIndex, pthIndex, VITAMIND_FOLDER_IMAGES_SCATTERPLOTS,
                                  horizontalLinesCuts = cutOffPTH,
        						  myRounding   = 3,
                                  plotTitle    = "Relationship between PTH and testosreno in men",
        	                      plotSubtitle = paste0( "All male students with valid values are included (n = ", totalMales , ")" ),
                                  plotYLabel   = "PTH (pmol/L)",
                                  plotXLabel   = "Testosterone (nmol/l)",
                                  plotTheme    = "regression",
        	                      ymin         = 0,
                                  overrideImageWidth = 12)                    
                
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between PTH and testosterone in men. All the students are included in this analysis. 
        	                              Horizontal lines represent the healthy boundaries of PTH levels.",
                         overrideLabel = "fig:RelationshipTestMalesPTH",
                         pageHeight = 0.3, overrideFloat = TRUE)      
        
        totalFemales = nrow( menOnlyTable[ !is.na(womenOnlyTable[,testosteroneIndex]) & !is.na(womenOnlyTable[,pthIndex]) , ] )
        
        myPlot = doRegressionPlot(womenOnlyTable, testosteroneIndex, pthIndex, VITAMIND_FOLDER_IMAGES_SCATTERPLOTS,
                                  horizontalLinesCuts = cutOffPTH,
        						  myRounding   = 3,
                                  plotTitle    = "Relationship between PTH and BMI in women",
        						  plotSubtitle = paste0("All female students with valid values are included (n = ", totalFemales, ")" ),
                                  plotYLabel   = "PTH (pmol/L)",
                                  plotXLabel   = "Testosterone (nmol/l)",
                                  plotTheme    = "regression",
        	                      ymin         = 0,
                                  overrideImageWidth = 12)            
        
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between PTH and testosterone in women. All the students are included in this analysis. 
        	                              Horizontal lines represent the healthy boundaries of PTH levels.",
                         overrideLabel = "fig:RelationshipTestFemalesPTH",
                         pageHeight = 0.3, overrideFloat = TRUE)            
        
        
        
        summarizeNumerical(womenOnlyTable, testosteroneIndex)
        summarizeNumerical(menOnlyTable,   testosteroneIndex)
        
    
    }
    
    # Checking for blood variable that has something to do with liver or kidney diseases
    {
        
        
        myPlot = doRegressionPlot(completeTableTemp, vitamimDIndex, apoBIndex, VITAMIND_FOLDER,
                                  horizontalLinesCuts = cutOffAPOB,
                                  verticalLinesCuts   = cutOffVitD,
                                  plotTitle = "Relationship between 25OHD and Apolipoprotein B",
                                  plotXLabel = "25OHD (nmol/L)",
                                  plotYLabel = "ApoB (g/L)",
                                  plotTheme = "regression",
                                  overrideImageWidth = 12)
    
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between apolipoprotein B and vitamin D in blood. The central square shows people within healthy limits",
                         overrideLabel = "fig:RelationshipAPOBVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)             
        
        
        
        
        myPlot = doRegressionPlot(completeTableTemp, vitamimDIndex, retinolIndex, VITAMIND_FOLDER,
                                  horizontalLinesCuts = cutOffRetinol,
                                  verticalLinesCuts   = cutOffVitD,
                                  plotTitle = "Relationship between 25OHD and Retinol",
                                  plotXLabel = "25OHD (nmol/L)",
                                  plotYLabel = "Retinol (umol/L)",
                                  plotTheme = "regression",
                                  overrideImageWidth = 12)
    
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between retinol and vitamin D in blood. The central square shows people within healthy limits",
                         overrideLabel = "fig:RelationshipRETINOLVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)             
        
        
    }
    
    # Let see the multivariate regression, but there is nothing here
    model.final = lm(Calcium_.mmol.L. ~ log(X25.OH.D_.nmol.L.) + log(PTH_.pmol.L.) + Retinol_.µmol.L. + Apolipoprotein_B_.g.L.,
                     data=completeTable,
                     na.action(na.omit))

    summary(model.final)

    
    library(GGally)
    newDataTable = completeTable[,c(calciumIndex,vitamimDIndex,pthIndex)]
    ggpairs(newDataTable)    
    
    
}

# Let see diseases
{
    
    myPlot = doLongBarPlot(diseasesDBDF, 2, VITAMIND_FOLDER,
                           plotTitle = NULL, plotSubtitle = NULL,
                           plotXLabel = NULL, plotYLabel = NULL,
                           barsFontSize = 2,
                           crop = 0, top = 0, sort = "descending",
                           overrideHeigh = 25)

    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                     captionText   = "All diseases sorted by frequency",
                     overrideLabel = "fig:alldisease",
                     pageHeight = 1, overrideFloat = TRUE)     
    
    vitDdiseases = c("Celiac disease", "Food allergy", "Lactose intolerance",
                     "Gastritis", "Eating Disorder", "Artritis", "Juvenile osteochondrosis of patella",
                     "Crohns disease", "Gastric ulcer", "Scoliosis", "Ankylosing spondylitis of unspecified sites in spine",
                     "Other arthritis", "Osteomyelitis, unspecified", "Juvenile arthritis", "Chronic kidney disease, stage 2 (mild)",
                     "Osteomyelitis")
    
    peopleIDVitD = unique(diseasesDBDF[diseasesDBDF$Diagnostic %in% vitDdiseases,1])

    length(peopleIDVitD)

    completeTableTemp = completeTable
    completeTableTemp$DiseaseGroup = "no related"
    for(i in 1:totalRows){
        if (i %in% peopleIDVitD == TRUE) completeTableTemp$DiseaseGroup[i] = "vitD related"
    }

    diseaseGroupIndex = getIndexDF("DiseaseGroup", completeTableTemp)
    
    nonSolariumOnlyTable = completeTableTemp[!is.na(completeTableTemp[, solariumIndex]),] # Fuck you R, and your complete fuckity of NA values FALSE != NA , you fucking cunt!!!
    nonSolariumOnlyTable = nonSolariumOnlyTable[nonSolariumOnlyTable[,solariumIndex] == "No",]
    myColorVector        = c(colorGroupB, colorGroupA)
    
    myPlot = doBoxPlotV2 (nonSolariumOnlyTable, vitamimDIndex, VITAMIND_FOLDER,
                          groupIndex = diseaseGroupIndex,
                          colorsVector = myColorVector,
                          showPValues = TRUE,
                          pValuesFormat = "number4",
                          cutOffLine = cutOffVitD,
                          ymax = 155,
                          plotTitle = "25OHD levels with vitamin D related diseases",
                          plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
                          plotYLabel = "25OHD (nmol/L)",
                          overrideImageWidth = 7)

    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                     captionText   = "25OHD levels with respect people who have vitamin D related diseases",
                     overrideLabel = "fig:vitDdiseasesLevels",
                     pageHeight = 0.3, overrideFloat = TRUE)  
    
    # Divide the data into people  who doesn't go to the solarium and:
    # ---- Take pills or vitamins
    # ---- Don't take pills and vitamins
    #
    # So, just do a xi2 test
    #
    # First, take away people who didn't answered the question
    #
    # ---- Fish Oil
    nonSolariumOnlyTable2 = deleteCategory(nonSolariumOnlyTable, fishOilIndex, "Didn't Answered")
    xiResults = categoricalXiV2(nonSolariumOnlyTable2, fishOilIndex ,diseaseGroupIndex)
    
    writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                    tableCaption = "Xi-square test for vitamin D related diseases group and fish oil consumption",
                    overrideTableName = "noSolariumXiTestVITDFishOil",
                    widthProportion = 0.5, heightProportion = 0.07)            
    
    # ---- Vitamins 
    nonSolariumOnlyTable2 = deleteCategory(nonSolariumOnlyTable, vitaminsIndex, "Didn't Answered")
    xiResults = categoricalXiV2(nonSolariumOnlyTable2, vitaminsIndex ,diseaseGroupIndex)
    
    writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                    tableCaption = "Xi-square test for vitamin D related diseases group and vitamins consumption",
                    overrideTableName = "noSolariumXiTestVITDVitamins",
                    widthProportion = 0.5, heightProportion = 0.07)     
    
    # ---- Lean Fish
    nonSolariumOnlyTable2 = deleteCategory(nonSolariumOnlyTable, leanFishIndex, "Didn't Answered")
    xiResults = categoricalXiV2(nonSolariumOnlyTable2, leanFishIndex ,diseaseGroupIndex)
    
    writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                    tableCaption = "Xi-square test for vitamin D related diseases group and lean fish consumption",
                    overrideTableName = "noSolariumXiTestVITDLeanFish",
                    widthProportion = 0.5, heightProportion = 0.07)                
    
    # ---- Fat Fish
    nonSolariumOnlyTable2 = deleteCategory(nonSolariumOnlyTable, fatFishIndex, "Didn't Answered")
    xiResults = categoricalXiV2(nonSolariumOnlyTable2, fatFishIndex ,diseaseGroupIndex)

    writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                    tableCaption = "Xi-square test for vitamin D related diseases group and fat fish consumption",
                    overrideTableName = "noSolariumXiTestVITDFatFish",
                    widthProportion = 0.5, heightProportion = 0.07)       
        
    
    
    # ---- Check by sex
    xiResults = categoricalXiV2(nonSolariumOnlyTable, sexIndex ,diseaseGroupIndex)
    
    writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                    tableCaption = "Xi-square test for vitamin D related diseases group and sex",
                    overrideTableName = "noSolariumXiTestVITDSex",
                    widthProportion = 0.5, heightProportion = 0.07)      
    
    

    # ---- Check by HS
    xiResults = categoricalXiV2(nonSolariumOnlyTable, highSchoolIndex ,diseaseGroupIndex)
    
    writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                    tableCaption = "Xi-square test for vitamin D related diseases group and sex",
                    overrideTableName = "noSolariumXiTestVITDSex",
                    widthProportion = 0.5, heightProportion = 0.07)       
    

        

    
}

# Let see carrier infection respect VD
{
    
    # For each of the carrier statuses
    relevantIndexes = c(nasalDirectCarrierIndex, nasalEnrichmentCarrierIndex,
                        throatDirectCarrierIndex, throatEnrichmentCarrierIndex,
                        carrierDirectIndex, carrierEnrichmentIndex)
    
    plotTitles = c("Boxplot for 25OHD levels with respect nasal colonized",
                   "Boxplot for 25OHD levels with respect nasal carrier",
                   "Boxplot for 25OHD levels with respect throat colonized",
                   "Boxplot for 25OHD levels with respect throat carrier",
                   "Boxplot for 25OHD levels with respect colonized",
                   "Boxplot for 25OHD levels with respect carrier")

    for(i in 1: length(relevantIndexes) ){
        
        currentIndex = relevantIndexes[i]
        
        # Clean the NA data
        nonNASolariumOnlyTable = deleteNA(nonSolariumOnlyTable, currentIndex)
        
        myPlot = doBoxPlotV2 (nonNASolariumOnlyTable, vitamimDIndex, VITAMIND_FOLDER,
                              groupIndex = relevantIndexes[i],
                              colorsVector = COLOR_VECTOR_CARRIER,
                              showPValues = TRUE,
                              significantPValue = 0.05,
                              cutOffLine = cutOffVitD,
                              ymax = 155,
                              plotTitle = plotTitles[i],
                              plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
                              plotYLabel = "25OHD (nmol/L)",
                              overrideImageWidth = 7)          
        
        
         latexFilePath = writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                                          captionText   = plotTitles[i],
                                          overrideLabel = paste0("fig:RelationshipVITDCarrier",i),
                                          pageHeight = 0.3, overrideFloat = TRUE)     
         
         print(latexFilePath)
        
    }
    
       
    
    
    
    
}


# Let see social interaction
{

    # The original data is a bit fuck up, lets fix some things
    completeTableTemp = completeTable
    # -- Get the ID that anyone who has a NA in the solarium
    whoIsNA = completeTableTemp[is.na(completeTableTemp[,solariumIndex]),]$ID
    # -- Delete everybody in the solarium variable with NAs
    overallEdgesFilterDF = overallEdgesDF[!deleteConnections(overallEdgesDF, whoIsNA, whoIsNA)[[1]],]
    completeTableTemp = deleteNA(completeTableTemp, solariumIndex)
    #
    nodesDF                  = completeTableTemp
    nodesDF[]                = lapply(nodesDF, as.character) # Need to transform integers ID to strings IDs for whatever reason o_O , why??
    # -- Create the new graph
    overallFilterGraph  = graph_from_data_frame(overallEdgesFilterDF,  vertices = nodesDF, directed = T)
    
    
    # If you go to the solarium does your friends go to the solarium also?
    {
        
        # Homophily tests
        partialHomophilyDF  = partialHomophilyV2(overallFilterGraph,  solariumIndex)
        completeHomophilyDF = completeHomophilyV2(overallFilterGraph, solariumIndex)
        
        # Bias test
        # First both men and women
        # ---- How many simulations do you want
        totalSimulations = 10
        # ---- Add the same to same relationships
        overallEdgesFilterDFTemp             = overallEdgesFilterDF
        overallEdgesFilterDFTemp$SameValue   = addEdgeRelationship(overallEdgesFilterDFTemp,  completeTableTemp, solariumIndex)
        listOfEdges = newList(1) # I need to put this into a list because the function is optimize for several networks simulations because R is crappy, I need to change all of that to make the code more legible and screw optimization because of bad language syntax; fuck R
        listOfEdges[[1]] = overallEdgesFilterDFTemp 
        # ---- Count how many we have
        totalSameRelationships = sum(overallEdgesFilterDFTemp$SameValue)
        totalRelationships     = nrow(overallEdgesFilterDFTemp)
        # ---- Check if there is bias friendship
        biasResult = doCategoricalBiasAnalysis(completeTableTemp, listOfEdges, solariumIndex, totalSimulations)
        #  Network Total Relationships Equal Relationships  MIN   Q1 Median  Average   Q3  MAX       SD SolariumLast4Weeks
        #1       1                3575                2561 2184 2319   2366 2364.968 2408 2626 62.08821       0.0007961479
        
        # Women only
        completeTableWomenOnlyTemp = deleteCategory(completeTableTemp, sexIndex, "Man")
        whoIsMan = completeTableTemp[(completeTableTemp[,sexIndex] == "Man"),]$ID
        overallEdgesWomenFilterDF = overallEdgesFilterDF[!deleteConnections(overallEdgesFilterDF, whoIsMan, whoIsMan)[[1]],]
        nodesDF                   = completeTableWomenOnlyTemp
        nodesDF[]                 = lapply(nodesDF, as.character)
        overallWomenFilterGraph   = graph_from_data_frame(overallEdgesWomenFilterDF,  vertices = nodesDF, directed = T)
        overallEdgesWomenFilterDFTemp            = overallEdgesWomenFilterDF
        overallEdgesWomenFilterDFTemp$SameValue  = addEdgeRelationship(overallEdgesWomenFilterDFTemp,  completeTableWomenOnlyTemp, solariumIndex)
        listOfEdges = newList(1) # I need to put this into a list because the function is optimize for several networks simulations because R is crappy, I need to change all of that to make the code more legible and screw optimization because of bad language syntax; fuck R
        listOfEdges[[1]] = overallEdgesWomenFilterDFTemp
        biasResult = doCategoricalBiasAnalysis(completeTableWomenOnlyTemp, listOfEdges, solariumIndex, 100)
        # Network Total Relationships Equal Relationships MIN  Q1 Median Average  Q3  MAX       SD SolariumLast4Weeks
        # 1       1                1557                1049 821 885    908   910.7 939 1022 40.01199       0.0002736565
        
        # Men only
        completeTableMenOnlyTemp = deleteCategory(completeTableTemp, sexIndex, "Woman")
        whoIsWoman = completeTableTemp[(completeTableTemp[,sexIndex] == "Woman"),]$ID
        overallEdgesMenFilterDF = overallEdgesFilterDF[!deleteConnections(overallEdgesFilterDF, whoIsWoman, whoIsWoman)[[1]],]
        nodesDF                 = completeTableMenOnlyTemp
        nodesDF[]               = lapply(nodesDF, as.character)
        overallMenFilterGraph   = graph_from_data_frame(overallEdgesMenFilterDF,  vertices = nodesDF, directed = T)
        overallEdgesMenFilterDFTemp            = overallEdgesMenFilterDF
        overallEdgesMenFilterDFTemp$SameValue  = addEdgeRelationship(overallEdgesMenFilterDFTemp,  completeTableMenOnlyTemp, solariumIndex)
        listOfEdges = newList(1) # I need to put this into a list because the function is optimize for several networks simulations because R is crappy, I need to change all of that to make the code more legible and screw optimization because of bad language syntax; fuck R
        listOfEdges[[1]] = overallEdgesMenFilterDFTemp
        biasResult = doCategoricalBiasAnalysis(completeTableMenOnlyTemp, listOfEdges, solariumIndex, 100)
        #   Network Total Relationships Equal Relationships MIN   Q1 Median Average   Q3  MAX       SD SolariumLast4Weeks
        #1       1                1453                1122 997 1064   1085 1093.04 1123 1188 40.11547          0.2351731
        
        
    }
    
    
    # If I have low vitamin D, how likely is that my friend has low vitamin D too
    # If I have high vitamin D, how likely is that my friend has high vitamin D too
    {
        
        # Divide the data into high and low vitamin D
        nonSolariumOnlyTableTemp = nonSolariumOnlyTable
        nonSolariumOnlyTableTemp = deleteNA(nonSolariumOnlyTableTemp, vitamimDIndex )
        nonSolariumOnlyTableTemp$Cat25OHD = "Low"
        nonSolariumOnlyTableTemp[nonSolariumOnlyTableTemp[,vitamimDIndex] > VITAMIND_MIN_LIMIT,]$Cat25OHD = "High"
        
        # Redo the network for only these people
        validIDs   = nonSolariumOnlyTableTemp$ID
        allIDs     = completeTable$ID
        invalidIDs = setdiff(allIDs, validIDs)
        overallEdgesFilterDF = overallEdgesDF[!deleteConnections(overallEdgesDF, invalidIDs, invalidIDs)[[1]],]
        totalPeople          = length(validIDs)
        totalRelationships   = nrow(overallEdgesDF)
        
        # Count how many friends each person has, and how many of those have high vitamin D
        totalHFriends = rep(0, totalPeople)
        totalFriends  = rep(0, totalPeople)        
        # ---- First supress every ID
        nonSolariumOnlyTableTemp$ID = c(1:totalPeople)
        for(i in 1:totalPeople){
        
            overallEdgesFilterDF = swapRelationshipsIDs(overallEdgesFilterDF, validIDs[i], i) # Since IDs are sorted, is impossible to hit old ones
            
        }
        currentFriendshipMatrix = getFriendshipMatrix(overallEdgesFilterDF, totalPeople)
        
        # ---- Start counting
        for(i in 1:totalPeople){
                
            # Supress the ID
            currentID = i
    
            # Find the friends surrounding you (you nominate or nominate you)
            currentFriends = unique(c(getFrienshipTypes(currentID, currentFriendshipMatrix)[[4]],
                                      getFrienshipTypes(currentID, currentFriendshipMatrix)[[5]]))
    
            currentTotalFriends = length(currentFriends) 
            totalFriends[i] = currentTotalFriends
        
            # Find how many of those are high vitamin D if you have more than 0 friends
            if(currentTotalFriends > 0){
                for(j in 1:currentTotalFriends){
        
                    # Friend ID
                    currentFriendID = currentFriends[j]
        
                    # Status of the friend
                    friendHStatus = as.character(nonSolariumOnlyTableTemp$Cat25OHD[currentFriendID])
                    if(friendHStatus == "High") totalHFriends[i] = totalHFriends[i] + 1
                
                }
            }            
            
        }
        
            
        # Grab only people who has friends
        onlyPeopleWithFriendsTotalH  = totalHFriends[totalFriends > 0]
        onlyPeopleWithFriendsHStatus = nonSolariumOnlyTableTemp[totalFriends > 0,]$Cat25OHD
  
        # Make the numbers for the model
        onlyPeopleWithFriendsHStatusNumerical = rep(0, length(onlyPeopleWithFriendsHStatus))
        onlyPeopleWithFriendsHStatusNumerical[onlyPeopleWithFriendsHStatus == "High"] = 1
        HModel = glm(onlyPeopleWithFriendsHStatusNumerical ~ onlyPeopleWithFriendsTotalH, family=binomial(link="logit"))
  
        # Prepare the dataframes for the plots
        logisticsPlotsDF           = data.frame(matrix(NA, nrow = length(onlyPeopleWithFriendsTotalH), ncol = 3))
        colnames(logisticsPlotsDF) = c("H_Status", "TotalFriendsH", "H_Numerical")
        logisticsPlotsDF[,1]       = onlyPeopleWithFriendsHStatus
        logisticsPlotsDF[,2]       = onlyPeopleWithFriendsTotalH
        logisticsPlotsDF[,3]       = onlyPeopleWithFriendsHStatusNumerical
  
        # This plot is ugly and useless, but the analysis tells us that there is a difference
        myPlot = doBoxPlotV2 (logisticsPlotsDF, 2, VITAMIND_FOLDER, groupIndex = 1)  
        
        # Prepare the exponential functions
        myHfunction = function(x){
            return(1/(1 + exp(-(0.39*x-1.39) ) ) )
        }
        
        # Find the values for the manual boxplots
        {
            
            HNegatives = summary(logisticsPlotsDF[logisticsPlotsDF$H_Status == "Low",]$TotalFriendsH)
            HPositives = summary(logisticsPlotsDF[logisticsPlotsDF$H_Status == "High",]$TotalFriendsH)

            # Boxplot bottom, the 0
            HNegativeMin = as.numeric(HNegatives[1])
            HNegativeMax = as.numeric(HNegatives[6])
            HNegative1   = as.numeric(HNegatives[2])
            HNegative2   = as.numeric(HNegatives[3])
            HNegative3   = as.numeric(HNegatives[5])
            HNegativeA   = as.numeric(HNegatives[4])

            # Boxplot on top, the 1
            HPositiveMin = as.numeric(HPositives[1])
            HPositiveMax = as.numeric(HPositives[6])
            HPositive1   = as.numeric(HPositives[2])
            HPositive2   = as.numeric(HPositives[3])
            HPositive3   = as.numeric(HPositives[5])
            HPositiveA   = as.numeric(HPositives[4])            
            
        }
        
        myPlot = ggplot() +
            # Jitter points
            geom_jitter(data = logisticsPlotsDF, aes(x=TotalFriendsH,y=H_Numerical),  width = 0.25, height = 0.025, color = "#e88e2e", alpha = 0.2) +
            # Horizontal lines
            geom_hline(yintercept=0) +
            geom_hline(yintercept=1) +
            
            # Formulas
            geom_function(fun = myHfunction, colour = "#e88e2e", lwd = 1, linetype = 1) +


            # -- E Negatives
            geom_segment(aes(x = HNegativeMin, y = -0.09, xend = HNegative1,   yend = -0.09), colour = "black") + 
            geom_segment(aes(x = HNegative3,   y = -0.09, xend = 6, yend = -0.09), colour = "black") + 
            geom_rect(aes(xmin=HNegative1, xmax=HNegative3, ymin=-0.07, ymax=-0.11), fill="#e88e2e", color="black", alpha=0.5) +
            geom_segment(aes(x = HNegativeA,   y = -0.07, xend = HNegativeA,   yend = -0.11), colour = "black") +

            # -- E Positives
            geom_segment(aes(x = HPositiveMin, y = +1.15, xend = HPositive1,   yend = +1.15), colour = "black") + 
            geom_segment(aes(x = HPositive3,   y = +1.15, xend = 6, yend = +1.15), colour = "black") + 
            geom_rect(aes(xmin=HPositive1, xmax=HPositive3, ymin=+1.13, ymax=+1.17), fill="#e88e2e", color="black", alpha=0.5) +
            geom_segment(aes(x = HPositiveA,   y = +1.13, xend = HPositiveA,   yend = +1.17), colour = "black") +
        
        
            # Break in integers numbers only
            scale_x_continuous( breaks = seq(0,6,1),   limits=c(-0.5, 6.5))    +
            scale_y_continuous( breaks = seq(0,1,0.2), limits=c(-0.17, 1.17))  +
        
            # Labels
            labs(x = "Total friends with high vitamin D (>50 n/mol)",
                 y = "Probability of having high vitamin D",
                 title = "Logit Regression between total high vitamin D friends and probability of high vitamin D status",
                 subtitle = "Only people with friends are shown, p-value < 0.0001") + 
        
            theme_linedraw()
        
            imageFilePath = paste0(VITAMIND_FOLDER, "LogisticVD.png")
            ggsave(imageFilePath, myPlot,
                       width = 8, height = 8)
        
            writeImageLATEX2(imageFilePath, VITAMIND_FOLDER, 
                             captionText   = "Relationship between number of friends with high vitamin D and probability of having high vitamin D",
                             overrideLabel = "fig:vitDpoweroffriendship",
                             pageHeight = 0.3, overrideFloat = TRUE)    
            
            
            ccc = myHfunction(seq(10,0,-1))

            averageC = 0
            for(i in 1:(length(ccc)-1)){
    
                difference = ccc[i] - ccc[i+1] 
                averageC = averageC + difference
    
            }
            averageC = averageC / (length(ccc)-1)
  
  
            # Find CI for increase probability by numbers of friends
            {
    
                ccc2 = ccc
                for(i in 1:(length(ccc)-1)){

                    ccc2[i] = ccc[i] - ccc[i+1] 
            
                }
                ccc2 = ccc2[1:length(ccc2)-1]
        
                getCI(ccc2)
        
            }            
        
    }
        
    
    # Same in function
    

        # Get the data ready
        #  -- Divide the data into high and low vitamin D
        nonSolariumOnlyTableTemp = nonSolariumOnlyTable
        nonSolariumOnlyTableTemp = deleteNA(nonSolariumOnlyTableTemp, vitamimDIndex )
        nonSolariumOnlyTableTemp$Cat25OHD = "Low"
        nonSolariumOnlyTableTemp[nonSolariumOnlyTableTemp[,vitamimDIndex] > VITAMIND_MIN_LIMIT,]$Cat25OHD = "High"
        
        # Redo the network for only these people
        # -- Get the good and bad IDs
        validIDs    = nonSolariumOnlyTableTemp$ID
        allIDs      = completeTable$ID
        invalidIDs  = setdiff(allIDs, validIDs)
        totalPeople = length(validIDs)
        # -- Delete the bad connections
        overallEdgesFilterDF = overallEdgesDF[!deleteConnections(overallEdgesDF, invalidIDs, invalidIDs)[[1]],]
        # -- Update the new IDs
        nonSolariumOnlyTableTemp$ID = c(1:totalPeople)
        for(i in 1:totalPeople){
        
            overallEdgesFilterDF = swapRelationshipsIDs(overallEdgesFilterDF, validIDs[i], i) # Since IDs are sorted, is impossible to hit old ones
            
        }
        currentFriendshipMatrix = getFriendshipMatrix(overallEdgesFilterDF, totalPeople) 

        #myLogitData = prepareLogitData(nonSolariumOnlyTableTemp, 546, "High", currentFriendshipMatrix)
        #aaa = doLogitAnalysis(myLogitData, 1, 2)

        myPlot = doBinaryLogitPlot(nonSolariumOnlyTableTemp, 546, "High", currentFriendshipMatrix, VITAMIND_FOLDER,
                          plotTitle    = "Logit Regression between total high vitamin D friends and probability of high vitamin D status",
                          plotSubtitle = "Only people with friends are shown, p-value < 0.0001" , 
                          plotXLabel   = "Total friends with high vitamin D (>50 n/mol)",
                          plotYLabel   = "Probability of having high vitamin D")       
    
        writeImageLATEX2(myPlot[[3]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between number of friends with high vitamin D and probability of having high vitamin D",
                         overrideLabel = "fig:vitDpoweroffriendship",
                         pageHeight = 0.3, overrideFloat = TRUE)           
        
    
    # Let see if highschool have some association
    {
    
        # Divide the data into high and low vitamin D
        nonNASolariumOnlyTable = completeTable
        nonNASolariumOnlyTable = deleteNA(nonNASolariumOnlyTable, solariumIndex)
        nonNASolariumOnlyTable = deleteCategory(nonNASolariumOnlyTable, solariumIndex, "Yes")
        nonNASolariumOnlyTable = deleteNA(nonNASolariumOnlyTable, vitamimDIndex )
        nonNASolariumOnlyTable$Cat25OHD = "Low"
        nonNASolariumOnlyTable[nonNASolariumOnlyTable[,vitamimDIndex] > VITAMIND_MIN_LIMIT,]$Cat25OHD = "High"
        
        xiResults = categoricalXiV2(nonNASolariumOnlyTable, highSchoolIndex ,ncol(nonNASolariumOnlyTable))
        
            
        writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                        tableCaption = "Xi-square test for high or low vitamin D (>50nmol) with respect highschool",
                        overrideTableName = "noSolariumXiHS",
                        widthProportion = 0.5, heightProportion = 0.14)      
        
        myPlot = doBoxPlotV2 (nonNASolariumOnlyTable, vitamimDIndex, VITAMIND_FOLDER,
                              groupIndex = highSchoolIndex,
                          
                              showPValues = TRUE,
                              significantPValue = 0.05,
            
                              cutOffLine = cutOffVitD,
                              ymax = 155,
                              plotTitle = "25OHD levels with respect highschool",
                              plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
                              plotYLabel = "25OHD (nmol/L)",
                              overrideImageWidth = 7)        
            
         writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                          captionText   = "Vitamin D levels with respect highschool",
                          overrideLabel = "fig:RelationshipVITDHS",
                          pageHeight = 0.6, overrideFloat = TRUE)   
        
         
         
         xiResults = categoricalXiV2(womenOnlyTable, highSchoolIndex ,hormonalTypeIndex)
         
         
         
        myPlot = doBoxPlotV2 (completeTable, vitamimDIndex, VITAMIND_FOLDER,
                              groupIndex = highSchoolIndex,
                          
                              showPValues = TRUE,
                              significantPValue = 0.05,
            
                              cutOffLine = cutOffVitD,
                              ymax = 155,
                              plotTitle = "25OHD levels with respect highschool",
                              plotSubtitle = "Showing everybody, solarium included",
                              plotYLabel = "25OHD (nmol/L)",
                              overrideImageWidth = 7)     
         
         
    }
    
    # HS has a very high association, so let check again for each HS
    {
    
        myListOfPlotsObjects      = newList(8)

        # Get the colors that you are going to use
        myPalette = colorRampPalette(brewer.pal(8, "Spectral"))
        HSColors  = myPalette(8)
        
        # Get the data ready
        #  -- Divide the data into high and low vitamin D
        nonSolariumOnlyTableTemp = nonSolariumOnlyTable
        nonSolariumOnlyTableTemp = deleteNA(nonSolariumOnlyTableTemp, vitamimDIndex )
        nonSolariumOnlyTableTemp$Cat25OHD = "Low"
        nonSolariumOnlyTableTemp[nonSolariumOnlyTableTemp[,vitamimDIndex] > VITAMIND_MIN_LIMIT,]$Cat25OHD = "High"
        
        # For each HS
        myHighschools = getCategories(completeTable, highSchoolIndex)
        for( j in 1:length(myHighschools)){
        
            # Get the HS
            currentHighschool = myHighschools[j]
                
            # Repeat everything again for this HS
            nonSolariumOnlyTableTempHS = keepCategory(nonSolariumOnlyTableTemp, highSchoolIndex, currentHighschool)

            # -- Get the good and bad IDs
            validIDs    = nonSolariumOnlyTableTempHS$ID
            allIDs      = completeTable$ID
            invalidIDs  = setdiff(allIDs, validIDs)
            totalPeople = length(validIDs)
            # -- Delete the bad connections
            overallEdgesFilterDF = overallEdgesDF[!deleteConnections(overallEdgesDF, invalidIDs, invalidIDs)[[1]],]
            # -- Update the new IDs
            nonSolariumOnlyTableTempHS$ID = c(1:totalPeople)
            for(i in 1:totalPeople){
        
                overallEdgesFilterDF = swapRelationshipsIDs(overallEdgesFilterDF, validIDs[i], i) # Since IDs are sorted, is impossible to hit old ones
            
            }
            currentFriendshipMatrix = getFriendshipMatrix(overallEdgesFilterDF, totalPeople) 

            # This is very badly optimized, but whatever, is not like R is a turbo ferrari anyway
            myPlot = doBinaryLogitPlot(nonSolariumOnlyTableTempHS, 546, "High", currentFriendshipMatrix, VITAMIND_FOLDER,
                          myColor = HSColors[j],
                          plotTitle    = paste0( currentHighschool, " and logit for vitamin D status" ),
                          plotSubtitle = "" , 
                          plotXLabel   = "",
                          plotYLabel   = "",
                          overrideTableName = paste0("Logit",currentHighschool))                   
            
            myPlot = doBinaryLogitPlot(nonSolariumOnlyTableTempHS, 546, "High", currentFriendshipMatrix, VITAMIND_FOLDER,
                          myColor = HSColors[j],
                          plotTitle    = paste0( currentHighschool, " and logit for vitamin D status" ),
                          plotSubtitle = paste0( "p-value = ",myPlot[[4]]) , 
                          plotXLabel   = "",
                          plotYLabel   = "",
                          overrideTableName = paste0("Logit",currentHighschool))       
    
            latexPath = writeImageLATEX2(myPlot[[3]], VITAMIND_FOLDER, 
                               captionText   = "Relationship between number of friends with high vitamin D and probability of having high vitamin D",
                               overrideLabel = "fig:vitDpoweroffriendship",
                               pageHeight = 0.3, overrideFloat = TRUE)  
            
            myListOfPlotsObjects[[j]] = myPlot[[1]]

        }
      
                
        ggarrange(plotlist =  myListOfPlotsObjects , ncol = 2, nrow = 4)
        
        allGraphPath  = file.path(paste(VITAMIND_FOLDER, "4x2HSLogit.png", sep = ""))
        ggsave(allGraphPath, width = 8, height = 16)
        
        latexPlot = writeImageLATEX2(allGraphPath, VITAMIND_FOLDER, 
                                     captionText   = "Logit for each high-school",
                                     overrideLabel = "fig:RelationshipDHSAll",
                                     pageHeight = 1.0, overrideFloat = TRUE)
        
        
        
        
        # LOESS by HS
        myPlot = doTimeSeriesPlot(nonSolariumOnlyTableTemp, bloodExtractionDateIndex, vitamimDIndex, VITAMIND_FOLDER,
                                  groupingIndex = highSchoolIndex,
                                  horizontalLinesCuts = cutOffVitD,
                                  plotTitle    = "LOESS regression between 25OHD and time of blood extraction",
                                  plotSubtitle = "Grouped by Highschool",
                                  plotXLabel   = "Date",
                                  plotYLabel   = "25OHD (nmol/L)",
                                  plotTheme    = "regression",
                                  ymax         = 80,
                                  overrideImageWidth = 12)        
        
        
        writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                         captionText   = "Relationship between vitamin D and date of the extraction",
                         overrideLabel = "fig:RelationshipSEXDATEVITD",
                         pageHeight = 0.3, overrideFloat = TRUE)
        
              
    }
    
    # Blood extraction by HS and date
    {
    
        # Create the date plot with the date references
        {

            # Find the minimum and maximum date from all the dates
        
            # R is horrible and here is another reason to hate it. Turn out that
            # the friendship creation date starts at 2010-09-20. This date is correct,
            # however it lacks the timezone information (ie UTC). The rest of the dates
            # do have the timezone information. When you do the min() of a no timezone
            # date, and a timezone date, R say "fuck-you" and gives you back POSIX at
            # 1970-01-01. :/ , so I don't include the friendship creation minimum
            # since it is the same at the other minimum anyway.
            minimumDate = min(bloodTable$BloodAnalysisDate, na.rm=TRUE)
            maximumDate = max(bloodTable$BloodAnalysisDate, na.rm=TRUE)
    
            totalWeeks    = week(maximumDate) + week(as.Date("2010-12-31")) - week(minimumDate) + 1
            myHighschools = getCategories(completeTable, highSchoolIndex)
            totalHS       = length(myHighschools)

            # This dataframe represent each of the weeks from 2010 and 2011
            timelineDF           = data.frame(matrix(0, nrow = totalWeeks * totalHS, ncol = 6))
            colnames(timelineDF) = c("Week", "Year", "Total", "HS", "y", "x")

            # Init the week , year and concept in the timelineDF
            currentWeek    = week(minimumDate)
            currentYear    = year(minimumDate)
            targetWeek     = week(as.Date("2010-12-31"))
    
            currentIndex   = 1
            currentXWeek   = 1
    
            # Get the highschools
            for(i in 1:totalWeeks){
    
                for(j in 1:totalHS){
        
                    # Get current HS
                    currentHS = myHighschools[j]
                    
                    # Write the current indexes
                    timelineDF$Week[currentIndex] = currentWeek
                    timelineDF$Year[currentIndex] = currentYear
                    timelineDF$HS[currentIndex]   = currentHS
            
                    # Update the high and wide x,y variable for plotting
                    timelineDF$x[currentIndex] = currentXWeek
                    timelineDF$y[currentIndex] = j

                    currentIndex   = currentIndex + 1
                    
                }
        
                # Update week and year
                currentXWeek = currentXWeek + 1
                currentWeek  = currentWeek  + 1
        
                # Reset the week if we go too far
                if(currentWeek == 54){
                    currentWeek = 1
                    currentYear = currentYear + 1
                }
        
            }
        
            # Now you just need to go throw each date and increase the proper counter
            for(i in 1:nrow(completeTable)){
        
                # Get the date
                currentDate = completeTable$BloodAnalysisDate[i]
            
                # If it is not an NA date
                if(!is.na(currentDate)){
                
                    # Get the week and year
                    currentWeek = week(currentDate)
                    currentYear = year(currentDate)            
            
                    # Get the concept
                    currentConcept = as.character(completeTable$HighSchool[i])
            
                    # Search for this combination and increase the total+1
                    relevantIndex = as.numeric(rownames(timelineDF[timelineDF$Week == currentWeek & timelineDF$Year == currentYear & timelineDF$HS == currentConcept,]))
                    timelineDF$Total[relevantIndex] = timelineDF$Total[relevantIndex] + 1

            }
        

            
        }
            
    }
     
        # Everything is initialize now
    
        # Create a variable with a date string so it get sorted properly
        timelineDF$Date_String = paste0(timelineDF$Year,"_",timelineDF$Week)
        timelineDF$Date_String = paste0(timelineDF$Year,timelineDF$Week)


        #setting customized first monday of the year
        timelineDF$Actual_Monday <- floor_date( make_date(timelineDF$Year), unit = "week", week_start = 1)

        #date (first monday of the week)
        timelineDF$Actual_Date <- timelineDF$Actual_Monday + dweeks(timelineDF$Week - 1)        
        
        
        
        # Actual lines rotated (this one is fine)
        # This is for all highschools.
        myHPlot = ggplot( timelineDF, aes(x=Actual_Date, y=Total, fill=HS)) + 
        	              geom_line(color = "black", size = 2)+
                          geom_line(aes(color=HS), size = 1)+
                          scale_color_brewer(palette="Spectral") +
                          #scale_y_continuous(sec.axis=  sec_axis(~ . + 10, name = derive()) ) +
        	              scale_y_continuous( ) +
                          scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
                          facet_grid(HS~., switch = "y")+
                          theme(strip.text.y.left = element_text(angle = 0),
                                legend.position="none",
                          	    panel.background = element_rect(fill = "grey90")) +
                          labs(title = "Time of the year for blood extraction for each high-school",
                          	   x="Date",
                          	   y="Total blood samples"
                          	   )

        currentFilename = paste0(VITAMIND_FOLDER,"allHighschools_dates.png")
        ggsave(plot = myHPlot, filename = currentFilename)
        
        
        writeImageLATEX2(currentFilename, VITAMIND_FOLDER, 
                         captionText   = "Total blood extractions per highschool across time of the year for each high-school.",
                         overrideLabel = "fig:dateBloodHS",
                         pageHeight    = 0.4, overrideFloat = TRUE)           
            
    }
    
    
    # Diet doesn't show correlation for vit D, but let check if it is spoil by date.
    # So let checks for HS stratificiation
    {
        
        # Prepare a new folder
        currentFolder = paste0(VITAMIND_FOLDER, "DietByHighschool/")
        dir.create(currentFolder)
        
        myListOfPlotsObjects     = newList( length(myHighschools) * length(vitaminDIndexes) )
        plotIndex                = 1
        
        # Get the data ready
        nonSolariumOnlyTableTemp = completeTable
        nonSolariumOnlyTableTemp = deleteNA(nonSolariumOnlyTableTemp, solariumIndex)
        nonSolariumOnlyTableTemp = deleteCategory(nonSolariumOnlyTableTemp, solariumIndex, "Yes")
        nonSolariumOnlyTableTemp = deleteNA(nonSolariumOnlyTableTemp, vitamimDIndex )
        
        # For each HS
        myHighschools = getCategories(completeTable, highSchoolIndex)
        for( j in 1:length(myHighschools)){
        
            # Get the HS
            currentHighschool = myHighschools[j]
            # Filter data for people in that HS only
            currentHSDataDF   = keepCategory(nonSolariumOnlyTableTemp, highSchoolIndex, currentHighschool)

            # For each dietary data source 
            for(i in 1:length(vitaminDIndexes)){

                # Get the index and name of the diet concept
                currentDietIndex = vitaminDIndexes[i]
                currentDietName  = colnames(completeTable)[currentDietIndex]

                # Filter people who didn't awnserd the question
                currentDietDataDF = deleteCategory(currentHSDataDF, currentDietIndex, "Didn't Answered")

                # Count how many categories are left, delete those that have only 1 if any
                currentSummary = summarizeCategorical(currentDietDataDF, currentDietIndex)
                for(k in 1:nrow(currentSummary)){
                    
                    if(currentSummary[k,2] == 1){
                    
                        badCategoryName   = as.character(currentSummary[k,1])           # FUCKING FACTORS! I Hate you, ALL THE FUCKING TIME THE SAME SHIT!
                        currentDietDataDF = deleteCategory(currentDietDataDF, currentDietIndex, badCategoryName)
                        
                    }
                    
                }
                
                
                currentPlotTitle = paste0(currentDietName, " and ", currentHighschool)
                currentTableName = paste0(currentDietName, "_", currentHighschool)
                
                myPlot = doBoxPlotV2 (currentDietDataDF, vitamimDIndex, currentFolder,
                                      groupIndex = currentDietIndex,
                                      showPValues = TRUE,
                                      significantPValue = 0.2,
                                      pValuesFormat = "number4",
                                      cutOffLine = cutOffVitD,
                                      plotTheme = "simple",
                                      ymax = 155,
                                      plotTitle = currentPlotTitle,
                                      plotSubtitle = "",
                                      plotXLabel = "",
                                      plotYLabel = "",
                                      overrideTableName = currentTableName,
                                      overrideImageWidth = 7)  
                
                # Add it to the list of plots for later
                myListOfPlotsObjects[[plotIndex]]   = myPlot[[1]]
                plotIndex = plotIndex + 1

            }
         
        }
     
        # Transpose the list of plots
        {
            
            originalDF = DF(length(myHighschools),  length(vitaminDIndexes))
            originalIndex = 1
            for( j in 1:length(myHighschools)){
            
                for(i in 1:length(vitaminDIndexes)){
                    
                    #print(j + (i-1)*length(vitaminDIndexes))
                    #print( paste(j,i) )
                    
                    #originalDF[j,i] =  j + (i-1)*length(vitaminDIndexes)
                    originalDF[j,i] = originalIndex
                    originalIndex   = originalIndex + 1
                    
                }    
            }
            transposeDF = t(originalDF)

            myListOfPlotsObjectsT    = newList( length(myHighschools) * length(vitaminDIndexes) )
            plotIndexT               = 1
            
            for(i in 1:length(vitaminDIndexes)){
            
                for( j in 1:length(myHighschools)){
            
                    myListOfPlotsObjectsT[[plotIndexT]] = myListOfPlotsObjects[[ transposeDF[i,j] ]]
                    plotIndexT = plotIndexT + 1
                    
                }
                
            }
            
        }
        
        ggarrange(plotlist =  myListOfPlotsObjectsT , nrow = length(vitaminDIndexes), ncol = length(myHighschools))
        
        allGraphPath  = file.path(paste(currentFolder, "6x8HDiets.png", sep = ""))
        ggsave(allGraphPath, width = 32, height = 32)
        
        latexPlot = writeImageLATEX2(allGraphPath, currentFolder, 
                                 captionText   = "Comparison of each diet in each highshool for 25OHD level",
                                 overrideLabel = "fig:allDietsAllHS",
                                 pageHeight = 1.0, overrideFloat = TRUE)        
            
    }
        
    # Does your friends influence your diet?
    {
        
        influenceDietDF = DF(length(vitaminDIndexes), 11)
        colnames(influenceDietDF) = c("DietConcept", "Total Relationships", "Equal Relationships", "MIN", "Q1", "Median", "Average", "Q3", "MAX", "SD", "p-value")
        
        totalSimulations = 100
        
        completeTableCopy = completeTable
        
        # For each diet source
        
        for(i in 1:length(vitaminDIndexes)){
            
            # Get the index and name of the diet concept
            currentDietIndex = vitaminDIndexes[i]
            currentDietName  = colnames(completeTable)[currentDietIndex]

            # Filter people who didn't awnserd the question
            currentDietDataDF = deleteCategory(completeTableCopy, currentDietIndex, "Didn't Answered")
            
            # Check who is not in the data anymore
            missingIDs =completeTable$ID[ !(completeTable$ID %in% currentDietDataDF$ID)]
            # Delete those IDs from the edge list
            overallEdgesFilterDF = overallEdgesDF[!deleteConnections(overallEdgesDF, missingIDs, missingIDs)[[1]],]
            
            # Add the same to same relationships
            overallEdgesFilterDF$SameValue   = addEdgeRelationship(overallEdgesFilterDF,  currentDietDataDF, currentDietIndex)
            listOfEdges = newList(1) # I need to put this into a list because the function is optimize for several networks simulations because R is crappy, I need to change all of that to make the code more legible and screw optimization because of bad language syntax; fuck R
            listOfEdges[[1]] = overallEdgesFilterDF
            # ---- Count how many we have
            totalSameRelationships = sum(overallEdgesFilterDF$SameValue)
            totalRelationships     = nrow(overallEdgesFilterDF)
            # ---- Check if there is bias friendship
            biasResult = doCategoricalBiasAnalysis(currentDietDataDF, listOfEdges, currentDietIndex, totalSimulations)
            #  Network Total Relationships Equal Relationships  MIN   Q1 Median  Average   Q3  MAX       SD SolariumLast4Weeks
            #1       1                3575                2561 2184 2319   2366 2364.968 2408 2626 62.08821       0.0007961479            
            
            # Add it to the results
            influenceDietDF[i,]  = biasResult[[1]][1,]
            influenceDietDF[i,1] = currentDietName
            
        }

        influenceDietDF2 = influenceDietDF
        influenceDietDF2$SD = round(influenceDietDF2$SD, 2)
        influenceDietDF2$`p-value` = round(influenceDietDF2$`p-value`, 2)
        
        writeTableLATEX(influenceDietDF2, VITAMIND_FOLDER,
                        tableCaption = "Influence of friends towards same food frequency by vitamin D related foods",
                        overrideTableName = "influenceDieting100",
                        widthProportion = 0.9, heightProportion = 0.07)    
        
        
        # Stratify by highschool for xi2 test only
        {
            
            # Prepare a new folder
            currentFolder = paste0(VITAMIND_FOLDER, "DietByHighschool/")

            # For each dietary data source 
            for(i in 1:length(vitaminDIndexes)){

                # Get the index and name of the diet concept
                currentDietIndex = vitaminDIndexes[i]
                currentDietName  = colnames(completeTable)[currentDietIndex]

                # Filter people who didn't awnserd the question
                currentDietDataDF = deleteCategory(completeTable, currentDietIndex, "Didn't Answered")

                # Count how many categories are left, delete those that have only 1 if any
                currentSummary = summarizeCategorical(currentDietDataDF, currentDietIndex)
                for(k in 1:nrow(currentSummary)){
                    
                    if(currentSummary[k,2] == 1){
                    
                        badCategoryName   = as.character(currentSummary[k,1])           # FUCKING FACTORS! I Hate you, ALL THE FUCKING TIME THE SAME SHIT!
                        currentDietDataDF = deleteCategory(currentDietDataDF, currentDietIndex, badCategoryName)
                        
                    }
                    
                }

                xiResults = categoricalXiV2(currentDietDataDF, highSchoolIndex ,currentDietIndex)
                
                print(xiResults[[7]])

            }
            
        }
        
    }
          
}


# Blood extraction date is not the same as PTH date, what??
# Not really, mean difference is 0.1 days, so it doesn't matter
{

    mean(bloodTable$BloodAnalysisDate - bloodTable$PlasmaAnalysisDate, na.rm = TRUE)
        
    
}

# Check for etnicity, not very good data though
{

    # I hate R, the weird things you need to do to create a clean DF based on other columns
    ethnicDF            = DF(nrow(completeTable), 2)
    ethnicDF$Original   = sociologyTable$Ethnicity
    ethnicDF$VitD       = bloodTable$X25.OH.D_.nmol.L.
    ethnicDF$Solariun   = hygieneTable$SolariumLast4Weeks
    ethnicDF            = ethnicDF[,3:5]
    ethnicDF$Skin       = "Fair"
    ethnicDF$Highschool = completeTable$HighSchool
    
    # Change a lonely NA to Didn't Answer
    ethnicDF[is.na(ethnicDF$Original),1] = "Didn't Answer"
    
    # myPlot = doLongBarPlot(ethnicDF, 1, VITAMIND_FOLDER,
      #                     plotTitle = "Ethnicity background", plotSubtitle = NULL,
       #                    plotXLabel = NULL, plotYLabel = NULL,
        #                   barsFontSize = 2,
         #                  crop = 0, top = 0, sort = "descending",
          #                 overrideHeigh = 25)  
    
    myPlot = doBarPlotV2(ethnicDF, 1, VITAMIND_FOLDER,
                         rotation     = TRUE,        
                         sort         = "ascending",
                         plotTitle    = "Ethnicity background",
    	                 overrideImageHeigh = 25)
    
    
    writeImageLATEX2(myPlot[[2]], VITAMIND_FOLDER, 
                     captionText   = "Absolute frequency for each ethnicity sorted by frequency",
                     overrideLabel = "fig:EthnicityCounts",
                     pageHeight = 0.9, overrideFloat = TRUE)  
    
    
    summaryEthnit     = summarizeCategorical(ethnicDF, 1)
    summaryEthnit[,3] = round(summaryEthnit[,3],3)
    summaryEthnit[,4] = round(summaryEthnit[,4],3)
    
    writeTableLATEX(summaryEthnit, VITAMIND_FOLDER,
                    tableCaption = "Summary of the different ethnicities",
                    overrideTableName = "allEthnicities",
                    widthProportion = 0.7, heightProportion = 0.5)      
    
    # Delete people who didn't awnser from the ethnic
    # Find skin color
    ethnicDF = deleteCategory(ethnicDF, 1, "Didn't Answer")
    
    for(i in 1:nrow(ethnicDF)){
        
        currentEthnicity = ethnicDF[i,1]
        
        if(currentEthnicity == "Norwegian-Somalian")  ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Afghan")              ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "African")             ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Thai")                ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Erithrean")           ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Turquish")  ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Thai")      ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Thamil")    ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Philipine") ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-African")   ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Gambian")   ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Ghanaian")  ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Brasilian") ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Colombian") ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Norwegian-Other")     ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Palestinian")         ethnicDF[i,4] = "Dark"
        if(currentEthnicity == "Tamil")               ethnicDF[i,4] = "Dark"

    }
    
    # Check spread in highschools
    xiResults = categoricalXiV2(ethnicDF, 5 ,4)
    summaryEthnit     = summarizeCategorical(ethnicDF, 4)

    writeTableLATEX(xiResults[[7]], VITAMIND_FOLDER,
                    tableCaption = "Xi-square test skin group and high-school",
                    overrideTableName = "noSolariumXiTestSkinHS",
                    widthProportion = 0.5, heightProportion = 0.15)  
    
    
    
        # Everythin that is not "No" from solarium
    # NA values for vitD
    
    
    
    
    ethnicDF = keepCategory(ethnicDF,   3, "No")
    ethnicDF = deleteNA(ethnicDF, 2)

    
    myPlot = doBoxPlotV2 (ethnicDF, 2, VITAMIND_FOLDER,
                          groupIndex = 4,
                          colorsVector = rev(COLOR_VECTOR_SKIN),
                          showPValues = TRUE,
                          pValuesFormat = "number2",
                          cutOffLine = cutOffVitD,
                          ymax = 155,
                          plotTitle = "25OHD levels with respect skin group",
                          plotSubtitle = "Only analyzing peoplpe not going to the solarium in the last 4 week",
                          plotYLabel = "25OHD (nmol/L)",
                          overrideImageWidth = 7)    
    
    writeBoxPlotCompositeLATEX ( myPlot[[2]], myPlot[[4]], myPlot[[5]], VITAMIND_FOLDER,
                                 captionText = "Relationship between vitamin D and skin group",
                                 minipageTop = 0.4,  minipageBottom = 0.5 ,         
                                 pageWidth = 1, pageHeight = 0.5, keepRatio = TRUE,
                                 topTableWidth    = 0.4, topTableHeight    = 0.05,
                                 bottomTableWidth = 0.4, bottomTableHeight = 0.05
                                )    
    
    
    
}


# Homophily for each highschool 
# How good is friendship in each high-school
{

    
    # Homophily for all
    homoResults = completeHomophilyV3(overallGraph, completeTable, highSchoolIndex)
    # ---- Add a column for the overall grade
    # ---- Delete the Delta, sign, difference, and p-value as there are not needed
    homoResults[,c(4,5,6,7)] = NULL
    homoResults$Overview     = 0
    
    homoResults$Overview[1] = mean(completeTable$Overview, na.rm = TRUE)
    
    
    
    # For each HS
    myHighschools        = getCategories(completeTable, highSchoolIndex)
    myListOfPlotsObjects = newList( length(myHighschools))
    
    for(i in 1:length(myHighschools)){
    
            currentHighschool = myHighschools[i]
            currentSubtable   = keepCategory(completeTable, highSchoolIndex, currentHighschool)
        
            currentPlotTitle      = paste(currentHighschool, " network overview")
            currentTableOverride  = paste(currentHighschool, "_overview")
            
            myListOfPlotsObjects[[i]] = doHistogramPlot2(currentSubtable, overviewIndex, VITAMIND_FOLDER,
                                                          colorsVector   = HIGHSCHOOL_COLOR_VECTOR[i],
                                                          binsWidth      = 1,
                                                          binBorderColor = "#333333",
                                                          plotTitle      = currentPlotTitle,
                                                          plotSubtitle   = " 0 = Low, 10 = High",
                                                          plotXLabel     = "Points", plotYLabel = "Total",
                                                          overrideTableName = currentTableOverride)[[1]]
        
            homoResults$Overview[1+i] = mean(currentSubtable[,overviewIndex], na.rm = TRUE)
            
    }
        
    ggarrange(plotlist =  myListOfPlotsObjects , nrow = 4, ncol = 2)
        
    allGraphPath  = file.path(paste(VITAMIND_FOLDER, "4x2HistoFriends.png", sep = ""))
    ggsave(allGraphPath, width = 8, height = 16)

    latexPlot = writeImageLATEX2(allGraphPath, VITAMIND_FOLDER, 
                                 captionText   = "Comparison of each highshool network representation",
                                 overrideLabel = "fig:allOverviewAllHS",
                                 pageHeight = 1.0, overrideFloat = TRUE)      
    
    
    # Round the numbers in the homophily table
    homoResults[,2]  = round(homoResults[,2],2)
    homoResults[,3]  = round(homoResults[,3],2)
    homoResults[,5]  = round(homoResults[,5],2)
    # Delete the weird NAs
    homoResults[1,3] = ""
    homoResults[1,4] = ""
    
    writeTableLATEX(homoResults, VITAMIND_FOLDER,
                    tableCaption = "Highschools and friendship overview",
                    overrideTableName = "HSHomophily",
                    widthProportion = 0.7, heightProportion = 0.2)          
    
}


# Prepare the data to do a Xi2 here (actually, you can put the xi2 info inside the plots!)
# Delete the unknowns for better viewing

# We need to change these plots to be amount of category independent, as in
# of all women, how many eat rarely, how many eat 6 per week and so on.
# and display the men information at the same time

# Put the Xi2 in the subtitle (in other color if possible)

doBarRelativeCombinePlot(completeTable, leanFishIndex, sexIndex, VITAMIND_FOLDER,
                         colorsVector = NULL,
                         plotTitle = NULL, plotSubtitle = NULL,
                         plotXLabel = NULL, plotYLabel = NULL)


doBarRelativeCombinePlot(completeTable, sexIndex, leanFishIndex, VITAMIND_FOLDER,
                         colorsVector = COLOR_VECTOR_SEX,
                         plotTitle = NULL, plotSubtitle = NULL,
                         plotXLabel = NULL, plotYLabel = NULL)


doLongBarAbsoluteCombinePlot(completeTable, sexIndex, leanFishIndex, sexIndex,
                                         VITAMIND_FOLDER
                             , sort = "none")



for(i in firstDietIndex:(lastDietIndex+1)){

    
    doBarRelativeCombinePlot(completeTable, sexIndex, i, VITAMIND_FOLDER,
                             colorsVector = COLOR_VECTOR_SEX,
                             plotTitle = NULL, plotSubtitle = NULL,
                             plotXLabel = NULL, plotYLabel = NULL)
    
    
}










# How is your general health with respect vitamin D levels
filteredNATable  = filterNAs(completeTable, healthIndex)
myBoxplotResults = doCategoricalBoxPlot (filteredNATable,
                                         healthIndex,
                                         vitamimDIndex,
                                         VITAMIND_FOLDER,
                                         colorsVector = COLOR_VECTOR_HEALTH,
                                         showPValues = FALSE)

# How many people have very bad self reported health
sum(completeTable$GeneralHealth == "Very bad", na.rm = TRUE)
# What are the VD levels of those people
subsetTable = filterModality(completeTable, healthIndex, "Very bad", type = "ON")
subsetTable[,vitamimDIndex]
    



# With respect physical exercises?

# With respect ethnic background

# SA infection with respect VD levels:

# SPA-types with respect vitamin, some are more prevalent



        # Same thing without regression analysis, since we don't have numbers anymore
        completeTableTemp = completeTable
        completeTableTemp = completeTableTemp[ completeTableTemp[,pthIndex] < 10 , ]
        myPlot = doLOESSRegressionPlot(completeTableTemp, pthIndex, vitamimDIndex, VITAMIND_FOLDER,
                                       horizontalLinesCuts = cutOffVitD,
                                       plotTitle = "Relationship between 25OHD and time of blood extraction",
                                       plotXLabel = "PTH",
                                       plotYLabel = "25OHD (nmol/L)",
                                       plotTheme = "regression",
                                       ymax = 160,
                                       overrideImageWidth = 12)        


        
# Christopher stepwise multiple regression analysis
{

	
	# Get the non solarium, and keep those with valid 25OHD only
	nonSolariumOnlyTable = completeTable[completeTable[,solariumIndex] == "No",]
	nonSolariumOnlyTable = nonSolariumOnlyTable[!is.na(nonSolariumOnlyTable$X25.OH.D_.nmol.L.),]
	variablesOfInterest  = c(vitamimDIndex, sexIndex, BMICatIndex, healthIndex, smokeIndex, snuffIndex, alcoholIndex, sportsIndex, screenIndex, sunbathingIndex, highSchoolIndex)
	
	# Make the table for the regression, R sucks at this, can't wait to dith this forever!
	regressionTable      = nonSolariumOnlyTable[,c(variablesOfInterest)]
	
	# Delete rows with NAs values
    regressionTable = regressionTable[complete.cases(regressionTable), ]
    
    # Delete unknown values
    rowsToDelete = rep(FALSE, nrow(regressionTable))
    
    for (i in 1:nrow(regressionTable)) {
    	# Check if the string is present in any cell of the row
    
    	for (j in 1:ncol(regressionTable)) {
    	
    		if(regressionTable[i,j] == "Unknown") rowsToDelete[i] = TRUE
    			
    	}
	}
    
    regressionTable = regressionTable[!rowsToDelete,]
    
    
	
	# Add ethnicity skin type (all whites, and stupid dummy function gives an
	# error for all the same instead of ignoring, fuck R)
	#ethnicDF$ID = 1
	#for(i in 1:nrow(ethnicDF)){
	#	ethnicDF$ID[i] = i
	#}
	#regressionTable$SkinType = ethnicDF[ethnicDF$ID %in% nonSolariumOnlyTable$ID, ]$Skin
	
	#nrow(regressionTable)
	
	
	# TRANSFORM THIS INTO DUMMIES, BECAUSE R IS DUMMY ITSELF AND HATE THE USER
	# THE USER NEEDS TO MICROMANAGE EVERYTHING!!!!!
	# NEVER AGAIN, FUCK THIS PROGRAM
	#
	# The worse thing is that this is done somewhere else, but I'm not going
	# to take the time to make it into a function because I never never never
	# EVER going to use R again. Fuck off. Piss off. Not putting any effort.
	# Migrating everything to Python and C.
	
	# Get the dimensions
    totalColumns = ncol(regressionTable)
    totalRows    = nrow(regressionTable)
      
    # First we need to figure it out which variables are categoricals and which one isn't
    categoricalDataVector = getCategoricalVariables(regressionTable)
        
    # Get a DF with only the categorical values if any, and transform it to dummy variables
    # Add it to the explicative DF and delete the old variables
    totalCategorical = sum(categoricalDataVector)
    if(totalCategorical > 0){
        
        # Create the dummy DF
        categoricalDF               = regressionTable[,categoricalDataVector]
        dummyExplicativeVariablesDF = fastDummies::dummy_cols(categoricalDF, remove_most_frequent_dummy = FALSE)
        dummyExplicativeVariablesDF = dummyExplicativeVariablesDF[,-c(1:totalCategorical)]

        # Add a single column to the explicative variables to ensure that we
        # don't delete the entire DF ( I hate you R ), when we delete the
        # categorical columns
        regressionTable$IHateYouR = 0
        
        # Delete the old categorical variables, whatever is left is only
        # numerical variables (if any)
        regressionTable = regressionTable[,!categoricalDataVector]
        
        # Add it to the original DF with all the variables
        regressionTable = cbind(regressionTable,dummyExplicativeVariablesDF)
      
        # Delete the extra column
        regressionTable$IHateYouR = NULL
        
    }
    
    
    
    
	# Intercept only model
	interceptOnlyModel = lm(X25.OH.D_.nmol.L. ~ 1, data=regressionTable)
	summary(interceptOnlyModel)
	
	# All  model
	allModels = lm(X25.OH.D_.nmol.L. ~ ., data=regressionTable)
	summary(allModels)
	
	#perform backward stepwise regression
	bothWays = step(interceptOnlyModel, direction='both', scope=formula(allModels), trace=0)	
	summary(bothWays)
	bothWays$anova
	bothWays$coefficients	
	
	#perform forward stepwise regression
	forwardWay = step(interceptOnlyModel, direction='forward', scope=formula(allModels), trace=0)
	summary(forwardWay)
	forwardWay$anova
	forwardWay$coefficients	
	
	#perform backward stepwise regression
	backwardWay = step(interceptOnlyModel, direction='backward', scope=formula(allModels), trace=0)
	summary(backwardWay)
	backwardWay$anova
	backwardWay$coefficients	
	
		
        }
        
# Finding H5 why?
        
        
variablesOfInterest  = c(vitamimDIndex, sexIndex, BMICatIndex, healthIndex, smokeIndex, snuffIndex, alcoholIndex, sportsIndex, screenIndex, sunbathingIndex, highSchoolIndex)
        
        categoricalXiV2(nonSolariumOnlyTable, highSchoolIndex , BMICatIndex)
        
        
        


        
# Lets try to pull some good vitamin D networks
{
	
	totalSimulations = 100
        	library(wesanderson)
        	
			nonValidIDs = completeTable[is.na(completeTable[,vitamimDIndex]),]$ID
        	
        	completeTableTemp = completeTable
        	
        	completeTableTemp$vitaminDHigh = "Yes"
        	
        	for(i in 1:nrow(completeTableTemp)){
        		
        		if(is.na(completeTableTemp[i, vitamimDIndex])) completeTableTemp$vitaminDHigh[i] = NA
        		else{
        		
        			if(completeTableTemp[i, vitamimDIndex ] < VITAMIND_MIN_LIMIT) completeTableTemp$vitaminDHigh[i] = "No"	
        		}
        		
        	}
        	
        	
        	
  # Eigen
        	completeTableTemp[,vitamimDIndex] = completeTableTemp[,vitamimDIndex] * 0.2
    plotResults = doGraphPlot(overallEdgesDF, completeTableTemp, VITAMIND_FOLDER_IMAGES_HS,
                              
    						  #sizeVariableIndex      = overallPopularityIndex,
            			      #highlightVariable      = vitamimDIndex,
            			      
    	
    	                      highlightVariable = ncol(completeTableTemp),
                              #colorVectorHighlight = COLOR_VECTOR_SEX,
				              sizeVariableIndex = vitamimDIndex,
    	
                              selectedLayouts        = 'mds',
                              plotTitle              = "TTEST",
    						  plotSubtitle           = "TT",
            				  plotTheme              = "blank",
                              #overrideTableName      = currentOverridedPlotName,
            				  overrideLegendSize     = 5,
                    		  overrideLegendPosition = c(0.9, 0.9))   
        	
    
    
    
    
    		completeTableTemp2 = completeTableTemp
    		completeTableTemp2 = completeTableTemp2[ !is.na(completeTableTemp2$SolariumLast4Weeks),]
    		completeTableTemp2 = completeTableTemp2[ !is.na(completeTableTemp2$vitaminDHigh),]
    		completeTableTemp2 = completeTableTemp2[completeTableTemp2$SolariumLast4Weeks == "No",]
    
    		myIDs = completeTableTemp2$ID
			tempEdgesDF = overallEdgesDF
			tempEdgesDF = tempEdgesDF[tempEdgesDF$from %in% myIDs,]
			tempEdgesDF = tempEdgesDF[tempEdgesDF$to   %in% myIDs,]    		
    		
			
			currentGraph  = graph_from_data_frame(tempEdgesDF,  vertices = completeTableTemp2, directed = T)
			
    			# Get the societal layout from our self-made function
        	highSchoolLayout    = createCategoricalLayout(tempEdgesDF, completeTableTemp2, ncol(completeTableTemp2))
        	myConstantLayoutB   = create_layout(graph = currentGraph, layout = 'mds')
        	myConstantLayoutB$x = highSchoolLayout[[1]]$x
        	myConstantLayoutB$y = highSchoolLayout[[1]]$y
        
        	# Add the edge information
        	tempEdgesDF$SameDStatus = "Yes"
        	
        	for(i in 1:nrow(tempEdgesDF)){
        	
        		personAID =  as.numeric(tempEdgesDF[i,1])
        		personBID =  as.numeric(tempEdgesDF[i,2])
        		
        		if(completeTableTemp$vitaminDHigh[personAID] != completeTableTemp$vitaminDHigh[personBID]){
        			
        			tempEdgesDF$SameDStatus[i] = "No"	
        		} 
        		
        	}
        	
        	
        	colnames(tempEdgesDF)[4] = "Same 25OHD Status"  
        	
        	# Prepare the plot titles
        	currentPlotTitle          = "TEMTP"
            currentOverridedPlotName  = paste0("TEMPNetwork_HS_societal")
        	
	        doGraphPlot(tempEdgesDF,  completeTableTemp2, VITAMIND_FOLDER_NETWORK,
	                    sizeVariableIndex = overallConnectionsIndex,
	                    highlightVariable = ncol(completeTableTemp2),
	        	        colorVectorHighlight = c("grey","orange"),
	        			colorVectorEdge   = c("blue", "red"),
	        	        edgesHighlight    = 4,
	                    edgesAlpha        = 0.1,
	                    manualLayout      = myConstantLayoutB,
	        	        
	                    plotTitle         = currentPlotTitle,
	        	        plotTheme              = "blank",
	                    overrideLegendSize = 5,
	        	        overrideTableName      = currentOverridedPlotName,
	        	        overrideLegendPosition = c(0.9, 0.9))	
    
    
    
    
    
    
    
    
    
    
    
        	
    #                         'gem',
#                         'dh',
#                         'graphopt',
#                         'mds' = multidimensional scaling, it tries to keep a balance in between everything.
#                         'fr' = Fruchterman - Reingold , it keeps related vertices toguether
#                         'kk' = Kawai - Emphases distance as information
#                         'drl'
#                         'lgl'
    
    
    
    # Save results here
    
    resultsDF = DF(8,11)
    colnames(resultsDF) = c("Highschool", "Total Relationships", "Equal Relationships", "MIN", "Q1", "Median", "Average", "Q3", "MAX", "SD", "Same Vitamin D Status")
    
    
    
        	
    # For each highschool
    {
    
    	# ( 0_o , superweird syntax to make something so simple!)
    	# Function to check if a row is reversed or not
		is_reversed <- function(row, df) {
			# Check if the reversed row exists in the data frame
		   reversed_exists = any(df["from"] == row["to"] & df["to"] == row["from"])
  
			# Return TRUE if the reversed row doesn't exist
			return(!reversed_exists)
		}

    	for(i in 1:8){
    	
			# Let check social dynamics for HX
			myHighschoolTable = keepCategory(completeTableTemp2, highSchoolIndex, paste0("H",i))
			
			# Reduce the graph to only people in HX
			myIDs = myHighschoolTable$ID
			
			# keep IDs only with valid VitD values
			myIDs = setdiff(myIDs, nonValidIDs)
			
			# Do the network edges
			tempEdgesDF = overallEdgesDF
			tempEdgesDF = tempEdgesDF[tempEdgesDF$from %in% myIDs,]
			tempEdgesDF = tempEdgesDF[tempEdgesDF$to   %in% myIDs,]
			
			h8Graph = graph_from_data_frame(tempEdgesDF,  vertices = myHighschoolTable, directed = T)    		
    	
			
			# Add the edge information
        	tempEdgesDF$SameDStatus = "Yes"
        	
        	for(j in 1:nrow(tempEdgesDF)){
        	
        		personAID =  as.numeric(tempEdgesDF[j,1])
        		personBID =  as.numeric(tempEdgesDF[j,2])
        		
        		if(completeTableTemp$vitaminDHigh[personAID] != completeTableTemp$vitaminDHigh[personBID]){
        			
        			tempEdgesDF$SameDStatus[j] = "No"	
        		} 
        		
        	}
        	
        	colnames(tempEdgesDF)[4] = "Same 25OHD Status"  
			
			
			
			# Correct vitD to graph visualization
			myHighschoolTable[,vitamimDIndex] = myHighschoolTable[,vitamimDIndex] * 0.3
	
			print(i)
			print("Nodes")
			print(nrow(myHighschoolTable))
			print("Edges")
			print(nrow(tempEdgesDF))
					
			#print("Test1")
			
			unique_combinations = tempEdgesDF %>%
			distinct(from, to) %>%
			count()
			
			#print(unique_combinations)
			
			#print("Test2")
			counterDF  = tempEdgesDF[,c(1,2)]
			reversedDF = counterDF[, c(2, 1)]
			appendedDF = rbind(counterDF, reversedDF)
			finalDF    = unique(appendedDF)			
			#print(nrow(finalDF))
			

			#print("Test3")			
			# Apply the is_reversed function to each row and count
			unique_not_reversed = apply(counterDF, 1, is_reversed, df = counterDF)
			count_unique_not_reversed = sum(unique_not_reversed)
			#print(count_unique_not_reversed)
			
			print("Test 4")
			# Create an empty logical vector to track whether each row should be deleted or not
			delete_row =  rep(FALSE, nrow(counterDF))
			
			# Loop through each row of OriginalDF
			for (j in 1:nrow(counterDF)) {
			  # Get the A and B values for the current row
			  current_A <- counterDF$from[j]
			  current_B <- counterDF$to[j]
			
			  for (k in 1:nrow(counterDF)) {
			  
			  	if(j<k){
			  	
			  		if ( counterDF$from[k] == current_B & counterDF$to[k] == current_A)  {
			  		
			  			delete_row[k] = TRUE
			  			
			  		}
			  			
			  	}
			  	
			  }
			    
			}
			
			# Delete the rows that have been flagged for deletion
			ModifiedDF <- counterDF[!delete_row, ]
			
			# Count the number of rows remaining
			final_row_count <- nrow(ModifiedDF)
			
			# Print the final row count
			print(final_row_count)
			
			
			# Do the graph
			if(FALSE){
			        
			    myGraphPlot = doGraphPlot(tempEdgesDF,  myHighschoolTable, VITAMIND_FOLDER_NETWORK,
			                              
			    		                  highlightVariable = ncol(myHighschoolTable),
		        	        			  colorVectorHighlight = c("grey","orange"),
			    		        		  colorVectorEdge   = c("blue", "red"),
		        	        			  edgesHighlight    = 4,
							              sizeVariableIndex = vitamimDIndex,
			    	                      selectedLayouts = "fr",
			                              plotTitle    = paste0("Non solarium relationships in Highschool ",i),
			                              plotSubtitle = "Fruchterman - Reingold layout with node size proportional to 25OHD levels",
			        		              plotTheme    = "blank",
							              overrideTableName  = paste0("H",i,"SubGraphNonSolariums"),
			                              overrideCaption    = paste0("H",i,"School only"),
			                              overrideLegendSize = 5,
			                              overrideLegendPosition = c(0.9, 0.1)) 
	
			}
    	
    	
		    # Simulations
		    if(FALSE){
		    
			    resultsDF[i,1] = paste0("H",i)
			    
	    		overallEdgesFilterDFTemp             = h8OverallEdgesDF
	    		overallEdgesFilterDFTemp$SameValue   = addEdgeRelationship(overallEdgesFilterDFTemp,  completeTableTemp, ncol(completeTableTemp))
			
	    		
	        	listOfEdges = newList(1) # I need to put this into a list because the function is optimize for several networks simulations because R is crappy, I need to change all of that to make the code more legible and screw optimization because of bad language syntax; fuck R
	        	listOfEdges[[1]] = overallEdgesFilterDFTemp 
	    	
	        	# ---- Count how many we have
	        	totalSameRelationships = sum(overallEdgesFilterDFTemp$SameValue)
	        	totalRelationships     = nrow(overallEdgesFilterDFTemp)	
	    	
				# ---- Check if there is bias friendship
		        biasResult = doCategoricalBiasAnalysis(completeTableTemp, listOfEdges, ncol(completeTableTemp), totalSimulations)
	        	#  Network Total Relationships Equal Relationships  MIN   Q1 Median  Average   Q3  MAX       SD SolariumLast4Weeks
	        	#1       1                3575                2561 2184 2319   2366 2364.968 2408 2626 62.08821       0.0007961479
	                	
		        
		        
		        resultsDF[i,2]  = biasResult[[1]][1,2]
		        resultsDF[i,3]  = biasResult[[1]][1,3]
		        resultsDF[i,4]  = biasResult[[1]][1,4]
		        resultsDF[i,5]  = biasResult[[1]][1,5]
		        resultsDF[i,6]  = biasResult[[1]][1,6]
		        resultsDF[i,7]  = biasResult[[1]][1,7]
		        resultsDF[i,8]  = biasResult[[1]][1,8]
		        resultsDF[i,9]  = biasResult[[1]][1,9]
		        resultsDF[i,10] = biasResult[[1]][1,10]
		        resultsDF[i,11] = biasResult[[1]][1,11]
	        
		    }
        	
    	}
    			
    }
    	
    
    resultsDF$ShortP = getAsterkisPValue(resultsDF[,11])
    
    resultsDFSave = resultsDF
    
    # Save the table in disk
	microSimulationsPath = paste0(VITAMIND_FOLDER_TABLES_NETWORK,"microSimulations.csv")
	write.csv2(resultsDF, microSimulationsPath)
	
    
    
    
    
    dataDF = DF(nrow(completeTable), 3)
    colnames(dataDF) = c("myD", "myFriendsD", "Highschool")
    
    dataDF[,1] = completeTable[, vitamimDIndex]
    dataDF[,3] = completeTable$HighSchool
    dataDF[,2] = NA
    
    # Something else
    for(i in 1:nrow(completeTable)){

    		print(i)
    	
    	    myFriends = getFrienshipTypes(i, friendshipMatrix)
    		myFriends = union(myFriends[[4]], myFriends[[5]])   	
    		
    		if(i == 571){
    			
    			print(myFriends)
    			
    		}
    		
    		if(length(myFriends) > 0){
    		
	    		validD   = 0
	    		averageD = 0
	    		
	    		
	    		
	    		for(j in 1:length(myFriends)){
	    			
	    			if(!is.na(completeTable[myFriends[j],vitamimDIndex])){
	    			
	    				averageD = averageD + completeTable[myFriends[j],vitamimDIndex]	
	    				validD   = validD + 1
	    				
	    			}
	    			
	    		if(i == 571){
	    		
	    			print(averageD)
	    			print(validD)
	    			
	    		}
	    			
	    		}
	    		if(validD > 0){
	    			averageD = averageD / validD	
					dataDF[i,2] = averageD
	    		} 
    			
    		}
    	
    }
    
    dataDF = dataDF[complete.cases(dataDF),]
    
    doRegressionPlot(dataDF, 1, 2, VITAMIND_FOLDER_NETWORK,
    				 horizontalLinesCuts   = cutOffVitD,
    				 verticalLinesCuts = cutOffVitD,
    				 plotTitle    = "Relationship between each student 25OHD and friends' average 25OHD",
    			     plotSubtitle = "Only students with at least one friend with valid 25OHD levels are included (n = 930)",
    			     plotXLabel = "Students 25OHD (nmol/L)",
		             plotYLabel = "Friends's average 25OHD (nmol/L)",
		             plotTheme = "regression",
    			     xmax = 110,
    				 ymax = 110,
    				 overrideImageWidth = 16,
    	             overrideImageHeight =  8)
    
    myListOfPlotsObjects = newList( length(myHighschools))
    
    for (i in 1:8) {
    
        currentHS       = paste0("H",i)
        howMany         = sum(dataDF[,3] == currentHS)
        currentSubtitle = paste0("Students from ", currentHS, " are highlighted (n=", howMany, ")")
    	
        currentPlot = doRegressionPlot(dataDF, 1, 2, VITAMIND_FOLDER_NETWORK,
       	                               groupingIndex = 3,
       					               highlightCategory = currentHS,
    					
    					               plotTitle    = "Relationship between each student 25OHD and friends' average 25OHD",
    			    	               plotSubtitle = currentSubtitle,
    			    	               plotXLabel = "Students 25OHD (nmol/L)",
		            	               plotYLabel = "Friends's average 25OHD (nmol/L)",
		            	               plotTheme = "regression",
    			    	               xmax = 110,
    					               ymax = 110,
    					               overrideImageWidth  = 10,
    	            	               overrideImageHeight =  8)    	
        
        myListOfPlotsObjects[[i]] = currentPlot[[1]]
    	
    }
    
    ggarrange(plotlist =  myListOfPlotsObjects , nrow = 4, ncol = 2)
	allRegressionsPath  = file.path(paste(VITAMIND_FOLDER_NETWORK, "4x2RegressionsFriends.png", sep = ""))
	ggsave(allRegressionsPath, width = 8, height = 16)
    


	# Create a list to store the regression results
    resultsRegressionDF = DF(8,4)
    colnames(resultsRegressionDF) = c("R2","P", "N", "H")
    
	# Create a ggplot object with different colors for each group
	p <- ggplot(dataDF, aes(x = dataDF$myD, y = dataDF$myFriendsD, color = dataDF$Highschool)) + geom_point()

	
	
	
	# Loop through each color group
	for (i in 1: length(levels(dataDF$Highschool))) {
		
		mySchool = levels(dataDF$Highschool)[i]
		
		print(mySchool)
		
		# Subset the data frame to keep only the current color group
		df_color <- subset(dataDF, dataDF$Highschool == mySchool)
  
		print(nrow(df_color))
		
		# Fit a linear regression model for the current color group
		model <- lm(myD ~ myFriendsD, data = df_color)
  
		
		# Get the R-squared and p-value of the model
		rsq <- summary(model)$r.squared
		pval <- summary(model)$coefficients[2, 4]
  
		# Add the regression line to the plot with the R-squared and p-value as labels
		p <- p + geom_point() + geom_smooth(method = "lm", fill = NA)
    	     #annotate("text", x = max(df_color$myFriendsD), y = max(df_color$myD), label = paste0("R-sq: ", round(rsq, 2), "\n", "p-value: ", round(pval, 2)), hjust = 1, vjust = 1)
  
		resultsRegressionDF[i,1] = rsq
		resultsRegressionDF[i,2] = pval
		resultsRegressionDF[i,3] = nrow(df_color)
		resultsRegressionDF[i,4] = mySchool
		
	}

	# Print the regression results
	

	# Print the plot
	print(p)    
    
	
	resultsRegressionDF$A = getAsterkisPValue(resultsRegressionDF$P)
    
    
}
    
    
    
    
        	
        	
        	