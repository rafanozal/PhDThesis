
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

# Constants

MEDICINES_FOLDER          = file.path(paste(RESULT_FOLDER,           "medicinePaper/",        sep = ""))
MEDICINES_FOLDER_IMAGES   = file.path(paste(MEDICINES_FOLDER,            "images/",               sep = ""))
MEDICINES_FOLDER_NETWORK  = file.path(paste(MEDICINES_FOLDER_IMAGES,         "networks/",             sep = ""))
MEDICINES_FOLDER_MEDICINE = file.path(paste(MEDICINES_FOLDER_IMAGES,         "medicine/",             sep = ""))

MEDICINES_FOLDER_TABLES                          = file.path(paste(MEDICINES_FOLDER, "tables/",                        sep = ""))
MEDICINES_FOLDER_TABLES_NETWORK                  = file.path(paste(MEDICINES_FOLDER_TABLES,  "network/",               sep = ""))
MEDICINES_FOLDER_TABLES_MEDICINE                 = file.path(paste(MEDICINES_FOLDER_TABLES,  "medicine/",              sep = ""))                
MEDICINES_FOLDER_TABLES_SEX                      = file.path(paste(MEDICINES_FOLDER_TABLES,  "sex/",                   sep = ""))        
MEDICINES_FOLDER_TABLES_HS                       = file.path(paste(MEDICINES_FOLDER_TABLES,  "highschools/",           sep = ""))                        

LATEX_RELATIVE_MEDICINES                         = file.path(paste(LATEX_RELATIVE_BASE_PATH2,               "vitaminD/",  sep = ""))
LATEX_RELATIVE_MEDICINES_FOLDER_IMAGES           = file.path(paste(LATEX_RELATIVE_MEDICINES,                "images/",    sep = ""))
LATEX_RELATIVE_MEDICINES_FOLDER_IMAGES_NETWORK   = file.path(paste(LATEX_RELATIVE_MEDICINES_FOLDER_IMAGES,  "network/",   sep = ""))
        


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

            plotResults = doGraphPlot(allEdges[[i]],  completeTable, MEDICINES_FOLDER_NETWORK,
                                      sizeVariableIndex = overallConnectionsIndex,
                                      selectedLayouts   = "mds",
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
    
        plotObject = doHistogramPlot2(completeTable, overviewIndex, MEDICINES_FOLDER_NETWORK,
                                      colorsVector   = COLOR_FRIENDSHIP,
                                      binsWidth      = 1,
                                      binBorderColor = "#333333",
                                      plotTitle      = " Does these friends give a good overview of your social network? ",
                                      plotSubtitle   = " 0 = Low, 10 = High",
                                      plotXLabel     = "Points", plotYLabel = "Total")       
        
        
        
        writeImageLATEX2(plotObject[[2]], LATEX_RELATIVE_MEDICINES_FOLDER_IMAGES_NETWORK, 
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
	            
	            myListOfPlotsObjects[[i]] = doHistogramPlot2(currentSubtable, overviewIndex, MEDICINES_FOLDER_NETWORK,
	                                                         colorsVector   = HIGHSCHOOL_COLOR_VECTOR[i],
	                                                         binsWidth      = 1,
	                                                         binBorderColor = "#333333",
	                                                         plotTitle      = currentPlotTitle,
	                                                         plotSubtitle   = " 0 = Low, 10 = High",
	                                                         plotXLabel     = "Points", plotYLabel = "Total",
	                                                         overrideTableName = currentTableOverride)[[1]]
	            
	    }
	        
	    ggarrange(plotlist =  myListOfPlotsObjects , nrow = 4, ncol = 2)
	        
	    allGraphPath  = file.path(paste(MEDICINES_FOLDER_NETWORK, "4x2HistoFriends.png", sep = ""))
	    ggsave(allGraphPath, width = 8, height = 16)
	
	    latexPlot = writeImageLATEX2(allGraphPath, LATEX_RELATIVE_MEDICINES_FOLDER_IMAGES_NETWORK, 
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
    	
    	writeTableLATEX(homoResults, MEDICINES_FOLDER_TABLES_NETWORK,
        	            tableCaption = "Highschools and friendship overview. The first row represent all high schools combined. H1 to H8 rows represent each high school separetly.
    		                            Homophily represent how many students of this school form friendship with a student of the same school. Frequency is the relative frequency
    		                            of students in each high school. Significance is the p-value of a two sided binomial test of relationships with-in same high school, total
    		                            relationships, and relative frequency of students in each high-school. Average and Median are the values of how good is the self-reported
    		                            network (0 - 10) by each student in each high school.",
            	        overrideTableName = "HSHomophily",
                	    widthProportion = 0.7, heightProportion = 0.09)          
    
    	csvPath = paste0(MEDICINES_FOLDER_TABLES_NETWORK, "homophilyResults.csv")
    	write.csv2(homoResults, csvPath)
    	
	}	
	
}


# ------------------------------------------------------------------------------
# Medication count
# ------------------------------------------------------------------------------
{

    # Medication
	{

		# Medicine frequency in the introduction
			
		# Total Count Table
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
			
		    writeTableLATEX(drugsSummaryDF, MEDICINES_FOLDER_TABLES_MEDICINE,
		        	        tableCaption      = "Summary of all medicine comsuption in the population. From left to right,
		    	                                 brand of the medicine, ATC, type of medicine, and total consumption.",
		            	    overrideTableName = "drugTable",
		                	widthProportion   = 0.7, heightProportion = 0.3)
		    
		    
		    csvPath = paste0(MEDICINES_FOLDER_TABLES_MEDICINE, "drugsSummaryDF.csv")
    		write.csv2(drugsSummaryDF, csvPath)
		    
		    
			
		}
		
		# Total Count absolute
		{
		
			# THE ABSOLUTE FUCK OF THE R LANGUAGE!
			
			medicinesDBDFSorted = medicinesDBDF
			medicinesDBDFSorted[,3] = factor(medicinesDBDFSorted[,3], levels = rev(sort(unique(medicinesDBDF[,3]))))  
			
			myPlotTitle    = "Absolute frequency of medicine"
			myXLabel       = "Absolute frequency"
			myYLabel       = "Medicine"
			
    		myPlot = doBarPlotV2(medicinesDBDFSorted, 3, MEDICINES_FOLDER_MEDICINE,
    							 groupIndex = 2,
    							 rotation = TRUE,
                                 plotTitle    = "Absolute frequency of medicine brand, grouped by type",
    							 plotSubtitle = "and sorted alphabetically",
                                 plotXLabel = myXLabel, plotYLabel = myYLabel,
                                 barsFontSize = 2,
    							 legendPosition = "bottom",
                                 overrideImageHeight = 25,
    							 overrideTableName = "byNamesAbsolute",
    							 overrideCaption = "myCaption1")			
	    		
			myPlot = doBarPlotV2(medicinesDBDFSorted, 2, MEDICINES_FOLDER_MEDICINE,
    							 rotation = TRUE,
    							 plotTitle    = "Absolute frequency of medicine type",
				                 plotSubtitle = "and sorted alphabetically",
                                 plotXLabel = myXLabel, plotYLabel = myYLabel,
                                 barsFontSize = 2,
    							 legendPosition = "bottom",
								 overrideImageWidth  = 13,
                                 overrideImageHeight = 4,
    							 overrideTableName = "typesAbsolute",
    							 overrideCaption = "myC")	    		
    		
			
			
			# Factor them by type
			myTypes   = sort(unique(medicinesDBDF$Type))
			myFactors = rep("a", length(unique(medicinesDBDF$Brand)))
			
			currentCounter = 1
			for(i in 1:length(myTypes)){
				
				# Get all the brands belonging to this group
				currentGroup  = myTypes[i]
				currentSubset = medicinesDBDF[medicinesDBDF$Type == currentGroup,]
				currentBrands = sort(unique(currentSubset$Brand))
				
				# Add each brand to the factors vector
				for(j in 1:length(currentBrands)){
				
					myFactors[currentCounter] = currentBrands[j]
					currentCounter            = currentCounter + 1
				}

			}
			
			medicinesDBDFTypeSorted     = medicinesDBDF
			medicinesDBDFTypeSorted[,3] = factor(medicinesDBDFTypeSorted[,3], levels = rev( myFactors ))  

			myPlot = doBarPlotV2(medicinesDBDFTypeSorted, 3, MEDICINES_FOLDER_MEDICINE,
    							 groupIndex = 2,
    							 rotation = TRUE,
                                 plotTitle    = "Absolute frequency of medicine brand, grouped by type",
    							 plotSubtitle = "and sorted by type",
                                 plotXLabel = myXLabel, plotYLabel = myYLabel,
                                 barsFontSize = 2,
    							 legendPosition = "bottom",
                                 overrideImageHeight = 25,
    							 overrideTableName = "byNamesAbsoluteSorted",
    							 overrideCaption = "myCaption1")	
			
			
			
			
			
		}

		# Break down by sex
				
		# Relative Count by sex
		{
		
			medicinesDBDFSex = medicinesDBDF
			medicinesDBDFSex$Sex = "Man"
			
			for (i in 1:nrow(medicinesDBDFSex)) {
				
				# Get ID and sex
				currentID  = medicinesDBDFSex$ID[i] 
				currentSex = completeTable$Sex[currentID]
				
				# Check the complete table
				if(currentSex == "Woman") medicinesDBDFSex$Sex[i] = "Woman"
				
			}
			
	    		
			myPlot = doBarPlotV2(medicinesDBDFSex, 2, MEDICINES_FOLDER_MEDICINE,
							     groupIndex = 7,
								 colorsVector = COLOR_VECTOR_SEX,
    							 rotation = TRUE,
								 countingType = "absolute",
                                 plotTitle    = "Absolute frequency of medicine types, grouped by sex",
    							 plotSubtitle = "",
                                 plotXLabel = myXLabel, plotYLabel = myYLabel,
                                 barsFontSize = 2,
    							 legendPosition = "bottom",
								 overrideImageWidth  = 10,
                                 overrideImageHeight = 10,
    							 overrideTableName = "typesBySex",
    							 overrideCaption = "Absolute frequency of medicine types, grouped by sex")
			

			myPlot = doBarPlotV2(medicinesDBDFSex, 2, MEDICINES_FOLDER_MEDICINE,
							     groupIndex = 7,
								 colorsVector = COLOR_VECTOR_SEX,
    							 rotation = TRUE,
								 countingType = "relative",
                                 plotTitle    = "Relative frequency of medicine types, grouped by sex",
    							 plotSubtitle = "",
                                 plotXLabel = myXLabel, plotYLabel = myYLabel,
                                 barsFontSize = 2,
    							 legendPosition = "bottom",
								 overrideImageWidth  = 10,
                                 overrideImageHeight = 10,
    							 overrideTableName = "typesBySex",
    							 overrideCaption = "Relative frequency of medicine types, grouped by sex")	
			
			# Delete the sex hormones
			
			medicinesNoHormonesDBDFSex = medicinesDBDFSex[medicinesDBDFSex$Type != "Genito-urinary system and sex hormones",]
			
			# Factor them by type
			myTypes   = sort(unique(medicinesNoHormonesDBDFSex$Type))
			myFactors = rep("a", length(unique(medicinesNoHormonesDBDFSex$Brand)))
			
			currentCounter = 1
			for(i in 1:length(myTypes)){
				
				# Get all the brands belonging to this group
				currentGroup  = myTypes[i]
				currentSubset = medicinesNoHormonesDBDFSex[medicinesNoHormonesDBDFSex$Type == currentGroup,]
				currentBrands = sort(unique(currentSubset$Brand))
				
				# Add each brand to the factors vector
				for(j in 1:length(currentBrands)){
				
					myFactors[currentCounter] = currentBrands[j]
					currentCounter            = currentCounter + 1
				}

			}
			
			medicinesNoHormonesDBDFSexSorted     = medicinesNoHormonesDBDFSex
			medicinesNoHormonesDBDFSexSorted[,3] = factor(medicinesNoHormonesDBDFSexSorted[,3], levels = rev( myFactors ))  
			
			

			myPlot = doBarPlotV2(medicinesNoHormonesDBDFSexSorted, 3, MEDICINES_FOLDER_MEDICINE,
							     groupIndex = 7,
								 colorsVector = COLOR_VECTOR_SEX,
    							 rotation = TRUE,
								 countingType = "relative",
                                 plotTitle    = "Relative frequency of medicine brand, grouped by sex",
    							 plotSubtitle = "Sex hormones not included (100% females in all cases)",
                                 plotXLabel = myXLabel, plotYLabel = myYLabel,
                                 barsFontSize = 2,
    							 legendPosition = "bottom",
								 overrideImageWidth  = 10,
                                 overrideImageHeight = 25,
    							 overrideTableName = "typesBySex",
    							 overrideCaption = "Relative frequency of medicine types, grouped by sex")				
			

			summaryDF = summarizeBicategoricalV3(medicinesNoHormonesDBDFSexSorted, 3, 7)
			csvPath = paste0(MEDICINES_FOLDER_TABLES_MEDICINE, "medicineCountByBrandType.csv")
    		write.csv2(summaryDF, csvPath)
			
		}
			
	}
 
}

# ------------------------------------------------------------------------------
# Checking medicines types
# ------------------------------------------------------------------------------
{
	
	reduceMedicinesDBDF = medicinesDBDF
	reduceMedicinesDBDF = reduceMedicinesDBDF[,-c(1,4,5,6)]
	reduceMedicinesDBDF = reduceMedicinesDBDF[!duplicated(reduceMedicinesDBDF), ]
	
	csvPath = paste0(MEDICINES_FOLDER_TABLES_MEDICINE, "medicineTypesSummary.csv")
    write.csv2(summaryDF, csvPath)	
}


# ------------------------------------------------------------------------------
# Contraceptive, Antiinflammatories, Antihistaminics, Painkillers usage by sex and highschool
# ------------------------------------------------------------------------------
{

	# Prepare the data for the medicine usage in this case
	{

		reducedData = DF(nrow(completeTable), 7)
		colnames(reducedData) = c("ID", "Sex", "Highschool", "HormonalType", "Antiflammatories", "Antihistaminic", "Painkiller")
		
		reducedData$ID           = completeTable$ID
		reducedData$Sex          = completeTable$Sex
		reducedData$Highschool   = completeTable$HighSchool
		reducedData$HormonalType = completeTable$HormonalType
		
		reducedData$Antiflammatories = "No"
		reducedData$Antihistaminic   = "No"
		reducedData$Painkiller       = "No"
		
		# Who is taking...
		for(i in 1:nrow(reducedData)){
		
			currentID = reducedData$ID[i]
			
			# What medicines are you taking?
			currentMedicinesDF = medicinesDBDF[medicinesDBDF$ID == currentID,]
			if(nrow(currentMedicinesDF) > 0){
				
				myMedicines = currentMedicinesDF$Brand
				
				# Are you taking antiinflammatories?
				if (sum( c("Ibux 200 mg", "Ibux 400 mg", "Ibux 600 mg", "Naprosyn") %in% myMedicines ) > 0) reducedData$Antiflammatories[i] = "Yes"
			
				# Are you taking antihistamines?
				if (sum( c("Cetrizin", "Zyrtec") %in% myMedicines ) > 0)                                    reducedData$Antihistaminic[i] = "Yes"
				
				# Are you taking painkillers?
				if (sum( c("Paracetamol") %in% myMedicines ) > 0)                                           reducedData$Painkiller[i] = "Yes"
				
			}
			
		}
		
		reducedDataWomenOnly = reducedData[reducedData$Sex == "Woman",]
		
	}
	
	xi2TableDF = DF(7,3)
	colnames(xi2TableDF) = c("Bias type", "Significance", "Asterisk")
	xi2TableDF[1,1] = "Highschool, hormonal (women only)"
	xi2TableDF[2,1] = "Highschool, antinflammatory"
	xi2TableDF[3,1] = "Highschool, antihistamine"
	xi2TableDF[4,1] = "Highschool, painkiller"
	xi2TableDF[5,1] = "Sex, antiinflammatory"
	xi2TableDF[6,1] = "Sex, antihistamine"
	xi2TableDF[7,1] = "Sex, painkiller"
	
	
	# Xi2 by highschools
	{

		# Hormonal		
		xiResults       = categoricalXiV2(reducedDataWomenOnly, 3 ,4)
		xi2TableDF[1,2] = xiResults[[8]][1]
		csvPath = paste0(MEDICINES_FOLDER_TABLES_HS, "biasByHormonalHS.csv")
    	write.csv2(xiResults[[7]], csvPath)
		
    	# Antiinflammatories
		xiResults       = categoricalXiV2(reducedData, 3 ,5)
		xi2TableDF[2,2] = xiResults[[8]][1]
		csvPath = paste0(MEDICINES_FOLDER_TABLES_HS, "biasByAntinflammatoryHS.csv")
    	write.csv2(xiResults[[7]], csvPath)		

    	# Antihistamines
		xiResults       = categoricalXiV2(reducedData, 3 ,6)
		xi2TableDF[3,2] = xiResults[[8]][1]
		csvPath = paste0(MEDICINES_FOLDER_TABLES_HS, "biasByAntihistaminesHS.csv")
    	write.csv2(xiResults[[7]], csvPath)				
    	
    	# Painkiller
		xiResults       = categoricalXiV2(reducedData, 3 ,7)
		xi2TableDF[4,2] = xiResults[[8]][1]
		csvPath = paste0(MEDICINES_FOLDER_TABLES_HS, "biasByPainkillerHS.csv")
    	write.csv2(xiResults[[7]], csvPath)						
    	
		
	}
	
	# Xi2 by sex
	{
	
		# Hormonal (not necessary)		

    	# Antiinflammatories
		xiResults       = categoricalXiV2(reducedData, 2 ,5)
		xi2TableDF[5,2] = xiResults[[8]][1]
		csvPath = paste0(MEDICINES_FOLDER_TABLES_HS, "biasByAntinflammatorySEX.csv")
    	write.csv2(xiResults[[7]], csvPath)		

    	# Antihistamines
		xiResults       = categoricalXiV2(reducedData, 2 ,6)
		xi2TableDF[6,2] = xiResults[[8]][1]
		csvPath = paste0(MEDICINES_FOLDER_TABLES_HS, "biasByAntihistaminesSEX.csv")
    	write.csv2(xiResults[[7]], csvPath)				
    	
    	# Painkiller
		xiResults       = categoricalXiV2(reducedData, 2 ,7)
		xi2TableDF[7,2] = xiResults[[8]][1]
		csvPath = paste0(MEDICINES_FOLDER_TABLES_HS, "biasByPainkillerSEX.csv")
    	write.csv2(xiResults[[7]], csvPath)	
			
	}
		
	
	# Pretty format and save
	for(i in 1:nrow(xi2TableDF)){
	
		xi2TableDF[i,3] = getAsterkisPValue(xi2TableDF[i,2])
			
	}
	
	csvPath = paste0(MEDICINES_FOLDER_TABLES_NETWORK, "biasXi2Tables.csv")
    write.csv2(xi2TableDF, csvPath)		
	
}

# ------------------------------------------------------------------------------
# Simulations
# ------------------------------------------------------------------------------
{
	
	totalSimulations = 50
	
    # Get all edges into a list
    edgeList      = newList(TOTAL_NETWORKS)
    edgeList[[1]] = overallEdgesDF
    edgeList[[2]] = physicalEdgesDF
    edgeList[[3]] = schoolEdgesDF
    edgeList[[4]] = sportsEdgesDF
    edgeList[[5]] = homeEdgesDF
    edgeList[[6]] = otherEdgesDF

    # Run the bias analysis with respect...
    
    # Antiinflammatories
    AntiflammatoriesBias = doCategoricalBiasAnalysis(reducedData,
                                                     edgeList,
                                                     5,
                                                     totalSimulations,
                                                     listOfNetworksNames = NETWORK_NAMES)	
    
    csvPath = paste0(MEDICINES_FOLDER_TABLES_NETWORK, "antinflammatorySimulations.csv")
    write.csv2(AntiflammatoriesBias, csvPath)		
    
    
    # Antihistamines
    AntihistamineBias = doCategoricalBiasAnalysis(reducedData,
                                                     edgeList,
                                                     6,
                                                     totalSimulations,
                                                     listOfNetworksNames = NETWORK_NAMES)    
    
    csvPath = paste0(MEDICINES_FOLDER_TABLES_NETWORK, "antihistamineSimulations.csv")
    write.csv2(AntihistamineBias, csvPath)		    
    
    # Painkillers
    PainkillerBias = doCategoricalBiasAnalysis(reducedData,
                                               edgeList,
                                               7,
                                               totalSimulations,
                                               listOfNetworksNames = NETWORK_NAMES)    
    
    csvPath = paste0(MEDICINES_FOLDER_TABLES_NETWORK, "painkillerSimulations.csv")
    write.csv2(PainkillerBias, csvPath)		        
    
    # Hormonal types (women only)
	reducedDataWomenOnly = reducedData[reducedData$Sex == "Woman",]
	reducedDataMenOnly   = reducedData[reducedData$Sex == "Man",]    
    
	# We need to delete all non-woman form all edges and tables
    menIDs = reducedDataMenOnly$ID

    deleteThisLists          = deleteConnections(overallEdgesDF,  menIDs, menIDs)[[1]]
    womenOnlyOverallEdgesDF  = overallEdgesDF[  !deleteThisLists, ]
    deleteThisLists          = deleteConnections(physicalEdgesDF, menIDs, menIDs)[[1]]
    womenOnlyPhysicalEdgesDF = physicalEdgesDF[ !deleteThisLists, ]
    deleteThisLists          = deleteConnections(schoolEdgesDF,   menIDs, menIDs)[[1]]
    womenOnlySchoolEdgesDF   = schoolEdgesDF[    !deleteThisLists, ]
    deleteThisLists          = deleteConnections(sportsEdgesDF,   menIDs, menIDs)[[1]]
    womenOnlySportsEdgesDF   = sportsEdgesDF[   !deleteThisLists, ]
    deleteThisLists          = deleteConnections(homeEdgesDF,     menIDs, menIDs)[[1]]
    womenOnlyHomeEdgesDF     = homeEdgesDF[     !deleteThisLists, ]
    deleteThisLists          = deleteConnections(otherEdgesDF,    menIDs, menIDs)[[1]]
    womenOnlyOthersEdgesDF   = otherEdgesDF[    !deleteThisLists, ]

    # Get all edges into a list
    edgeList      = newList(TOTAL_NETWORKS)
    edgeList[[1]] = womenOnlyOverallEdgesDF
    edgeList[[2]] = womenOnlyPhysicalEdgesDF
    edgeList[[3]] = womenOnlySchoolEdgesDF
    edgeList[[4]] = womenOnlySportsEdgesDF
    edgeList[[5]] = womenOnlyHomeEdgesDF
    edgeList[[6]] = womenOnlyOthersEdgesDF
    
    
    HormonalBias = doCategoricalBiasAnalysis(reducedDataWomenOnly,
                                             edgeList,
                                             4,
                                             totalSimulations,
                                             listOfNetworksNames = NETWORK_NAMES)
	
    csvPath = paste0(MEDICINES_FOLDER_TABLES_NETWORK, "hormonalSimulations.csv")
    write.csv2(HormonalBias, csvPath)	
	
}




			diseasesDBDFSex = diseasesDBDF
			diseasesDBDFSex$Sex = "Man"
			
			for (i in 1:nrow(diseasesDBDFSex)) {
				
				# Get ID and sex
				currentID  = diseasesDBDFSex$ID[i] 
				currentSex = completeTable$Sex[currentID]
				
				# Check the complete table
				if(currentSex == "Woman") diseasesDBDFSex$Sex[i] = "Woman"
				
			}
			
	    		
			myPlot = doBarPlotV2(diseasesDBDFSex, 2, MEDICINES_FOLDER_MEDICINE,
							     groupIndex = 7,
								 colorsVector = COLOR_VECTOR_SEX,
    							 rotation = TRUE,
								 countingType = "absolute",
                                 plotTitle    = "Absolute frequency of diseases types, grouped by sex",
    							 plotSubtitle = "",
                                 plotXLabel = myXLabel, plotYLabel = myYLabel,
                                 barsFontSize = 2,
							     minValue = 6,
    							 legendPosition = "bottom",
								 overrideImageWidth  = 10,
                                 overrideImageHeight = 10,
    							 overrideTableName = "typesDiseasesBySex",
    							 overrideCaption = "Absolute frequency of diseasesmedicine types, grouped by sex")



    		myPlot = doBarPlotV2(medicinesDBDFSorted, 3, MEDICINES_FOLDER_MEDICINE,
    							 groupIndex = 2,
    							 rotation = TRUE,
                                 plotTitle    = "Absolute frequency of medicine brand, grouped by type",
    							 plotSubtitle = "and sorted alphabetically",
                                 plotXLabel = myXLabel, plotYLabel = myYLabel,
                                 barsFontSize = 2,
    							 legendPosition = "bottom",
                                 overrideImageHeight = 25,
    							 overrideTableName = "byNamesAbsolute",
    							 overrideCaption = "myCaption1")

