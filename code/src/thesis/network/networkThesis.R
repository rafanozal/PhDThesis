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

# Load tools
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/basic/toolsBasic.R"),       encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/basic/toolsSummarizers.R"), encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/basic/toolsNetworks.R"),    encoding="utf-8")

# Load plotting
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingCommon.R"),       encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingNetwork.R"),      encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingHistograms.R"),   encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingRegression.R"),   encoding="utf-8")

# Load analysis
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/analysis/analysisNetworks.R"), encoding="utf-8")


# ------------------------------------------------------------------------------
# Network plots overview
# ------------------------------------------------------------------------------
{
	
	# Make a constant layout for consistence between plots
    myGraph           = graph_from_data_frame(overallEdgesDF, vertices = completeTable, directed = FALSE)
    myConstantLayoutA = create_layout(graph = myGraph, layout = 'mds')
		
	
	
	# Layout demonstration
    # ALL_LAYOUTS = c('grid','star','circle','gem', 'dh', 'graphopt', 'mds', 'fr', 'kk', 'drl', 'lgl')
	{
	
		my_layouts = c('grid','star','circle', 'graphopt', 'mds', 'fr', 'kk', 'drl', 'lgl')
		#my_layouts = c('mds')
			
		# For each layout
		for(i in 1:length(my_layouts)){
		
			# Get the current layout
			currentLayout = my_layouts[i]
			print(currentLayout)
			
			# Set the title
		    currentOverridedPlotName = paste0("OverallNetwork_with_no_highlight_", currentLayout)
            currentPlotTitle         = paste0("Overall Network with ", currentLayout, " layout")
            currentOverrideCaption   = "Size based on number of undirected relationships"		
            
            #baseColor = "#9bd0f0"
            
			# Do the plot
            plotResults = doGraphPlot(overallEdgesDF, completeTable, NETWORK_FOLDER_IMAGES,
                                      sizeVariableIndex      = overallConnectionsIndex,
            						  highlightVariable      = highSchoolIndex,
            						  #colorVectorHighlight  = baseColor,
                                      selectedLayouts        = currentLayout,
                                      plotTitle              = currentPlotTitle,
            						  plotTheme              = "blank",
                                      overrideTableName      = currentOverridedPlotName,
            						  overrideLegendSize     = 5,
                    				  overrideLegendPosition = c(0.9, 0.9))             
            
            
            #COLOR_NA
			
		}
		
		# Do the circles layout
		{
			# Get the societal layout from our self-made function
        	highSchoolLayout    = createCategoricalLayout(overallEdgesDF, completeTable, highSchoolIndex)
        	myConstantLayoutB   = create_layout(graph = overallGraph, layout = 'mds')
        	myConstantLayoutB$x = highSchoolLayout[[1]]$x
        	myConstantLayoutB$y = highSchoolLayout[[1]]$y
        
        	# Prepare the plot titles
        	currentPlotTitle          = "Overall network with nodes grouped by school."
            currentOverridedPlotName  = paste0("OverallNetwork_HS_societal")
        	
	        doGraphPlot(overallEdgesDF,  completeTable, NETWORK_FOLDER_IMAGES,
	                    sizeVariableIndex = overallConnectionsIndex,
	                    highlightVariable = highSchoolIndex,
	                    edgesAlpha        = 0.1,
	                    manualLayout      = myConstantLayoutB,
	                    plotTitle         = currentPlotTitle,
	        	        plotTheme              = "blank",
	                    overrideLegendSize = 5,
	        	        overrideTableName      = currentOverridedPlotName,
	        	        overrideLegendPosition = c(0.9, 0.9))	
			
		}
	
		# Do the geographical layout
		{
			
			width  = 1675
			height = 840
			
			compressY = 0.6
			
			myCentroids = newList(8)
			myCentroids[[1]] = c( 1094 , height - 197 )
			myCentroids[[2]] = c( 995  , height - 235 )
			myCentroids[[3]] = c( 902  , height - 654 )
			myCentroids[[4]] = c( 114  , height - 132 )
			myCentroids[[5]] = c( 180  , height - 641 )
			myCentroids[[6]] = c( 842  , height - 646 )
			myCentroids[[7]] = c( 1595 , height - 188 )
			myCentroids[[8]] = c( 954  , height - 571 )
			
			for (i in 1:8) {
				
				myCentroids[[i]][1] = myCentroids[[i]][1] / width
				myCentroids[[i]][2] = myCentroids[[i]][2] / height
				myCentroids[[i]][2] = myCentroids[[i]][2] * compressY
				
			}
			
			
			
			
			# Get the societal layout from our self-made function
        	highSchoolLayout    = createCategoricalLayout(overallEdgesDF, completeTable, highSchoolIndex, manualCentroids = myCentroids)
        	myConstantLayoutB   = create_layout(graph = overallGraph, layout = 'mds')
        	myConstantLayoutB$x = highSchoolLayout[[1]]$x
        	myConstantLayoutB$y = highSchoolLayout[[1]]$y
        
        	# Prepare the plot titles
        	currentPlotTitle          = "Overall network with nodes grouped by school."
        	currentPlotSubtitle       = "Centered around geographical location"
        	currentOverridedPlotName  = paste0("OverallNetwork_HS_geographical")
        
	        doGraphPlot(overallEdgesDF,  completeTable, NETWORK_FOLDER_IMAGES,
	                    sizeVariableIndex   = overallConnectionsIndex,
	                    highlightVariable   = highSchoolIndex,
	                    edgesAlpha          = 0.2,
	                    manualLayout        = myConstantLayoutB,
	                    plotTitle           = currentPlotTitle,
	        	        plotSubtitle        = currentPlotSubtitle,
	        	        plotTheme           = "blank",
	                    overrideLegendSize  = 5,
	        	        overrideImageWidth  = 17,
	        	        overrideImageHeight = 17,
	        	        overrideTableName      = currentOverridedPlotName,
	        	        overrideLegendPosition = c(0.9, 0.2))				
			
			
		}
		
	}
	
	
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
# Basic statistics
# ------------------------------------------------------------------------------
{

	# Create the friendships matrices for the upcoming functions
	overallMatrix  = getFriendshipMatrix(overallEdgesDF,  1038)
	physicalMatrix = getFriendshipMatrix(physicalEdgesDF, 1038)
	schoolMatrix   = getFriendshipMatrix(schoolEdgesDF,   1038)
	sportMatrix    = getFriendshipMatrix(sportsEdgesDF,   1038)
	homeMatrix     = getFriendshipMatrix(homeEdgesDF,     1038)
	otherMatrix    = getFriendshipMatrix(otherEdgesDF,    1038)
	
	#Update the network numbers
	for(i in 1:nrow(completeTable)){
		
    	# Get the friend of each person
    	overallFriendShipAnalysis  = getFrienshipTypes (i, overallMatrix)
    	physicalFriendShipAnalysis = getFrienshipTypes (i, physicalMatrix)
    	schoolFriendShipAnalysis   = getFrienshipTypes (i, schoolMatrix)
    	sportFriendShipAnalysis    = getFrienshipTypes (i, sportMatrix)
    	homeFriendShipAnalysis     = getFrienshipTypes (i, homeMatrix)
    	otherFriendShipAnalysis    = getFrienshipTypes (i, otherMatrix)
    	
    	# Connections
    	overallCurrentConnections  = length(getUndirectedFriends(i, overallMatrix))		
    	physicalCurrentConnections = length(getUndirectedFriends(i, physicalMatrix))		
    	schoolCurrentConnections   = length(getUndirectedFriends(i, schoolMatrix))		
    	sportCurrentConnections    = length(getUndirectedFriends(i, sportMatrix))		
    	homeCurrentConnections     = length(getUndirectedFriends(i, homeMatrix))		
    	otherCurrentConnections    = length(getUndirectedFriends(i, otherMatrix))		
    	
    	# Popularity
    	overallCurrentPopularity   = overallFriendShipAnalysis[[1]]
    	physicalCurrentPopularity  = physicalFriendShipAnalysis[[1]]
    	schoolCurrentPopularity    = schoolFriendShipAnalysis[[1]]
    	sportCurrentPopularity     = sportFriendShipAnalysis[[1]]
    	homeCurrentPopularity      = homeFriendShipAnalysis[[1]]
    	otherCurrentPopularity     = otherFriendShipAnalysis[[1]]
    	
    	# Nomination
    	overallCurrentNominations  = overallFriendShipAnalysis[[2]]
    	physicalCurrentNominations = physicalFriendShipAnalysis[[2]]
    	schoolCurrentNominations   = schoolFriendShipAnalysis[[2]]
    	sportCurrentNominations    = sportFriendShipAnalysis[[2]]
    	homeCurrentNominations     = homeFriendShipAnalysis[[2]]
    	otherCurrentNominations    = otherFriendShipAnalysis[[2]]
    	
    	# Reciprocity
    	overallCurrentReciprocity  = overallFriendShipAnalysis[[3]]
    	physicalCurrentReciprocity = physicalFriendShipAnalysis[[3]]
    	schoolCurrentReciprocity   = schoolFriendShipAnalysis[[3]]
    	sportCurrentReciprocity    = sportFriendShipAnalysis[[3]]
    	homeCurrentReciprocity     = homeFriendShipAnalysis[[3]]
    	otherCurrentReciprocity    = otherFriendShipAnalysis[[3]]
    	
    	
    	
		# Overall
		completeTable$OverallFollowing[i]   = overallCurrentNominations
		completeTable$OverallPopularity[i]  = overallCurrentPopularity
		completeTable$OverallReciprocity[i] = overallCurrentReciprocity
		completeTable$OverallConnections[i] = overallCurrentConnections
        		
		# Physical
		completeTable$PhysicalFollowing[i]   = physicalCurrentNominations
		completeTable$PhysicalPopularity[i]  = physicalCurrentPopularity
		completeTable$PhysicalReciprocity[i] = physicalCurrentReciprocity
		completeTable$PhysicalConnections[i] = physicalCurrentConnections
		
		# School
		completeTable$SchoolFollowing[i]   = schoolCurrentNominations
		completeTable$SchoolPopularity[i]  = schoolCurrentPopularity
		completeTable$SchoolReciprocity[i] = schoolCurrentReciprocity
		completeTable$SchoolConnections[i] = schoolCurrentConnections
		
		# Sport
		completeTable$SportFollowing[i]   = sportCurrentNominations
		completeTable$SportPopularity[i]  = sportCurrentPopularity
		completeTable$SportReciprocity[i] = sportCurrentReciprocity
		completeTable$SportConnections[i] = sportCurrentConnections
		
		# Home
		completeTable$HomeFollowing[i]   = homeCurrentNominations
		completeTable$HomePopularity[i]  = homeCurrentPopularity
		completeTable$HomeReciprocity[i] = homeCurrentReciprocity
		completeTable$HomeConnections[i] = homeCurrentConnections
		
		# Other
		completeTable$OtherFollowing[i]   = otherCurrentNominations
		completeTable$OtherPopularity[i]  = otherCurrentPopularity
		completeTable$OtherReciprocity[i] = otherCurrentReciprocity
		completeTable$OtherConnections[i] = otherCurrentConnections		

		
		
	}
	
	
	# Make the dataframe where we save the info
	networksInfoDF           = DF(TOTAL_NETWORKS, 11)
	colnames(networksInfoDF) = c("Name", "Total Connections",
	                               "Out Degree", "In Degree", "Reciprocity", "Connections",
	                               "SdFollowing",  "SdPopularity",  "SdReciprocity",  "SdConnection",
	                               "Average Path Length")
	
	# Fill the name of the rows
	# ---- Name
	{
		networksInfoDF[1,1] = "Overall"
	    networksInfoDF[2,1] = "Physical"
	    networksInfoDF[3,1] = "School"
	    networksInfoDF[4,1] = "Sport"
	    networksInfoDF[5,1] = "Home"
	    networksInfoDF[6,1] = "Other"
	 }

	# ---- Everything else
	for(i in 1:TOTAL_NETWORKS){

    	# ---- Total Connections
    	networksInfoDF[i,2] = nrow(allEdges[[i]])

    	
    	# ---- Following, popularity, reciprocity and connections and path length
    	{
    		# -------- Overall
    		{
        		networksInfoDF[1,3]  = mean(completeTable$OverallFollowing,   na.rm=TRUE)
        		networksInfoDF[1,4]  = mean(completeTable$OverallPopularity , na.rm=TRUE)
		        networksInfoDF[1,5]  = mean(completeTable$OverallReciprocity, na.rm=TRUE)
		        networksInfoDF[1,6]  = mean(completeTable$OverallConnections, na.rm=TRUE)
		
		        networksInfoDF[1,7]  = sd(completeTable$OverallFollowing,     na.rm=TRUE)
		        networksInfoDF[1,8]  = sd(completeTable$OverallPopularity ,   na.rm=TRUE)
		        networksInfoDF[1,9]  = sd(completeTable$OverallReciprocity,   na.rm=TRUE)
		        networksInfoDF[1,10] = sd(completeTable$OverallConnections,   na.rm=TRUE)

        		networksInfoDF[1,11] = mean_distance(overallGraph, directed = TRUE, unconnected = TRUE)
    		}

		      # -------- Physical
		      {
		        networksInfoDF[2,3]  = mean(completeTable$PhysicalFollowing,   na.rm=TRUE)
		        networksInfoDF[2,4]  = mean(completeTable$PhysicalPopularity , na.rm=TRUE)
		        networksInfoDF[2,5]  = mean(completeTable$PhysicalReciprocity, na.rm=TRUE)
		        networksInfoDF[2,6]  = mean(completeTable$PhysicalConnections, na.rm=TRUE)
		
		        networksInfoDF[2,7]  = sd(completeTable$PhysicalFollowing,     na.rm=TRUE)
		        networksInfoDF[2,8]  = sd(completeTable$PhysicalPopularity ,   na.rm=TRUE)
		        networksInfoDF[2,9]  = sd(completeTable$PhysicalReciprocity,   na.rm=TRUE)
		        networksInfoDF[2,10] = sd(completeTable$PhysicalConnections,   na.rm=TRUE)
		
		        networksInfoDF[2,11] = mean_distance(physicalGraph, directed = TRUE, unconnected = TRUE)
		      }

		      # -------- Home
		      {
		        networksInfoDF[3,3]  = mean(completeTable$HomeFollowing,   na.rm=TRUE)
		        networksInfoDF[3,4]  = mean(completeTable$HomePopularity , na.rm=TRUE)
		        networksInfoDF[3,5]  = mean(completeTable$HomeReciprocity, na.rm=TRUE)
		        networksInfoDF[3,6]  = mean(completeTable$HomeConnections, na.rm=TRUE)
		
		        networksInfoDF[3,7]  = sd(completeTable$HomeFollowing,     na.rm=TRUE)
		        networksInfoDF[3,8]  = sd(completeTable$HomePopularity ,   na.rm=TRUE)
		        networksInfoDF[3,9]  = sd(completeTable$HomeReciprocity,   na.rm=TRUE)
		        networksInfoDF[3,10] = sd(completeTable$HomeConnections,   na.rm=TRUE)
		
		        networksInfoDF[3,11] = mean_distance(homeGraph, directed = TRUE, unconnected = TRUE)
		      }

		      # -------- School
		      {
		        networksInfoDF[4,3]  = mean(completeTable$SchoolFollowing,   na.rm=TRUE)
		        networksInfoDF[4,4]  = mean(completeTable$SchoolPopularity , na.rm=TRUE)
		        networksInfoDF[4,5]  = mean(completeTable$SchoolReciprocity, na.rm=TRUE)
		        networksInfoDF[4,6]  = mean(completeTable$SchoolConnections, na.rm=TRUE)
		
		        networksInfoDF[4,7]  = sd(completeTable$SchoolFollowing,     na.rm=TRUE)
		        networksInfoDF[4,8]  = sd(completeTable$SchoolPopularity ,   na.rm=TRUE)
		        networksInfoDF[4,9]  = sd(completeTable$SchoolReciprocity,   na.rm=TRUE)
		        networksInfoDF[4,10] = sd(completeTable$SchoolConnections,   na.rm=TRUE)
		
		        networksInfoDF[4,11] = mean_distance(schoolGraph, directed = TRUE, unconnected = TRUE)
		      }

		      # -------- Sport
		      {
		        networksInfoDF[5,3]  = mean(completeTable$SportsFollowing,   na.rm=TRUE)
		        networksInfoDF[5,4]  = mean(completeTable$SportsPopularity , na.rm=TRUE)
		        networksInfoDF[5,5]  = mean(completeTable$SportsReciprocity, na.rm=TRUE)
		        networksInfoDF[5,6]  = mean(completeTable$SportsConnections, na.rm=TRUE)
		
		        networksInfoDF[5,7]  = sd(completeTable$SportsFollowing,     na.rm=TRUE)
		        networksInfoDF[5,8]  = sd(completeTable$SportsPopularity ,   na.rm=TRUE)
		        networksInfoDF[5,9]  = sd(completeTable$SportsReciprocity,   na.rm=TRUE)
		        networksInfoDF[5,10] = sd(completeTable$SportsConnections,   na.rm=TRUE)
		
		        networksInfoDF[5,11] = mean_distance(sportsGraph, directed = TRUE, unconnected = TRUE)
		      }

		      # -------- Others
		      {
		        networksInfoDF[6,3]  = mean(completeTable$OtherFollowing,   na.rm=TRUE)
		        networksInfoDF[6,4]  = mean(completeTable$OtherPopularity , na.rm=TRUE)
		        networksInfoDF[6,5]  = mean(completeTable$OtherReciprocity, na.rm=TRUE)
		        networksInfoDF[6,6]  = mean(completeTable$OtherConnections, na.rm=TRUE)
		
		        networksInfoDF[6,7]  = sd(completeTable$OtherFollowing,     na.rm=TRUE)
		        networksInfoDF[6,8]  = sd(completeTable$OtherPopularity ,   na.rm=TRUE)
		        networksInfoDF[6,9]  = sd(completeTable$OtherReciprocity,   na.rm=TRUE)
		        networksInfoDF[6,10] = sd(completeTable$OtherConnections,   na.rm=TRUE)
		
		        networksInfoDF[6,11] = mean_distance(othersGraph, directed = TRUE, unconnected = TRUE)
		      }

    	}

	}

    
    # Round the numbers
    networksInfoDF[,3]  = round(networksInfoDF[,3], 2)
    networksInfoDF[,4]  = round(networksInfoDF[,4], 2)
    networksInfoDF[,5]  = round(networksInfoDF[,5], 2)
    networksInfoDF[,6]  = round(networksInfoDF[,6], 2)
    
    networksInfoDF[,7]  = round(networksInfoDF[,7], 2)
    networksInfoDF[,8]  = round(networksInfoDF[,8], 2)
    networksInfoDF[,9]  = round(networksInfoDF[,9], 2)
    networksInfoDF[,10] = round(networksInfoDF[,10], 2)
    
    networksInfoDF[,11] = round(networksInfoDF[,11], 2)
    
    # Add the SDs to the average as strings
    for(i in 1:TOTAL_NETWORKS){
	    networksInfoDF[i,3]  = paste0(networksInfoDF[i,3], " ± " , networksInfoDF[i,7])
	    networksInfoDF[i,4]  = paste0(networksInfoDF[i,4], " ± " , networksInfoDF[i,8])
	    networksInfoDF[i,5]  = paste0(networksInfoDF[i,5], " ± " , networksInfoDF[i,9])
	    networksInfoDF[i,6]  = paste0(networksInfoDF[i,6], " ± " , networksInfoDF[i,10])
    	
    }
    

    	
    writeTableLATEX(networksInfoDF, NETWORK_FOLDER_TABLES,
                    tableCaption = "Basic network statistics.",
                    widthProportion = 0.8, heightProportion = 0.05)
    
    write.csv2(networksInfoDF, paste0(NETWORK_FOLDER_TABLES,"basicNetworkStatistics.csv") )
    
    
    
 
    # Find the different centralities for our graph, normalize in all cases
	aa = igraph::closeness(overallGraph, mode = "all"          )
    bb = igraph::betweenness(overallGraph, directed = FALSE    )
    cc = igraph::power_centrality(overallGraph, exponent = 0.9)
    dd = igraph::eigen_centrality(overallGraph                 )

    
    # Put it into a dataframe for plotting
    centralitiesDF = completeTable[, c(IDIndex, highSchoolIndex, sexIndex, overallConnectionsIndex)]
    # -- The direct numbers are bad for describing node diameter, so reescale them for plotting only
    centralitiesDF$Closeness  = aa
    centralitiesDF$Betweeness = bb
    centralitiesDF$Eigen      = dd$vector
    centralitiesDF$Bonacich   = cc
    
	# Get average connectivity per highschool and sex
    highschoolConnectivityDF = DF(11,6)
    colnames(highschoolConnectivityDF) = c("Highschool", "Degree", "Closeness", "Between", "Eigen", "Bonacich")

    myHighschoolsNames = getCategories(completeTable, highSchoolIndex)
    totalHS            = length(myHighschoolsNames)
    
    highschoolConnectivityDF[,1] = c(myHighschoolsNames, "Men", "Women", "Population")

    # For the whole population
    # Find the average of
    # -- Degree
    highschoolConnectivityDF[11,2] = round( mean(centralitiesDF[,4], na.rm = TRUE), 2)
    # -- Clossenness
    highschoolConnectivityDF[11,3] = round( mean(centralitiesDF[,5], na.rm = TRUE), 4)
    # -- Between
    highschoolConnectivityDF[11,4] = round( mean(centralitiesDF[,6], na.rm = TRUE), 0)
    # -- Eigen
    highschoolConnectivityDF[11,5] = round( mean(centralitiesDF[,7], na.rm = TRUE), 4)
    # -- Italian
    highschoolConnectivityDF[11,6] = round( mean(centralitiesDF[,8], na.rm = TRUE), 4)    
    
    # For each highschool
    for(i in 1:totalHS){
    
    	# Get the school
    	currentHS = myHighschoolsNames[i]
    	
    	# Get the subset
    	currentSubsetDF = keepCategory(centralitiesDF, 2, currentHS)

    	# Find the average of
    	# -- Degree
    	highschoolConnectivityDF[i,2] = round( mean(currentSubsetDF[,4], na.rm = TRUE), 2)
    	# -- Clossenness
    	highschoolConnectivityDF[i,3] = round( mean(currentSubsetDF[,5], na.rm = TRUE), 4)
    	# -- Between
    	highschoolConnectivityDF[i,4] = round( mean(currentSubsetDF[,6], na.rm = TRUE), 0)
    	# -- Eigen
    	highschoolConnectivityDF[i,5] = round( mean(currentSubsetDF[,7], na.rm = TRUE), 4)
    	# -- Italian
    	highschoolConnectivityDF[i,6] = round( mean(currentSubsetDF[,8], na.rm = TRUE), 4)

    }    

    # For each sex
    for(i in 1:2){
    
    	# Get the school
    	currentSex = "Man"
    	if(i == 2) currentSex = "Woman"
    	
    	# Get the subset
    	currentSubsetDF = keepCategory(centralitiesDF, 3, currentSex)

    	# Find the average of
    	# -- Degree
    	highschoolConnectivityDF[8+i,2] = round( mean(currentSubsetDF[,4], na.rm = TRUE), 2)
    	# -- Clossenness
    	highschoolConnectivityDF[8+i,3] = round( mean(currentSubsetDF[,5], na.rm = TRUE), 4)
    	# -- Between
    	highschoolConnectivityDF[8+i,4] = round( mean(currentSubsetDF[,6], na.rm = TRUE), 0)
    	# -- Eigen
    	highschoolConnectivityDF[8+i,5] = round( mean(currentSubsetDF[,7], na.rm = TRUE), 4)
    	# -- Italian
    	highschoolConnectivityDF[8+i,6] = round( mean(currentSubsetDF[,8], na.rm = TRUE), 4)

    }      
    
    write.csv2(highschoolConnectivityDF, paste0(NETWORK_FOLDER_TABLES,"basicNetworkCentralities.csv") )
 
    # Find the different centralities for our graph, normalize in all cases
	#aa = igraph::closeness(overallGraph, mode = "all",          normalized = TRUE)
    #bb = igraph::betweenness(overallGraph, directed = FALSE,    normalized = TRUE)
    #cc = igraph::power_centrality(overallGraph, exponent = 0.9, rescale = TRUE)
    #dd = igraph::eigen_centrality(overallGraph,                 scale = TRUE)    
               
    
    # Put it into a dataframe for plotting
    centralitiesDF = completeTable[, c(IDIndex, highSchoolIndex, sexIndex, overallConnectionsIndex)]
    # -- The direct numbers are bad for describing node diameter, so reescale them for plotting only
    centralitiesDF$Closeness  = aa
    centralitiesDF$Betweeness = bb
    centralitiesDF$Bonacich   = cc
    centralitiesDF$Eigen      = dd$vector
    
    # -- Do some reescaling for node sizes
    centralitiesDF$BetweenessSize = centralitiesDF$Betweeness/1000 + 1
    centralitiesDF$EigenSize      = centralitiesDF$Eigen * 10 + 3
    
    # Delete the outliers from the network
    temporalEdgesDF = overallEdgesDF
    
    problemNodesIDs  = c(274, 398, 405, 617, 886)
    deleteEdges = deleteConnections(temporalEdgesDF, problemNodesIDs, problemNodesIDs)[[2]]
    temporalEdgesDF = temporalEdgesDF[!deleteEdges,]
    
    
    # Closeness
    plotResults = doGraphPlot(temporalEdgesDF, centralitiesDF, NETWORK_FOLDER_IMAGES,
    						  suppressAloneNode = TRUE,
                              #sizeVariableIndex      = 5,
            			      highlightVariable      = 5,
            			      #colorVectorHighlight   = '#808080',
                              selectedLayouts        = 'mds',
                              plotTitle              = "Overall Network with nodes proportional to normalized closeness centrality",
    						  plotSubtitle           = "How fast can the node reach other nodes in the network? (higher is faster)",
            				  plotTheme              = "blank",
                              #overrideTableName      = currentOverridedPlotName,
            				  overrideLegendSize     = 5,
                    		  overrideLegendPosition = c(0.9, 0.9))
    
    # Between
    plotResults = doGraphPlot(overallEdgesDF, centralitiesDF, NETWORK_FOLDER_IMAGES,
                              sizeVariableIndex      = 9,
            			      highlightVariable      = 6,
            			      #colorVectorHighlight  = baseColor,
                              selectedLayouts        = 'mds',
                              plotTitle              = "Overall Network with nodes proportional to normalized between centrality",
    						  plotSubtitle           = "Is the node connecting subnetworks toguether? (higher is yes)",
            				  plotTheme              = "blank",
                              #overrideTableName      = currentOverridedPlotName,
            				  overrideLegendSize     = 5,
                    		  overrideLegendPosition = c(0.9, 0.9))
    
    # Eigen
    plotResults = doGraphPlot(overallEdgesDF, centralitiesDF, NETWORK_FOLDER_IMAGES,
                              sizeVariableIndex      = 10,
            			      highlightVariable      = 8,
            			      #colorVectorHighlight  = baseColor,
                              selectedLayouts        = 'mds',
                              plotTitle              = "Overall Network with nodes proportional to eigencentrality",
    						  plotSubtitle           = "How 'well connected' is this node? (higher is better)",
            				  plotTheme              = "blank",
                              #overrideTableName      = currentOverridedPlotName,
            				  overrideLegendSize     = 5,
                    		  overrideLegendPosition = c(0.9, 0.9))     
 
     
       
}

# ------------------------------------------------------------------------------
# Histograms
# ------------------------------------------------------------------------------

    # -- # Histogram with number of friends
    {
    
        doHistogramPlot2(completeTable, overallConnectionsIndex, NETWORK_FOLDER_IMAGES,
                         colorsVector   = COLOR_FRIENDSHIP,
                         binsWidth      = 1,
                         binBorderColor = "#333333",
                         plotTitle      = "Histogram with the total connections per student",
                         plotSubtitle   = "",
                         plotXLabel     = "Total friends", plotYLabel = "N",
        	)       
        
    }

# ------------------------------------------------------------------------------
# Homophily
# ------------------------------------------------------------------------------
{

	completeHomophilyV3(overallGraph, completeTable, sexIndex)
	
}

# ------------------------------------------------------------------------------
# Reachability
# ------------------------------------------------------------------------------
{
	doReachabilityPlot(overallEdgesDF , completeTable, NETWORK_FOLDER_IMAGES,
				       totalSteps = 15,
		
	    			   plotTitle = "Reachability plot for the overall network",
					   plotSubtitle = "",
					   plotXLabel = "Total Step", plotYLabel = "Network Coverage",
	                                
					   plotTheme = "simple",
	                   overrideTableName = "reachplot")
}

# ------------------------------------------------------------------------------
# Simulation
# ------------------------------------------------------------------------------
{

	
	naturalResults = doSimulationPlot2(overallEdgesDF, completeTable, NETWORK_FOLDER_IMAGES,
					                   totalSteps = 60, totalSimulations = 200,
		
       	                               plotTitle    = "Simulation for an unknown disease in the overall network",
		                               plotSubtitle = "Probability of: Transmission = 0.3, Immunity = 0.1, Dead = 0.01",
					                   plotXLabel   = "Total Steps",
					                   plotYLabel   = "Network proportion",
					                   plotTheme    = "simple",
	                                   overrideTableName = "simulationplot")
	
	
	vaccinesResults = doSimulationPlot2(overallEdgesDF, completeTable, NETWORK_FOLDER_IMAGES,
					                    totalSteps = 60, totalSimulations = 200,
		
            					        vaccineStep = 10,
		                                totalVaccinesSteps = 30,
		
	                                    plotTitle    = "Simulation for an unknown disease in the overall network",
		                                plotSubtitle = "Probability of: Transmission = 0.3, Immunity = 0.1, Dead = 0.01, vaccine (t) = 10, vaccine (n) = 30",
					                    plotXLabel   = "Total Steps",
					                    plotYLabel   = "Network proportion",
					                    plotTheme    = "simple",
	                                    overrideTableName = "simulationvaccinesplot")	
	
	bestResults = doSimulationPlot2(overallEdgesDF, completeTable, NETWORK_FOLDER_IMAGES,
				  	                totalSteps = 60, totalSimulations = 200,
		
					                vaccineStep = 10,
		                            totalVaccinesSteps = 30,
					                forceVaccines = TRUE,
		
	                                plotTitle    = "Simulation for an unknown disease in the overall network",
		                            plotSubtitle = "Probability of: Transmission = 0.3, Immunity = 0.1, Dead = 0.01, vaccine (t) = 10, vaccine (n) = 30, best candidates",
					                plotXLabel   = "Total Steps",
					                plotYLabel   = "Network proportion",
					                plotTheme    = "simple",
	                                overrideTableName = "simulationvaccinesForcedplot")		
	
	bestResults = doSimulationPlot2(overallEdgesDF, completeTable, NETWORK_FOLDER_IMAGES,
				  	                totalSteps = 60, totalSimulations = 200,
		
					                vaccineStep = 10,
		                            totalVaccinesSteps = 30,
					                forceVaccines = TRUE,
		
	                                plotTitle    = "Simulation for an unknown disease in the overall network",
		                            plotSubtitle = "Probability of: Transmission = 0.3, Immunity = 0.1, Dead = 0.01, vaccine (t) = 10, vaccine (n) = 30, best between",
					                plotXLabel   = "Total Steps",
					                plotYLabel   = "Network proportion",
					                plotTheme    = "simple",
	                                overrideTableName = "simulationvaccinesBetweenplot")	
	
	bestResults = doSimulationPlot2(overallEdgesDF, completeTable, NETWORK_FOLDER_IMAGES,
				  	                totalSteps = 60, totalSimulations = 200,
		
					                vaccineStep = 10,
		                            totalVaccinesSteps = 30,
					                forceVaccines = TRUE,
		
	                                plotTitle    = "Simulation for an unknown disease in the overall network",
		                            plotSubtitle = "Probability of: Transmission = 0.3, Immunity = 0.1, Dead = 0.01, vaccine (t) = 10, vaccine (n) = 30, best closeness",
					                plotXLabel   = "Total Steps",
					                plotYLabel   = "Network proportion",
					                plotTheme    = "simple",
	                                overrideTableName = "simulationvaccinesClosseness")	
	
	bestResults = doSimulationPlot2(overallEdgesDF, completeTable, NETWORK_FOLDER_IMAGES,
				  	                totalSteps = 60, totalSimulations = 200,
		
					                vaccineStep = 10,
		                            totalVaccinesSteps = 30,
					                forceVaccines = TRUE,
		
	                                plotTitle    = "Simulation for an unknown disease in the overall network",
		                            plotSubtitle = "Probability of: Transmission = 0.3, Immunity = 0.1, Dead = 0.01, vaccine (t) = 10, vaccine (n) = 30, best eigen",
					                plotXLabel   = "Total Steps",
					                plotYLabel   = "Network proportion",
					                plotTheme    = "simple",
	                                overrideTableName = "simulationvaccinesEigen")		
	                              

		
}


# Communities

cluster_leiden(overallGraph)

tempGraph = overallGraph


fc = cluster_walktrap(as.undirected(tempGraph))
fc = fastgreedy.community(as.undirected(tempGraph))
V(tempGraph)$color <- ifelse(membership(fc)==1,"red","blue")
plot(tempGraph)


communitySwapDF = DF(8,4)
colnames(communitySwapDF) = c("Highschool", "1", "2", "3")

