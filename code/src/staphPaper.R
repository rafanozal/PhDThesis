# This script generates the STAPH social paper only.
# 
# Set working directory to file location since R doesn't like to this
# automatically anymore.
# -----------------------------------------------------------------------------
#this.dir = dirname(parent.frame(2)$ofile)
#setwd(this.dir)

#library(ergm)
library(statnet)
library(ggpubr) # ggarrange()

source("constants.R",        encoding="utf-8") # This set the working directory to the root directory,


source( paste0(MAIN_CODE_FOLDER,"loadDataV2.R"),                           encoding="utf-8") # Ensure that the data is loaded
source( paste0(MAIN_CODE_FOLDER,"indexesV2.R"),                            encoding="utf-8") # Ensure that the indexes are loaded

source( paste0(MAIN_CODE_FOLDER,"lib/analysis/analysisNetworks.R"),        encoding="utf-8") # Import each of the needed libraries
source( paste0(MAIN_CODE_FOLDER,"lib/analysis/analysisCategorical.R"),     encoding="utf-8")

source( paste0(MAIN_CODE_FOLDER,"lib/basic/toolsSummarizers.R"),           encoding="utf-8")
source( paste0(MAIN_CODE_FOLDER,"lib/basic/toolsNetworks.R"),              encoding="utf-8")

source( paste0(MAIN_CODE_FOLDER,"lib/plotting/toolsPlottingBarplots.R"),   encoding="utf-8")
source( paste0(MAIN_CODE_FOLDER,"lib/plotting/toolsPlottingHistograms.R"), encoding="utf-8")
source( paste0(MAIN_CODE_FOLDER,"lib/plotting/toolsPlottingNetwork.R"),    encoding="utf-8")



# How many simulations do you want to do in the network simulation part.
# 1000 is good for results
# 10 is good for debugging
totalSimulations = 10

# -----------------------------------------------------------------------------
# Declare some extra constant variables. This is just to set the name of the
# files that are generated in this script
# -----------------------------------------------------------------------------
{
    
	# Define where do you want to save the results
	PAPER_FOLDER = file.path(paste(RESULT_FOLDER, "aureus/", sep = ""))
	
    # -- Xi2 tables
    PREVALENCE_VARIABLE_TABLE_FILEPATH = paste0(PAPER_FOLDER,"prevalences.csv")
    PREVALENCE_HORMONAL_TABLE_FILEPATH = paste0(PAPER_FOLDER,"prevalences_hormonal.csv")
    XI2_VARIABLE_TABLE_FILEPATH        = paste0(PAPER_FOLDER,"xi2.csv")
    XI2_HORMONAL_TABLE_FILEPATH        = paste0(PAPER_FOLDER,"xi2_hormonal.csv")
 
    # -- Graph plots
    ALLGRAPH_FILEPATH                  = file.path(paste(PAPER_FOLDER, "3x2Graphs.png", sep = ""))
       
    # -- Network simulations
    SPABIAS_FILEPATH                   = paste0(PAPER_FOLDER,"spaBias.csv")
    
    # -- How many friends got swabbed at the same time as you
    FRIENDSHIP_SWAB_FILEPATH           = paste0(PAPER_FOLDER,"friendshipSwab.csv")
    
    # -- Popularity
    POPULARITY_FILEPATH                = paste0(PAPER_FOLDER,"popularity.csv")
    
}

# -----------------------------------------------------------------------------
# Prepare the proper datasets and what variables are we going to analyze automatically
#
#     - In here we are just adding the hormonal info from the database to the
#       complete table so R can works properly.
#
#     - Also selecting the interesting independent variables, sex, BMI, ...
#
#     - Also selecting, which target variables we want. In our case only two
#       nasal direct culture carrier, and nasal enrich carrier, but we can do
#       it with as many as we want.
# -----------------------------------------------------------------------------
{
    
    # Count how many people we have in total
    TOTAL_SUBJECTS = nrow(completeTable)
    
    # Add hormonal information to each menstruating woman
    {

        # Add the hormonal contraceptives information to this table
        completeTable$HormonalContraceptives = "None or non-hormonal"
    
        # For each person from which we have any info about hormonal contraceptives
        # add it to the table.
        for(i in 1:nrow(contraceptivesDBDF)){

            currentID            = contraceptivesDBDF$ID[i]
            currentContraceptive = contraceptivesDBDF$Hormonal[i]
            
            # I HATE R SO MUCH! Look at this!
            # You need to convert a string to string, because otherwise, this get
            # equal to a number that you need to convert later into a proper string
            # again.
            #
            # And on top of that, you lose the levels() information while doing this
            # There is a proper way of doing this, which is with enum(), which has
            # existed in C since the beggining of time. WHY YOU DON'T JUST COPY 
            # WHAT IT ACTUALLY WORK AND HAS BEEN DONE BEFORE!!!??? I hereby curse
            # the inventor of R to 1000 year of pain.
            currentContraceptive = as.character(currentContraceptive)
            
            # If is not the default option, added to the DF
            if(currentContraceptive!="Non-hormonal"){
                completeTable$HormonalContraceptives[currentID] = as.character(currentContraceptive)    
            }
            
        }
        
        # Set the proper levels of hormonal contraceptives AGAIN as stated before
        # because R sucks. (Also, why can't we have objects T_T is very annoying
        # to do X = X whatever all the time, it needs to be X.setFactors()
        completeTable$HormonalContraceptives = factor(completeTable$HormonalContraceptives , 
                                                      levels = c("None or non-hormonal", "Progestin",
                                                                 "Low Estradiol", "High Estradiol", "Unknown"))
    
        # Create the special table for women and contraceptives only
        # -- Filter by women
        womenOnlyCopyTable = completeTable[ completeTable$Sex == "Woman",]
        # -- Filter by menstruating women only
        womenMenstruatingTable = womenOnlyCopyTable[womenOnlyCopyTable$MenstruationStart == "Yes",]
        
    }
    
    # Choose which variables do you want to study
    {

        # -- Sorted by sex, antropometrics, drugs, and highschool
        explanatoryIndexes      = c(sexIndex, BMICatIndex,
                                    smokeIndex, snuffIndex, alcoholIndex,
                                    sportsIndex, hsProgrameIndex)
        totalExplanatoryIndexes = length(explanatoryIndexes)
        explanatoryNames        = colnames(completeTable)[explanatoryIndexes]
        
        # -- Get the contraceptive index, which is a bit special as it doesn't
        #    apply for the complete table, for the women menstruating only table
        hormonalIndex = grep("^HormonalContraceptives$" ,   colnames(womenMenstruatingTable))

        # Choose which variables do we want to get results about
        consequenceIndexes      = c(nasalDirectCarrierIndex, nasalEnrichmentCarrierIndex)
        totalConsequenceIndexes = length(consequenceIndexes)
        consequenceNames        = colnames(completeTable)[consequenceIndexes]
        
    }

    # Create the edges of people who has valid spa-types, and are sharing the
    # same SPA-types, based only on SPAT1 variable.
    {
        spaTableOnly    = subset(completeTable, completeTable[,spaT1IndexComplete] != "")
        nonSPATableOnly = subset(completeTable, completeTable[,spaT1IndexComplete] == "")
        
        # Add the information of Same Spa-type to each relationship
        # -- Delete the non spa-type relationships
        nonSPAIDs              = nonSPATableOnly$ID
        deleteThisLists        = deleteConnections(overallEdgesDF,  nonSPAIDs, nonSPAIDs)[[1]]
        spaOnlyOverallEdgesDF  = overallEdgesDF[  !deleteThisLists, ]
        deleteThisLists        = deleteConnections(physicalEdgesDF, nonSPAIDs, nonSPAIDs)[[1]]
        spaOnlyPhysicalEdgesDF = physicalEdgesDF[ !deleteThisLists, ]
        deleteThisLists        = deleteConnections(schoolEdgesDF,   nonSPAIDs, nonSPAIDs)[[1]]
        spaOnlySchoolEdgesDF   = schoolEdgesDF[    !deleteThisLists, ]
        deleteThisLists        = deleteConnections(sportsEdgesDF,   nonSPAIDs, nonSPAIDs)[[1]]
        spaOnlySportsEdgesDF   = sportsEdgesDF[   !deleteThisLists, ]
        deleteThisLists        = deleteConnections(homeEdgesDF,     nonSPAIDs, nonSPAIDs)[[1]]
        spaOnlyHomeEdgesDF     = homeEdgesDF[     !deleteThisLists, ]
        deleteThisLists        = deleteConnections(otherEdgesDF,    nonSPAIDs, nonSPAIDs)[[1]]
        spaOnlyOthersEdgesDF   = otherEdgesDF[    !deleteThisLists, ]
        # -- Add the same to same relationships (TRUE / FALSE)
        spaOnlyOverallEdgesDF$SameSPAT1 = addEdgeRelationship(spaOnlyOverallEdgesDF, completeTable, spaT1IndexComplete)
        sameSPAT1Index                  = grep("^SameSPAT1$",   colnames(spaOnlyOverallEdgesDF))            
    }
    
    # Add the edge relationship information on whether two people share
    # the same SPA-type or not in the school network. This include people with
    # no SPA type.
    schoolEdgesDF$SameSPAT1   = addEdgeRelationship(schoolEdgesDF, completeTable, spaT1IndexComplete)
}

# ------------------------------------------------------------------------------
# Prevalence + Xi² Analysis
# ------------------------------------------------------------------------------
{
 
    # Generate the prevalence table
    prevalenceDF = summarizePrevalences(completeTable, explanatoryIndexes, consequenceIndexes, 
                                        prevalenceTarget="Positive", showInPercentage= TRUE, 
                                        roundMe = 1, skipUnknowns=TRUE)

    # Generate the xi² results summary
    xi2SummaryDF = multiplesXis(completeTable, explanatoryIndexes, consequenceIndexes)

    
    
    # Repeat the same thing for the hormonal contraceptives
    prevalenceHDF = summarizePrevalences(womenMenstruatingTable, hormonalIndex, consequenceIndexes, 
                                         prevalenceTarget="Positive", showInPercentage= TRUE, 
                                         roundMe = 1, skipUnknowns=TRUE)
    
    xi2SummaryHDF = multiplesXis(womenMenstruatingTable, hormonalIndex, consequenceIndexes)


    # Save all the tables into disk
    write.csv2(prevalenceDF,  PREVALENCE_VARIABLE_TABLE_FILEPATH)
    write.csv2(prevalenceHDF, PREVALENCE_HORMONAL_TABLE_FILEPATH)
    write.csv2(xi2SummaryDF,  XI2_VARIABLE_TABLE_FILEPATH)
    write.csv2(xi2SummaryHDF, XI2_HORMONAL_TABLE_FILEPATH)
            
    # If you want to see each xi table one by one, turn this to TRUE
    if(FALSE){
    
        # Big table
        allXi2List = completeXi(completeTable, explanatoryIndexes, consequenceIndexes)    
        
        for(i in 1: (totalExplanatoryIndexes * totalConsequenceIndexes)){
            
            print(allXi2List[[i]])
            
        }
        
        # Contraceptives table
        allXi2HList = completeXi(womenMenstruatingTable, hormonalIndex, consequenceIndexes)
        
        print(allXi2HList[[1]])
        print(allXi2HList[[2]])
            
    }
    
        
}

# ------------------------------------------------------------------------------
# SPA tables and barplots
# ------------------------------------------------------------------------------
{
    # --------
    # NOTES:
    # --------
    #
    # throatDirectCarrierIndex and throatEnrichmentCarrierIndex both refers
    # to the persistent carrier for direct culture, and persistent carrier 
    # for enrichment broth, both in throat (obviously).
    #
    # Variable naming can be a bit confusing since we change the definition
    # back and forth, and the variable of interest.
    #
    # --------
    
    
    # Check what Johanna said:
    #
    #     - "All positive throats samples are spa-typed" (sort of true)
    #     - "Blanks means Nasal carrier positive, throat carrier negative"
    #
    # In here, we are considered the T1 as the spatype variable, ignoring N1, N2, and T2
    {
        # First we are going to transform everything that isn't "" to "SPA-typed"
        # and "" to "not SPA-typed"
        tempTable = completeTable
        tempTable[completeTable[,spaT1IndexComplete] == "", spaT1IndexComplete] = "not SPA-typed"
        tempTable[completeTable[,spaT1IndexComplete] != "", spaT1IndexComplete] = "SPA-typed"
        
        completeTable$S1_Enrich_CoagulaseThroat
        
        # Checking for all combinations
        # -- S1 direct throat
        throatDirectColonizeS1Index = grep("^S1_Direct_CoagulaseThroat$", colnames(completeTable))
        summarizeBicategorical(tempTable, throatDirectColonizeS1Index, spaT1IndexComplete)
        # -- S1 enrich throat
        throatEnrichColonizeS1Index = grep("^S1_Enrich_CoagulaseThroat$", colnames(completeTable))
        summarizeBicategorical(tempTable, throatEnrichColonizeS1Index, spaT1IndexComplete)
        # -- S2 direct throat
        throatDirectColonizeS2Index = grep("^S2_Direct_CoagulaseThroat$", colnames(completeTable))
        summarizeBicategorical(tempTable, throatDirectColonizeS2Index, spaT1IndexComplete)
        # -- S2 enrich throat
        throatEnrichColonizeS2Index = grep("^S2_Enrich_CoagulaseThroat$", colnames(completeTable))
        summarizeBicategorical(tempTable, throatEnrichColonizeS2Index, spaT1IndexComplete)        
        # -- persistent direct throat
        summarizeBicategorical(tempTable, throatDirectCarrierIndex, spaT1IndexComplete)
        # -- persistent enrich throat
        summarizeBicategorical(tempTable, throatEnrichmentCarrierIndex, spaT1IndexComplete)
        
        # So yes, Johanna is sort of correct, all positives are spa-typed, but many negatives also.
        
        # For the other question:
        #     - "Blanks means Nasal carrier positive, throat carrier negative"
        
        #
        # The summaries for such combinations are here
        negativeDirectThroatTable = subset(completeTable, completeTable[,throatDirectCarrierIndex]                            == "Negative")
        negativeDirectThroatTable = subset(negativeDirectThroatTable, negativeDirectThroatTable[,nasalDirectCarrierIndex]     == "Positive")
        negativeEnrichThroatTable = subset(completeTable, completeTable[,throatEnrichmentCarrierIndex]                        == "Negative")
        negativeEnrichThroatTable = subset(negativeEnrichThroatTable, negativeEnrichThroatTable[,nasalEnrichmentCarrierIndex] == "Positive")
        
        summarizeCategorical(negativeDirectThroatTable, spaT1IndexComplete, crop = 15)
        summarizeCategorical(negativeEnrichThroatTable, spaT1IndexComplete, crop = 15)       
        
        # No, this is not true, many positive nasal and negative throat are included.
        
    }
    
    # Now, the editor wanted to change the table so the non-typed doesn't appear in the table
    # Furthermore, we divided the tables into direct and enrichment culture, and the editor doesn't want that either
    # so we are summarizing the most prevalent spa-types in one table only
    {
     
        # I made 3 versions of this for Dina:
        
        # The plot needs to coincide with the summarize we just did,
        # so the modalities are addjusted accordingly after each
        
        # Remake of the old table, proper align with the barplot (done in Photoshop, as the rest)
        tempTable = completeTable
        tempTable = tempTable[tempTable$E_NasalCarrier == "Positive",]
        spaTargetModalitiesEnrichNasal = summarizeCategorical(tempTable, spaT1IndexComplete, crop = 15, roundMe = 2)        
        
        tempTable = tempTable[tempTable[,spaT1IndexComplete] %in% spaTargetModalitiesEnrichNasal$Modality,]
        
        doBarPlotV2(tempTable, spaT1IndexComplete, AUREUS_FOLDER ,
    				rotation     = TRUE,        
                    sort         = "ascending", 
					labelsAlignment = "above",  
                    plotTitle = "Old",
					overrideTableName   = "OldTable",
                    overrideImageWidth  = 8,
                    overrideImageHeight = 8)

        
        writeTableLATEX(spaTargetModalitiesEnrichNasal, AUREUS_FOLDER, tableCaption = "Original",
                        widthProportion = 0.9, heightProportion = 0.35)
        
        # Using the enrichment persistent throat as filter
        tempTable = completeTable
        tempTable = tempTable[tempTable$E_ThroatCarrier == "Positive",]
        spaTargetModalitiesEnrichThroat = summarizeCategorical(tempTable, spaT1IndexComplete, crop = 15, roundMe = 2)        
        
        tempTable = tempTable[tempTable[,spaT1IndexComplete] %in% spaTargetModalitiesEnrichThroat$Modality,]
        
        
        doBarPlotV2(tempTable, spaT1IndexComplete, AUREUS_FOLDER ,
    				rotation     = TRUE,        
                    sort         = "ascending", 
					labelsAlignment = "above",  
					plotTitle = "SPA types in Enriched Throat Carriers ", overrideTableName = "New1Table",
                    overrideImageWidth = 8, overrideImageHeight = 8)
        
        
        writeTableLATEX(spaTargetModalitiesEnrichThroat, AUREUS_FOLDER, tableCaption = "New",
                        widthProportion = 0.9, heightProportion = 0.35)
        
        # Using the enrichment persistent throat as filter
        tempTable = completeTable
        tempTable = tempTable[tempTable$S1_E_ThroatColonize == "Positive",]
        spaTargetModalitiesColonizedThroat = summarizeCategorical(tempTable, spaT1IndexComplete, crop = 15, roundMe = 2)
        
        doBarPlotV2(tempTable, spaT1IndexComplete, AUREUS_FOLDER ,
    				rotation     = TRUE,        
                    sort         = "ascending", 
					labelsAlignment = "above",  
        			minValue = 10,
					plotTitle = "SPA types in Enriched Throat Colonized", overrideTableName = "New2Table",
                    overrideImageWidth = 8, overrideImageHeight = 8)
        
        writeTableLATEX(spaTargetModalitiesColonizedThroat, AUREUS_FOLDER, tableCaption = "New2",
                        widthProportion = 0.9, heightProportion = 0.35)
        
    }

    
}

# ------------------------------------------------------------------------------
# All graph plots
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

        # ---- PNG
        ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows)
        ggsave(ALLGRAPH_FILEPATH, width = 10, height = 16)
        
    }
    
    # Figure for the overall network with the Enrichment Carrier
    {
        
        currentPlotTitle          = "Overall Network for enriched nasal carriage status"
        currentOverridedPlotName  = "OverallDFCarrierOnly"
        currentOverrideCaption    = "Graph with the network for carrier status."
    
        # Change the name of the variable
        # (later change it back to the original value)
        originalColumnNames       = colnames(completeTable)
        colnames(completeTable)[nasalEnrichmentCarrierIndex] = "Enriched nasal carriage status"
        
        COLOR_VECTOR_POSTER = c("#F77FEE", "#9bd0f0")
        
        doGraphPlot(overallEdgesDF,  completeTable, PAPER_FOLDER,
                    highlightVariable = nasalEnrichmentCarrierIndex,
                    #colorVectorHighlight = COLOR_VECTOR_CARRIER,
        	        colorVectorHighlight = COLOR_VECTOR_POSTER,
                    sizeVariableIndex = overallConnectionsIndex,
                    manualLayout = myConstantLayoutA,
                    plotTitle    = currentPlotTitle,
                    plotSubtitle = "Size based on number of undirected relationships",
        			plotTheme    = "blank",
                    overrideTableName  = currentOverridedPlotName,
                    overrideCaption    = currentOverrideCaption,
                    overrideLegendSize = 5,
                    overrideLegendPosition = c(0.2, 0.1)) 
    
        # (Change the values back)
        colnames(completeTable)    = originalColumnNames
        
    }
    
    # Figure for the highschool network with SPA-types edges
    {
    
        # Set the plot title and subtitle  
        currentPlotTitle          = "School network, highchools in nodes and same spa-type in edges."
        currentPlotSubtitle       = "MDS layout, with isolated nodes hidden (n = 21)"
      
        # Change the variables names so they are a bit more readable to humans
        # (This changes are not reverted back later, since we are using a copy of the table)
        schoolEdgesDFModfied = schoolEdgesDF
        schoolEdgesDFModfied[schoolEdgesDFModfied$SameSPAT1 == TRUE,]$SameSPAT1   = "Yes"
        schoolEdgesDFModfied[schoolEdgesDFModfied$SameSPAT1 == FALSE,]$SameSPAT1  = "No"
        colnames(schoolEdgesDFModfied)[4] = "Same spa-type"
        
        # Do the actual plot
        doGraphPlot(schoolEdgesDFModfied,  completeTable, PAPER_FOLDER,
                    sizeVariableIndex  = overallConnectionsIndex,
                    highlightVariable  = highSchoolIndex,
                    edgesHighlight     = sameSPAT1Index,
                    colorVectorEdge    = rev(COLOR_VECTOR_SAME_RELATIONSHIP),
                    plotTitle          = currentPlotTitle,
                    plotSubtitle       = currentPlotSubtitle,
                    overrideLegendSize = 5,
                    #overrideLegendPosition = c(0.9, 0.9),
                    suppressAloneNode  = TRUE)
    }

    # Figure for the societal highschool network where each highschool has it
    # own circle.
    {
        
        # Get the societal layout from our self-made function
        highSchoolLayout = createCategoricalLayout(spaOnlyOverallEdgesDF, spaTableOnly, highSchoolIndex)
        myGraph = graph_from_data_frame(spaOnlyOverallEdgesDF, vertices = spaTableOnly, directed = FALSE)
        myConstantLayoutB = create_layout(graph = myGraph, layout = 'mds')
        myConstantLayoutB$x = highSchoolLayout[[1]]$x
        myConstantLayoutB$y = highSchoolLayout[[1]]$y
        
        # Prepare the plot titles
        currentPlotTitle          = "School network, highchools in nodes and same spa-type in edges."
        currentPlotSubtitle       = "Only nodes with a valid spa-type are shown (n = 746)."
        
        # Change the variables names so they are a bit more readable to humans
        # (This changes are not reverted back later, since we are using a copy of the table)
        spaOnlyOverallEdgesDFModified = spaOnlyOverallEdgesDF
        spaOnlyOverallEdgesDFModified[spaOnlyOverallEdgesDFModified$SameSPAT1 == TRUE,]$SameSPAT1   = "Yes"
        spaOnlyOverallEdgesDFModified[spaOnlyOverallEdgesDFModified$SameSPAT1 == FALSE,]$SameSPAT1  = "No"
        colnames(spaOnlyOverallEdgesDFModified)[4] = "Same spa-type"        
        
        doGraphPlot(spaOnlyOverallEdgesDFModified,  spaTableOnly, PAPER_FOLDER,
                    sizeVariableIndex = overallConnectionsIndex,
                    highlightVariable = highSchoolIndex,
                    edgesHighlight    = sameSPAT1Index,
                    edgesAlpha        = 0.3,
                    colorVectorEdge   = c(NA, "red"),
                    manualLayout      = myConstantLayoutB,
                    plotTitle         = currentPlotTitle,
                    plotSubtitle      = currentPlotSubtitle,
                    overrideLegendSize = 5)
        
    }
   
}
    
# ------------------------------------------------------------------------------
# Network simulations
# ------------------------------------------------------------------------------
if(FALSE){
    
    # SPA Bias analysis by simulations
    {
        
        # We need to reset the overall SPA edges first
        deleteThisLists        = deleteConnections(overallEdgesDF,  nonSPAIDs, nonSPAIDs)[[1]]
        spaOnlyOverallEdgesDF  = overallEdgesDF[  !deleteThisLists, ]
        
        # Get all edges into a list
        edgeList      = newList(TOTAL_NETWORKS)
        edgeList[[1]] = spaOnlyOverallEdgesDF
        edgeList[[2]] = spaOnlyPhysicalEdgesDF
        edgeList[[3]] = spaOnlySchoolEdgesDF
        edgeList[[4]] = spaOnlySportsEdgesDF
        edgeList[[5]] = spaOnlyHomeEdgesDF
        edgeList[[6]] = spaOnlyOthersEdgesDF

        # Run the bias analysis with respect the SPA variable
        SPABiasDF = doCategoricalBiasAnalysis(spaTableOnly,
                                              edgeList,
                                              spaT1IndexComplete,
                                              totalSimulations,
                                              listOfNetworksNames = NETWORK_NAMES)
        # Add the asterisk to the p-values
        SPABiasDF[[1]]$Significance = getAsterkisPValue(SPABiasDF[[1]][,11])
        
        write.csv2(SPABiasDF[[1]],  SPABIAS_FILEPATH)
        

    }
    
    # Carrier bias analysis by simulations
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
                                                  consequenceIndexes,
                                                  totalSimulations,
                                                  listOfNetworksNames = NETWORK_NAMES)

        # Add the significances asterisk and save it into disk
        for (i in 1:totalConsequenceIndexes){
            
            # -- Add significances
            CarrierBiasDF[[i]]$Significance = NA
            for (j in 1:TOTAL_NETWORKS){
                
                CarrierBiasDF[[i]][j,12] = getAsterkisPValue(CarrierBiasDF[[i]][j,11])
                
            }
            
            # -- Made a filepath 
            current_filepath = paste0(PAPER_FOLDER, "carrier_simulation_", consequenceNames[i], ".csv")
            write.csv2(CarrierBiasDF[[i]],  current_filepath)
            
        }
        
    }
    
    # Specific simulation for each studied variable
    {
        
        # Get all edges into a list
        #edgeList      = newList(TOTAL_NETWORKS)
        edgeList      = newList(2)
        edgeList[[1]] = overallEdgesDF
        edgeList[[2]] = physicalEdgesDF
        #edgeList[[3]] = schoolEdgesDF
        #edgeList[[4]] = sportsEdgesDF
        #edgeList[[5]] = homeEdgesDF
        #edgeList[[6]] = otherEdgesDF 
        
        # Run the default, that is, Each modality againts case A of the bootstrap vector.    
        modalitiesBiasDFList =  doModalityBiasAnalysis(completeTable,
                                                       edgeList,
                                                       consequenceIndexes,
                                                       explanatoryIndexes,
                                                       totalSimulations,
                                                       listOfNetworksNames = NETWORK_NAMES)
        
        
        # For each consequence and network
        # -- Add the significance,
        # -- add whether or not both CI are bellow or above base CI,
        # -- and write into disk
        for (i in 1:totalConsequenceIndexes){
            
            for (j in 1:2){
            #for (j in 1:TOTAL_NETWORKS){
            
                modalitiesBiasDFList[[i]][[j]]$Significance   = NA
                modalitiesBiasDFList[[i]][[j]]$Base_Inside_CI = NA

                # Add the asterisk and CI inside
                for (k in 1:nrow(modalitiesBiasDFList[[i]][[j]])){
                
                    # Add the asterisk to each row
                    modalitiesBiasDFList[[i]][[j]][k,15] = getAsterkisPValue(modalitiesBiasDFList[[i]][[j]][k,11])
                    
                    # Find the CI inside if needed
                    if(!is.na(modalitiesBiasDFList[[i]][[j]][k,13]) ){
                        
                        if( (modalitiesBiasDFList[[i]][[j]][k,13] < 1 & modalitiesBiasDFList[[i]][[j]][k,14] < 1) | 
                            (modalitiesBiasDFList[[i]][[j]][k,13] > 1 & modalitiesBiasDFList[[i]][[j]][k,14] > 1)   ){
                    
                             modalitiesBiasDFList[[i]][[j]][k,16] = FALSE
                            
                        }
                        else{
                            
                            modalitiesBiasDFList[[i]][[j]][k,16]  = TRUE
                            
                        }
                        
                    }
                    
                    

                        
                }
                    
                # Save it to disk
                current_filepath = paste0(PAPER_FOLDER, "modalities_simulation_", 
                                          consequenceNames[i],"_",NETWORK_NAMES[j], ".csv")    
                    
            
                write.csv2(modalitiesBiasDFList[[i]][[j]],  current_filepath)    
                
            }
            
        }
        
        
        
    }

    # Everything is finish for the regular variables
    # We have to study now the special variable for hormonal contraceptives
    #
    # Since this is for women only, the network simulation are a bit special
    # we just run the special function we prepared.
    {
        
        # Get all edges into a list
        #edgeList      = newList(TOTAL_NETWORKS)
        edgeList      = newList(2)
        edgeList[[1]] = overallEdgesDF
        edgeList[[2]] = physicalEdgesDF
        #edgeList[[3]] = schoolEdgesDF
        #edgeList[[4]] = sportsEdgesDF
        #edgeList[[5]] = homeEdgesDF
        #edgeList[[6]] = otherEdgesDF 
        
        contraceptivesBiasDFList = doContraceptivesCase(completeTable, edgeList,
                                                        consequenceIndexes,
                                                        hormonalIndex,
                                                        totalSimulations,
                                                        sexIndex,
                                                        hormonalIndex,
                                                        listOfNetworksNames = NETWORK_NAMES)
        
    }
    
}

# ------------------------------------------------------------------------------
# Isolation
# ------------------------------------------------------------------------------
{

    # While we could run this for all network, we do it for the overall network
    # only. It doesn't really make too much sense to measure the popularity at
    # home for example.
    
    # Find the average popularity for everybody and 
    averagePopularity   = mean(completeTable[,overallPopularityIndex])
    
    # The first thing we need to do is create the table with the p-values
    # for each variable with respect average popularity
    #
    # Participant that has a value = "Unknown" are excluded from the analysis
    # This is not the same as hiding people with Unknown value, that was the
    # original phrasing in the article (synonyms matters)
    {
        
        popularityPValuesDF              = readyDFVariables(completeTable, c(explanatoryIndexes,consequenceIndexes,hormonalIndex), overallPopularityIndex)
        popularityPValuesDF$Significance = NA
            
        # I hate you R, I want to declare int i, run the for, and keep the i >:(
        counter = 1
        for(i in 1:(totalExplanatoryIndexes + totalConsequenceIndexes)){
        
            # Are we in the explanatory indexes or the carrier variables?
            currentIndex = 0
            if(i <= totalExplanatoryIndexes) currentIndex = explanatoryIndexes[i]
            else currentIndex = consequenceIndexes[i - totalExplanatoryIndexes]

            popularityPValuesDF[i,2] = simpleCategoricalPValue(completeTable, currentIndex, overallPopularityIndex, skipUnknowns = TRUE)
            popularityPValuesDF[i,3] = getAsterkisPValue(popularityPValuesDF[i,2])
                
            counter = counter + 1
        }
        
        # Add the hormonal value
        popularityPValuesDF[counter,2] = simpleCategoricalPValue(womenMenstruatingTable, hormonalIndex, overallPopularityIndex, skipUnknowns = TRUE)
        popularityPValuesDF[counter,3] = getAsterkisPValue(popularityPValuesDF[counter,2])
        
    }
    
    # Now we are going to do the average for each modality, and add the previous
    # p-values to the table
    {
        
        # Get the base DF
        generalIsolationDF = readyDFModalities(completeTable, c(explanatoryIndexes,consequenceIndexes, hormonalIndex), 1)
        # Add some columns and remove ID column
        generalIsolationDF$AveragePopularity         = NA
        generalIsolationDF$RelativePhysicalIsolation = NA
        generalIsolationDF$RelativeAll               = NA
        generalIsolationDF$ID                        = NULL

        # Create the table with the people in physical isolation
        # Also create a women only version so we can compare the hormonal data later
        physicalIsolatedTable               = completeTable[completeTable$PhysicalPopularity == 0,]
        totalPhysicalIsolationRows          = nrow(physicalIsolatedTable)        
        womenOnlyPhysicalIsolatedTable      = womenMenstruatingTable[womenMenstruatingTable$PhysicalPopularity == 0,]
        womenOnlyTotalPhysicalIsolationRows = nrow(womenOnlyPhysicalIsolatedTable)        
        
        
        # For each variable of interest
        pointerIndex = 0
        for(i in 1:(totalExplanatoryIndexes + totalConsequenceIndexes)){
        
            pointerIndex = pointerIndex + 1
            
            currentIndex = 0
            if(i <= totalExplanatoryIndexes) currentIndex = explanatoryIndexes[i]
            else currentIndex = consequenceIndexes[i - totalExplanatoryIndexes]
            
            # Copy the p-value into the table
            generalIsolationDF[pointerIndex,2] = paste0(popularityPValuesDF[i,3], " ", round(popularityPValuesDF[i,2],3)) 
            
            # Get how many modalities this variable has
            currentTotalModalities = getTotalModalities(completeTable, currentIndex)
            
            # For each modality find the average values for each column
            for(j in 1:currentTotalModalities){
                
                pointerIndex = pointerIndex + 1
                
                currentModality         = generalIsolationDF[pointerIndex,1]
                currentSubTable         = completeTable[completeTable[,currentIndex]                 == currentModality,]
                currentPhysicalSubTable = physicalIsolatedTable[physicalIsolatedTable[,currentIndex] == currentModality,]
                
                generalIsolationDF[pointerIndex,2] = round(mean(currentSubTable[,overallPopularityIndex], na.rm = TRUE)      , 2)
                generalIsolationDF[pointerIndex,3] = round( 100 * nrow(currentPhysicalSubTable) / totalPhysicalIsolationRows , 2)
                generalIsolationDF[pointerIndex,4] = round( 100 * nrow(currentSubTable)         / nrow(completeTable)        , 2)
            }
            
            
        }
        
        # Finally, add the hormonal data using the women menstruating table
        {
            
            pointerIndex = pointerIndex + 1
            
            # Copy the p-value into the table
            generalIsolationDF[pointerIndex,2] = paste0(popularityPValuesDF[counter,3], " ", round(popularityPValuesDF[counter,2],3)) 
            
            # Get how many modalities this variable has
            currentTotalModalities = getTotalModalities(womenMenstruatingTable, hormonalIndex)
            
            # For each modality find the average values for each column
            for(j in 1:currentTotalModalities){
                
                pointerIndex = pointerIndex + 1
                
                currentModality         = generalIsolationDF[pointerIndex,1]
                currentSubTable         = womenMenstruatingTable[womenMenstruatingTable[,hormonalIndex]                 == currentModality,]
                currentPhysicalSubTable = womenOnlyPhysicalIsolatedTable[womenOnlyPhysicalIsolatedTable[,hormonalIndex] == currentModality,]

                generalIsolationDF[pointerIndex,2] = round(mean(currentSubTable[,overallPopularityIndex], na.rm = TRUE)                , 2)
                generalIsolationDF[pointerIndex,3] = round( 100 * nrow(currentPhysicalSubTable) / womenOnlyTotalPhysicalIsolationRows  , 2)
                generalIsolationDF[pointerIndex,4] = round( 100 * nrow(currentSubTable)         / nrow(womenMenstruatingTable)         , 2)
            }
            
        }
        
        
    }
 
    # Write results into disk
    write.csv2(generalIsolationDF,  POPULARITY_FILEPATH)
    
    
       
}

# ------------------------------------------------------------------------------
# Logistic regression
# ------------------------------------------------------------------------------
{
 
    # We do two things here, finding the odd ratio model and doing the logistic
    # regression plot.
    
    # First the odd ratio model, keep in mind that there is no good results
    # here, and all of this is just to keep track that something was done:
    {
        
    }
    
    # Now the logistic regression model and plot
    {
        
        # This plot is heavily personalized and is for 2 consequence variables
        # only, which are the 2 appearing in the paper. Maybe would be
        # interesting to put this into a function form somehow, but I can't see
        # how that would work with 20+ variables
        
        # Count how many people surrounding you  are positive in each variable
        totalDPositiveFriends = rep(0, totalRows)
        totalEPositiveFriends = rep(0, totalRows)
        totalFriends          = rep(0,  totalRows)
  
        for(i in 1:TOTAL_SUBJECTS){
    
            # Current ID
            currentID = i
    
            # Find the friends surrounding you (you nominate or nominate you)
            currentFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
                                      getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
            currentTotalFriends = length(currentFriends) 
            
            # Write that into the total friends vector for later
            totalFriends[i]     = currentTotalFriends
      
            # Find how many of those are positive if you have more than 0 friends
            if(currentTotalFriends > 0){
                for(j in 1:currentTotalFriends){
        
                    # Friend ID
                    currentFriendID = currentFriends[j]
        
                    # Status of the friend
                    friendDStatus = as.character(completeTable[currentFriendID, nasalDirectCarrierIndex])
                    friendEStatus = as.character(completeTable[currentFriendID, nasalEnrichmentCarrierIndex])
        
                    # Add it to the list whatever the result
                    if(friendDStatus == "Positive") totalDPositiveFriends[i] = totalDPositiveFriends[i] + 1
                    if(friendEStatus == "Positive") totalEPositiveFriends[i] = totalEPositiveFriends[i] + 1
        
                }
            }

    
        }
        
        # Grab only people who has friends
        onlyPeopleWithFriendsTotalDPositives = totalDPositiveFriends[totalFriends > 0]
        onlyPeopleWithFriendsTotalEPositives = totalEPositiveFriends[totalFriends > 0]
        onlyPeopleWithFriendsCarrierDStatus  = completeTable[totalFriends > 0, nasalDirectCarrierIndex]
        onlyPeopleWithFriendsCarrierEStatus  = completeTable[totalFriends > 0, nasalEnrichmentCarrierIndex]
        
        # Make the numbers for the model
        onlyPeopleWithFriendsCarrierDStatusNumerical = rep(0, length(onlyPeopleWithFriendsCarrierDStatus))
        onlyPeopleWithFriendsCarrierDStatusNumerical[onlyPeopleWithFriendsCarrierDStatus == "Positive"] = 1
        onlyPeopleWithFriendsCarrierEStatusNumerical = rep(0, length(onlyPeopleWithFriendsCarrierEStatus))
        onlyPeopleWithFriendsCarrierEStatusNumerical[onlyPeopleWithFriendsCarrierEStatus == "Positive"] = 1
  
        DModel  = glm(onlyPeopleWithFriendsCarrierDStatus~onlyPeopleWithFriendsTotalDPositives, family=binomial(link="logit"))
        EModel  = glm(onlyPeopleWithFriendsCarrierEStatus~onlyPeopleWithFriendsTotalEPositives, family=binomial(link="logit"))
  
        #DModel2 = glm(formula = onlyPeopleWithFriendsCarrierDStatusNumerical ~ onlyPeopleWithFriendsTotalDPositives, family = binomial)
        #EModel2 = glm(formula = onlyPeopleWithFriendsCarrierEStatusNumerical ~ onlyPeopleWithFriendsTotalEPositives, family = binomial)
        
        # Change the numbers height so they looks nicer in the plot
        onlyPeopleWithFriendsCarrierDStatusNumerical = rep(-0.02, length(onlyPeopleWithFriendsCarrierDStatus))
        onlyPeopleWithFriendsCarrierDStatusNumerical[onlyPeopleWithFriendsCarrierDStatus == "Positive"] = 0.98
        onlyPeopleWithFriendsCarrierEStatusNumerical = rep(+0.02, length(onlyPeopleWithFriendsCarrierEStatus))
        onlyPeopleWithFriendsCarrierEStatusNumerical[onlyPeopleWithFriendsCarrierEStatus == "Positive"] = 1.02
        
        # Prepare the dataframes for the plots
        logisticsPlotsDF           = DF(length(onlyPeopleWithFriendsTotalDPositives), 6)
        colnames(logisticsPlotsDF) = c("D_Status", "E_Status", "TotalFriendsDPositives", "TotalFriendsEPositives", "D_Numerical", "E_Numerical")
        logisticsPlotsDF[,1]       = onlyPeopleWithFriendsCarrierDStatus
        logisticsPlotsDF[,2]       = onlyPeopleWithFriendsCarrierEStatus
        logisticsPlotsDF[,3]       = onlyPeopleWithFriendsTotalDPositives
        logisticsPlotsDF[,4]       = onlyPeopleWithFriendsTotalEPositives
        logisticsPlotsDF[,5]       = onlyPeopleWithFriendsCarrierDStatusNumerical
        logisticsPlotsDF[,6]       = onlyPeopleWithFriendsCarrierEStatusNumerical
        
        # Do the boxplots and get the averages and significant (or not) p-value
        #myBoxplotResults = doCategoricalBoxPlot (logisticsPlotsDF,
         #                                        1,
          #                                       3,
           #                                      AUREUS_FOLDER,
            #                                     colorsVector = COLOR_VECTOR_CARRIER,
             #                                    showPValues = TRUE)

        
		myBoxplotResults = doBoxPlotV2(logisticsPlotsDF, 3, AUREUS_FOLDER,
                                       groupIndex = 1,
                        			   colorsVector = COLOR_VECTOR_CARRIER,
                        			   showPValues = TRUE, 
                                       )           
        
        
        DPositiveFriendsPvalue  = myBoxplotResults[[3]][[2]]
        DNegativeFriendsAverage = myBoxplotResults[[3]][[3]][1,2]
        DPositiveFriendsAverage = myBoxplotResults[[3]][[3]][2,2]
        
        
        myBoxplotResults = doBoxPlotV2(logisticsPlotsDF, 4, AUREUS_FOLDER,
                                       groupIndex = 2,
                        			   colorsVector = COLOR_VECTOR_CARRIER,
                        			   showPValues = TRUE, 
                                       )
        
        
        #myBoxplotResults = doCategoricalBoxPlot (logisticsPlotsDF,
         #                                        2,
          #                                       4,
           #                                      AUREUS_FOLDER,
            #                                     colorsVector = COLOR_VECTOR_CARRIER,
             #                                    showPValues = TRUE)
        
        EPositiveFriendsPvalue  = myBoxplotResults[[3]][[2]]
        ENegativeFriendsAverage = myBoxplotResults[[3]][[3]][1,2]
        EPositiveFriendsAverage = myBoxplotResults[[3]][[3]][2,2]
        
        # Prepare the data to draw the logistic functions
        # These two numbers comes from the D and E model each
        myDfunction = function(x){
            return(1/(1 + exp(-(0.16*x - 1.09) ) ) )
        }
    
        myEfunction = function(x){
            return(1/(1 + exp(-(0.14*x - 0.63) ) ) )
        }
        
        # Prepare the points for the boxplots inside the plot
        # -- These are the minimum, Q1, Average, Mean, Q3, and max
        # -- There are two carrier variables (Direct and Enrich) and two 
        #    possible status (Positive and Negative)
        {
            DNegatives = summary(logisticsPlotsDF[logisticsPlotsDF$D_Status == "Negative",]$TotalFriendsDPositives)
            DPositives = summary(logisticsPlotsDF[logisticsPlotsDF$D_Status == "Positive",]$TotalFriendsDPositives)
            ENegatives = summary(logisticsPlotsDF[logisticsPlotsDF$E_Status == "Negative",]$TotalFriendsEPositives)
            EPositives = summary(logisticsPlotsDF[logisticsPlotsDF$E_Status == "Positive",]$TotalFriendsEPositives)
  
            DNegativeMin = as.numeric(DNegatives[1])
            DNegativeMax = as.numeric(DNegatives[6])
            DNegative1   = as.numeric(DNegatives[2])
            DNegative2   = as.numeric(DNegatives[3])
            DNegative3   = as.numeric(DNegatives[5])
            DNegativeA   = as.numeric(DNegatives[4])
      
            ENegativeMin = as.numeric(ENegatives[1])
            ENegativeMax = as.numeric(ENegatives[6])
            ENegative1   = as.numeric(ENegatives[2])
            ENegative2   = as.numeric(ENegatives[3])
            ENegative3   = as.numeric(ENegatives[5])
            ENegativeA   = as.numeric(ENegatives[4])
      
            DPositiveMin = as.numeric(DPositives[1])
            DPositiveMax = as.numeric(DPositives[6])
            DPositive1   = as.numeric(DPositives[2])
            DPositive2   = as.numeric(DPositives[3])
            DPositive3   = as.numeric(DPositives[5])
            DPositiveA   = as.numeric(DPositives[4])
      
            EPositiveMin = as.numeric(EPositives[1])
            EPositiveMax = as.numeric(EPositives[6])
            EPositive1   = as.numeric(EPositives[2])
            EPositive2   = as.numeric(EPositives[3])
            EPositive3   = as.numeric(EPositives[5])
            EPositiveA   = as.numeric(EPositives[4])
      
        }
        
        # Prepare the dataframes for the density plots
        # -- These are all the little points in each (Positive or Negative) line
        {
            # -- The negatives
            {
                totalD_Negatives  = sum(logisticsPlotsDF$D_Status == "Negative")
                totalE_Negatives  = sum(logisticsPlotsDF$E_Status == "Negative")
                totalLogisticRows = totalD_Negatives + totalE_Negatives
                meltedLogisticsNegativesDF           = data.frame(matrix(NA, nrow = totalLogisticRows, ncol = 2))
                colnames(meltedLogisticsNegativesDF) = c("Status", "Total_Friends")
        
                currentMeltedIndex = 1
                for(z in 1:length(onlyPeopleWithFriendsTotalDPositives)){ # The 1:X range is correct, is the total length of the summary DF
          
                    # For the direct culture
                    if(logisticsPlotsDF$D_Status[z] == "Negative"){
            
                    meltedLogisticsNegativesDF[currentMeltedIndex,1] = "D_Negative"
                    meltedLogisticsNegativesDF[currentMeltedIndex,2] = logisticsPlotsDF$TotalFriendsDPositives[z]
                    currentMeltedIndex = currentMeltedIndex + 1
            
                }
          
                    # For the enrichment
                    if(logisticsPlotsDF$E_Status[z] == "Negative"){
            
                    meltedLogisticsNegativesDF[currentMeltedIndex,1] = "E_Negative"
                    meltedLogisticsNegativesDF[currentMeltedIndex,2] = logisticsPlotsDF$TotalFriendsEPositives[z]
                    currentMeltedIndex = currentMeltedIndex + 1
            
                }
                }  
                
            }
                
            # -- The Positives
            {
                totalD_Positives  = sum(logisticsPlotsDF$D_Status == "Positive")
                totalE_Positives  = sum(logisticsPlotsDF$E_Status == "Positive")
                totalLogisticRows = totalD_Positives + totalE_Positives
                meltedLogisticsPositivesDF           = data.frame(matrix(NA, nrow = totalLogisticRows, ncol = 2))
                colnames(meltedLogisticsPositivesDF) = c("Status", "Total_Friends")
        
                currentMeltedIndex = 1
                for(z in 1:length(onlyPeopleWithFriendsTotalDPositives)){ # The 1:X range is correct, is the total length of the summary DF
          
                    # For the direct culture
                    if(logisticsPlotsDF$D_Status[z] == "Positive"){
            
                        meltedLogisticsPositivesDF[currentMeltedIndex,1] = "D_Positive"
                        meltedLogisticsPositivesDF[currentMeltedIndex,2] = logisticsPlotsDF$TotalFriendsDPositives[z]
                        currentMeltedIndex = currentMeltedIndex + 1
            
                    }
          
                    # For the enrichment
                    if(logisticsPlotsDF$E_Status[z] == "Positive"){
            
                        meltedLogisticsPositivesDF[currentMeltedIndex,1] = "E_Positive"
                        meltedLogisticsPositivesDF[currentMeltedIndex,2] = logisticsPlotsDF$TotalFriendsEPositives[z]
                        currentMeltedIndex = currentMeltedIndex + 1
            
                    }
                    
                }        
            }
                
        }
        
        
        # Now lets do the actual plot. We have two version of this.
        #
        #     A) One version is with both variables Direct and Enrichment
        #
        #     B) The other version is just with the Enrichment, but properly
        #        aligned with the 0,1 lines. B is not in the paper, but is used
        #        in some people presentations.
        {
        
            # Version A) Both variables
            myAPlot = ggplot() +
                      # Jitter points
                      geom_jitter(data = logisticsPlotsDF, aes(x=TotalFriendsDPositives,y=D_Numerical),  width = 0.25, height = 0.025, color = "red",   alpha = 0.2) +
                      geom_jitter(data = logisticsPlotsDF, aes(x=TotalFriendsEPositives,y=E_Numerical),  width = 0.25, height = 0.025, color = "black", alpha = 0.2) +
                      # Horizontal lines
                      geom_hline(yintercept=0) +
                      geom_hline(yintercept=1) +
                      # Exponential functions
                      geom_function(fun = myDfunction, colour = "red",   lwd = 1, linetype = 1) +
                      geom_function(fun = myEfunction, colour = "black", lwd = 1, linetype = 1) +
                      # Manual boxplots
                      #
                      # Each is the same:
                      # ---- A line from 0 to to Q1
                      # ---- A rectangle from Q1 to Q3
                      # ---- A line from Q3 to 6 because it looks good, 7 and 8 is horrible
                      # ---- The middle line with the average on top of the rectangle
                      #
                      # -- Direct Culture
                      # ---- D Negatives
                      geom_segment(aes(x = DNegativeMin, y = -0.15, xend = DNegative1,   yend = -0.15), colour = "black") + 
                      geom_segment(aes(x = DNegative3,   y = -0.15, xend = 6,            yend = -0.15), colour = "black") + 
                      geom_rect(aes(xmin=DNegative1, xmax=DNegative3, ymin=-0.13, ymax=-0.17), fill="red", color="black", alpha=0.5) +
                      geom_segment(aes(x = DNegativeA,   y = -0.13, xend = DNegativeA,   yend = -0.17), colour = "black") +       
                      # ---- D Positives
                      geom_segment(aes(x = DPositiveMin, y = +1.09, xend = DPositive1,   yend = +1.09), colour = "black") + 
                      geom_segment(aes(x = DPositive3,   y = +1.09, xend = 6,            yend = +1.09), colour = "black") + 
                      geom_rect(aes(xmin=DPositive1, xmax=DPositive3, ymin=+1.07, ymax=+1.11), fill="red", color="black", alpha=0.5) +
                      geom_segment(aes(x = DPositiveA,   y = +1.07, xend = DPositiveA,   yend = +1.11), colour = "black") + 
                      # -- Enrichment
                      # ---- E Negatives
                      geom_segment(aes(x = ENegativeMin, y = -0.09, xend = ENegative1,   yend = -0.09), colour = "black") + 
                      geom_segment(aes(x = ENegative3,   y = -0.09, xend = 6, yend = -0.09), colour = "black") + 
                      geom_rect(aes(xmin=ENegative1, xmax=ENegative3, ymin=-0.07, ymax=-0.11), fill="black", color="black", alpha=0.5) +
                      geom_segment(aes(x = ENegativeA,   y = -0.07, xend = ENegativeA,   yend = -0.11), colour = "black") + 
                      # ---- E Positives
                      geom_segment(aes(x = EPositiveMin, y = +1.15, xend = EPositive1,   yend = +1.15), colour = "black") + 
                      geom_segment(aes(x = EPositive3,   y = +1.15, xend = 6,            yend = +1.15), colour = "black") + 
                      geom_rect(aes(xmin=EPositive1, xmax=EPositive3, ymin=+1.13, ymax=+1.17), fill="black", color="black", alpha=0.5) +
                      geom_segment(aes(x = EPositiveA,   y = +1.13, xend = EPositiveA,   yend = +1.17), colour = "black") + 
                      # Break the axys in integers numbers only
                      scale_x_continuous( breaks = seq(0,6,1),   limits=c(-0.5, 6.5))    +
                      scale_y_continuous( breaks = seq(0,1,0.2), limits=c(-0.17, 1.17))  +
                      # Legends and axys names
                      scale_colour_manual(name="Legend",
                                          labels=c("Direct Culture", "Enrichment"),
                                          values=c("red",            "black")) +
                      labs(x = "Number of friends defined as persistent carriers",
                           y = "Probability of S. aureus persistent carriage",
                           title = "Logistic Regression between total friends who are carriers and probability of carrier status",
                           color = "Legend") +
                      # Apply a theme to the whole plot that looks nice
                      theme_linedraw()
            
                      AplotFilePath = paste0(PAPER_FOLDER, "logisticA.png")
                      ggsave(AplotFilePath, plot = myAPlot, width = 8, height = 8)
                      ggsave(changeFileExtension(AplotFilePath, "pdf"), plot = myAPlot, width = imageWidth, height = imageHeight)  
            
            # Version B) Enrichment only for Dina's NBCISS 2021 presentation
            myBPlot = ggplot() +
                      # Jitter points
                      geom_jitter(data = logisticsPlotsDF, aes(x=TotalFriendsEPositives,y=E_Numerical - 0.02),  width = 0.25, height = 0.025, color = "#e88e2e", alpha = 0.2) +
                      # Horizontal lines
                      geom_hline(yintercept=0) +
                      geom_hline(yintercept=1) +
                      # Exponential functions
                      geom_function(fun = myEfunction, colour = "#e88e2e", lwd = 1, linetype = 1) +
                      # Manual boxplots
                      #
                      # Each is the same:
                      # ---- A line from 0 to to Q1
                      # ---- A rectangle from Q1 to Q3
                      # ---- A line from Q3 to 6 because it looks good, 7 and 8 is horrible
                      # ---- The middle line with the average on top of the rectangle
                      #
                      # -- Enrichment
                      # ---- E Negatives
                      geom_segment(aes(x = ENegativeMin, y = -0.09, xend = ENegative1,   yend = -0.09), colour = "black") + 
                      geom_segment(aes(x = ENegative3,   y = -0.09, xend = 6, yend = -0.09), colour = "black") + 
                      geom_rect(aes(xmin=ENegative1, xmax=ENegative3, ymin=-0.07, ymax=-0.11), fill="#e88e2e", color="black", alpha=0.5) +
                      geom_segment(aes(x = ENegativeA,   y = -0.07, xend = ENegativeA,   yend = -0.11), colour = "black") + 
                      # ---- E Positives
                      geom_segment(aes(x = EPositiveMin, y = +1.15, xend = EPositive1,   yend = +1.15), colour = "black") + 
                      geom_segment(aes(x = EPositive3,   y = +1.15, xend = 6,            yend = +1.15), colour = "black") + 
                      geom_rect(aes(xmin=EPositive1, xmax=EPositive3, ymin=+1.13, ymax=+1.17), fill="#e88e2e", color="black", alpha=0.5) +
                      geom_segment(aes(x = EPositiveA,   y = +1.13, xend = EPositiveA,   yend = +1.17), colour = "black") + 
                      # Break the axys in integers numbers only
                      scale_x_continuous( breaks = seq(0,6,1),   limits=c(-0.5, 6.5))    +
                      scale_y_continuous( breaks = seq(0,1,0.2), limits=c(-0.17, 1.17))  +
                      # Legends and axys names
                      scale_colour_manual(name="Legend",
                                          labels=c("Direct Culture", "Enrichment"),
                                          values=c("red",            "black")) +
                      labs(x = "Number of friends defined as persistent carriers",
                           y = "Probability of S. aureus persistent carriage",
                           title = "Logistic Regression between total friends who are carriers and probability of carrier status",
                           color = "Legend") +
                      # Apply a theme to the whole plot that looks nice
                      theme_linedraw()
            
                      BplotFilePath = paste0(PAPER_FOLDER, "logisticB.png")
                      ggsave(BplotFilePath, plot = myBPlot, width = 8, height = 8)
                      
        }
         
    }
    
    # Now that the regression model is finish, let find out how much your
    # probability increase every time you have a new positive friend
    {
     
        # Find the average of increase probability for each variable
        ddd = myDfunction(seq(10,0,-1)) 
        eee = myEfunction(seq(10,0,-1)) 
  
        averageD = 0
        averageE = 0
        for(i in 1:(length(ddd)-1)){
    
            difference = ddd[i]   - ddd[i+1] 
            averageD   = averageD + difference
    
            difference = eee[i]   - eee[i+1] 
            averageE   = averageE + difference
    
        }
        
        averageD = averageD / (length(ddd)-1)
        averageE = averageE / (length(ddd)-1)
  
        # Find CI for increase probability by numbers of friends
        {
    
            ddd2 = ddd
            for(i in 1:(length(ddd)-1)){

                ddd2[i] = ddd[i] - ddd[i+1] 
            
            }
            ddd2  = ddd2[1:length(ddd2)-1]
            CI95D = getCI(ddd2)
        
            eee2 = eee
            for(i in 1:(length(eee)-1)){

                eee2[i] = eee[i] - eee[i+1] 
            
            }
            eee2  = eee2[1:length(eee2)-1]
            CI95E = getCI(eee2)
        
        }
           
    }
    
}

# ------------------------------------------------------------------------------
# Autocorrelation model
# ------------------------------------------------------------------------------
{
  
    # Select your epsilon constant for the LNAM.
    # There is no particular reason as to why we used this one, after several
    # tries, this one worked best.
    EPSILON = 0.01
    
    # Prepare the frienship matrix with numbers
    if(FALSE){
        # The friend matrix, as 1 (friend) and 0 (non friend)
        # -- Friends if the original code is defined by rows (each row a maximum of 5)
        tempOverall      = overallNetworkDF
        tempOverall$ID   = NULL
        friendshipMatrix = as.matrix(tempOverall, nrow(completeTable))    
    }

    # Prepare the explicative variables DF with numbers rather than categorical
    {
        categoricalExplicativeDF = completeTable[, explanatoryIndexes]  
        myExplicativeVariablesDF = data.matrix(categoricalExplicativeDF)

        # It would be better to have XOR dummy variables for each sex, and each school
        #
        # ie:
        #
        #  Sex man | Sex woman | School General | School Vocational | School Sports
        #     1          0             0                 1                 0
        #     1          0             1                 0                 0
        #     0          1             1                 0                 0
        #
        #
        # However when I tried this, the result is a non-inversible matrix, so the
        # LM function can't solve the equations. Because of this, this is not done.
        #
        # The following code is the attemp to do this in case someone want to
        # replicate it, but again, keep in mind that if you run it, and it doesn't
        # work, that's why.
        if(FALSE){
            sex_women         = rep(1, nrow(myExplicativeVariablesDF))
            sex_men           = rep(1, nrow(myExplicativeVariablesDF))
            school_general    = rep(1, nrow(myExplicativeVariablesDF))
            school_vocational = rep(1, nrow(myExplicativeVariablesDF))
            school_sports     = rep(1, nrow(myExplicativeVariablesDF))
         
            for(i in 1:nrow(myExplicativeVariablesDF)){
                # Sex
                if(completeTable$Sex[i] == "Woman") sex_women[i] = 2
                else sex_men[i] = 2
                # School
                if(completeTable$School[i] == "General") school_general[i] = 2
                else{
                    if(completeTable$School[i] == "Vocational") school_vocational[i] = 2  
                    else school_sports[i] = 2
                }
            
            }
           
            myExplicativeVariablesDF = cbind(school_sports,     myExplicativeVariablesDF)
            myExplicativeVariablesDF = cbind(school_vocational, myExplicativeVariablesDF)
            myExplicativeVariablesDF = cbind(school_general,    myExplicativeVariablesDF)
            myExplicativeVariablesDF = cbind(sex_men,           myExplicativeVariablesDF)
            myExplicativeVariablesDF = cbind(sex_women,         myExplicativeVariablesDF)
          
            # Delete the old variables
            myExplicativeVariablesDF = myExplicativeVariablesDF[,-6:-7]
          
        }
    
        # Transform back into dataframe
        myExplicativeVariablesDF = as.data.frame(myExplicativeVariablesDF)
    
        # Substract -1 so the baseline is 0 in everything
        myExplicativeVariablesDF = myExplicativeVariablesDF - 1
  }
  
    # Prepare the dependent variables DF with numbers rather than categorical
    # for each consequence index (in the paper only 2)
    {
  
        numericalDataList = newList(totalConsequenceIndexes)
        for(i in 1:totalConsequenceIndexes){
        
            # Get the current index
            currentConsequenceIndex = consequenceIndexes[i]
            
            # The default value is 0, which is equal to Negative, so we don't
            # do to do anythin special with that case
            numericalDataList[[i]] = rep(0, nrow(completeTable))
            
            # Now fill those which are positives with 1s
            numericalDataList[[i]][ completeTable[, currentConsequenceIndex] == "Positive" ] = 1

        }

    }
  
    # Run the Linear Network Autocorrelation Model for each consequence index
    {
        
        # IT IS BORDERLINE IMPOSSIBLE TO EXTRACT THE STUPID P-VALUES FROM
        # THE SUMMARY MODEL!!!
        #
        # W-T-F , R!!. Your summary objects are the most stupid thing I have
        # seem in programming maybe ever.
        #
        # I HATE YOU; AND EVERYONE NOT DOING THE PROPER PROGRAMING JOB!!
        
        
        LNAMResultsList = newList(totalConsequenceIndexes)
        
        for(i in 1:totalConsequenceIndexes){
            
            builtInResults = builtInLNAM(numericalDataList[[i]], myExplicativeVariablesDF,
                                 friendshipMatrix = friendshipMatrix, epsilon = EPSILON)

            LNAMResultsList[[i]] = summary(builtInResults)
            
            #LNAMResultsList[[2]]$rho1
            #LNAMResultsList[[2]]$beta
            #LNAMResultsList[[2]]$beta.se

            #tidy(LNAMResultsList[[2]])
            
            #plot(builtInResults)
            
        }
        
        
    }

}

# ------------------------------------------------------------------------------
# Extras
# ------------------------------------------------------------------------------
{
    
    # -- Flowchart, done with LibreOffice, check the folder
    
    # -- Overview of all the graphs. Already done in the Graph Plots section
    
    # -- Overview of the enrichment graph plot, already done, same as before
    
    # -- Histogram with How well is this friendship a representation of real life
    {
    
        doHistogramPlot2(completeTable, overviewIndex, PAPER_FOLDER,
                         colorsVector   = COLOR_FRIENDSHIP,
                         binsWidth      = 1,
                         binBorderColor = "#333333",
                         plotTitle      = " Does these friends give a good overview of your social network? ",
                         plotSubtitle   = " 0 = Low, 10 = High",
                         plotXLabel     = "Points", plotYLabel = "Total")       
        
    }
    
    # -- EGRM Model
    {
        
        # Do the homophily analysis first and see what going on, there is some obvious
        # bias for almost everybody in the network.
        #partialHomophilyDF  = partialHomophily(overallGraph,  explanatoryIndexes)
        #completeHomophilyDF = completeHomophily(overallGraph, explanatoryIndexes)
        
        completeHomophilyV3(overallGraph, completeTable, explanatoryIndexes)
        
        
        
        # Prepare the frienship matrix with numbers
        {
            # The friend matrix, as 1 (friend) and 0 (non friend)
            # -- Friends if the original code is defined by rows (each row a maximum of 5)
            tempOverall = overallNetworkDF
            tempOverall$ID = NULL
            friendshipMatrix = as.matrix(tempOverall, nrow(completeTable))    
        }
  
  
        # Create the network model
        #
        # This is nothing special, is just that the egrm function need it own
        # way to define a network. Since everybody has it own standard which is
        # the best according to themselves, this is just an annoyance.
        {

            myOverallNetwork = as.network(x           = friendshipMatrix, 
                                          directed    = FALSE, 
                                          loops       = FALSE, 
                                          matrix.type = "adjacency" )
                        
            # Set the IDs  
            network.vertex.names(myOverallNetwork) = completeTable$ID

            # Set the categorical attributes (there will be no numerical values)
            for(i in 1:totalExplanatoryIndexes){
    
                currentIndex = explanatoryIndexes[i]
                currentName  = explanatoryNames[i]
  
                set.vertex.attribute(myOverallNetwork, currentName, as.vector(completeTable[,currentIndex]))
      
            }
            
                            
        }

            
        # ERGM
        #
        # There are several possibilities to run a EGRM. For this paper we run
        # it with all to all, but in the extended version of the code you have
        # more options
        # 
        # By some variables (everybody get together with similar peers as shown in the homophily table)
        myHomoModel = ergm(myOverallNetwork ~ edges + 
                                              nodematch("Sex") + nodematch("MainPrograme") +
                                              nodematch("BMICategorical") + nodematch("SportsLeisure") +                       
                                              nodematch("Smoke") + nodematch("Snuff") +                                              
                                              nodematch('Alcohol'))
        myHomoResults = summary(myHomoModel)
        # (as usual, R obfuscate how to actually get the results from summary >:[ )
          
        homoGoodnessOfFit = gof(myHomoModel ~ model) #here use a gof function to test for
        plot(homoGoodnessOfFit)
        
    }
    
    # -- When were people swabbed, and how many friends with you
    {

          myDates  = completeTable[,swab1AttendanceDateIndex]
          myHS     = sort(as.character(unique(completeTable[,highSchoolIndex])))
  
          # The first date is 2010-09-20, which is Week 38 of 2010
          # The last  date is 2011-04-27, which is Week 17 of 2010
          # 2010 has 52 weeks (last day ending in Saturday)
          totalWeeks = 52-38+17+1
                        
          # Dataframe where we will write the results
          datesDF = DF(totalWeeks, (4 + length(myHS)), defaultValue = 0)
          colnames(datesDF) = c("Week", "Year", myHS, "Friends", "IDs")

          # Initialize the dataframe
          currentWeek = 38
          currentYear = 2010
          for(i in 1:totalWeeks){
  
              datesDF$Week[i] = currentWeek
              datesDF$Year[i] = currentYear
              datesDF$IDs[i]  = ""
    
              currentWeek = currentWeek + 1
              if(currentWeek == 53){
                  currentWeek = 1
                  currentYear = 2011
              } 
    
           }
          
          # Find out how many people don't have valid or unregistered dates
          totalUnknownDates = sum(is.na(myDates))
          
          # Start filling the dataframe
          for(i in 1:TOTAL_SUBJECTS){
    
              # Check if the date is valid
              currentDate = myDates[i]
    
              # If the date is valid, do stuff
              if(!is.na(currentDate)){
      
                  # Get the data for that row
                  currentYear = year(ymd(currentDate))
                  currentWeek = as.numeric( strftime(currentDate, format = "%V"))
                  currentHS   = as.numeric(substring(completeTable[i,highSchoolIndex], 2))
                  currentID   = completeTable$ID[i]
      
                  # Find the proper row in the dataframe that correspond with this Year and Week
                  # Since weeks doesn't overlap, we can find the Week only
                  currentIndex = as.numeric(rownames(datesDF[datesDF$Week==currentWeek,]))

                  # Add the total to the proper highshool
                  currentHSTotal = datesDF[currentIndex, (2+currentHS)]
                  datesDF[currentIndex, (2+currentHS)] = currentHSTotal + 1
      
                  # Add this person ID to the list of IDs
                  datesDF$IDs[currentIndex] = paste0(datesDF$IDs[currentIndex]," ", currentID)
      
              }
              
          }

          # Find out the friend coverage
          for(i in 1:totalWeeks){
    
              # Get the list of people that were this week, week before, or week after doing the test
              currentList  = as.numeric(strsplit(datesDF$IDs[i]," ")[[1]])
              # The first one is always NA
              currentList  = currentList[2:length(currentList)]
              currentTotal = length(currentList)
    
              # For each ID, find out how many of his friends attended swabbing that week
              currentAverage = 0
    
              for (j in 1:currentTotal) {
    
                  currentID = currentList[j]
      
                  nominatedByThisID = overallEdgesDF[overallEdgesDF$from == currentID,]$to
                  totalNominations  = length(nominatedByThisID)
      
                  # If you nominated more than one person, count how many we have
                  if(totalNominations>0){
        
                      myAverage = sum(nominatedByThisID %in% currentList)/totalNominations
                      currentAverage = myAverage + currentAverage
        
                  }
                  # If you don't nominated anyone, technically 100% of your friends are there
                  else currentAverage = currentAverage + 1
        
              }
    
              currentAverage = currentAverage/currentTotal
    
              # Register the Friends coverage average
              datesDF$Friends[i] = round(currentAverage,4)
    
          }
    
          # Find the average of friendship
          averageFrienship = 0
          for(i in 1:totalWeeks){
    
              averageFrienship = averageFrienship + (sum(datesDF[i,3:10]) * datesDF$Friends[i])
    
          }
          
          print("Weighted average friendship coverage per week")
          print(averageFrienship / sum(datesDF[,3:10]))
          
          # Write the table into disk
          {
              # Convert Frienship into %
              datesDF$Friends = paste(round(datesDF$Friends * 100,2),"%")
    
              # Drop the ID column
              datesDF$IDs = NULL
    
              # Write into disk
              write.csv2(datesDF,  FRIENDSHIP_SWAB_FILEPATH)
              
          }
          
                    
    }
          
}

# ------------------------------------------------------------------------------
# If you want, show all the results
# ------------------------------------------------------------------------------
if(FALSE){

    # Prevalence + Xi² Analysis
    {
        
        prevalenceDF
        
        # R is horrible and have inconsistency when showing results in scientific
        # format and do whatever it wants. Sometimes it writes 3.2e-01 and sometimes
        # it does 0.32 ; is insanity!.
        #
        # Anyway, show this table in not scientific format
        format(xi2SummaryDF, scientific=FALSE)    
        
        prevalenceHDF
        
        xi2SummaryHDF
        
    }
    
    # Simulations
    
    # -- SPA bias
    print("SPA Simulation P-value:")
    print(pValueSPA)
    
    # -- Carrier to Carrier
    print("Carriers Simulation P-values:")
    for (i in 1:totalConsecuenceIndexes){

      print(biasResultsList[[i]])
      
    }
    
}