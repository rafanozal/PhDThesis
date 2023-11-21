library(ergm)
library(statnet)
library(ggpubr) #ggarrange
library(broom) # How completely stupid R is, that you can't extract the p-value
               # from a model, without using an external library T_T

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

        # ---- PNG
        ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows)
        ggsave(BIOMARKERS_GRAPH_GRID, width = 11, height = 16)
        
        writeImageLATEX2(BIOMARKERS_GRAPH_GRID, LATEX_RELATIVE_BIOMARKERS_FOLDER_IMAGES_NETWORK, 
                         captionText   = "Overview of all friendship networks.",
                         overrideLabel = "fig:allGraphsOverview", 
                         pageHeight    = 0.7, overrideFloat = TRUE)
        
    }
    
    # Figure for the societal highschool
    # -- Red friend with close biomarkers (what does close mean?)
    # -- blue friend with far away biomarkers
    # Figure for the same to same BMI (it looks very ugly with no relevant info)
    if(FALSE){
        
        # Create a DF that we can modify freely without destroying the original one
        myCustomEdgesDF = overallEdgesDF
        
        # Create another DF where we are going to count the same to same relationships
        sameToSameBMI = DF(length(BMILevels),length(BMILevels), defaultValue = 0)
        colnames(sameToSameBMI) = BMILevels
        
        
        # Add the same to same relationship with respect the BMI
        myCustomEdgesDF$SameBMI = "Yes"
        myCustomEdgesDF$FromBMI = ".."
        myCustomEdgesDF$ToBMI   = "..."
        for(i in 1:nrow(myCustomEdgesDF)){
        
            # Get the from and to edge IDs
            fromID = as.numeric(myCustomEdgesDF$from[i])
            toID   = as.numeric(myCustomEdgesDF$to[i])
                
            # Check if they have different BMIs
            fromBMI = as.character(completeTable[fromID, BMICatIndex])
            toBMI   = as.character(completeTable[toID,   BMICatIndex])
        
            myCustomEdgesDF$FromBMI[i] = fromBMI
            myCustomEdgesDF$ToBMI[i]   = toBMI
                
            if(fromBMI != toBMI) myCustomEdgesDF$SameBMI[i] = "No"
            
            # Add the info to our relationship table
            fromRow  = grep(fromBMI, BMILevels)
            toColumn = grep(toBMI,   BMILevels)
            
            currentValue = sameToSameBMI[fromRow, toColumn]
            sameToSameBMI[fromRow, toColumn] = currentValue + 1
        }
        
        
        
        # Get the numbers of same to same relationship compared to what we would expect randomly
        if(FALSE){

            totalSameAAA = sum(myCustomEdgesDF$SameBMI == "Yes")
            totalRelAAA  = nrow(myCustomEdgesDF)
        
            totalSameAAA / totalRelAAA
        
            # Do the homophily analysis first and see what going on, there is some obvious
            # bias for almost everybody in the network.
            partialHomophilyDF  = partialHomophily(overallGraph,  BMICatIndex)
            completeHomophilyDF = completeHomophily(overallGraph, BMICatIndex)    
            
            
            
            
            
            
            
            # Fill the table and check if there is bias in the relationships
            
            # Copy the table and delete the unknowns
            sameToSameBMI2 = sameToSameBMI
            sameToSameBMI2$Unknown = NULL
            sameToSameBMI2 = sameToSameBMI2[-5,]
            # Find the probabilities and delete unknowns
            totalBMI = summarizeCategorical(completeTable, BMICatIndex, sorted = "none")[,2]
            totalBMI = totalBMI[1:4]
            probabilityBMI = summarizeCategorical(completeTable, BMICatIndex, sorted = "none")[,3]
            probabilityBMI = probabilityBMI[1:4]
            # Perform the xi2
            aaa = data.frame(myCustomEdgesDF$FromBMI, myCustomEdgesDF$ToBMI)
            bbb = table(myCustomEdgesDF$FromBMI,      myCustomEdgesDF$ToBMI)
            bbb = bbb[-5,-5]
            bbb = bbb[BMILevels[1:4], BMILevels[1:4]]
            ccc = bbb
            ddd = bbb
            
            chisq.test(bbb)
            
            # Check the individual combinations
            for(i in 1:nrow(ccc)){
            
                for(j in 1:ncol(ccc)){

                    #ccc[i,j] = binom.test(bbb[i,j], totalRelAAA, probabilityBMI[i]* probabilityBMI[j], alternative = "two.sided")$p.value
                    ccc[i,j] = binom.test(bbb[i,j], sum(bbb[i,]), probabilityBMI[i], alternative = "two.sided")$p.value
                    ccc[i,j] = getAsterkisPValue(ccc[i,j])
                    ddd[i,j] = probabilityBMI[i] * probabilityBMI[j] * totalRelAAA
                }
                    
            }
            
            
        }


        # Get the societal layout from our self-made function
        highSchoolLayout = createCategoricalLayout(myCustomEdgesDF, completeTable, highSchoolIndex)
        myGraph = graph_from_data_frame(myCustomEdgesDF, vertices = completeTable, directed = FALSE)
        myConstantLayoutB = create_layout(graph = myGraph, layout = 'mds')
        myConstantLayoutB$x = highSchoolLayout[[1]]$x
        myConstantLayoutB$y = highSchoolLayout[[1]]$y
        
        # Prepare the plot titles
        currentPlotTitle          = "School network, highchools in nodes and same spa-type in edges."
        currentPlotSubtitle       = "Only nodes with a valid spa-type are shown (n = 746)."
        
        doGraphPlot(myCustomEdgesDF,  completeTable, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                    sizeVariableIndex = overallConnectionsIndex,
                    highlightVariable = highSchoolIndex,
                    edgesHighlight    = 4,
                    edgesAlpha        = 0.2,
                    colorVectorEdge   = c("blue", "red"),
                    manualLayout      = myConstantLayoutB,
                    plotTitle         = currentPlotTitle,
                    plotSubtitle      = currentPlotSubtitle,
                    overrideLegendSize = 5)
        
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
# Check my friends average biomarker (stratify by sex and highschool) againts
# my own biomarkers levels. Check for R2 and p-value
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
                    
            myBioModel = doSimpleRegression(regressionAllDF, 1, 2, BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS,
                                            plotTitle = currentPlotTitle,
                                            overrideTableName = currentTableOverride)
                    
            mySummary = summary(myBioModel)
                    
                    
            # Add results to dataframe        
            resultsSimpleDF[myCounter,1] = currentBiomarkerName
            resultsSimpleDF[myCounter,2] = "Both"
            resultsSimpleDF[myCounter,3] = "All"
            resultsSimpleDF[myCounter,4] = mySummary$r.squared
            resultsSimpleDF[myCounter,5] = glance(myBioModel)$p.value[[1]]
                    
            myCounter = myCounter + 1
            
            
        }
        
        # Transform the results into a readable asterisk table (if any)
        resultsSimpleRoundedDF         = resultsSimpleDF
        resultsSimpleRoundedDF$R2      = round(resultsSimpleRoundedDF$R2, 2)
        resultsSimpleRoundedDF$Pvalue  = getAsterkisPValue(resultsSimpleRoundedDF$Pvalue)
        
        write.csv2(resultsSimpleRoundedDF, SCATTER_SIMPLE_ROUNDED_FILEPATH)
        
        #print("There is almost no results in here")
        #view(resultsSimpleRoundedDF)
                
    }
    
    # Now we do the same but stratifying by highschool and sex
    {
        
        # Get the highschools info
        myHighschools    = levels(completeTable[,highSchoolIndex])
        totalHighschools = length(myHighschools)
        
        # Sex info without highschool stratification
        resultsMenWomenDF          = DF(TOTAL_BIOMARKERS*2,5)
        colnames(resultsComplexDF) = c("Protein", "Sex", "Highschool","R2","Pvalue")
        mySexCounter               = 1
        
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
                    
                    currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " in all HS")
                    currentTableOverride = paste0(currentBiomarkerName,"_",currentSex,"_allHS")
                    
                    myBioModel = doSimpleRegression(regressionAllDF, 1, 2, BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS,
                                                    plotTitle = currentPlotTitle,
                                                    overrideTableName = currentTableOverride)

                    mySummary = summary(myBioModel)
                    
                    
                    
                    resultsMenWomenDF[myCounter,1] = currentBiomarkerName
                    resultsMenWomenDF[myCounter,2] = currentSex
                    resultsMenWomenDF[myCounter,3] = currentHighschool
                    resultsMenWomenDF[myCounter,4] = mySummary$r.squared
                    resultsMenWomenDF[myCounter,5] = mySummary$coefficients[2,4]
                    
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
                    
                    myBioModel = doSimpleRegression(regressionAllDF, 1, 2, BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS,
                                                    plotTitle = currentPlotTitle,
                                                    overrideTableName = currentTableOverride)

                    mySummary = summary(myBioModel)
                    
                    
                    
                    resultsComplexDF[myCounter,1] = currentBiomarkerName
                    resultsComplexDF[myCounter,2] = currentSex
                    resultsComplexDF[myCounter,3] = currentHighschool
                    resultsComplexDF[myCounter,4] = mySummary$r.squared
                    #resultsComplexDF[myCounter,5] = as.numeric(mySummary$fstatistic[1])
                    resultsComplexDF[myCounter,5] = mySummary$coefficients[2,4]
                    
                    myCounter = myCounter + 1
                    
        }
        
    }
    
    
}
    
    }

    # Write to disk
    # (but round numbers first)
    resultsComplexRoundedDF    = resultsComplexDF
    resultsComplexRoundedDF$R2 = round(resultsComplexRoundedDF$R2, 2)
    
    for( i in 1:nrow(resultsComplexRoundedDF)){
    
        resultsComplexRoundedDF$Pvalue[i] = getAsterkisPValue(resultsComplexDF$Pvalue[i])

    }
    # -- These are the raw results
    write.csv2(resultsComplexRoundedDF, SCATTER_STRATOS_ROUNDED_FILEPATH)
    write.csv2(resultsComplexDF,        SCATTER_STRATOS_FILEPATH)
    
    # We have waaaay too many significances, let delete those that are ns 
    # and include that in the paper, the rest of the raw result can be found
    # at the github page
    
    # -- Delete those that are ns
    resultsMyVSFriendBioPaper = resultsComplexRoundedDF
    resultsMyVSFriendBioPaper = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$Pvalue != "ns",]
    # -- Sort by Sex, Highschool, and Protein in that order
    resultsMyVSFriendBioPaper = resultsMyVSFriendBioPaper[order(resultsMyVSFriendBioPaper$Sex,resultsMyVSFriendBioPaper$Highschool, resultsMyVSFriendBioPaper$Protein),]
    # -- The table is too big, divide into two tables, for men and women
    resultsMyVSFriendBioPaperMen    = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$Sex == "Man",]
    resultsMyVSFriendBioPaperWomen  = resultsMyVSFriendBioPaper[resultsMyVSFriendBioPaper$Sex == "Woman",]
    
    
    
    writeTableLATEX(resultsMyVSFriendBioPaperMen, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Protein for men, which are similar to your male friend, stratify by highschool",
                    overrideTableName = "biofriendsMalesHighschool", widthProportion = 0.7, heightProportion = 0.4)
    
    writeTableLATEX(resultsMyVSFriendBioPaperWomen, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Protein for women, which are similar to your male friend, stratify by highschool",
                    overrideTableName = "biofriendsFemalesHighschool", widthProportion = 0.7, heightProportion = 0.4)
    
    
    # Now we do the same but for blood proteins too (HS and sex)
    {
        
        # Get the highschools info
        myHighschools    = levels(completeTable[,highSchoolIndex])
        totalHighschools = length(myHighschools)
        
        # Prepare the DF where we accumulate the results
        resultsMenWomenHSBloodDF           = DF(totalBloodColumns*2*totalHighschools,5)
        colnames(resultsMenWomenHSBloodDF) = c("Protein", "Sex", "Highschool","R2","Pvalue")
        myCounter                          = 1
        mySexCounter                       = 1
        
        # For men and women
        for(i in 1:2){
        
            currentSex = "Man"
            if(i == 2) currentSex = "Woman"
   
            # For high school stratification
            for(j in 1:totalHighschools){
                
                currentHighschool = myHighschools[j]
                
                # Get the table of people within the same sex and highschool
                currentSubtable = completeTable[ completeTable[,sexIndex]        == currentSex &
                                                 completeTable[,highSchoolIndex] == currentHighschool, ]
                
                peopleWithFriendsDF    = currentSubtable[currentSubtable[ , overallConnectionsIndex] > 0,]
                totalPeopleWithFriends = nrow(peopleWithFriendsDF)
                
                # For each biomarker
                # for(k in 1:totalBloodColumns){ Only caring for CRP
				for(k in 12:12){                	
                    
                	print("----------------")
                	print(i/2)
                	print(j/totalHighschools)
                	print(k/totalBloodColumns)
                	
                    # Prepare the blank DF where we put the dataset
                    # Some of these can be NA since not everyone friend has the biomarkers
                    # analysis done (or could even be under LOD)
                    regressionAllDF = DF(totalPeopleWithFriends,2, defaultValue = NA)
                    colnames(regressionAllDF) = c("MyFriendsAverage", "MyLevel")
                    
                    currentBloodName = colnames(completeTable)[firstBloodIndex + k - 1]
                    	
                    # For each person, find their friends averages
                    for(x in 1:totalPeopleWithFriends){
                        
                        # Get the person relevant information
                        currentPersonID       = peopleWithFriendsDF[x, IDIndex     ]
                        currentPersonBioLevel = completeTable[currentPersonID, firstBloodIndex + k - 1]
                        
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
                                currentFriendBioLevel = completeTable[currentFriendID, firstBloodIndex + k - 1]
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
                    
                    currentPlotTitle     = paste0(currentBloodName," for ",currentSex, " in ", currentHighschool)
                    currentTableOverride = paste0(currentBloodName,"_",currentSex,"_",currentHighschool)
                    
                    myBioModel = doRegressionPlot(regressionAllDF, 1, 2, BIOMARKERS_FOLDER_IMAGES_REGRESSION_BIOFRIENDS,
                                                    plotTitle = currentPlotTitle,
                                                    overrideTableName = currentTableOverride)

                    mySummary = summary(myBioModel)
                    
                    

                    
                    
                    resultsMenWomenHSBloodDF[myCounter,1] = currentBloodName
                    resultsMenWomenHSBloodDF[myCounter,2] = currentSex
                    resultsMenWomenHSBloodDF[myCounter,3] = currentHighschool
                    resultsMenWomenHSBloodDF[myCounter,4] = max(myBioModel[[5]]$R2)
                    #resultsComplexDF[myCounter,5] = as.numeric(mySummary$fstatistic[1])
                    resultsMenWomenHSBloodDF[myCounter,5] = min(myBioModel[[5]][,3])
                    
                    myCounter = myCounter + 1
                    
        		}
        
			}
    
    
		}
    
    }    
    
    
}

#---------------------------------------------------------------------------
# Check friends distances
#---------------------------------------------------------------------------
{
    # This variables are for the final results
    menFriendDistances = NA
    menFriendsTotal    = NA
    menEnemiesDistances = NA
    menEnemiesTotal     = NA
    menRatios           = NA
    
    womenFriendDistances = NA
    womenFriendsTotal    = NA
    womenEnemiesDistances = NA
    womenEnemiesTotal     = NA
    womenRatios           = NA
    
    # Prepare the tables with the NDL values for men and women
    # Inclue people with non NA values only
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
                    overrideTableName = "distancesBiomarkers", widthProportion = 0.5, heightProportion = 0.4)
 
    # How many have a distance greater than 1.1 or lower than 0.9
    sum(distancesDF$Men > 1.1)
    sum(distancesDF$Men < 0.9)
    sum(distancesDF$Women > 1.1)
    sum(distancesDF$Women < 0.9)
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
        
        colnames(CarrierBiasDF2)[11] = "p-value"
        colnames(CarrierBiasDF3)[11] = "p-value"
        
        CarrierBiasDF2$BMICategorical = getAsterkisPValue(CarrierBiasDF2$BMICategorical)
        CarrierBiasDF2$SD             = round(CarrierBiasDF2$SD,2) 
        
        CarrierBiasDF3$BMICategorical = round(CarrierBiasDF3$BMICategorical,3) 
        CarrierBiasDF3$SD             = round(CarrierBiasDF3$SD,2) 
        
        current_filepath = paste0(BIOMARKERS_FOLDER_TABLES_NETWORK, "BMI_simulation,csv")
        write.csv2(CarrierBiasDF2,  current_filepath)
        
        
        writeTableLATEX(CarrierBiasDF2, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Simulated networks (n=1000) same-to-same relationships againts the real network same-to-same relationships. All simulated network shows bias of BMI spread in the real network.",
                       overrideTableName = "bmiSimulated", widthProportion = 0.9, heightProportion = 0.05)
        
        writeTableLATEX(CarrierBiasDF3, BIOMARKERS_FOLDER_TABLES_NETWORK, tableCaption = "Simulated networks (n=1000) same-to-same relationships againts the real network same-to-same relationships. All simulated network shows bias of BMI spread in the real network.",
                       overrideTableName = "bmiSimulatedNumbers", widthProportion = 0.9, heightProportion = 0.05)
        
        
    }
    
    
    
}

#---------------------------------------------------------------------------
# PCA analysis
#---------------------------------------------------------------------------
{

    # Do the PCA analysis
    pcaAnalysis = prcomp(menBiomarkersNDLTable, scale=TRUE)

    # Do the Scree plot
    # About 20% is explain by the first two variables, not very good for 2D plots.
    pcaAnalysis.var = pcaAnalysis$sdev^2
    pcaAnalysis.var.per = round(pcaAnalysis.var/sum(pcaAnalysis.var)*100, 1)
    barplot(pcaAnalysis.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

    ## plot pc1 and pc2
    plot(pcaAnalysis$x[,1], pcaAnalysis$x[,2])

    # Try PCA with selected immune markers only
    naturalKillerIndex = grep("Natural.killer.cell.receptor" ,         colnames(completeTable))[2]
    cd4Index           = grep("CD40L" ,                                colnames(completeTable))[2]
    cd5Index           = grep("T.cell.surface.glycoprotein.CD5" ,      colnames(completeTable))[2]
    cd6Index           = grep("T.cell.surface.glycoprotein.CD6" ,      colnames(completeTable))[2]
    macrophageIndex    = grep("Macrophage.colony.stimulating.factor" , colnames(completeTable))[2]
    monocityIndex      = grep("Monocyte.chemotactic.protein.1" ,       colnames(completeTable))[2]

    inmmuneMarkersCollection = c(naturalKillerIndex, cd4Index, cd5Index, cd6Index, macrophageIndex, monocityIndex)
    
    menImmuneTable = menOnlyTable[,inmmuneMarkersCollection]
    
    # Delete men without complete bioprofile
    {
        
        keepTheseMen = rep(TRUE, nrow(menImmuneTable))
        
        for (i in 1:nrow(menImmuneTable)){
            
            
            print(sum(is.na(menImmuneTable[i,])))
            
            if(sum(is.na(menImmuneTable[i,])) > 0) keepTheseMen[i] = FALSE
            
        }
        
    }
    
    menImmuneTable = menImmuneTable[keepTheseMen,]
    
    
    # Do the PCA analysis
    pcaAnalysis = prcomp(menImmuneTable, scale=TRUE)

    # Do the Scree plot
    # About 65% is explain by the first two variables, ok, lets call it good enough
    pcaAnalysis.var = pcaAnalysis$sdev^2
    pcaAnalysis.var.per = round(pcaAnalysis.var/sum(pcaAnalysis.var)*100, 1)
    barplot(pcaAnalysis.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

    ## plot pc1 and pc2
    for(i in 1:length(inmmuneMarkersCollection)){
    
        plot(pcaAnalysis$x[,1], pcaAnalysis$x[,i])
            
    }
    
        
}

# ------------------------------------------------------------------------------
# Autocorrelation model
# ------------------------------------------------------------------------------
if(FALSE){
  
    # Select your epsilon constant for the LNAM.
    # There is no particular reason as to why we used this one, after several
    # tries, this one worked best.
    EPSILON = 0.01
    
    # Prepare the frienship matrix with numbers
    {
        # The friend matrix, as 1 (friend) and 0 (non friend)
        # -- Friends if the original code is defined by rows (each row a maximum of 5)
        tempOverall = overallNetworkDF
        tempOverall$ID = NULL
        friendshipMatrix = as.matrix(tempOverall, nrow(completeTable))    
    }

    # Prepare the explicative variables DF with numbers rather than categorical
    
    
    
    myExplicativeVariablesDF     = DF(TOTAL_PEOPLE,2,0)
    myExplicativeVariablesDF[,1] = completeTable$BMI
    
    if(FALSE){
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
    myDependentVariables = completeTable$BMI
    if(FALSE){
  
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
    
    builtInResults = builtInLNAM(myDependentVariables, myExplicativeVariablesDF,
                                 friendshipMatrix = friendshipMatrix, epsilon = EPSILON)
    
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
# Regression with respect BMI
# ------------------------------------------------------------------------------
{
 
    # We do two things here, finding the odd ratio model and doing the logistic
    # regression plot.
    
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
                        if(currentFriendBMI >     30 ) currentTotalObese    = currentTotalObese    + 1

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
        myBoxplotResults = doCategoricalBoxPlot (logisticsPlotsDF,
                                                 1,
                                                 2,
                                                 BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                                 showPValues = TRUE)
        
        OPositiveFriendsPvalue  = myBoxplotResults[[2]][2,2]
        ONegativeFriendsAverage = myBoxplotResults[[3]][1,2]
        OPositiveFriendsAverage = myBoxplotResults[[3]][2,2]
     
        # Prepare the function for the plot
        myObesefunction = function(x){
            return(1/(1 + exp(-(0.71*x - 2.96) ) ) )
        }
        
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
                      labs(x = "Total friends with BMI > 30",
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
                         captionText   = "Logistic regression between number of obese friends and obesity status. Each dot represent a person who can be obese (1) or non-obese (0),
                                          and who has a total number of obese friends (x-axis).",
                        overrideLabel  = "fig:EgoObeseFriendsObese",
                        pageHeight = 0.3, overrideFloat = TRUE)
        
           
    }
    
    # Do the scatter plots
    {
     
            myBMIIncrease = doSimpleRegression(BMIRelationDF, 3, 4, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                       plotTitle = "Relationship of my BMI with my friends BMI")
    
    glance(myBMIIncrease)$p.value[[1]]
    glance(myBMIIncrease)$r.squared[[1]]
    
    
    mySexIncrease = doSimpleRegression(sameSexBMIDF, 4, 5, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                       plotTitle = "Relationship between my BMI and opposite friends sex (ALL)")
    

    sameSexBMIDFMen    = sameSexBMIDF[sameSexBMIDF$Sex == "Man",]
    sameSexBMIDFWomen  = sameSexBMIDF[sameSexBMIDF$Sex == "Woman",]
    
    mySexIncreaseMen   = doSimpleRegression(sameSexBMIDFMen,   4, 5, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                          plotTitle = "Relationship between men BMI and total number of women friends")
    
    mySexIncreaseWomen = doSimpleRegression(sameSexBMIDFWomen, 4, 5, BIOMARKERS_FOLDER_IMAGES_NETWORK,
                                          plotTitle = "Relationship between women BMI and total number of men friends")
    
    
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
    

    
}

#---------------------------------------------------------------------------
# Autorocorrelation of BMI with all cofunding variables ????
#---------------------------------------------------------------------------
{
    
}

#---------------------------------------------------------------------------
# Network in time FF1 FF2
#---------------------------------------------------------------------------
{
    
}
