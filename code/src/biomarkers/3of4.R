#---------------------------------------------------------------------------
# Create a mega table with all the modalities averages for men and women
# the next part of the analysis is to check if there is any significance
# differences in here
#---------------------------------------------------------------------------
{

    prevalencesDF = summarizePrevalences(completeTable, allCategoricalIndexes,
                                         sexIndex, prevalenceTarget = "Man")
        
    averagesDF    = summarizeAverages(completeTable, allCategoricalIndexes,
                                      allNDLIndex, sexIndex)
    
    
    prevalencesDF[,5] = NA
    
    # Find the prevalence for women
    
    #(I really hate R, NA values are considered numeric and non-numeric as the same time -_-)
    for(i in 1:nrow(prevalencesDF)){
    
        if(grepl(".", prevalencesDF[i,4], fixed=TRUE)) prevalencesDF[i,5] = 1 - as.numeric(prevalencesDF[i,4])
        if("1" %in% prevalencesDF[i,4])                prevalencesDF[i,5] = 1 - as.numeric(prevalencesDF[i,4])
        if("0" %in% prevalencesDF[i,4])                prevalencesDF[i,5] = 1 - as.numeric(prevalencesDF[i,4])
        
    }
    
    
    
    # Write into disk
    write.csv2(prevalencesDF, PREVALENCES_TABLE_FILEPATH)
    write.csv2(averagesDF,    AVERAGES_NDL_TABLE_FILEPATH)
    
    # Create the plots
    for(i in 1:totalCategoricalIndexes){
    
        currentVariableName = colnames(completeTable)[allCategoricalIndexes[i]]
        currentPlotTitle    = paste0("Total men and women for ", currentVariableName)
        
        doBarRelativeCombinePlot(completeTable, sexIndex, allCategoricalIndexes[i], BIOMARKERS_FOLDER_IMAGES_HOSTFACTORS_SEX,
                                 colorsVector = COLOR_VECTOR_SEX,
                                 plotTitle = currentPlotTitle)
            
    }
    
    
    
}

#---------------------------------------------------------------------------
# Try the Xi² for all categories
#---------------------------------------------------------------------------
{

    # First, generate the big table with all the results
    # -- Generate the xi² results summary
    xi2SummaryDF  = simpleXi(completeTable, sexIndex, allCategoricalIndexes)
    # -- Make the table looks better
    xi2SummaryDFT = transposeDF(xi2SummaryDF) # The base code R function for transpose sucks for DFs with row names in columns rather than in row names
    xi2SummaryDFT = xi2SummaryDFT[-1,]
    colnames(xi2SummaryDFT)[2] = "p-value"
    xi2SummaryDFT$Significance = getAsterkisPValue(xi2SummaryDFT[,2])
    # -- Round the p-values with proper sci-notation
    xi2SummaryDFT$`p-value` = formatC(xi2SummaryDFT$`p-value`, format = "e", digits = 2)
    
    # -- Prepare a column where we write the report
    xi2SummaryDFT$Report = ""
        
    # Now, generate each individual sub-table and the proper latex input code
    # This goes into the supplementary
    currentTableCaption = ""
    currentTableName    = ""
    currentVariableName = ""
    for(i in 1:totalCategoricalIndexes){
    
        currentVariableName = colnames(completeTable)[allCategoricalIndexes[i]]

        currentXiResults            = categoricalXi(completeTable, sexIndex, allCategoricalIndexes[i])
        
        currentOverrepresentedTable = currentXiResults[[7]]
        currentSummaryTable         = currentXiResults[[9]]
        
        colnames(currentSummaryTable)[1] = currentVariableName
        
        currentTableCaptionA = paste0("Sex differences for ",      currentVariableName)
        currentTableNameA    = paste0("CategoricalSexDifferences", currentVariableName)
        
        currentTableCaptionB = paste0("Overrepresented Sex differences for ",     currentVariableName)
        currentTableNameB    = paste0("OverrepresentedCategoricalSexDifferences", currentVariableName)
        
        tableInfo = writeTableLATEX(currentSummaryTable, BIOMARKERS_FOLDER_TABLES_HOSTFACTORS_SEX, tableCaption = currentTableCaptionA,
                                    overrideTableName = currentTableNameA, widthProportion = 0.8)
        
        tableInfo = writeTableLATEX(currentOverrepresentedTable, BIOMARKERS_FOLDER_TABLES_HOSTFACTORS_SEX, tableCaption = currentTableCaptionB,
                                    overrideTableName = currentTableNameB, widthProportion = 0.8)
        
        #print(paste0("\\input{../../../../results/biomarkers/tables/",tableInfo[[2]],".tex}"))
        
        	
        
    }
    
    # Write the latex table
    writeTableLATEX(xi2SummaryDFT, BIOMARKERS_FOLDER_TABLES_HOSTFACTORS, tableCaption = "Sex differences for all categorical host factor",
                    overrideTableName = "SexDifferencesHostFactorsTable", widthProportion = 0.7)
        
    
}


#---------------------------------------------------------------------------
# Do biomarkers levels for every categorical variable (men and women)
#---------------------------------------------------------------------------
{
    
    # Create the tables for men and women where we are going to fill the results
    menCategoricalTotal             = readyDFModalities(completeTable, allCategoricalIndexes, allNDLIndex)
    menCategoricalAverages          = menCategoricalTotal
    menCategoricalPValues           = readyDFVariables(completeTable,  allCategoricalIndexes, allNDLIndex)
    
    colnames(menCategoricalPValues) = c("Variable",biomarkersMetadataDF$Protein) # Change the names from Protein NDL ..293848.. to just Protein
    
    menCategoricalPAsterisk   = menCategoricalPValues
    
    womenCategoricalTotal     = menCategoricalTotal
    womenCategoricalAverages  = menCategoricalTotal
    womenCategoricalPValues   = menCategoricalPValues
    womenCategoricalPAsterisk = menCategoricalPValues
    
    # We will need to addjust because we have way too many p.values, so here are two methods
    menCategoricalPValuesBonferroni     = menCategoricalPValues
    menCategoricalPValuesBenjamini      = menCategoricalPValues
    menCategoricalPAsteriskBonferroni   = menCategoricalPValues
    menCategoricalPAsteriskBenjamini    = menCategoricalPValues
    
    womenCategoricalPValuesBonferroni   = menCategoricalPValues
    womenCategoricalPValuesBenjamini    = menCategoricalPValues
    womenCategoricalPAsteriskBonferroni = menCategoricalPValues
    womenCategoricalPAsteriskBenjamini  = menCategoricalPValues
    
    # For each biomarker and for each categorical variable, check if there is
    # a significance with the NDL values, using only non NA values.
    # whether this makes sense or not, is up to the reader to check with
    # the table of LOD levels
    for(i in 1:TOTAL_BIOMARKERS){
    
        for(j in 1:length(allCategoricalIndexes)){
            
            # Get the indexes for the biomarker and the variable
            currentBiomarkerIndex = allNDLIndex[i]
            currentVariableIndex  = allCategoricalIndexes[j]
            
            # We skip the j=1 for the pvalues because that is the sex variable
            # and is only men and women
            if(j>1){
            
                # Find the p-values, and skip the unknown categories
                # later on, we will show how many unknown categories are there
                # but they are not use in the calculation
                menCategoricalPValues[j,(i+1)]     = simpleCategoricalPValue(biomenOnlyTable,   currentVariableIndex, currentBiomarkerIndex, skipUnknowns = TRUE)
                menCategoricalPAsterisk[j,(i+1)]   = getAsterkisPValue(menCategoricalPValues[j,(i+1)])
                
                womenCategoricalPValues[j,(i+1)]   = simpleCategoricalPValue(biowomenOnlyTable, currentVariableIndex, currentBiomarkerIndex, skipUnknowns = TRUE)
                womenCategoricalPAsterisk[j,(i+1)] = getAsterkisPValue(womenCategoricalPValues[j,(i+1)])
                
            }
            else{
            
                menCategoricalPValues[j,(i+1)]     = 1
                menCategoricalPAsterisk[j,(i+1)]   = 'ns'
                
                womenCategoricalPValues[j,(i+1)]   = 1
                womenCategoricalPAsterisk[j,(i+1)] = 'ns'
                
                
            }
        }
            
    }
  
    # We need to count how many significances we have in each table and see
    # if we get too many positives just by random chance. So we adjust the
    # previous results by Bonferroni and Benjamini methods
    {
    
        # First we addjust the p-values
        for(i in 1:TOTAL_BIOMARKERS){
        
            menCategoricalPValuesBonferroni[,i+1]   = p.adjust(menCategoricalPValues[,i+1],   method="bonferroni") 
            menCategoricalPValuesBenjamini[,i+1]    = p.adjust(menCategoricalPValues[,i+1],   method="fdr") 
            
            womenCategoricalPValuesBonferroni[,i+1] = p.adjust(womenCategoricalPValues[,i+1], method="bonferroni") 
            womenCategoricalPValuesBenjamini[,i+1]  = p.adjust(womenCategoricalPValues[,i+1], method="fdr") 
            
        }
        
        # Now we converted to asterisks for easy reading
        for(i in 1:TOTAL_BIOMARKERS){
            
            for(j in 1:length(allCategoricalIndexes)){
                
                # Get the indexes for the biomarker and the variable
                currentBiomarkerIndex = allNDLIndex[i]
                currentVariableIndex  = allCategoricalIndexes[j]
                
                # We skip the j=1 for the pvalues because that is the sex variable
                # and is only men and women
                if(j>1){
                    
                    menCategoricalPAsteriskBonferroni[j,(i+1)]   = getAsterkisPValue(menCategoricalPValuesBonferroni[j,(i+1)])
                    menCategoricalPAsteriskBenjamini[j,(i+1)]    = getAsterkisPValue(menCategoricalPValuesBenjamini[j,(i+1)])
                    
                    womenCategoricalPAsteriskBonferroni[j,(i+1)] = getAsterkisPValue(womenCategoricalPValuesBonferroni[j,(i+1)])
                    womenCategoricalPAsteriskBenjamini[j,(i+1)]  = getAsterkisPValue(womenCategoricalPValuesBenjamini[j,(i+1)])
                    
                    
                }
                
            }
            
        }
          
    }
    
    # All the analysis is finish now, we are going to try to make the tables
    # a bit more comprehensible for here on.
    {
    
        # First we are going to check for each cell, if there is a significance
        # in either in men or women, and label it TRUE or FALSE accordingly
        anySignificancePvalues           = menCategoricalPValues
        anySignificancePvaluesBonferroni = menCategoricalPValues
        anySignificancePvaluesBenjamini  = menCategoricalPValues
        
        for(i in 1:TOTAL_BIOMARKERS){
            
            for(j in 1:length(allCategoricalIndexes)){
                
                # Skip the sex line that is set to FALSE
                if(j>1){
                
                    if(   menCategoricalPAsteriskBenjamini[j,(i+1)] != 'ns' ||
                          womenCategoricalPAsteriskBenjamini[j,(i+1)] != 'ns' ){
                        
                        anySignificancePvaluesBenjamini[j,(i+1)] = TRUE    
                        
                    }
                    else{
                        
                        anySignificancePvaluesBenjamini[j,(i+1)] = FALSE
                        
                    }
                        
                }
                else{
                
                    anySignificancePvaluesBenjamini[j,(i+1)] = FALSE    
                    
                }
                
                
            }
            
        }
        
        # Now we mark which columns and rows need to be deleted to simplify the
        # table:
        deleteTheseBonferroniRows    = rep(FALSE, length(allCategoricalIndexes))
        deleteTheseBonferroniColumns = rep(FALSE, TOTAL_BIOMARKERS+1)
        
        deleteTheseBenjaminiRows     = rep(FALSE, length(allCategoricalIndexes))
        deleteTheseBenjaminiColumns  = rep(FALSE, TOTAL_BIOMARKERS+1)
        
        # Delete rows
        for(i in 1:length(deleteTheseBenjaminiRows)){
            
            if(sum(anySignificancePvaluesBenjamini[i,2:TOTAL_BIOMARKERS]) == 0 ){
            
                deleteTheseBenjaminiRows[i] = TRUE
                    
            }

        }
        
        # Delete columns
        for(i in 1:TOTAL_BIOMARKERS){
            
            if(sum(anySignificancePvaluesBenjamini[,i+1]) == 0 ){
                
                deleteTheseBenjaminiColumns[i+1] = TRUE
                
            }
            
        }
        
        # Now we have everything that need to be deleted. So we are going to do
        # that, and create a mixed table with men and women for easy looking.
        {
        
            menBenjaminiSimplified = menCategoricalPAsteriskBenjamini    
            menBenjaminiSimplified = menBenjaminiSimplified[!deleteTheseBenjaminiRows,
                                                            !deleteTheseBenjaminiColumns]
            
            womenBenjaminiSimplified = womenCategoricalPAsteriskBenjamini    
            womenBenjaminiSimplified = womenBenjaminiSimplified[!deleteTheseBenjaminiRows,
                                                                !deleteTheseBenjaminiColumns]
            
        }
        
        # Finally, we can also offer the complete list of significances
        # which is just a melting of the previous tables deleting all the ns
        {

            # All tables have the same structure, for both men and women
            # and whether you do simple, bonferroni or Benjamini addjustment
                        
            menSimpleMelted       = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            menBonferroniMelted   = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            menBenjaminiMelted    = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            womenSimpleMelted     = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            womenBonferroniMelted = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            womenBenjaminiMelted  = DF(nrow(menCategoricalPAsteriskBonferroni) * (ncol(menCategoricalPAsteriskBonferroni)-1), 3)
            
            colnames(menSimpleMelted)        = c("Explicative", "Biomarker", "Significance")
            colnames(menBonferroniMelted)    = c("Explicative", "Biomarker", "Significance")
            colnames(menBenjaminiMelted)     = c("Explicative", "Biomarker", "Significance")
            colnames(womenSimpleMelted)      = c("Explicative", "Biomarker", "Significance")
            colnames(womenBonferroniMelted)  = c("Explicative", "Biomarker", "Significance")
            colnames(womenBenjaminiMelted)   = c("Explicative", "Biomarker", "Significance")
            
            # For every table, do the exactly same thing
            counter = 1
            for(i in 1:nrow(menCategoricalPAsteriskBonferroni)){
            
                for(j in 1:(ncol(menCategoricalPAsteriskBonferroni)-1)){
                    
                    currentExplicative = menCategoricalPAsteriskBonferroni[i,1]
                    currentBiomarker   = colnames(menCategoricalPAsteriskBonferroni)[j+1]
                    
                    # Men
                    menSimpleMelted[counter,1]     = currentExplicative
                    menSimpleMelted[counter,2]     = currentBiomarker
                    menSimpleMelted[counter,3]     = menCategoricalPAsterisk[i,j+1]
                    
                    menBonferroniMelted[counter,1] = currentExplicative
                    menBonferroniMelted[counter,2] = currentBiomarker
                    menBonferroniMelted[counter,3] = menCategoricalPAsteriskBonferroni[i,j+1]
                    
                    menBenjaminiMelted[counter,1]  = currentExplicative
                    menBenjaminiMelted[counter,2]  = currentBiomarker
                    menBenjaminiMelted[counter,3]  = menCategoricalPAsteriskBenjamini[i,j+1]
                    
                    
                    # Women
                    womenSimpleMelted[counter,1]     = currentExplicative
                    womenSimpleMelted[counter,2]     = currentBiomarker
                    womenSimpleMelted[counter,3]     = womenCategoricalPAsterisk[i,j+1]
                    
                    womenBonferroniMelted[counter,1] = currentExplicative
                    womenBonferroniMelted[counter,2] = currentBiomarker
                    womenBonferroniMelted[counter,3] = womenCategoricalPAsteriskBonferroni[i,j+1]
                    
                    womenBenjaminiMelted[counter,1]  = currentExplicative
                    womenBenjaminiMelted[counter,2]  = currentBiomarker
                    womenBenjaminiMelted[counter,3]  = womenCategoricalPAsteriskBenjamini[i,j+1]
                    
                    # Next
                    counter = counter + 1
                }
                    
            }
           
            # Now that we have the melted results, delete the NS from the melted tables
            menSimpleMelted       = menSimpleMelted[       menSimpleMelted$Significance       != 'ns' & !is.na(menSimpleMelted$Significance),       ]
            menBonferroniMelted   = menBonferroniMelted[   menBonferroniMelted$Significance   != 'ns' & !is.na(menBonferroniMelted$Significance),   ]
            menBenjaminiMelted    = menBenjaminiMelted[    menBenjaminiMelted$Significance    != 'ns' & !is.na(menBenjaminiMelted$Significance),    ]
            womenSimpleMelted     = womenSimpleMelted[     womenSimpleMelted$Significance     != 'ns' & !is.na(womenSimpleMelted$Significance),     ]
            womenBonferroniMelted = womenBonferroniMelted[ womenBonferroniMelted$Significance != 'ns' & !is.na(womenBonferroniMelted$Significance), ]
            womenBenjaminiMelted  = womenBenjaminiMelted[  womenBenjaminiMelted$Significance  != 'ns' & !is.na(womenBenjaminiMelted$Significance),  ]
            
            # Reset the row names off all tables because R is a stupid language
            # and I hate the lack of pointers and references. So instead of
            # making an easy and simple code in C++ we need to do this hackerish
            # code tricks in this third tier language which problem has been
            # solved since the 70's
            #
            # This doesn't even have a operation of myDataframe.resetRows() :(
            row.names(menSimpleMelted)       = c(1:nrow(menSimpleMelted))
            row.names(menBonferroniMelted)   = c(1:nrow(menBonferroniMelted))
            row.names(menBenjaminiMelted)    = c(1:nrow(menBenjaminiMelted))
            row.names(womenSimpleMelted)     = c(1:nrow(womenSimpleMelted))
            row.names(womenBonferroniMelted) = c(1:nrow(womenBonferroniMelted))
            row.names(womenBenjaminiMelted)  = c(1:nrow(womenBenjaminiMelted))            
            
            # Finally, what we are going to do is to expand the list of modalities
            # of each variable under the biomarker protein, and add the averages value
            # This is already done in the megatable with the averages. The only
            # thing that is a pain is that R is very annoying adding rows to DF, so
            # this takes a while in term of memory allocation
            
            # We are going to merge both tables into one
            {
             
                bothSimpleMelted       = menSimpleMelted
                bothBonferroniMelted   = menBonferroniMelted
                bothBenjaminiMelted    = menBenjaminiMelted
                
                bothSimpleMelted$X     = NA
                bothBonferroniMelted$X = NA
                bothBenjaminiMelted$X  = NA

                colnames(bothSimpleMelted)     = c("Variable","Biomarker","Men","Women")
                colnames(bothBonferroniMelted) = c("Variable","Biomarker","Men","Women")
                colnames(bothBenjaminiMelted)  = c("Variable","Biomarker","Men","Women")
                
                # In here we have initialize the table for men, and we need to add
                # the information for women.
                #
                # So from all the column in the women table, we are going to mark
                # each row and check if the row is already here. If it is, we just
                # add the result, if it is not, we mark it to added later, and set
                # men as 'ns'
                processedWomenRowsPSimple     = rep(FALSE, nrow(womenSimpleMelted))
                processedWomenRowsPBonferroni = rep(FALSE, nrow(womenBonferroniMelted))
                processedWomenRowsPBenjamini  = rep(FALSE, nrow(womenBenjaminiMelted))
                
    
                # SIMPLE
                
                # For each row in the men table (here)
                for(i in 1:nrow(bothSimpleMelted)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = bothSimpleMelted[i,1]
                    currentBiomarkerName = bothSimpleMelted[i,2]
                    
                    # Is this in the woman table too?
                    womenMatch = womenSimpleMelted[(womenSimpleMelted[,1] == currentVariableName &
                                                    womenSimpleMelted[,2] == currentBiomarkerName),]
                    
                    # YES: Copy the significant of women here, and mark the row as copied
                    if(nrow(womenMatch) != 0){
                        
                        processedWomenRowsPSimple[as.numeric(row.names(womenMatch)[1])] = TRUE
                        bothSimpleMelted[i,4] = womenMatch[1,3]
                        
                        if(nrow(womenMatch) > 1){
                            
                            print("ERROR ERROR ERROR")
                            print(womenMatch)
                            
                        }
                        
                    }
                    # NO:  Set woman to NS
                    else{
                        
                        bothSimpleMelted[i,4] = 'ns'
                        
                    }
                    
                }
                #
                # For each row in the women table that is not copied yet
                # Dump everything into the men table (here) and set all men to NS
                remainingWomen = womenSimpleMelted[!processedWomenRowsPSimple,]
                for(i in 1:nrow(remainingWomen)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = remainingWomen[i,1]
                    currentBiomarkerName = remainingWomen[i,2]
                    currentSignificance  = remainingWomen[i,3]
                    newRow  = c(currentVariableName,currentBiomarkerName,'ns',currentSignificance)
                    
                    bothSimpleMelted = rbind(bothSimpleMelted, newRow)  
                    
                }
                
                
                # BONFERRONI
                
                # For each row in the men table (here)
                for(i in 1:nrow(bothBonferroniMelted)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = bothBonferroniMelted[i,1]
                    currentBiomarkerName = bothBonferroniMelted[i,2]
                    
                    # Is this in the woman table too?
                    womenMatch = womenBonferroniMelted[(womenBonferroniMelted[,1] == currentVariableName &
                                                        womenBonferroniMelted[,2] == currentBiomarkerName),]
                    
                    # YES: Copy the significant of women here, and mark the row as copied
                    if(nrow(womenMatch) != 0){
                        
                        processedWomenRowsPBonferroni[as.numeric(row.names(womenMatch)[1])] = TRUE
                        bothBonferroniMelted[i,4] = womenMatch[1,3]
                        
                        if(nrow(womenMatch) > 1){
                            
                            print("ERROR ERROR ERROR")
                            print(womenMatch)
                            
                        }
                        
                    }
                    # NO:  Set woman to NS
                    else{
                        
                        bothBonferroniMelted[i,4] = 'ns'
                        
                    }
                    
                }
                #
                # For each row in the women table that is not copied yet
                # Dump everything into the men table (here) and set all men to NS
                remainingWomen = womenBonferroniMelted[!processedWomenRowsPBonferroni,]
                for(i in 1:nrow(remainingWomen)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = remainingWomen[i,1]
                    currentBiomarkerName = remainingWomen[i,2]
                    currentSignificance  = remainingWomen[i,3]
                    newRow  = c(currentVariableName,currentBiomarkerName,'ns',currentSignificance)
                    
                    bothBonferroniMelted = rbind(bothBonferroniMelted, newRow)  
                     
                }
                    
                # BENJAMINI
                
                # For each row in the men table (here)
                for(i in 1:nrow(bothBenjaminiMelted)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = bothBenjaminiMelted[i,1]
                    currentBiomarkerName = bothBenjaminiMelted[i,2]
                    
                    # Is this in the woman table too?
                    womenMatch = womenBenjaminiMelted[(womenBenjaminiMelted[,1] == currentVariableName &
                                                       womenBenjaminiMelted[,2] == currentBiomarkerName),]
                    
                    # YES: Copy the significant of women here, and mark the row as copied
                    if(nrow(womenMatch) != 0){
                        
                        processedWomenRowsPBenjamini[as.numeric(row.names(womenMatch)[1])] = TRUE
                        bothBenjaminiMelted[i,4] = womenMatch[1,3]
                        
                        if(nrow(womenMatch) > 1){
                            
                            print("ERROR ERROR ERROR")
                            print(womenMatch)
                            
                        }
                        
                    }
                    # NO:  Set woman to NS
                    else{
                        
                        bothBenjaminiMelted[i,4] = 'ns'
                        
                    }
                    
                }
                #
                # For each row in the women table that is not copied yet
                # Dump everything into the men table (here) and set all men to NS
                remainingWomen = womenBenjaminiMelted[!processedWomenRowsPBenjamini,]
                for(i in 1:nrow(remainingWomen)){
                    
                    # Get the variable and get the biomarker
                    currentVariableName  = remainingWomen[i,1]
                    currentBiomarkerName = remainingWomen[i,2]
                    currentSignificance  = remainingWomen[i,3]
                    newRow  = c(currentVariableName,currentBiomarkerName,'ns',currentSignificance)
                    
                    bothBenjaminiMelted = rbind(bothBenjaminiMelted, newRow)  
                    
                }
                
                
            }
            
            # Everything is merged, and these results are a good report on its
            # own, so save it to disk
            write.csv2(bothSimpleMelted,     BOTH_P_SIMPLE)
            write.csv2(bothBenjaminiMelted,  BOTH_P_BENJAMINI)
            write.csv2(bothBonferroniMelted, BOTH_P_BONFERRONI)
            
            
            # However, now I don't want to have to look back and forth from
            # the big super average table. So we are going to copy the relevant
            # results into here.
            
            
            
            # For each row in each table
            # Add an extra column for the opposite sex significance
            currentIndex = 1
            for(i in 1:nrow(bothBonferroniMelted)){
            
                # Grab the variables where the pointer is
                currentVariableName   = bothBonferroniMelted[currentIndex,1]
                currentBiomarkerName  = bothBonferroniMelted[currentIndex,2]
                
                # Find these variable index in the big table
                currentVariableIndex  = getIndex(currentVariableName,  completeTable)    
                currentBiomarkerIndex = getIndex(currentBiomarkerName, biomarkersMetadataDF$Protein)    
                currentBiomarkerIndex = NDLIndex + currentBiomarkerIndex - 1
                
                # Find how many modalities this variable have
                currentModalities      = getModalities(completeTable, currentVariableIndex, skipUnknowns = TRUE)
                totalCurrentModalities = length(currentModalities)
                
                # To continue...
            }
            
            
            # Write the tables into disk
            write.csv2(menSimpleMelted,       MEN_P_SIMPLE)
            write.csv2(menBonferroniMelted,   MEN_P_BONFERRONI)
            write.csv2(menBenjaminiMelted,    MEN_P_BENJAMINI)
            write.csv2(womenSimpleMelted,     WOMEN_P_SIMPLE)
            write.csv2(womenBonferroniMelted, WOMEN_P_BONFERRONI)
            write.csv2(womenBenjaminiMelted,  WOMEN_P_BENJAMINI)
             
        }
        
    } 
    
    # Write tables into Latex form
    {
     
        # Sort by variable first
        bothSimpleMelted2 = bothSimpleMelted[order(bothSimpleMelted$Variable), ]
        rownames(bothSimpleMelted2) = c(1:nrow(bothSimpleMelted2))
        
        # Break the superlong table into 5 pieces
        # There is manual addjustment so it doesn't break in the middle of
        # a variable series
        FirstBothSimpleMelted   = bothSimpleMelted2[1:64,]
        SecondBothSimpleMelted  = bothSimpleMelted2[65:109,]
        ThirdBothSimpleMelted   = bothSimpleMelted2[110:149,]
        ForthBothSimpleMelted   = bothSimpleMelted2[150:205,]
        FivthBothSimpleMelted   = bothSimpleMelted2[206:253,]
        SixthBothSimpleMelted   = bothSimpleMelted2[254:297,]
        SeventhBothSimpleMelted = bothSimpleMelted2[298:343,]
        EigthBothSimpleMelted   = bothSimpleMelted2[344:386,]
        #NinthBothSimpleMelted   = bothSimpleMelted2[351:386,]

        writeTableLATEX(FirstBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (1 of 8)",
                        overrideTableName = "BiomarkersBothPSimple1", heightProportion = 0.4)        
        
        writeTableLATEX(SecondBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (2 of 8)",
                        overrideTableName = "BiomarkersBothPSimple2", heightProportion = 0.4)        
        
        writeTableLATEX(ThirdBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (3 of 8)",
                        overrideTableName = "BiomarkersBothPSimple3", heightProportion = 0.4)        
        
        writeTableLATEX(ForthBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (4 of 8)",
                        overrideTableName = "BiomarkersBothPSimple4", heightProportion = 0.4)        
                
        writeTableLATEX(FivthBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (5 of 8)",
                        overrideTableName = "BiomarkersBothPSimple5", heightProportion = 0.4)
        
        writeTableLATEX(SixthBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (6 of 8)",
                        overrideTableName = "BiomarkersBothPSimple6", heightProportion = 0.4)
        
        writeTableLATEX(SeventhBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (7 of 8)",
                        overrideTableName = "BiomarkersBothPSimple7", heightProportion = 0.4)
        
        writeTableLATEX(EigthBothSimpleMelted, BIOMARKERS_FOLDER_TABLES,     tableCaption = "Biomarkers that are statistically significant for either men or women (8 of 8)",
                        overrideTableName = "BiomarkersBothPSimple8", heightProportion = 0.4)        
        
        # Benjamini and Bonferroni
        
        writeTableLATEX(bothBenjaminiMelted, BIOMARKERS_FOLDER_TABLES,  tableCaption = "Biomarkers that are statistically significant for either men or women, after Benjamini correction",
                        overrideTableName = "BiomarkersBothPBenjamini", heightProportion = 0.4)
        
        writeTableLATEX(bothBonferroniMelted, BIOMARKERS_FOLDER_TABLES, tableCaption = "Biomarkers that are statistically significant for either men or women, after Bonferroni correction",
                        overrideTableName = "BiomarkersBothPBonferroni", heightProportion = 0.4)

    }
    
}


# Sort the results by columns

#sort by var2 ascending, then var1 ascending
bonferroniSorted = bothBonferroniMelted[order(bothBonferroniMelted$Variable, bothBonferroniMelted$Biomarker), ]

writeTableLATEX(bonferroniSorted, BIOMARKERS_FOLDER_TABLES, tableCaption = "Biomarkers that are statistically significant for either men or women, after Bonferroni correction",
                overrideTableName = "BiomarkersBothPBonferroni", heightProportion = 0.4)



#---------------------------------------------------------------------------------------------------
# Do all subtables with all averages for men and women , with all host factors, with all biomarkers
#---------------------------------------------------------------------------------------------------
{

    for(i in 1:TOTAL_BIOMARKERS){
        
        for(j in 1:length(allCategoricalIndexes)){
            
            # Get the indexes for the biomarker and the variable
            currentBiomarkerIndex = allNDLIndex[i]
            currentVariableIndex  = allCategoricalIndexes[j]
            
            # We skip the j=1 for the pvalues because that is the sex variable
            # and is only men and women
            if(j>1){
                
                currentBiomarker  = biomarkersMetadataDF$Acronym[i]
                currentHostFactor = colnames(completeTable)[currentVariableIndex]
            
                # Do a box plot for both men and women for these variables
            
                currentTitle    = paste0("Levels for ",currentBiomarker, " with ", currentHostFactor)
                currentOverride = paste0("Levels_",currentBiomarker, "_", currentHostFactor)
                    
                doBiCategoricalBoxPlot(completeTable, currentVariableIndex, currentBiomarkerIndex, sexIndex, BIOMARKERS_FOLDER_IMAGES_HOSTFACTORS_LEVELS,
                                       colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                                       plotTitle = currentTitle, plotSubtitle = "Stratified by sex",
                                       overrideTableName = currentOverride)
                    
                
            }
        }
            
        
            
    
    }
        
  
}
