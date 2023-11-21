#---------------------------------------------------------------------------
# Do biomarkers levels for every disease (men and women if they exist)
#---------------------------------------------------------------------------
{

    # Specify the cutoff limit for analizing diseases.
    # For example, if we have only two people with a diseases, that is too little
    # for a proper statistical analysis. If we have 100, that is a lot and enough.
    # Somewhere in between is the magic spot, so you set that threshold here:
    minimumDiseasesRequiered = 4
    
    # Divide the disease DB into men and women
    {
    
        totalDiseases   = nrow(diseasesDBDF)
        
        diseasesMenDF   = diseasesDBDF
        diseasesWomenDF = diseasesDBDF
        
        thisRowIsMan    = rep(FALSE, totalDiseases)
        
        # For each row
        for(i in 1:totalDiseases){
        
            # Is this ID a man or a woman?
            currentID  = diseasesMenDF$ID[i]
            currentSex = completeTable$Sex[currentID]
            
            if(currentSex == "Man") thisRowIsMan[i] = TRUE
            
        }
            
        diseasesMenDF   = diseasesMenDF[thisRowIsMan,]
        diseasesWomenDF = diseasesWomenDF[!thisRowIsMan,]
        
        # Put both into a list that we will use later to iterate
        diseaseDFs = newList(2)
        diseaseDFs[[1]] = diseasesMenDF
        diseaseDFs[[2]] = diseasesWomenDF
    }
    
    # Get the general statistics for every disease
    {

        
        # Summarizing tables for general diseases and specific diseases
        
        currentGeneralSummaryMen          = summarizeCategorical(diseasesMenDF, 4,   roundMe = 2)
        currentSpecializationSummaryMen   = summarizeCategorical(diseasesMenDF, 2,   roundMe = 2)
        currentGeneralSummaryWomen        = summarizeCategorical(diseasesWomenDF, 4, roundMe = 2)
        currentSpecializationSummaryWomen = summarizeCategorical(diseasesWomenDF, 2, roundMe = 2)            
        
        writeTableLATEX(currentGeneralSummaryMen,   BIOMARKERS_FOLDER_TABLES_DISEASES,  tableCaption = "Absolute frequency of diseases for MEN by ICD10 group",
                        overrideTableName = "GeneralDiseasesMen", heightProportion = 0.2)
        
        writeTableLATEX(currentGeneralSummaryWomen, BIOMARKERS_FOLDER_TABLES_DISEASES,  tableCaption = "Absolute frequency of diseases for WOMEN by ICD10 group",
                        overrideTableName = "GeneralDiseasesWomen", heightProportion = 0.2)
        
        writeTableLATEX(currentSpecializationSummaryMen,   BIOMARKERS_FOLDER_TABLES_DISEASES,  tableCaption = "Absolute frequency of diseases for MEN by ICD10",
                        overrideTableName = "SpecificDiseasesMen", heightProportion = 0.5)
        
        writeTableLATEX(currentSpecializationSummaryWomen, BIOMARKERS_FOLDER_TABLES_DISEASES,  tableCaption = "Absolute frequency of diseases for WOMEN by ICD10",
                        overrideTableName = "SpecificDiseasesWomen", heightProportion = 0.5)        
        
        
        # Histograms
        
        # Create a dataframe where we are going to write how many diseases each person has
        totalDiseasesDF = DF(TOTAL_PEOPLE, 2, 0)
        colnames(totalDiseasesDF) = c("ID", "Diseases")
        totalDiseasesDF[,1] = completeTable$ID
        
        for(i in 1:nrow(diseasesDBDF)){
        
            # Get the id
            currentID = diseasesDBDF$ID[i]
            totalDiseasesDF[currentID,2] = totalDiseasesDF[currentID,2] + 1
            
        }
        
        
        
        
    }
    
    # Make a table of healthy men and healthy women for reference
    {
        sickMenIDs      = unique(diseasesMenDF$ID)
        sickWomenIDs    = unique(diseasesWomenDF$ID)
        healthyMenIDs   = menOnlyTable$ID[!(menOnlyTable$ID     %in% sickMenIDs)]
        healthyWomenIDs = womenOnlyTable$ID[!(womenOnlyTable$ID %in% sickWomenIDs)]
        
        healthyMenDF    = completeTable[completeTable$ID %in% healthyMenIDs,]
        healthyWomenDF  = completeTable[completeTable$ID %in% healthyWomenIDs,]
        
        menOnlyTable$CurrentHealth   = "Healthy"
        womenOnlyTable$CurrentHealth = "Healthy"
        
        for(i in 1:nrow(menOnlyTable)){
            
            currentID = menOnlyTable$ID[i]
            
            if(currentID %in% sickMenIDs) menOnlyTable$CurrentHealth[i] = "Sick"
            
        }
        
        for(i in 1:nrow(womenOnlyTable)){
            
            currentID = womenOnlyTable$ID[i]
            
            if(currentID %in% sickWomenIDs) womenOnlyTable$CurrentHealth[i] = "Sick"
            
        }
        
    }
    
    # Find out how many valid diseases we have for each sex
    # Prepare a summary DF accordingly
    {
    
        validDiseasesMenDF      = currentSpecializationSummaryMen[currentSpecializationSummaryMen$Count >= minimumDiseasesRequiered,]
        menDiseasesList         = as.character(validDiseasesMenDF[,1])
        totalDiseasesMen        = length(menDiseasesList)
            
        validDiseasesWomenDF    = currentSpecializationSummaryWomen[currentSpecializationSummaryWomen$Count >= minimumDiseasesRequiered,]
        womenDiseasesList       = as.character(validDiseasesWomenDF[,1])
        totalDiseasesWomen      = length(womenDiseasesList)
        
        summaryDiseasesMenDF           = DF(TOTAL_BIOMARKERS, totalDiseasesMen+1)
        colnames(summaryDiseasesMenDF) = c("Protein", menDiseasesList)
        summaryDiseasesMenDF[,1]       = biomarkersMetadataDF$Protein
        
        summaryDiseasesWomenDF           = DF(TOTAL_BIOMARKERS, totalDiseasesWomen+1)
        colnames(summaryDiseasesWomenDF) = c("Protein", womenDiseasesList)        
        summaryDiseasesWomenDF[,1]       = biomarkersMetadataDF$Protein
    }
    
    
    # For those we have enough people, compare biomarkers with reference
    {
    
        # Make a DF that contain the template for the results.
        # This has, the biomarker name, the significance value with the 3 options
        # (no correction, benjamini, bonferroni), the average for the disease
        # group, and the average for the control group.
        #
        # The template for men and women is the same, and only the averages
        # and significance changes, so let create one for each.
        templateMenDF           = DF(TOTAL_BIOMARKERS, 7)
        colnames(templateMenDF) = c("Protein", "No correction", "Benjamini", "Bonferroni", "Avg Disease", "Avg Healthy", "Image")
        templateMenDF[,1]       = biomarkersMetadataDF$Protein
        templateWomenDF         = templateMenDF
        for(i in 1:TOTAL_BIOMARKERS){
        
            templateMenDF[i,6]   =  mean(healthyMenDF[,   (firstBiomarkerIndex + i - 1)] ,  na.rm = TRUE)
            templateWomenDF[i,6] =  mean(healthyWomenDF[, (firstBiomarkerIndex + i - 1)] ,  na.rm = TRUE)    
            
        }
        
        menResultsList   = newList(totalDiseasesMen)
        womenResultsList = newList(totalDiseasesWomen)
        
        
        
        # From here onward we do the analysis
        currentSex                 = "Men"
        currentDiseasesBySex       = diseasesMenDF
        currentSpecializationTable = currentSpecializationSummaryMen
        currentHealthyTable        = healthyMenDF
        currentSexTable            = menOnlyTable
        currentResultsList         = menResultsList
        currentTemplate            = templateMenDF
        currentMetaSummary         = summaryDiseasesMenDF
        
        # For each sex (men and women)
        for(s in 1:2){
            
            # Set the proper tables
            if (s == 2){
                
                currentSex                 = "Women" 
                currentDiseasesBySex       = diseasesWomenDF
                currentSpecializationTable = currentSpecializationSummaryWomen        
                currentHealthyTable        = healthyWomenDF
                currentSexTable            = womenOnlyTable
                currentResultsList         = womenResultsList
                currentTemplate            = templateWomenDF
                currentMetaSummary         = summaryDiseasesWomenDF
            } 

            # Get all the diseases that are above the given disease threshold
            validDiseasesDF      = currentSpecializationTable[currentSpecializationTable$Count >= minimumDiseasesRequiered,]
            currentDiseaseList   = as.character(validDiseasesDF[,1])
            totalCurrentDiseases = length(currentDiseaseList)
            
            # For each disease
            for(d in 1:totalCurrentDiseases){
                
                # Get the disease
                currentDisease = currentDiseaseList[d]
                
                # Get all the IDs for people that coincide with the current disease
                peopleWithThisDisease  = currentDiseasesBySex[currentDiseasesBySex$Diagnostic == currentDisease,]$ID
                totalPeopleWithDisease = length(peopleWithThisDisease)
                diseasePeopleDF        = completeTable[completeTable$ID %in% peopleWithThisDisease,]
                
                # Prepare the template
                diseaseSpecificTemplate = currentTemplate
                
                # For each biomarker    
                for(b in 1:TOTAL_BIOMARKERS){
                    
                    # Init the biomarker
                    currentBiomarkerName = biomarkersMetadataDF$Protein[b]
                    
                    # Prepare the subtable for this case
                    currentDataDF           = DF(nrow(currentHealthyTable) + totalPeopleWithDisease , 2)
                    colnames(currentDataDF) = c(currentBiomarkerName, "CurrentHealth")
                    currentDataDF[,2]       = c(rep("Healthy", nrow(currentHealthyTable)) , rep("Sick", totalPeopleWithDisease))
                    currentDataDF[,1]       = c(currentHealthyTable[, (firstBiomarkerIndex + b - 1)] , diseasePeopleDF[, (firstBiomarkerIndex + b - 1)])
                    
                    #print(head(currentDataDF))
                    #print(tail(currentDataDF))
                    
                    # Do the analysis
                    currentPValue                = simpleCategoricalPValue(currentDataDF, 2, 1,    skipUnknowns = TRUE)
                    diseaseSpecificTemplate[b,2] = currentPValue
                    diseaseSpecificTemplate[b,5] = mean(diseasePeopleDF[, (firstBiomarkerIndex + b - 1)], na.rm = TRUE)
                    
                }
                
                # The analysis is finish, do the benjamini and bonferroni corrections
                diseaseSpecificTemplate[,3] = p.adjust(diseaseSpecificTemplate[,2], method="fdr") 
                diseaseSpecificTemplate[,4] = p.adjust(diseaseSpecificTemplate[,2], method="bonferroni") 
                # Transform into asterisk
                diseaseSpecificTemplate[,2] = getAsterkisPValue(diseaseSpecificTemplate[,2], nsEmpty = TRUE)
                diseaseSpecificTemplate[,3] = getAsterkisPValue(diseaseSpecificTemplate[,3], nsEmpty = TRUE)
                diseaseSpecificTemplate[,4] = getAsterkisPValue(diseaseSpecificTemplate[,4], nsEmpty = TRUE)
                # Round the averages
                diseaseSpecificTemplate[,5] = round(diseaseSpecificTemplate[,5],2)
                diseaseSpecificTemplate[,6] = round(diseaseSpecificTemplate[,6],2)
                
                # Add the results to the metasummary
                currentMetaSummary[,(d+1)]  = diseaseSpecificTemplate[,4]
                
                
                # Write the table into disk
                
                currentDiseaseClean = cleanWeirdCharacters(currentDisease) #Make the disease name Latex compatible for the filename
                  
                currentCaption   = paste0( currentSex," table for biomarkers significance, disease ",currentDisease)
                currentTableName = paste0("DiseasesSummaryii",s,"ii",d) 
                
                latexString = writeTableLATEX(diseaseSpecificTemplate, BIOMARKERS_FOLDER_TABLES_DISEASES, tableCaption = currentCaption,
                                              overrideTableName = currentTableName, heightProportion = 0.5)
                
                
                
                latexString = paste0("input{../../../../results/biomarkers/tables/diseases/", latexString[[2]], ".tex}")
                
                print(latexString)
                
                # Write the table into the list
                currentResultsList[[d]] = diseaseSpecificTemplate
                
            }
            
            # Save the metasummary, because R is a stupid language and
            # passing variable as reference is so difficult these days -_-
            if (s == 1) summaryDiseasesMenDF   = currentMetaSummary
            if (s == 2) summaryDiseasesWomenDF = currentMetaSummary
            
            
        }
        
        

            
    }
        
    
    # Write the summaries into disk
    writeTableLATEX(summaryDiseasesMenDF, BIOMARKERS_FOLDER_TABLES_DISEASES, tableCaption = "Significant biomarkers for diseases and men, addjusted for Bonferroni",
                    overrideTableName = "MenDiseasesSummary", heightProportion = 0.5, rotateColumnHeaders = TRUE)
    
    # Write the summaries into disk
    writeTableLATEX(summaryDiseasesWomenDF, BIOMARKERS_FOLDER_TABLES_DISEASES, tableCaption = "Significant biomarkers for diseases and women, addjusted for Bonferroni",
                    overrideTableName = "WomenDiseasesSummary", heightProportion = 0.5, rotateColumnHeaders = TRUE)
    
    
}


#---------------------------------------------------------------------------
# Do biomarkers levels for every medicine, with focus on hormonal
#---------------------------------------------------------------------------
{
    
    # Specify the cutoff limit for minimum amount of medicine taken
    # (same as with disease)
    minimumMedicineRequiered = 4
    
    # Divide the disease DB into men and women
    {
    
        totalMedicines  = nrow(medicinesDBDF)
        
        medicinesMenDF   = medicinesDBDF
        medicinesWomenDF = medicinesDBDF
        
        thisRowIsMan    = rep(FALSE, totalMedicines)
        
        # For each row
        for(i in 1:totalMedicines){
        
            # Is this ID a man or a woman?
            currentID  = medicinesMenDF$ID[i]
            currentSex = as.character(completeTable$Sex[currentID])  # FUCKING STRINGS!!! NO LEVELS!!!! STRINGS!! AAAAH, I hate casting in R
            
            if(currentSex == "Man") thisRowIsMan[i] = TRUE
            
        }
            
        medicinesMenDF   = medicinesMenDF[thisRowIsMan,]
        medicinesWomenDF = medicinesWomenDF[!thisRowIsMan,]
        
        # Put both into a list that we will use later to iterate
        medicinesDFs      = newList(2)
        medicinesDFs[[1]] = medicinesMenDF
        medicinesDFs[[2]] = medicinesWomenDF
    }
    
    # Get the general statistics for every disease
    {

        currentGeneralSummaryMen          = summarizeCategorical(medicinesMenDF, 2,   roundMe = 2)
        currentSpecializationSummaryMen   = summarizeCategorical(medicinesMenDF, 3,   roundMe = 2)
        currentGeneralSummaryWomen        = summarizeCategorical(medicinesWomenDF, 2, roundMe = 2)
        currentSpecializationSummaryWomen = summarizeCategorical(medicinesWomenDF, 3, roundMe = 2)            
        
        writeTableLATEX(currentGeneralSummaryMen,          BIOMARKERS_FOLDER_TABLES_MEDICINES,  tableCaption = "Absolute frequency of medicines for MEN by ATC group",
                        overrideTableName = "GeneralMedicineMen",    heightProportion = 0.1)
        
        writeTableLATEX(currentGeneralSummaryWomen,        BIOMARKERS_FOLDER_TABLES_MEDICINES,  tableCaption = "Absolute frequency of medicines for WOMEN by ATC group",
                        overrideTableName = "GeneralMedicineWomen",  heightProportion = 0.1)
        
        writeTableLATEX(currentSpecializationSummaryMen,   BIOMARKERS_FOLDER_TABLES_MEDICINES,  tableCaption = "Absolute frequency of medicines for MEN by brand",
                        overrideTableName = "SpecificMedicineMen",   widthProportion = 0.5, heightProportion = 0.5)
        
        writeTableLATEX(currentSpecializationSummaryWomen, BIOMARKERS_FOLDER_TABLES_MEDICINES,  tableCaption = "Absolute frequency of medicines for WOMEN by brand",
                        overrideTableName = "SpecificMedicineWomen", widthProportion = 0.5, heightProportion = 0.5)        
        
        
        
    }
    
    # Make a table of medicine-free men and women for reference
    {
        sickMenIDs      = unique(medicinesMenDF$ID)
        sickWomenIDs    = unique(medicinesWomenDF$ID)
        healthyMenIDs   = menOnlyTable$ID[!(menOnlyTable$ID     %in% sickMenIDs)]
        healthyWomenIDs = womenOnlyTable$ID[!(womenOnlyTable$ID %in% sickWomenIDs)]
        
        healthyMenDF    = completeTable[completeTable$ID %in% healthyMenIDs,]
        healthyWomenDF  = completeTable[completeTable$ID %in% healthyWomenIDs,]
        
        menOnlyTable$CurrentMedicine   = "Healthy"
        womenOnlyTable$CurrentMedicine = "Healthy"
        
        for(i in 1:nrow(menOnlyTable)){
            
            currentID = menOnlyTable$ID[i]
            
            if(currentID %in% sickMenIDs)   menOnlyTable$CurrentMedicine[i]   = "Medicated"
            
        }
        
        for(i in 1:nrow(womenOnlyTable)){
            
            currentID = womenOnlyTable$ID[i]
            
            if(currentID %in% sickWomenIDs) womenOnlyTable$CurrentMedicine[i] = "Sick"
            
        }
        
    }
    
    # Find out how many valid medicines we have for each sex
    # Prepare a summary DF accordingly
    {
    
        validMedicinesMenDF      = currentSpecializationSummaryMen[currentSpecializationSummaryMen$Count >= minimumMedicineRequiered,]
        menMedicinesList         = as.character(validMedicinesMenDF[,1])
        totalMedicinesMen        = length(menMedicinesList)
            
        validMedicinesWomenDF    = currentSpecializationSummaryWomen[currentSpecializationSummaryWomen$Count >= minimumMedicineRequiered,]
        womenMedicinesList       = as.character(validMedicinesWomenDF[,1])
        totalMedicinesWomen      = length(womenMedicinesList)
        
        summaryMedicinesMenDF             = DF(TOTAL_BIOMARKERS, totalMedicinesMen+1)
        colnames(summaryMedicinesMenDF)   = c("Protein", menMedicinesList)
        summaryMedicinesMenDF[,1]         = biomarkersMetadataDF$Protein
        
        summaryMedicinesWomenDF           = DF(TOTAL_BIOMARKERS, totalMedicinesWomen+1)
        colnames(summaryMedicinesWomenDF) = c("Protein", womenMedicinesList)        
        summaryMedicinesWomenDF[,1]       = biomarkersMetadataDF$Protein
    }
    
    
    # For those we have enough people, compare biomarkers with reference
    {
    
        # Make a DF that contain the template for the results.
        # This has, the biomarker name, the significance value with the 3 options
        # (no correction, benjamini, bonferroni), the average for the disease
        # group, and the average for the control group.
        #
        # The template for men and women is the same, and only the averages
        # and significance changes, so let create one for each.
        templateMenDF           = DF(TOTAL_BIOMARKERS, 7)
        colnames(templateMenDF) = c("Protein", "No correction", "Benjamini", "Bonferroni", "Avg Medicated", "Avg Healthy", "Image")
        templateMenDF[,1]       = biomarkersMetadataDF$Protein
        templateWomenDF         = templateMenDF
        for(i in 1:TOTAL_BIOMARKERS){
        
            templateMenDF[i,6]   =  mean(healthyMenDF[,   (firstBiomarkerIndex + i - 1)] ,  na.rm = TRUE)
            templateWomenDF[i,6] =  mean(healthyWomenDF[, (firstBiomarkerIndex + i - 1)] ,  na.rm = TRUE)    
            
        }
        
        menResultsList   = newList(totalMedicinesMen)
        womenResultsList = newList(totalMedicinesWomen)
        
        
        
        # From here onward we do the analysis
        currentSex                 = "Men"
        currentDiseasesBySex       = medicinesMenDF
        currentSpecializationTable = currentSpecializationSummaryMen
        currentHealthyTable        = healthyMenDF
        currentSexTable            = menOnlyTable
        currentResultsList         = menResultsList
        currentTemplate            = templateMenDF
        currentMetaSummary         = summaryMedicinesMenDF
        
        # For each sex (men and women)
        for(s in 1:2){
            
            # Set the proper tables
            if (s == 2){
                
                currentSex                 = "Women" 
                currentDiseasesBySex       = medicinesWomenDF
                currentSpecializationTable = currentSpecializationSummaryWomen        
                currentHealthyTable        = healthyWomenDF
                currentSexTable            = womenOnlyTable
                currentResultsList         = womenResultsList
                currentTemplate            = templateWomenDF
                currentMetaSummary         = summaryMedicinesWomenDF
            } 

            # Get all the diseases that are above the given disease threshold
            validDiseasesDF      = currentSpecializationTable[currentSpecializationTable$Count >= minimumMedicineRequiered,]
            currentDiseaseList   = as.character(validDiseasesDF[,1])
            totalCurrentDiseases = length(currentDiseaseList)

            # For each disease
            for(d in 1:totalCurrentDiseases){

                # Get the disease
                currentDisease = currentDiseaseList[d]
                
                # Get all the IDs for people that coincide with the current disease
                peopleWithThisDisease  = currentDiseasesBySex[currentDiseasesBySex$Brand == currentDisease,]$ID
                totalPeopleWithDisease = length(peopleWithThisDisease)
                diseasePeopleDF        = completeTable[completeTable$ID %in% peopleWithThisDisease,]
                
                # Prepare the template
                diseaseSpecificTemplate = currentTemplate
                
                # For each biomarker    
                for(b in 1:TOTAL_BIOMARKERS){

                    # Init the biomarker
                    currentBiomarkerName = biomarkersMetadataDF$Protein[b]
                    
                    # Prepare the subtable for this case
                    currentDataDF           = DF(nrow(currentHealthyTable) + totalPeopleWithDisease , 2)
                    colnames(currentDataDF) = c(currentBiomarkerName, "CurrentHealth")
                    currentDataDF[,2]       = c(rep("Healthy", nrow(currentHealthyTable)) , rep("Medicated", totalPeopleWithDisease))
                    currentDataDF[,1]       = c(currentHealthyTable[, (firstBiomarkerIndex + b - 1)] , diseasePeopleDF[, (firstBiomarkerIndex + b - 1)])

                    # Do the analysis
                    currentPValue                = simpleCategoricalPValue(currentDataDF, 2, 1,    skipUnknowns = TRUE)
                    diseaseSpecificTemplate[b,2] = currentPValue
                    diseaseSpecificTemplate[b,5] = mean(diseasePeopleDF[, (firstBiomarkerIndex + b - 1)], na.rm = TRUE)
                    
                }
                
                # The analysis is finish, do the benjamini and bonferroni corrections
                diseaseSpecificTemplate[,3] = p.adjust(diseaseSpecificTemplate[,2], method="fdr") 
                diseaseSpecificTemplate[,4] = p.adjust(diseaseSpecificTemplate[,2], method="bonferroni") 
                # Transform into asterisk
                diseaseSpecificTemplate[,2] = getAsterkisPValue(diseaseSpecificTemplate[,2], nsEmpty = TRUE)
                diseaseSpecificTemplate[,3] = getAsterkisPValue(diseaseSpecificTemplate[,3], nsEmpty = TRUE)
                diseaseSpecificTemplate[,4] = getAsterkisPValue(diseaseSpecificTemplate[,4], nsEmpty = TRUE)
                # Round the averages
                diseaseSpecificTemplate[,5] = round(diseaseSpecificTemplate[,5],2)
                diseaseSpecificTemplate[,6] = round(diseaseSpecificTemplate[,6],2)
                
                # Add the results to the metasummary
                currentMetaSummary[,(d+1)]  = diseaseSpecificTemplate[,4]
                
                
                # Write the table into disk
                
                currentDiseaseClean = cleanWeirdCharacters(currentDisease) #Make the disease name Latex compatible for the filename
                  
                currentCaption   = paste0( currentSex," table for biomarkers significance, medicine ",currentDisease)
                currentTableName = paste0("MedicinesSummaryii",s,"ii",d) 
                
                latexString = writeTableLATEX(diseaseSpecificTemplate, BIOMARKERS_FOLDER_TABLES_MEDICINES, tableCaption = currentCaption,
                                              overrideTableName = currentTableName, heightProportion = 0.5)
                
                
                
                latexString = paste0("input{../../../../results/biomarkers/tables/medicines/", latexString[[2]], ".tex}")
                
                print(latexString)
                
                # Write the table into the list
                currentResultsList[[d]] = diseaseSpecificTemplate
                
            }
            
            # Save the metasummary, because R is a stupid language and
            # passing variable as reference is so difficult these days -_-
            if (s == 1) summaryMedicinesMenDF   = currentMetaSummary
            if (s == 2) summaryMedicinesWomenDF = currentMetaSummary
            
            
        }
        
        

            
    }
        
    
    # Write the summaries into disk
    writeTableLATEX(summaryMedicinesMenDF,   BIOMARKERS_FOLDER_TABLES_MEDICINES, tableCaption = "Significant biomarkers for medicines and men, addjusted for Bonferroni",
                    overrideTableName = "MenMedicinesSummary", heightProportion = 0.5, rotateColumnHeaders = TRUE)
    
    # Write the summaries into disk
    writeTableLATEX(summaryMedicinesWomenDF, BIOMARKERS_FOLDER_TABLES_MEDICINES, tableCaption = "Significant biomarkers for medicines and women, addjusted for Bonferroni",
                    overrideTableName = "WomenMedicinesSummary", heightProportion = 0.5, rotateColumnHeaders = TRUE)
    
    
    # Do the same analysis as before, but for women only, and hormonal with
    # specific look at hormonal type by levels of progesteron / estradiol
    # take healthy women (disease) only this time
    if(FALSE){
        
        # Get how many hormonal we are using
        totalHormonalModalities = length(unique(womenMenstruatingTable$HormonalContraceptives))
        
        # Prepare the dataset with the results
        hormonalResultsDF           = DF(TOTAL_BIOMARKERS,4)
        colnames(hormonalResultsDF) = c("Protein", "Significance", "Benjamini", "Bonferroni")
        
        # Find all p-values
        for(i in 1:TOTAL_BIOMARKERS){
    
            # Get the indexes for the biomarker and the variable
            currentBiomarkerIndex = allNDLIndex[i]
            currentBiomarkerName  = biomarkersMetadataDF$Protein[i]
        
            # Get the result of the t-test
            currentPValue          = simpleCategoricalPValue(womenMenstruatingTable, hormonalIndex, currentBiomarkerIndex, skipUnknowns = TRUE)
            #currentSignificance    = getAsterkisPValue(currentPValue)
            
            hormonalResultsDF[i,1] = currentBiomarkerName
            hormonalResultsDF[i,2] = currentPValue
            
        }
        
        # Do the Benjamini and Bonferroni Corrections
        {
            
            hormonalResultsDF[,3] = p.adjust(hormonalResultsDF[,2],   method="fdr") 
            hormonalResultsDF[,4] = p.adjust(hormonalResultsDF[,2],   method="bonferroni") 
            
            
        }
        
        # I have no idea of why this need to be done, but the code doesn't
        # works if I use DF directly instead of DF2
        hormonalResultsDF2           = hormonalResultsDF
        
        # Convert to significance for easy reading
        {
            
            for(i in 1:TOTAL_BIOMARKERS){
                
                hormonalResultsDF2[i,2] = getAsterkisPValue(hormonalResultsDF[i,2])
                hormonalResultsDF2[i,3] = getAsterkisPValue(hormonalResultsDF[i,3])
                hormonalResultsDF2[i,4] = getAsterkisPValue(hormonalResultsDF[i,4])
                
            }

        }
        
        # Save to disk
        {
        
            write.csv2(hormonalResultsDF2,  HORMONAL_FILEPATH)
            
            
        }
            
    }
    
}


# Make a subtable of medicines women for taking screenshot for presentations
# This is not in the paper!
{
    
    # Compare Ibux
    {
      
        tinySummaryMedicinesWomenDF = summaryMedicinesWomenDF
        tinySummaryMedicinesWomenDF = tinySummaryMedicinesWomenDF[,c(1,4,8,18)]
    
        deleteTheseRows = rep(FALSE, TOTAL_BIOMARKERS)
        for(i in 1:TOTAL_BIOMARKERS){
    
            Ablank = tinySummaryMedicinesWomenDF[i,2] == ""
            Bblank = tinySummaryMedicinesWomenDF[i,3] == ""
            Cblank = tinySummaryMedicinesWomenDF[i,4] == ""
     
            if(Ablank == TRUE & Bblank == TRUE & Cblank == TRUE){
            
                deleteTheseRows[i] = TRUE
            
            }
               
        }
    
    
        tinySummaryMedicinesWomenDF = tinySummaryMedicinesWomenDF[!deleteTheseRows,]
    
        writeTableLATEX(tinySummaryMedicinesWomenDF, BIOMARKERS_FOLDER_TABLES_MEDICINES, tableCaption = "Comparison of antiinflamatories with respect biomarkers",
                        overrideTableName = "WomenIbuxBiomarkers", heightProportion = 0.3, rotateColumnHeaders = TRUE)  
        
    }

    # Compare Hormonal
    {
        tinySummaryMedicinesWomenDF = summaryMedicinesWomenDF
        tinySummaryMedicinesWomenDF = tinySummaryMedicinesWomenDF[,c(1,3,5,9,10,11,12,14,19)]
    
        deleteTheseRows = rep(FALSE, TOTAL_BIOMARKERS)
        for(i in 1:TOTAL_BIOMARKERS){
    
            Ablank = tinySummaryMedicinesWomenDF[i,2] == ""
            Bblank = tinySummaryMedicinesWomenDF[i,3] == ""
            Cblank = tinySummaryMedicinesWomenDF[i,4] == ""
            Dblank = tinySummaryMedicinesWomenDF[i,5] == ""
            Eblank = tinySummaryMedicinesWomenDF[i,6] == ""
            Fblank = tinySummaryMedicinesWomenDF[i,7] == ""
            Gblank = tinySummaryMedicinesWomenDF[i,8] == ""
            Hblank = tinySummaryMedicinesWomenDF[i,9] == ""
            
            if(Ablank == TRUE & Bblank == TRUE & Cblank == TRUE &
               Dblank == TRUE & Eblank == TRUE & Fblank == TRUE &
               Gblank == TRUE & Hblank == TRUE){
            
                deleteTheseRows[i] = TRUE
            
            }
               
        }
    
    
        tinySummaryMedicinesWomenDF = tinySummaryMedicinesWomenDF[!deleteTheseRows,]
    
        writeTableLATEX(tinySummaryMedicinesWomenDF, BIOMARKERS_FOLDER_TABLES_MEDICINES, tableCaption = "Comparison of hormonal contraceptives with respect biomarkers",
                        overrideTableName = "WomenContraceptivesBiomarkers", heightProportion = 0.3, rotateColumnHeaders = TRUE)    
        
    }
    
}
