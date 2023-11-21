library(broom) # How completely stupid R is, that you can't extract the p-value
               # from a model, without using an external library T_T

# Start making the HTML document
{
    HTMLReportString = generateEmptyResults()
}

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
        
        antropometryTotal           = DF(ncol(antropometricTable), 3)
        colnames(antropometryTotal) = c("Variable", "NonNA", "Prc")
        
        # -- Init the names and count
        for(i in 1:ncol(antropometricTable)){
            
            antropometryTotal[i,1] = colnames(antropometricTable)[i]
            antropometryTotal[i,2] = sum(!is.na(antropometricTable[i]))
            antropometryTotal[i,3] = sum(!is.na(antropometricTable[i]))/TOTAL_PEOPLE
            
        }
        
    }
    
}

#---------------------------------------------------------------------------
# Do the basic blood analysis
#
# -- This generate the blood summary table, as well as the differences
#    for each blood level between men and women
#
# -- Figure that is very similar to the biomarker one, but for blood
#    instead, and show the absolute differences between men and women
#---------------------------------------------------------------------------
{

    # Copy the original metadata
    bloodMetadataLatexDF = bloodMetadataDF
    # Delete the ID and all event data from the table, not interested
    bloodMetadataLatexDF = bloodMetadataLatexDF[-c(1:17),]
    # Delete the original name, not interesting in the article
    bloodMetadataLatexDF$Original = NULL
    # Rename the final columns
    colnames(bloodMetadataLatexDF)[4] = "Men Lower Limit"
    colnames(bloodMetadataLatexDF)[5] = "Men Upper Limit"
    colnames(bloodMetadataLatexDF)[6] = "Women Lower Limit"
    colnames(bloodMetadataLatexDF)[7] = "Women Upper Limit"
    # Add men, women and significance
    bloodMetadataLatexDF$Men_Average     = 0
    bloodMetadataLatexDF$Women_Average   = 0
    bloodMetadataLatexDF$Significance    = 0
    bloodMetadataLatexDF$Freq_Men_Out    = 0
    bloodMetadataLatexDF$Freq_Women_Out  = 0
    
    # Prepare the DF for the blood differences by sex
    sexBloodSummmaryDF            = DF(totalBloodColumns,7)
    colnames(sexBloodSummmaryDF)  = c("BloodLevel", "PValue", "Significance","DeltaSigma","AbsSigma","Type", "AvgPro")
    sexBloodSummmaryDF$BloodLevel = bloodMetadataLatexDF$Short
    
    # Trick the upper and lower limit
    # We don't have the information for this yet, so just put whatever there
    # base in the SD. Everybody +-2SD away are bad for example
    {
    
        for(i in 1:nrow(bloodMetadataLatexDF)){

            # R, and RStudio in particular, are a a POS eviroments for debugging
            # It doesn't tell you almost never the line where the error is located,
            # and in RStudio 4.x now it has an option by default that send you
            # to a weird browse() function, in the lowest level functions, every
            # time that there is an error. I completely, and utterly, hate this
            # language and I hope one day it dies in a fire.
            
            if(bloodMetadataLatexDF$`Men Upper Limit`[i] == 9999){
                
                currentAverage     = mean(completeTable[, (firstBloodIndex + i - 1)] , na.rm = TRUE)
                currentSTdeviation = sd(completeTable[, (firstBloodIndex + i - 1)] , na.rm = TRUE)
                
                newUpperLimit = currentAverage + 2*currentSTdeviation
                newLowerLimit = currentAverage - 2*currentSTdeviation
                
                bloodMetadataLatexDF$`Men Upper Limit`[i]   = newUpperLimit
                bloodMetadataLatexDF$`Men Lower Limit`[i]   = newLowerLimit
                bloodMetadataLatexDF$`Women Upper Limit`[i] = newUpperLimit
                bloodMetadataLatexDF$`Women Lower Limit`[i] = newLowerLimit                
                
            }
            
    
                
        }
            
    }
    
    # Find the sex differences within blood for each blood parameter
    for(i in firstBloodIndex:lastBloodIndex){
        
        currentTableIndex = i-firstBloodIndex+1
        
        currentPlotTitle = paste0(bloodMetadataLatexDF$Short[currentTableIndex]," (",bloodMetadataLatexDF$Unit[currentTableIndex],")" )
        
        # Average for men and women
        menMean   =  mean(menOnlyTable[,i], na.rm = TRUE)
        womenMean = mean(womenOnlyTable[,i], na.rm = TRUE)
        menSD     = sd(menOnlyTable[,i], na.rm = TRUE)
        womenSD   = sd(womenOnlyTable[,i], na.rm = TRUE)
        
        bloodMetadataLatexDF$Men_Average[currentTableIndex]   = menMean
        bloodMetadataLatexDF$Women_Average[currentTableIndex] = womenMean
        
        # Get the cutoff values for that blood variable
        #cutOffList      = newList(3)
        #cutOffList[[1]] = as.numeric( c( bloodMetadataLatexDF$`Men Lower Limit`[currentTableIndex] , bloodMetadataLatexDF$`Men Upper Limit`[currentTableIndex]))
        #cutOffList[[2]] = c("Lower", "Upper")
        #cutOffList[[3]] = batchIDsColor        
        
        
        # Do the plot
        # This only compares the different between men and women RAW levels.
        # It doesn't make sense to compare the limit between both at this point and
        # that is done in the next series of plots.
        currentBoxPlot = doCategoricalBoxPlot(completeTable, sexIndex, i,
                                              BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                              colorsVector = COLOR_VECTOR_SEX, plotSubtitle = "",
                                              plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitle)
        
        #currentBoxPlot = doCategoricalBoxPlot(completeTable, sexIndex, i,
        #                                      BIOMARKERS_FOLDER_IMAGES_BLOOD,
        #                                      colorsVector = COLOR_VECTOR_SEX, plotSubtitle = "",
        #                                      cutOffLine   = cutOffList,
        #                                      plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitle)
        
        
                
        # Get the p-value
        currentPValue  = currentBoxPlot[[2]][2,2]
        asteriskPValue = getAsterkisPValue(currentPValue)
        
        # Find the significance
        bloodMetadataLatexDF$Significance[currentTableIndex] = asteriskPValue
        
        # Count how many men and women are not eating properly
        # -- Lower and Upper Limits
        currentMenLowerLimit   = bloodMetadataLatexDF$`Men Lower Limit`[currentTableIndex]
        currentMenUpperLimit   = bloodMetadataLatexDF$`Men Upper Limit`[currentTableIndex]
        currentWomenLowerLimit = bloodMetadataLatexDF$`Women Lower Limit`[currentTableIndex]
        currentWomenUpperLimit = bloodMetadataLatexDF$`WomenUpper Limit`[currentTableIndex]
        # -- Do the counting
        totalMenData     = sum(!is.na(menOnlyTable[,i]))
        totalWomenData   = sum(!is.na(womenOnlyTable[,i]))
        menEatingUnder   = sum(currentMenLowerLimit > menOnlyTable[,i],   na.rm = TRUE)
        menEatingAbove   = sum(currentMenUpperLimit < menOnlyTable[,i],   na.rm = TRUE)
        womenEatingUnder = sum(currentWomenLowerLimit > womenOnlyTable[,i], na.rm = TRUE)
        womenEatingAbove = sum(currentWomenUpperLimit < womenOnlyTable[,i], na.rm = TRUE)
        # -- Write in table
        bloodMetadataLatexDF$Freq_Men_Out[currentTableIndex]   = paste0(round(((menEatingUnder   + menEatingAbove)/totalMenData)    *100,1),"%")
        bloodMetadataLatexDF$Freq_Women_Out[currentTableIndex] = paste0(round(((womenEatingUnder + womenEatingAbove)/totalWomenData)*100,1),"%")
        
        
        # Write everything into the sex summary result table
        sexBloodSummmaryDF[currentTableIndex,2] = currentPValue
        sexBloodSummmaryDF[currentTableIndex,3] = asteriskPValue
        sexBloodSummmaryDF[currentTableIndex,4] = (bloodMetadataLatexDF$Men_Average[currentTableIndex] - bloodMetadataLatexDF$Women_Average[currentTableIndex]) / womenSD
        sexBloodSummmaryDF[currentTableIndex,5] = abs(sexBloodSummmaryDF[currentTableIndex,4])
        
        # Finally, check out the pvalue, and the sex average difference
        # Fill the type accordingly
        
        # If we have significant values
        if(currentPValue < 0.05){
            
            # If men average is greater than women average
            if( menMean > womenMean){
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Men low p-value"
                sexBloodSummmaryDF[currentTableIndex,7] = (-1) * ((menMean / womenMean) -  0) 
                
            }
            # If women average is greater than men average
            else{
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Women low p-value"
                sexBloodSummmaryDF[currentTableIndex,7] = 1 * ((womenMean / menMean) - 0)
            }
            
        }
        else{
            
            # If men average is greater than women average
            if(menMean > womenMean){
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Men"
                sexBloodSummmaryDF[currentTableIndex,7] = (-1) * ((menMean / womenMean) -  0) 
                
                    
            }
            # If women average is greater than men average
            else{
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Women"
                sexBloodSummmaryDF[currentTableIndex,7] = 1 * ((womenMean / menMean) - 0)
            }
            
        }
        
        # What the fuck
        if((menEatingUnder + menEatingAbove) > totalMenData){
            
            print("Variable")
            print(currentPlotTitle)
            print("In blood table")
            print(currentTableIndex)
            print("In real table")
            print(i)
            print("Data")
            print(menOnlyTable[,i])
            print("Limits")
            print(currentLowerLimit)
            print(currentUpperLimit)
         
            mean(completeTable[, 140] , na.rm = TRUE)
            sd(completeTable[, 140] , na.rm = TRUE)
               
        }
    }
    
    # Round the numbers a bit
    bloodMetadataLatexDF$`Men Lower Limit`   = round(bloodMetadataLatexDF$`Men Lower Limit`, 2)
    bloodMetadataLatexDF$`Men Upper Limit`   = round(bloodMetadataLatexDF$`Men Upper Limit`, 2)
    bloodMetadataLatexDF$`Women Lower Limit` = round(bloodMetadataLatexDF$`Women Lower Limit`, 2)
    bloodMetadataLatexDF$`Women Upper Limit` = round(bloodMetadataLatexDF$`Women Upper Limit`, 2)
    bloodMetadataLatexDF$Men_Average         = round(bloodMetadataLatexDF$Men_Average, 2)
    bloodMetadataLatexDF$Women_Average       = round(bloodMetadataLatexDF$Women_Average, 2)
    
    # Change the column names into latex format
    colnames(bloodMetadataLatexDF)[8] = "$\\overline{x}_{men}$"
    colnames(bloodMetadataLatexDF)[9] = "$\\overline{x}_{women}$"
    colnames(bloodMetadataLatexDF)[11] = "${Men}_{out}$"
    colnames(bloodMetadataLatexDF)[12] = "${Women}_{out}$"
    
    # Write final result
    writeTableLATEX(bloodMetadataLatexDF, BIOMARKERS_FOLDER_TABLES_BLOOD, tableCaption = "Summary of all blood variables",
                    overrideTableName = "SummaryBloodVariablesTable", widthProportion = 1)
    
    
    # Finish the plot for the blood levels as well
    
    # Give the factor order
    sexBloodSummmaryDF$Type = factor(sexBloodSummmaryDF$Type, levels = c("Men",  "Men low p-value",
                                                                         "Women","Women low p-value"))  
    
    
    # Do the plot
    filePath = doLongBarAbsoluteCombinePlot(sexBloodSummmaryDF, 1, 7, 6, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                            colorsVector = rev(c(COLOR_MAN_LOW,  COLOR_MAN,
                                                                 COLOR_WOMAN_LOW,COLOR_WOMAN)),
                                            plotTitle    = "Ratio of averages for each blood levels with respect sex",
                                            plotYLabel   = "Greater average / lower average", 
                                            sort = "none", imageHeight = 20, imageWidth = 10)
        
    writeImageLATEX2(filePath[[2]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Overview of all blood diferences with respect sex. In many cases there is a significant difference between men and women (p<0.05), 
                                      due biological reasons. Ratio is calculated by dividing the greater average between the lowest average. Negative and positive values
                                      are arbitrary and merelly to separate men to the left and women to the right.",
                     overrideLabel = "fig:BloodBySexDifference",
                     pageHeight = 0.3, overrideFloat = TRUE)
    
    
    # Write the HTML tables and images
    {
        
        # Blood metadatada
        currentTableTitle    = "Blood Metadata"
        currentTableSubtitle = "This table shows, from left to right, the blood serum
                                variable long name, short name, unit in which is measure,
                                healthy values intervals (minimum for men, maximum for men
                                minimum for women, maximum for women, average for men and
                                women, if there is a significant difference between men and women,
                                and how many men and women (percentage of the total) are
                                outside the healthy limit."
        
        bloodMetadataHTMLDF = bloodMetadataLatexDF
        colnames(bloodMetadataHTMLDF) = c("Description",     "Short",             "Unit",              "Men Lower Limit",
                                          "Men Upper Limit", "Women Lower Limit", "Women Upper Limit",
                                          "x̄ Men","x̄ Women",  "Significance",      "Men % Out",
                                          "Women % Out")
        
        tableHTMLString = generateHTMLSimpleTable( bloodMetadataHTMLDF,
                                                   tableTitle = currentTableTitle,
                                                   tableSubtitle = currentTableSubtitle, 
                                                   overrideTableName = "Blood_MetaSummary")
        
        HTMLReportString = replaceEmptyResults(HTMLReportString, tableHTMLString, leaveOpen = TRUE)
        
        # Ratio of averages barplot
        
        currentImageTitle    = "Ratio of averages for men and women"
        currentImageSubtitle = "This image shows the differences in blood serum levels in
                                between men and women. Each bar shows the ratio between
                                the greatest average between the lowest average. Bars to
                                the left shows ratios in which men have higher average,
                                while bars to the rights shows ratios in which women have
                                gretest average. If there is a statistically significant 
                                difference (p-value &lt 0.05) the bar is collored with a 
                                higher saturated color. Sex hormones, particullary
                                testosterone shows the greatest ratio differences."
        
        imageHTMLString = generateHTMLSimpleImage( filePath[[2]], 
                                                   imageTitle = currentImageTitle,
                                                   imageSubtitle = currentImageSubtitle )
        
        HTMLReportString = replaceEmptyResults(HTMLReportString, imageHTMLString, leaveOpen = TRUE)
            
    }
    
    
}

#---------------------------------------------------------------------------
# Do figures for differences between men and women for blood
#
# -- Relative levels between 0 to 1 for both men and women
#---------------------------------------------------------------------------
{
    
    # We are going to prepare the table with the relative values.
    relativeBloodValues           = DF((TOTAL_PEOPLE*totalBloodColumns), 3)
    colnames(relativeBloodValues) = c("RelativeValue", "Sex", "BloodConcept")
    
    currentCounter = 1
    
    # For each person, and for each blood concept, start filling the table
    # with the appropiate relative value
    for(i in 1:TOTAL_PEOPLE){
        
        for(j in 1:totalBloodColumns){

            # Get the sex
            currentSex       = "Man"
            if(as.character(completeTable[i, sexIndex]) == "Woman") currentSex = "Woman"  # R is a stupid language and I HATE the levels class. GET ME A STRING ALWAYS FOR FUCK SAKE >:(

            # Get the blood concept
            currentBloodConcent = bloodMetadataLatexDF$Short[j]
            
            # Get the real value
            currentRealValue     = completeTable[i,j+firstBloodIndex-1]
            currentRelativeValue = NA
            currentLowerValue    = 0
            currentUpperValue    = 9999
            
            # If we have an actual value, find the relative of it
            if(!is.na(currentRealValue)){
                
                # Transform it to relative levels
                # -- Get the values for men and women
                if(currentSex == "Man"){
                
                    currentUpperValue = bloodMetadataLatexDF$`Men Upper Limit`[j]
                    currentLowerValue = bloodMetadataLatexDF$`Men Lower Limit`[j]
                
                }
                else{
                
                    currentUpperValue = bloodMetadataLatexDF$`Women Upper Limit`[j]
                    currentLowerValue = bloodMetadataLatexDF$`Women Lower Limit`[j]
                
                }
            
                interValueRange = (currentUpperValue - currentLowerValue)
            
                # The real value is in between and the person is healthy
                if(currentRealValue>currentLowerValue && currentRealValue<currentUpperValue){
                    
                    currentRelativeValue = (currentRealValue - currentLowerValue) / interValueRange
                    
                }
                # The real value is lower, and the person should eat more
                if(currentRealValue<currentLowerValue){
                
                    realDistance         = currentLowerValue - currentRealValue
                    relativeDistance     = realDistance / interValueRange
                    currentRelativeValue = 0 - relativeDistance
                
                }
                # The real value is greater, and the person should eat less
                if(currentRealValue>currentUpperValue){
                
                    realDistance         = currentRealValue - currentUpperValue
                    relativeDistance     = realDistance / interValueRange
                    currentRelativeValue = 1 + relativeDistance    
                
                }
                
            }
            
            # Otherwise, keep the NA and go to the next one
            relativeBloodValues[currentCounter,1] = currentRelativeValue             
            relativeBloodValues[currentCounter,2] = currentSex
            relativeBloodValues[currentCounter,3] = currentBloodConcent
            
            currentCounter = currentCounter + 1 
            
        }
        
    }
    
    # Give levels so we keep consistency in the plots
    relativeBloodValues$BloodConcept = factor(relativeBloodValues$BloodConcept, levels = c(bloodMetadataLatexDF$Short))
    
    # From here, the table is finish, now we need to make the plot for both
    
    # However, we are going to break the image into three parts.
    # -- FA
    # -- wFA
    # -- Everything else
    relativeBloodValues_A = relativeBloodValues
    relativeBloodValues_B = relativeBloodValues
    relativeBloodValues_C = relativeBloodValues
    
    A_names = bloodMetadataLatexDF$Short[1:28]
    B_names = bloodMetadataLatexDF$Short[29:52]
    C_names = bloodMetadataLatexDF$Short[53:nrow(bloodMetadataLatexDF)]
    
    relativeBloodValues_A              = relativeBloodValues_A[relativeBloodValues_A$BloodConcept %in% A_names,]
    relativeBloodValues_A$BloodConcept = factor(relativeBloodValues_A$BloodConcept, levels = c(A_names))
    relativeBloodValues_B              = relativeBloodValues_B[relativeBloodValues_B$BloodConcept %in% B_names,]
    relativeBloodValues_B$BloodConcept = factor(relativeBloodValues_B$BloodConcept, levels = c(B_names))    
    relativeBloodValues_C              = relativeBloodValues_C[relativeBloodValues_C$BloodConcept %in% C_names,]
    relativeBloodValues_C$BloodConcept = factor(relativeBloodValues_C$BloodConcept, levels = c(C_names))    

    # men and women
    # -- Make the titles
    currentPlotTitle    = "Relative Blood levels for men and women"
    currentPlotTitleA   = "Relative Blood levels for men and women A"
    currentPlotTitleB   = "Relative Blood levels for men and women B"
    currentPlotTitleC   = "Relative Blood levels for men and women C"
    currentPlotSubtitle = "Outliers are hidden to avoid visual cluttering"
    # -- Add the 0/1 lines
    cutOffList      = newList(3)
    cutOffList[[1]] = as.numeric(c(0,1))
    cutOffList[[2]] = c("Lower", "Upper")
    cutOffList[[3]] = batchIDsColor
    
    plotA = doBiCategoricalBoxPlot(relativeBloodValues_A, 3, 1, 2, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                   colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                                   plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitleA,
                                   plotSubtitle = currentPlotSubtitle,
                                   cutOffLine = cutOffList, outlierShape = NA,
                                   overrideImageWidth = 16, longPlot = TRUE,
                                   ymin = -0.2, ymax = 1.2 )
    
    plotB = doBiCategoricalBoxPlot(relativeBloodValues_B, 3, 1, 2, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                           colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                           plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitleB,
                           plotSubtitle = currentPlotSubtitle,
                           cutOffLine = cutOffList, outlierShape = NA,
                           overrideImageWidth = 16, longPlot = TRUE,
                            ymin = -0.2, ymax = 1.2 )    
    
    plotC = doBiCategoricalBoxPlot(relativeBloodValues_C, 3, 1, 2, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                           colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                           plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitleC,
                           plotSubtitle = currentPlotSubtitle,
                           cutOffLine = cutOffList, outlierShape = NA,
                           overrideImageWidth = 16, longPlot = TRUE,
                            ymin = -0.2, ymax = 1.2 )    
    

    writeImageLATEX2(plotA[[4]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Relative Blood levels with respect healthy upper and lower bounds, for men and women; including variables that are not Fatty Acids.",
                     overrideLabel = "fig:RelativeBloodLevelsA",
                     pageHeight = 0.7, overrideFloat = TRUE)
    
    writeImageLATEX2(plotB[[4]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Relative Blood levels with respect healthy upper and lower bounds, for men and women; including absolute levels of Fatty Acids.",
                     overrideLabel = "fig:RelativeBloodLevelsB",
                     pageHeight = 0.7, overrideFloat = TRUE)
    
    writeImageLATEX2(plotC[[4]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Relative Blood levels with respect healthy upper and lower bounds, for men and women; including relative levels of Fatty Acids.",
                     overrideLabel = "fig:RelativeBloodLevelsC",
                     pageHeight = 0.7, overrideFloat = TRUE)    
    
    
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
                
        
        currentBoxPlot = doCategoricalBoxPlot(completeTable, sexIndex, (fistAntropometryIndex + i - 1),
                                              BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
                                              colorsVector = COLOR_VECTOR_SEX, plotSubtitle = "",
                                              plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitle)
        
        myListOfPlotsObjects[[i]] = currentBoxPlot[[1]]
        
        # Fill the table
        antropometricSummaryDF[i,1] = colnames(completeTable)[(fistAntropometryIndex + i - 1)]
        antropometricSummaryDF[i,2] = round(mean(menOnlyTable[,(fistAntropometryIndex + i - 1)],   na.rm = TRUE),1)
        antropometricSummaryDF[i,3] = round(mean(womenOnlyTable[,(fistAntropometryIndex + i - 1)], na.rm = TRUE),1)
        antropometricSummaryDF[i,4] = round(sd(menOnlyTable[,(fistAntropometryIndex + i - 1)],     na.rm = TRUE),1)
        antropometricSummaryDF[i,5] = round(sd(womenOnlyTable[,(fistAntropometryIndex + i - 1)],   na.rm = TRUE),1)
        antropometricSummaryDF[i,6] = getAsterkisPValue(currentBoxPlot[[2]][2,2])

    }

    # Make the grid image
    totalGridColumns = 2
    totalGridRows    = ceiling(totalAntropometries/totalGridColumns)

    # Save as PNG
    ALL_ANTROPOMETRY_FILEPATH = file.path(paste(BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY, "BloodGrid.png", sep = ""))
    ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows,
              common.legend = TRUE, legend = "bottom")
    ggsave(ALL_ANTROPOMETRY_FILEPATH, width = 10, height = 16)
 
    writeImageLATEX2(ALL_ANTROPOMETRY_FILEPATH, "../../../../results/biomarkers/images/antropometry/", 
                     captionText   = "Overview of all antropometric variables diferences with respect sex.",
                     overrideLabel = "fig:BloodBySexDifference",
                     pageWidth = 1, overrideFloat = TRUE)
     
    
    # Save the table
    writeTableLATEX(antropometricSummaryDF, BIOMARKERS_FOLDER_TABLES, tableCaption = "Sex differences for antropometry variables",
                    overrideTableName = "SexDifferencesAntropometryTable", widthProportion = 0, heightProportion = 0.1)
    
}


#---------------------------------------------------------------------------
# Do the basic blood analysis
#
# -- This generate the blood summary table, as well as the differences
#    for each blood level between men and women
#
# -- Figure that is very similar to the biomarker one, but for blood
#    instead, and show the absolute differences between men and women
#---------------------------------------------------------------------------
{

    # Copy the original metadata
    bloodMetadataLatexDF = bloodMetadataDF
    # Delete the ID and all event data from the table, not interested
    bloodMetadataLatexDF = bloodMetadataLatexDF[-c(1:17),]
    # Delete the original name, not interesting in the article
    bloodMetadataLatexDF$Original = NULL
    # Rename the final columns
    colnames(bloodMetadataLatexDF)[4] = "Men Lower Limit"
    colnames(bloodMetadataLatexDF)[5] = "Men Upper Limit"
    colnames(bloodMetadataLatexDF)[6] = "Women Lower Limit"
    colnames(bloodMetadataLatexDF)[7] = "Women Upper Limit"
    # Add men, women and significance
    bloodMetadataLatexDF$Men_Average     = 0
    bloodMetadataLatexDF$Women_Average   = 0
    bloodMetadataLatexDF$Significance    = 0
    bloodMetadataLatexDF$Freq_Men_Out    = 0
    bloodMetadataLatexDF$Freq_Women_Out  = 0
    
    # Prepare the DF for the blood differences by sex
    sexBloodSummmaryDF            = DF(totalBloodColumns,7)
    colnames(sexBloodSummmaryDF)  = c("BloodLevel", "PValue", "Significance","DeltaSigma","AbsSigma","Type", "AvgPro")
    sexBloodSummmaryDF$BloodLevel = bloodMetadataLatexDF$Short
    
    # Trick the upper and lower limit
    # We don't have the information for this yet, so just put whatever there
    # base in the SD. Everybody +-2SD away are bad for example
    {
    
        for(i in 1:nrow(bloodMetadataLatexDF)){

            # R, and RStudio in particular, are a a POS eviroments for debugging
            # It doesn't tell you almost never the line where the error is located,
            # and in RStudio 4.x now it has an option by default that send you
            # to a weird browse() function, in the lowest level functions, every
            # time that there is an error. I completely, and utterly, hate this
            # language and I hope one day it dies in a fire.
            
            if(bloodMetadataLatexDF$`Men Upper Limit`[i] == 9999){
                
                currentAverage     = mean(completeTable[, (firstBloodIndex + i - 1)] , na.rm = TRUE)
                currentSTdeviation = sd(completeTable[, (firstBloodIndex + i - 1)] , na.rm = TRUE)
                
                newUpperLimit = currentAverage + 2*currentSTdeviation
                newLowerLimit = currentAverage - 2*currentSTdeviation
                
                bloodMetadataLatexDF$`Men Upper Limit`[i]   = newUpperLimit
                bloodMetadataLatexDF$`Men Lower Limit`[i]   = newLowerLimit
                bloodMetadataLatexDF$`Women Upper Limit`[i] = newUpperLimit
                bloodMetadataLatexDF$`Women Lower Limit`[i] = newLowerLimit                
                
            }
            
    
                
        }
            
    }
    
    # Find the sex differences within blood for each blood parameter
    for(i in firstBloodIndex:lastBloodIndex){
        
        currentTableIndex = i-firstBloodIndex+1
        
        currentPlotTitle = paste0(bloodMetadataLatexDF$Short[currentTableIndex]," (",bloodMetadataLatexDF$Unit[currentTableIndex],")" )
        
        # Average for men and women
        menMean   =  mean(menOnlyTable[,i], na.rm = TRUE)
        womenMean = mean(womenOnlyTable[,i], na.rm = TRUE)
        menSD     = sd(menOnlyTable[,i], na.rm = TRUE)
        womenSD   = sd(womenOnlyTable[,i], na.rm = TRUE)
        
        bloodMetadataLatexDF$Men_Average[currentTableIndex]   = menMean
        bloodMetadataLatexDF$Women_Average[currentTableIndex] = womenMean
        
        # Get the cutoff values for that blood variable
        #cutOffList      = newList(3)
        #cutOffList[[1]] = as.numeric( c( bloodMetadataLatexDF$`Men Lower Limit`[currentTableIndex] , bloodMetadataLatexDF$`Men Upper Limit`[currentTableIndex]))
        #cutOffList[[2]] = c("Lower", "Upper")
        #cutOffList[[3]] = batchIDsColor        
        
        
        # Do the plot
        # This only compares the different between men and women RAW levels.
        # It doesn't make sense to compare the limit between both at this point and
        # that is done in the next series of plots.
        currentBoxPlot = doCategoricalBoxPlot(completeTable, sexIndex, i,
                                              BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                              colorsVector = COLOR_VECTOR_SEX, plotSubtitle = "",
                                              plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitle)
        
        #currentBoxPlot = doCategoricalBoxPlot(completeTable, sexIndex, i,
        #                                      BIOMARKERS_FOLDER_IMAGES_BLOOD,
        #                                      colorsVector = COLOR_VECTOR_SEX, plotSubtitle = "",
        #                                      cutOffLine   = cutOffList,
        #                                      plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitle)
        
        
                
        # Get the p-value
        currentPValue  = currentBoxPlot[[2]][2,2]
        asteriskPValue = getAsterkisPValue(currentPValue)
        
        # Find the significance
        bloodMetadataLatexDF$Significance[currentTableIndex] = asteriskPValue
        
        # Count how many men and women are not eating properly
        # -- Lower and Upper Limits
        currentMenLowerLimit   = bloodMetadataLatexDF$`Men Lower Limit`[currentTableIndex]
        currentMenUpperLimit   = bloodMetadataLatexDF$`Men Upper Limit`[currentTableIndex]
        currentWomenLowerLimit = bloodMetadataLatexDF$`Women Lower Limit`[currentTableIndex]
        currentWomenUpperLimit = bloodMetadataLatexDF$`WomenUpper Limit`[currentTableIndex]
        # -- Do the counting
        totalMenData     = sum(!is.na(menOnlyTable[,i]))
        totalWomenData   = sum(!is.na(womenOnlyTable[,i]))
        menEatingUnder   = sum(currentMenLowerLimit > menOnlyTable[,i],   na.rm = TRUE)
        menEatingAbove   = sum(currentMenUpperLimit < menOnlyTable[,i],   na.rm = TRUE)
        womenEatingUnder = sum(currentWomenLowerLimit > womenOnlyTable[,i], na.rm = TRUE)
        womenEatingAbove = sum(currentWomenUpperLimit < womenOnlyTable[,i], na.rm = TRUE)
        # -- Write in table
        bloodMetadataLatexDF$Freq_Men_Out[currentTableIndex]   = paste0(round(((menEatingUnder   + menEatingAbove)/totalMenData)    *100,1),"%")
        bloodMetadataLatexDF$Freq_Women_Out[currentTableIndex] = paste0(round(((womenEatingUnder + womenEatingAbove)/totalWomenData)*100,1),"%")
        
        
        # Write everything into the sex summary result table
        sexBloodSummmaryDF[currentTableIndex,2] = currentPValue
        sexBloodSummmaryDF[currentTableIndex,3] = asteriskPValue
        sexBloodSummmaryDF[currentTableIndex,4] = (bloodMetadataLatexDF$Men_Average[currentTableIndex] - bloodMetadataLatexDF$Women_Average[currentTableIndex]) / womenSD
        sexBloodSummmaryDF[currentTableIndex,5] = abs(sexBloodSummmaryDF[currentTableIndex,4])
        
        # Finally, check out the pvalue, and the sex average difference
        # Fill the type accordingly
        
        # If we have significant values
        if(currentPValue < 0.05){
            
            # If men average is greater than women average
            if( menMean > womenMean){
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Men low p-value"
                sexBloodSummmaryDF[currentTableIndex,7] = (-1) * ((menMean / womenMean) -  0) 
                
            }
            # If women average is greater than men average
            else{
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Women low p-value"
                sexBloodSummmaryDF[currentTableIndex,7] = 1 * ((womenMean / menMean) - 0)
            }
            
        }
        else{
            
            # If men average is greater than women average
            if(menMean > womenMean){
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Men"
                sexBloodSummmaryDF[currentTableIndex,7] = (-1) * ((menMean / womenMean) -  0) 
                
                    
            }
            # If women average is greater than men average
            else{
                
                sexBloodSummmaryDF[currentTableIndex,6] = "Women"
                sexBloodSummmaryDF[currentTableIndex,7] = 1 * ((womenMean / menMean) - 0)
            }
            
        }
        
        # What the fuck
        if((menEatingUnder + menEatingAbove) > totalMenData){
            
            print("Variable")
            print(currentPlotTitle)
            print("In blood table")
            print(currentTableIndex)
            print("In real table")
            print(i)
            print("Data")
            print(menOnlyTable[,i])
            print("Limits")
            print(currentLowerLimit)
            print(currentUpperLimit)
         
            mean(completeTable[, 140] , na.rm = TRUE)
            sd(completeTable[, 140] , na.rm = TRUE)
               
        }
    }
    
    # Round the numbers a bit
    bloodMetadataLatexDF$`Men Lower Limit`   = round(bloodMetadataLatexDF$`Men Lower Limit`, 2)
    bloodMetadataLatexDF$`Men Upper Limit`   = round(bloodMetadataLatexDF$`Men Upper Limit`, 2)
    bloodMetadataLatexDF$`Women Lower Limit` = round(bloodMetadataLatexDF$`Women Lower Limit`, 2)
    bloodMetadataLatexDF$`Women Upper Limit` = round(bloodMetadataLatexDF$`Women Upper Limit`, 2)
    bloodMetadataLatexDF$Men_Average         = round(bloodMetadataLatexDF$Men_Average, 2)
    bloodMetadataLatexDF$Women_Average       = round(bloodMetadataLatexDF$Women_Average, 2)
    
    # Change the column names into latex format
    colnames(bloodMetadataLatexDF)[8] = "$\\overline{x}_{men}$"
    colnames(bloodMetadataLatexDF)[9] = "$\\overline{x}_{women}$"
    colnames(bloodMetadataLatexDF)[11] = "${Men}_{out}$"
    colnames(bloodMetadataLatexDF)[12] = "${Women}_{out}$"
    
    # Write final result
    writeTableLATEX(bloodMetadataLatexDF, BIOMARKERS_FOLDER_TABLES_BLOOD, tableCaption = "Summary of all blood variables",
                    overrideTableName = "SummaryBloodVariablesTable", widthProportion = 1)
    
    
    # Finish the plot for the blood levels as well
    
    # Give the factor order
    sexBloodSummmaryDF$Type = factor(sexBloodSummmaryDF$Type, levels = c("Men",  "Men low p-value",
                                                                         "Women","Women low p-value"))  
    
    
    # Do the plot
    filePath = doLongBarAbsoluteCombinePlot(sexBloodSummmaryDF, 1, 7, 6, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                            colorsVector = rev(c(COLOR_MAN_LOW,  COLOR_MAN,
                                                                 COLOR_WOMAN_LOW,COLOR_WOMAN)),
                                            plotTitle    = "Ratio of averages for each blood levels with respect sex",
                                            plotYLabel   = "Greater average / lower average", 
                                            sort = "none", imageHeight = 20, imageWidth = 10)
        
    writeImageLATEX2(filePath[[2]], "../../../../results/biomarkers/images/blood/", 
                        captionText   = "Overview of all blood diferences with respect sex. In many cases there is a significant difference between men and women (p<0.05), 
                                         due biological reasons. Ratio is calculated by dividing the greater average between the lowest average. Negative and positive values
                                         are arbitrary and merelly to separate men to the left and women to the right.",
                        overrideLabel = "fig:BloodBySexDifference",
                        pageHeight = 0.3, overrideFloat = TRUE)
    
    
    
}

#---------------------------------------------------------------------------
# Do figures for differences between men and women for blood
#
# -- Relative levels between 0 to 1 for both men and women
#---------------------------------------------------------------------------
{
    
    # We are going to prepare the table with the relative values.
    relativeBloodValues           = DF((TOTAL_PEOPLE*totalBloodColumns), 3)
    colnames(relativeBloodValues) = c("RelativeValue", "Sex", "BloodConcept")
    
    currentCounter = 1
    
    # For each person, and for each blood concept, start filling the table
    # with the appropiate relative value
    for(i in 1:TOTAL_PEOPLE){
        
        for(j in 1:totalBloodColumns){

            # Get the sex
            currentSex       = "Man"
            if(as.character(completeTable[i, sexIndex]) == "Woman") currentSex = "Woman"  # R is a stupid language and I HATE the levels class. GET ME A STRING ALWAYS FOR FUCK SAKE >:(

            # Get the blood concept
            currentBloodConcent = bloodMetadataLatexDF$Short[j]
            
            # Get the real value
            currentRealValue     = completeTable[i,j+firstBloodIndex-1]
            currentRelativeValue = NA
            currentLowerValue    = 0
            currentUpperValue    = 9999
            
            # If we have an actual value, find the relative of it
            if(!is.na(currentRealValue)){
                
                # Transform it to relative levels
                # -- Get the values for men and women
                if(currentSex == "Man"){
                
                    currentUpperValue = bloodMetadataLatexDF$`Men Upper Limit`[j]
                    currentLowerValue = bloodMetadataLatexDF$`Men Lower Limit`[j]
                
                }
                else{
                
                    currentUpperValue = bloodMetadataLatexDF$`Women Upper Limit`[j]
                    currentLowerValue = bloodMetadataLatexDF$`Women Lower Limit`[j]
                
                }
            
                interValueRange = (currentUpperValue - currentLowerValue)
            
                # The real value is in between and the person is healthy
                if(currentRealValue>currentLowerValue && currentRealValue<currentUpperValue){
                    
                    currentRelativeValue = (currentRealValue - currentLowerValue) / interValueRange
                    
                }
                # The real value is lower, and the person should eat more
                if(currentRealValue<currentLowerValue){
                
                    realDistance         = currentLowerValue - currentRealValue
                    relativeDistance     = realDistance / interValueRange
                    currentRelativeValue = 0 - relativeDistance
                
                }
                # The real value is greater, and the person should eat less
                if(currentRealValue>currentUpperValue){
                
                    realDistance         = currentRealValue - currentUpperValue
                    relativeDistance     = realDistance / interValueRange
                    currentRelativeValue = 1 + relativeDistance    
                
                }
                
            }
            
            # Otherwise, keep the NA and go to the next one
            relativeBloodValues[currentCounter,1] = currentRelativeValue             
            relativeBloodValues[currentCounter,2] = currentSex
            relativeBloodValues[currentCounter,3] = currentBloodConcent
            
            currentCounter = currentCounter + 1 
            
        }
        
    }
    
    # Give levels so we keep consistency in the plots
    relativeBloodValues$BloodConcept = factor(relativeBloodValues$BloodConcept, levels = c(bloodMetadataLatexDF$Short))
    
    # From here, the table is finish, now we need to make the plot for both
    
    # However, we are going to break the image into three parts.
    # -- FA
    # -- wFA
    # -- Everything else
    relativeBloodValues_A = relativeBloodValues
    relativeBloodValues_B = relativeBloodValues
    relativeBloodValues_C = relativeBloodValues
    
    A_names = bloodMetadataLatexDF$Short[1:28]
    B_names = bloodMetadataLatexDF$Short[29:52]
    C_names = bloodMetadataLatexDF$Short[53:nrow(bloodMetadataLatexDF)]
    
    relativeBloodValues_A              = relativeBloodValues_A[relativeBloodValues_A$BloodConcept %in% A_names,]
    relativeBloodValues_A$BloodConcept = factor(relativeBloodValues_A$BloodConcept, levels = c(A_names))
    relativeBloodValues_B              = relativeBloodValues_B[relativeBloodValues_B$BloodConcept %in% B_names,]
    relativeBloodValues_B$BloodConcept = factor(relativeBloodValues_B$BloodConcept, levels = c(B_names))    
    relativeBloodValues_C              = relativeBloodValues_C[relativeBloodValues_C$BloodConcept %in% C_names,]
    relativeBloodValues_C$BloodConcept = factor(relativeBloodValues_C$BloodConcept, levels = c(C_names))    

    # men and women
    # -- Make the titles
    currentPlotTitle    = "Relative Blood levels for men and women"
    currentPlotTitleA   = "Relative Blood levels for men and women A"
    currentPlotTitleB   = "Relative Blood levels for men and women B"
    currentPlotTitleC   = "Relative Blood levels for men and women C"
    currentPlotSubtitle = "Outliers are hidden to avoid visual cluttering"
    # -- Add the 0/1 lines
    cutOffList      = newList(3)
    cutOffList[[1]] = as.numeric(c(0,1))
    cutOffList[[2]] = c("Lower", "Upper")
    cutOffList[[3]] = batchIDsColor
    
    plotA = doBiCategoricalBoxPlot(relativeBloodValues_A, 3, 1, 2, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                                   colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                                   plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitleA,
                                   plotSubtitle = currentPlotSubtitle,
                                   cutOffLine = cutOffList, outlierShape = NA,
                                   overrideImageWidth = 16, longPlot = TRUE,
                                   ymin = -0.2, ymax = 1.2 )
    
    plotB = doBiCategoricalBoxPlot(relativeBloodValues_B, 3, 1, 2, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                           colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                           plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitleB,
                           plotSubtitle = currentPlotSubtitle,
                           cutOffLine = cutOffList, outlierShape = NA,
                           overrideImageWidth = 16, longPlot = TRUE,
                            ymin = -0.2, ymax = 1.2 )    
    
    plotC = doBiCategoricalBoxPlot(relativeBloodValues_C, 3, 1, 2, BIOMARKERS_FOLDER_IMAGES_BLOOD,
                           colorsVector = COLOR_VECTOR_SEX, showPValues = FALSE,
                           plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitleC,
                           plotSubtitle = currentPlotSubtitle,
                           cutOffLine = cutOffList, outlierShape = NA,
                           overrideImageWidth = 16, longPlot = TRUE,
                            ymin = -0.2, ymax = 1.2 )    
    

    writeImageLATEX2(plotA[[4]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Relative Blood levels with respect healthy upper and lower bounds, for men and women; including variables that are not Fatty Acids.",
                     overrideLabel = "fig:RelativeBloodLevelsA",
                     pageHeight = 0.7, overrideFloat = TRUE)
    
    writeImageLATEX2(plotB[[4]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Relative Blood levels with respect healthy upper and lower bounds, for men and women; including absolute levels of Fatty Acids.",
                     overrideLabel = "fig:RelativeBloodLevelsB",
                     pageHeight = 0.7, overrideFloat = TRUE)
    
    writeImageLATEX2(plotC[[4]], "../../../../results/biomarkers/images/blood/", 
                     captionText   = "Relative Blood levels with respect healthy upper and lower bounds, for men and women; including relative levels of Fatty Acids.",
                     overrideLabel = "fig:RelativeBloodLevelsC",
                     pageHeight = 0.7, overrideFloat = TRUE)    
    
    
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
                
        
        currentBoxPlot = doCategoricalBoxPlot(completeTable, sexIndex, (fistAntropometryIndex + i - 1),
                                              BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
                                              colorsVector = COLOR_VECTOR_SEX, plotSubtitle = "",
                                              plotXLabel = "", plotYLabel = "", plotTitle = currentPlotTitle)
        
        myListOfPlotsObjects[[i]] = currentBoxPlot[[1]]
        
        # Fill the table
        antropometricSummaryDF[i,1] = colnames(completeTable)[(fistAntropometryIndex + i - 1)]
        antropometricSummaryDF[i,2] = round(mean(menOnlyTable[,(fistAntropometryIndex + i - 1)],   na.rm = TRUE),1)
        antropometricSummaryDF[i,3] = round(mean(womenOnlyTable[,(fistAntropometryIndex + i - 1)], na.rm = TRUE),1)
        antropometricSummaryDF[i,4] = round(sd(menOnlyTable[,(fistAntropometryIndex + i - 1)],     na.rm = TRUE),1)
        antropometricSummaryDF[i,5] = round(sd(womenOnlyTable[,(fistAntropometryIndex + i - 1)],   na.rm = TRUE),1)
        antropometricSummaryDF[i,6] = getAsterkisPValue(currentBoxPlot[[2]][2,2])

    }

    # Make the grid image
    totalGridColumns = 2
    totalGridRows    = ceiling(totalAntropometries/totalGridColumns)

    # Save as PNG
    ALL_ANTROPOMETRY_FILEPATH = file.path(paste(BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY, "BloodGrid.png", sep = ""))
    ggarrange(plotlist =  myListOfPlotsObjects , ncol = totalGridColumns, nrow = totalGridRows,
              common.legend = TRUE, legend = "bottom")
    ggsave(ALL_ANTROPOMETRY_FILEPATH, width = 10, height = 16)
 
    writeImageLATEX2(ALL_ANTROPOMETRY_FILEPATH, "../../../../results/biomarkers/images/antropometry/", 
                     captionText   = "Overview of all antropometric variables diferences with respect sex.",
                     overrideLabel = "fig:BloodBySexDifference",
                     pageWidth = 1, overrideFloat = TRUE)
     
    
    # Save the table
    writeTableLATEX(antropometricSummaryDF, BIOMARKERS_FOLDER_TABLES, tableCaption = "Sex differences for antropometry variables",
                    overrideTableName = "SexDifferencesAntropometryTable", widthProportion = 0, heightProportion = 0.1)
    
}

#---------------------------------------------------------------------------
# Do biomarkers levels for every numerical (Antropometrics and Blood)
#---------------------------------------------------------------------------
{
    
    # Let start with the antropometry table
    menResultsBloodDF           = DF(TOTAL_BIOMARKERS, (totalAntropometries+1))
    colnames(menResultsBloodDF) = c("Protein",antropometricSummaryDF$Concept)
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
            for(j in 1:totalAntropometries){
                
                currentAntropometricIndex = firstBloodIndex + j - 1
                currentAntropometricName  = antropometricSummaryDF$Concept[j]
                
                # Clean the data from NA data
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentBiomarkerIndex]),]
                currentSubtable = currentSubtable[!is.na(currentSubtable[, currentAntropometricIndex]),]
                
                # Run the model
                # Plot the graph
                currentPlotTitle     = paste0(currentBiomarkerName," for ",currentSex, " and ", currentAntropometricName)
                currentTableOverride = paste0(currentBiomarkerName,"_",    currentSex, "_",     currentAntropometricName)
                
                myAntropomode        = doSimpleRegression(currentSubtable, currentAntropometricIndex, currentBiomarkerIndex, BIOMARKERS_FOLDER_IMAGES_ANTROPOMETRY,
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
    
        for(j in 1:totalAntropometries){
    
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
            
            if(sum(menAsteriskResultsBloodDF[i,] == 'ns')           == 8) deleteTheseSimpleRows[i]     = TRUE
            if(sum(menAsteriskResultsBloodBenjaminiDF[i,]  == 'ns') == 8) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(menAsteriskResultsBloodBonferroniDF[i,] == 'ns') == 8) deleteTheseBonferroniRows[i] = TRUE
            
        }
                
        menAsteriskResultsBloodDF           = menAsteriskResultsBloodDF[!deleteTheseSimpleRows,]
        menAsteriskResultsBloodBenjaminiDF  = menAsteriskResultsBloodBenjaminiDF[!deleteTheseBenjaminiRows,]
        menAsteriskResultsBloodBonferroniDF = menAsteriskResultsBloodBonferroniDF[!deleteTheseBonferroniRows,]
        
        deleteTheseSimpleRows     = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBenjaminiRows  = rep(FALSE,TOTAL_BIOMARKERS)
        deleteTheseBonferroniRows = rep(FALSE,TOTAL_BIOMARKERS)
        for(i in 1:TOTAL_BIOMARKERS){
            
            if(sum(womenAsteriskResultsBloodDF[i,] == 'ns')           == 8) deleteTheseSimpleRows[i]     = TRUE
            if(sum(womenAsteriskResultsBloodBenjaminiDF[i,]  == 'ns') == 8) deleteTheseBenjaminiRows[i]  = TRUE
            if(sum(womenAsteriskResultsBloodBonferroniDF[i,] == 'ns') == 8) deleteTheseBonferroniRows[i] = TRUE
            
        }
                
        womenAsteriskResultsBloodDF           = womenAsteriskResultsBloodDF[!deleteTheseSimpleRows,]
        womenAsteriskResultsBloodBenjaminiDF  = womenAsteriskResultsBloodBenjaminiDF[!deleteTheseBenjaminiRows,]
        womenAsteriskResultsBloodBonferroniDF = womenAsteriskResultsBloodBonferroniDF[!deleteTheseBonferroniRows,]
           
    }
    
    # Write to latex
    writeTableLATEX(menAsteriskResultsBloodBonferroniDF, BIOMARKERS_FOLDER_TABLES_BLOOD,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in men, after applying Bonferroni correction",
                    overrideTableName = "BiomarkersBloodBonferroniMen", heightProportion = 0.2)
    
    writeTableLATEX(womenAsteriskResultsBloodBonferroniDF, BIOMARKERS_FOLDER_TABLES_BLOOD,  tableCaption = "Biomarkers that are statistically significant with respect the antropometry variables in women, after applying Bonferroni correction",
                    overrideTableName = "BiomarkersBloodBonferroniWomen", heightProportion = 0.2)
            
  
    
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

# Write the final HTML file

writeHTMLReport(HTMLReportString, REPORTS_WEB_HTML_FILEPATH, "Biomarkers_Blood.html")
    
    

