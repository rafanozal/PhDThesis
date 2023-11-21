
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/analysis/analysisCategorical.R"),       encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingBoxplots.R"),     encoding="utf-8")
source( paste0(MAIN_PROJECT_FOLDER,"src/lib/plotting/toolsPlottingRegression.R"),   encoding="utf-8")



ANNE_FOLDER = paste0(RESULT_FOLDER,"annePaper/")

# Create the table with the data.
{

    # We don't really need to do this, but I'm creating an independent table
    # to test the self-made functions. Having a small table makes debugging easier
    myData               = DF(nrow(completeTable), 2)
    myData$ID            = completeTable$ID
    myData$Sex           = completeTable$Sex
    
    myData$Smoke         = completeTable$Smoke
    myData$SmokePerDay   = completeTable$SmokePerDay
    myData$SmokePerWeek  = completeTable$SmokePerWeek
    
    myData$Snuff         = completeTable$Snuff
    myData$SnuffPerDay   = completeTable$SnuffPerDay
    myData$SnuffPerWeek  = completeTable$SnuffPerWeek
    
    myData$Alcohol       = completeTable$Alcohol
    myData$AlcoholUnits  = completeTable$AlcoholUnits
    myData$Alcohol6Units = completeTable$Alcohol6Units
    
    myData$HormonalType  = completeTable$HormonalType
    
    # Define the carrier status.
    # In this case, is a positive Throat in both sample 1 and sample 2 (few months later)
    # These two variables is what we want:
    myData$DirectCarrier = completeTable$D_ThroatCarrier
    myData$EnrichCarrier = completeTable$E_ThroatCarrier
    
    # Define the highschool for later
    myData$HighSchool = completeTable$HighSchool
    
    # Drop the extra columns
    myData$X1 = NULL
    myData$X2 = NULL
    
    # Stratify by sex
    menData   = keepCategory(myData, 2, "Man")
    womenData = keepCategory(myData, 2, "Woman")
}

# Skip these results from the analysis
skipModalities     = UNKNOWN_VARIABLES  

# These are the X variables
bothImportantVariables  = c(2:11) # HC not included
menImportantVariables   = c(3:11) # Sex not included, HC not included
womenImportantVariables = c(3:12) # Sex not included, 

# Perform the analysis
bothAnalysisResults   = doOddRiskRatio(myData,    14, bothImportantVariables,  skipModalities)
menAnalysisResults    = doOddRiskRatio(menData,   14, menImportantVariables,   skipModalities)
womenAnalysisResults  = doOddRiskRatio(womenData, 14, womenImportantVariables, skipModalities)

# Write stuff in latex
{
    
    # Marginal tables
    {
    
        marginalTables = bothAnalysisResults[[1]]
        for(i in 1: length(marginalTables) ){
            
            currentCaption = paste0("Marginal tables for Enrich STAPH status and ", colnames(myData)[i+1])
            currentName    = paste0("MarginalEnrich",colnames(myData)[i+1])
            currentTable   = writeTableLATEX(marginalTables[[i]], ANNE_FOLDER,
                                             tableCaption = currentCaption,
                                             overrideTableName = currentName,
                                             widthProportion = 0.8, heightProportion = 0.05)
            
            print(currentTable[[2]])
        }        
        
        marginalTables = menAnalysisResults[[1]]
        for(i in 1: length(marginalTables) ){
            
            currentCaption = paste0("Marginal tables for MEN ONLY with Enrich STAPH status and ", colnames(myData)[menImportantVariables[i]])
            currentName    = paste0("MENMarginalEnrich", colnames(myData)[menImportantVariables[i]])
            currentTable   = writeTableLATEX(marginalTables[[i]], ANNE_FOLDER,
                                             tableCaption = currentCaption,
                                             overrideTableName = currentName,
                                             widthProportion = 0.8, heightProportion = 0.05)
            
            print(currentTable[[2]])
        }
        
        marginalTables = womenAnalysisResults[[1]]
        for(i in 1: length(marginalTables) ){
            
            currentCaption = paste0("Marginal tables for WOMEN ONLY with Enrich STAPH status and ", colnames(myData)[womenImportantVariables[i]])
            currentName    = paste0("WOMENMarginalEnrich", colnames(myData)[womenImportantVariables[i]])
            currentTable   = writeTableLATEX(marginalTables[[i]], ANNE_FOLDER,
                                             tableCaption = currentCaption,
                                             overrideTableName = currentName,
                                             widthProportion = 0.8, heightProportion = 0.05)
            
            print(currentTable[[2]])
        }        
        
    }
    
    # RAW results (RISK)
    {
    
        # Get the raw results
        bothRiskRawTables  = bothAnalysisResults[[2]]
        menRiskRawTables   = menAnalysisResults[[2]]
        womenRiskRawTables = womenAnalysisResults[[2]]
        
        # Turn it into something more readable
        bothRiskHumanTables  = bothRiskRawTables
        menRiskHumanTables   = menRiskRawTables
        womenRiskHumanTables = womenRiskRawTables
        
        for(i in 1:nrow(bothRiskHumanTables)){
            for(j in 1:ncol(bothRiskHumanTables)){
            
                # If it is the variable or modality column
                # ---- Turn NA into ""
                if(j == 1 | j == 2)
                    if(is.na(bothRiskHumanTables[i,j])) bothRiskHumanTables[i,j] = ""
                
                # If it is the estimates or interval
                # ---- Turn NA into ""
                # ---- Round to 3
                if(j == 3 | j == 4 | j == 5)
                    if(is.na(bothRiskHumanTables[i,j])) bothRiskHumanTables[i,j] = ""
                    else                                bothRiskHumanTables[i,j] = round( as.numeric(bothRiskHumanTables[i,j]),2)
                    
                # If it is the p-values
                # ---- Turn NA into ""
                # ---- Make starts
                if(j == 6 | j == 7 | j == 8)
                    if(is.na(bothRiskHumanTables[i,j])) bothRiskHumanTables[i,j] = ""
                    else                                bothRiskHumanTables[i,j] = getAsterkisPValue( as.numeric(bothRiskHumanTables[i,j]) , nsEmpty = TRUE)
            }           
        }
        
        for(i in 1:nrow(menRiskHumanTables)){
            for(j in 1:ncol(menRiskHumanTables)){
            
                # If it is the variable or modality column
                # ---- Turn NA into ""
                if(j == 1 | j == 2)
                    if(is.na(menRiskHumanTables[i,j])) menRiskHumanTables[i,j] = ""
                
                # If it is the estimates or interval
                # ---- Turn NA into ""
                # ---- Round to 3
                if(j == 3 | j == 4 | j == 5)
                    if(is.na(menRiskHumanTables[i,j])) menRiskHumanTables[i,j] = ""
                    else                               menRiskHumanTables[i,j] = round( as.numeric(menRiskHumanTables[i,j]),2)
                    
                # If it is the p-values
                # ---- Turn NA into ""
                # ---- Make starts
                if(j == 6 | j == 7 | j == 8)
                    if(is.na(menRiskHumanTables[i,j])) menRiskHumanTables[i,j] = ""
                    else                               menRiskHumanTables[i,j] = getAsterkisPValue( as.numeric(menRiskHumanTables[i,j]) , nsEmpty = TRUE)
            }           
        }
        
        for(i in 1:nrow(womenRiskHumanTables)){
            for(j in 1:ncol(womenRiskHumanTables)){
            
                # If it is the variable or modality column
                # ---- Turn NA into ""
                if(j == 1 | j == 2)
                    if(is.na(womenRiskHumanTables[i,j])) womenRiskHumanTables[i,j] = ""
                
                # If it is the estimates or interval
                # ---- Turn NA into ""
                # ---- Round to 3
                if(j == 3 | j == 4 | j == 5)
                    if(is.na(womenRiskHumanTables[i,j])) womenRiskHumanTables[i,j] = ""
                    else                                 womenRiskHumanTables[i,j] = round( as.numeric(womenRiskHumanTables[i,j]),2)
                    
                # If it is the p-values
                # ---- Turn NA into ""
                # ---- Make starts
                if(j == 6 | j == 7 | j == 8)
                    if(is.na(womenRiskHumanTables[i,j])) womenRiskHumanTables[i,j] = ""
                    else                                 womenRiskHumanTables[i,j] = getAsterkisPValue( as.numeric(womenRiskHumanTables[i,j]) , nsEmpty = TRUE)
            }           
        }

        currentCaption = paste0("Risk ratio table for Enrich STAPH status and everything")
        currentName    = paste0("RiskEnrichTable")
        currentTable   = writeTableLATEX(bothRiskHumanTables, ANNE_FOLDER,
                                         tableCaption      = currentCaption,
                                         overrideTableName = currentName,
                                         widthProportion   = 0.9, heightProportion = 0.4)
        print(currentTable[[2]])
        
        
        currentCaption = paste0("Risk ratio table for MEN ONLY Enrich STAPH status and everything")
        currentName    = paste0("MenRiskEnrichTable")
        currentTable   = writeTableLATEX(menRiskHumanTables, ANNE_FOLDER,
                                         tableCaption      = currentCaption,
                                         overrideTableName = currentName,
                                         widthProportion   = 0.9, heightProportion = 0.4)
        print(currentTable[[2]])
        
        
        currentCaption = paste0("Risk ratio table for WOMEN ONLY Enrich STAPH status and everything")
        currentName    = paste0("WomenRiskEnrichTable")
        currentTable   = writeTableLATEX(womenRiskHumanTables, ANNE_FOLDER,
                                         tableCaption      = currentCaption,
                                         overrideTableName = currentName,
                                         widthProportion   = 0.9, heightProportion = 0.4)        
        print(currentTable[[2]])
        
    }    

}





# Compare by highschol
# Is there some school that has more STAPH than others?\vspace{3 mm}
xiResults = categoricalXiV2(myData, 15 , 14)
writeTableLATEX(xiResults[[7]], ANNE_FOLDER,
                tableCaption = "STAPH with respect high-school",
                overrideTableName = "anneSTAPHHS",
                widthProportion = 0.8, heightProportion = 0.09) 

# Is there some school that has more drug use than others?\vspace{3 mm}
noUnknownData = deleteCategory(myData, 3, "Unknown")
xiResults = categoricalXiV2(noUnknownData, 15 , 3)
writeTableLATEX(xiResults[[7]], ANNE_FOLDER,
                tableCaption = "Smoke with respect high-school",
                overrideTableName = "anneSmokeHS",
                widthProportion = 0.8, heightProportion = 0.09) 

noUnknownData = deleteCategory(myData, 6, "Unknown")
xiResults = categoricalXiV2(noUnknownData, 15 , 6)
writeTableLATEX(xiResults[[7]], ANNE_FOLDER,
                tableCaption = "Snuff with respect high-school",
                overrideTableName = "anneSnuffHS",
                widthProportion = 0.8, heightProportion = 0.09) 

noUnknownData = deleteCategory(myData, 9, "Unknown")
xiResults = categoricalXiV2(noUnknownData, 15 , 9)
writeTableLATEX(xiResults[[7]], ANNE_FOLDER,
                tableCaption = "Alcohol with respect high-school",
                overrideTableName = "anneAlcoholHS",
                widthProportion = 0.8, heightProportion = 0.09)
        



# Load the library
library(fastDummies)

noUnknownWomenData = deleteCategory(womenData, 6, "Unknown")  
noUnknownWomenData = deleteCategory(noUnknownWomenData, 9, "Unknown")
noUnknownWomenData = deleteCategory(noUnknownWomenData, 12, "Unknown")

dummyData = dummy_cols(noUnknownWomenData,  select_columns = c("Snuff", "Alcohol", "HormonalType"))
dummyData = dummyData[,14:ncol(dummyData)]

# Summarize High and low estradiol as requested
dummyData$`HormonalType_Estradiol` = dummyData$`HormonalType_Low Estradiol` + dummyData$`HormonalType_High Estradiol`
dummyData$`HormonalType_High Estradiol` = NULL
dummyData$`HormonalType_Low Estradiol` = NULL

# Transform response in 1 0
dummyData$Response = 0
dummyData[dummyData[,1] == "Positive",]$Response = 1
dummyData$EnrichCarrier = NULL


# Run multinomial

library(PerformanceAnalytics)


chartFilename = paste0(ANNE_FOLDER, "CorrelationChart.png")
png(chartFilename)
chart.Correlation(dummyData,
                  method="spearman",
                  histogram=TRUE,
                  pch=16)
dev.off()
            
writeImageLATEX2(chartFilename, ANNE_FOLDER, 
                 captionText   = "Relationship between all vice dummy variables and STAPH infection",
                 overrideLabel = "fig:SpearmanChartVice",
                 pageHeight = 0.7, overrideFloat = TRUE)   


library(psych)

corr.test(dummyData,
          use = "pairwise",
          method="spearman",
          adjust="none",      # Can adjust p-values; see ?p.adjust for options
          alpha=.05)


model.null = glm(Response ~ 1,
                 data=dummyData,
                 family = binomial(link="logit")
                 )

# R JODER, LA PUTA MIERDA DE LOS COJONES DE R DE NO USAR LOS PUTOS NOMBRES BIEN

model.full = glm(Response ~ Snuff_Never + Snuff_Sometimes + Snuff_Daily +
                            Alcohol_Never + `Alcohol_Once per month or less` + `Alcohol_Twice of more per month` +
                            `HormonalType_Non-hormonal` + HormonalType_Progestin + `HormonalType_Estradiol`, 
                 data=dummyData,
                 family = binomial(link="logit")
                 )

step(model.null,
     scope = list(upper=model.full),
             direction="both",
             test="Chisq",
             data=Data)

model.final = glm(Response ~ Snuff_Never + `Alcohol_Once per month or less`,
                  data=dummyData,
                  family = binomial(link="logit"),
                  na.action(na.omit)
                  )


summary(model.final)

summary(model.full)



linearModel = lm(Response ~ Snuff_Never + Snuff_Sometimes + Snuff_Daily +
                            Alcohol_Never + `Alcohol_Once per month or less` + `Alcohol_Twice of more per month` +
                            `HormonalType_Non-hormonal` + HormonalType_Progestin + `HormonalType_Estradiol`, 
                 data=dummyData
                 )


summary(linearModel)