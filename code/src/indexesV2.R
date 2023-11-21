# MAKE SURE THAT YOU HAVE LOADED THE DATA !!!!
# This code goes inside the loadDataV2.R
# so there is no source() in here.


# We have divided the data into FF1, FF2, and FF3.
# Each is created in dataCleaning.R and added to
#
# completeData[]
#
# Which is a list of matrices that contain the same
# structure, but with different numbers.
# 
# Refering to temporal variables is done via 1,2,3
# index in that list, while the topic index remain
# the same for all tables.



# Individual indexes and where they come from
{

    # -- Basic Table
    {
        IDIndex          = grep("^ID$",               colnames(completeTable))
        sexIndex         = grep("^Sex$",              colnames(completeTable))
        healthIndex      = grep("^GeneralHealth$",    colnames(completeTable))
        ageIndex         = grep("^Age$",              colnames(completeTable))

    }
    
    # -- Antropometry
    {
        BMIIndex         = grep("^BMI$",              colnames(completeTable))
        BMICatIndex      = grep("^BMICategorical$",   colnames(completeTable))
        
        fistAntropometryIndex = grep("^Waist$",       colnames(completeTable))
        lastAntropometryIndex = BMICatIndex
    }
    
    # -- Drug table
    {
        smokeIndex       = grep("^Smoke$",            colnames(completeTable))
        snuffIndex       = grep("^Snuff$",            colnames(completeTable))
        alcoholIndex     = grep("^Alcohol$",          colnames(completeTable))
    }
    
    # -- Sport table
    {
        sportsIndex          = grep("^SportsLeisure$",   colnames(completeTable))
        transportSummerIndex = grep("^SummerTransport$", colnames(completeTable))
        transportWinterIndex = grep("^WinterTransport$", colnames(completeTable))
        screenIndex          = grep("^ScreenTime$",      colnames(completeTable))
    }

    # -- Highschool table
    {
        highSchoolIndex  = grep("^HighSchool$",     colnames(completeTable))
        hsProgrameIndex  = grep("^MainPrograme$",   colnames(completeTable))
    }
    
    # -- Aureus table
    {

        # Direct culture carrier
        nasalDirectCarrierIndex      = grep("^D_NasalCarrier$",         colnames(completeTable))
        throatDirectCarrierIndex     = grep("^D_ThroatCarrier$",        colnames(completeTable))
        carrierDirectIndex           = grep("^D_Carrier$",              colnames(completeTable))
        # Enrichment carrier
        nasalEnrichmentCarrierIndex  = grep("^E_NasalCarrier$",         colnames(completeTable))
        throatEnrichmentCarrierIndex = grep("^E_ThroatCarrier$",        colnames(completeTable))
        carrierEnrichmentIndex       = grep("^E_Carrier$",              colnames(completeTable))
        # SPA types indexes
        spaTClonningIndexComplete    = grep("^SPAThroatClonning$",      colnames(completeTable))
        spaT1IndexComplete           = grep("^SPAThroat1$",             colnames(completeTable))
        spaT2IndexComplete           = grep("^SPAThroat2$",             colnames(completeTable))
        spaN1IndexComplete           = grep("^SPANasal1$",              colnames(completeTable))
        spaN2IndexComplete           = grep("^SPANasal2$",              colnames(completeTable))

    }
    
    # -- Dates
    {
        
        attendanceFF1Index         = grep("^AttendanceDateFF1$",    colnames(completeTable))
        attendanceFF12Index        = grep("^AttendanceDateFF12$",   colnames(completeTable))
        networkCreationIndex       = grep("^Created$",              colnames(completeTable))
        swab1AttendanceDateIndex   = grep("^S1_AttendanceDate$",    colnames(completeTable))
        swab2AttendanceDateIndex   = grep("^S2_AttendanceDate$",    colnames(completeTable))
        swab1CultureDateIndex      = grep("^S1_CultureDate$",       colnames(completeTable))
        #swab2CultureDateIndex     = grep("^S2_CultureDate$",       colnames(completeTable)) This variable doesn't exist yet
        swab1NasalFreezeDateIndex  = grep("^S1_Nasal_FreezeDate$",  colnames(completeTable))
        swab1ThroatFreezeDateIndex = grep("^S1_Throat_FreezeDate$", colnames(completeTable))
        bloodExtractionDateIndex   = grep("^BloodAnalysisDate$",    colnames(completeTable))
        plasmaExtractionDateIndex  = grep("^PlasmaAnalysisDate$",   colnames(completeTable))

    }

    # -- Network indexes
    {

        # How well representative is your network of friends
        overviewIndex            = grep("^Overview$",                    colnames(completeTable))

        # Each of the popularity statistics for each network
        overallConnectionsIndex  = grep("^OverallConnections$",          colnames(completeTable))
        overallPopularityIndex   = grep("^OverallPopularity$" ,          colnames(completeTable))

        physicalConnectionsIndex = grep("^PhysicalConnections$",         colnames(completeTable))
        physicalPopularityIndex  = grep("^PhysicalPopularity$" ,         colnames(completeTable))

        homeConnectionsIndex     = grep("^HomeConnections$",             colnames(completeTable))
        homePopularityIndex      = grep("^HomePopularity$" ,             colnames(completeTable))

        sportsConnectionsIndex   = grep("^SportsConnections$",           colnames(completeTable))
        sportsPopularityIndex    = grep("^SportsPopularity$" ,           colnames(completeTable))

        schoolConnectionsIndex   = grep("^SchoolConnections$",           colnames(completeTable))
        schoolPopularityIndex    = grep("^SchoolPopularity$" ,           colnames(completeTable))

        othersConnectionsIndex   = grep("^OthersConnections$",           colnames(completeTable))
        othersPopularityIndex    = grep("^OthersPopularity$" ,           colnames(completeTable))

        # Which one is the first connection index to do things automatically in the network later
        # You allways have NETWORK connections, NETWORK popularity , NETWORK following, NETWORK reciprocity
        baseNetworkConnectionIndex = overallConnectionsIndex
        
    }  
    
    # -- Biomarkers
    {
        
        firstBiomarkerIndex   = grep("Adenosine.Deaminase", colnames(completeTable))[1]
        
    }
    
    # -- Diet
    {
        
        leanFishIndex       = grep("^LeanFishFrequency$",       colnames(completeTable))[1]
        fatFishIndex        = grep("^FatFishFrequency$",        colnames(completeTable))[1]
        cheeseIndex         = grep("^CheeseFrequency$",         colnames(completeTable))[1]
        chocolateIndex      = grep("^ChocolateFrequency$",      colnames(completeTable))[1]
        fruitsIndex         = grep("^FruitsFrequency$",         colnames(completeTable))[1]
        vegetablesIndex     = grep("^VegetablesFrequency$",     colnames(completeTable))[1]
        dairyIndex          = grep("^DairyFrequency$",          colnames(completeTable))[1]
        juiceFruitIndex     = grep("^FruitJuiceFrequency$",     colnames(completeTable))[1]
        juiceSugarIndex     = grep("^SugarJuiceFrequency$",     colnames(completeTable))[1]
        sugarDrinkIndex     = grep("^SugarDrinkFrequency$",     colnames(completeTable))[1]
        sweetenerDrinkIndex = grep("^SweetenerDrinkFrequency$", colnames(completeTable))[1]
        waterIndex          = grep("^WaterFrequency$",          colnames(completeTable))[1]
        
        fishOilIndex        = grep("^FishOilFrequency$",        colnames(completeTable))[1]
        vitaminsIndex       = grep("^VitaminsFrequency$",       colnames(completeTable))[1]

        #firstDietIndex      = grep("^LeanFishFrequency$",       colnames(completeTable))[1]
        #lastDietIndex       = grep("^VitaminsFrequency$",        colnames(completeTable))[1]
        
        dietIndexes         = c(leanFishIndex, fatFishIndex, cheeseIndex, chocolateIndex,
                                fruitsIndex, vegetablesIndex, dairyIndex, juiceFruitIndex,
                                juiceSugarIndex, sugarDrinkIndex, sweetenerDrinkIndex, waterIndex,
                                fishOilIndex, vitaminsIndex)
        totalDietIndexes    = length(dietIndexes)
        
        
        vitaminDIndexes     = c(leanFishIndex, fatFishIndex, dairyIndex, cheeseIndex, fishOilIndex, vitaminsIndex) 
        
    }
    
    # -- Blood
    {
        
        vitamimDIndex       = grep("^X25.OH.D_.nmol.L.$",              colnames(completeTable))[1]
        calciumIndex        = grep("^Calcium_.mmol.L.$",               colnames(completeTable))[1]
        pthIndex            = grep("^PTH_.pmol.L.$",                   colnames(completeTable))[1]
        
        apoBIndex           = grep("^Apolipoprotein_B_.g.L.$",         colnames(completeTable))[1]
        retinolIndex        = grep("^Retinol_.µmol.L.$",               colnames(completeTable))[1]
        
        estradiolIndex      = grep("^Estradiol_E2_.nmol.L.$",          colnames(completeTable))[1]
        progesteronIndex    = grep("^Progesterone_.nmol.L.$",          colnames(completeTable))[1]
        
            
        firstBloodIndex     = grep("Mean_Corposcular_Hemoglobin_.pg.", colnames(completeTable))[1]
        lastBloodIndex      = grep("FA_C22.6_n.3_.weight..",           colnames(completeTable))[1]
        totalBloodColumns   = lastBloodIndex - firstBloodIndex + 1
        
        firstFAIndex        = grep("FA_C12.0_.mcg.ml",                 colnames(completeTable))[1]
        lastFA2Index        = grep("FA_C22.6_n.3_.mcg.ml.",            colnames(completeTable))[1]
        
        lastFAIndex         = lastBloodIndex
        totalFAColumns      = lastFAIndex  - firstFAIndex + 1
        totalFA2Columns     = lastFA2Index - firstFAIndex + 1
        
        testosteroneIndex   = grep("Testosterone_.nmol.L.",            colnames(completeTable))[1]
        shbgIndex           = grep("SHBG_.nmol.L.",                    colnames(completeTable))[1]
        
        
        
        
        
        
    }
    
    # -- Hygiene Table
    {
    
        solariumIndex       = grep("^SolariumLast4Weeks$",             colnames(completeTable))[1]
    	sunbathingIndex     = grep("^HolidaySunbathing$",              colnames(completeTable))[1]
        
    }
    
    # -- Contraceptives
    {
        
        hormonalTypeIndex   = grep("^HormonalType$",                   colnames(completeTable))[1]
    }
    
    # -- Antropometric
    {
        firstAntropometricIndex = grep("^Waist$",                      colnames(completeTable))[1]
    }
    
    
}

# Collections of previous indexes
{

    # Variables used for stratification
    {
        categoricalStratificationIndexes = c(sexIndex)  
        numericalStratificationIndexes   = c(NA,NA,NA)
    }
    
    # All indexes that are not ID or any other minor auxiliary variable
    {
        allIndexes = c(sexIndex, healthIndex,                  # -- Basic
                       BMIIndex, BMICatIndex,                  # -- Antropometric
                       smokeIndex, snuffIndex, alcoholIndex,   # -- Drugs
                       sportsIndex)                            # -- Sports
    }
    
    # All dates indexes
    {
        
        allDatesIndexes = c(attendanceFF1Index, attendanceFF12Index, networkCreationIndex,
                            swab1AttendanceDateIndex, swab2AttendanceDateIndex, swab1CultureDateIndex,
                            swab1NasalFreezeDateIndex, swab1ThroatFreezeDateIndex, bloodExtractionDateIndex,
                            plasmaExtractionDateIndex)

    }
    
    
    # All diet indexes
    {
    
        dietIndexes = c(leanFishIndex,   fatFishIndex,        cheeseIndex, 
                        chocolateIndex,  fruitsIndex,         vegetablesIndex, 
                        dairyIndex,      juiceFruitIndex,     juiceSugarIndex,
                        sugarDrinkIndex, sweetenerDrinkIndex, waterIndex)
    	
    	
        vitDdietIndexes = c(leanFishIndex,   fatFishIndex,    cheeseIndex, 
                            dairyIndex,      vitaminsIndex,   fishOilIndex)    	

    }
    
    # All lifestyles variables indexes
    {
        
        lifeStyleCategoricalIndexes = c(sexIndex, healthIndex, BMICatIndex,
                                        smokeIndex, snuffIndex, alcoholIndex,
                                        sportsIndex, transportSummerIndex, 
                                        transportWinterIndex, screenIndex, dietIndexes)
            
    }
    
    
    # All basic STAPH indexes
    {
    
        basicStaphIndexes = c(nasalDirectCarrierIndex,     throatDirectCarrierIndex,
                              nasalEnrichmentCarrierIndex, throatEnrichmentCarrierIndex)
            
    }
    
    # All categorical indexes
    {
    
        allCategoricalIndexes = c(lifeStyleCategoricalIndexes,
                                  highSchoolIndex,
                                  basicStaphIndexes)
        
        totalCategoricalIndexes = length(allCategoricalIndexes)
        
    }
        
    
}

# Colors for each variable
#
# -- This tell the script that, if you use a particular index (ie the sex Index)
#    Here is where you can look up for the colors corresponding to that variable.
#    This helps a lot doing the automatic analysis and keeping color consistency
#    in between plots without having to define the colors for each plot, only
#    the index that you want to analyze.
#
{

    myListOfColorVectors = newList(ncol(completeTable))
    
    # Init everything to NA (R is a horrible language, can't init to NULL which is what makes sense here :( ,
    # Seriously, there are so many bugs and inconsistencies around because of this already. I want to go back to C++ ))
    for(i in 1:ncol(completeTable)){
  
        myListOfColorVectors[[i]] = NA
    
    }
  
    # Add the specific colors
    myListOfColorVectors[[sexIndex]]           = COLOR_VECTOR_SEX     # Sex
    myListOfColorVectors[[smokeIndex]]         = COLOR_VECTOR_SMOKE   # Smoke
    myListOfColorVectors[[snuffIndex]]         = COLOR_VECTOR_SMOKE   # Snuff
    myListOfColorVectors[[sportsIndex]]        = COLOR_VECTOR_SPORTS  # Sports
    myListOfColorVectors[[BMICatIndex]]        = COLOR_VECTOR_BMI     # BMI Categorical
    myListOfColorVectors[[healthIndex]]        = COLOR_VECTOR_HEALTH  # Health
    
  # myListOfColorVectors[[nasalCarrierIndex]]  = COLOR_VECTOR_CARRIER # Nasal Carrier
  # myListOfColorVectors[[throatCarrierIndex]] = COLOR_VECTOR_CARRIER # Throat Carrier
  # myListOfColorVectors[[carrierIndex]]       = COLOR_VECTOR_CARRIER # SA Carrier

}

# Human readable variables names for each index
{

    # By default, we just copy the original names
    humanReadableVariablesNames = colnames(completeTable)
    
    # If you want to change something, we do it one by one here
    humanReadableVariablesNames[healthIndex] = "How is your general health?"
        
    
}
