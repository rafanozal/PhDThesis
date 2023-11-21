################################################################################
#
# This file encode functions related to medicine
#
# -- getATCInfo()      For a ATC medical code, get some info about it
# -- getICD10Info()    For a ICD10 code, get some info about it
# -- getHormonalType() For a given hormonal contraceptive brand, return the type
#                      of hormonal contraceptive that it is (ie, Progestin,
#                      Progestin + Estradiol, Non-hormonal, or Unknown)
################################################################################

# For a ATC medical code, get some info about it
getATCInfo <- function(atcCode){
    
    drugFamily = paste0("ATC_CODE_BAD_FORMAT_", atcCode)
    
    #Get the first letter
    firstLetter = substring(atcCode, 1, 1) # R is a horrible language, can't reference chars in strings, and can't do pop() in strings
    
    # R is a very horrible language that doesn't have switch
    if(firstLetter == "A") drugFamily = "Alimentary tract and metabolism"
    if(firstLetter == "B") drugFamily = "Blood and blood forming organs"
    if(firstLetter == "C") drugFamily = "Cardiovascular system"
    if(firstLetter == "D") drugFamily = "Dermatologicals"
    if(firstLetter == "G") drugFamily = "Genito-urinary system and sex hormones"
    if(firstLetter == "H") drugFamily = "Systemic hormonal preparations, excluding sex hormones and insulins"
    if(firstLetter == "J") drugFamily = "Antiinfectives for systemic use"
    if(firstLetter == "L") drugFamily = "Antineoplastic and immunomodulating agents"
    if(firstLetter == "M") drugFamily = "Musculo-skeletal system"
    if(firstLetter == "N") drugFamily = "Nervous system"
    if(firstLetter == "P") drugFamily = "Antiparasitic products, insecticides and repellents"
    if(firstLetter == "R") drugFamily = "Respiratory system"
    if(firstLetter == "S") drugFamily = "Sensory organs"
    if(firstLetter == "V") drugFamily = "Various"
    
    return(drugFamily)
 }
  
# For a ICD10 code, get some info about it
getICD10Info <- function(icd10Code){
    
    diseaseFamily = paste0("Unknown_Code_", icd10Code)
    
    #Get the first letter
    firstLetter = substring(icd10Code, 1, 1) # R is a horrible language, can't reference chars in strings, and can't do pop() in strings
    #Get the next 2 numbers
    numericCode = as.numeric(substring(icd10Code, 2, 2))
    
    # R is a very horrible language that doesn't have switch
    if(firstLetter == "A") diseaseFamily  = "Infectious and parasitic"
    if(firstLetter == "B") diseaseFamily  = "Infectious and parasitic"
    if(firstLetter == "C") diseaseFamily  = "Neoplasms"
    if(firstLetter == "D"){
      if(numericCode <  50) diseaseFamily = "Neoplasms"
      if(numericCode >= 50) diseaseFamily = "Blood and blood-forming organs"
    } 
    
    if(firstLetter == "E") diseaseFamily  = "Endocrine, nutritional and metabolic"
    if(firstLetter == "F") diseaseFamily  = "Mental and behavioural"
    if(firstLetter == "G") diseaseFamily  = "Nervous system"
    if(firstLetter == "H"){
      if(numericCode <  60) diseaseFamily = "Eye and adnexa"
      if(numericCode >= 60) diseaseFamily = "Ear and mastoid process"
    }
    
    if(firstLetter == "I") diseaseFamily  = "Circulatory system"  
    if(firstLetter == "J") diseaseFamily  = "Respiratory system"
    if(firstLetter == "K") diseaseFamily  = "Digestive system"
    if(firstLetter == "L") diseaseFamily  = "Skin and subcutaneous tissue"
    
    if(firstLetter == "M") diseaseFamily  = "Musculoskeletal system and connective tissue"
    if(firstLetter == "N") diseaseFamily  = "Genitourinary system"
    if(firstLetter == "O") diseaseFamily  = "Pregnancy, childbirth and the puerperium"
    if(firstLetter == "P") diseaseFamily  = "Perinatal period"
    
    if(firstLetter == "Q") diseaseFamily  = "Congenital malformations, deformations and chromosomal abnormalities"
    if(firstLetter == "R") diseaseFamily  = "Not elsewhere classified"
    if(firstLetter == "S") diseaseFamily  = "Injury, poisoning and certain other consequences of external causes"
    if(firstLetter == "T") diseaseFamily  = "Injury, poisoning and certain other consequences of external causes"
    if(firstLetter == "U") diseaseFamily  = "Codes for special purposes "
    
    if(firstLetter == "V") diseaseFamily  = "External causes of morbidity and mortality"
    if(firstLetter == "W") diseaseFamily  = "External causes of morbidity and mortality"
    if(firstLetter == "X") diseaseFamily  = "External causes of morbidity and mortality"
    if(firstLetter == "Y") diseaseFamily  = "External causes of morbidity and mortality"
    
    if(firstLetter == "Z") diseaseFamily  = "Factors influencing health status and contact with health services"
    if(firstLetter == "Ñ") diseaseFamily  = "Review ICD10 Code manually "
    
    return(diseaseFamily)

    
}
  

# For a given hormonal contraceptive brand, return whether it is Progesting,
# combination of progestin and estradiol, or unknown type of HC.
#
# The rules for the hormonal column goes as follows
#
#     Non-Hormonal:        Condons
#
#     Progestin-only:      Cerazette, Nexplanon, Depo-provera, Implanon
#
#     Progestin-Estradiol: 
# 
#         Low Estradiol:   Mercilon, Yasminelle, Loette 28, Nuvaring,
#
#         High Estradiol:  Marvelon, Yasmin, Microgynon, Oralcon, Diane,
#                          Synfase, Evra, Zyrona
#
#     Unknown:             Any other brand/type 
getHormonalType <- function(brandName){
    
    hormonalType = "Unknown"
    
    if(brandName == "Condons")      hormonalType = "Non-Hormonal"
    
    if(brandName == "Cerazette")    hormonalType = "Progestin"
    if(brandName == "Nexplanon")    hormonalType = "Progestin"
    if(brandName == "Depo-provera") hormonalType = "Progestin"
    if(brandName == "Implanon")     hormonalType = "Progestin"
    
    if(brandName == "Mercilon")     hormonalType = "Low Estradiol"
    if(brandName == "Yasminelle")   hormonalType = "Low Estradiol"
    if(brandName == "Loette")       hormonalType = "Low Estradiol"
    if(brandName == "Loette 28")    hormonalType = "Low Estradiol"
    if(brandName == "Nuvaring")     hormonalType = "Low Estradiol"
    
    if(brandName == "Marvelon")     hormonalType = "High Estradiol"
    if(brandName == "Yasmin")       hormonalType = "High Estradiol"
    if(brandName == "Microgynon")   hormonalType = "High Estradiol"
    if(brandName == "Oralcon")      hormonalType = "High Estradiol"
    if(brandName == "Diane")        hormonalType = "High Estradiol"
    if(brandName == "Synfase")      hormonalType = "High Estradiol"
    if(brandName == "Evra")         hormonalType = "High Estradiol"
    if(brandName == "Zyrona")       hormonalType = "High Estradiol"
    
    return(hormonalType)
  
}