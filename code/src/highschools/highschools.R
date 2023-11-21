# This script only analyze the population dynamic in each highschool

H1Table = completeTable[completeTable$HighSchool == "H1",]
H2Table = completeTable[completeTable$HighSchool == "H2",]
H3Table = completeTable[completeTable$HighSchool == "H3",]
H4Table = completeTable[completeTable$HighSchool == "H4",]

H5Table = completeTable[completeTable$HighSchool == "H5",]
H6Table = completeTable[completeTable$HighSchool == "H6",]
H7Table = completeTable[completeTable$HighSchool == "H7",]
H8Table = completeTable[completeTable$HighSchool == "H8",]


allHighshoolTables = newList(8)
allHighshoolTables[[1]] = H1Table
allHighshoolTables[[2]] = H2Table
allHighshoolTables[[3]] = H3Table
allHighshoolTables[[4]] = H4Table

allHighshoolTables[[5]] = H5Table
allHighshoolTables[[6]] = H6Table
allHighshoolTables[[7]] = H7Table
allHighshoolTables[[8]] = H8Table

for(i in 1:8){
	
	print("------")
	print(i)
	print("------")
	print(summarizeNumerical(allHighshoolTables[[i]], ageIndex))
	print( sum(allHighshoolTables[[i]][7] > 18))
	
	doHistogramPlot2(allHighshoolTables[[i]], ageIndex, HIGHSCHOOL_FOLDER, binsWidth = 1,
		             overrideTableName = paste0("Histo_",i))
	
}

sum(completeTable$Age <= 16) / totalRows
sum(completeTable$Age > 18)  / totalRows