# We want to check how friends influence using ML techniques.
# Let's prepare a dataframe with the data to use there so is easier to
# move around


machineLearningVariables = c(sexIndex, BMIIndex, smokeIndex, snuffIndex, alcoholIndex, sportsIndex, highSchoolIndex, overallConnectionsIndex)
machineDF = completeTable[,machineLearningVariables]

# Add the BMI in FF2
machineDF$BMIFF2 = antropometricTableFF2$BMI

# We are going to add how many friends in each category you have
machineDF$UnderweightFriends = 0
machineDF$HealthyFriends     = 0
machineDF$OverweightFriends  = 0
machineDF$ObeseFriends       = 0

matrixOverall = getFriendshipMatrix(overallEdgesDF, 1038)

for(i in 1:nrow(machineDF)){

	# Get the ID
	currentID = completeTable$ID[i]
	# Get how many friends
	currentFriends = getUndirectedFriends(currentID, matrixOverall)
	currentTotalFriends = length(currentFriends)
	machineDF$OverallConnections[i] = currentTotalFriends

	if(currentTotalFriends > 0){
	
		# Get the friends (Stupid lack of pointers, and stupid lack of proper classes!)

		currentUnderweight = 0
		currentHealthy     = 0
		currentOverweight  = 0
		currentObese       = 0
	
		for(j in 1:currentTotalFriends){
			
			print("j")
			print(j)
			
			currentFriendID  = currentFriends[j]
			
			print(currentFriendID)
			
			currentFriendBMI = completeTable$BMICategorical[currentFriendID]
			
			print(currentFriendBMI)
			
			# Bacause fack NAs in R
			if(!is.na(currentFriendBMI)){
				 
				print("aaa")
				if(currentFriendBMI == "Undeweight") currentUnderweight = currentUnderweight + 1
				if(currentFriendBMI == "Healthy")    currentHealthy     = currentHealthy     + 1
				if(currentFriendBMI == "Overweight") currentOverweight  = currentOverweight  + 1
				if(currentFriendBMI == "Obese")      currentObese       = currentObese       + 1
				print("bbb")
				
			}
			
			print(currentUnderweight)
			print(currentHealthy)
			print(currentOverweight)
			print(currentObese)
			
			
		}
	
		machineDF$UnderweightFriends[i] = currentUnderweight
		machineDF$HealthyFriends[i]     = currentHealthy
		machineDF$OverweightFriends[i]  = currentOverweight
		machineDF$ObeseFriends[i]       = currentObese
		
	}

}



write.csv2(machineDF, "/home/gromenawer/Desktop/machine.csv")