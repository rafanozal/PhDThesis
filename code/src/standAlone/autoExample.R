# Generate the table
AutoTable            = data.frame(matrix(NA, nrow = originalRows, ncol = 1))
AutoTable$ID         = completeTable$ID
AutoTable$Highschool = completeTable$HighSchoolID
AutoTable$Sex        = completeTable$Sex
AutoTable$Popularity = completeTable$OverallConnections

totalStudents = nrow(AutoTable)

# Prepare the color scheme so it match the high school we already have
myPalette    = colorRampPalette(brewer.pal(5, "Spectral"))
colorsVector = myPalette(length(unique(AutoTable$Highschool)))


# Generate the political view
# Assign random values from -1 to +1 in both
AutoTable$PoliticalEconomy = runif(totalStudents, min=-1, max=1)
AutoTable$PoliticalFreedom = runif(totalStudents, min=-1, max=1)

OriginalOpinionEconomy = AutoTable$PoliticalEconomy
OriginalOpinionFreedom = AutoTable$PoliticalFreedom

# Make the scatterplot with the random data highlighted by HS
{
  ggplot(AutoTable, aes(x = PoliticalEconomy, y = PoliticalFreedom)) +
    
    geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax=  1, fill = "#92d9f9", alpha = 0.03) +
    geom_rect(xmin= -1, xmax= 0, ymin=  1, ymax=  0, fill = "#fababa", alpha = 0.03) +
    geom_rect(xmin= -1, xmax= 0, ymin= -1, ymax=  0, fill = "#c9e5bd", alpha = 0.03) +
    geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax= -1, fill = "#f5f5a8", alpha = 0.03) +
    
    
    geom_point(aes(fill = Highschool), color = "black", pch = 21, size = 2) +
    scale_fill_manual(values = colorsVector) +
  
    labs(title = "Original Random Believes",
         x = " ",
         y = " ") +
    
    annotate(geom="text", x=-1,  y=0, hjust=+1.05, label="Left",
             color="black") +
    annotate(geom="text", x=+1, y=0, hjust=0.05, label="Right",
             color="black") +
    annotate(geom="text", x=0, y=+1, vjust = -1.05, label="Authoritarian",
             color="black") +
    annotate(geom="text", x=0, y=-1, vjust = +1.05, label="Libertarian",
             color="black") +
    
    xlim(-1.1, +1.1) +
    theme_bw()
  
}

ggsave("Political_0", device="png")

# Define the alpha value
alphaV = 0.7

# Prepare the weight matrix
weightMatrix = friendshipMatrix

# Run the equation for 
# Yt+1 = AWYt + (1-A)Y1

# R is horrible and does weird things when calling columns, 
# transform columns into nx1 matrix first
for(i in 1:10){
  
  EconomyN1Matrix  = matrix(AutoTable$PoliticalEconomy, ncol=1)
  FreedomN1Matrix  = matrix(AutoTable$PoliticalFreedom, ncol=1)
  
  AutoTable$PoliticalEconomy = alphaV * (weightMatrix %*% EconomyN1Matrix) + (1-alphaV) * OriginalOpinionEconomy
  AutoTable$PoliticalFreedom = alphaV * (weightMatrix %*% FreedomN1Matrix) + (1-alphaV) * OriginalOpinionFreedom
  
  # Normalize the resulst to -1 +1 since the weight matrix is so close to 0
  AutoTable$PoliticalEconomy = scaleValues(AutoTable$PoliticalEconomy, -1, +1)
  AutoTable$PoliticalFreedom = scaleValues(AutoTable$PoliticalFreedom, -1, +1)
  
  # Make the scatterplot with the random data highlighted by HS
  ggplot(AutoTable, aes(x = PoliticalEconomy, y = PoliticalFreedom)) +
    
         geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax=  1, fill = "#92d9f9", alpha = 0.03) +
         geom_rect(xmin= -1, xmax= 0, ymin=  1, ymax=  0, fill = "#fababa", alpha = 0.03) +
         geom_rect(xmin= -1, xmax= 0, ymin= -1, ymax=  0, fill = "#c9e5bd", alpha = 0.03) +
         geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax= -1, fill = "#f5f5a8", alpha = 0.03) +
    
         geom_point(aes(fill = Highschool), color = "black", pch = 21, size = 2) +
         scale_fill_manual(values = colorsVector) +
    
         labs(title = paste0("Political believes at time ",i),
              x = " ",
              y = " ") +
          
         annotate(geom="text", x=-1,  y=0, hjust=+1.05, label="Left",
                  color="black") +
         annotate(geom="text", x=+1, y=0, hjust=0.05, label="Right",
                  color="black") +
         annotate(geom="text", x=0, y=+1, vjust = -1.05, label="Authoritarian",
                  color="black") +
         annotate(geom="text", x=0, y=-1, vjust = +1.05, label="Libertarian",
                  color="black") +
          
    
         xlim(-1.1, +1.1) +
         theme_bw()
  
  ggsave(paste0("Political_",i), device="png")
  
  deltaValues = sum(abs(AutoTable$PoliticalEconomy - OriginalOpinionEconomy))
  
  print(deltaValues)
}

#-----------------------------------------------------------------
#-----------------------------------------------------------------

# Reset believes
# AutoTable$PoliticalEconomy = OriginalOpinionEconomy
# AutoTable$PoliticalFreedom = OriginalOpinionFreedom

# Generate the political view
# Assign random values from -1 to +1 in both
AutoTable$PoliticalEconomy = runif(totalStudents, min=-1, max=1)
AutoTable$PoliticalFreedom = runif(totalStudents, min=-1, max=1)

# Make the scatterplot with the random data highlighted by HS
{
  ggplot(AutoTable, aes(x = PoliticalEconomy, y = PoliticalFreedom)) +
    
    geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax=  1, fill = "#92d9f9", alpha = 0.03) +
    geom_rect(xmin= -1, xmax= 0, ymin=  1, ymax=  0, fill = "#fababa", alpha = 0.03) +
    geom_rect(xmin= -1, xmax= 0, ymin= -1, ymax=  0, fill = "#c9e5bd", alpha = 0.03) +
    geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax= -1, fill = "#f5f5a8", alpha = 0.03) +
    
    
    geom_point(aes(fill = Highschool), color = "black", pch = 21, size = 2) +
    scale_fill_manual(values = colorsVector) +
    
    labs(title = "Original Random Believes",
         x = " ",
         y = " ") +
    
    annotate(geom="text", x=-1,  y=0, hjust=+1.05, label="Left",
             color="black") +
    annotate(geom="text", x=+1, y=0, hjust=0.05, label="Right",
             color="black") +
    annotate(geom="text", x=0, y=+1, vjust = -1.05, label="Authoritarian",
             color="black") +
    annotate(geom="text", x=0, y=-1, vjust = +1.05, label="Libertarian",
             color="black") +
    
    xlim(-1.1, +1.1) +
    theme_bw()
  
}

ggsave("Political_0", device="png")

OriginalOpinionEconomy = AutoTable$PoliticalEconomy
OriginalOpinionFreedom = AutoTable$PoliticalFreedom

# Make the weight matrix so some people won't change their position ever
# And their opinion are supper important
weightMatrix = friendshipMatrix

# Make a dynamic alpha
alphaV = runif(totalStudents, min=0, max=1)
#alphaV = rep(0.8, totalStudents)
alphaV = 0.8


# Find someone that is very up-left
UpLeftID = AutoTable[AutoTable$PoliticalEconomy > 0.8 & AutoTable$PoliticalFreedom < -0.8, ]$ID[1]
# alphaV[UpLeftID] = 0
  weightMatrix[UpLeftID,] = rbinom(totalStudents, 1, 0.01)
  weightMatrix[,UpLeftID] = rbinom(totalStudents, 1, 0.01)
#weightMatrix[UpLeftID,UpLeftID] = 10
# 
UpRightID = AutoTable[AutoTable$PoliticalEconomy > 0.8 & AutoTable$PoliticalFreedom > 0.8, ]$ID[1]
# alphaV[UpRightID] = 0
  weightMatrix[UpRightID,] = rbinom(totalStudents, 1, 0.01)
  weightMatrix[,UpRightID] = rbinom(totalStudents, 1, 0.01)
#weightMatrix[UpRightID,UpRightID] = 10
# 
DownRightID = AutoTable[AutoTable$PoliticalEconomy < -0.8 & AutoTable$PoliticalFreedom > 0.8, ]$ID[1]
# alphaV[DownRightID] = 0
  weightMatrix[DownRightID,] = rbinom(totalStudents, 1, 0.01)
  weightMatrix[,DownRightID] = rbinom(totalStudents, 1, 0.01)
#weightMatrix[DownRightID,DownRightID] = 10
# 
DownLeftID = AutoTable[AutoTable$PoliticalEconomy < -0.8 & AutoTable$PoliticalFreedom < -0.8, ]$ID[1]
# alphaV[DownLeftID] = 0
  weightMatrix[DownLeftID,] = rbinom(totalStudents, 1, 0.01)
  weightMatrix[,DownLeftID] = rbinom(totalStudents, 1, 0.01)
#weightMatrix[DownLeftID,DownLeftID] = 10

# R is horrible and does weird things when calling columns, 
# transform columns into nx1 matrix first
for(i in 1:10){
  
  EconomyN1Matrix  = matrix(AutoTable$PoliticalEconomy, ncol=1)
  FreedomN1Matrix  = matrix(AutoTable$PoliticalFreedom, ncol=1)
  
  AutoTable$PoliticalEconomy = alphaV * (weightMatrix %*% EconomyN1Matrix) + (1-alphaV) * OriginalOpinionEconomy
  AutoTable$PoliticalFreedom = alphaV * (weightMatrix %*% FreedomN1Matrix) + (1-alphaV) * OriginalOpinionFreedom
  
  # Normalize the resulst to -1 +1 since the weight matrix is so close to 0
  AutoTable$PoliticalEconomy = scaleValues(AutoTable$PoliticalEconomy, -1, +1)
  AutoTable$PoliticalFreedom = scaleValues(AutoTable$PoliticalFreedom, -1, +1)
  
  # Reset the original positions of the leaders
  # Stalin
  AutoTable$PoliticalEconomy[UpLeftID] = OriginalOpinionEconomy[UpLeftID]
  AutoTable$PoliticalFreedom[UpLeftID] = OriginalOpinionFreedom[UpLeftID]
  # Pinochet
  AutoTable$PoliticalEconomy[UpRightID] = OriginalOpinionEconomy[UpRightID]
  AutoTable$PoliticalFreedom[UpRightID] = OriginalOpinionFreedom[UpRightID]
  # Jeff Bezos
  AutoTable$PoliticalEconomy[DownRightID] = OriginalOpinionEconomy[DownRightID]
  AutoTable$PoliticalFreedom[DownRightID] = OriginalOpinionFreedom[DownRightID]
  # Pato
  AutoTable$PoliticalEconomy[DownLeftID] = OriginalOpinionEconomy[DownLeftID]
  AutoTable$PoliticalFreedom[DownLeftID] = OriginalOpinionFreedom[DownLeftID]
   
  
  # Make the scatterplot with the random data highlighted by HS
  ggplot(AutoTable, aes(x = PoliticalEconomy, y = PoliticalFreedom)) +
    
    geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax=  1, fill = "#92d9f9", alpha = 0.03) +
    geom_rect(xmin= -1, xmax= 0, ymin=  1, ymax=  0, fill = "#fababa", alpha = 0.03) +
    geom_rect(xmin= -1, xmax= 0, ymin= -1, ymax=  0, fill = "#c9e5bd", alpha = 0.03) +
    geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax= -1, fill = "#f5f5a8", alpha = 0.03) +
    
    geom_point(aes(fill = Highschool), color = "black", pch = 21, size = 2) +
    scale_fill_manual(values = colorsVector) +
    
    labs(title = paste0("Political believes at time ",i),
         x = " ",
         y = " ") +
    
    annotate(geom="text", x=-1,  y=0, hjust=+1.05, label="Left",
             color="black") +
    annotate(geom="text", x=+1, y=0, hjust=0.05, label="Right",
             color="black") +
    annotate(geom="text", x=0, y=+1, vjust = -1.05, label="Authoritarian",
             color="black") +
    annotate(geom="text", x=0, y=-1, vjust = +1.05, label="Libertarian",
             color="black") +
    
    
    xlim(-1.1, +1.1) +
    theme_bw()
  
  ggsave(paste0("Political_",i), device="png")
  
  deltaValues = sum(abs(AutoTable$PoliticalEconomy - OriginalOpinionEconomy))
  
  print(deltaValues)
}

  #-----------------------------------------------------------------
  #-----------------------------------------------------------------
  
  # Introduce someone in random highschools that is radicalized and won't change opinion
  
  # Generate the political view
  # Assign random values from -1 to +1 in both
  AutoTable$PoliticalEconomy = runif(totalStudents, min=-1, max=1)
  AutoTable$PoliticalFreedom = runif(totalStudents, min=-1, max=1)
  
  # Make the weight matrix so some people won't change their position ever
  # And their opinion are supper important
  weightMatrix = friendshipMatrix
  
  # Make a dynamic alpha
  alphaV = 0.8
  
  # Enter the radicals that has the most connections with the highschool
  
  # Stalin for H7
  allH        = AutoTable[AutoTable$Highschool == "H7",]
  totalH      = nrow(allH)
  maxID       = 0
  candidateID = 0
  for (i in 1:totalH){
    
    # Get the ID
    currentID = allH$ID[i]
    
    # Get their friend
    currentFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
                              getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
    
    totalCurrentFriends = length(currentFriends)
    
    totalCurrentFriendsSameSchool = 0
    
    # How many of those friends belongs to the same highshool
    for(j in 1:totalCurrentFriends){
    
      currentFriendID = currentFriends[j]
      
      if(AutoTable[AutoTable$ID == currentFriendID,]$Highschool == "H7")
      
        totalCurrentFriendsSameSchool = totalCurrentFriendsSameSchool + 1
        
    }
    if(totalCurrentFriendsSameSchool > maxID) candidateID = currentID
    
  }
  H3StalinID = candidateID
  
  # Pinochet for H5
  allH        = AutoTable[AutoTable$Highschool == "H5",]
  totalH      = nrow(allH)
  maxID       = 0
  candidateID = 0
  for (i in 1:totalH){
    
    # Get the ID
    currentID = allH$ID[i]
    
    # Get their friend
    currentFriends = unique(c(getFrienshipTypes(currentID, friendshipMatrix)[[4]],
                              getFrienshipTypes(currentID, friendshipMatrix)[[5]]))
    
    totalCurrentFriends = length(currentFriends)
    
    totalCurrentFriendsSameSchool = 0
    
    # How many of those friends belongs to the same highshool
    for(j in 1:totalCurrentFriends){
      
      currentFriendID = currentFriends[j]
      
      if(AutoTable[AutoTable$ID == currentFriendID,]$Highschool == "H5")
        
        totalCurrentFriendsSameSchool = totalCurrentFriendsSameSchool + 1
      
    }
    if(totalCurrentFriendsSameSchool > maxID) candidateID = currentID
    
  }
  H5PinochetID = candidateID
  
  
  
  #H3StalinID   = AutoTable[AutoTable$Highschool == "H7" & AutoTable$Popularity > 9 ,]$ID[1]
  #H5PinochetID = AutoTable[AutoTable$Highschool == "H5" & AutoTable$Popularity > 9 ,]$ID[1]
  # H7BezosID    = AutoTable[AutoTable$Highschool == "H7" & AutoTable$Popularity > 5 ,]$ID[1]
  
  # Enter their political believes
  AutoTable$PoliticalEconomy[H3StalinID]   = -0.8
  AutoTable$PoliticalFreedom[H3StalinID]   =  0.8
  
  AutoTable$PoliticalEconomy[H5PinochetID] =  0.8
  AutoTable$PoliticalFreedom[H5PinochetID] =  0.8
  
  # AutoTable$PoliticalEconomy[H7BezosID]    =  0.8
  # AutoTable$PoliticalFreedom[H7BezosID]    = -0.8
  
  # Make them popular
  # weightMatrix[H3StalinID,] = rbinom(totalStudents, 1, 0.01)
  # weightMatrix[,H3StalinID] = rbinom(totalStudents, 1, 0.01)
  # 
  # weightMatrix[H5PinochetID,] = rbinom(totalStudents, 1, 0.01)
  # weightMatrix[,H5PinochetID] = rbinom(totalStudents, 1, 0.01)
  # 
  # weightMatrix[H7BezosID,] = rbinom(totalStudents, 1, 0.01)
  # weightMatrix[,H7BezosID] = rbinom(totalStudents, 1, 0.01)
  
  OriginalOpinionEconomy = AutoTable$PoliticalEconomy
  OriginalOpinionFreedom = AutoTable$PoliticalFreedom
  
  # Make the scatterplot with the random data highlighted by HS
  {
    ggplot(AutoTable, aes(x = PoliticalEconomy, y = PoliticalFreedom)) +
      
      geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax=  1, fill = "#92d9f9", alpha = 0.03) +
      geom_rect(xmin= -1, xmax= 0, ymin=  1, ymax=  0, fill = "#fababa", alpha = 0.03) +
      geom_rect(xmin= -1, xmax= 0, ymin= -1, ymax=  0, fill = "#c9e5bd", alpha = 0.03) +
      geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax= -1, fill = "#f5f5a8", alpha = 0.03) +
      
      
      geom_point(aes(fill = Highschool), color = "black", pch = 21, size = 2) +
      scale_fill_manual(values = colorsVector) +
      
      labs(title = "Original Random Believes",
           x = " ",
           y = " ") +
      
      annotate(geom="text", x=-1,  y=0, hjust=+1.05, label="Left",
               color="black") +
      annotate(geom="text", x=+1, y=0, hjust=0.05, label="Right",
               color="black") +
      annotate(geom="text", x=0, y=+1, vjust = -1.05, label="Authoritarian",
               color="black") +
      annotate(geom="text", x=0, y=-1, vjust = +1.05, label="Libertarian",
               color="black") +
      
      xlim(-1.1, +1.1) +
      theme_bw()
    
  }
  
  ggsave("Political_0", device="png")
  
  # R is horrible and does weird things when calling columns, 
  # transform columns into nx1 matrix first
  for(i in 1:10){
    
    EconomyN1Matrix  = matrix(AutoTable$PoliticalEconomy, ncol=1)
    FreedomN1Matrix  = matrix(AutoTable$PoliticalFreedom, ncol=1)
    
    AutoTable$PoliticalEconomy = alphaV * (weightMatrix %*% EconomyN1Matrix) + (1-alphaV) * OriginalOpinionEconomy
    AutoTable$PoliticalFreedom = alphaV * (weightMatrix %*% FreedomN1Matrix) + (1-alphaV) * OriginalOpinionFreedom
    
    # Normalize the resulst to -1 +1 since the weight matrix is so close to 0
    AutoTable$PoliticalEconomy = scaleValues(AutoTable$PoliticalEconomy, -1, +1)
    AutoTable$PoliticalFreedom = scaleValues(AutoTable$PoliticalFreedom, -1, +1)
    
    # Reset the original positions of the leaders
    # Stalin
    AutoTable$PoliticalEconomy[H3StalinID] = OriginalOpinionEconomy[H3StalinID]
    AutoTable$PoliticalFreedom[H3StalinID] = OriginalOpinionFreedom[H3StalinID]
    # Pinochet
    AutoTable$PoliticalEconomy[H5PinochetID] = OriginalOpinionEconomy[H5PinochetID]
    AutoTable$PoliticalFreedom[H5PinochetID] = OriginalOpinionFreedom[H5PinochetID]
    # Jeff Bezos
    # AutoTable$PoliticalEconomy[H7BezosID] = OriginalOpinionEconomy[H7BezosID]
    # AutoTable$PoliticalFreedom[H7BezosID] = OriginalOpinionFreedom[H7BezosID]
    
    # Make the scatterplot with the random data highlighted by HS
    ggplot(AutoTable, aes(x = PoliticalEconomy, y = PoliticalFreedom)) +
      
      geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax=  1, fill = "#92d9f9", alpha = 0.03) +
      geom_rect(xmin= -1, xmax= 0, ymin=  1, ymax=  0, fill = "#fababa", alpha = 0.03) +
      geom_rect(xmin= -1, xmax= 0, ymin= -1, ymax=  0, fill = "#c9e5bd", alpha = 0.03) +
      geom_rect(xmin=  0, xmax= 1, ymin=  0, ymax= -1, fill = "#f5f5a8", alpha = 0.03) +
      
      geom_point(aes(fill = Highschool), color = "black", pch = 21, size = 2) +
      scale_fill_manual(values = colorsVector) +
      
      labs(title = paste0("Political believes at time ",i),
           x = " ",
           y = " ") +
      
      annotate(geom="text", x=-1,  y=0, hjust=+1.05, label="Left",
               color="black") +
      annotate(geom="text", x=+1, y=0, hjust=0.05, label="Right",
               color="black") +
      annotate(geom="text", x=0, y=+1, vjust = -1.05, label="Authoritarian",
               color="black") +
      annotate(geom="text", x=0, y=-1, vjust = +1.05, label="Libertarian",
               color="black") +
      
      
      xlim(-1.1, +1.1) +
      theme_bw()
    
    ggsave(paste0("Political_",i), device="png")
    
    deltaValues = sum(abs(AutoTable$PoliticalEconomy - OriginalOpinionEconomy))
    
    print(deltaValues)
  }
  