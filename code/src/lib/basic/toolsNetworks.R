############################
# GET NETWORK INFORMATION
# -- getFrienshipTypes()   For a given ID and Frienships Matrix, return the
#                          friends IDs and Total numbers of relationships for
#                          the given ID.
#
# -- addEdgeRelationship() For a given edges DF, check if the nodes share some
#                          common value. Doesn't modify the original DF.
#
# -- deleteConnections()   For a given edges DF and list of IDS, tells you which
#                          rows from the given edgesDF you should delete.
#                          Doesn't modify the original DF.
############################
{
  
    # For a given edgeDF, swap an ID for another one
    # It doesn't test if the new ID already exist!!!
    swapRelationshipsIDs <- function(edgeDF, originalID, newID){
    
        if(originalID != newID){
        
            for(i in 1:nrow(edgeDF)){
        
                if(edgeDF[i,1] == originalID) edgeDF[i,1] = newID
                if(edgeDF[i,2] == originalID) edgeDF[i,2] = newID
                
            }            
            
        }

        return(edgeDF)
    }
    
    
    # For a given ID and a given friendship matrix (not graph), return:
  #
  # How many people this ID nominated and their IDs
  # How many people nominated this ID and their IDs (popularity)
  # How many frienships are reciprocal and their IDs
  #
  # The friendship matrix must be of the type 0/1 and have IDs coincide
  # with the rows and column indexes.
  #
  # Return a list
  #
  # 1: total key popularity
  # 2: total key nominations
  # 3: total key reciprocals
  # 4: IDs that nominated this key
  # 5: IDs that this key nominated
  # 6: IDs that are reciprocal
  #
  getFrienshipTypes <- function(key, frienshipMatrix){
    
    totalRows = nrow(frienshipMatrix)
    
    # -- Popularity
    myPopularity     = sum(frienshipMatrix[,key])
    myPopularityIDs  = grep(1, frienshipMatrix[,key])
    # -- Nominations
    myNominations    = sum(frienshipMatrix[key,1:totalRows])
    myNominationsIDs = grep(1,frienshipMatrix[key,1:totalRows])
    # -- Reciprocals
    myReciprocalIDs  = myPopularityIDs[myPopularityIDs %in% myNominationsIDs]
    myReciprocal     = length(myReciprocalIDs)
    
    myReturn = vector("list", length = 6)
    myReturn[[1]] = myPopularity
    myReturn[[2]] = myNominations
    myReturn[[3]] = myReciprocal
    myReturn[[4]] = myPopularityIDs
    myReturn[[5]] = myNominationsIDs
    myReturn[[6]] = myReciprocalIDs
    
    return (myReturn)
    
  }
  
  # Shortcut for the function above, gives the IDs of all undirected friends
  # of a given ID.
  getUndirectedFriends <- function(key, friendshipMatrix){

    myFriends =  unique(c(getFrienshipTypes(key, friendshipMatrix)[[4]],          # Same fucking shit all over again, a bug where R is using something outside of the fucking function!
                          getFrienshipTypes(key, friendshipMatrix)[[5]]))         # This is horrible, I hope someone drop the nukes already and the new civilization unite under one proper language
    
    return(myFriends)
            
      
  }
  
  # For a given list of edges, and a given table, check whether the FROM and TO
  # nodes have the same value for a giving index referencing a category.
  #
  # The function does not modify the original DF, you need to assign the returned
  # vector to a new column of your edgesDF.
  #
  # Return a list of either: 
  #     TRUE/FALSE for each edge, if they have the same category (1)
  #     Category name / NOT_QUAL, if they have the same category (2)
  #
  # (bool) keepName. If FALSE (default) , it will return case (1), otherwise (2)
  #
  # Examples:
  #
  # edgesDF
  
  # From     To
  #   1       2
  #   1       3
  #   3       1
  #   3       4
  #   4       2
  #
  # tableBase
  # 1   2     3        4
  #
  # 1   Man   Healthy  Never
  # 2   Man   Obese    Never
  # 3   Woman Obese    Daily
  # 4   Woman Healthy  Sometimes
  #
  # ComparingIndex: 4
  #
  # KeepName = FALSE
  # From     To         Returned Vector:
  #   1       2         TRUE
  #   1       3         FALSE
  #   3       1         FALSE
  #   3       4         FALSE
  #   4       2         FALSE
  #
  # KeepName = TRUE
  # From     To         Returned Vector:
  #   1       2         Never
  #   1       3         NOT_EQUAL
  #   3       1         NOT_EQUAL
  #   3       4         NOT_EQUAL
  #   4       2         NOT_EQUAL
  #
  addEdgeRelationship <- function(edgesDF, tableBase, comparingIndex, keepName = FALSE){
    
    totalEdges   = nrow(edgesDF)
    resultVector = rep(FALSE, totalEdges)
    
    # Add the same/different relationship to the edges list
    for(i in 1:totalEdges){
      
      # Get the From and To
      myFromID = edgesDF[i,1]
      myToID   = edgesDF[i,2]
      
      # Find them in the table
      modalityFrom = tableBase[tableBase$ID == myFromID, comparingIndex]
      modalityTo   = tableBase[tableBase$ID == myToID,   comparingIndex]
      
      # Check if they are the same or not
      resultVector[i] = (modalityFrom == modalityTo)
      
      # Add the modality if necesary
      if(keepName == TRUE){
        if(resultVector[i] == TRUE)  resultVector[i] = as.character(modalityFrom)
        else                         resultVector[i] = "NOT_EQUAL"
      }
      
    }
    
    return(resultVector)
    
  }
  
  # This function is useful to delete relationships in an edges dataframe.
  #
  # You might want to delete all edges that source or arrives to a particular ID,
  # or you might want to delete all edges that precisely from a given ID to a 
  # given ID.
  #
  # Typically, you always want the first case, and you want to delete IDs {3,5,7}
  # for example. So you want to delete all FROMS 3,5,7, and all TOS 3,5,7. So
  # you will almost always use the first column of the returned DF to modify your
  # original edges table
  #
  # Given a list of from_IDs and to_IDs, return a list with:
  # -- All the edges that have any FROM or TO IDs
  # -- All the edges that have both FROM or TO IDs
  #
  # ie:
  #
  # Edges:
  # From To
  # 1    2
  # 1    3
  # 2    1
  # 2    4
  # 4    1
  # 1    4
  #
  # Delete FROM{1,4} TO{1}
  #
  # Will return
  #
  # OR AND
  # T  F
  # T  F
  # F  F
  # T  F
  # F  F
  # T  T
  #
  #
  # Return two list of TRUE/FALSE rows that satisfy the OR/AND relationship
  deleteConnections <- function(edgesDF, listOfFroms, listOfTos){
    
    totalFroms         = length(listOfFroms)
    totalTos           = length(listOfTos)
    totalRelationships = nrow(edgesDF)
    
    candidatesOR  = rep(FALSE, totalRelationships)
    candidatesAND = rep(FALSE, totalRelationships)
    
    # Analyze stuff
    # ---- FROMS
    for(i in 1:totalFroms){
      
      currentFrom   = listOfFroms[i]
      
      candidatesOR  = candidatesOR | (edgesDF[,1] == currentFrom)
      candidatesAND = candidatesOR
    }
    # ---- TOS
    for(i in 1:totalTos){
      
      currentTo = listOfTos[i]
      
      candidatesOR  = candidatesOR  | (edgesDF[,2] == currentTo)
      candidatesAND = candidatesAND & candidatesOR
      
    }
    
    # Return the dataframes
    myReturn = vector("list", length = 2)
    myReturn[[1]] = candidatesOR
    myReturn[[2]] = candidatesAND
    
    return (myReturn)
    
  }
  
}

# I absolutely hate that R don't have pointers, working with IDs is so annoying
# I swear to god, as soon as I'm finish with the PhD, I'm never touching R again
#
# From a frienship matrix, get a list of how many undirected frienship each
# person has
getTotalFriends <- function(frienshipMatrix){
    
    # Get how many people we have
    totalPeople     = nrow(frienshipMatrix)
    
    # Prepare the return list
    totalFriends    = rep(0, totalPeople)
    
    # First, count how many friends have the person with higher number
    # of friends
    for(i in 1:totalPeople){

        totalFriends[i] = length(getUndirectedFriends(i, frienshipMatrix))

    }    
    
    return(totalFriends)
}


# From an edge dataframe, generate the friendship matrix. I also need the total
# number of people since there are people who don't nominate or get nominated.
getFriendshipMatrix <- function(edgesDF, totalPeople){

    friendshipMatrix = matrix(0, nrow = totalPeople, ncol = totalPeople)
    
    # Now you just need to fill the matrix edge by edge
    for(i in 1:nrow(edgesDF)){
    
        currentFrom  = as.numeric(edgesDF[i,1])
        currentTo    = as.numeric(edgesDF[i,2])
        currentValue = as.numeric(edgesDF[i,3])
        
        friendshipMatrix[currentFrom, currentTo] = currentValue
        
        
    }
    
    return(friendshipMatrix)
    
        
    
}



#
# For a given network in a form of a friendship matrix, and a table with a
# categorical variable, count how many people have each category
#
# It return the following table:
#
# -----------------------------------------------------------
# Total Friends | Total People | Category A | Category B | ... | Category Z
# -----------------------------------------------------------
#     0         |       7      | 0         |    0       | ...  |  0 <-- The first line is always 0 in the categories, there might be different people with 0 friends, but the total for each category is obviously 0
#     1         |       3      | 0         |    3       | ...  |  0
#     2         |       6      | 2         |    4       | ...  |  6
#     3         |       1      | 3         |    0       | ...  |  0
#     4         |       6      | 0         |    24      | ...  |  0
#     5         |       0      | 0         |    0       | ...  |  0  <-- If nobody has 5 friends, the rest of the row is 0 by default
#                            ...
#
# For every row, this must be true
#
# The Total Friends * Total People = Category A + Category B + ... + Category Z
#
# Example:
# 
#     countFriendsByCategory(completeTable, sexIndex, overallMatrix)

countFriendsByCategory <- function(tableBase, categoricalIndex, frienshipMatrix){
    
        # Get how many people we have
        totalPeople      = nrow(frienshipMatrix)
    
        # Get how many friends each person has
        totalFriendsList = getTotalFriends(frienshipMatrix)
        maxFriends       = max(totalFriendsList)

        # Get how many modalities are in the categorical column
        myCategories     = getCategories(tableBase, categoricalIndex)
        totalCategories  = length(myCategories)
        
        # Initialize the friendship counter DF
        frienshipDF = DF( (maxFriends+1) , (totalCategories + 2), defaultValue = 0)
        colnames(frienshipDF) = c("TotalFriends", "TotalPeople", myCategories)
        # -- Fill the initial information
        for(i in 1:(maxFriends + 1)){
            
            currentTotalFriends = i - 1
            frienshipDF[i,1]    = currentTotalFriends
            frienshipDF[i,2]    = sum(totalFriendsList == currentTotalFriends)
            
        }
        
        # Now, for each person, start counting how many friends of each category you have
        for(i in 1:totalPeople){
                
            # Supress the ID
            currentID = i
    
            # Check how many friends this person has
            currentTotalFriends = totalFriendsList[i]
            
            # If you have at least one friend, count for each category, otherwise
            # The row for 0 friends is already done as we don't have to fill anything
            if(currentTotalFriends > 0){
                
                # If you have at least one friend, get their IDs
                currentFriends = getUndirectedFriends(currentID, frienshipMatrix)
                
                # For each friend that you have
                for(j in 1:currentTotalFriends){
        
                    # Friend ID
                    currentFriendID = currentFriends[j]
        
                    # Find the category
                    currentCategory = tableBase[currentFriendID, categoricalIndex]
                    
                    # Find in which position is this category in the list of categories
                    
                    # (parenthesis)
                    # This is one of the reasons of why I despise R and everything about it
                    #
                    # THIS IS NOT OK
                    #
                    # > which(getCategories(completeTable, sexIndex) == "Man")
                    # [1] 1
                    # > which(getCategories(completeTable, sexIndex) == "Man")[[1]]
                    # [1] 1
                    # > which(getCategories(completeTable, sexIndex) == "Man")[[1]][[1]]
                    # [1] 1
                    # > which(getCategories(completeTable, sexIndex) == "Man")[1]
                    # [1] 1
                    # > which(getCategories(completeTable, sexIndex) == "Man")[1][1]
                    # [1] 1
                    #
                    # WHAT IN THE HELL FUCK IS WRONG WITH YOUR INDEXING!???
                    # THIS IS NOT A VALID OBJECT STRUCTURE
                    # THIS IS A TOTAL NIGHMARE
                    # PLEASE DIE
                    #
                    # WHY IN FUCK DO YOU EVEN HAVE DIFFERENT INDEXING FOR LISTS AND VECTORS!!?=!=!=!?
                    # THIS DOESN?T MAKE ANY SENSE, WHO DESIGNED THIS GARBAGE OF LANGUAGE!??
                    # ARE YOU A LUNATIC? ARE YOU HIGH ON DRUGS!?
                    
                    # Find out the index of that category
                    categoryIndex = which(myCategories == currentCategory)
                    
                    # Increment++ the total of people with that many friends for that category
                    # (I hate that indexes don't start at 0 in R, you always go with the stupid +1 everywhere )
                    frienshipDF[ (currentTotalFriends + 1), (categoryIndex + 2)] = frienshipDF[ (currentTotalFriends + 1), (categoryIndex + 2)] + 1 # AND YOU DONT EVEN HAVE A ++ OPERATOR, LOOK A THIS STUPID LINE IS SO DIFFICULT TO UNDESTAND FOR SOMETHING SO SIMPLE!!!!!
                    
                }
            }            
            
        }
        
        # We are finish, return the object
        return(frienshipDF)
        
}



# From the function countFriendsByCategory() , return the same data in melted format
#
# This is:
#
# -----------------------------------------------------------------------
# My Status   | Total Friends | Category A | Category B | ... | Category Z
# -----------------------------------------------------------------------
# Category B |      3         |     1      |      2     | ... |  0
# Category A |      5         |     0      |      2     | ... |  3
# Category D |      0         |     0      |      0     | ... |  0
#                                         ...
# Category Z |      1         |     1      |      0     | ... |  0
#
# For every row, this must be true
#
# The Total Friends  = Category A + Category B + ... + Category Z
#
# Example:
#
#     meltedCountFriendsByCategory(completeTable, sexIndex, overallMatrix)
meltedCountFriendsByCategory <- function(tableBase, categoricalIndex, frienshipMatrix){
    
        # Get how many people we have
        totalPeople      = nrow(frienshipMatrix)
    
        # Get how many friends each person has
        totalFriendsList = getTotalFriends(frienshipMatrix)                       # R IS A FUCKING SHIT FOCK FUUUUUUUUUCK, There was an error here where the function was calling
                                                                                  # a matrix outside the function. Of course if you have something similar to a real language
                                                                                  # it will never let you do that unless you declare it first as a global variable; which you will
                                                                                  # never do because is not A FUCKING UNIVERSAL CONSTANT. I hate you, I hate this shit. Half my
                                                                                  # time in the PhD is dealing with R stupidity. This is a waste of time!
        # Get how many modalities are in the categorical column
        myCategories     = getCategories(tableBase, categoricalIndex)
        totalCategories  = length(myCategories)
        
        # Initialize the friendship counter DF
        frienshipDF = DF( totalPeople , (totalCategories + 2), defaultValue = 0)
        colnames(frienshipDF) = c("MyStatus", "TotalFriends", myCategories)
        
        # Fill the information 
        for(i in 1:totalPeople){
            
            # Get the ID
            currentID           = i
            currentTotalFriends = totalFriendsList[i]
            
            # Fill the basic information
            frienshipDF[i,1] = as.character(tableBase[i, categoricalIndex]) # R is a stupid language, I want the value, not the enumeration, always have to convert to string all the time :(
            frienshipDF[i,2] = currentTotalFriends

            # If you have more than one friend
            if(currentTotalFriends > 0){
                
                # If you have at least one friend, get their IDs
                currentFriends = getUndirectedFriends(currentID, frienshipMatrix)
                
                # For each friend that you have
                for(j in 1:currentTotalFriends){
                
                    # Friend ID
                    currentFriendID = currentFriends[j]
        
                    # Find the category
                    currentCategory = tableBase[currentFriendID, categoricalIndex]                    

                    # Find out the index of that category
                    categoryIndex = which(myCategories == currentCategory)
                    
                    # Increment++ the total of people with that many friends for that category
                    frienshipDF[ currentID, (categoryIndex + 2)] = frienshipDF[ currentID, (categoryIndex + 2)] + 1 
                    
                }
                
            }
            
        }
    
        # Return the final table
        return(frienshipDF)
    
}


# From a given edges dataframe, and a table with a categorical index, return all the
# relationships between each category. The table MUST HAVE a column named "ID", and
# this column need to be a numerical value that coincide with the given edges DF
#
# (R is stupid, pointers exist for reasons like this)
#
# For example, a table with 1000 people that have 3000 relationships in total returns
# a dataframe with 3000 rows in this format:
#
# ------------
# From  | To
# ------------
# Man     Woman
# Man     Man
# Woman   Man
# Woman   Man
# Man     Man
#    ... (x 3000)
# Woman   Woman
#
# This is useful to run a xi2 test later and see if there bias in the relationships
# is a bit better than the complete homophily test
#
# Example:
#
#     meltedRelationships(completeTable, sexIndex, overallEdgesDF)
meltedRelationships <- function(tableBase, categoricalIndex, edgesDF){

    # Get the total edges and create the empty return DF
    totalEdges   = nrow(edgesDF)
    returnDF     = DF(totalEdges, 2)
    colnames(returnDF) = c("From", "To")

    # Add the same/different relationship to the edges list
    for(i in 1:totalEdges){
      
      # Get the From and To
      myFromID = edgesDF[i,1]
      myToID   = edgesDF[i,2]
      
      # Find them in the table
      modalityFrom = as.character(tableBase[tableBase$ID == myFromID, categoricalIndex])
      modalityTo   = as.character(tableBase[tableBase$ID == myToID,   categoricalIndex])
 
      # Write the categories into the DF     
      returnDF[i,1] = modalityFrom
      returnDF[i,2] = modalityTo
      
    }
    
    
    return(returnDF)
    
}

# For a given table with a categorical column, and friendship matrix prepare
# the data so it can be use for a logistic regression later on.
# This is use to measure number of friends with respect SA or vitamin D
#
# The function needs
#
#     tableBase        (Dataframe)  : dataframe with the data
#
#     categoricalIndex (int)        : index with the categorical data that you want to analyze
#
#     modalityOne      (string)     : Which modality should be consider 1. Your data should only
#                                     have two modalities possible, otherwise doing this is wrong.
#                                     The function substitute this modality to 1 and everything
#                                     else to 0.
#
#     friendshipMatrix (int Matrix) : matrix with the frienship information
#                                     (see getFriendshipMatrix(edgesDF, totalPeople) to get one easily)
#
#
# And return a DF with the following structure
#
#     -----------------------------------
#     "x" | "y"  | "Original Categories"
#     -----------------------------------
#      0     0         Man
#      2     1         Woman
#      3     1         Woman
#      1     0         Man
#      0     1         Woman
#        ...
#
prepareLogitData <- function(tableBase, categoricalIndex, modalityOne, frienshipMatrix){
    
    # Get the data that you need for the plot and analysis
    myBasicData = meltedCountFriendsByCategory(tableBase, categoricalIndex, frienshipMatrix)

    # Filter people with 0 friends
    myBasicData = myBasicData[myBasicData[,2] > 0 , ]
    myOldNames  = myBasicData[,1]
    
    # Change the modalities to 0 and 1
    for(i in 1:nrow(myBasicData)){
    
        if(myBasicData[i,1] == modalityOne) myBasicData[i,1] = 1        # R, WHAT THE FUCK SHIT. A fucking 1 , is a number 1, is not a "1" you fucking piece of shit
        else                                myBasicData[i,1] = 0        # if you don't like a 1 being a "1", fuck you, and don't cast it as a string because you want to
                                                                        # without telling anybody. At least have the decency of giving a compilation error. Oh wait,
                                                                        # right, you don't compile because you are an schizophrenic inbreed language.
        
                                                                        # The worse is that when you print the DF, it doesn't print "1" in the columns, it prints 1, and
                                                                        # it looks like a fucking number which it is!!

    }
    myBasicData[,1] = as.numeric(myBasicData[,1])                       # Stupic cunt shit 
    
    
    # Off all the columns that you have, figure out which one is the 1 column
    categoryIndex = which(colnames(myBasicData)[3:ncol(myBasicData)] == modalityOne)
    
    # These are the column that you want for the analysis
    yData = myBasicData[,1]
    xData = myBasicData[,categoryIndex+2]

    # Return everything
    myReturnDF = DF(length(yData), 3)
    colnames(myReturnDF) = c("x", "y", "OriginalCategories")
    myReturnDF[,1] = xData
    myReturnDF[,2] = yData
    myReturnDF[,3] = myOldNames
    
    return(myReturnDF)

}

# TODO
#
# Given a edge dataframe, return all the basic statistics regarding that graph
#
#     edgesDF is the dataframe with the edges
#
#     nodeDF is the dataframe with the node information, you need this because
#                                                        you have people with 0
#                                                        friends that bring down
#                                                        the density for example
#
#     directed, whether your graph is directed (TRUE) or undirected (FALSE)
#                     
summarizeGraph <- function(edgeDF, nodeDF, directed = TRUE){

	myResults = DF(4,2)
	myResults[1,1] = "Density"
	myResults[2,1] = "Max Path"
	myResults[3,1] = "Max non Inf Path"
	myResults[4,1] = "Excentricity"
	
    myGraph = graph_from_data_frame(edgeDF,  vertices = nodesDF, directed = directed) 
    
    # Density
    myResults[1,2] = graph.density(myGraph,loop=FALSE)
    
    # Max path
    myResults[2,2] = max(shortest.paths(myGraph,mode="all"))
    
    # Max non infinity path (diameter)
    myResults[3,2] = diameter(overallGraph)
 
    # Excentricity   (diameter with weights)
    myResults[4,2] = max(eccentricity(myGraph,mode="all"))
    
    #diameter(graph_data, weights = NA)
    
    return(myResults)
}


	
	    
