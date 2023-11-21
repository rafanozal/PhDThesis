# Find the homophily for a given graph and a given set of attributes
# homophily(overallGraph, "Sex"), homophily(overallGraph, "Sex", "Woman")
homophilyV2 <- function(graph , vertex.attr , attr.val=NULL , prop=TRUE){

    
    # Every graph comes from a table with attributes and a frienship matrix
    #
    # For a given variable name (because fuck indexes right?) and a given graph,
    # it does the reverse operation and gives you the column that correspont
    # with that table.
    #
    # For example, the column for the "Sex" variable
    #
    # And that thing goes into a variable called "name" that we gives to the graph
    #
    # so it end like this:
    #
    # From | To | Sex From | Sex To  | Value (*)
    # --------------------------------------
    #   1    2     Man        Woman      1
    #   1    3     Man        Man        1
    #   2    3     Woman      Man        1
    #
    # (*) Value is an arbitrary number, in our case is always 1 because we don't
    #     have half-assed relationships where you are friends but no so much, or
    #     you don't have enemies with negative values, or whatever.
    V(graph)$name<-vertex_attr(graph,vertex.attr)
    
    # From the previous table, get only the Sex From, Sex To, and Value
    ee<-get.data.frame(graph)
    
    
    #If not specifying on particular attribute value, get percentage (prop=T)#
    #or count (prop=F) of all nodes tied with matching attribute#
    
    
    # If you don't specify the modality, just count how many edges are equal
    # such as:
    
    
    if(is.null(attr.val)){
        
        if(prop==TRUE){
            
            sum(ee[,1]==ee[,2])/nrow(ee)
        }
        
        else{
            
            sum(ee[,1]==ee[,2])
        }
        
        
        #If not null, get proportion (prop=T) or count (prop=F) of#
        #edges among nodes with that particular node attribute value#
    } else {
        ifelse(prop==T,sum(ee[,1]==attr.val & ee[,2]==attr.val)/nrow(ee[ee[,1]==attr.val|ee[,2]==attr.val,]),
                 sum(ee[,1]==attr.val & ee[,2]==attr.val))
    }

}

# Provide the Xi2 analysis of the variable in the graph
# Not working well, need to re-structure the code
DELETETHISdepecratedCompleteXi <- function(graph, categoricalIndex){

      # Get the tables
      tableEdges    = igraph::as_data_frame(graph, "edges")
      tableBase     = igraph::as_data_frame(graph, "vertices")
      variableName  = colnames(tableBase)[categoricalIndex]
      totalRows     = nrow(tableBase)
      totalEdges    = nrow(tableEdges)

      # Get info about different categories
      myCategories  = unique(tableBase[,categoricalIndex])
      nCategories   = length(myCategories)
      groupingName  = colnames(tableBase)[categoricalIndex]

      # Count how many uniques for each group (Nodes)
      uniquesDF           =  data.frame(matrix(NA, nrow = nCategories, ncol = 6))
      colnames(uniquesDF) = c(groupingName, "NTotal", "NFreq", "Out", "No", "To")

      for(i in 1:nCategories){

        currentModality   = myCategories[i]
        totalModality     = sum(tableBase[,categoricalIndex] == currentModality, na.rm = TRUE)
        fqcModality       = totalModality/totalRows

        uniquesDF[[i,1]]  = currentModality
        uniquesDF[[i,2]]  = totalModality
        uniquesDF[[i,3]]  = fqcModality

      }

      # Count how many uniques for each group (Edges)
      uniquesEdgesDF        =  uniquesDF
      temporalGraph         = graph
      V(temporalGraph)$name <- vertex_attr(temporalGraph,variableName) # this doesnt work? V(temporalGraph)$names = vertex_attr(temporalGraph,variableName)
      temporalEdges         <- get.data.frame(temporalGraph) #This doesn't work either temporalEdges          = get.data.frame(temporalGraph)
                                                            # These two lines changes ID for attribute, so if you have:
      # 1 --> 3
      # 3 --> 2
      # 2 --> 1
      #
      # 1 = Man
      # 2 = Woman
      # 3 = Woman
      #
      # Man   -> Woman
      # Woman -> Woman
      # Woman -> Man
      
      for(i in 1:nCategories){

        currentModality   = myCategories[i]
        totalModality     = sum(temporalEdges[,1] == currentModality) # Take the froms that coincide with the variable name
        fqcModality       = totalModality/totalEdges

        uniquesEdgesDF[[i,1]]  = currentModality
        uniquesEdgesDF[[i,2]]  = totalModality
        uniquesEdgesDF[[i,3]]  = fqcModality

      }

      # From the edges point of view doesn't make sense to keep these columns
      uniquesEdgesDF$Out = NULL
      uniquesEdgesDF$No  = NULL
      uniquesEdgesDF$To  = NULL

      # Rename the columns
      colnames(uniquesEdgesDF) = c(groupingName, "ETotal", "EFreq")

      # Copy this info into the main summary
      uniquesDF$ETotal = uniquesEdgesDF$ETotal
      uniquesDF$EFreq  = uniquesEdgesDF$EFreq

      # We are going to make the following matrices now:
      # -- uniquesDF (already done)
      #     For each modality:
      #       (TOTAL)     Total Nodes,
      #       (FREQUENCY) Rel Frequency,
      #       (OUT)       How many of them give out at least one nomination,
      #       (NO)        How many give no nominations, 
      #       (TO)        How many of the general population receive a nomination
      #
      # -- xiDF , the absolute number of nodes linking. Each row sum should be equal to (TOTAL)
      #
      # -- xiFqcDF, the relative number of nodes linking. Each row sum should be equal to 1 (100%)
      #
      # -- xiDiffDF, the proportion between the relative frequencies of nomination with respect the actual frequencies in the population
      #              An unbias relationship should be equal to 1.
      #              A bias relationship should be greater than 1 if LIKE or smoller than 1 if DISLIKE
      #
      # -- xiBiDF,   The p-value of the difference obtained with a binomial test
      
      # Make the matrix with the absolute relationships
      xiDF           =  data.frame(matrix(0, nrow = nCategories, ncol = nCategories + 1))
      colnames(xiDF) = c("Total", myCategories)

      # Make another one with the relative frequencies and divergence
      xiFqcDF  = xiDF
      xiDiffDF = xiDF
      xiBiDF   = xiDF

      # Make the same for the edge base Xi analysis
      xiEdgesDF     = xiDF
      xiEdgesFqcDF  = xiDF
      xiEdgesDiffDF = xiDF
      xiEdgesBiDF   = xiDF

      xiEdgesConditionalFqcDF  = xiDF

      # Rename the top square
      colnames(xiFqcDF)[1]  = "N Relative"
      colnames(xiDiffDF)[1] = "N Difference"
      colnames(xiBiDF)[1]   = "N Binomial"
      
      colnames(xiEdgesFqcDF)[1]  = "E Relative"
      colnames(xiEdgesDiffDF)[1] = "E Difference"
      colnames(xiEdgesBiDF)[1]   = "E Binomial"

      #xiEdgesConditionalFqcDF[1,1] = "Rel/Cond"

      # Count who follows who
      for(i in 1:nCategories){

        # Init variables
        currentModality      = myCategories[i]

        xiDF[[i,1]]          = currentModality
        xiFqcDF[[i,1]]       = currentModality
        xiDiffDF[[i,1]]      = currentModality
        xiBiDF[[i,1]]        = currentModality

        xiEdgesDF[[i,1]]     = currentModality
        xiEdgesFqcDF[[i,1]]  = currentModality
        xiEdgesDiffDF[[i,1]] = currentModality
        xiEdgesBiDF[[i,1]]   = currentModality

        #xiEdgesConditionalFqcDF[[i,1]]  = currentModality

        # FOR THE NODES
        
        # Get the list of nodes that coincide with that category and to whom is adressed
        currentNodes        = tableBase[tableBase[,categoricalIndex] == currentModality,1]
        goingSomewhere      = unique(tableEdges[tableEdges$from %in% currentNodes ,]$from)
        goingNowhere        = setdiff(currentNodes , goingSomewhere)
        currentEdgesDestiny = unique(tableEdges[tableEdges$from %in% currentNodes ,]$to)
        totalDestiny        = length(currentEdgesDestiny)

        # Get the list of edges that coincide with that category and to whom is adressed
        uniquesDF[[i,4]]  = length(goingSomewhere)
        uniquesDF[[i,5]]  = length(goingNowhere)
        uniquesDF[[i,6]]  = totalDestiny

        # FOR THE EDGES
        
        currentTotalEdges = uniquesDF$ETotal[i]
        
        # For each of the destiny, find out which modality is
        for(j in 1:nCategories){

          destinyModality        = myCategories[j]
          candidateDestinyNodes  = tableBase[tableBase[,categoricalIndex] == destinyModality,1] # All the IDs for that modality
          destinyNodes           = intersect(currentEdgesDestiny, candidateDestinyNodes) # Intersect all the IDs with the actual destiny IDs

          # Nodes
          xiDF[[i,1+j]]          = length(destinyNodes)              # Copy the count into the main DF
          xiFqcDF[[i,1+j]]       = length(destinyNodes)/totalDestiny # Find the relative count for each
          if(totalDestiny == 0)  xiFqcDF[[i,1+j]] = 0                # If is someone who doesn't nominate anybody ever, set it as 0
                                                                     # Actually not sure what would be the correct in this case

          # Edges
          andEdges               = sum(temporalEdges[,1] == currentModality & temporalEdges[,2] == destinyModality)
          orEdges                = sum(temporalEdges[,1] == currentModality | temporalEdges[,2] == destinyModality)
          xiEdgesDF[[i,1+j]]     = andEdges                   # Absolute count
          xiEdgesFqcDF[[i,1+j]]  = andEdges/currentTotalEdges # Relative count

          #xiEdgesConditionalFqcDF[[i,1+j]]  = andEdges/orEdges   # Relative count conditional

        }

      }

      # print("---")
      # print(xiEdgesDF)
      # print(uniquesDF)
      # print("---")
      
      # Now that the uniqueDF table is complete, we can do the Binomial test
      for(i in 1:nCategories){

        # Init variables
        currentModality   = myCategories[i]

        for(j in 1:nCategories){

          destinyModality       = myCategories[j]

          # print("Trying for...")
          # print(currentModality)
          # print(destinyModality)
          # print("--------------")
          # print("--")
          # print(xiEdgesDF[[i,1+j]])
          # print(uniquesDF[[j,7]])
          # print("--")
          # print("--------------")
          
          
          
          # For the nodes
          xiDiffDF[[i,1+j]]  = xiFqcDF[[i,1+j]]/uniquesDF[[j,3]] # See how much is above the rest
          xiBiDF[[i,1+j]]    = binom.test(xiDF[[i,1+j]],         uniquesDF[[i,6]],     uniquesDF[[i,3]],            alternative = "two.sided")$p.value
                                          # how many           , row sum             , probability
                                          # Actual Connections / Total Connections   / Probability for that group
                                          #                      that goes somewhere /
                                          #                      not total nodes.

          #print(paste0 (xiDF[[i,1+j]] , " / ", uniquesDF[[i,6]], ". p = ", uniquesDF[[i,3]], " = ", xiBiDF[[i,1+j]]  ))
          
          # For the edges
          xiEdgesDiffDF[[i,1+j]] = xiEdgesFqcDF[[i,1+j]]/uniquesDF[[j,8]]
          xiEdgesBiDF[[i,1+j]]   = binom.test(xiEdgesDF[[i,1+j]], uniquesDF[[i,7]], uniquesDF[[i,8]],               alternative = "two.sided")$p.value

          # Addjust p-values to be negative which represent AVOID
          if(xiDiffDF[[i,1+j]] < 1){

            # If the value is very close to 0 (around e^-300), R will round it to 0
            # We need to keep the 0s that are negative
            if(xiBiDF[[i,1+j]]      < 1/10^250) xiBiDF[[i,1+j]]      = 1/10^250
            if(xiEdgesBiDF[[i,1+j]] < 1/10^250) xiEdgesBiDF[[i,1+j]] = 1/10^250
            
            # In any case, flip the sign
            xiBiDF[[i,1+j]]      = (-1) * xiBiDF[[i,1+j]]
            xiEdgesBiDF[[i,1+j]] = (-1) * xiEdgesBiDF[[i,1+j]]

          }

        }

      }

      # Do the Chi² Test for the whole contingency table for both NODES and EDGES
      # print("Xi-Squares")
      pValue  = chisq.test(xiDF[,-1])$p.value
      pValue2 = chisq.test(xiEdgesDF[,-1])$p.value

      myReturn = vector("list", length = 6)
      myReturn[[1]]  = uniquesDF
      myReturn[[2]]  = xiDF
      myReturn[[3]]  = xiFqcDF
      myReturn[[4]]  = xiDiffDF
      myReturn[[5]]  = xiBiDF
      myReturn[[6]]  = pValue
      myReturn[[7]]  = xiEdgesDF
      myReturn[[8]]  = xiEdgesFqcDF
      myReturn[[9]]  = xiEdgesDiffDF
      myReturn[[10]] = xiEdgesBiDF
      myReturn[[11]] = pValue2
      #myReturn[[11]] = xiEdgesConditionalFqcDF

      return (myReturn)

    }

# Given a Graph and a set of variables that are categorical it gives back summary table
# with all the homophilies combinations.
#
# The result has this structure:
#
# Variable        |  Homophily |  Frequency |   Delta |   Sign   |  Difference | P-value | Significance
# ----------------------------------------------------------
# Sex                0.83
# -- Man             0.71         0.54         0.17       +         +0.17       0.003      **
# -- Woman           0.82         0.46         0.36       +         +0.36       0.221      ns
# BMI
# -- Underweight     0.32 ...
DELETETHIScompleteHomophilyV2 <- function(graph, indexList){

    # Init variables
    totalVariables = length(indexList)

    # Get the table
    completeTable  = igraph::as_data_frame(graph, "vertices")
    variablesNames = colnames(completeTable)[indexList]

    # Count how many uniques for each group
    totalRows = 0
    for(i in 1:totalVariables){

        currentIndex = indexList[i]

        totalRows = totalRows + 1 # +1 per variable

        totalRows = totalRows + length(unique(completeTable[,currentIndex])) # + X for each category

     }

     # Create the DF where we are going to put the results
     homoDF               =  DF(totalRows,8)
     colnames(homoDF)     = c("Variable", "Homophily", "Frequency", "Delta", "Sign", "Difference", "P-value", "Significance")

     # Fill the table
     tableIndex = 1
     for(i in 1:totalVariables){

        currentIndex      = indexList[i]
        variableName      = variablesNames[i]
        variableHomophily = homophily(graph, variableName)
        #variableUniques   = summarizeCategorical(completeTable, currentIndex, sorted = "none", crop = 0)[,1]
        variableUniques   = getCategories(completeTable, currentIndex)
        totalUniques      = length(variableUniques)

        print(variableUniques)
        
        homoDF[tableIndex,1] = variableName
        homoDF[tableIndex,2] = variableHomophily

        tableIndex = tableIndex + 1

        for(j in 1:totalUniques){

          currentModality         = variableUniques[j]
          variableHomophily       = homophily(graph, variableName, currentModality)
          totalModality           = sum(completeTable[,currentIndex] == currentModality, na.rm = TRUE)
          proportionModality      = totalModality / nrow(completeTable)
          totalSameToSameModality = round(variableHomophily * totalModality)
          currentSign             = "+"

          homoDF[tableIndex,1] = currentModality
          homoDF[tableIndex,2] = variableHomophily
          homoDF[tableIndex,3] = proportionModality
          homoDF[tableIndex,4] = abs(variableHomophily - (proportionModality))
          homoDF[tableIndex,6] = variableHomophily - (proportionModality)
          print(currentModality)
          if(homoDF[tableIndex,6] <0) currentSign = "-"
          
          homoDF[tableIndex,5] = currentSign
          
          
          # The binomial test goes as:
          #
          # -- How many women are friends with another women? The variable homophily (ie 0.75 , 75%)
          # -- How many women do we have? The total Modality variable (ie: 500)
          # -- How many women are friends with another women, but in integer form? 0.75 * 500 = 375 round to the nearest integer
          # -- What is the probability of being friend with another women just by random chance? The proportion modality (ie 0.5, 50%)
          #
          #    And we do the two sided as we don't care if it is overrepresented or underrepresented, either case is interesting.          
          homoDF[tableIndex,7] = binom.test( totalSameToSameModality,  totalModality  , p = proportionModality, alternative = "two.sided")$p.value

          
          homoDF[tableIndex,8] = getAsterkisPValue(homoDF[tableIndex,7])
          
          
          
          tableIndex = tableIndex + 1

        }

      }
      
      
      return(homoDF)

}
    
# Same, but the explicit nodes table is given because stupid R has no pointers
completeHomophilyV3 <- function(graph, nodesTable, indexList){

    # Init variables
    totalVariables = length(indexList)

    # Get the table
    variablesNames = colnames(nodesTable)[indexList]

    # Count how many uniques for each group
    totalRows = 0
    for(i in 1:totalVariables){

        currentIndex = indexList[i]

        totalRows = totalRows + 1 # +1 per variable

        totalRows = totalRows + length(unique(nodesTable[,currentIndex])) # + X for each category

     }

     # Create the DF where we are going to put the results
     homoDF               =  DF(totalRows,8)
     colnames(homoDF)     = c("Variable", "Homophily", "Frequency", "Delta", "Sign", "Difference", "P-value", "Significance")

     # Fill the table
     tableIndex = 1
     for(i in 1:totalVariables){

        currentIndex      = indexList[i]
        variableName      = variablesNames[i]
        variableHomophily = homophilyV2(graph, variableName)
        variableUniques   = getCategories(nodesTable, currentIndex)
        totalUniques      = length(variableUniques)

        homoDF[tableIndex,1] = variableName
        homoDF[tableIndex,2] = variableHomophily

        tableIndex = tableIndex + 1

        for(j in 1:totalUniques){

          currentModality         = variableUniques[j]
          variableHomophily       = homophilyV2(graph, variableName, currentModality)
          totalModality           = sum(nodesTable[,currentIndex] == currentModality, na.rm = TRUE)
          proportionModality      = totalModality / nrow(nodesTable)
          totalSameToSameModality = round(variableHomophily * totalModality)
          currentSign             = "+"

          homoDF[tableIndex,1] = currentModality
          homoDF[tableIndex,2] = variableHomophily
          homoDF[tableIndex,3] = proportionModality
          homoDF[tableIndex,4] = abs(variableHomophily - (proportionModality))
          homoDF[tableIndex,6] =     variableHomophily - (proportionModality)
          
          if(homoDF[tableIndex,6] <0) currentSign = "-"
          
          homoDF[tableIndex,5] = currentSign
          
          
          # The binomial test goes as:
          #
          # -- How many women are friends with another women? The variable homophily (ie 0.75 , 75%)
          # -- How many women do we have? The total Modality variable (ie: 500)
          # -- How many women are friends with another women, but in integer form? 0.75 * 500 = 375 round to the nearest integer
          # -- What is the probability of being friend with another women just by random chance? The proportion modality (ie 0.5, 50%)
          #
          #    And we do the two sided as we don't care if it is overrepresented or underrepresented, either case is interesting.          
          homoDF[tableIndex,7] = binom.test( totalSameToSameModality,  totalModality  , p = proportionModality, alternative = "two.sided")$p.value

          
          homoDF[tableIndex,8] = getAsterkisPValue(homoDF[tableIndex,7])
          
          
          
          tableIndex = tableIndex + 1

        }

      }
      
      
      return(homoDF)

}    

# Given a Graph and a set of variables that are categorical it gives back summary table
# with only the main variables, no the sub modalities
partialHomophilyV2 <- function(graph, indexList){

    # Init variables
    totalVariables = length(indexList)

    # Get the table
    completeTable  = igraph::as_data_frame(graph, "vertices")
    variablesNames = colnames(completeTable)[indexList]

    # Create the DF where we are going to put the results
    homoDF               =  data.frame(matrix(NA, nrow = totalVariables, ncol = 2))
    colnames(homoDF)     = c("Variable", "Homophily")

    # Fill the table
    for(i in 1:totalVariables){

        currentIndex      = indexList[i]
        variableName      = variablesNames[i]
        variableHomophily = homophilyV2(graph, variableName)

        homoDF[i,1] = variableName
        homoDF[i,2] = variableHomophily

    }

    return(homoDF)

}

# Built in lnam
# This is the Linear Network Autocorrelation Model that comes with the $LIBRARY
# although I have no idea of what it does internally.
#
# You can add the deviance weight matrix too, as this solve the formula
#
#    Yn = W1 * Yn-1 + XB + W2 * e + nu
#
# The rho doesn't appear anywhere here, but somehow you get it back.
#
# Results
#
builtInLNAM <- function(dependentVariablesVector, myExplicativeVariablesDF,
                        friendshipMatrix = NULL, devianceMatrix = NULL,
                        epsilon = 0.01){
      
      # Find the variables dimensions
      totalPeople = nrow(myExplicativeVariablesDF)
      print(totalPeople)
      # Define the matrixes for frienship, devience, and explicative variables
      friendshipW = matrix(0, totalPeople,totalPeople)
      devianceW   = matrix(0, totalPeople,totalPeople)
      explicative = as.matrix(myExplicativeVariablesDF)
      
      if(!is.null(friendshipMatrix)) friendshipW = friendshipMatrix
      if(!is.null(devianceMatrix))   devianceW   = devianceMatrix
      
      # Define the initialization of the rhos, sigmas, betas, nu, e and y0
      # r1    = 0.2
      # r2    = 0.1
      # sigma = 0.1
      # beta  = rnorm(ncol(myExplicativeVariablesDF))
      # nu    = rnorm(totalPeople,0,sigma)     
      # e     = qr.solve(diag(totalPeople),nu) 
      
      r1    = 0.2
      r2    = 0.1
      sigma = 0.1
      beta  = rnorm(ncol(myExplicativeVariablesDF))
      nu    = rnorm(totalPeople,0,sigma)     
      e     = qr.solve(diag(totalPeople),nu) 
      
      # Why is the example giving me Y from W1 and BX????
      # This is true for the first iteration, but this overwrite the y in the code,
      # which makes the variables independent ??????? WHAT????
      #y     = qr.solve(diag(totalPeople) -r1 * friendshipW , explicative %*% beta + e)
      y = dependentVariablesVector
      
      # I need this for now to see how long this takes
      print("Doing the Linear Autocorrelation Model")
      print("I need to do a bunch of matrix multiplication,")
      print(paste0("of size ", totalPeople, ", this is going to take some time"))
      print("I'm not stuck, I'm just slow, please wait...")
      print(paste0("Started at: ", Sys.time()))
      
      
      #Now, fit the autocorrelation model:
      fit = 0
      # -- If we provide W1 and W2
      if(  !is.null(friendshipMatrix)  &&  !is.null(devianceMatrix))
        fit = lnam(y, x = explicative , W1 = friendshipW, W2 = devianceW, tol = epsilon)
      
      # -- If we provide W1
      if(  !is.null(friendshipMatrix)  &&  is.null(devianceMatrix))
        fit = lnam(y,x = explicative ,  W1 = friendshipW, tol = epsilon)
      
      # -- If we provide W2
      if(  is.null(friendshipMatrix)   &&  !is.null(devianceMatrix))
        fit = lnam(y, x = explicative , W2 = devianceW, tol = epsilon)
      
      # -- If we provide nothing, do nothing, and return fit = 0
      
      # I need this for now to see how long this takes
      print(paste0("Finished at: ", Sys.time()))
      
      return(fit)
      
    }
    
    
    # Here comes the function to do the network simulations. In general this is
    # quite straight forward, however we have an special case for a function
    # that we do later.
    #
    # I tried for more than a week to generalize that function into the general
    # case in a simple way, and is simply not possible to make it simple and in
    # a code that is understandable. So for the sake of simplicity, I'm not 
    # making the function generalization, but rather a specific function for a
    # very specific case later on.
    
    # Here come first the general simple cases that are intuitive:
    
    
    # This function does a bunch of simulations and return a vector of size
    # total simulations. Each element of the vector contain how many same to
    # same relationships we have in that simulation.
    #
    # This function doesn't tell you whether the relationships are bias or not
    # it only generate the vector, later on you have to analyize that.
    #
    # There are 3 ways to run the bootstrap vector:
    #
    #     A)
    #
    #        Get the general prevalence. Whatever you give in the categoricalIndex
    #        variable has a prevalence (ie Positive 60% Negative 40%), and that
    #        frequency is apply to everyone in the nodes table.
    #
    #     B)
    #
    #        Get an specific prevalence. You might have an special categorical
    #        variable given by overrideFrequenciesIndex, which has an specific
    #        modality given by overrideFrequencyCategory (ie Sex -> Woman).
    #        This modality has a prevalence likely different from the general
    #        population (ie Positive 60% Negative 40% was the original, but 
    #        women has a Positive 20% and Negative 80%). Then this frequency
    #        is apply to everyone in the table.
    #
    #        The point of this is to compare what would happens if we consider
    #        everybody to have this category. If the result are significant
    #        it means that this modality has a different risk (higher or lower)
    #        than the rest.
    #
    #     C)
    #
    #        Get an specific prevalence. You might have an special categorical
    #        variable given by overrideFrequenciesIndex, which doesn't has an
    #        specific modality given by overrideFrequencyCategory (NULL/NA).
    #
    #        This means that you are going to take each modality (Men, Women)
    #        And apply that prevalence to each element of the node table.
    #        (ie Men Positive 80% Negative 20%, Women Positive 20% and
    #        Negative 80%). Then this frequency is apply to everyone in the table.
    #        accordingly to it modality.
    #
    #
    # tableBase                = which DF do you want
    #
    # tableEdges               = edges with the real relationships
    #                            with "from" , "to" variables, "value" optional
    #
    # categoricalIndex         = For the given DF in tableBase, which column do you
    #                            want to analyze. The column must contain a
    #                            categorical type of variable.
    #                            (ie: SPAType or Carrier Status)
    #
    #                            If you have a numerical variable, you need to
    #                            convert it to categorical first.
    #
    # totalSimulations         = How many simulations do you want to do. As a
    #                            general rule, 1000 simulations is good enough.
    #
    # simulateRelationships    = Whether to use original relationships or simulate
    #                            new random ones. (DEFAULT = FALSE)
    #
    #                            Keeping the same relationships maintain the network
    #                            topology, so you are analyzing if there is a bias
    #                            in the relationships (ie: Do same type of carrier
    #                            are more likely to be friends with each others)
    #
    #                            If you don't keep the relationship, the results
    #                            will have several different meanings depending of
    #                            what you are doing. Consult your local statician
    #                            for more information.
    #                           
    #
    # overrideFrequenciesIndex = Which column are you going to use to build the
    #                            frequency table. The default is the same as
    #                            categoricalIndex, so nothing change by default.
    #
    #                            (ie: Index for Sex, BMI, Smoking, or whatever)
    #
    #                            You might want to do, for example, the analysis
    #                            for smoking, but using the frequency of Sex: MEN
    #                            and Sex: WOMEN, to check whether men or women have
    #                            some sort of bias, higher risk, or whatever.
    #
    # overrideFrequencyCategory = If you want to use the previous variable, I need
    #                             a category to filter by (ie: "Woman", "Yes", "40")
    #                             If you give me an index in the previous variable,
    #                             but this is still NULL (default) I will run
    #                             B2 instead of B1.
    #
    #
    # showProgressBar         =  (String) If not NULL (default), the console will show
    #                            a little progress bar from 0% to 100% telling
    #                            how far we are doing the simulations.
    #
    #                            Beware that the progress bar will clear the console
    #                            of all text.
    #
    #                            If not NULL, you need to add a string here that
    #                            will show up later in console. Recomendation is
    #                            that you give something meaningfull like
    #
    #                            "Doing simulations for school: "
    #
    #                            Is useful to set it to NULL if you use this inside
    #                            another function, of if you don't want to loose the
    #                            text in the console for whatever reason.
    #
    # Return: A vector of size totalSimulations, which how many same to same relationship
    #         where found in each particular simulation
  
    getBootstrapVector <- function(tableBase, tableEdges, categoricalIndex, totalSimulations,
                                   simulateRelationships     = FALSE,
                                   overrideFrequenciesIndex  = NULL,
                                   overrideFrequencyCategory = NULL,
                                   showProgressBar           = NULL){
    
    
        # Prepare the vector with the bootsraps results
        bootstrapVector      = rep(0,totalSimulations)
    
        # Get the basic statistics from the nodes table
        frequencyTable          = summarizeCategorical(tableBase, categoricalIndex)
        totalNodes              = nrow(tableBase)
        
        # Get the basic statistics from the edge table
        totalRelationships      = nrow(tableEdges)
    
        # If you want to override the Frequency table it means that you want
        # to use the categorical index but restricted to only a particular
        # category of another column.
        
        # Cases B and C 
        if(!is.null(overrideFrequenciesIndex)){
      
            # Check that you have a valid category to filter
            #
            # B
            if(!is.null(overrideFrequencyCategory)){
                
                filterTableBase = tableBase[tableBase[,overrideFrequenciesIndex] == overrideFrequencyCategory,]
                frequencyTable  = summarizeCategorical(filterTableBase, categoricalIndex)                    
                
            }
            
            # C There is nothing else to do here, we will fix the frequency
            # tables later as we go.


        }
    
        # Case A
        # This is the default and there is nothing special that need to be done.
        
        # Dataframes we are going to use to run the simulations. In each
        # simulation, we fill the patient table and the relationship table.
        # -- People
        totalPatients           = totalNodes
        patientsTable           = DF(totalPatients, 3)
        colnames(patientsTable) = c("ID", "TargetVariable", "ModalityOverride")
        # -- Relationships
        frienshipDF             = DF(totalRelationships, 4)
        colnames(frienshipDF)   = c("from","to","value","SameRelationship")
    
    
        # For each simulation:
        #
        # ---- Generate the random patient table with their random attributes which 
        #      follows the given frequency rules.
        #
        # ---- Generate the friendship table (or not depending of your function call)
        #
        # ---- Count how many patients that are friends share the same attribute
        
        for (j in 1:totalSimulations) {

            # Print feedback of simulation progress to the user
            if(!is.null(showProgressBar)){

                cat("\014")
                print(showProgressBar)
                print("")
                print(getProgressCharacters((100*j)/totalSimulations))
                                
            }
            
            # Init the patients
            # -- Init the IDs
            patientsTable[,1] = tableBase[,1] 
            # -- Init the random categorical variable by sampling from the pool of
            #    frequencies that you choose in the frequency table
            #    This depends on whether you are in case A, B or C
            #
            #    A and B, we use the frequency table that we already found out
            #    These are the simple case, and the case in which we use the
            #    conditional probability.
            #
            #    C is when we use a frequency table for each modality independently
            #    so we need to adapt to it as we go.
            
            
            # Case A (there is nothing else to do, this case is finish).
            if(is.null(overrideFrequenciesIndex)){
                
                patientsTable[,2] = sample(frequencyTable$Modality, size = totalPatients, replace = TRUE, prob = frequencyTable$Relative)    
                
            }
            # Case B and C
            else{
                
                # B (also, there is nothing else to do, this is also a simple case)
                if(!is.null(overrideFrequencyCategory)){
                        
                    patientsTable[,2] = sample(frequencyTable$Modality, size = totalPatients, replace = TRUE, prob = frequencyTable$Relative)    
                        
                }
                # C (this is a bit more complicated)
                else{
                    
                    # Give each patient it original modality for the override
                    # (ie, same sex, same categorical BMI, whatever)
                    patientsTable[,3] = tableBase[,overrideFrequenciesIndex] 
                
                    # Find out how many modalities are in the override index
                    myModalities    = getModalities(tableBase, overrideFrequenciesIndex)
                    totalModalities = length(myModalities)
                
                    # Here we create a matrix of samples.
                    # There is one row for each modality of the target category
                    sampleMatrix    = newList(totalModalities)
                    for(i in 1:totalModalities){
                        
                        # Init the row
                        sampleMatrix[[i]] = rep(NA, totalPatients)
                            
                        # Get the modality we are studying
                        currentModality = myModalities[i]
                            
                        specialFilterTable    = tableBase[tableBase[,overrideFrequenciesIndex] == currentModality,]
                        specialFrequencyTable = summarizeCategorical(specialFilterTable, categoricalIndex)                    
                                    
                        sampleMatrix[[i]] = sample(specialFrequencyTable$Modality, size = totalPatients, replace = TRUE, prob = specialFrequencyTable$Relative)     
                             
                    }
                
                    # For each patient, check the modality and apply the
                    # proper sample.
                    for(i in 1:totalPatients){
                            
                        # Get the patient modality
                        currentPatientModality = patientsTable[i,3]
                        
                        # Find that modality in the sample matrix
                        currentModalityIndex   = grep(TRUE, (currentPatientModality == myModalities))
                            
                        # Assign the sample value from the sample matrix
                        patientsTable[i,2] = sampleMatrix[[currentModalityIndex]][i]
                            
                    }                    

                }
                
            }

            # We have now simulated each status of each patient.
            # Now we need to figure it out how many same-to-same relationships
            # are in the simulation.

            # For each relationship
            # -- Simulate a new relationship if needed
            # -- Check if the have the same category.
            for (i in 1:totalRelationships) {
        
                # Get the real relationship From and To values
                # Right here, despise variable name, they are not random yet
                randomFrom = tableEdges[i,1]
                randomTo   = tableEdges[i,2]
        
                # If you need the simulation of relationship, simulate random from and to
                if(simulateRelationships == TRUE){
          
                    # Take a random from and to, that are not the same, and are not already in the list
          
                    # -- Pick the first one
                    candidatesList = patientsTable$ID
                    randomFrom     = sample(candidatesList, 1)
          
                    # -- Take away the first one from the candidates list as you are not suppose to have a relationship with yourself
                    candidatesList = candidatesList[candidatesList != randomFrom]
          
                    # -- Pick the second one
                    # ---- You already have a list of relationships that start with FROM
                    # ---- This list could be empty though
                    # ---- In any case, grab the TOs from that list
                    # ---- Those are the forbidden numbers that you need to take away from the candidate list
                    forbiddenTos = frienshipDF[frienshipDF$from == randomFrom,2]
          
                    # ---- Update the candidate list if there is one or more forbiddens
                    if (length(forbiddenTos) > 0){
            
                        candidatesList = candidatesList[!(candidatesList %in% forbiddenTos)]
            
                    }
          
                    # ---- Finally pick the second one
                    randomTo = sample(candidatesList, 1)
          
                }
        
                # Set this particular relationship into our friendship DF
                frienshipDF$from[i]  = randomFrom
                frienshipDF$to[i]    = randomTo
                frienshipDF$value[i] = 1
        
                # Check if they share same target variable
                targetTypeFrom = patientsTable[patientsTable[,1] == randomFrom,2]
                targetTypeTo   = patientsTable[patientsTable[,1] == randomTo,  2]
            
                # Sometimes you get a random number that doesn't work, is very weird but the bug is there, I'm trying to catch it with this
                # (but It haven't show in sometime so I think is fixed)
                if(is.na((targetTypeFrom == targetTypeTo))){
          
                    print("ALERT!")
                    print(targetTypeFrom)
                    print(targetTypeTo)
                    print(randomFrom)
                    print(randomTo)
                    print(i)
          
          
                }
        
                # Label if both targets have the same category
                frienshipDF$SameRelationship[i] = (targetTypeFrom == targetTypeTo)
        
            }
      
            # Add how many have the same relationship to the result of this simulation
            bootstrapVector[j] = sum(frienshipDF$SameRelationship)
      
        }
    
        return(bootstrapVector)
    
  }  
    
    
    # This function tells you if your relationships are bias or not
    # Only for categorical variables. It can run the version A, B and C,
    # of the Bootstrap vector.
    #
    # In order to select each version, fill the input variables accordingly.
    #
    # nodesTable    = Dataframe with the information about your nodes
    #                 It can have any structure you want, the only restriction is
    #                 the first column must be the ID column, and it must be
    #                 a numerical ID
    #
    #
    # listOfEdgesDF = List of Dataframes. Each dataframe have a different network
    #                 The dataframe structure goes like this:
    #
    #                 from     to     value 
    #                    1      2         3
    #                    2      3         5
    #                          ...
    #
    #                The "value" column is irrelevant for this analysis, but
    #                is the standard way to save the edges in a network.
    #
    # listOfNetworkNames = List of Strings with the name of each network. The
    #                      default value is NULL and it will be named from 1 to 
    #                      X. Otherwise, if you want proper names, give them to
    #                      the function in this paremeter.
    #
    #
    # listOfConsequenceIndexes = List of Indexes with the variables that you want
    #                            to study. The variables must be categorical, and
    #                            the indexes must be a valid index contain within
    #                            the nodeTable.
    #
    # totalSimulations         = How many simulations you want to run
    #                            There is no default, 10 is good for testing
    #                            And 1000 is good for getting results.
    # 
    # Return:
    #
    # 
    # A list of dataframes with the following columns
    #
    #     "Network"               Name of the Network
    #     "Total Relationships",  How many relationships we have
    #     "Equal Relationships",  How many same to same relationships we have
    #  
    #     "MIN",                  Minimum same to same relationships count that we found in the simulation
    #     "Q1",                   Percentile 25 of same to same relationships count that we found in the simulation
    #     "Median",               Median same to same relationships count that we found in the simulation
    #     "Average",              Average same to same relationships count that we found in the simulation
    #     "Q3",                   Percentile 75 same to same relationships count that we found in the simulation
    #     "MAX",                  Maximum same to same relationships count that we found in the simulation
    #     "SD",                   Standard Deviation same to same relationships count that we found in the simulation
    #
    #     "ConsequenceIndex"      The actual name of this column change depending of the name of each consequence index.
    #                             In here we have the actual p-value for each network, on whether your relationship
    #                             is bias or not.
    # 
    doCategoricalBiasAnalysis <- function(nodesTable, listOfEdgesDF,
                                          listOfConsequenceIndexes,
                                          totalSimulations,
                                          overrideFrequenciesIndex  = NULL,
                                          overrideFrequencyCategory = NULL,
                                          listOfNetworksNames = NULL){

        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
        }
        
        # Prepare the blank base DF where we write the results.
        # We will have one of this for each consequence index.
        biasSimulationsDF           =  data.frame(matrix(NA, nrow = totalNetworks, ncol = 10 + 1 ))
        colnames(biasSimulationsDF) = c("Network", "Total Relationships", "Equal Relationships", "MIN", "Q1", "Median", "Average", "Q3", "MAX", "SD",  "ConsequenceIndex"  )
        for(i in 1:totalNetworks){
            biasSimulationsDF[i,1]  = myNetworkNames[i]
        }
        
        # We have a list of result for each of the consequence index
        # So in here we prepare such list, and give a blank DF to each
        biasResultsList =  newList(totalConsequenceIndexes)
        for( i in 1:totalConsequenceIndexes){
      
            # Init the DF to empty
            biasResultsList[[i]] = biasSimulationsDF

            # Change the name of the variable we are interested in for this DF
            colnames(biasResultsList[[i]])[11] = consequenceNames[i]

        }

        # For each of the consequence index, we do these 1000-ish simulation for
        # each of the networks that you have.
        for (i in 1:totalConsequenceIndexes){
      
            # Get the DF where we save the results for this variable
      
            # R is stupid, why can't I pass a reference? why do I need to use an index
            # here when then an alias to the variable would make everything more
            # readable and efficient??? >:[
            # currentDF = biasResultsList[[i]]
      
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[i]
                currentName  = consequenceNames[i]        
            }

            # For each of the network, do the proper bias analysis
            # For each network ( each network is represented by an edges DF)
            for(j in 1:totalNetworks){
                    
                    # Get the current edges
                    currentEdges = listOfEdgesDF[[j]]
                    
                    # Find out the if "from" - "to" have the same relationship
                    currentEdges$SameRelationship = addEdgeRelationship(currentEdges,  nodesTable, currentIndex)
                    
                    # Find out how many relationships we have
                    currentTotalRelationships          = nrow(currentEdges)
                    
                    # Find out how many same to same relationships we have
                    # This is the real value that we use in the p-value calculation
                    currentTotalSameRelationships      = sum(currentEdges$SameRelationship == TRUE)
                 
                    # Check if carrier have bias friendship towards people with the same carrier status
                    #
                    # -- Prepare the custom message 
                    currentWaitingMessage = paste0( "Doing simulations for ", myNetworkNames[j], " please wait..." )

                    # -- Do the bias analysis
                    currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentIndex, totalSimulations,
                                                           overrideFrequenciesIndex  = overrideFrequenciesIndex,
                                                           overrideFrequencyCategory = overrideFrequencyCategory,
                                                           showProgressBar           = currentWaitingMessage)
                    
                    
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write all this info in the appropriate part of the results
                    biasResultsList[[i]][j,  2 ] = currentTotalRelationships
                    biasResultsList[[i]][j,  3 ] = currentTotalSameRelationships
                    biasResultsList[[i]][j,  4 ] = min(currentBiasResult)
                    biasResultsList[[i]][j,  5 ] = as.integer(summary(currentBiasResult)[2])
                    biasResultsList[[i]][j,  6 ] = median(currentBiasResult)
                    biasResultsList[[i]][j,  7 ] = currentAverage
                    biasResultsList[[i]][j,  8 ] = as.integer(summary(currentBiasResult)[5])
                    biasResultsList[[i]][j,  9 ] = max(currentBiasResult)
                    biasResultsList[[i]][j, 10 ] = currentSD
                    biasResultsList[[i]][j, 11 ] = currentPValue
                       
                }

            
        }
        
                
        # Everything is finish, give back the list of biases and close.
        return(biasResultsList)
        
        
    }
    
    
    # This function tells you if your relationships are bias or not with respect
    # each modality of each categorical variable.
    #
    # It also find the confident interval
    #
    # Only for categorical variables (obviously). It compares the simulations
    # against the results A from the categorical bias function
    #
    # Return
    #
    # 
    # A list of dataframes with the following columns
    #
    #     "Variable"
    #     "Modality"
    #     "Index"
    #     "Network"
    #     "Real Relationships"
    #     "Real Same to Same"
    #     "Simulated Unbias Average Same"
    #     "Simulated Unbias Minimum Same"
    #     "Simulated Bias Average Same"
    #     "Simulated Bias SD"
    #     "Target Variable"
    #     "Base Risk"
    #     "Low CI"
    #     "High CI"
    doModalityBiasAnalysis <-function(nodesTable, listOfEdgesDF,
                                      listOfConsequenceIndexes,
                                      listOfExplanatoryIndexes,
                                      totalSimulations,
                                      listOfNetworksNames = NULL,
                                      confidentValue = 95){
        
        
        # Give some alias to variables, get easy to read the code
        {
        
            consequenceIndexes = listOfConsequenceIndexes
            explanatoryIndexes = listOfExplanatoryIndexes
                
        }
        
        # Get the basic information for the given variables
        {
        
            # Network information
            #
            # How many networks do we have
            totalNetworks = length(listOfEdgesDF)
            # Get the network names or prepare some random ones
            myNetworkNames = as.character(c(1:totalNetworks))
            if(!is.null(listOfNetworksNames)) myNetworkNames = listOfNetworksNames
            
            # Dependent variables information
            #
            # How many variables are we going to study
            totalConsequenceIndexes = length(consequenceIndexes)
            # The name of each 
            consequenceNames = colnames(nodesTable)[consequenceIndexes]
            
            # Independent variables information
            #
            # How many variables are we going to study
            totalExplanatoryIndexes = length(explanatoryIndexes)
            # The name of each 
            explanatoryNames = colnames(nodesTable)[explanatoryIndexes]
            
        }
        
        # Prepare the z-score for the given confident interval value
        confidentMargin = qnorm( 1 - ( (1 - ( confidentValue / 100 ) ) / 2 ) )
            
        # Prepare the list where we are going to write the results
        simulationByVariablesDFList = newList(totalConsequenceIndexes)
      
        # Create a blank DF that we are going to copy inside each element of the list
        # -- Count how many rows do we have
        totalModalities = 0
        for (i in 1:totalExplanatoryIndexes) {
        
            explanatoryModalities = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            totalModalities       = totalModalities + length(explanatoryModalities)
        
        }
        # -- Create the dataframe
        blankSimulationResultsDF           = DF(totalModalities, 14)
        colnames(blankSimulationResultsDF) = c("Variable", "Modality", "Index", "Network",
                                               "Real Relationships", "Real Same to Same",
                                               "Simulated Unbias Average Same", "Simulated Unbias Minimum Same",
                                               "Simulated Bias Average Same",   "Simulated Bias SD",
                                               "Target Variable", "Base Risk",  "Low CI", "High CI")
        # -- Init the basic columns
        importantIndex = 1
        for (i in 1:totalExplanatoryIndexes) {
        
            # Get the name of the variable
            currentCategory        = explanatoryNames[i]
            
            currentModalities = NA
            if(is.null(levels(nodesTable[,explanatoryIndexes[i]])))
                currentModalities      = as.character(unique(nodesTable[,explanatoryIndexes[i]]))
            else
                currentModalities      = as.character(levels(nodesTable[,explanatoryIndexes[i]]))            
            
            totalCurrentModalities = length(currentModalities)
            
            # For each modality, add it to the dataframe
            for (j in 1:totalCurrentModalities) {
          
                blankSimulationResultsDF[importantIndex,1] = currentCategory
                blankSimulationResultsDF[importantIndex,2] = currentModalities[j]
                blankSimulationResultsDF[importantIndex,3] = explanatoryIndexes[i]
          
                importantIndex = importantIndex + 1
            }
        
        }

        # For each consequence variable and each network, we are going to need
        # to Simulate the bias analysis, and get the bias average. We do that
        # now, and save it for later
        biasResultsDF = doCategoricalBiasAnalysis (nodesTable, listOfEdgesDF,
                                                   consequenceIndexes,
                                                   totalSimulations,
                                                   listOfNetworksNames = myNetworkNames)

        # Everything is initialized at this point.
        #
        # We are going to get A LOT of results from this function.
        #
        # For each consequence index, we have several networks, and for each
        # network we have a huge dataframe with all the modalities.        
        
        # Get the starting time so we can tell aprox. how much to finish
        startTime = Sys.time()
        
        
        # For each consequence variable...
        for (j in 1:totalConsequenceIndexes) {
            
            # Get index and variable name
            {
                currentIndex = consequenceIndexes[j]
                currentName  = consequenceNames[j]        
            }
            
            # Give feedback to the user
            print("----------------------------")
            print( paste0(" Doing index: ", currentName, " ", round(100*j / totalConsequenceIndexes ,2), "%" ))
            print("----------------------------")
            
            # Prepare the network list
            simulationByVariablesDFList[[j]] = newList(totalNetworks)

            # For each of the network...
            for(k in 1:totalNetworks){

                # Prepare the blank dataframe
                simulationByVariablesDFList[[j]][[k]] = blankSimulationResultsDF
                                    
                # Get the current edges
                currentEdges = listOfEdgesDF[[k]]
                currentNetwork = myNetworkNames[k]
                
                # Get the index and name
                currentConsequenceIndex = consequenceIndexes[j]
                currentConsequenceName  = consequenceNames[j]
      
                # Tell the user where are you 
                print("------------------------")
                print( paste0(" Doing network: ", currentNetwork, " ", round(100*k / totalNetworks ,2), "%" ))
                print("------------------------")
                
                # Get the results from the pre-simulations, these are the
                # unbias values and the total relationships values
                currentTotalRelationships     = biasResultsDF[[j]][k,2]
                currentTotalSameRelationships = biasResultsDF[[j]][k,3]
                currentUnbiasSameMinimum      = biasResultsDF[[j]][k,4]
                currentUnbiasSameAverage      = biasResultsDF[[j]][k,7]
                
                # For each row in the table
                for(i in 1:totalModalities){

                    # Get the variable index and modality name
                    currentVariableName  = simulationByVariablesDFList[[j]][[k]][i,1]
                    currentModalityName  = simulationByVariablesDFList[[j]][[k]][i,2]
                    currentVariableIndex = simulationByVariablesDFList[[j]][[k]][i,3]
                    
                    # Give some feedback to the user
                    # -- Time to finish
                    currentTime          = Sys.time()
                    secondsFromStart     = as.numeric(currentTime-startTime,units="secs")
                    proportionFromStart  = ((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)
                    percentagePerSecond  = proportionFromStart / secondsFromStart
                    secondsToFinish      = ((1 - proportionFromStart) * secondsFromStart) / proportionFromStart
                    # -- String to user
                    print("--------------------")
                    print( paste0(" Doing row: ", currentModalityName, " / ", currentVariableName, " - " ,  round(100 * i / totalModalities ,2), "%" ))
                    print("--------------------")
                    print( paste0(" Total: ",   round(100 * (((j-1)*totalNetworks*totalModalities + (k-1)*totalModalities + i) / (totalConsequenceIndexes * totalNetworks * totalModalities)) , 2),  "%" ))
                    print( paste0(" Finish in: ", secondsToFinish))
                    print("--------------------")

                    # Get the bootstrap vector (version B)
                    currentBiasResult = getBootstrapVector(nodesTable, currentEdges, currentConsequenceIndex, totalSimulations,
                                                           overrideFrequenciesIndex  = currentVariableIndex,
                                                           overrideFrequencyCategory = currentModalityName)

                    # Get how much is the average and sd of the simulations
                    currentAverage    = mean(currentBiasResult)
                    currentSD         = sd(currentBiasResult)
                    currentPValue     = pnorm(currentTotalSameRelationships,
                                              mean = currentAverage,
                                              sd   = currentSD, 
                                              lower.tail = FALSE)

                    # Write everything into the current results dataframe
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  4 ] = currentNetwork
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  5 ] = currentTotalRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  6 ] = currentTotalSameRelationships
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  7 ] = currentUnbiasSameAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  8 ] = currentUnbiasSameMinimum
                    simulationByVariablesDFList[[ j ]][[ k ]][ i ,  9 ] = currentAverage
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 10 ] = currentSD
                    simulationByVariablesDFList[[ j ]][[ k ]][ i , 11 ] = currentPValue
                    
                }

            }
            
            
        }        

        # Everything is simulated at this point.
        #
        # Now that this is finish, we have the base value for each modality
        # and we can run the CI columns in each result. So we do the same run
        # again, for each consequence index, for each network, for each row:
        
        for (j in 1:totalConsequenceIndexes){
            
            for(k in 1:totalNetworks){
                
                # Get the default base values
                currentReferenceIndex = simulationByVariablesDFList[[j]][[k]][1,3]
                currentBaseValue      = simulationByVariablesDFList[[j]][[k]][1,9]
                currentOrderValue     = 1
                
                
                for(i in 1:totalModalities){
                    
                    # Compare the average with the base value, if they are the same, this is the reference and is equal to 1 
                    simulationByVariablesDFList[[j]][[k]][i,12] = simulationByVariablesDFList[[j]][[k]][i,9] / currentBaseValue
        
                    # Get the lower and upper intervals for a CI of 95%
                    # -- If you are the base value, you don't have a confident interval
                    if(currentOrderValue == 1){
                        simulationByVariablesDFList[[j]][[k]][i,13] = NA
                        simulationByVariablesDFList[[j]][[k]][i,14] = NA
                    }
                    # -- Every other modality of that categorical variable, has a CI
                    else{
                        simulationByVariablesDFList[[j]][[k]][i,13] = ( (-confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                        simulationByVariablesDFList[[j]][[k]][i,14] = ( (+confidentMargin * simulationByVariablesDFList[[j]][[k]][i,9]) + simulationByVariablesDFList[[j]][[k]][i,9]) / currentBaseValue
                    }
          
                    # Now go to the next row, but we need to check if we are still
                    # studying modalities of the same variable, or if we have jump
                    # to another different variable.
        
                    # Is there a next index?
                    # -- If yes...

                    if(i < totalModalities){
                        # Which one?
                        nextReferenceIndex = simulationByVariablesDFList[[j]][[k]][(i+1),3]
                        
                        # Is the same as the current?
                        # -- If yes...
                        #    Take the next order
                        if(currentReferenceIndex == nextReferenceIndex){
                            
                            currentOrderValue = currentOrderValue + 1
            
                        }
                        # -- If no...
                        #    Change the base value and reference index, reset order to 1
                        else{
                            
                            currentOrderValue     = 1
                            currentReferenceIndex = nextReferenceIndex
                            currentBaseValue      = simulationByVariablesDFList[[j]][[k]][(i+1),9]
            
                        }
          
                    }
                    # -- If no... we are finish
                    
                    
                }
                
            }
            
        }
        
        
        # Finally, everything is done, we just return the final list
        return(simulationByVariablesDFList)

        
    }



