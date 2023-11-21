
# ------------------------------------------------
# PLOTS FOR NETWORKS RELATED IMAGES
# ------------------------------------------------

# edgesDF - A dataframe for the edges. It has this form:
#           
#                 from to value Category 0 ... Category N
#           315   315  1     1        TRUE     Modality_A
#           475   475  1     1       FALSE     Modality_B
#           1042  24   2     1       FALSE             NA
#           1094  76   2     1        TRUE     No modality
#           1687  669  2     1       FALSE     Modality_A
#           2474  438  3     1       FALSE     Modality_C
#
#           from and to is the ID of the nodes that you can find in nodesDF
#           All the from to to edges IDs must be in the nodesDF, but you might
#           have nodes without edges (which is perfectly normal)
#
#           Everything else is not mandatory, but is necessary if you want to
#           highlight something special about the edges (see later)
#
# nodesDF - A dataframe with the nodes. It has this shape:
#
#              ID  Age    Sex    BMI     School 
#           1  1    16  Woman  20.18 Vocational
#           2  2    16  Woman  18.08 Vocational
#           3  3    17    Man  18.10    General
#           4  4    16  Woman  21.82    General
#           5  5    16  Woman  24.26    General
#           6  6    16  Woman  18.84 Vocational
#
#           Only the ID is mandatory, the rest you only need it if you want
#           to highlight something special about the nodes (see later)
#
# (string) folderPath: A string to the folder where you want to save the image
#
# (int)    highlightVariable: The index column of the nodesDF that you want to
#                             use to color the nodes in the graph plot. If you
#                             leave to NULL, the dots will be black by default.
#
# List<String>  colorVectorHighlight: If you choose a highlight variable, you
#                                     might choose the color vector for that
#                                     variable. If you choose a categorical
#                                     variable the color vector must have a size
#                                     equal to the number of categories.
#
#                                     ie: c("#FFFFBF", "#ABDDA4", "#FDAE61",
#                                           "#D7191C", "#7F7F7F")
#
#                                     This is a color vector for BMI, for
#                                     categories "Underweight", "Healthy",
#                                     "Overweight", "Obese" and "Unknown".
#
#                                     If your nodesDF follows a factor scheme
#                                     the color vector will also follow that
#                                     color scheme order.
#
# (int) sizeVariableIndex: Which column from nodesDF shall be use as a size for
#                          each of the nodes in the plot. For example, you
#                          could use the number of connection to that node.
#
#                          If nothing is given, the default size is 0, and all
#                          the nodes are plotted as dots.
#
# (int) rimVariable: Each node in the plot has a black rim around it. You can
#                    change that and color the rim based on any variable you
#                    want. Is not recommended as the plot tend to get very
#                    convoluted, but you can use it nevertheless.
#
# List<String> colorVectorRim: The list of color for the rim.
#
# (int) edgesHighlight: Which column from edgesDF do you want to use to
#                       highlight the edges.
#
# List<String>  colorVectorEdge The list of color for the edges.

# (bool) directedPlot: If you want your plot to be directed. FALSE by default.
#                      If you do, expect arrows marker in each edge.
#
# (String) selectedLayout: Which layout do you want to use. The default is
#                         'mds'. The possible layouts are:
# 
#                         'manual' - Mark this if you want to use your own
#                                    layout (see next)
#
#                         'gem',
#                         'dh',
#                         'graphopt',
#                         'mds' = multidimensional scaling, it tries to keep a balance in between everything.
#                         'fr' = Fruchterman - Reingold , it keeps related vertices toguether
#                         'kk' = Kawai - Emphases distance as information
#                         'drl'
#                         'lgl'
#
#
# (dataFrame) manualLayout: If you want to give your own layout for the nodes.
#                           First you need to select 'manual' in selectedLayout
#                           argument, otherwise this variable will be ignored
#                           and do a MDS by default.
#
#                           The dataframe has this shape, where each row
#                           correspond to each node in nodeDF
#                           
#                                 x        y
#                           1     23.87218 2.307090
#                           2     23.87183 2.322370
#                           3     22.63146 2.821012
#                           4     20.87682 1.066371
#                           5     22.63123 2.833566
#                           6     23.87079 2.337620
#
# (bool) suppressAloneNode: If you want to not show nodes that aren't connecting
#                           to anything. Default is FALSE.
#
# (bool) savePDF:           PDFs might take too long to be generated even for
#                           small networks. So you you need to say explicitely
#                           that you want the vectorial image to be save.

doGraphPlot <- function(edgesDF, nodesDF, folderPath,
                        highlightVariable = NULL, colorVectorHighlight = NULL,
                        sizeVariableIndex = NULL,
	                    defaultNodeSize   = 2,
                        rimVariable = NULL,       colorVectorRim = NULL,
                        edgesHighlight = NULL,    colorVectorEdge = NULL,
                        edgesThickness = 1,       edgesAlpha = 0.2,
                        directedPlot = FALSE,
                        selectedLayouts = NULL,   manualLayout = NULL,
                        plotTitle = NULL,         plotSubtitle = NULL,
						plotCaption = NULL,       plotXLabel = NULL, plotYLabel = NULL,
						plotTheme = NULL,
                        overrideLegendSize = NULL,
                        overrideLegendPosition = NULL,
                        overrideTableName = NULL,
                        overrideCaption = NULL,
	                    overrideImageWidth = 0,
	                    overrideImageHeight = 0,
                        suppressAloneNode = FALSE,
						savePDF = TRUE){
  
    # Get the name of the variable where the edges and nodes come from
    edgesNames    = deparse(substitute(edgesDF))
    nodesNames    = deparse(substitute(nodesDF))
    
    # Get the theme information
    themeData = getThemeParameters(plotTheme)
      
    # Init file name
    {
    	myPlotType  = "GraphPlot"
        myTableName = ""
        if(is.null(overrideTableName)){
        	myTableName = paste(edgesNames,"_",nodesNames, sep="")
        }
        else{
        	myTableName = overrideTableName
        } 
	}
    
    # Add a column where we are going to store the size information
    # If we have a size variable, init to that instead.
    {
    	nodesDF$FinalSize = defaultNodeSize
        if(!is.null(sizeVariableIndex)) nodesDF$FinalSize = nodesDF[,sizeVariableIndex]/3
    }

      # You might have edges between nodes that are not in the node list
      # Delete the edges that doesn't appear in the nodes
      # Also give a warning to the user
      {
        notInNodesEdgesSet = union( setdiff(edgesDF$from, nodesDF$ID) , setdiff(edgesDF$to, nodesDF$ID) )
        totalMissingNodes  = length(notInNodesEdgesSet)
        # ---- The warning
        if(totalMissingNodes > 0){
          
          print("WARNING: Some of the provided edges are not in the provided nodes")
    
          totalEdges    = nrow(edgesDF)
          keepTheseRows = rep(TRUE, totalEdges)
          
          for (i in 1:totalEdges) {
            
            fromHere = edgesDF$from[i]
            toHere   = edgesDF$to[i]
            
            keepTheseRows[i] = !( (sum(fromHere == notInNodesEdgesSet) > 0)  ||  (sum(toHere == notInNodesEdgesSet) > 0) )
            
          }
          
          print("Previous total")
          print(totalEdges)
          
          edgesDF = edgesDF[keepTheseRows,]
          
          print("New total")
          totalEdges    = nrow(edgesDF)
          print(totalEdges)
          
        }    
      }
    
      # If you want to not show independent nodes
      if(suppressAloneNode == TRUE){
        
	    	# Get all the nodes ID
	        nodesIDs = nodesDF$ID
	        
	        # Get all nodes that connects to something
	        fromEdgesID    = unique(edgesDF$from)
	        toEdgesID      = unique(edgesDF$to)
	        nodesConnected = unique(append(fromEdgesID,toEdgesID))
	        
	        # Find the difference and delete
	        validNodes     = nodesIDs %in% nodesConnected
	        
	        # Delete what is not valid
	        nodesDF = nodesDF[validNodes,]
	        
	        print(paste("Not showing", sum(!validNodes), "nodes"))
        
      }

      # Grab the variables names from the nodes dataframe
      {
	        myVariables   = colnames(nodesDF)
	        
	        highlitedName = NULL
	        rimName       = NULL
	        edgesName     = NULL

	        highlighColumn = NULL
	        rimColumn      = NULL
	        sizeColumn     = NULL
	        edgeColumn     = NULL
	        
	        nCategoriesHighlight = NULL
	        nCategoriesRim       = NULL
	        nCategoriesEdges     = NULL
	        	        
	        if(!is.null(highlightVariable)) highlitedName = myVariables[highlightVariable]
	        if(!is.null(rimVariable))       rimName       = myVariables[rimVariable]
	        if(!is.null(edgesHighlight))    edgesName     = colnames(edgesDF)[edgesHighlight]
	        
	        if(!is.null(highlightVariable)) highlighColumn = nodesDF[,highlightVariable]
	        if(!is.null(rimVariable))       rimColumn      = nodesDF[,rimVariable]
	        if(!is.null(edgesHighlight))    edgeColumn     = edgesDF[,edgesHighlight]
	        sizeColumn = nodesDF$FinalSize
	        
	        if(!is.null(highlightVariable)) nCategoriesHighlight = length(unique(nodesDF[,highlightVariable]))
	        if(!is.null(rimVariable))       nCategoriesRim       = length(unique(nodesDF[,rimVariable]))     
	        if(!is.null(edgesHighlight))    nCategoriesEdges     = length(unique(edgesDF[,edgesHighlight]))
      }
    
      # Prepare the defaults text and color schemes
      {
      	
	        defaultVector         = getCategoricalNetworkDefaults(highlitedName, rimName, nCategoriesHighlight, nCategoriesRim, colorVectorHighlight, colorVectorRim,
	                                                              plotTitle, plotSubtitle, plotCaption, plotXLabel, plotYLabel)
	        print(colorVectorHighlight)
	        colorVectorHighlight  = defaultVector[[1]]
	        colorVectorRim        = defaultVector[[2]]
	        plotTitle             = defaultVector[[3]][1]
	        plotSubtitle          = defaultVector[[4]][1]
	        plotCaption           = defaultVector[[5]][1]
	        plotXLabel            = defaultVector[[6]][1]
	        plotYLabel            = defaultVector[[7]][1]    
	      
	        # If you have edges highlighted by something, but you don't give a color for
	        # them, init the vector color to standard palette and give a warning to the
	        # user.
	        if(!is.null(edgesHighlight) &&  is.null(colorVectorEdge) ){
	    
		          print("WARNING! Doing a graph plot, you told me to highlight by edge")
		          print("but you didn't gave me the colors. I'm initializing the colors")
		          print("to something, but you should check this manually.")
		          
		          myPalette       = colorRampPalette(brewer.pal(5, "Spectral"))
		          colorVectorEdge = myPalette(nCategoriesEdges)
	          
	        }
          
      }

      # Check which layouts are you going to use
      {
	        # Default inits
	        doingManual  = FALSE  
	        graphLayouts = 'mds'
	        # Check that we have a selected layout
	        if( !is.null(selectedLayouts) ){
	          
		          # If you selected manual, check that we have an actual layout
		          if(selectedLayouts == 'manual'){
		              
		              # If you are suppose to do manual, but you don't give me anything
		              # give a warning and do mds instead
		              if( is.null(manualLayout) ){
		                
		                print(" WARNING!! ")
		                print(" You selected a manual layout but didn't gave any coordinates")
		                print(" I'm doing the graph anyway but with a mds layout instead")
		                
		              }
		              # If you selected manual, and give an actual valid layout, flag that
		              # we are going to do a manual layout.
		              else{
		                graphLayouts = 'manual'
		                doingManual  = TRUE
		              }
		            
		          }
		          
		          # If you selected something else, do nothing and trust (lol) that the user
		          # has selected a valid layout option. If not, the function will gives an
		          # error on running time.
		          else{
		        	   graphLayouts = selectedLayouts
		          }
	          
	        }
	        # If the selected layout is null, check that we don't have a manual layout
	        else{
		          # If it is not null, then we default to manual
		          if(!is.null(manualLayout)){
		              graphLayouts = 'manual'
		              doingManual  = TRUE
		          }
	          
	        }
    
      }

      # Prepare the filenames, paths, and so on
      {
       
	        # Add GRAPH + highlight name, rim name, edge name, layout name (if any of these)
	        fileName = paste0("Graph_",  myTableName, "_")
	        if(!is.null(highlitedName)) fileName = paste0(fileName, highlitedName, "_")
	        if(!is.null(highlitedName)) fileName = paste0(fileName, rimName,       "_")
	        if(!is.null(highlitedName)) fileName = paste0(fileName, edgesName,     "_")
	        fileName = paste0(fileName, graphLayouts)
	        
	        # Get the filenames for the plot in raster, vectorial, and the latex
	        imgFileName   = paste0(fileName, ".png")
	        vecFileName   = paste0(fileName, ".pdf")
	        #latexFileName = paste0(fileName, ".tex")
	        
	        # Remove all the spaces from the names if any, latex don't like that
	        imgFileName   = str_replace_all(string = imgFileName,   pattern=" ", repl="")
	        vecFileName   = str_replace_all(string = vecFileName,   pattern=" ", repl="")
	        
	        #latexFileName = str_replace_all(string=latexFileName, pattern=" ", repl="")
	        
	        # Make the final file path.
	        imgFilePath   = file.path(paste(folderPath, imgFileName,   sep = ""))
	        vecFilePath   = file.path(paste(folderPath, vecFileName,   sep = ""))
	        #latexFilePath = file.path(paste(folderPath, latexFileName, sep = ""))
        
      }
		
      # Create the graph object and start plotting
      {
	        # Graph object
	        myGraph = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot)    
	        
	        # Base plot object
	        myPlot = NA
	        
	        # Init the plot depending on whether you are doing a manual or normal layout
	        # manualLayout is given as an argument and we trust the user that everything is right
	        if(doingManual == FALSE) myPlot = ggraph(myGraph, layout = graphLayouts)
	        if(doingManual == TRUE)  myPlot = ggraph(manualLayout)
	        
	        # Add the edges
	        # -- If you didn't gave any hightlight variable
	        if( is.null(edgesHighlight)) {
	          
	          myPlot = myPlot + geom_edge_link0(edge_alpha = edgesAlpha)
	          
	        }
	        # -- If you want a highlight variable
	        else{
	    
	          
		          edge_legend_title = edgesName
		          
		          myPlot = myPlot + geom_edge_link(alpha      = edgesAlpha,
		                                           width      = edgesThickness,
		                                           aes(colour = edgesDF[,edgesHighlight])) +
		            
		                            scale_edge_color_manual(edge_legend_title, values = colorVectorEdge)
	          
	        }
	        print("AA")
	        print(colorVectorHighlight)
	        # Add the nodes and the rim of the nodes.
	        # This part is a bit weird because you can't do that independent from each
	        # other, so you have 4 possible combinations
	        {
	          
	          # If you have a fill, you may or may not have a rim
	          if(!is.null(highlightVariable)){
	            
	              # If the variable is categorical, proceed as usual
	          	  if(getCategoricalVariables(nodesDF)[highlightVariable] == TRUE){
	          		
	        	      # -- Fill, no rim
	                  if(is.null(rimVariable)){
	              
		                  myPlot = myPlot +
		                           geom_node_point(aes(fill = highlighColumn), size = sizeColumn, stroke = 1, shape = 21) +
		                           scale_fill_manual(values = colorVectorHighlight)
	              
	                  }
	            
	                  # -- Fill, and rim
	                  else{
	              
		                  myPlot = myPlot +
		                           geom_node_point(aes(fill = highlighColumn, color = rimColumn), size = sizeColumn, stroke = 2, shape = 21) +
		                           scale_color_manual(values=colorVectorRim) +
		                           scale_fill_manual(values=colorVectorHighlight)
		              
	                  }
	          		
	          	  }
	          	  # If the variable is numerical, do a chromatic scale
	          	  else{
	          	  	
	          		myPlot = myPlot +
		                     geom_node_point(aes(fill = highlighColumn), size = sizeColumn, stroke = 0.5, shape = 21) + 
							 scale_fill_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous"))
	          		
	          	 }
	          	
	          	
	
	            
	          }
	          
	          # If you don't have a fill, you may or may not have a rim
	          else{
	            
	            # -- No fill, no rim
	            if(is.null(rimVariable)){
	              
	        		print(colorVectorHighlight)
	            	
	            	myPlot = myPlot +
	                		          geom_node_point(aes(fill = "grey"), size = sizeColumn, stroke = 1, shape = 21) +
	                				  #theme(legend.position="none")
	              					  scale_fill_manual(values = colorVectorHighlight)
	              	
	            }
	            
	            # -- No fill, with rim
	            else{
	              
	              myPlot = myPlot +
	                geom_node_point(aes(fill = "grey", color = rimColumn), size = sizeColumn, stroke = 2, shape = 21) +
	                scale_color_manual(values=colorVectorRim)
	              
	            }
	            
	            
	            
	          }
	          
	          
	        }
	        
	        # Add Theme and Labs
	        {
	        	
	        	
	        	myPlot = myPlot +
	                
	            	theme(panel.background   = themeData[[1]],
	                	  axis.line          = themeData[[2]],
	                      panel.grid.major.y = themeData[[3]],
	                      panel.grid.major.x = themeData[[4]],
	                      legend.position    = themeData[[5]],
	            		
	            		  plot.background    = element_rect(fill='transparent', color=NA),
	            		
	            		  panel.grid.minor = element_blank(),
	            		  axis.text.x      = element_blank(),
	            		  axis.text.y      = element_blank(),
	            		  axis.ticks       = element_blank()) 
	        	
	        	myPlot = myPlot +
	
	            	labs(title    = plotTitle,
	                	 subtitle = plotSubtitle,
	                	 caption  = plotCaption,
	                	 color    = rimName,
	                	 fill     = highlitedName,
	                     x = plotXLabel, y = plotYLabel)
	          
	        }
	        
	        # If you want a bigger legend
	        if(!is.null(overrideLegendSize)){
	            myPlot = myPlot +
	        
	                guides(colour = guide_legend(override.aes = list(size=overrideLegendSize))) +
	                guides(fill   = guide_legend(override.aes = list(size=overrideLegendSize)))
	                
	                #theme(legend.key.size = unit(overrideLegendSize, 'cm'))        
	        
	        }
	        
	        # If you want the legend somewhere else
	        if(!is.null(overrideLegendPosition)){
	            
	            myPlot = myPlot + theme(legend.position= overrideLegendPosition )
	            
	        }
	    
      }
      print("AA")
      # Save the image and the .tex files to generate the image
      {
      	
      	  imageWidth  = 8
      	  imageHeight = 8
          if(overrideImageWidth>0)  imageWidth  = overrideImageWidth
          if(overrideImageHeight>0) imageHeight = overrideImageHeight
		  print(imgFilePath)
		  print(imageWidth)
          ggsave(imgFilePath, plot = myPlot, width = imageWidth, height = imageHeight, limitsize = FALSE)  
    		  
          if(savePDF == TRUE){
           	  ggsave(changeFileExtension(imgFilePath, "pdf"), plot = myPlot, width = imageWidth, height = imageHeight, bg='transparent', limitsize = FALSE)   # FUCK SHIT; FUCK! R doesn't tell you that the "imageFilePath" is in another function still on memory. HOW WAS THE RETARD THAT MADE THE SCOPE VARIABLES WORK IN THIS STUPID LANGUAGE!!!??
          }
        print("CC")
          myReturn      = vector("list", length = 3)
          myReturn[[1]] = myPlot
          myReturn[[2]] = imgFilePath
          myReturn[[3]] = vecFilePath
        
      }
  
    return (myReturn)
    
}





# Reachability plot
# A bunch of steps to see how is the coverage of your network after that many steps
doReachabilityPlot <- function(edgesDF, nodesDF, plotFileFolder,
							   directedPlot = FALSE, totalSteps = 15,
	
    						   plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL,
							   plotXLabel = NULL, plotYLabel = NULL,
                                
							   plotTheme = NULL,
                               overrideTableName = NULL,
                               overrideCaption = NULL,
                               ymin = NULL, ymax = NULL){
	
	# Define plot type
    myPlotType = "RechabilityBoxplot"
    myTableName = deparse(substitute(edgesDF))
      
    # If you need to override your table name, then do it
    if(!is.null(overrideTableName)) myTableName = overrideTableName
    
    # Get the theme information
    themeData = getThemeParameters(plotTheme)
      
    # Create the graph objects and check for distances
    myGraph      = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot) # I could give the graph to the function, but R is a stupid language with no proper classes and no proper pointer so why even bother to program in this!??
    distanceMap  = distances(myGraph)

    # Get how many nodes we have
    totalNodes   = nrow(nodesDF)

    # We need to create a table like this to make the boxplots:
    # -----------------------------------
    # Node ID \ Steps \ Coverage \
    #    A        1       10%
    #    A        2       15%
    #    B        1        7%
    #    C        3       25%
    #   ...      ...       ...
    #    Z        x        y

    # The easies way to make this, is to make a NumberOfNodes X Steps matrix, and then melt it
    coverageMatrix = data.frame(matrix(0, nrow = totalNodes, ncol = totalSteps))
    colnames(coverageMatrix) = c(1:totalSteps)
    coverageMatrix$ID = nodesDF$ID

    # Now we check all the combinations in the distance map
    for(i in 1:totalNodes){

    	for(j in 1:totalSteps){

        	# Get the row (or the column, since is undirected it doesn't matter) TODO: Make it directed
        	currentRow = distanceMap[i,]

        	# Check how many are in the current reach
        	totalReached = sum(currentRow <= j)

        	# Add that info to the matrix
        	coverageMatrix[i,j] = totalReached/totalNodes

        }
     }

     # In here we have the coverage matrix finished, so we melted it
     meltedCoverage = melt(coverageMatrix, id.vars = "ID")

     # Prepare the color vector which is going to be plain grey for all the steps
     boringColorVector = rep("grey", totalSteps)

     # Get an automatic name if you don't have a proper one
     if(FALSE){
        
    	imageFilePath = ""
        
		myFileExtension = getFileExtension(plotFilePath)
        
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = meltedCoverage,
                                               tableName = myTableName, fileType = myPlotType)
        
     }
      
     # We don't care about the ID anymore, so we can throw that away (we care so little that we don't care about deleting it even)

     # Now we do the plot
     # 2 = Number of steps
     # 3 = Coverage
     # ---- Boxplot
     
      
	 myPlot = doBoxPlotV2(meltedCoverage, 3, plotFileFolder,
                          groupIndex = 2,
	 	                  outlierShape = NA,
	 	                  colorsVector = boringColorVector,
	 					  showPValues = FALSE,
	 					  showANOVA   = FALSE,
	 					  ymin = ymin, ymax = ymax,
                          plotTitle = plotTitle, plotSubtitle = plotSubtitle,
                          plotCaption = plotCaption, plotXLabel = plotXLabel, plotYLabel = plotYLabel,
                          plotTheme = plotTheme,
                          overrideTableName = myTableName,
                          overrideCaption = overrideCaption)     
      
	return (myPlot)

}	



# Simulation plot
# THIS IS IMPOSSIBLE!!!! to run good on R, need to use Rcpp to make anything good of this
# A bunch of steps to see how a disease advance in your network after that many steps
doSimulationPlot2 <- function(edgesDF, nodesDF, plotFilePath,
							  directedPlot = FALSE, totalSteps = 10, totalSimulations = 5,
	
							  vaccineStep = 0, totalVaccinesSteps = 100,
	
							  forceVaccines = FALSE,
	
                              plotTitle = NULL, plotSubtitle = NULL, plotCaption = NULL, plotXLabel = NULL, plotYLabel = NULL,
                              plotTheme = NULL,
                              overrideTableName = NULL,
                              overrideCaption = NULL,
                              ymin = NULL, ymax = NULL){

      
	# Define plot type
    myPlotType  = "SimulationLinePlot"
    myTableName = deparse(substitute(edgesDF))
      
    # If you need to override your table name, then do it
    if(!is.null(overrideTableName)) myTableName = overrideTableName
      
    # Get the theme information
    themeData = getThemeParameters(plotTheme)

    # Get an automatic name if you don't have a proper one
    {
        
        imageFilePath = ""
        
        myFileExtension = getFileExtension(plotFilePath)
        
        if(myFileExtension == ".png") imageFilePath = plotFilePath
        
        else imageFilePath = automaticFilePath(plotFilePath, myDataFrame = edgesDF,
                                               tableName = myTableName, fileType = myPlotType)
        
    }

    print(imageFilePath)
    print("Simulating, please wait...")

    # Create the graph objects and check for distances
    myGraph      = graph_from_data_frame(edgesDF, vertices = nodesDF, directed = directedPlot)
    distanceMap  = distances(myGraph)

    # Get how many nodes we have
    totalNodes   = nrow(nodesDF)

    # Initialize the conditions matrices
    # -- Probability to infect others and be infected yourself by others.
      
    # -- Probability to get a recurrent virus after cured
    #    (herpes type, very simple, now is none)
    recurrentVirusMatrix           = DF(totalNodes, totalNodes, 0.01)
    colnames(recurrentVirusMatrix) = c(1:totalNodes)
    recurrentVirusMatrix $ID       = nodesDF$ID
    
        
    # -- Probability of getting immunity
    #    (if somebody try to infect you and fails, do you become immune forever, now is NO)

    # -- Probability of dying
    #    (in each given step, how likely is that you are remove from the infected set, no is ZERO)
      
    # TODO: There is more, see slides

    # CONTAGIOUS MATRIX
    # ----  If you are infected, what is the probability of giving the disease to another
    #       (leave it constant for simplicity, but you can model people giving away disease
    #        to other being more easy/difficult, ie: they live together and person A doesn't
    #        wash his hands ever, agh!)
    giveDeseaseMatrix           = DF(totalNodes, totalNodes, 0.3)
    colnames(giveDeseaseMatrix) = c(1:totalNodes)
    giveDeseaseMatrix$ID        = nodesDF$ID

    # INFECTABILITY MATRIX
    # ---- If you are not infected, what is the probability of receiving the disease if you are expose
    #      (how good your immune system is with respect the person that gives you the disease,
    #       if the person doesn't matter, leave the rows constant)
    receiveDeseaseMatrix           = DF(totalNodes, totalNodes, 0.9)
    colnames(receiveDeseaseMatrix) = c(1:totalNodes)
    receiveDeseaseMatrix$ID        = nodesDF$ID

    # IMMUNE VECTOR
    # ---- If you are infected, what is the probability of gaining immunity
    #      (you can change this to follow a distribution, so you have more chances the healthier is your diet, or lower for old people)
    immunityProbabilityVector = rep(0.1, totalNodes)    
          
    # DYING VECTOR
    # ---- If you are infected, what is the probability of dying at any given time
    #      (you can change this to follow a distribution, so you have more chances the more the disease advance for example)
    #      (this is a vector, you don't have more chances of dying depending of how many people you are friend, you
    #       have more probabity of getting infected, but dying is an independent event)
    dyingProbabilityVector = rep(0.01, totalNodes)

      
    # We need to save all the neighbours of each person so the simulations run fasters
    # because R is a very stupid language and don't have pointers. And I hate it, and I want to die becuase of it.
    
    currentFriendshipMatrix  = getFriendshipMatrix(edgesDF,  totalNodes)
    
    totalNeighboursList = rep(0, totalNodes)
    neighboursList      = newList(totalNodes)
    #centralityList      = as.vector(igraph::closeness(myGraph, mode = "all"))
    #centralityList      = as.vector(igraph::betweenness(myGraph, directed = FALSE))
    centralityList      = as.vector(igraph::eigen_centrality(myGraph)$vector)
    
    for(i in 1:totalNodes){
    	
    	currentID = i
    	results           = getFrienshipTypes(currentID, currentFriendshipMatrix)
        neighboursList[[i]] = results[[4]]
        totalNeighboursList[i] = length(results[[4]])
        
    }
    
    
    # ---- Reverse order by popularity (most popular first)
    #popularIDs          = rev(order(totalNeighboursList))
    popularIDs          = rev(order(centralityList))
            		
    #print("---TTTTTT----")
    #print(popularIDs)    
    
    
    
    
    # We need to create a table like this to make the lineplot:
    # ------------------------------------------------------------
    # Start X  | Start Y | End X | End Y | Type
    #  1.3        5.3       5.2     6.7     Infected , Dead, Immune ...
    #          ................
    # (There is going to be TotalSteps x Total Simulation x {Infected, Immune, Dead} lines)
    evolutionDF                = DF( (totalSteps * totalSimulations * 3), 6, 0 )
    colnames(evolutionDF)      = c( "StartX", "StartY", "EndX", "EndY", "Type", "Simulation")
    currentEvolutionRow        = 1

    # Pick up your starting nodes
    # One for each simulation
    startingNodes = sample(x = nodesDF$ID,size = totalSimulations, replace = FALSE)

    # ------------------------------------------------------
    # FOR EACH SIMULATION (each one of the starting points)
    # ------------------------------------------------------
    for(i in 1:totalSimulations){

    	#print("Simulation Started:")
        #print(i)

        # Initialize everything
        {
            # Get how many nodes we have, this might vary during the simulation if people die
        	totalNodes        = nrow(nodesDF)
        	currentTotalNodes = totalNodes

        	# Initialize your set of infected to the first person
        	#infectedSet = c(startingNodes[i])
        	infectedSet2 = rep(FALSE, totalNodes)
        	infectedSet2[as.numeric(startingNodes[i])] = TRUE  # Stupid R doesn't want the IDs as numbers but strings :(
                                        	                   # Like, this whole thing with boolean vector to optimize is silly
                                            	               # a real programing language has pointers to optimize all of this :((((

        	# The dead people come here
        	deadPeopleSet  = rep(FALSE, totalNodes)

        	# The immune people come here
        	immunePeopleSet = rep(FALSE, totalNodes)        	
        	
        	# In here we put the people who are candidate to be infected in whatever current step
        	#potentialCurrentStep = c()
        	
        	# Some people get infected, let say 400. Then some get immunity and some die, 350 infected remain
        	# We need to keep track of the 400 number so the plot looks nice
        	lastInfectedYNumber = 0
        }

        # ------------------------------------------------------
        # For each step in the simulation
        # ------------------------------------------------------
        for(j in 1:totalSteps){

        	# Show how far away we are in the current simulation
        	#print( paste("        ", round(j/totalSteps,2), sep="") )

        	# Save the previous step results for later
        	lastStepInfected2 = infectedSet2
        	lastStepDead      = deadPeopleSet
        	lastStepImmune    = immunePeopleSet

        	# Update how many people we have in each set
        	currentTotalInfected = sum(infectedSet2)       # totalInfected
        	currentTotalDead     = sum(deadPeopleSet)
        	currentTotalImmune   = sum(immunePeopleSet)
        	
        	# Get who are these people
        	currentInfectedIDs = nodesDF[infectedSet2,]$ID
        	currentDeadIDs     = nodesDF[deadPeopleSet,]$ID
        	currentImmuneIDs   = nodesDF[immunePeopleSet,]$ID
        	
        	# Save here the new numbers
        	currentTotalNewInfected = 0
        	currentTotalNewImmune   = 0
        	currentTotalNewDead     = 0
        	
        	# Save here the new people
        	currentNewInfectedID    = NA

        	
        	# ------------------------------------------------------------------
        	# Infections
        	# ------------------------------------------------------------------
        	
        	# Check how many people are infected in this step
        	# If we have more than 0, the simulation continues.
        	if(currentTotalInfected > 0){
        		
            	# People that are going to be infected during this step
            	newInfected = rep(FALSE, totalNodes)
            	
            	# For each infected person (if there are any left)
            	# Give diseases away
            	for(k in 1:currentTotalInfected){
            		
            		# Who is giving the disease
					currentIllPersonID = as.numeric(currentInfectedIDs[k])
              
            		# Check his neighbors
            		#results           = getFrienshipTypes(currentIllPersonID, overallNetworkDF)
            		currentNeighbours = neighboursList[[currentIllPersonID]]
            		totalNeighbours   = totalNeighboursList[currentIllPersonID]

            		# Check in the matrix if the person get infected or not based on the die roll
            		if(totalNeighbours > 0){

                		# Make the disease roll for each neighbor
                		currentGivingDiseaseRoll = runif(totalNeighbours, 0, 1)
                		currentSavingDiseaseRoll = runif(totalNeighbours, 0, 1)
                
                		# For each neighbour
                		for(l in 1:totalNeighbours){
                  
                			#The person candidate to receive the disease
                			currentNeighbourID = as.numeric(currentNeighbours[l])
                  
                			# print("My next neighbour ID")
                			# print(currentNeighbourID)
                  
                			# If the person that we are trying to infect:
                			# -- has already been infected in this step
                			# -- is dead
                			# -- is immune
                			# -- is already infected
                			# Then skip it
                			skipThis = ( newInfected[currentNeighbourID]    ||
                            		     deadPeopleSet[currentNeighbourID]  ||
                            			 infectedSet2[currentNeighbourID]   ||
                				         immunePeopleSet[currentNeighbourID])
                  
                			# DEBUG
                			#if( skipThis == TRUE ){
                				
                				#print("--")
                				#print( paste0( "Me: ", currentIllPersonID))
                				#print( paste0( "Ne: ", currentNeighbourID))
                				#print("Neighbour skipped")
                				
                				#if(newInfected[currentNeighbourID]     == TRUE) print("Now is infected")
                				#if(deadPeopleSet[currentNeighbourID]   == TRUE) print("Already dead")
                				#if(infectedSet2[currentNeighbourID]    == TRUE) print("Already infected")
                				#if(immunePeopleSet[currentNeighbourID] == TRUE) print("Already immune")
                				
                			#}
                			
                			
                			# If we have a valid candidate to receive the disease, then check if he does receive it
                			if( skipThis == FALSE ){
                    
                    			# Get the probabilities for giving and receiving
                    			probOfGiving       = giveDeseaseMatrix[   currentIllPersonID, currentNeighbourID]    
                    			probOfReceiving    = receiveDeseaseMatrix[currentIllPersonID, currentNeighbourID] 
                    
                    			# First if you manage to give it away
                    			if(currentGivingDiseaseRoll[l] < probOfGiving){
                        			# Second, if the person defended himself
                        			# print("...It gave him the disease")
                        			if(currentSavingDiseaseRoll[l] < probOfReceiving){
                            			# At this point, the neighbor is mark as infected
                            			newInfected[currentNeighbourID] = TRUE
                            			# print( paste0("NEW INFECTED!! ", currentNeighbourID )  )
                        			}  
                        			else{
                            			# print("...but failed to receive it")
                        			}
                    			}
                    			else{
                        			# print("...but the disease didn't transmit")
                    			}
                    
                			}

                		}
                		
            		}
            	} 
            
            	# Get the ID of the newly infected people
            	#currentNewInfectedID    = nodesDF[newInfected,]$ID
            	currentTotalNewInfected = sum(newInfected)
            
            	# Add them to the list of infected
            	if(currentTotalNewInfected > 0) infectedSet2 = ( infectedSet2 | newInfected)

        	}
        	
        	# Add the infection part to the dataframe
        	{

            	startY = lastInfectedYNumber                              # (old number)
            	endY   = sum(infectedSet2)/totalNodes  # (new number)
            	startX = j
            	endX   = j + 1
            	currentType = "Infected"
            	
            	evolutionDF$StartX[currentEvolutionRow] = startX
            	evolutionDF$StartY[currentEvolutionRow] = startY
            	evolutionDF$EndX[currentEvolutionRow]   = endX
            	evolutionDF$EndY[currentEvolutionRow]   = endY
            	evolutionDF$Type[currentEvolutionRow]   = currentType
            	evolutionDF$Simulation[currentEvolutionRow] = i
            
            	currentEvolutionRow = currentEvolutionRow + 1
            	
            	lastInfectedYNumber = endY
        	}
        	
        	# ------------------------------------------------------------------
        	# Re-Infections
        	# ------------------------------------------------------------------
        	
        	# ------------------------------------------------------------------
        	# Immunities
        	# ------------------------------------------------------------------        	
        	{

        		# Make the immunity roll for each node and mark people for death
        		immuneRoll = runif(totalNodes, 0, 1)
            	immuneRoll = immuneRoll < immunityProbabilityVector # People marked for immunity
        		
            	# Only infected people can gain immunity
            	# In particular, only infected people with at least one step gain immunity
            	currentImmunePeople = (lastStepInfected2 & immuneRoll)         		
        		
            	#DEBUG
            	#currentTotalInfectedIMM1 = sum(infectedSet2)
            	
            	# Update the people that is immune
            	immunePeopleSet = (immunePeopleSet | currentImmunePeople) 
            	
            	# Update the people that are infected, take away the immune people
            	infectedSet2    = (!immunePeopleSet & infectedSet2) 
            	
            	#DEBUG2
            	#currentTotalInfectedIMM2 = sum(infectedSet2)
            	#if(currentTotalInfectedIMM2 < currentTotalInfectedIMM1) print("Someone gain immunity!")
            	
            	# Remove people who are immune from the infected set
            	#for(z in 1:totalNodes){
              
            	#	if(immunePeopleSet[z] == TRUE) infectedSet2[z] == FALSE # No need to check if it is infected, if it is immune it get desinfected
              
            	#}            	
            	        	
            	# Add the vaccines effect if any
            	# If we have a vaccine
            	if(vaccineStep > 0){
            		
            		# If the vaccine is introduced already
            		if(j >= vaccineStep){
            		
            			# We immunize random people.
            			# The people must be:
            			# ---- Not dead
            			# ---- Not already infected
            			# ---- Not immune, because they either got vaccine already or got infected in the pass and are self-cured
            			vaccinatedSet  = rep(FALSE, totalNodes)
            			notPossibleSet = (infectedSet2 | immunePeopleSet | deadPeopleSet) 
            			candidatesID   = nodesDF[ !notPossibleSet ,]$ID
            			
            			# If we have more candidates than vaccines, the vaccines are distributed randomly
            			if( length(candidatesID) > totalVaccinesSteps ){
            			
            				# Give vaccines at random, and don't give the vaccine twice to the same person
            				actuallIDs = sample(candidatesID, totalVaccinesSteps, replace=FALSE)
            			
            				# If the vaccines are given to the best candidates, 
            				if(forceVaccines == TRUE){
            					
            					# print("Forced!")

            					#print( length(rev(order(totalNeighboursList))) )
            					
            					#print(  length(candidatesID) )
            					
            					#print( length( rev(order(totalNeighboursList)) %in% candidatesID ) ) 
            					
            					#candidatesID = candidatesID[ candidatesID  %in% rev(order(totalNeighboursList))]
            					#actuallIDs   = candidatesID[1:totalVaccinesSteps]
            					
            					#previousIDs      = actuallIDs
            					
            					candidatesValid  = popularIDs[popularIDs %in% candidatesID]
            					bestCandidates   = candidatesValid[1:totalVaccinesSteps]
            					actuallIDs       = bestCandidates

            					#print("--")
            					#print(previousIDs)
            					#print(actuallIDs)
            					#print(sum(actuallIDs%in%previousIDs))
            					
            				}
            				
            				# Add people to the vaccination list
            				for(z in 1:length(actuallIDs)){
              
            					vaccinatedSet[actuallIDs[z]] = TRUE
            				
            				}             				
            						
            			}
            			
            			# If we have more vaccines than candidates, everybody get one
            			else{
            				
            				# Add people to the vaccination list
            				for(z in 1:length(candidatesID)){
              
            					vaccinatedSet[candidatesID[z]] = TRUE
            				
            				}            				

            			}
            			
            			# Update the people that is immune via vaccine
            			immunePeopleSet     = (immunePeopleSet | vaccinatedSet) 
            			
            		}
            	}
            	
            	# We don't need to remove immune people from the graph
            	# Immune people are not infected, and as such they don't
            	# transmit the disease in any way according to the infection
            	# part of the code.
            	
            	# Add the dead part to the dataframe
        		{
            		startY = sum(lastStepImmune)/totalNodes    # (old number)
            		endY   = sum(immunePeopleSet)/totalNodes   # (new number)
            		startX = j
            		endX   = j + 1
            		currentType = "Immune"
            	
            		evolutionDF$StartX[currentEvolutionRow] = startX
            		evolutionDF$StartY[currentEvolutionRow] = startY
            		evolutionDF$EndX[currentEvolutionRow]   = endX
            		evolutionDF$EndY[currentEvolutionRow]   = endY
            		evolutionDF$Type[currentEvolutionRow]   = currentType
                    evolutionDF$Simulation[currentEvolutionRow] = i
            		
            		currentEvolutionRow = currentEvolutionRow + 1
        		}
        		
        	}
        	
        	
        	# ------------------------------------------------------------------
        	# Deads
        	# ------------------------------------------------------------------        	
        	{
        		# Make the deadroll for each node and mark people for death
        		deathRoll = runif(totalNodes, 0, 1)
            	deathRoll = deathRoll < dyingProbabilityVector # People marked for death
        		
            	# Only infected people die
            	# In particular, only infected people with at least one step dies
            	currentDeadPeople = (lastStepInfected2 & deathRoll) 
            	
            	# Update the people that is dead
            	deadPeopleSet     = (deadPeopleSet | currentDeadPeople) 
            	
            	# Remove people who are dead from the infected set
            	infectedSet2    = (!deadPeopleSet & infectedSet2) 
            	
            	# Remove people who are dead from the infected set
            	#for(z in 1:totalNodes){
              
            		#if(infectedSet2[z] == TRUE && deadPeopleSet[z] == TRUE) infectedSet2[z] == FALSE
              
            	#}
            	
            	# We don't need to remove dead people from the graph
            	# Dead people are not infected, and as such they don't
            	# transmit the disease in any way according to the infection
            	# part of the code.
            	
            	# Add the dead part to the dataframe
        		{
            		startY = sum(lastStepDead)/totalNodes    # (old number)
            		endY   = sum(deadPeopleSet)/totalNodes   # (new number)
            		startX = j
            		endX   = j + 1
            		currentType = "Dead"
            	
            		evolutionDF$StartX[currentEvolutionRow] = startX
            		evolutionDF$StartY[currentEvolutionRow] = startY
            		evolutionDF$EndX[currentEvolutionRow]   = endX
            		evolutionDF$EndY[currentEvolutionRow]   = endY
            		evolutionDF$Type[currentEvolutionRow]   = currentType
            		evolutionDF$Simulation[currentEvolutionRow] = i
            
            		currentEvolutionRow = currentEvolutionRow + 1
        		}
            	
        		
        	}
         
        }

        #print("Simulation Ended:")
        #print(i)
        print(round(i/totalSimulations,2))

	}
  
    # ------------------------------------------------------
    # DO THE PLOT
    # ------------------------------------------------------

    # It is possible that the evolution DF don't have any infected, or dead, or immune.
    # We still want to keep the color coding for each type accordingly
    evolutionDF$Type = factor(evolutionDF$Type, levels = c("Infected", "Immune", "Dead"))
    
    # Do the plot with all the lines
    simulationPlot = ggplot(evolutionDF) +

				     # Draw each line
                     geom_segment(aes(x = StartX, y = StartY, xend = EndX, yend = EndY, colour = Type), alpha = 0.1) +
    	
					 # Specify color for each
    				 scale_color_manual(values=c("Infected"="red", "Immune"="blue", "Dead"="black")) + 
    	
                       # Draw ALL the steps in the X axys
                       scale_x_continuous(breaks=c(1:totalSteps)) +

                       # Scale the y axis to whatever
                       scale_y_continuous(limits=c(ymin, ymax)) +

                       # Create titles and subtitles
                       labs(title    = plotTitle,
                            subtitle = plotSubtitle,
                            caption  = plotCaption,
                            x = plotXLabel, y = plotYLabel) +

                       # Apply the theme
                       theme(panel.background   = themeData[[1]],
                             axis.line          = themeData[[2]],
                             panel.grid.major.y = themeData[[3]],
                             panel.grid.major.x = themeData[[4]])

    # Save the image
    imageWidth = totalSteps 
    ggsave(imageFilePath, plot = simulationPlot, width = imageWidth, limitsize = FALSE)

    
    # Prepare some stats about the run
    # ---- Immunization rate at the end
    immuneTypes = evolutionDF[evolutionDF$Type == "Immune",]
    endImmune   = immuneTypes[immuneTypes$EndX == max(immuneTypes$EndX),]
    meanImmune  = mean(endImmune$EndY)
    sdImmune    = sd(endImmune$EndY)
    
    # ---- Dead rate at the end
    deadTypes = evolutionDF[evolutionDF$Type == "Dead",]
    endDead   = deadTypes[deadTypes$EndX == max(deadTypes$EndX),]
    meanDead  = mean(endDead$EndY)
    sdDead    = sd(endDead$EndY)    
    
    # ---- Maximum infected
    # ---- ---- For each simulation, find the maximum, and the time in which happen
    infectedTypes = evolutionDF[evolutionDF$Type == "Infected",]
    maximumVector = rep(0,totalSimulations)
    stepsVector   = rep(0,totalSimulations)
    
    for (i in 1:totalSimulations) {
    	
    	currentSubset = infectedTypes[infectedTypes$Simulation == i,]
    	
    	currentMaximum = max(currentSubset$EndY)
    	currentStep    = -1
    	
    	for(n in 1:nrow(currentSubset)){
    		
    		if(currentSubset$EndY[n] == currentMaximum){
    				
    				currentStep    = currentSubset$EndX[n]
    				
    		}
    			
    	}
    
    	maximumVector[i] = currentMaximum
    	stepsVector[i]   = currentStep
    		
    }

    
    #infectedTypes = evolutionDF[evolutionDF$Type == "Infected",]
    
    #max(infectedTypes$EndY)
    
    
    # ---- Time of maximum infected
    
    print("-------------")
    print(paste0("Mean Immunity:       ", meanImmune , " +- ", sdImmune))
    print(paste0("Mean Dead:           ", meanDead   , " +- ", sdDead))
    print(paste0("Mean max infections: ", mean(maximumVector)   , " +- ",  sd(maximumVector)))
    print(paste0("at time:             ", mean(stepsVector)   , " +- ",  sd(stepsVector)))
    
    
    # Return
    myReturn = vector("list", length = 3)
    myReturn[[1]] = simulationPlot
    myReturn[[2]] = imageFilePath
    myReturn[[3]] = evolutionDF
      
    return (myReturn)

}