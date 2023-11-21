# Please set the working directory to this script directory manually
#
# R use to have the option to do this by code, but an update broke it
# Now, in 2023, there is no way of doing this without the use of bloated
# external libraries, or using RStudio api.

# Load some data to do analysis and plots

# ---- Iris data set
data("iris")

# ---- Iris, but with unbalance number of species
iris2 = iris
iris2 = iris2[-c(1,149,150),]


data("mtcars")
mtcars2 <- within(mtcars, {
   vs <- factor(vs, labels = c("V", "S"))
   am <- factor(am, labels = c("automatic", "manual"))
   cyl  <- ordered(cyl)
   gear <- ordered(gear)
   carb <- ordered(carb)
})

# ------------------------------------------------------------------------------
# Creating dataframes and lists more easy
# ------------------------------------------------------------------------------
{

    # Load our own libraries
    source( "../lib/basic/toolsBasic.R" ,   encoding="utf-8")

    # R uses ultracomplicated syntax. In contrast, C based languages declares
    # datatypes constructors more easily with minimum amount of characters
    
    # That can be adapted, and create dataframes and list with minimum syntax
    sepals_samples_DF = DF(5,3)
    species_list      = newList(5)

    # We copy part of the iris dataframe into our little sample
    sepals_samples_DF[,1] = iris$Sepal.Length[1:5]
    sepals_samples_DF[,2] = iris$Sepal.Width[1:5]
    sepals_samples_DF[,3] = iris$Species[1:5]
    species_list          = sepals_samples_DF[,3]
        
}

# ------------------------------------------------------------------------------
# Analyzing data
# ------------------------------------------------------------------------------
{

	# --------------------------------------------------------------------------
	# Categorical p-values
	# --------------------------------------------------------------------------
	
	# We are going to try to find statistically significant differences in the
	# iris data set.
	#
	# 5 is the column index of the species, any categorical data would do
	#
	# 1 is the column index of the length, any numerical data would do.
	simpleCategoricalPValueV2(iris, 5, 1)

	# The returned object is as follow:
	#
	# A list of 4 elements
	#
    # [[1]] first element, error code
	#
    # 0     Error code 0, everything is fine
	#
	#
    # [[2]] second element, p-value
	#
	# 1.669669e-31 , a very low p-value, this is an ANOVA result because we
	#                have more than 2 categories.
	#
	# [[3]] third element, a summary of each category
	#       This is a dataframe that you can call and use
	#
    #           ID  Mean Median  Variance     Sigma Size
    # 1     setosa 5.006    5.0 0.1242490 0.3524897   50
    # 2 versicolor 5.936    5.9 0.2664327 0.5161711   50
    # 3  virginica 6.588    6.5 0.4043429 0.6358796   50
	#
	# [[4]] fourth element, each combination of p-values for each variable
	#       This is a triangular matrix.
	#       Each variable with itself don't have any p-value
	#       This is also a dataframe from where you can extract information
	#
	#
    #                    setosa   versicolor virginica
    # 1     setosa           NA           NA        NA
    # 2 versicolor 8.985235e-18           NA        NA
    # 3  virginica 6.892546e-28 1.724856e-07        NA
	
	# --------------------------------------------------------------------------
	# Categorical Xi^2 tables
	# --------------------------------------------------------------------------
	
	# The mtcars dataset has information about cars models. Two particular
	# variables are
	# 8 - vs - Engine shape (V-shape, straight)
	# 9 - am - Transmission (automatic, manual)
	#
	# Let's do a Xi^2 test on that
	categoricalXiV2(mtcars2, 8, 9)
	
	# [[1]] first element, absolute and relative count for the first variable
	#
    #   vs NTotal  NFreq
    # 1  V     18 0.5625
    # 2  S     14 0.4375

	
    # [[2]] second element, absolute and relative count for the second variable
	#
    #          am NTotal   NFreq
    # 1 automatic     19 0.59375
    # 2    manual     13 0.40625

	
    # [[3]] third element, the absolute count of each combination
    #     Absolute automatic manual
    # 1          V        12      6
    # 2          S         7      7

	
    # [[4]] forth element, the relative count of each combination
    #     Relative   automatic   manual
    #   1        V     0.37500  0.18750
    #   2        S     0.21875  0.21875

	
    # [[5]] fifth element, relative frequency that is above or bellow expected
    #   Difference   automatic       manual
    # 1          V   1.1228070   0.8205128   
    # 2          S   0.8421053   1.2307692
	#
	# The 1.12 value indicates that we have 1.12 more V and automatic cars that
	# we should. We have 32 models in total, of which 18 are V-shaped and 19 are
	# automatic. We should have 10 if we have random cars, but we have 12, which
	# is above expected. In particular 1.12 more than expected.
	
	
	# [[6]] sixth element, a SIGNED p-value with the significant of difference
    #   Binomial  automatic     manual
    # 1        V  0.6470098 -0.5788392
    # 2        S -0.6470098  0.5788392
	#
	# This is based on a binomial test for each cell.
	#
	# 0.64 means a p-value = 0.64. So even though we have 12 cars, that number
	# is not very different from 10.
	#
	# -0.58 means that the p-value is 0.58, and the - sign means that we have
	# lower than expected.
	
	
	# [[7]] Break down of total vs expected values and Xi-test
	#
    #     0.6  automatic   manual   Total  Freq
    # 1     V      12/10      6/7      18  0.56
    # 2     S        7/8      7/5      14  0.44
    # 3 Total         19       13      32     
    # 4  Freq       0.59     0.41            1
	#
	# Here we see that V and automatic have 12 cars, but we expected 10 in total
	# Because the binomial test is not significant, there is no mark in this
	# examples that indicates an important difference
	#
	# The 0.6 is the round up Xi^2 test value (next).

	
    # [[8]] The Xi^2 test value
    # 0.5555115  , 0.56 is not statistically significant, this table doesn't
	#              seems to have a bias towards engine shape or transmission.

	
    # [[9]] An overview dataframe of [[1]] and [[2]] combined
	#
    #     0.6 automatic        manual        Total Freq
    # 1     V        12 (0.38)      6 (0.19)    18 0.56
    # 2     S         7 (0.22)      7 (0.22)    14 0.44
    # 3 Total        19            13           32     
    # 4  Freq             0.59          0.41          1

	
	# [[10]] Same as [[7]], but with relative frequencies instead.
    #	
    #     0.6 automatic        manual        Total Freq
    # 1     V      0.38 (0.33)   0.19 (0.23)    18 0.56
    # 2     S      0.22 (0.26)   0.22 (0.18)    14 0.44
    # 3 Total        19            13           32     
    # 4  Freq             0.59          0.41          1
	
	
	
}

# ------------------------------------------------------------------------------
# Plotting data
# ------------------------------------------------------------------------------
{
	
	# Let define where we want to save the examples
	
	EXAMPLE_FOLDER  = file.path(paste(RESULT_FOLDER,  "examples/", sep = ""))
	BARPLOTS_FOLDER = file.path(paste(EXAMPLE_FOLDER, "barplots/", sep = ""))
	
	
	# A simple barplot, giving the table, the column index, and where you want
	# to save it, default order is "none", so whatever factor order if there is
	# one.
	doBarPlotV2(iris2, 5, BARPLOTS_FOLDER)	
	
	# Same, but in descending order
	doBarPlotV2(iris2, 5, BARPLOTS_FOLDER,
		        sort = "descending")
	
	# Same, but in ascending order
	doBarPlotV2(iris2, 5, BARPLOTS_FOLDER,
		        sort = "ascending")	
	
	
	
	
	
	doBarPlotV2(iris2, 5, BARPLOTS_FOLDER,	
    			rotation            = TRUE,        
                sort                = "descending", 
                plotTitle           = "Barplot Rotated",
				overrideTableName   = "rotatedIris",
                overrideImageWidth  = 8,
                overrideImageHeight = 8)
	
	
	
}


# Examples of data analysis