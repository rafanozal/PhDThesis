# ------------------------------------------------------------------------------
# R current working directory
# ------------------------------------------------------------------------------
{

    # You need to set working directory to the script location manually
    
    # R use to have the option to do this by code, but an update broke it
    # Now, in 2023, there is no way of doing this without the use of bloated
    # external libraries, or using RStudio api. Both options defeat 
    
}

# ------------------------------------------------------------------------------
# RStudio problems
# ------------------------------------------------------------------------------
{

    # RStudio start lagging when you collapse brackets in a script that is
    # barely 5000 lines long. I'm running this on a i5-6400T CPU @ 2.20GHz
    # with 16GiB of RAM
    
    # RStudio changes spaces into tabs arbitrarily, no way to op-out
	
    
}


# ------------------------------------------------------------------------------
# R basic data types
# ------------------------------------------------------------------------------
{

    
    print(NA & TRUE)
        
}

# ------------------------------------------------------------------------------
# R logic operators
# ------------------------------------------------------------------------------
{

}

# ------------------------------------------------------------------------------
# R basic data structures
# ------------------------------------------------------------------------------
{

    # R uses very complicated syntax. This is how you create dataframes and lists:
    myNewDF   = data.frame(matrix(0, nrow = 5, ncol = 5))
    myNewList = vector("list", length = 5)

    # Notice how you need first to create a matrix, then converted to a
    # dataframe. Same 
        
    
}

# ------------------------------------------------------------------------------
# R indexing
# ------------------------------------------------------------------------------
{

}
