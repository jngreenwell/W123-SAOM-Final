#SAOMWC123 Procedural/Loading codes
library(RSiena)
library(network)


#To Load in the Matrices and dyadic covariate matrices for the three waves

WC1=as.matrix(read.csv("WCW1Mat.csv",na.strings="9",header=FALSE))
WC2=as.matrix(read.csv("WCW2Mat.csv",na.strings="9",header=FALSE))
WC3=as.matrix(read.csv("WCW3Mat.csv",na.strings="9",header=FALSE))

WC1Dyad=as.matrix(read.csv("WCW1DyadMat.csv",na.strings="9",header=FALSE))
WC2Dyad=as.matrix(read.csv("WCW2DyadMat.csv",na.strings="9",header=FALSE))
WC3Dyad=as.matrix(read.csv("WCW3DyadMat.csv",na.strings="9",header=FALSE))


#to load in some of the varying covariate files
#also consider making this drinking variable a constant covariate in subsequent models
PersonDrink=as.matrix(read.csv('PersonDrink.csv',na.strings='0',header=FALSE))

#To load in the constant covariates "demographics"
demographics1=as.matrix(read.csv("Demographics1.csv",na.strings="0"))
demographics2=as.matrix(read.csv("Demographics2.csv",na.strings="0"))

#Read in composition change file
composition=sienaCompositionChangeFromFile('CompositionWC.csv')
composition

#it is necessary to make a variable with the number of actors in the model
numberActors=dim(demographics1)[1]
numberActors



# First we have to create objects for the dependent variables.

# sienaDependent or sienaNet creates a sienaDependent object, here a network,
# from a matrix or array or list of sparse matrix of triples.
# This object will have the role of a dependent variable in the analysis.
# The name of this network object (here: friendship) will be used
# in the output file.
#to identify dependent variables for the analysis
friendship=sienaNet(array(c(WC1,WC2,WC3),dim=c(numberActors,numberActors,3)))
# The integers in the dim() here refer to the number of nodes (senders,
# receivers) and the number of waves.
# This object is an array of dimension 85 x 85 x 3, representing
# three adjacency matrices, with a number of attributes.
# Note that this is an object of class "sienaDependent"

class(friendship)

# with specific attributes and methods associated with it.
# You can get the detailed information by requesting

dim( friendship )
attributes( friendship )

# If you only are interested in the value of one particular attribute,
# you can request this by, e.g.,

attributes( friendship )$type

