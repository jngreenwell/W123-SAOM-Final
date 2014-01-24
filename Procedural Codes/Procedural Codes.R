#this script is for SAOM WC procedures (loading data sets, and estimations)

#Need to load RSiena and statnet and associated packages
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

# The entire contents of the object are listed by typing

#       friendship


# The function sienaDependent can also be used to create a behavior variable object
# with the extra argument type = "behavior".
# (Non-mentioned attributes get the default value, and in this case
# oneMode is the default; see below.)

# The 'attribute' data is made available as a
# dependent behavior variable by the function

#drinkingbeh <- sienaDependent( drink, type = "behavior" )

# the class, class(drinkingbeh), is still sienaDependent.

# (Note: only use the variable in ONE role in a given model:
#  behavior variable or changing covariate not both!)


# ---- B. ----------------------------------------------------------------------
# construct objects for the explanatory (independent) variables.
# From the help request
#       ?sienaDataCreate
# we see that these can be of five kinds:
# coCovar            Constant actor covariates
# varCovar           Time-varying actor covariates
# coDyadCovar        Constant dyadic covariates
# varDyadCovar       Time-varying dyadic covariates
# compositionChange  Composition change indicators

# You can get help about this by the following requests:
#       ?coCovar
#       ?varCovar
#       ?coDyadCovar
#       ?varDyadCovar
#       ?sienaCompositionChange

# The variables available for this data set all are changing actor covariates.
# For illustrative purposes, we use smoking as observed at the first wave
# as a constant covariate:

# The options available for defining a sienaDependent object
# are displayed by typing

sex <- coCovar( demographics1[ , 2 ] )
ethnic=coCovar(demographics1[,3])

# This selects the 2nd and 3rd collumns of demographics 1,
# which contains the first wave observations for gender and ethnicity,
# and makes it available as a constant covariate.
# This is the pattern for for evey covariate file, e.g.
#       Attr1 <- coCovar( Covariate1 )
# where Covariate1 is a matrix with dim(Covariate1) equal to n x 1
# Note, if Covariates is a matrix with dim(Covariates) equal to n x p
# you can create constant covariates through
#       Attr1 <- coCovar(Covariates[,1])
#       ...
#       Attrk <- coCovar(Covariates[,p])

# We will use the drinking data as a changing covariate for a preliminary tes.
# The function varCovar creates a changing covariate object from a matrix;
# the name comes from 'varying covariate'.

alcohol <- varCovar( PersonDrink )

# You need at least three waves in the data set to define a varying covariate
# by the function varCovar as the previous wave is used
# as a predictor of the next wave.

# The command

attributes( alcohol )

# will tell you the information that RSiena now has added to the drink data.

# ---- C. ----------------------------------------------------------------------
# We now combine the dependent and independent variables.
# The function sienaDataCreate creates a Siena data object from input networks,
# covariates and composition change objects;
# the objects that earlier were created by sienaDependent will have the role
# of dependent variables, and similarly the other roles are predetermined
# by creation by the functions coCovar, varCovar,
# coDyadCovar, varDyadCovar, and sienaCompositionChange.

WCDyads=varDyadCovar(array(c(WC1Dyad,WC2Dyad),dim=c(85,85,2)))

# the vardyadcovar format is somewhat similar to the sienaNet function 
# friendship=sienaNet(array(c(WC1,WC2,WC3),dim=c(numberActors,numberActors,3)))

mydata <- sienaDataCreate( friendship, alcohol, sex,ethnic,composition,WCDyads )



mydata


#####################################

# ---- D. ----------------------------------------------------------------------
################################################################################
###
### ---- DEFINING EFFECTS ------------------------------------------------------
###
################################################################################
# The data set as combined in mydata implies a certain set of effects
# that can be included in the specification of the model.
# The function getEffects creates a dataframe of effects with a number of extra
# properties for use in RSiena:

myeff <- getEffects( mydata )

# mydata is needed as an argument as the effects depend on the number
# and types of covariates and dependent variables.
# Before we explain the object myeff and how we shall be going to use it,
# we first produce a data description which is available now:

print01Report( mydata, myeff, modelname = 'WC_init' )

# This writes a basic report of the data to the file
# WC_init.out in the current working directory. Locate and open it!
# Inspecting this is important because it serves as a check and also contains
# a number of basic descriptives.
# In this description you can see that the third wave data for alcohol are not used.
# This is because changing covariates are assumed to be constant from one wave until
# immediately before the next wave, so that the values for the last wave are ignored.

# Let us now consider the myeff object, which is used to specify the model.
# It is of the class "sienaEffects", and contains the model specification.
# You can inspect the current model specification by simply requesting
myeff

# For starting, the model specification is just a very limited default
# (including rates of change, outdegree and reciprocity only)
# To make a meaningful analysis, you will need to add to it.

# The rows of myeff correspond to the effects.
# By requesting

names( myeff )

# you see the type of information that is stored about the effects,
# i.e., the columns (variables) defined for the effects.
# If desired, more information about these variables can be obtained
# from the help files:
#      ?getEffects

# Some often used variables are effectName, shortName, type, and parameter.
# The set of available effects and their most used columns
# can be inspected as follows:

#       effectsDocumentation(myeff)

# This gives a long list of effects: all defined in Section 12 of the manual,
# as far as they are meaningful for dataset mydata.
# The "include" column defines whether effects are included in the model.
myeff$include

# Here the TRUE values correspond to the default model specification which,
# however, is not meant as a serious model, being too limited.
# There are 3 main ways to operate on myeff.
# 1. Changing myeff in spreadsheet form by the function fix();
# 2. Using RSiena functions "includeEffects", "setEffects", etc;
# 3. Changing myeff directly by operating on its elements.
# Which one to use is a matter of personal preference.
# The second way is most in line with the design philosophy of R,
# and allows you to save scripts that also can be used when
# there will be new versions of RSiena.
# Therefore, we suggest that you skip the explanations of options 1 and 3, and
# proceed directly to option 2, ' Adding/removing effects using includeEffects'.

# For identifying your effects you need the "shortName"s,
# which can be read in the manual (section "Mathematical definition of effects"),
# or obtained from the "effectsDocumentation()" function mentioned above.

# ---- 2. Adding/removing effects using includeEffects -------------------------
# The best way of specifying the model is by the includeEffects function.
# This function uses short names instead of full names.
# The short names are given by the effectsDocumentation() function
# mentioned above, and also are listed in the descriptions given in
# Section 12 of the manual.
# A different table of effect information, including short names,
# that covers effects available for whatever data sets,
# is available as a pdf file in the R directory, and can be opened by

#        RShowDoc("effects", package="RSiena")

# For illustration, let us start from scratch with a new sienaEffects object,
# and add the transitive triples and 3-cycles effects

myeff <- getEffects( mydata )
myeff <- includeEffects( myeff, transTrip, cycle3 )

# To see the current model specification,

myeff

# Note that we can set several effects in one go!
# To remove an effect, e.g., the 3-cycle effects

myeff <- includeEffects( myeff, cycle3, include=FALSE )

# Check again which effects now are included in the model

myeff

# ---- Adding/removing covariate related effects -------------------------------
# The short names do not differentiate between the covariates:
# e.g., the effects 'alcohol ego' and 'smoke1 ego' both have shortName 'egoX',
# and the command

myeff <- includeEffects( myeff, egoX )
# results in a message that does not (like the earlier one)
# confirm the newly included effect.
# The covariates are indicated by the variable "interaction1"
# in the sienaEffects object, listed as "inter1" in the result of
# effectsDocumentation(), and this has to be mentioned to include these effects:

myeff <- includeEffects( myeff, egoX, altX, egoXaltX,
                         interaction1 = "alcohol" )
myeff <- includeEffects( myeff, simX, interaction1 = "sex" )

# We check the results again:

myeff

# By looking at the help offered by

?includeEffects

# you can see how to include endowment and creation effects.
# Effects that depend on other variables, such as egoX, altX, etc. above,
# need the specification of these variables to define them.
# This is done by the interaction1 parameter
# when only one variable name is needed,
# and by interaction2 if there is a second variable involved,
# such as AltsAvAlt (see the manual).
# Although the names of these parameters are interaction1 and interaction2,
# this does not refer to an interaction as commonly used
# in statistical modelling!

# ---- Creating interaction effects --------------------------------------------
# As a special topic, let us show how interaction effects are created.

# A convenient method to include an interaction is offered by the
# includeInteraction function.
# This can be used to interact two or three effects
# (if the interactions are allowed, which depends on their interactionType;
# see the manual for this).
# The interaction between smoke1 ego and reciprocity, for instance,
# can be defined by the command

myeff <- includeInteraction( myeff, egoX, recip,
                             interaction1 = c("sex","") )
myeff

# This shows the interaction as an "unspecified interaction effect";
# but when printing results of the estimation the names of the
# interacting effects will be mentioned.
# E.g., an interaction between smoke1 ego and alcohol ego is defined by

#        myeff <- includeInteraction( myeff, egoX, egoX,
#                                    interaction1 = c( "smoke1", "alcohol" ) )

# Note that the keyword 'interaction1' used by RSiena is used for identifying
# the covariate for which the ego effect is selected, and does not
# refer to the interaction effect itself.
# If at least one of the interacting effects requires the interaction1 parameter
# for it specification, then this parameter is also required for the
# includeInteraction function.
# Then the two or three interaction1 parameters must be combined using c();
# the same goes for interaction2, if that also is necessary for the definition.

# A second special topic is how to access other characteristics of effects.
# This can be done by the setEffect function.
# E.g., the dense triads effects
# counts the number of triplets with at least xx ties,
# where xx is the parameter of the effect, which can be 5 or 6
# (note that 6 is the maximum number of ties in a triplet).
# The default is 5. This is changed to 6 by the command

myeff <- setEffect(myeff, denseTriads, parameter = 6)
myeff

# The 'parameter' keyword refers to the effect parameter, described in
# Section 12 of the manual.



########################### ESTIMATION OF PARAMETERS ###########################

# Parameters of the model are estimated by the function siena07.
# This requires the data specification; the effects specification;
# and a number of parameters, or settings, for the estimation algorithm.
# The latter are contained in an object created by the function
# sienaAlgorithmCreate. You can look at the help provided by
# ?sienaAlgorithmCreate
# to find out about options that you may use here;
# for beginning users, only the two options mentioned below are relevant.
#
# Output will be written to a file with name projname.out, where projname is
# whatever name is given; the default (used if no name is given) is Siena.
# This file will be written to your current directory.
# New estimation runs will append to it.
# A new call to print01Report will overwrite it!

myalgorithm <- sienaAlgorithmCreate(useStdInits = FALSE, projname = 'WCalgInit',MaxDegree=c(friendship=10))
# The useStdInits parameter determines the initial values used for
# the estimation algorithm.
# If useStdInits = TRUE, standard initial values are used;
# if useStdInits = FALSE, the initial values are used that are contained
# in the "initialValue" column of the effects object,
# which are reported by the information request
myeff

# Below we shall see how these initial values can be altered.

# Let us first redefine the model, to obtain a simpler specification
# that will serve as an illustration here.

myeff <- getEffects( mydata )
myeff <- includeEffects( myeff, transTrip, cycle3 )
myeff <- includeEffects( myeff, egoX, altX, egoXaltX,
                         interaction1 = "alcohol" )
myeff <- includeEffects( myeff, simX, interaction1 = "sex" )
myeff

# The function siena07 actually fits the specified model to the data
# If you wish the pretty picture of Siena on the screen as information
# about the progress of the algorithm, type

ans <- siena07( myalgorithm, data = mydata, effects = myeff,nbrNodes=4,useCluster=TRUE,initC=TRUE)

# (ans for "answer").
# If however you do not want the pretty picture, or if this leads to
# difficulties (which may happen e.g. on a Mac), then type

#  	ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)

# and intermediate information will be written to the console.

# Function siena07 produces a so-called sienaFit object, here called ans;
# and it fills in a few things in the sienaEffects object myeff,
# if this is the first use of myeff in a siena07 call.
# By using various different effects objects, i.e., with different names,
# you can switch between specifications.

# The batch = FALSE parameters will give a graphical user interface being opened
# which reports on the progress of the estimation algorithm;
# verbose = TRUE leads to extensive diagnostic information being sent
# to the console during the estimation, and results after the estimation
# (these results are also copied to the output file projname.out, see above);
# while batch=TRUE gives only a limited amount of printout sent to the console
# during the estimation (which is seen when clicking in the console,
# or more immediately if the Buffered Output is deselected in the Misc menu)
# which monitors the progress of the estimation algorithm in a different way.

# The call of siena07 leads to output in the file s50_3.out
# (or more generally projname.out,
# where projname is the name given in sienaAlgorithmCreate)
# and to the creation of the object which here is called ans (for "answer").

# To use multiple processors, in the simplest case where your computer has 2
# processors, use
#  	ans <- siena07( myalgorithm, data = mydata, effects = myeff,
#					  nbrNodes = 2, useCluster = TRUE)

# Adjust the nbrNodes to the number available.
# If you wish to work on with other programs while running siena07,
# it is advisable to use one node less than the number of available processors.
# If you wish to use other machines as well,
# see the more detailed instructions below.
# You will then need to use the clusterString argument as well.
## For more advanced use, it can be helpful to have access to the networks
# simulated in the so-called third phase of the estimation algorithm.
# These networks can be used, e.g., for checking goodness of fit.
# This can be achieved by using the parameter returnDeps=TRUE.
# The fitted object ans will then have a component named "sims"
# which contains a list (each iteration) of lists (each data object)
# of lists (each dependent network or behavior variable) of edgelists for
# networks or vectors for behavior variables.
# See the manual for further explanation.
## This option when used with multiple processors would require
# rather a lot of communication between multiple processes,
# slowing down the computations,
# so it might be better to avoid using the two options together.

################### LOOKING AT THE RESULTS ################################

# The file "s50_3.out" will contain the results of the estimation.
# It is contained in the current directory ("getwd()").
# This file can be read by any text editor.
# A summary of the results is obtained on the screen by

ans
# and a larger summary by

summary(ans)

# Depending on the random seed and the model specification,
# the results could be something like the following.

# Estimates, standard errors and convergence t-ratios
#
#  									Estimate   Standard	  Convergence
#													 Error		t-ratio
#
#Rate parameters:
#  0.1  	Rate parameter period 1		 6.6331	 ( 1.1770	)
#  0.2		Rate parameter period 2		 5.2105	 ( 0.8790	)
#
#Other parameters:
#  1.  eval outdegree (density)			-2.7194	 ( 0.1191	)	 0.0024
#  2.  eval reciprocity					 2.4344	 ( 0.2208	)	 0.0013
#  3.  eval transitive triplets			 0.6449	 ( 0.1378	)	 0.0039
#  4.  eval 3-cycles					-0.0881	 ( 0.2917	)	 0.0168
#  5.  eval smoke1 similarity			 0.2239	 ( 0.1991	)	-0.0792
#  6.  eval alcohol alter				-0.0256	 ( 0.0676	)	-0.0048
#  7.  eval alcohol ego					 0.0348	 ( 0.0730	)	 0.0075
#  8.  eval alcohol ego x alcohol alter	 0.1295	 ( 0.0489	)	 0.0126
#

# The results can also be viewed externally in the output file s50_3.out
# It is advisable that you have a look at all three reports and
# understand how information is organized in each of them.

# To understand the table above, note that the "convergence t-ratio"
# is the t-ratio for convergence checking,
# not the t statistic for testing the significance of this effect!
# (See Section 6.1.2 of the manual.)
# In the external output file, these are called
# "t-ratios for deviations from targets".
# The rule of thumb is that all t-ratios for convergence
# should ideally be less than 0.1 in absolute value;
# this signifies good convergence of the algorithm.
# In the example here, this is the case.
# If this would not be the case, the best thing to do would be
# to continue the estimation, using the estimates produced here,
# and contained in ans, as the new initial values.
# This is explained below.
# Because the estimation algorithm is based on random simulations of the
# network evolution, there always will be small differences between
# different runs of the algorithm.
# To obtain "publication grade" estimates, where this variability is minimized,
# choose the value of parameter n3 in sienaAlgorithmCreate()
# ("Number of iterations in phase 3") larger than the default value of 1000;
# e.g., n3=3000.

# With function siena07 we made ans as the object containing
# all the results of the estimation. For example,
ans$theta

# contains the vector of parameter estimates while

ans$covtheta

# contains the covariance matrix of the estimates.
# There are several "methods" available for viewing the object
# containing the results of the estimation.
# Above we already mentioned
#  	ans
# and
#		summary( ans )
# The command

xtable( ans )
# will produce a table formatted for inclusion in a LaTeX document
# or formatted in html. Use e.g.

xtable( ans, type = 'html' )

# to get html, and e.g.

xtable( ans, file = 'ff.tex' )

# to write the results to a file.
# At http://cran.r-project.org/web/packages/xtable you can find
# a set of vignettes for the xtable package, the xtable gallery,
# which gives more options.
# A function siena.table is available that is specially designed
# for RSiena results and produces html or LaTeX files.
# See

?print.sienaFit
############## MORE ON INITIALIZING PARAMETERS FOR ESTIMATION ########

# If the estimation algorithm has not produced good estimates
# (it 'has not converged well'),
# as will be indicated by some of the t-ratios for convergence being larger
# than 0.1 (this threshold is not to be taken too precisely, though),
# the best thing to do is continuing the estimation,
# using the estimates produced here,
# and contained in ans, as the new initial values.
# This is done by the option prevAns ('previous ans') as in
ans1 <- siena07(myalgorithm, data=mydata, effects=myeff, prevAns=ans,nbrNodes=4,useCluster=TRUE,initC=TRUE)

# the parameter estimates in ans then are extracted and
# used in the new estimation;
# moreover, Phase 1 will be omitted from the algorithm,
# as derivatives and covariance matrix are used from the previous run.
# This should be used only if the model specification in myeff
# has not changed, and if the provisional parameter estimates obtained
# in ans are reasonable; if they are not reasonable,
# omit the prevAns option, use
#  	myalgorithm$useStdInits <- TRUE
# to get back on track, and return at the next estimation to
#  	myalgorithm$useStdInits <- FALSE
# To understand what happens here, read on:

# Another and more flexible way for determining initial values is by
# using the useStdInits element of the model object,
# and the initial values in the effects object.
# This is done as follows.
# The option useStdInits = TRUE in sienaAlgorithmCreate, will make
# each estimation run start with standard initial values.
# The option useStdInits = FALSE makes the estimation start
# with the initial values in the effects object.
# You can switch between these by commands such as
#  	myalgorithm$useStdInits <- FALSE
#		myalgorithm$useStdInits <- TRUE

# Putting the estimates from the results object ans into the
# effects object myeff is done by

myeff <- updateTheta(myeff, ans)

# A check that the effects object contains the desired initial values is made by
myeff

# The initial values are in the vector

myeff$initialValue[myeff$include]

# and this also can be initialised differently, if this is desired.
# Note that this initial vector will be used until you change it again,
# e.g., to the results of a new run, or if you use the prevAns parameter,
# or until you set useStdInits to TRUE.
################################################################################
###
### ---- Testing effects -------------------------------------------------------
###
################################################################################
#
# Two types of tests are available in SIENA.
# 1. t-type tests of single parameters can be carried out by dividing
# the parameter estimate by its standard error.
# Under the null hypothesis that the parameter is 0, these tests have
# approximately a standard normal distribution.
# 2. Score-type tests of single and multiple parameters are described
# in the manual.
# Parameters can be restricted by putting TRUE in the
# include, fix and test columns of the effects object.
# For example, to request a score test for the indegree popularity effect,
# the commands can be as follows.
myeff <- setEffect(myeff, inPopSqrt, fix=TRUE, test=TRUE,
                   initialValue=0.0)

ans <- siena07(myalgorithm, data=mydata, effects=myeff,nbrNodes=4,useCluster=TRUE,initC=TRUE)

# After such an operation, again request

summary(ans)

# to see the results, including those for the score test.

################################################################################
###
### ---- Time test -------------------------------------------------------------
###
################################################################################
#
# An application of the score test is given for the special case of parameter
# heterogeneity by Lospinoso et al. (2010) and implemented in RSiena.
# To apply the test to the results obtained above, request, e.g.,
tt2 <- sienaTimeTest(ans)
tt2
# If you wish more information, also
summary(tt2)
plot(tt2, effects=3:4)
# If as a consequence of this analysis you wish to add time dummy terms,
# this may be done via
myeff <- includeTimeDummy(myeff, transTrip, cycle3)
myeff
ans3 <- siena07(myalgorithm, data=mydata, effects=myeff)
# and testing again,
(tt3 <- sienaTimeTest(ans3))
tt3
summary(tt3)
plot(tt3,effects=3:4)
# and so on.



################################################################################
###
### ---- Summary of model fitted -----------------------------------------------This section just summarises the list of commands 
###
################################################################################
#friend.data.w1 <- as.matrix(read.table("s50-network1.dat")) # read data
#friend.data.w2 <- as.matrix(read.table("s50-network2.dat"))
#friend.data.w3 <- as.matrix(read.table("s50-network3.dat"))
#drink <- as.matrix(read.table("s50-alcohol.dat"))
#smoke <- as.matrix(read.table("s50-smoke.dat"))

#friend.data.w1[ friend.data.w1 %in% c(6,9) ] <- NA # define missing data
#friend.data.w1[ friend.data.w2 %in% c(6,9) ] <- NA
#friend.data.w1[ friend.data.w3 %in% c(6,9) ] <- NA

#friendship <- sienaDependent( array( c( friend.data.w1,
#                                        friend.data.w2, friend.data.w3 #),
#                                     dim = c( 50, 50, 3 ) ) )

#drinkingbeh <- sienaDependent( drink, type = "behavior" )
#smoke1 <- coCovar( smoke[ , 1 ] )
#alcohol <- varCovar( drink )

#mydata <- sienaDataCreate( friendship, smoke1, alcohol )

#myeff <- getEffects( mydata )  # create effects structure

#print01Report( mydata, myeff, modelname = 's50_3_init' )

#myeff <- includeEffects( myeff, transTrip, cycle3 )
#myeff <- includeEffects( myeff, egoX, altX,
#                         egoXaltX, interaction1 = "alcohol" )
#myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )

#myalgorithm <- sienaAlgorithmCreate( projname = 's50_3' )
#ans <- siena07( myalgorithm, data = mydata, effects = myeff)


