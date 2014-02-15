#Following are model scripts for SAOMWC123 adapted from the tutorial codes and without many comments.

# We now combine the dependent and independent variables.
# The function sienaDataCreate creates a Siena data object from input networks,
# covariates and composition change objects;
# the objects that earlier were created by sienaDependent will have the role
# of dependent variables, and similarly the other roles are predetermined
# by creation by the functions coCovar, varCovar,
# coDyadCovar, varDyadCovar, and sienaCompositionChange.

##############################################################################
##############################################################################
##############################################################################
# The variants for this model proceed as 12, 12.1, 12.2, 12.3, etc.


######################################################################################
#Model12 - the base model for SAOM with the WC123 data set focusing on the alcohol attribute as an ind variable

#To incorporate all the varCovar, coCovar, dependent vars, dyadCovar, etc.
Model12Data <- sienaDataCreate( friendship, Drink,Sex,Ethnic,composition,WCDyads )
Model12Data

# getEffects function creates a data frame of effects for the model to later take
Model12eff <- getEffects( Model12Data )

#Produce a description of this 
print01Report( Model12Data, Model12eff, modelname = 'Model12' )

#Now to start specifying the model - first take a look at the defrault for the base getEffects object
Model12eff
names(Model12eff)
Model12eff$include

#Now we can add effects to the base model
Model12eff <- includeEffects( Model12eff, transTrip, cycle3 )
Model12eff

#Can also remove an effect by
## To remove an effect, e.g., the 3-cycle effects
#myeff <- includeEffects( myeff, cycle3, include=FALSE )

#interaction effects
Model12eff <- includeEffects( Model12eff, egoX, altX, egoXaltX,
                         interaction1 = "Drink" )
Model12eff <- includeEffects( Model12eff, simX, interaction1 = "Sex" )
Model12eff

# A convenient method to include an interaction is offered by the
# includeInteraction function.
# This can be used to interact two or three effects
# (if the interactions are allowed, which depends on their interactionType;
# see the manual for this).
# The interaction between smoke1 ego and reciprocity, for instance,
# can be defined by the command

#myeff <- includeInteraction( myeff, egoX, recip,
                         #    interaction1 = c("sex","") )

#E.g., an interaction between smoke1 ego and alcohol ego is defined by

#        myeff <- includeInteraction( myeff, egoX, egoX,
#                                    interaction1 = c( "smoke1", "alcohol" ) )

### Now for parameter estimation
Model12gorithm <- sienaAlgorithmCreate(useStdInits = FALSE, n3=3000,projname = 'Model12Init',MaxDegree=c(friendship=10))

#Now for Model Fitting
Model12ans <- siena07( Model12gorithm, data = Model12Data, effects = Model12eff,nbrNodes=4,useCluster=TRUE,initC=TRUE)
Model12ans
summary(Model12ans)

#The use of prevans if almost converged but not quite or not well enough - refer to procedural codes for other options with updatetheta etc.
#Model12ansa <- siena07(Model12gorithm, data=Model12Data, effects=Model12eff, prevAns=Model12ans,nbrNodes=4,useCluster=TRUE,initC=TRUE)

#Testing effects for significance either with a t-type test (Wald-type test) or with score type test
#Model12ans$theta/sqrt(Model12ans$covtheta)



##############################
Model12stt=updateTheta(Model12eff,Model12ans)

Model12stt <- setEffect(Model12eff, inPopSqrt, fix=TRUE, test=TRUE,
                        initialValue=0.0)

Model12sttans <- siena07(Model12gorithm, data=Model12Data, effects=Model12stt,nbrNodes=4,useCluster=TRUE,initC=TRUE)
Model12sttans
summary(Model12sttans)

##########################################################################
##########################################################################
#Model12.1



