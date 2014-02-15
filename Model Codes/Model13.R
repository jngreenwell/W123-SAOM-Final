#Model13 family is for the coevolution of smoking as a dependent variable with the friendship network variable tested against drinking, sex, gender, etc.

# identify dependent behavior variable:
Smoking <- sienaDependent(PersonSmoke,type="behavior")

#Bind data together
Model13data <- sienaDataCreate( friendship, Smoking,Drink,Sex,Ethnic,composition,WCDyads )
Model13data

#create model specification object for the data
Model13eff <- getEffects(Model13data)
Model13eff

print01Report(Model13data,Model13eff,modelname='Model13-init')

# Hypothesis a) Smoking is a cry for help. You start smoking when you don't have many friends; expected sign: - 
Model13eff <- includeEffects(Model13eff,recipDeg,
                                   interaction1="friendship",name="Smoking")
# Hypothesis b) If you want to make new friends, need to smoke cigarettes; expected sign: +
Model13eff <- includeEffects(Model13eff,egoX,
                                   interaction1="Smoking",name="friendship")
# Hypothesis c) Smokers want to be among themselves and non-smokers want to be among themselves; expected sign: +
Model13eff <- includeEffects(Model13eff,simX,
                                   interaction1="Smoking",name="friendship")
# Hypothesis d) Smokers smell bad and it's not particularly inviting; expected sign: -
Model13eff <- includeEffects(Model13eff,altX,
                                   interaction1="Smoking",name="friendship")
# Hypothesis e) Smoking is a social happening. Do as your friends do; expected sign: +
Model13eff <- includeEffects(Model13eff,totSim,
                                   interaction1="friendship",name="Smoking")


# add structural effects as control variables:
Model13eff <- includeEffects(Model13eff,recip,transTrip,cycle3,
                                   name="friendship")

# add sex segregation (and sender & receiver) effects as control variables:
Model13eff <- includeEffects(Model13eff,egoX,altX,
                                   interaction1="Sex",name="friendship")

# ... & for completeness, a main effect of sex on drinking:
Model13eff <- includeEffects(Model13eff,sameX,type='endow',
                                   interaction1="Sex",name="friendship")
Model13eff <- includeEffects(Model13eff,sameX,type='creation',
                                   interaction1="Sex",name="friendship")
# full model looks like this now
Model13eff

# create an object:
Model13gorithm <- sienaAlgorithmCreate(useStdInits=FALSE,n3=3000,MaxDegree=c(friendship=10),
                                           projname='Model13-results')

# estimate the model (using two processor cores):
Model13results <- siena07(Model13gorithm,data=Model13data,
                              effects=Model13eff,nbrNodes=4,useCluster=TRUE,initC=TRUE)
Model13results
#Might need to rerun with prevans
#Model13aresults <- siena07(Model13gorithm,data=Model13data,
 #                         effects=Model13,prevAns=Model13results,nbrNodes=4,useCluster=TRUE,initC=TRUE)
#Model13aresults

Model13results$theta /sqrt(Model13results$covtheta)

#########Try this too for a Score type test
#Model13stt=updateTheta(Model13,Model13aresults)
#
#Model13stt <- setEffect(Model13, inPopSqrt, fix=TRUE, test=TRUE,
#                        initialValue=0.0)
#
#Model13sttresults <- siena07(Model13gorithm, data=Model13data, effects=Model13stt,nbrNodes=4,useCluster=TRUE,initC=TRUE)
#Model13sttresults
#summary(Model13sttresults)

###############
tc<-function(k){
  t=Model13results$theta[k] /sqrt(Model13results$covtheta[k,k])
  p<-2*pnorm(-abs(t))
  return(c(t,p))
}
tc(9)


# (7) moderation analysis (sex variable)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# drop some non-significant effects from the model:
ModerationModel <- includeEffects(Model13eff,egoX,altX,
                                  interaction1="Sex",name="friendship",include=FALSE)
ModerationModel <- includeEffects(ModerationModel,isolate,
                                  interaction1="friendship",name="drinking",include=FALSE)
ModerationModel <- includeEffects(ModerationModel,egoX,altX,
                                  interaction1="drinking",name="friendship",include=FALSE)
ModerationModel <- includeEffects(ModerationModel,effFrom,
                                  interaction1="sexF",name="drinking",include=FALSE)

# include interaction terms of alcohol homophily and peer influence
# with the sex variable:
?includeInteraction
ModerationModel <- includeInteraction(Model13eff,egoX,simX,
                                      interaction1=c("Sex","Smoking"),name="friendship")
ModerationModel <- includeInteraction(ModerationModel,effFrom,totSim,
                                      interaction1=c("Sex","friendship"),name="Smoking")

# How does the model specification look like?
ModerationModel

# estimate the new model specification:
ModerationResults <- siena07(Model13gorithm,data=Model13data,
                             effects=ModerationModel,
                             useCluster=TRUE,nbrNodes=4)
# (when this runs, it is time for a break - takes a while; on my computer ~1000 seconds...)

# take a look at the results:
ModerationResults
ModerationResults$theta /sqrt(ModerationResults$covtheta)
