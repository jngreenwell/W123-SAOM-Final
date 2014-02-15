#This Model1 follows the logic of Model13 but incorporates Score Type testing (Lospinoso) for GOF

#Start with a simple model only looking at the structural effects
Model1Data <- sienaDataCreate(friendship,composition,WCDyads)
Model1Data

Model1Algorithm=sienaAlgorithmCreate(useStdInits=F,n3=3000,MaxDegree=c(friendship=10),projname='Model1-Results')

Model1 <- getEffects(Model1Data)
Model1

Model1 <- includeEffects(Model1, transTrip, name="friendship")

Model1 <- setEffect(Model1, cycle3, fix=TRUE, test=TRUE, include=TRUE)
Model1 <- setEffect(Model1, nbrDist2, fix=TRUE, test=TRUE, include=TRUE)
Model1 <- setEffect(Model1, transTies, fix=TRUE, test=TRUE, include=TRUE)


Model1

(Model1Results=siena07(Model1Algorithm,data=Model1Data,effects=Model1,returnDeps=T,nbrNodes=4,useCluster=T,initC=T))

summary(Model1Results)

#before adding any of the score-type tested effects it might be good to look at the Gof
gofModel1.id <- sienaGOF(Model1Results, verbose=TRUE,
                    varName="friendship", IndegreeDistribution)
gofModel1.id
plot(gofModel1.id)
#########
gofModel1.od <- sienaGOF(Model1Results, verbose=TRUE, varName="friendship",
                    OutdegreeDistribution)
gofModel1.od
plot(gofModel1.od)
#############
gofModel1.gd <- sienaGOF(Model1Results, verbose=TRUE,
                    varName="friendship", GeodesicDistribution)
gofModel1.gd
plot(gofModel1.gd)
##############
gofModel1.tc <- sienaGOF(Model1Results, verbose=TRUE,
                    varName="friendship", TriadCensus)
gofModel1.tc
plot(gofModel1.tc, scale=TRUE, center=TRUE)
#################
Model1tt <- sienaTimeTest(Model1Results)
summary(Model1tt)
plot(Model1tt,effects=c(1,2,3))
#There is time heterogeneity but effect 1 (outdegree density) has the highest chi-sq so this effect will be added to time heterogeneity
Model1a <- includeTimeDummy(Model1,density,timeDummy="all")
Model1aResults <- siena07(Model1Algorithm, data=Model1Data, effects=Model1a,returnDeps=T,nbrNodes=4,useCluster=T,initC=T)
Model1aResults
summary(Model1aResults)
Model1att<- sienaTimeTest(Model1aResults)
summary(Model1att)
plot(Model1att, effects=c(1,2,3))

################################
#Next can redo the Gof's with the new time dummy variables
gofModel1a.id <- sienaGOF(Model1aResults, verbose=TRUE,
                         varName="friendship", IndegreeDistribution)
gofModel1a.id
plot(gofModel1a.id)
#########
gofModel1a.od <- sienaGOF(Model1aResults, verbose=TRUE, varName="friendship",
                         OutdegreeDistribution)
gofModel1a.od
plot(gofModel1a.od)
#############
gofModel1a.gd <- sienaGOF(Model1aResults, verbose=TRUE,
                         varName="friendship", GeodesicDistribution)
gofModel1a.gd
plot(gofModel1a.gd)
##############
gofModel1a.tc <- sienaGOF(Model1aResults, verbose=TRUE,
                         varName="friendship", TriadCensus)
gofModel1a.tc
plot(gofModel1a.tc, scale=TRUE, center=TRUE)
#################

Model2 <- setEffect(Model1a, cycle3,fix=F,test=F,include=T)
Model2


EstimationSettings=Model1Algorithm
Model2Results <- siena07(EstimationSettings, data=Model1Data, effects=Model2,
                    returnDeps=TRUE,nbrNodes=4,useCluster=T,initC=T)
summary(Model2Results)

#Model2Results <- siena07(EstimationSettings, data=Model1Data, effects=Model2,
#                    returnDeps=TRUE, prevAns=Model2Results,nbrNodes=4,useCluster=T,initC=T)

Model2tt<- sienaTimeTest(Model2Results)
summary(Model2tt)
plot(Model2tt, effects=c(1:4))

#Model3 uses a score-type tet to determine if any of the degree related structural effects should be used 

Model2
Model3=Model2
Model3=setEffect(Model3,X,interaction1='WCDyads',fix=T,test=T,include=T)
Model3
BasicData=Model1Data

(Model3Results <- siena07(EstimationSettings, data=BasicData,
                     effects=Model3, returnDeps=TRUE,nbrNodes=4,useCluster=T,initC=T))
summary(Model3Results)


Model3a <- setEffect(Model3, transTies, fix=F, test=F, include=TRUE)
Model3a

(Model3aResults <- siena07(EstimationSettings, data=BasicData,
                          effects=Model3a, returnDeps=TRUE,nbrNodes=4,useCluster=T,initC=T))
summary(Model3aResults)

#Next can redo the Gof's with the new time dummy variables
gofModel3a.id <- sienaGOF(Model3aResults, verbose=TRUE,
                          varName="friendship", IndegreeDistribution)
gofModel3a.id
plot(gofModel3a.id)
#########
gofModel3a.od <- sienaGOF(Model3aResults, verbose=TRUE, varName="friendship",
                          OutdegreeDistribution)
gofModel3a.od
plot(gofModel3a.od)
#############
gofModel3a.gd <- sienaGOF(Model3aResults, verbose=TRUE,
                          varName="friendship", GeodesicDistribution)
gofModel3a.gd
plot(gofModel3a.gd)
##############
gofModel3a.tc <- sienaGOF(Model3aResults, verbose=TRUE,
                          varName="friendship", TriadCensus)
gofModel3a.tc
plot(gofModel3a.tc, scale=TRUE, center=TRUE)
#################

Model3tt<- sienaTimeTest(Model3Results)
summary(Model3tt)
plot(Model3tt, effects=c(1:5))


######################################

Model3a
Model3b=Model3a
Model3b=setEffect(Model3b,X,interaction1='WCDyads',include=F)
Model3b=setEffect(Model3b,transRecTrip,fix=T,test=T,include=T)
Model3b

Model3bResults <- siena07(EstimationSettings, data=BasicData, effects=Model3b,
                         returnDeps=TRUE,nbrNodes=4,useCluster=T,initC=T)
summary(Model3bResults)


Model3c=Model3b
Model3c=setEffect(Model3c,nbrDist2,fix=F,test=F,include=T)
Model3c

Model3cResults <- siena07(EstimationSettings, data=BasicData, effects=Model3c,
                          returnDeps=TRUE,nbrNodes=4,useCluster=T,initC=T)
summary(Model3cResults)


Model3cResults <- siena07(EstimationSettings, data=BasicData, effects=Model3c,
                          returnDeps=TRUE,prevAns=Model3Results,nbrNodes=4,useCluster=T,initC=T)
summary(Model3cResults)

##############################
gofModel3c.id <- sienaGOF(Model3cResults, verbose=TRUE,
                          varName="friendship", IndegreeDistribution)
gofModel3c.id
plot(gofModel3c.id)
#########
gofModel3c.od <- sienaGOF(Model3cResults, verbose=TRUE, varName="friendship",
                          OutdegreeDistribution)
gofModel3c.od
plot(gofModel3c.od)
#############
gofModel3c.gd <- sienaGOF(Model3cResults, verbose=TRUE,
                          varName="friendship", GeodesicDistribution)
gofModel3c.gd
plot(gofModel3c.gd)
##############
gofModel3c.tc <- sienaGOF(Model3cResults, verbose=TRUE,
                          varName="friendship", TriadCensus)
gofModel3c.tc
plot(gofModel3c.tc, scale=TRUE, center=TRUE)
#################

Model3ctt<- sienaTimeTest(Model3cResults)
summary(Model3ctt)
plot(Model3ctt, effects=c(1:7))

##############
#Now to fix the reciprocity time heterogeneity

Model3d <- includeTimeDummy(Model3c,nbrDist2,timeDummy="all")
Model3d <- setEffect(Model3d,transRecTrip,include=F)
Model3d

Model3dResults <- siena07(EstimationSettings, data=BasicData, effects=Model3d,
                          returnDeps=TRUE,nbrNodes=4,useCluster=T,initC=T)
summary(Model3dResults)


Model3dtt<- sienaTimeTest(Model3dResults)
summary(Model3dtt)
plot(Model3dtt, effects=c(1:6))
##############################
gofModel3d.id <- sienaGOF(Model3dResults, verbose=TRUE,
                          varName="friendship", IndegreeDistribution)
gofModel3d.id
plot(gofModel3d.id)
#########
gofModel3d.od <- sienaGOF(Model3dResults, verbose=TRUE, varName="friendship",
                          OutdegreeDistribution)
gofModel3d.od
plot(gofModel3d.od)
#############
gofModel3d.gd <- sienaGOF(Model3dResults, verbose=TRUE,
                          varName="friendship", GeodesicDistribution)
gofModel3d.gd
plot(gofModel3d.gd)
##############
gofModel3d.tc <- sienaGOF(Model3dResults, verbose=TRUE,
                          varName="friendship", TriadCensus)
gofModel3d.tc
plot(gofModel3d.tc, scale=TRUE, center=TRUE)
#################


#Now to fix the transTrip time heterogeneity

Model3e <- includeTimeDummy(Model3d,transTrip,timeDummy="all")
Model3e

Model3eResults <- siena07(EstimationSettings, data=BasicData, effects=Model3e,
                          returnDeps=TRUE,nbrNodes=4,useCluster=T,initC=T)
summary(Model3eResults)

Model3eResults <- siena07(EstimationSettings, data=BasicData, effects=Model3e,
                          returnDeps=TRUE,prevAns=Model3Results,nbrNodes=4,useCluster=T,initC=T)
summary(Model3eResults)

Model3ett<- sienaTimeTest(Model3eResults)
summary(Model3ett)
plot(Model3ett, effects=c(1:6))
##############################
gofModel3e.id <- sienaGOF(Model3eResults, verbose=TRUE,
                          varName="friendship", IndegreeDistribution)
gofModel3e.id
plot(gofModel3e.id)
#########
gofModel3e.od <- sienaGOF(Model3eResults, verbose=TRUE, varName="friendship",
                          OutdegreeDistribution)
gofModel3e.od
plot(gofModel3e.od)
#############
gofModel3e.gd <- sienaGOF(Model3eResults, verbose=TRUE,
                          varName="friendship", GeodesicDistribution)
gofModel3e.gd
plot(gofModel3e.gd)
##############
gofModel3e.tc <- sienaGOF(Model3eResults, verbose=TRUE,
                          varName="friendship", TriadCensus)
gofModel3e.tc
plot(gofModel3e.tc, scale=TRUE, center=TRUE)
#################

#It might also be good to re-run the models without any time dummy's and then do a time test to see which ones should be added from scratch to come up with the final structural component




#######################
#left of here - 
#########################
###################
#######################









