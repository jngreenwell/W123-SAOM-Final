################################################################################
## Model selection and RSiena                                                  #
## Script prepared by Josh Lospinoso and Tom Snijders                          #
## Date: May 19, 2013                                                          #
## Version: 11                                                                 #
################################################################################

# In this script, we will load the s50 data (type ?s50 for more information)
# and go through an iteration of forward model selection. The idea is to specify
# a relatively simple model containing a conservative number of effects and then
# evaluate evidence for inclusion of more elaborate terms. This approach
# contrasts with backward model selection, which would entail inclusion of a
# large number of terms and subsequent evaluation for omission of terms.

# It is strongly emphasized here that theory should always guide model
# selection! We follow a data driven approach,
# and do not imply that this is always best.
# But it often is a meaningful component of model building.
# In this script, our intent is to illustrate some of the tools that
# are available to you and not necessarily to provide a "cookie cutter" solution
# to fitting SAOMs to all datasets:
#
# 1) sienaGOF
# 2) The score type test of Schweinberger
# 3) sienaTimeTest

# First, install "RSiena" or "RSienaTest".
# The sienaGOF() functionality in these packages is identical since
# version 1.1-227 (April 2013).
# This script requires version 1.1-227 or higher;
# It is advisable to use the most recent version which can be obtained
# from R-Forge or from the Siena website, depending on your hardware
# (see http://www.stats.ox.ac.uk/~snijders/siena/siena_news.htm).

# Later, we will use the sna and network packages to show GOF functionality.
# If they are not installed yet, install them now:
# install.packages(c("network", "sna"))

# After these packages have been installed, you must tell R to load the
# RSiena or RSienaTest package:
library(RSiena) # or library(RSienaTest)

# First, specify the data.
# The s50 dataset is already loaded into the R environment once the
# library(.) function is called.
# See e.g. ?s501 for information on these data objects.

# Now we load three waves of 50 actors into a sienaDependent object.
friendship <- sienaDependent(array(c(s501, s502, s503), dim=c(50, 50, 3)))
s50data <- sienaDataCreate(friendship)
# Inspect what has been created:
s50data

# Now, give some estimation settings.
# Here we will use 4 phase 2 subphases and 1000 phase 3 iterations.
# These settings are the standard and work well for most datasets.
# They are the default values and therefore do not need to be
# specified explicitly.
estimationSettings <- sienaAlgorithmCreate(n3=3000,projname='s50_GoF')
# For publication-grade results, especially for stability of the
# standard errors, it is advisable to use more (e.g., 3000) phase 3 iterations.

# Here we begin to build up a preliminary model selection.
# By default, this model will have density and reciprocity effects
# included from getEffects(.)
model1 <- getEffects(s50data)
# We add the transitive triplets effects.
model1 <- includeEffects(model1, transTrip, name="friendship")

# Here, we can add a few score type tests for more elaborate
# transitive closure terms.
model1 <- setEffect(model1, cycle3, fix=TRUE, test=TRUE, include=TRUE)
model1 <- setEffect(model1, nbrDist2, fix=TRUE, test=TRUE, include=TRUE)
model1 <- setEffect(model1, transTies, fix=TRUE, test=TRUE, include=TRUE)

# Check what is specified in model1:
model1

# We use the siena07() function to estimate the parameters of model1.
# To use the goodness of fit testing functionality, it is necessary
# to specify returnDeps=TRUE; this will include the simulated networks
# in the results object, and will permit goodness of fit checking later on.
(results1 <- siena07(estimationSettings, data=s50data,
                     effects=model1, returnDeps=TRUE))
# If not all t-ratios for convergence are less than 0.10,
# then re-estimate using results1 as the new starting point:
# results1 <- siena07(estimationSettings, data=s50data,
#                effects=model1, returnDeps=TRUE, prevAns=results1)

# Note that putting the entire command between parentheses leads to
# printing the results obtained after the finish of computations on the screen.
# The convergence t-ratios will be large for some of the fixed effects;
# this is not a problem at all, because the requirement that they
# should be small applies only to the estimated parameters, not the fixed ones.
# However, these printed results do not contain the score-type tests
# requested by "test=TRUE".
# Therefore we request more complete information.
# In the section "Generalised score test <c>", we will see the results.

summary(results1)

# The fit is not satisfactory: the joint test for the three fixed parameters
# is a chi-squared with 3 d.f. and yields p < 0.0001.
# We could at this moment decide to add one of these effects,
# e.g., the number of actors at distance 2 effect.
# For the purpose of illustrating the goodness of fit test, however,
# we first look at what the goodness of fit tells us.

# Let us now calculate the fit with with respect to the indegree distribution.
# By specifying "verbose=TRUE" we get information on the screen telling us
# how far calculations have progressed.
gof1.id <- sienaGOF(results1, verbose=TRUE,
                    varName="friendship", IndegreeDistribution)
gof1.id
plot(gof1.id)
# The default for the InDegreeDistribution is to study the fit of the
# cumulative distribution of the indegrees in the network, summed over all waves
# except for the first.
# See the help page for sienaGOF if you wish to find out about other options.
# The plot shows the observed values as the red dots and numbers;
# e.g., the sum over waves 2 and wave 3 of the numbers of actors with indegree 0
# is 12; the sum of the numbers of actors with indegree at most 3 is 75.
# This applies to the observed data set.
# The rest of the plot refers to the simulated network in Phase 3
# of the algorithm, where it is supposed to have converged
# (if t-ratios for convergence all are less than 0.10).
# The shapes are so-called violin plots, which combine box plots
# with smoothed density plots.
# The dotted band is a pointwise 90% relative frequency region
# calculated for the simulated data.
# The p-value shows that the indegree distribution is represented well.
# The plot shows the same, as the data are within the band.

# Now we go on to assess the fit with respect to the outdegree distribution.
gof1.od <- sienaGOF(results1, verbose=TRUE, varName="friendship",
                    OutdegreeDistribution)
gof1.od
plot(gof1.od)
# Here also the p-value and the plot show that the
# outdegree distribution is well represented.

# Another important set of statistics is the distribution of
# geodesic distances (shortest undirected path lengths between actors).
# We use package sna for calculating the geodesic distances.
# This is not built into RSiena (or RSienaTest) but the help page
# for sienaGOF-auxiliary shows how it can be done

?sienaGOF-auxiliary

# This help page contains the following auxiliary function
# to work with the geodesic distances.

################################################################################
# GeodesicDistribution calculates the distribution of directed
# geodesic distances; see ?sna::geodist
# The default for \code{levls} reflects that geodesic distances larger than 5
# do not differ appreciably with respect to interpretation.
# Note that the levels of the result are named;
# these names are used in the \code{plot} method.
GeodesicDistribution <- function (i, data, sims, period, groupName,
                                  varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(x)$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}
################################################################################

# After having input this function in the R session, and thereby made it
# available, we calculate the fit with respect to the distribution of
# geodesic distances:
gof1.gd <- sienaGOF(results1, verbose=TRUE,
                    varName="friendship", GeodesicDistribution)
gof1.gd
plot(gof1.gd)
# The fit of the distribution of geodesic distances is not acceptable.
# The simulations have too few infinite geodesic distances, i.e.,
# too few pairs of actors that cannot reach each other through a path
# (irrespective of tie direction).
# Our simulations are *too connected*.

# Before we try to remedy this, let us also look at the
# fit for the triad census.
# This also goes by means of a function which you can find on the help page
# for sienaGOF-auxiliary.

################################################################################
# Holland and Leinhardt Triad Census; see ?sna::triad.census.
TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
  unloadNamespace("igraph") # to avoid package clashes
  require(sna)
  require(network)
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  tc <- sna::triad.census(x)[1,levls]
  # triad names are transferred automatically
  tc
}
################################################################################

gof1.tc <- sienaGOF(results1, verbose=TRUE,
                    varName="friendship", TriadCensus)
# This is more time-consuming...
gof1.tc

# Since the triad counts are wildly different in average and scale,
# we plot them using the scale and center options:
plot(gof1.tc, scale=TRUE, center=TRUE)
# If you don't know the codes for the triads, google for them;
# e.g., http://www.stats.ox.ac.uk/~snijders/Trans_Triads_ha.pdf
# or http://eprints.jcu.edu.au/1751/14/14Appendices_23-25.pdf

# The triad census also is unacceptable, although the
# marginal fit across the statistics is not too egregious
# for any particular configuration by itself.
# 111U is over-represented in the simulations, which suggests
# that the precise representation of transitivity is not adequate.

# At this point let us check that the poor fit is not due
# to time heterogeneity:
tt1 <- sienaTimeTest(results1)
summary(tt1)

# No it isn't.

# Let us go back to the score-type tests for model1.
# There the p-value for the number of distances 2 effect was smallest.
# Note that this is a component of the geodesic distance distribution.

model2 <- setEffect(model1, nbrDist2, fix=FALSE, test=FALSE, include=TRUE)
( results2 <- siena07(estimationSettings, data=s50data,
                      effects=model2, returnDeps=TRUE) )

# For the new model, we apply the same goodness of fit tests.
(gof2.id <- sienaGOF(results2, verbose=TRUE,
                     varName="friendship", IndegreeDistribution))
plot(gof2.id)
# Still good.

(gof2.od <- sienaGOF(results2, verbose=TRUE, varName="friendship",
                     OutdegreeDistribution))
plot(gof2.od)
# Still good.

(gof2.gd <- sienaGOF(results2, verbose=TRUE,
                     varName="friendship", GeodesicDistribution))
plot(gof2.gd)
# For this model also good.

(gof2.tc <- sienaGOF(results2, verbose=TRUE,
                     varName="friendship", TriadCensus))
plot(gof2.tc, scale=TRUE, center=TRUE)
# Improved but still not so good.
# Is there now evidence for time heterogeneity?
tt2 <- sienaTimeTest(results2)
summary(tt2)
# No.

# The triads whose fit is worst, are 111U, 120C, and 210.
# All of these are triads that combine reciprocated dyads
# with specific transitivity or non-transitivity.
# Let us add the interaction of transitivity with reciprocity
# as expressed by the transRecTrip effect;
# consult the manual for its definition.

model3 <- includeEffects(model2, transRecTrip)
model3
results3 <- siena07(estimationSettings, data=s50data, effects=model3,
                    returnDeps=TRUE)
results3 <- siena07(estimationSettings, data=s50data, effects=model3,
                    returnDeps=TRUE, prevAns=results3)
summary(results3)

# transRecTrip is strongly significant;
# but also the 3-cycles effect now has become significant in the score test!
# Apparently, the negative and significant transitive reciprocated triplets
# effect produces too few 3-cycles.

# For the new model, we apply the same goodness of fit tests.
(gof3.id <- sienaGOF(results3, verbose=TRUE,
                     varName="friendship", IndegreeDistribution))
plot(gof3.id)
# Still good.

(gof3.od <- sienaGOF(results3, verbose=TRUE, varName="friendship",
                     OutdegreeDistribution))
plot(gof3.od)
# Still good.

(gof3.gd <- sienaGOF(results3, verbose=TRUE,
                     varName="friendship", GeodesicDistribution))
plot(gof3.gd)
# Still good.

(gof3.tc <- sienaGOF(results3, verbose=TRUE,
                     varName="friendship", TriadCensus))
plot(gof3.tc, scale=TRUE, center=TRUE)
# Improved, on the edge of being satisfactory.

# Let us now add the 3-cycles effect to the model,
# which now is significant in the score test.

model4 <- setEffect(model3, cycle3, fix=FALSE, test=FALSE, include=TRUE)
model4
results4 <- siena07(estimationSettings, data=s50data, effects=model4,
                    returnDeps=TRUE)
results4
(gof4.tc <- sienaGOF(results4, verbose=TRUE,
                     varName="friendship", TriadCensus))
plot(gof4.tc, scale=TRUE, center=TRUE)

# Now the fit of the triad census is totally acceptable.
# In order not to overlook anything, let us check the other
# GOF tests and the time homogeneity.

(gof4.id <- sienaGOF(results4, verbose=TRUE,
                     varName="friendship", IndegreeDistribution))
plot(gof4.id)

(gof4.od <- sienaGOF(results4, verbose=TRUE, varName="friendship",
                     OutdegreeDistribution))
plot(gof4.od)

(gof4.gd <- sienaGOF(results4, verbose=TRUE,
                     varName="friendship", GeodesicDistribution))
plot(gof4.gd)

tt4 <- sienaTimeTest(results4)
summary(tt4)

# All these are still good.
