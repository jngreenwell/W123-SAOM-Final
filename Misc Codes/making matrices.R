# load vocabulary:
library(network)

setwd("<put appropriate path here>")

# read testfile:
testnodes=as.matrix(read.csv("nodes.test.csv",header=T))
testnodes

# cut into nominator (ego) and nominee (alter) parts:
ego <- testnodes[,1]
alterlist <- testnodes[,-1]
number.egos <- dim(alterlist)[1]
alters.per.ego <- dim(alterlist)[2]

# arrange in egelist form:
testedges <- cbind(
  rep(ego,each=alters.per.ego),
  as.vector(t(alterlist))
)
testedges

# get rid of NA rows:
testedges <- testedges[!is.na(testedges[,2]),]
testedges

# enforce qualitative interpretation of node labels / numbers:
class(testedges[1,1]) <- "character"

# transform to network object:
testnet <- as.network(testedges, matrix.type="edgelist")
testnet

# transform to matrix object (e.g., for RSiena):
testmatrix <- as.matrix(testnet)
testmatrix