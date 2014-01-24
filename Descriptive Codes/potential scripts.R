# scripts to consider using for the SAOM WC

#descriptives

# age of respondents:
plot(density(age,na.rm=TRUE))

# sex of respondents:
table(sex.F,useNA='always')

# alcohol use:
dim(alcohol) # 160 students, 3 time points
alcohol[1:5,]
apply(alcohol,FUN=table,MARGIN=2,useNA='always')

# network change between first two observations:
table(friendship.1,friendship.2,useNA='always')
# contains structurally absent ties as well as 
# tie strength (2='just a friend', 1='best friend')

# make friendship matrices binary, keep structural zeros:
friendship.1[friendship.1==2] <- 1
friendship.2[friendship.2==2] <- 1
friendship.3[friendship.3==2] <- 1


########################
########################
######################
# (3) cursory inspection of the data
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# find out what the number of actors in this data set is:
numberActors <- nrow(demographics)
numberActors

# means of demographic variables:
table(demographics$sex)
table(demographics$age)
table(demographics$ethnicity)
table(demographics$religion)

# take a look at the network change between observations,
# e.g., between the first two:
table(friendship.net1,friendship.net2,useNA='always')
# from such tables, Hamming distances and Jaccard indices can be calculated

# Hamming=the number of cells int eh adjacency matrix which are flipping
# 87 opportunities to estimate you parameters
# write two functions calculating these measures: - 
Hamming <- function(changetable) {
  return(changetable[2,1]+changetable[1,2])
}
Jaccard <- function(changetable) {
  return(changetable[2,2]/(changetable[1,2]+changetable[2,1]+changetable[2,2]))
}

# evaluate the functions for the friendship network sequence:
Hamming(table(friendship.net1,friendship.net2))
Hamming(table(friendship.net2,friendship.net3))
Hamming(table(friendship.net3,friendship.net4))
Jaccard(table(friendship.net1,friendship.net2))
Jaccard(table(friendship.net2,friendship.net3))
Jaccard(table(friendship.net3,friendship.net4))
# ... & once more for total change / stability:
Hamming(table(friendship.net1,friendship.net4))
Jaccard(table(friendship.net1,friendship.net4))
