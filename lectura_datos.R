getwd()

SeoulBikeData <- read.csv("datos/SeoulBikeData.csv")

str(SeoulBikeData)
summary(SeoulBikeData)

hist(SeoulBikeData$Rented.Bike.Count,60)
which.min(SeoulBikeData$Rented.Bike.Count)

dim(SeoulBikeData[SeoulBikeData$Rented.Bike.Count == 0,])
dim(SeoulBikeData)
hist(sqrt(SeoulBikeData$Rented.Bike.Count),40)

table(SeoulBikeData$Seasons)


