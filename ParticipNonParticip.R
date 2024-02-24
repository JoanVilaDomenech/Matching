rm(list=ls())
library(compareGroups)
source("./functions/matching_v03.r")

mydata <- read.csv("./data/datExample.CSV", header=TRUE)

dm <- matching(data = mydata,
            name.cascon = "parti",
            case.value=1,
            id.name="ID",
            num.controls=2,
            var.match=c("female", "age", "region" ),
            tol=c(0, 2, 0),
            name.pair="pair",
            seed.cases=09052022,
            seed.controls=09052022+1)

MatchedData <- merge(mydata, dm, by="ID", all.x=T)
MatchedData <- subset(MatchedData, !is.na(pair))

createTable(compareGroups(parti ~ female + age + casp + region,   
              data = MatchedData, method = c(3,1,1,3)))
