install.packages("plyr")
install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
install.packages("fBasics")
install.packages("RColorBrewer")
install.packages("reshape")
install.packages("readr")

library(plyr)
library(readr)
GTD <- read_csv("Data/globalterrorismdb_0617dist.csv", header = TRUE, NA.STRINGS = C("", "."))
View(globalterrorismdb_0617dist)

dim(GTD)


columnsKeep <- c("iyear", "imonth", "iday", "country_txt", "region_txt", "city",
                 "attacktype1_txt", "nkill", "nwound")
GTD <- GTD[columnsKeep]


GTD <- rename(GTD, c("iyear" = "year", "imonth" = "month", "iday" = "day",
                     "country_txt" = "country", "region_txt" = "region", "attacktype1_txt" = "attacktype"))


GTD <- within(GTD, attacktype <- revalue(attacktype,
                                         c("Hostage Taking (Kidnapping)" = "Hostage (Kidnapping)",
                                           "Facility/Infrastructure Attack" = "Facility Attack",
                                           "Hostage Taking (Barricade Incident)" = "Hostage (Barricade)"
                                           )))
GTD <- within(GTD, region <- revalue(region,
                                     c("Australasia & Oceania" = "Oceania",
                                       "Central America & Caribbean" = "Central America",
                                       "Middle East & North Africa" = "Middle East"
                                       )))


GTD$nkill[is.na(GTD$nkill)] <- 0
GTD$nwound[is.na(GTD$nwound)] <- 0


levels(GTD$region) <- c(levels(GTD$region), 'USSR')
GTD$region[GTD$region == 'Central Asia'] <- 'USSR'
GTD$region[GTD$region == 'Russia & the Newly Independent States (NIS)'] <- 'USSR'
GTD <- droplevels(GTD)


write.table(GTD, "globalterrorismdb_clean.csv", sep=",", col.names=TRUE, row.names=FALSE,
            quote=which(colnames(GTD) == 'city'))