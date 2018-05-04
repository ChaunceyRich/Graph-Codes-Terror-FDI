

source('common.R')


library(maps)
library(mapdata)
data(world.cities)

library(fBasics)  


cities <- table(dat$city)
cities <- sort(cities[cities > 100], decreasing = TRUE)
cities <- cities[-which(names(cities) == 'Unknown')]  
cityNames <- names(cities)
datcities <- subset(GTD, city %in% cityNames)
cityAttacks <- ddply(datcities, country~city, plyrFxCount, "totAttacks")

topNcities <- 20
cityAttacks <- head(arrange(cityAttacks, totAttacks, decreasing = TRUE), n=topNcities)


print(cityAttacks)
write.table(cityAttacks, paste0(resultsDir, "citiesMostAttacked.txt"), quote = FALSE, sep = "\t",
            col.names = TRUE, row.names = FALSE)

cityNameMapping <- c(Bayrut = "Beirut", "Guatemala" = "Guatemala City", "al-Mawsil" = "Mosul")
world.cities <- within(world.cities,
                       name <- revalue(name, cityNameMapping))
cityAttacksFullInfo <- merge(cityAttacks, world.cities,
                             by.x = c('country', 'city'),
                             by.y = c('country.etc', 'name'))

belfast <- cbind(cityAttacks[cityAttacks$city == 'Belfast', ],
                 world.cities[world.cities$name=='Belfast' & world.cities$country.etc == 'UK', ])
belfast$capital <- 1 
cityAttacksFullInfo <- rbind(cityAttacksFullInfo,
                             subset(belfast, select = -c(country.etc, name)))


cityAttacksFullInfo <- arrange(cityAttacksFullInfo, totAttacks)
cityHeatColorRank <- seqPalette(nrow(cityAttacksFullInfo), name = "Reds")
cityAttacksFullInfo$col <- cityHeatColorRank

map('worldHires',fill = TRUE, col = '#FCFCFC')
points(x = cityAttacksFullInfo$long,
       y = cityAttacksFullInfo$lat, 
       col = 'black', pch = 21, cex = 2,
       bg = cityAttacksFullInfo$col)
title(paste('Top', topNcities, 'Most Terror-Attacked Cities'))
dev.print(png, paste0(resultsDir, "mapTop", topNcities, "DangerousCities.png"),
          width = 500, height = 300)
dev.off()


capitalCounts <- table(cityAttacksFullInfo$capital)
names(capitalCounts) <- c('Non-Capital', 'Capital City')
capitalCounts <- as.data.frame(capitalCounts)
capitalCounts$Var1 <- factor(capitalCounts$Var1, levels = rev(levels(capitalCounts$Var1)))
ggplot(capitalCounts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width=0.5, show_guide = FALSE) +
  ggtitle(paste('Distribution of Capital Cities in Top', topNcities,
                'Most Terror-Attacked Cities')) +
  xlab('') +
  ylab(paste('# of Cities in Top', topNcities)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_fill_manual(values = c('cyan3', 'turquoise'))
ggsave(paste0(resultsDir, "capitalsAttacked.png"))
dev.off()  

dangerYear <- 2000
regionDanger <- ddply(subset(dat, year >= dangerYear), ~ region, plyrFxCount, "tot")
heatColors <- seqPalette(max(regionDanger$tot), name = "Reds")
regionDanger$col <- heatColors[regionDanger$tot]


map('worldHires')
for(i in 1:nrow(regionDanger)){
  regionCountries <- subset(dat, region == regionDanger[i,'region'])$country
  regionCountries <- as.character(unique(regionCountries))
  
  
  if (regionDanger[i, 'region'] == 'USSR') {
    regionCountries <- 'USSR'
  } else if (regionDanger[i, 'region'] == 'North America') {
    regionCountries <- c(regionCountries, 'USA')
  } else if (regionDanger[i, 'region'] == 'Western Europe') {
    regionCountries <- c(regionCountries, 'Greenland')
  }

  map('worldHires',
      regions = regionCountries,
      add = TRUE,
      col = regionDanger[i, 'col'], 
      fill = TRUE)
}
title('Heatmap of Terrorist Attacks\nin World Regions Since 1986')
dev.print(png, paste0(resultsDir, "mapRegionIntensities.png"), width = 500, height = 330)
dev.off()

