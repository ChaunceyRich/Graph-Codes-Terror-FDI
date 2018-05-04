
install.packages("plyr")
install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
install.packages("fBasics")
install.packages("RColorBrewer")
install.packages("reshape")
install.packages("readr")
install.packages("ddply")
install.packages("ggplot")

library(plyr)
library(ggplot2)
library(maps)
library(mapdata)
library(fBasics)
library(RColorBrewer)
library(reshape)
library(readr)
library(ddply)
library(ggplot)


source('common.R')


attacktypeDamage <- ddply(dat, ~attacktype, function(x){
  df <- data.frame(c("nkill", "nwound"), c(sum(x$nkill), sum(x$nwound)));
  colnames(df)<-c("stat","value");
  return(df)
})

ggplot(attacktypeDamage, aes(x = attacktype, y = value, fill = stat)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
  coord_flip() +
  ggtitle("Number of People Wounded or Killed\nby Terrorist Attacks Since 1970") +
  xlab("") +
  ylab("# of People") +
  scale_fill_manual(name = "Injury Type", values = c("black", "red"), labels = c('Killed', 'Wounded')) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(face="bold"))
ggsave(paste0(resultsDir, 'globalCasualtiesSince1970.png'))
dev.off()


regionTotal <- ddply(dat, ~region, plyrFxCount)
ggplot(regionTotal, aes(x = region, y = count, fill = region)) +
  geom_bar(stat="identity", show_guide=FALSE) +
  coord_flip() +
  ggtitle("Terrorist Attacks in World Regions Since 1970") +
  xlab("") +
  ylab("# of Attacks") +
  scale_fill_manual(values = regionCol) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(face="bold"))
ggsave(paste0(resultsDir, 'terrorismPerRegionTotal.png'))
dev.off()


regionYear <- ddply(dat, region ~ year, plyrFxCount, "nattacks")


regionYearPossibilities <- merge(regions, unique(dat$year))
regionYear <- merge(regionYear, regionYearPossibilities,
                    by.x = c('region','year'), by.y = c("x","y"), all.y = TRUE)
regionYear$nattacks[is.na(regionYear$nattacks)] <- 0

ggplot(regionYear, aes(x = year, y = nattacks, color = region)) +
  geom_line(show_guide=FALSE) +
  geom_point(show_guide=FALSE) +
  xlab("Year") + 
  ggtitle("Number of Terrorist Attacks in World Regions Since 1970") + 
  ylab("# of Attacks") +
  facet_wrap(~region) +
  scale_color_manual(values = regionCol) + 
  theme(strip.text = element_text(face="bold"),
        plot.title = element_text(face="bold"))
ggsave(paste0(resultsDir, 'terrorismPerRegionYears.png'))
dev.off()


ggplot(regionYear, aes(x = year, y = nattacks, color = region)) +
  geom_line() +
  geom_point() +
  xlab("Year") + 
  ggtitle("Number of Terrorist Attacks in World Regions Since 1970") + 
  ylab("# of Attacks") +
  scale_color_manual(values = regionCol) + 
  theme(legend.justification = c(0,1), legend.position = c(0,1), legend.title = element_blank(),
        plot.title = element_text(face="bold")) +
  guides(col = guide_legend(ncol = 2))
ggsave(paste0(resultsDir, 'terrorismPerRegionYearsComb.png'))
dev.off()


yearBucketSize <- 5
breaks <- seq(from = min(regionYear$year), to = max(regionYear$year), by = yearBucketSize)
bins <- cut(regionYear$year, breaks = breaks, include.lowest=TRUE, right=FALSE)
regionYear$bin <- bins


regionYear <- regionYear[complete.cases(regionYear), ]


regionYearBin <- ddply(regionYear, region ~ bin, plyrFxSum, "nattacks", "nattacks")
ggplot(regionYearBin, aes(x = region, y = nattacks, fill = region)) +
  geom_bar(stat = "identity", show_guide=FALSE) +
  facet_wrap(~bin, ncol = 4) +
  ylab("# of Attacks") + 
  ggtitle("Number of Terrorist Attacks in World Regions\nin 5-Year Intervals Since 1970") + 
  xlab("") +
  coord_flip() +  
  scale_fill_manual(values = regionCol) +
  theme(panel.background = element_rect(fill='#EEEEEE'),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face="bold"))
ggsave(paste0(resultsDir, 'terrorismPerRegion5Year.png'))
dev.off()


countriesTotal <- ddply(dat, .(country, region), plyrFxCount, "nattacks")
ggplot(countriesTotal, aes(x = nattacks, y = region, color = region, cex = 1.7)) +
  geom_jitter(position = position_jitter(height = 0.4), show_guide=FALSE) +
  ggtitle("Variation in Number of Attacks in Different Countries\nWithin Each Region Since 1970") +
  xlab("# of Attacks") +
  ylab("") +
  scale_color_manual(values = regionCol) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#EEEEEE"),
        panel.background = element_rect(fill = '#FCFCFC', colour = '#D3D3D3'),
        plot.title = element_text(face="bold"))
ggsave(paste0(resultsDir, 'countryVariationPerRegion.png'))
dev.off()


boringRegions <- rev(levels(countriesTotal$region))[1:3]
countriesTotalSubset <- subset(countriesTotal, !(region %in% boringRegions))
topNcountries <- 5
topNcountriesRegion <-
  ddply(countriesTotalSubset, ~region, function(x) {
    x <- arrange(x, -nattacks)
    x <- head(x, n = topNcountries)
    return(x)
  })
 
topNcountriesRegion <- subset(topNcountriesRegion, select = c("region", "country", "nattacks"))
colnames(topNcountriesRegion) <- c('Region', 'Country', '# Attacks')
write.table(topNcountriesRegion, paste0(resultsDir, "countriesMostAttackedPerRegion.txt"),
            quote = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE)


regionAttacktype <- ddply(dat, region ~ attacktype, plyrFxCount)
ggplot(regionAttacktype, aes(x = attacktype, y = count, fill = attacktype)) +
  geom_bar(stat="identity", show_guide=FALSE) +
  facet_wrap(~region) +
  coord_flip() +
  ggtitle("Terrorist Attack Types in World Regions Since 1970") +
  xlab("") +
  ylab("# of Attacks") +
  scale_fill_manual(values = attacktypeCol) +
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face="bold"))
ggsave(paste0(resultsDir, 'attackTypesPerRegion.png'))
dev.off()


