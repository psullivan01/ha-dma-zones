##################
# Load Libraries
##################

library(zipcode)
library(ggplot2)
library(utils)
library(geosphere)
library(randomForest)
library(plyr)

##################
# Load data
##################

data(zipcode)
dma.data <- read.csv("//Users//psullivan01//Documents//dmazip.csv", 
                     header=TRUE, sep=",")
zip.pop <- read.csv("//Users//psullivan01//Documents//Popbyzip.csv", 
                    header=TRUE, sep=",")	#zip code and population (33,092 rows)
dma.data2 <- read.csv("//Users//psullivan01//Documents//ziptodma.csv", 
                      header=TRUE, sep=",")

##################
# DMA Cleanup
##################

# deduplicate
dma.data2 <- dma.data2[!duplicated(dma.data2), ]
dma.data2$numeraire <- 1

# remove remaining zips with 2 entries - split across DMAs
zipcount <- aggregate(numeraire ~ nzip + LONGITUDE + LATITUDE, data=dma.data2, FUN=sum)
zipcount <- subset(zipcount, numeraire>1)
dma.data2 <- merge(dma.data2, zipcount, by="nzip", all.x=TRUE)
dma.data2 <- dma.data2[rowSums(is.na(dma.data2)) > 0,]
dma.data2<- dma.data2[1:5]
colnames(dma.data2) <- c("zip", "latitude", "longitude", "dma", "dmaname")

# build model off of zips with no overlap
dma.data2$dmafactor <- as.factor(dma.data2$dma)
set.seed(1234)
zip.RF <- randomForest(dmafactor ~ longitude + latitude, data = dma.data2)

# apply model to zips with multiple DMAs
colnames(zipcount) <- c("zip", "longitude", "latitude", "numeraire")
zipcount$dma <- predict(zip.RF, newdata=zipcount)
zipcount <- subset(zipcount, select=c('zip', 'longitude', 'latitude', 'dma'))

# Update misclassified zips with correct dma - forgot to set.seed on first run resulting in different results, no longer an issue with set.seed(1234)
zips <- c("2347", "13485","17017","22849","22853","26440","28447","28478",
          "29037","30731","31002","31552","31764","31787","36453","37354",
          "38201","38625","39362","39456","39841","40823","42731","43840",
          "44822","45672","46982","47948","48435","48873","49038","49228",
          "49247","52776","53079","54460","54859","55985","56013","56027",
          "58386","60922","61560","62424","62553","62621","63673","64474",
          "68768","69334","71841","74571","75491","75686","76372","79248",
          "81416","95563")
dmas <- c("521","526","566","569","511","598","550","545","567","575","520",
          "525","522","522","606","557","632","640","710","686","525","557",
          "659","510","535","535","588","602","505","563","588","505","547",
          "682","658","705","676","613","737","611","724","648","682","648",
          "648","648","609","638","652","764","693","671","657","612","662",
          "634","751","802")
zips <- data.frame(zips, dmas)
colnames(zips) <- c('zip', 'dmas')
zipcount <- merge(zipcount, zips, by="zip", all=TRUE)
zipcount$dma <- as.numeric(levels(zipcount$dma))[zipcount$dma] 
zipcount$dmas <- as.numeric(levels(zipcount$dmas))[zipcount$dmas]
zipcount$dma <- ifelse(is.na(zipcount$dmas), zipcount$dma, zipcount$dmas)
zipcount <- zipcount[1:4]

# create dmaname lookup table and merge with zipcount
dmalookup <- subset(dma.data2, select=c("dma", "dmaname"))
dmalookup <- dmalookup[!duplicated(dmalookup), ]
zipcount <- merge(zipcount, dmalookup, by="dma")

# Fold predictions back into original dataset
dma.data2 <- dma.data2[1:5]
dma.data2 <- rbind(dma.data2, zipcount)

##################
# Format data
##################

dma.rank <- subset(dma.data, select=c("rank", "dma"))
dma.rank <- dma.rank[!duplicated(dma.rank), ]

# filter for top 25 DMAs				
colnames(zip.pop) <- c("zip", "pop")
dma.data2 <- merge(dma.data2, dma.rank, by="dma")

# add population data	
zip.pop <- zip.pop[!duplicated(zip.pop), ]
zip.pop <- aggregate(pop ~ zip, data=zip.pop, FUN=sum)		
zip.data <- merge(dma.data2, zip.pop, by="zip")

# Add leading zeros to zips shorter than 5 characters
zip.data$zip <- as.character(zip.data$zip)
zip.data$zip <- ifelse(nchar(zip.data$zip, type="chars")==4, 
                       paste0("0", zip.data$zip), zip.data$zip)
zip.data$zip <- ifelse(nchar(zip.data$zip, type="chars")==3, 
                       paste0("00", zip.data$zip), zip.data$zip)

# Add city and state
zip.data <- merge(zip.data, zipcode, by="zip")
zip.data <- zip.data[1:9]
colnames(zip.data)[3:4] <- c("latitude", "longitude")

# Calculate DMA populations 
dmapop <- aggregate(pop ~ dmaname, data=zip.data, sum)
colnames(dmapop) <- c("dmaname", "dmapop")
zip.data <- merge(zip.data, dmapop, by="dmaname")
zip.data$optimalsplit <- as.integer(zip.data$dmapop/1500000)+1
zip.data$optimalpercent <- 1/zip.data$optimalsplit

# Update Wheatfield, IN with correct coordinates
zip.data[1755,4:5] <- c(41.02044, -87.1024)

# Force St. Louis Optimal Split to 2 instead of 3
zip.data$optimalsplit <- ifelse(zip.data$dmaname=='ST. LOUIS', 2, zip.data$optimalsplit)
zip.data$optimalpercent <- ifelse(zip.data$dmaname=='ST. LOUIS', .5, zip.data$optimalpercent)

########################
# Create objects - North
########################

zip.data.n <- zip.data				
remainder.data.n <- 0
zip.data.n$zone <- 0
zip.data.n$latrank <- 0

dma.data.n <- list()
dmadata.n <- list()
city.data.n <- list()

#####################
# Begin loop - North
#####################

for (n in 1:210) #by dma rank
{
  my.data.n <- subset(zip.data, rank==n) #subsets by DMA rank
  my.data.n$ziprank <- rank(-my.data.n$pop, ties.method = c("first"), 0)
  citydata.n <- my.data.n
  
  for (c in 1:citydata.n$optimalsplit) #by city rank
  {
    citydata.n <- citydata.n[order(-citydata.n$latitude),]
    citydata.n$latrank <- rank(-citydata.n$latitude, ties.method = c("first"), 0)
    northmost <- subset(citydata.n, latrank==1, select=c("dmaname", "zip", 
                                                         "longitude", "latitude"))
    colnames(northmost) <- c("dmaname", "nzip", "nlongitude", "nlatitude")
    allzips.n <- merge(northmost, citydata.n, by="dmaname")
    distance.n <- 0
    result.data.n <- list()
    
    for (i in 1:nrow(allzips.n)) #calculate distances
    {
      record.n <- subset(allzips.n, as.numeric(rownames(allzips.n)) == i)
      distance.n[i] <- distm(record.n[,c('nlongitude', 'nlatitude')],
                             record.n[,c('longitude', 'latitude')],
                             fun = distHaversine)
      record.n$distance <- distance.n[i]
      result.data.n[[i]] <- record.n
    }
    allzips.n <- do.call(rbind, result.data.n)
    allzips.n <- allzips.n[order(allzips.n$distance),]
    
    allzips.n$runpop <- cumsum(allzips.n$pop)
    allzips.n$runpercent <- allzips.n$runpop/allzips.n$dmapop
    allzips.n$zone <-	ifelse(allzips.n$runpercent < allzips.n$optimalpercent, c, 0)
    
    city.data.n[[c]] <- subset(allzips.n, zone > 0)
    citydata.n <- subset(citydata.n, !(zip %in% city.data.n[[c]]$zip))
  }
  mydmadata.n <- do.call(rbind, city.data.n)
  dmadata.n[[n]] <- mydmadata.n
  my.data.n <- subset(my.data.n, !(zip %in% dmadata.n[[n]]$zip))
}
finaldata.n <- do.call(rbind, dmadata.n)
remainder.data.n <- subset(zip.data, !(zip %in% finaldata.n$zip))
remainder.data.n$distance <- 0
remainder.data.n$zone <- remainder.data.n$optimalsplit

########################
# Format results - North
########################

finaldata.n <- subset(finaldata.n, select=c('dmaname', 'zip', 'rank', 'dma' , 
                                            'city', 'pop', 'state', 'latitude', 'longitude', 
                                            'dmapop', 'optimalsplit', 'optimalpercent', 'zone', 'distance'))
remainder.data.n$distance <- 0
finaldata.n <- rbind(finaldata.n, remainder.data.n)
finaldata.n <- finaldata.n[!duplicated(finaldata.n), ]
finaldata.n$zone <- ifelse(nchar(finaldata.n$zone, type="chars")==1, paste0("0", 
                                                                            finaldata.n$zone), finaldata.n$zone)
finaldata.n$zoneid <- paste(finaldata.n$dma, finaldata.n$zone, sep="-")

#Calculate Zone Population
zonepop.n <- aggregate(pop ~ zoneid, data=finaldata.n, sum)
colnames(zonepop.n) <- c("zoneid", "zonepop")
finaldata.n <- merge(finaldata.n, zonepop.n, by="zoneid")

finaldata.n <- finaldata.n[ , !(names(finaldata.n) %in% c('city'))]
finaldata.n <- finaldata.n[!duplicated(finaldata.n), ]
finaldata.n$method <- "Auto"
finaldata.n$direction <- "North"

##################
# Create objects - East
##################	

zip.data.e <- zip.data				
remainder.data.e <- 0
zip.data.e$zone <- 0
zip.data.e$latrank <- 0

dma.data.e <- list()
dmadata.e <- list()
city.data.e <- list()

#####################
# Begin loop - East
#####################

for (n in 1:210) #by dma rank
{
  my.data.e <- subset(zip.data, rank==n) #subsets by DMA rank
  my.data.e$ziprank <- rank(-my.data.e$pop, ties.method = c("first"), 0)
  citydata.e <- my.data.e
  
  for (c in 1:citydata.e$optimalsplit) #by city rank
  {
    citydata.e <- citydata.e[order(-citydata.e$longitude),]
    citydata.e$latrank <- rank(-citydata.e$longitude, ties.method = c("first"), 0)
    northmost <- subset(citydata.e, latrank==1, select=c("dmaname", "zip", 
                                                         "longitude", "latitude"))
    colnames(northmost) <- c("dmaname", "nzip", "nlongitude", "nlatitude")
    allzips.e <- merge(northmost, citydata.e, by="dmaname")
    distance.e <- 0
    result.data.e <- list()
    
    for (i in 1:nrow(allzips.e)) #calculate distances
    {
      record.e <- subset(allzips.e, as.numeric(rownames(allzips.e)) == i)
      distance.e[i] <- distm(record.e[,c('nlongitude', 'nlatitude')],
                             record.e[,c('longitude', 'latitude')],
                             fun = distHaversine)
      record.e$distance <- distance.e[i]
      result.data.e[[i]] <- record.e
    }
    allzips.e <- do.call(rbind, result.data.e)
    allzips.e <- allzips.e[order(allzips.e$distance),]
    
    allzips.e$runpop <- cumsum(allzips.e$pop)
    allzips.e$runpercent <- allzips.e$runpop/allzips.e$dmapop
    allzips.e$zone <-	ifelse(allzips.e$runpercent < allzips.e$optimalpercent, c, 0)
    
    city.data.e[[c]] <- subset(allzips.e, zone > 0)
    citydata.e <- subset(citydata.e, !(zip %in% city.data.e[[c]]$zip))
  }
  mydmadata.e <- do.call(rbind, city.data.e)
  dmadata.e[[n]] <- mydmadata.e
  my.data.e <- subset(my.data.e, !(zip %in% dmadata.e[[n]]$zip))
}
finaldata.e <- do.call(rbind, dmadata.e)
remainder.data.e <- subset(zip.data, !(zip %in% finaldata.e$zip))
remainder.data.n$distance <- 0
remainder.data.e$zone <- remainder.data.e$optimalsplit

########################
# Format results - East
########################

finaldata.e <- subset(finaldata.e, select=c('dmaname', 'zip', 'rank', 'dma' , 
                                            'city', 'pop', 'state', 'latitude', 'longitude', 
                                            'dmapop', 'optimalsplit', 'optimalpercent', 'zone', 'distance'))
remainder.data.e$distance <- 0
finaldata.e <- rbind(finaldata.e, remainder.data.e)
finaldata.e <- finaldata.e[!duplicated(finaldata.e), ]
finaldata.e$zone <- ifelse(nchar(finaldata.e$zone, type="chars")==1, paste0("0", 
                                                                            finaldata.e$zone), finaldata.e$zone)
finaldata.e$zoneid <- paste(finaldata.e$dma, finaldata.e$zone, sep="-")

#Calculate Zone Population
zonepop.e <- aggregate(pop ~ zoneid, data=finaldata.e, sum)
colnames(zonepop.e) <- c("zoneid", "zonepop")
finaldata.e <- merge(finaldata.e, zonepop.e, by="zoneid")

finaldata.e <- finaldata.e[ , !(names(finaldata.e) %in% c('city'))]
finaldata.e <- finaldata.e[!duplicated(finaldata.e), ]

finaldata.e$method <- "Auto"
finaldata.e$direction <- "East"

########################
# Create objects - South
########################

zip.data.s <- zip.data				
remainder.data.s <- 0
zip.data.s$zone <- 0
zip.data.s$latrank <- 0

dma.data.s <- list()
dmadata.s <- list()
city.data.s <- list()

#####################
# Begin loop - South
#####################

for (n in 1:210) #by dma rank
{
  my.data.s <- subset(zip.data, rank==n) #subsets by DMA rank
  my.data.s$ziprank <- rank(-my.data.s$pop, ties.method = c("first"), 0)
  citydata.s <- my.data.s
  
  for (c in 1:citydata.s$optimalsplit) #by city rank
  {
    citydata.s <- citydata.s[order(citydata.s$latitude),]
    citydata.s$latrank <- rank(citydata.s$latitude, ties.method = c("first"), 0)
    northmost <- subset(citydata.s, latrank==1, select=c("dmaname", "zip", 
                                                         "longitude", "latitude"))
    colnames(northmost) <- c("dmaname", "nzip", "nlongitude", "nlatitude")
    allzips.s <- merge(northmost, citydata.s, by="dmaname")
    distance.s <- 0
    result.data.s <- list()
    
    for (i in 1:nrow(allzips.s)) #calculate distances
    {
      record.s <- subset(allzips.s, as.numeric(rownames(allzips.s)) == i)
      distance.s[i] <- distm(record.s[,c('nlongitude', 'nlatitude')],
                             record.s[,c('longitude', 'latitude')],
                             fun = distHaversine)
      record.s$distance <- distance.s[i]
      result.data.s[[i]] <- record.s
    }
    allzips.s <- do.call(rbind, result.data.s)
    allzips.s <- allzips.s[order(allzips.s$distance),]
    
    allzips.s$runpop <- cumsum(allzips.s$pop)
    allzips.s$runpercent <- allzips.s$runpop/allzips.s$dmapop
    allzips.s$zone <-	ifelse(allzips.s$runpercent < allzips.s$optimalpercent, c, 0)
    
    city.data.s[[c]] <- subset(allzips.s, zone > 0)
    citydata.s <- subset(citydata.s, !(zip %in% city.data.s[[c]]$zip))
  }
  mydmadata.s <- do.call(rbind, city.data.s)
  dmadata.s[[n]] <- mydmadata.s
  my.data.s <- subset(my.data.s, !(zip %in% dmadata.s[[n]]$zip))
}
finaldata.s <- do.call(rbind, dmadata.s)
remainder.data.s <- subset(zip.data, !(zip %in% finaldata.s$zip))
remainder.data.n$distance <- 0
remainder.data.s$zone <- remainder.data.s$optimalsplit

########################
# Format results - South
########################

finaldata.s <- subset(finaldata.s, select=c('dmaname', 'zip', 'rank', 'dma' , 
                                            'city', 'pop', 'state', 'latitude', 'longitude', 
                                            'dmapop', 'optimalsplit', 'optimalpercent', 'zone', 'distance'))
remainder.data.s$distance <- 0
finaldata.s <- rbind(finaldata.s, remainder.data.s)
finaldata.s <- finaldata.s[!duplicated(finaldata.s), ]
finaldata.s$zone <- ifelse(nchar(finaldata.s$zone, type="chars")==1, paste0("0", 
                                                                            finaldata.s$zone), finaldata.s$zone)
finaldata.s$zoneid <- paste(finaldata.s$dma, finaldata.s$zone, sep="-")

#Calculate Zone Population
zonepop.s <- aggregate(pop ~ zoneid, data=finaldata.s, sum)
colnames(zonepop.s) <- c("zoneid", "zonepop")
finaldata.s <- merge(finaldata.s, zonepop.s, by="zoneid")

finaldata.s <- finaldata.s[ , !(names(finaldata.s) %in% c('city'))]
finaldata.s <- finaldata.s[!duplicated(finaldata.s), ]

finaldata.s$method <- "Auto"
finaldata.s$direction <- "South"

#######################
# Create objects - West
#######################

zip.data.w <- zip.data					
remainder.data.w <- 0
zip.data.w$zone <- 0
zip.data.w$latrank <- 0

dma.data.w <- list()
dmadata.w <- list()
city.data.w <- list()

#####################
# Begin loop - West
#####################

for (n in 1:210) #by dma rank
{
  my.data.w <- subset(zip.data, rank==n) #subsets by DMA rank
  my.data.w$ziprank <- rank(-my.data.w$pop, ties.method = c("first"), 0)
  citydata.w <- my.data.w
  
  for (c in 1:citydata.w$optimalsplit) #by city rank
  {
    citydata.w <- citydata.w[order(citydata.w$longitude),]
    citydata.w$latrank <- rank(citydata.w$longitude, ties.method = c("first"), 0)
    northmost <- subset(citydata.w, latrank==1, select=c("dmaname", "zip", 
                                                         "longitude", "latitude"))
    colnames(northmost) <- c("dmaname", "nzip", "nlongitude", "nlatitude")
    allzips.w <- merge(northmost, citydata.w, by="dmaname")
    distance.w <- 0
    result.data.w <- list()
    
    for (i in 1:nrow(allzips.w)) #calculate distances
    {
      record.w <- subset(allzips.w, as.numeric(rownames(allzips.w)) == i)
      distance.w[i] <- distm(record.w[,c('nlongitude', 'nlatitude')],
                             record.w[,c('longitude', 'latitude')],
                             fun = distHaversine)
      record.w$distance <- distance.w[i]
      result.data.w[[i]] <- record.w
    }
    allzips.w <- do.call(rbind, result.data.w)
    allzips.w <- allzips.w[order(allzips.w$distance),]
    
    allzips.w$runpop <- cumsum(allzips.w$pop)
    allzips.w$runpercent <- allzips.w$runpop/allzips.w$dmapop
    allzips.w$zone <-	ifelse(allzips.w$runpercent < allzips.w$optimalpercent, c, 0)
    
    city.data.w[[c]] <- subset(allzips.w, zone > 0)
    citydata.w <- subset(citydata.w, !(zip %in% city.data.w[[c]]$zip))
  }
  mydmadata.w <- do.call(rbind, city.data.w)
  dmadata.w[[n]] <- mydmadata.w
  my.data.w <- subset(my.data.w, !(zip %in% dmadata.w[[n]]$zip))
}
finaldata.w <- do.call(rbind, dmadata.w)
remainder.data.w <- subset(zip.data, !(zip %in% finaldata.w$zip))
remainder.data.n$distance <- 0
remainder.data.w$zone <- remainder.data.w$optimalsplit

########################
# Format results - West
########################

finaldata.w <- subset(finaldata.w, select=c('dmaname', 'zip', 'rank', 'dma' , 
                                            'city', 'pop', 'state', 'latitude', 'longitude', 
                                            'dmapop', 'optimalsplit', 'optimalpercent', 'zone', 'distance'))
remainder.data.w$distance <- 0
finaldata.w <- rbind(finaldata.w, remainder.data.w)
finaldata.w <- finaldata.w[!duplicated(finaldata.w), ]
finaldata.w$zone <- ifelse(nchar(finaldata.w$zone, type="chars")==1, paste0("0", 
                                                                            finaldata.w$zone), finaldata.w$zone)
finaldata.w$zoneid <- paste(finaldata.w$dma, finaldata.w$zone, sep="-")

#Calculate Zone Population
zonepop.w <- aggregate(pop ~ zoneid, data=finaldata.w, sum)
colnames(zonepop.w) <- c("zoneid", "zonepop")
finaldata.w <- merge(finaldata.w, zonepop.w, by="zoneid")

finaldata.w <- finaldata.w[ , !(names(finaldata.w) %in% c('city'))]
finaldata.w <- finaldata.w[!duplicated(finaldata.w), ]

finaldata.w$method <- "Auto"
finaldata.w$direction <- "West"

#############################
# Manually Assign
#############################

finaldata.m <- rbind(subset(finaldata.w, rank==1), subset(finaldata.s, rank==2),
                     subset(finaldata.e, rank==3), subset(finaldata.w, rank==4),
                     subset(finaldata.n, rank==5), subset(finaldata.e, rank==6),
                     subset(finaldata.n, rank==7), subset(finaldata.n, rank==8),
                     subset(finaldata.s, rank==9), subset(finaldata.n, rank==10),
                     subset(finaldata.n, rank==11), subset(finaldata.s, rank==12),
                     subset(finaldata.s, rank==13), subset(finaldata.s, rank==14),
                     subset(finaldata.s, rank==15), subset(finaldata.s, rank==16),
                     subset(finaldata.s, rank==17), subset(finaldata.e, rank==18),
                     subset(finaldata.n, rank==19), subset(finaldata.s, rank==20),
                     subset(finaldata.n, rank==21), subset(finaldata.w, rank==22),
                     subset(finaldata.s, rank==23), subset(finaldata.n, rank==24),
                     subset(finaldata.n, rank==25), subset(finaldata.w, rank==26),
                     subset(finaldata.n, rank==27), subset(finaldata.n, rank==28),
                     subset(finaldata.n, rank==29), subset(finaldata.n, rank==30),
                     subset(finaldata.n, rank==31), subset(finaldata.n, rank==32),
                     subset(finaldata.n, rank==33), subset(finaldata.n, rank==34),
                     subset(finaldata.n, rank==35), subset(finaldata.s, rank==36),
                     subset(finaldata.s, rank==37), subset(finaldata.n, rank==38),
                     subset(finaldata.n, rank==39), subset(finaldata.n, rank==40),
                     subset(finaldata.n, rank==41), subset(finaldata.n, rank==42),
                     subset(finaldata.n, rank==43), subset(finaldata.n, rank==44),
                     subset(finaldata.w, rank==45), subset(finaldata.n, rank==46),
                     subset(finaldata.n, rank==47), subset(finaldata.n, rank==48),
                     subset(finaldata.n, rank==49), subset(finaldata.n, rank==50),
                     subset(finaldata.n, rank==51), subset(finaldata.n, rank==52),
                     subset(finaldata.n, rank==53), subset(finaldata.s, rank==54),
                     subset(finaldata.n, rank==55), subset(finaldata.n, rank==56),
                     subset(finaldata.n, rank==57), subset(finaldata.n, rank==58),
                     subset(finaldata.n, rank==59), subset(finaldata.n, rank==60),
                     subset(finaldata.w, rank==61), subset(finaldata.n, rank==62),
                     subset(finaldata.n, rank==63), subset(finaldata.n, rank==64),
                     subset(finaldata.n, rank==65), subset(finaldata.n, rank==66),
                     subset(finaldata.n, rank==67), subset(finaldata.n, rank==68),
                     subset(finaldata.n, rank==69), subset(finaldata.n, rank==70),
                     subset(finaldata.n, rank==71), subset(finaldata.n, rank==72),
                     subset(finaldata.n, rank==73), subset(finaldata.n, rank==74),
                     subset(finaldata.n, rank==75), subset(finaldata.n, rank==76),
                     subset(finaldata.n, rank==77), subset(finaldata.n, rank==78),
                     subset(finaldata.n, rank==79), subset(finaldata.n, rank==80),
                     subset(finaldata.n, rank==81), subset(finaldata.n, rank==82),
                     subset(finaldata.n, rank==83), subset(finaldata.n, rank==84),
                     subset(finaldata.n, rank==85), subset(finaldata.n, rank==86),
                     subset(finaldata.n, rank==87), subset(finaldata.n, rank==88),
                     subset(finaldata.n, rank==89), subset(finaldata.n, rank==90),
                     subset(finaldata.n, rank==91), subset(finaldata.n, rank==92),
                     subset(finaldata.n, rank==93), subset(finaldata.n, rank==94),
                     subset(finaldata.n, rank==95), subset(finaldata.n, rank==96),
                     subset(finaldata.n, rank==97), subset(finaldata.n, rank==98),
                     subset(finaldata.n, rank==99), subset(finaldata.n, rank==100),
                     subset(finaldata.n, rank==101), subset(finaldata.n, rank==102),
                     subset(finaldata.n, rank==103), subset(finaldata.n, rank==104),
                     subset(finaldata.n, rank==105), subset(finaldata.n, rank==106),
                     subset(finaldata.n, rank==107), subset(finaldata.n, rank==108),
                     subset(finaldata.n, rank==109), subset(finaldata.n, rank==110),
                     subset(finaldata.n, rank==111), subset(finaldata.n, rank==112),
                     subset(finaldata.n, rank==113), subset(finaldata.n, rank==114),
                     subset(finaldata.n, rank==115), subset(finaldata.n, rank==116),
                     subset(finaldata.n, rank==117), subset(finaldata.n, rank==118),
                     subset(finaldata.n, rank==119), subset(finaldata.n, rank==120),
                     subset(finaldata.n, rank==121), subset(finaldata.n, rank==122),
                     subset(finaldata.n, rank==123), subset(finaldata.n, rank==124),
                     subset(finaldata.n, rank==125), subset(finaldata.n, rank==126),
                     subset(finaldata.n, rank==127), subset(finaldata.n, rank==128),
                     subset(finaldata.n, rank==129), subset(finaldata.n, rank==130),
                     subset(finaldata.n, rank==131), subset(finaldata.n, rank==132),
                     subset(finaldata.n, rank==133), subset(finaldata.n, rank==134),
                     subset(finaldata.n, rank==135), subset(finaldata.n, rank==136),
                     subset(finaldata.n, rank==137), subset(finaldata.n, rank==138),
                     subset(finaldata.n, rank==139), subset(finaldata.n, rank==140),
                     subset(finaldata.n, rank==141), subset(finaldata.n, rank==142),
                     subset(finaldata.n, rank==143), subset(finaldata.n, rank==144),
                     subset(finaldata.n, rank==145), subset(finaldata.n, rank==146),
                     subset(finaldata.n, rank==147), subset(finaldata.n, rank==148),
                     subset(finaldata.n, rank==149), subset(finaldata.n, rank==150),
                     subset(finaldata.n, rank==151), subset(finaldata.n, rank==152),
                     subset(finaldata.n, rank==153), subset(finaldata.n, rank==154),
                     subset(finaldata.n, rank==155), subset(finaldata.n, rank==156),
                     subset(finaldata.n, rank==157), subset(finaldata.n, rank==158),
                     subset(finaldata.n, rank==159), subset(finaldata.n, rank==160),
                     subset(finaldata.n, rank==161), subset(finaldata.n, rank==162),
                     subset(finaldata.n, rank==163), subset(finaldata.n, rank==164),
                     subset(finaldata.n, rank==165), subset(finaldata.n, rank==166),
                     subset(finaldata.n, rank==167), subset(finaldata.n, rank==168),
                     subset(finaldata.n, rank==169), subset(finaldata.n, rank==170),
                     subset(finaldata.n, rank==171), subset(finaldata.n, rank==172),
                     subset(finaldata.n, rank==173), subset(finaldata.n, rank==174),
                     subset(finaldata.n, rank==175), subset(finaldata.n, rank==176),
                     subset(finaldata.n, rank==177), subset(finaldata.n, rank==178),
                     subset(finaldata.n, rank==179), subset(finaldata.n, rank==180),
                     subset(finaldata.n, rank==181), subset(finaldata.n, rank==182),
                     subset(finaldata.n, rank==183), subset(finaldata.n, rank==184),
                     subset(finaldata.n, rank==185), subset(finaldata.n, rank==186),
                     subset(finaldata.n, rank==187), subset(finaldata.n, rank==188),
                     subset(finaldata.n, rank==189), subset(finaldata.n, rank==190),
                     subset(finaldata.n, rank==191), subset(finaldata.n, rank==192),
                     subset(finaldata.n, rank==193), subset(finaldata.n, rank==194),
                     subset(finaldata.n, rank==195), subset(finaldata.n, rank==196),
                     subset(finaldata.n, rank==197), subset(finaldata.n, rank==198),
                     subset(finaldata.n, rank==199), subset(finaldata.n, rank==200),
                     subset(finaldata.n, rank==201), subset(finaldata.n, rank==202),
                     subset(finaldata.n, rank==203), subset(finaldata.n, rank==204),
                     subset(finaldata.n, rank==205), subset(finaldata.n, rank==206),
                     subset(finaldata.n, rank==207), subset(finaldata.n, rank==208),
                     subset(finaldata.n, rank==209), subset(finaldata.n, rank==210))
finaldata.m$method <- "Manual"

# NY 1
zips <- c("06907", "06906", "06905", "06903", "06902", "06901", "06897", "06890", 
          "06883", "06880", "06856", "06855", "06854", "06853", "06851", "06850", 
          "06840", "06825", "06824", "06820", "06615", "06614", "06612", "06611", 
          "06610", "06608", "06607", "06606", "06605", "06604", "06484", "06482", 
          "06468")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "501-03"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "03"

# NY 2
zips <- c("12483")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "501-03"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "03"

# CHI 1
zips <- c("47964", "47963", "47951", "47922", "46349", "47943")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "602-01"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "01"

# CHI 2
zips <- c("60403")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "602-02"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "02"

# CHI 3
zips <- c("60431")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "602-06"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "06"

# PHI 1
zips <- c("08327")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "504-06"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "06"

# TSP 1
zips <- c("33857", "33876")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "539-01"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "01"

# TSP 1
zips <- c("33178")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "528-02"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "02"

# AZ 1
zips <- c("85392")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "753-02"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "02"

# CO 1
zips <- c("81325")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "751-01"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "01"

# Detroit 1
zips <- c("48103","48105","48109")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "505-04"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "04"

# Detroit 2
zips <- c("48085",	"48098",	"48306",	"48307",	"48309",
          "48326",	"48328",	"48329",	"48340",	"48342",
          "48346",	"48348",	"48350",	"48359",	"48360",
          "48362",	"48363",	"48367",	"48370",	"48371",
          "48430",	"48442",	"48462",	"48030",	"48067",
          "48069",	"48070",	"48071",	"48083",	"48220",	
          "48237")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "505-02"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "02"

# Austin 1
zips <- c("78619","78676","78735","78746","78738","78733",
          "78606")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "635-01"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "01"

# Austin 2
zips <- c("78753","78754","78757")
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- "635-02"
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- "02"

# Orlando 2
zips <- c('34797', '32784', '32778', '32767', '32736', '32735', 
          '32726', '32702', '32190', '32180', '32130', '32102')
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- '534-02'
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- '02'

# Orlando 4
zips <- c('34788',	'34785',	'34762',	'34748',	'34731',	'34491',
          '34488',	'34484',	'34482',	'34481',	'34480',	'34479',
          '34476',	'34475',	'34474',	'34473',	'34472',	'34471',
          '34470',	'34432',	'34431',	'34420',	'33585',	'33538',
          '33521',	'33514',	'33513',	'32696',	'32686',	'32681',
          '32668',	'32667',	'32664',	'32617',	'32195',	'32179',
          '32162',	'32159',	'32134',	'32133',	'32113')
zips <- data.frame(zips)
finaldata.m[finaldata.m$zip %in% zips$zips, 1] <- '534-04'
finaldata.m[finaldata.m$zip %in% zips$zips, 13] <- '04'


finaldata <- finaldata.m

zipcode <- subset(zipcode, select = c('zip', 'city'))
finaldata <- merge(finaldata, zipcode, by="zip")

write.table(finaldata, file = "//Users//psullivan01//Documents//DMAZones19.csv", 
            row.names=FALSE, sep=",")
