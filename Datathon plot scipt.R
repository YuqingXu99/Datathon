# install.packages("ggplot2")
# install.packages("dplyr")
#install.packages("tidyr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("data.table")
library("stringr")


data1 = read.csv("Part 1.csv")
data2 = read.csv("Part 2.csv")
data3 = read.csv("Part 3.csv")
data4a = read.csv("Part 4a.csv")
data4b = read.csv("Part 4b.csv")
data5 = read.csv("Part 5.csv")
description = read.csv("Metadata (1).csv")


data1 = data1[-1,]
data2 = data2[-1,]
data3 = data3[-1,]
data4a = data4a[-1,]
data4b = data4b[-1,]
data5 = data5[-1,]


data1lat = data1[,c(2,4,6,9)]
data2lat = data2[,c(2,4,6,9)]
data3lat = data3[,c(2,4,6,9)]
data4alat = data4a[,c(2,4,6,9)]
data4blat = data4b[,c(2,4,6,9)]
data5lat = data5[,c(2,4,6,9)]

#########################################

data1lat_w = spread(data1lat, RCPSZFE.id, ESTAB)
setnames(data1lat_w, c("GEO.id2", "NAICS.id", "All", "Entire", "First", "Less", "Mid", "Most", "Highest", "Close"))
data1lat_w = as.data.table(data1lat_w)


data1lat_w = data1lat_w[, lapply(.SD, function(xx) {str_replace_na(xx, 0)})]
mid = rowSums(cbind(as.numeric(data1lat_w$Less), as.numeric(data1lat_w$Mid), as.numeric(data1lat_w$Most)))
data1lat_w = data1lat_w[,c(1,2,3,4,5,9,10)]
data1lat_w$mid = mid
data1lat_w[,3:8] <- lapply(data1lat_w[,3:8],as.numeric)

count1 = aggregate(data1lat_w$All, by = list(data1lat_w$NAICS.id), FUN = sum)
count1$x = replace_na(count1$x, 0)

level_1 = aggregate(data1lat_w[,3:8], by = list(data1lat_w$NAICS.id), FUN = sum)

############################################

data2lat_w = spread(data2lat, RCPSZFE.id, ESTAB)
setnames(data2lat_w, c("GEO.id2", "NAICS.id", "All", "Entire", "First", "Less", "Mid", "Most", "Highest", "Close"))
data2lat_w = as.data.table(data2lat_w)

data2lat_w = data2lat_w[, lapply(.SD, function(xx) {str_replace_na(xx, 0)})]
mid = rowSums(cbind(as.numeric(data2lat_w$Less), as.numeric(data2lat_w$Mid), as.numeric(data2lat_w$Most)))
data2lat_w = data2lat_w[,c(1,2,3,4,5,9,10)]
data2lat_w$mid = mid
data2lat_w[,3:8] <- lapply(data2lat_w[,3:8],as.numeric)

count2 = aggregate(as.numeric(data2lat_w$All), by = list(data2lat_w$NAICS.id), FUN = sum)
count2$x = replace_na(count2$x, 0)

level_2 = aggregate(data2lat_w[,3:8], by = list(data2lat_w$NAICS.id), FUN = sum)

############################################

data3lat_w = spread(data3lat, RCPSZFE.id, ESTAB)
setnames(data3lat_w, c("GEO.id2", "NAICS.id", "All", "Entire", "First", "Less", "Mid", "Most", "Highest", "Close"))
data3lat_w = as.data.table(data3lat_w)

data3lat_w = data3lat_w[, lapply(.SD, function(xx) {str_replace_na(xx, 0)})]
mid = rowSums(cbind(as.numeric(data3lat_w$Less), as.numeric(data3lat_w$Mid), as.numeric(data3lat_w$Most)))
data3lat_w = data3lat_w[,c(1,2,3,4,5,9,10)]
data3lat_w$mid = mid
data3lat_w[,3:8] <- lapply(data3lat_w[,3:8],as.numeric)

count3 = aggregate(as.numeric(data3lat_w$All), by = list(data3lat_w$NAICS.id), FUN = sum)
count3$x = replace_na(count3$x, 0)

level_3 = aggregate(data3lat_w[,3:8], by = list(data3lat_w$NAICS.id), FUN = sum)

############################################

data4alat_w = spread(data4alat, RCPSZFE.id, ESTAB)
setnames(data4alat_w, c("GEO.id2", "NAICS.id",  "All", "Entire", "First", "Less", "Mid", "Most", "Highest", "Close"))
data4alat_w = as.data.table(data4alat_w)

data4alat_w = data4alat_w[, lapply(.SD, function(xx) {str_replace_na(xx, 0)})]
mid = rowSums(cbind(as.numeric(data4alat_w$Less), as.numeric(data4alat_w$Mid), as.numeric(data4alat_w$Most)))
data4alat_w = data4alat_w[,c(1,2,3,4,5,9,10)]
data4alat_w$mid = mid
data4alat_w[,3:8] <- lapply(data4alat_w[,3:8],as.numeric)

count4a = aggregate(as.numeric(data4alat_w$All), by = list(data4alat_w$NAICS.id), FUN = sum)
count4a$x = replace_na(count4a$x, 0)

level_4a = aggregate(data4alat_w[,3:8], by = list(data4alat_w$NAICS.id), FUN = sum)

############################################

data4blat_w = spread(data4blat, RCPSZFE.id, ESTAB)
setnames(data4blat_w, c("GEO.id2", "NAICS.id", "All", "Entire", "First", "Less", "Mid", "Most", "Highest", "Close"))
data4blat_w = as.data.table(data4blat_w)

data4blat_w = data4blat_w[, lapply(.SD, function(xx) {str_replace_na(xx, 0)})]
mid = rowSums(cbind(as.numeric(data4blat_w$Less), as.numeric(data4blat_w$Mid), as.numeric(data4blat_w$Most)))
data4blat_w = data4blat_w[,c(1,2,3,4,5,9,10)]
data4blat_w$mid = mid
data4blat_w[,3:8] <- lapply(data4blat_w[,3:8],as.numeric)

count4b = aggregate(as.numeric(data4blat_w$All), by = list(data4blat_w$NAICS.id), FUN = sum)
count4b$x = replace_na(count4b$x, 0)

level_4b = aggregate(data4blat_w[,3:8], by = list(data4blat_w$NAICS.id), FUN = sum)

############################################

data5lat_w = spread(data5lat, RCPSZFE.id, ESTAB)
setnames(data5lat_w, c("GEO.id2", "NAICS.id", "All", "Entire", "First", "Less", "Mid", "Most", "Highest", "Close"))
data5lat_w = as.data.table(data5lat_w)

data5lat_w = data5lat_w[, lapply(.SD, function(xx) {str_replace_na(xx, 0)})]
mid = rowSums(cbind(as.numeric(data5lat_w$Less), as.numeric(data5lat_w$Mid), as.numeric(data5lat_w$Most)))
data5lat_w = data5lat_w[,c(1,2,3,4,5,9,10)]
data5lat_w$mid = mid
data5lat_w[,3:8] <- lapply(data5lat_w[,3:8],as.numeric)

count5 = aggregate(as.numeric(data5lat_w$All), by = list(data5lat_w$NAICS.id), FUN = sum)
count5$x = replace_na(count5$x, 0)

level_5 = aggregate(data4blat_w[,3:8], by = list(data4blat_w$NAICS.id), FUN = sum)

############################################

count_t = rowSums(cbind(count1$x,count2$x, count3$x, count4a$x, count4b$x, count5$x))
factor = data.frame(count1$Group.1)
count_total = cbind(factor, count_t)
count_total = count_total[order(count_total[,2], decreasing = TRUE),]
channel_h = head(count_total)


# plot(channel_h$count_t, xlab = "types", xaxt = 'n')
# axis(side = 1, at = c(1,2,3,4,5,6), labels = channel_h$count1.Group.1)

ggplot(channel_h, aes(channel_h$count1.Group.1, channel_h$count_t)) +
  geom_bar(stat = "identity", fill = "blue")


level_whole = rbind(level_1, level_2, level_3, level_4a, level_4b, level_5)
level_whole[,2:7] =  lapply(level_whole[,2:7],as.numeric)

level_t = aggregate(level_whole[,2:7], by = list(level_whole$Group.1), FUN = sum)
level_long = gather(level_t, establishment, number, c(First, mid, Highest), factor_key = TRUE)
level_long = level_long[order(level_long[,2], decreasing = TRUE),]

ggplot(level_long, aes(x = Group.1, y = number, fill = establishment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_text()) +
  labs(x = "Channel",
       y = "Number") +
  scale_fill_discrete(name = "Establishment", labels = c("Low", "Mid", "High"))