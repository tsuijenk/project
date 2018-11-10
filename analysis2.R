library('dplyr')
library('leaps')

# model housing prices
dd <- read.csv('C:/Users/jenkin/Dropbox/Yale University/001 - MA Year 1A, Fall Term 2018/STAT 625 - Case Studies/Week 7-10/final_nhh.csv', as.is = TRUE)
#d <- sample_n(dd, 2000)

#Observe unique of zone/neighbourhood


#We want to be albe to control for things like the size of the house
#the average price are going up because of recession

dd_1 <- dd[dd$zone != "",]
sum(dd_1$zone == "") #should be 0

#Only retain years from saledate(1-5)
dd_1$saledate1 <- substring(dd_1$saledate1,7,10)
dd_1$saledate2 <- substring(dd_1$saledate2,7,10)
dd_1$saledate3 <- substring(dd_1$saledate3,7,10)
dd_1$saledate4 <- substring(dd_1$saledate4,7,10)
dd_1$saledate5 <- substring(dd_1$saledate5,7,10)

#Keep columns: yearbuilt, bedrooms, bathrooms, halfbaths, multibuilding, zone,
#acres, saledate1, saleprice1, saledate2, saleprice2, saledate3, saleprice3, 
#saledate4, saleprice4, saledate5, saleprice5, style, sqft
#(Keep the columns which encapsulates information about other columns,
#for example, the number of bedrms, bthrooms, and halfbaths should encapsulate info
#model and occupancy, etc.)

dd_2 <- select(dd_1, zone, acres, sqft, yearbuilt, 
               bedrooms, bathrooms, halfbaths, multibuilding, style,
               saledate1, saleprice1, saledate2, saleprice2, saledate3, saleprice3,
               saledate4, saleprice4, saledate5, saleprice5)

#Drop rows where saleprice(1-5) are all NAs

dd_3 <- dd_2[!with(dd_2,is.na(dd_2$saleprice1) & is.na(dd_2$saleprice2)
                    & is.na(dd_2$saleprice3) & is.na(dd_2$saleprice4)
                    & is.na(dd_2$saleprice5)),]
dd_4 <- dd_3[!with(dd_3,dd_3$saleprice1 == 0 & is.na(dd_3$saleprice2)
                   & is.na(dd_3$saleprice3) & is.na(dd_3$saleprice4)
                   & is.na(dd_3$saleprice5)),]
dd_5 <- dd_4[!with(dd_4,dd_4$saleprice1 == 0 & dd_4$saleprice2 == 0
                   & is.na(dd_4$saleprice3) & is.na(dd_4$saleprice4)
                   & is.na(dd_4$saleprice5)),]
dd_6 <- dd_5[!with(dd_5,dd_5$saleprice1 == 0 & dd_5$saleprice2 == 0
                   & dd_5$saleprice3 == 0 & is.na(dd_5$saleprice4)
                   & is.na(dd_5$saleprice5)),]
dd_7 <- dd_6[!with(dd_6,dd_6$saleprice1 == 0 & dd_6$saleprice2 == 0
                   & dd_6$saleprice3 == 0 & dd_6$saleprice4 == 0
                   & is.na(dd_6$saleprice5)),]
dd_8 <- dd_7[!with(dd_7,dd_7$saleprice1 == 0 & dd_7$saleprice2 == 0
                   & dd_7$saleprice3 == 0 & dd_7$saleprice4 == 0
                   & dd_7$saleprice5 == 0),]

dd_9 <- dd_8[order(dd_8$zone),]

write.csv(dd_9, "C:/Users/jenkin/Dropbox/Yale University/001 - MA Year 1A, Fall Term 2018/STAT 625 - Case Studies/Week 7-10/cleaned_dd.csv", row.names = FALSE)