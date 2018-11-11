install.packages("psych")

library('dplyr')
library('psych')
library('leaps')

######### Progress bar ###############
pb <- txtProgressBar(style=3)
for (i in 1:10000) {
  rnorm(100)
  setTxtProgressBar(pb, i/10000)
}

# model housing prices
dd <- read.csv("C:/Users/hinsm/Dropbox/Yale University/001 - MA Year 1A, Fall Term 2018/STAT 625 - Case Studies/Week 7-10/cleaned_dd.csv", as.is = TRUE)

# turn each sale into its own observation
df <- data.frame(matrix(nrow = 1, ncol = 11))
insert.cols <- c('zone', 'acres', 'sqft', 'yearbuilt', 'bedrooms', 'bathrooms',
                 'halfbaths', 'multibuilding', 'style', 'saledate', 'saleprice')
colnames(df) <- insert.cols

for (i in seq(1:nrow(dd)) ) {
  # for each sale in row
  
  for (sale in 1:5) {
    # create new row with the right data
    access.cols <- c('zone', 'acres', 'sqft', 'yearbuilt', 'bedrooms', 'bathrooms',
                     'halfbaths', 'multibuilding', 'style', paste(c("saledate", "saleprice"), sale, sep = ""))
    
    r_ <- dd[i, access.cols]
    names(r_)
    
    colnames(r_) <- insert.cols
    df <- rbind(df, r_)
    
  }
  
  #Setting for progress bar
  setTxtProgressBar(pb, i/nrow(dd))
  #checker: this will keep printing as long as the loop doesn't run into any errors
  #If the loop runs into some error, we know which .html file gives us the issue.
  #print(i)
}

#Expunge all rows with NAs for saledate and saleprice
df2 <- df[!with(df,is.na(df$saledate) & is.na(df$saleprice)),]

#Expunge all rows with 0 for saleprice as it doesn't have any meanings.
df3 <- df2[(df2$saleprice!=0),]

#clean the retail word for style column
df3$style <- sub("Retail&lt;10th sf", "Retail", df3$style)


#Subset to investigation ZONE: BA
#Make sure to log saleprice
ba <- df3[(df3$zone=='BA'),]
ba <- ba[!with(ba,is.na(ba$zone) & is.na(ba$acres) & is.na(ba$sqft) & is.na(ba$yearbuilt) & is.na(ba$bedrooms)
               & is.na(ba$bathrooms) & is.na(ba$halfbaths) & is.na(ba$multibuilding) & is.na(ba$style) & is.na(ba$saledate) & is.na(ba$saleprice)),]

#take out styles column as it has way too many categorical values, info of which are contained in other variables
ba$style <- NULL

unique(ba$acres)
unique(ba$sqft)
unique(ba$yearbuilt)
unique(ba$multibuilding)

unique(ba$bedrooms)
unique(ba$bathrooms)
unique(ba$halfbaths)
unique(ba$saledate)
unique(ba$saleprice)

#Plot to make observations - look at saleprice ~ saledate (controlling for bedrooms)

#Bedroom = 1 are the commercial ones
#Split into two sets: com_ba for commercial and res_ba for residential
com_ba <- ba[(ba$bedrooms==0),]
res_ba <- ba[(ba$bedrooms!=0),]


#plot(ba$saledate[ba$bedrooms==0], log(ba$saleprice[ba$bedrooms==0]), xlim=c(1964,2017), xlab='saledate', ylab='saleprice', pch=15, col='green')
plot(res_ba$saledate[res_ba$bedrooms==1], log(res_ba$saleprice[res_ba$bedrooms==1]), xlim=c(1964,2017), xlab='saledate', ylab='saleprice', pch=15, col='red')
points(res_ba$saledate[res_ba$bedrooms==2], log(res_ba$saleprice[res_ba$bedrooms==2]), xlab='saledate', ylab='saleprice', pch=15, col='blue')
points(res_ba$saledate[res_ba$bedrooms==3], log(res_ba$saleprice[res_ba$bedrooms==3]), xlab='saledate', ylab='saleprice', pch=15, col='orange')
points(res_ba$saledate[res_ba$bedrooms==4], log(res_ba$saleprice[res_ba$bedrooms==4]), xlab='saledate', ylab='saleprice', pch=15, col='pink')
points(res_ba$saledate[res_ba$bedrooms==5], log(res_ba$saleprice[res_ba$bedrooms==5]), xlab='saledate', ylab='saleprice', pch=15, col='purple')

#One-way ANCOVA model
results <- lm(saleprice ~ as.factor(saledate) + bedrooms, data=res_ba)
anova(results)

data.num = select(res_ba, acres, sqft, yearbuilt, bedrooms, bathrooms, halfbaths, saledate, saleprice)
corr.test(data.num, use="pairwise", method="pearson", adjust="none", alpha=0.05)


#---------------------------------------------------------------------------------------------------------
unique(com_ba$acres)
unique(com_ba$sqft)
unique(com_ba$yearbuilt)
unique(com_ba$multibuilding)

unique(com_ba$bedrooms)
unique(com_ba$bathrooms)
unique(com_ba$halfbaths)

com_ba[is.na(com_ba)] <- 0

data.com_ba = select(com_ba, acres, sqft, yearbuilt, bathrooms, halfbaths, saledate, saleprice)
corr.test(data.com_ba, use="pairwise", method="pearson", adjust="none", alpha=0.05)

model.1 <- lm(saleprice ~ saledate + acres + sqft + yearbuilt + bathrooms + halfbaths, data=com_ba)
anova(model.1)
#ANOVA shows that halfbaths is an insignificant predictor.

model.2 <- lm(saleprice ~ saledate + acres + sqft + yearbuilt + bathrooms, data=com_ba)
anova(model.2)

pairs(data=com_ba, ~ saleprice + saledate + acres + sqft + yearbuilt + bathrooms)

plot(com_ba$saledate, log(com_ba$saleprice), xlim=c(1970,2017), xlab='saledate', ylab='saleprice', pch=15)


#--------------------------Look at all zones for commercial -----------------------------------------------
df3_com <- df3[df3$bedrooms==0,]

df3_com_zone <- as.matrix(unique(df3_com$zone))
df3_com_zone <- as.matrix(df3_com_zone[complete.cases(df3_com_zone), ])
nrow(df3_com_zone)

df3_com <- df3_com[complete.cases(df3_com[ , 11]),]
sum(is.na(df3_com$saleprice))

df3_com <- df3_com[complete.cases(df3_com[ , 1]),]
sum(is.na(df3_com$zone))


for(i in 1:nrow(df3_com_zone)){
  plot(df3_com$saledate[df3_com$zone == df3_com_zone[i]], log(df3_com$saleprice[df3_com$zone == df3_com_zone[i]]), xlim=c(1960,2017), xlab='saledate', ylab='saleprice', pch=15)
}


#--------------------------Look at all zones for residential -----------------------------------------------
df3_res <- df3[df3$bedrooms!=0,]

df3_res_zone <- as.matrix(unique(df3_res$zone))
df3_res_zone <- as.matrix(df3_res_zone[complete.cases(df3_res_zone), ])
nrow(df3_res_zone)

df3_res <- df3_res[complete.cases(df3_res[ , 11]),]
sum(is.na(df3_res$saleprice))

df3_res <- df3_res[complete.cases(df3_res[ , 1]),]
sum(is.na(df3_res$zone))


#Basic Exploratory data analysis
for(i in 1:nrow(df3_res_zone)){
  plot(df3_res$saledate[df3_res$zone == df3_res_zone[i]], log(df3_res$saleprice[df3_res$zone == df3_res_zone[i]]), xlim=c(1960,2017), xlab='saledate', ylab='saleprice', pch=15)
}

count(df3_res, df3_res$zone)

for(i in 1:nrow(df3_res_zone)) {
  temp <- df3_res[df3_res$zone==df3_res_zone[i],]
  model <- lm(saleprice ~ saledate + acres + sqft + yearbuilt + bedrooms + bathrooms + halfbaths, data=temp)
  print(anova(model))
}
