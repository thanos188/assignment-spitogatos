#inserting data to R
library(readr)
spito<-read.csv("~/Downloads/assignment_rev2.csv")
###############################################################################
#Assignment Part 1
###############################################################################
#creating price_per_sq_meter column
price_per_sq_meter<-spito$price/spito$sq_meters
#identifying extreme values (outliers)
boxplot(price_per_sq_meter)
#in some cases we could identify the outliers based on the values outside the
#box plot and remove them, in this case there are several values outside but
#there are basically 2 values that could corrupt our metrics, we move back
#in our dataset (csv) create a new column price_per_sq_meter sort by this column
#and we can see that extreme values are from a houseboat and a house 1 sqr meter big
#we remove the 2 rows and save the new dataset. Outliers can be also removed in R
#as well, but sometimes csv files helps in order to better understand why a value
#is an outlier.
spito_new<-read.csv("~/Downloads/assignment_rev2_no_outliers.csv")
# Load the packages
library(dplyr)
library(tidyr)
#aggregate by subtype
f1 <- function(x) c(Mean = mean(x), Median = median(x), SD = sd(x))
st<-aggregate(spito_new$price_per_sq_meter ~ spito_new$subtype, spito_new,f1)
#aggregate by geography name
gn<-aggregate(spito_new$price_per_sq_meter ~ spito_new$geography_name, spito_new,f1)
#plot the price per sq meter in the different subtypes and geography areas
plot(spito_new$price_per_sq_meter ~ spito_new$subtype,xlab = "Subtype", ylab = "Price per sq meter")

###############################################################################
#Assignment Part 2
###############################################################################
#There are several metrics that could help an ad to rank high, such as Keywords
#difficulty, the popularity of each area, the ranking score and the competition
#In our dataset we can see the ranking score and try to analyze it further and
#the competition that each area has based on the number of ads per area.I could
#only suggest a different name for both beesy neighborhood and gentrification area
#as I find northern sub and south beach names much more attractive

geo_name<-as.factor(spito_new$geography_name)
table(geo_name)
#competition for each area: with table command we receive the number of ads per area
#we can see south beach is the most competitive area with 7324 ads (55% of total ads)
#followed by northern sub woth 5096 (38% of total ads). On the other hand
#gentrification area has only 6% of total ads with 841. It's obvious that the competition
#is much higher in both southern beach and northern sub against the gentrification area 

#Now lets compare the ranking score per area, to do so, there are 2 ways and maybe more
#Create a subset for each area or model our data against the geography_name variable
#which is the easiest way to compare the means of ranking score per area and the one 
#we choose
model_rank_per_area<-lm(spito_new$ranking_score~geo_name, data=spito_new)
summary(model_rank_per_area)
#Lets now interpret the summary. Intercept (111.6740) is the average ranking score an ad has in
#a beesy neighborhood which is our reference level in the variable (geo_name).
#geo_namegentrification area (7.1176) is the difference of means between the gentrification area
#and the beesy neighborhood in ranking score. geo_namenorthern sub (6.7339) is the difference of means between 
#the northern sub area and the beesy neighborhood in ranking score. Finally, geo_namesouth beach (7.0440)
#is the difference of the average ranking score in south beach  against an ad in
#beesy neighborhood

#plot the differences between areas (zoom to the see the full picture)
plot(spito_new$ranking_score ~ geo_name,xlab = "Geography Name", ylab = "Ranking Score")

#Now we are going to identify which characteristics are important for an ad to have a highranking score
#To do so we will build a model with ranking score as a response variable and all the other variables
#as explanatory, but first lets convert all variables to proper types

# identify variable types
var_types <- sapply(spito_new, class)
# convert logical and character variables to factors
spito_new[, var_types %in% c("logical", "character")] <- lapply(spito_new[, var_types %in% c("logical", "character")], factor)

# convert integer variables to numeric
spito_new[, var_types == "integer"] <- lapply(spito_new[, var_types == "integer"], as.numeric)
str(spito_new)
#removing equipped column as it has only NA values
spito_new$equipped <- as.numeric(as.character(spito_new$equipped)) 
spito_new <- subset(spito_new, select = -equipped)
model_rank<-lm(spito_new$ranking_score~., data = spito_new)
summary(model_rank)
#In the summary we can see the impact its variable has in ranking score from the estimate
#and if the impact is significant. For instance price_per_sq_meter has an estimate -1.813e-02
#which means that if the price per sq meter increases by 1 the ranking score decreases by 0.01813
#and this difference is statistically significant to the model. We can see that varaibles
#such as geography_name, price_per_sq_meter, subtypevilla, lux_homeTRUE, requires_renovation
#having a high impact in the ranking score. Lets further evaluate some of these variables.

m_ppsqm<-lm(spito_new$price_per_sq_meter~geo_name)
summary(m_ppsqm)
#same as above we can see the difference of the average price per sq meter between 
#a house in the south beach and one in the beesy neighborhood which is 2281
plot(spito_new$price_per_sq_meter~geo_name,xlab = "Geography Name", ylab = "Price per sq meter")

#Ranking score in different ad_type
#subseting our data based on the ad_type
levels(spito_new$ad_type)
premium<-subset(spito_new, spito_new$ad_type=='premium')
simple<-subset(spito_new, spito_new$ad_type=='simple')
up<-subset(spito_new, spito_new$ad_type=='up')
star<-subset(spito_new, spito_new$ad_type=='star')
#modelling simple type ads in different areas
table(simple$geography_name)
m_simple<-lm(simple$ranking_score~simple$geography_name, data=simple)
summary(m_simple)
plot(simple$ranking_score~simple$geography_name,xlab = "Geography Name (simple ad_type)", ylab = "Ranking score (simple ad_type)")
#from the summary and the plot once again we can observe the significant difference between the average ranking
#of an ad in south beach and one in beesy neighborhood (7.1), making things harder for a simple ad
#to rank higher in south beach against the chance it has in beesy neighborhood

#To sum up, it is clear from the above that south beach and northern sub are much more competitve areas agianst the others
#for an ad to rank higher and the level of discount should be based in the above findings.

###############################################################################
#Assignment Part 3
###############################################################################

