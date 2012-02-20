#daniel fehrenbach
#dnfehrenbach@gmail.com

rm(list=ls())

library(ggplot2)

#data from http://www.epa.gov/tri/tridata/current_data/index.html
#win
chem <- read.delim("C:\\Users\\dnfehren\\Desktop\\TRI_2008_US_v08.txt")
#osx
#chem <- read.delim("/Users/dnfehren/Desktop/TRI_2008_US_v08.txt")



####################################
#
#bar chart of industries with the most release events reported
#
####################################

#read the 6-digit NAICS listings (available at http://www.census.gov/naics/2007/index.html) into an R dataframe
#the file downloads as a .xls, convert it to csv before using it in R
#win
naics_description <- read.csv("C:\\Users\\dnfehren\\Desktop\\naics07-6.csv")
#osx
#naics_description <- read.csv("/Users/dnfehren/Desktop/naics07-6.csv")

#create new dataframe - a frequency table of NAICS codes from the full dataset
naics_cnt_frm = data.frame(table(chem$Primary.NAICS))

#NOT USED# merge full frame/all naics codes NB!!this will take a while
#subfrm_with_naics <- merge(naics_description, naics_cnt_frm, by.x = "X2007.NAICS.Code", by.y = "Var1")

#extract a subset of the new dataframe keeping NAICS identifiers that have a freq count greater than 500
naics_cnt_subfrm = subset(naics_cnt_frm, Freq > 250)

#merge subset with the NAICS list to get the real descriptions
subfrm_with_naics <- merge(naics_description, naics_cnt_subfrm, by.x = "X2007.NAICS.Code", by.y = "Var1")

#plot the subset
qplot(factor(X2007.NAICS.Code), data=subfrm_with_naics, geom="bar", fill=factor(X2007.NAICS.Title), weight=Freq)+scale_x_discrete("NAICS Industry Code")+scale_y_continuous("Number of Release Events")

#rotate tick lables??errors out  grid.gedit(factor(subfrm_with_naics$X2007.NAICS.Code), rot=45)

#according to the man pages code like this should work, but http://tolstoy.newcastle.edu.au/R/e6/devel/09/03/1169.html
# suggests that the ability to set incomparable columns has not actually be implmented yet and requires a patch
##skip <- c("decs", "Freq")
##chem_unq_with_naics <- unique(chem_with_naics, incomparables=skip)



####################################
#
#bar chart of companies and industries with the largest chemical releases
#
####################################

#sort the chem data frame by the descending amount of total releases
ordered_tot_release <- chem[ order(-chem$Total.Releases) ,]

#take the top 100 largest releases from the dataframe and create a new df
top_releasers <- ordered_tot_release[1:100,]

#merge the top100 dataframe with the NAICS codes
top_rel_with_naics <- merge(naics_description, top_releasers, by.x = "X2007.NAICS.Code", by.y = "Primary.NAICS")

#plot the top releasers data frame
qplot(factor(Facility.Name), data=top_rel_with_naics, geom="bar", fill=factor(X2007.NAICS.Title), weight=Total.Releases)



######################################
#
#map of facilities with large number of releases
#
######################################

#load the map library
library(maps)

#load R's sqlite module
library(RSQLite)

#create database connection object
con <- dbConnect(SQLite(), "/users/dnfehren/Desktop/epa.sqlite")

#query groups the data by facility ID, then counts the groups then returns the count, id, facility name and lat/long 
# from each facility that has more than 25 releases
event_mapping_query <- dbSendQuery(con, statement = "SELECT count(*) AS count, Facility_Name AS name, TRI_Facility_ID AS id, Latitude AS latitude, Longitude AS longitude FROM tri GROUP BY TRI_Facility_ID HAVING count(*) > 25 ORDER BY count(*) DESC");

#returns R object from the query object the n=-1 ensures we get the whole query object back, the rsqlite methods default to 500 rows
event_mapping_return <- fetch(event_mapping_query, n = -1)

#query returns as characters, replace with numeric data
event_mapping_return$count <- as.numeric(event_mapping_return$count)
event_mapping_return$longitude <-as.numeric(event_mapping_return$longitude)
event_mapping_return$latitude <-as.numeric(event_mapping_return$latitude)

#plot lat/long data over a map layer
#used example at http://had.co.nz/stat405/resources/drills/ggplot2.html
ggplot(event_mapping_return, aes(longitude, latitude)) + borders("state") + geom_point(aes(size = count))



######################################
#
#map largest releases
#
######################################
#the Total Release field is in the db as text characters, which results in dictionary style sorting and does not give 
# the correct sort sequence for this data
# thanks to Dan Kennedy for the tip from http://marc.info/?l=sqlite-users&m=112245536902335 to add the 0.0
quant_mapping_query <- dbSendQuery(con, statement = "SELECT Total_Releases AS tot_rel, TRI_Facility_ID AS id, Latitude AS latitude, Longitude AS longitude, ST AS state FROM tri ORDER BY Total_Releases +0.0 DESC LIMIT 100");

#returns R object from the query object
quant_mapping_return <- fetch(quant_mapping_query, n = -1)

#query returns as characters, replace with numeric data
quant_mapping_return$tot_rel <- as.numeric(quant_mapping_return$tot_rel)
quant_mapping_return$longitude <-as.numeric(quant_mapping_return$longitude)
quant_mapping_return$latitude <-as.numeric(quant_mapping_return$latitude)

#plot lat/long data over a map layer
#used example at http://had.co.nz/stat405/resources/drills/ggplot2.html
ggplot(quant_mapping_return, aes(longitude, latitude)) + borders("state") + geom_point(aes(size = tot_rel))



######################################
#
#presence of dangerous chemicals versus size of release
#
######################################

#query that removes a handful of really huge release events from the top of the dataset
useful_releases_query <- dbSendQuery(con, statement = "SELECT * FROM tri WHERE Total_Releases+0.0 < 50000000 ORDER BY Total_Releases+0.0 DESC");

return <- fetch(useful_releases_query, n = -1)

#reclass the Total_Release column as numeric
return$Total_Releases <-as.numeric(return$Total_Releases)

#create a lable vector to use in the cut fuction
label_holder = c("1","2","3","4","5","6","7","8","9","10")

#cut the Total_Releases into 10 parts, use the above vector to lable the new break factors and set ordered to true
return$Total_Releases <- cut(return$Total_Releases, breaks=10, labels = label_holder, ordered_result=TRUE)

#create levels in the Classification and Carcinogen columns based on the values in the column
levels(return$Classification) <- c("NON-PCB","PCB","DIOXIN")
levels(return$Carcinogen) <- c("YES", "NO")

#create a table cross tabluating Classification, Caricenogen and Total Releases and force it into a dataframe
combo_table <- as.data.frame(table(return$Classification, return$Total_Releases, return$Carcinogen))

#remane the columns of the dataframe
colnames(combo_table) <- c("classification","release_size","carcinogen","freq")

#full plot of data, jittered to show where the points are
p <- ggplot(combo_table, aes(factor(release_size), freq)) 
p + geom_point(aes(colour = factor(classification), shape = factor(carcinogen)), position="jitter") 

#most the frequencies are below 15, reducing the scale shows more useful variation
p <- ggplot(combo_table, aes(factor(release_size), freq)) 
p + geom_point(aes(colour = factor(classification), shape = factor(carcinogen)), position="jitter")+ scale_y_continuous(limits=c(0, 15))