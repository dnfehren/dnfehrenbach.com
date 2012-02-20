#load libraries
library(RSQLite)
library(ggplot2)

#connect to database
#con <- dbConnect(SQLite(), "/users/dnfehren/Desktop/epa.sqlite")
con <- dbConnect(SQLite(), "C:\\Users\\dnfehren\\vshare_w\\projects\\expl_analysis_618\\homework\\data_sets\\epa.sqlite")
#con <- dbConnect(SQLite(), "C:\\Users\\dnfehren\\Desktop\\epa.sqlite")


#send query and load data into R data frame
query <- dbSendQuery(con, statement = "SELECT ST, County, Carcinogen, Total_Releases from tri");
return <- fetch(query, n = -1)

#replace character columns with more appropriate data classes
replace_tr <- as.numeric(return$Total_Releases)
return$Total_Releases <- replace_tr

replace_car <- as.factor(return$Carcinogen)
return$Carcinogen <- replace_car

#rename columns
colnames(return) <- c("state", "county", "carcinogen", "total_rel")

#create points and lables to factor total release column
cutpoints_c <- c(-1,.3,200,4000,30000,3.305e+08)
cutlabels_c <- c("small","med-small","med","large","very large")

#cut total release column
chem_cut <- cut(return$total_rel, breaks = cutpoints_c, labels = cutlabels_c)

#replace columns
return$total_rel <- chem_cut

#load poverty data for US counties
#http://www.census.gov/did/www/saipe/data/statecounty/data/2008.html
#pov <- read.csv("/Users/dnfehren/Desktop/est08ALL_edit.csv")
pov <- read.csv("C:\\Users\\dnfehren\\Desktop\\est08ALL_edit.csv")

#create points and lables to factor poverty percent column
cutpoints_p <- c(0,11,16,20,30,55)
cutlabels_p <- c("low","med-low","med","med-high","high")

#perform cut and replace the old column in data frame
pov_cut <- cut(pov$Poverty_Percent_All_Ages, breaks = cutpoints_p, labels = cutlabels_p)
pov$Poverty_Percent_All_Ages <- pov_cut

#create smaller dataframe
pov_sum <- cbind(pov[3],pov[4],pov[8])

#rename columns to match those in the epa data
colnames(pov_sum) <- c("state", "county", "pov_percent")

#uppercase county names in poverty data for better matching against epa data
pov_sum$county <- toupper(pov_sum$county)

#merge the two data frames using both the "state" and "county" columns
merged_pov_chem <- merge(return, pov_sum)

#plot the count of total release factors split by poverty categories
ggplot(merged_pov_chem, aes(x=pov_percent, fill=total_rel)) + geom_bar(position="dodge")

table(merged_pov_chem$pov_percent)

table(merged_pov_chem$total_rel)

#plot the count of carcinogen factors split by poverty categories
ggplot(merged_pov_chem, aes(x=pov_percent, fill=carcinogen)) + geom_bar(position="dodge")

table(merged_pov_chem$pov_percent)

table(merged_pov_chem$carcinogen)

#histogram - counting release events using the poverty categories
qplot(pov_percent, data=merged_pov_chem, geom="histogram")
