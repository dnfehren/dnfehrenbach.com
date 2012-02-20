
rm(list=ls(all=TRUE))

############################################
#daniel fehrenbach

#dnfehrenbach@gmail.com

#presentation plots
############################################

#load libraries
library(RSQLite)
library(ggplot2)
library(doBy)

#install.packages("directlabels", repos = "http://r-forge.r-project.org")
#http://learnr.wordpress.com/2010/01/03/directlabels-adding-direct-labels-to-ggplot2-and-lattice-plots/
library(directlabels)

############################################
#epa tri data
############################################

#database connection

#OSX
#source(file = "/users/dnfehren/Desktop/regionalize.RData")
#con <- dbConnect(SQLite(), "/users/dnfehren/Desktop/epa_99_08.s3db")

#Windows
con <- dbConnect(SQLite(), "C:\\Users\\dnfehren\\Desktop\\epa_99_08.s3db")
source(file = "C:\\Users\\dnfehren\\Desktop\\regionalize.RData")

#query
query <- dbSendQuery(con, statement = "SELECT year, facility_state, facility_county, (total_air+total_surface_water+total_underground_water+total_land) AS total_release FROM tri")

#load query into R data object
ret <- fetch(query, n = -1)

releases <-regionalize(ret)

#factor year, states and regions
factor(releases$year)
factor(releases$region)
factor(releases$facility_state)

############################################
#county demographic data
############################################
#OSX
#pov <- read.csv("/Users/dnfehren/Desktop/poverty_data.csv")
#race <- read.csv("/Users/dnfehren/Desktop/race_data.csv")

#Windows
pov <- read.csv("C:\\Users\\dnfehren\\Desktop\\poverty_data.csv")
race <- read.csv("C:\\Users\\dnfehren\\Desktop\\race_data.csv")

######################################
# Michigan
######################################

#####################
# Data Creation
#####################

###########
# MI subsets of national data sets
###########
#subset release data
mi_rel <- subset(releases, facility_state == "MI")

#subset of poverty data
mi_pov <- subset(pov, state == "MI")

#subset of race data
mi_race <- subset(race, state == "MI")

#####
#Create factored release frequency data
#####
#tabulate the occurences of release by year and county
mi_freq <- as.data.frame(table(mi_rel$year, mi_rel$facility_county))
colnames(mi_freq) <- c("year", "county", "freq")

#load frequency data into var
freq_to_cut <- mi_freq$freq

#create cut points in frequency data
cutpoints <- c(-1,50,100,200,400,1000)

#create labels for cutpoints
cutlbls <- c("0 to 50", "50 to 100", "100 to 200", "200 to 400", "400 to 1000")

#apply the cut
freq_cut <- cut(freq_to_cut, breaks = cutpoints, labels = cutlbls)

#replace freq with new factors
mi_freq$freq <- freq_cut

#####
#Create factored release magnitude data
#####
#find the sum of total releases for each county/year combination
mi_mag <-aggregate(mi_rel$total_release, by=list(mi_rel$facility_county,mi_rel$year), FUN=sum, na.rm=TRUE)

colnames(mi_mag) <- c("county", "year", "total")

#load total amount data into var
mag_to_cut <- mi_mag$total

#create cut points in frequency data
cutpoints_mag <- c(-1,50000,100000,500000,1000000,5000000,10000000,40000000)

cutlbls_mag <- c("0 to 50,000","50,000 to 100,000","100,000 to 500,000", "500,000 to 1,000,000","1,000,000 to 5,000,000","5,000,000 to 10,000,000","10,000,000 to 40,000,000") 

#apply the cut
mag_cut <- cut(mag_to_cut, breaks = cutpoints_mag, labels = cutlbls_mag)

#replace freq with new factors
mi_mag$total <- mag_cut

#####################
# Combinations and Plots
#####################

###########
# Combination poverty/release frequency dataframe
###########
#merge factored/tabulated results with the poverty data
mi_rel_pov_mrg <- merge(mi_freq, mi_pov, by.x = c("county","year"), by.y = c("county_name","year"))

#remove factors from the poverty percentage data column
mi_rel_pov_mrg$all_age_pov_percent <- as.numeric(as.character(mi_rel_pov_mrg$all_age_pov_percent))

mi_pov_med <- summaryBy(all_age_pov_percent ~ county, data = mi_rel_pov_mrg, fun = c(median))
colnames(mi_pov_med) <- c("county", "pov")

mi_pov_rank <- orderBy(~-pov, data = mi_pov_med)

county_pov_order <- mi_pov_rank$county

#use the vector of county names add levels of the county column in the dataframe
mi_rel_pov_mrg$county  <- ordered(mi_rel_pov_mrg$county , levels = county_pov_order)

#new ggplot object
mi_pov_freq <- ggplot(mi_rel_pov_mrg)

#####
# Poverty/frequency heat map based on http://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
#####
mi_pov_freq <- mi_pov_freq + geom_tile(aes(year, county, fill = factor(freq)), color = "white") + scale_fill_brewer("Frequency", type="seq") + opts(axis.text.y = theme_text(hjust=1, size=5))
mi_pov_freq + scale_x_discrete("Year") + scale_y_discrete("Counties (Ordered by decending poverty rate)") + opts(title="MI Counties, frequency of toxic releases")
ggsave(filename="C:\\Users\\dnfehren\\Desktop\\mi_pov_freq.pdf",width = 10, height = 6)

###########
#Create combination poverty/magnitude dataframe
###########
#merge factored/tabulated results with the poverty data
mi_mag_pov_mrg <- merge(mi_mag, mi_pov, by.x = c("county","year"), by.y = c("county_name","year"))

#remove factors from the poverty percentage data column
mi_mag_pov_mrg$all_age_pov_percent <- as.numeric(as.character(mi_mag_pov_mrg$all_age_pov_percent))

#use the vector of county names add levels of the county column in the dataframe
mi_mag_pov_mrg$county  <- ordered(mi_mag_pov_mrg$county , levels = county_pov_order)

#new ggplot object
mi_pov_mag <- ggplot(mi_mag_pov_mrg)

#####
#poverty/magnitude heat map
#####
mi_pov_mag + geom_tile(aes(factor(year), county, fill = factor(total)), color = "white") + scale_fill_brewer("Release Amount (lbs)", type="seq")+ opts(title = "MI county trends in magnitude of toxic releases", axis.text.y = theme_text(hjust=1, size=5)) + scale_y_discrete("Counties (Ordered by decreasing percentage of residents in poverty)")
ggsave(filename="C:\\Users\\dnfehren\\Desktop\\mi_pov_mag.pdf",width = 10, height = 6)


###################
#Create combination race/frequency data
###################
#merge factored/tabulated results with the race data
mi_freq_race_mrg <- merge(mi_freq, mi_race, by.x = "county", by.y = "county_name")

#sort by the percentage of non-white residents
mi_freq_race_sort <- orderBy(~-percent_non_white, data = mi_freq_race_mrg)

#get a vector of each county in MI ordered by the decending amount of non-white residents
county_order_race <- unique(as.character(mi_freq_race_sort$county))

#use the vector of county names add levels of the county column in the dataframe
mi_freq_race_sort$county  <- ordered(mi_freq_race_sort$county , levels = county_order_race)

##plot object with all freq data
#set data frame based on tablulated and merged table
mi_race_freq <- ggplot(mi_freq_race_sort)

#######
#race/frequency heat map with colorbrewer
#######
mi_race_freq <- mi_race_freq + geom_tile(aes(factor(year), county, fill = freq), color = "white") + scale_fill_brewer("Frequency", type="seq")  + opts(axis.text.y = theme_text(hjust=1, size=5))
mi_race_freq + scale_x_discrete("Year") + scale_y_discrete("County (Ordered by decendening percentage of white residents)") + opts(title="Michigan county trends in frequency of toxic releases")
ggsave(filename="C:\\Users\\dnfehren\\Desktop\\mi_race_freq.pdf",width = 10, height = 6)

###########
#Create combination race/magnitude dataframe
###########
#merge factored/tabulated results with the poverty data
mi_mag_race_mrg <- merge(mi_mag, mi_race, by.x = "county", by.y = "county_name")

#use the vector of county names add levels of the county column in the dataframe
mi_mag_race_mrg$county  <- ordered(mi_mag_race_mrg$county , levels = county_order_race)

#new ggplot object
mi_race_mag <- ggplot(mi_mag_race_mrg)

#####
#race/magnitude heat map
#####
mi_race_mag + geom_tile(aes(factor(year), county, fill = factor(total)), color = "white") + scale_fill_brewer("Release Amount (lbs)",type="seq")+ opts(title = "Michigan county trends in toxic releases", axis.text.y = theme_text(hjust=1, size=5)) + scale_y_discrete("Counties (Ordered by decendening percentage of white residents)")
ggsave(filename="C:\\Users\\dnfehren\\Desktop\\mi_race_mag.pdf",width = 10, height = 6)

############################################
#regional trends
############################################

#table to summarize data by year and region
region_year <- as.data.frame(table(releases$year,releases$region))
colnames(region_year) <- c("year","region", "count")

region_plot <- ggplot(region_year, aes(x=year, y=count, group=region, color=region))

region_plot + geom_line() + scale_colour_manual("Region", values = c("ALASKA"="#A6CEE3", "EAST NORTH CENTRAL"="#1F78B4","EAST SOUTH CENTRAL"="#B2DF8A","MID ATLANTIC"="#33A02C"
,"MOUNTAIN"="#FB9A99","NEW ENGLAND"="#E31A1C","PACIFIC"="#FDBF6F","PACIFIC ISLANDS"="#FF7F00","SOUTH ATLANTIC"="#CAB2D6","WEST NORTH CENTRAL"="#6A3D9A","WEST SOUTH CENTRAL"="#FFFF99")) + opts(title = "Regional trends in reported toxic releases") + scale_x_discrete("Year") + scale_y_continuous("Number of Toxic Release Events")
ggsave("C:\\Users\\dnfehren\\Desktop\\region_trends.pdf", height=5, width=7)

############################################
#state and regional trends
############################################

#table to summarize data by year and state
state_year <- as.data.frame(table(releases$year,releases$facility_state))
colnames(state_year) <- c("year","facility_state", "count")

state_year_regions <- regionalize(state_year)

state_year_regions$region[state_year_regions$region == "EAST NORTH CENTRAL"] <- "E N CENTRAL"
state_year_regions$region[state_year_regions$region == "EAST SOUTH CENTRAL"] <- "E S CENTRAL"
state_year_regions$region[state_year_regions$region == "WEST NORTH CENTRAL"] <- "W N CENTRAL"
state_year_regions$region[state_year_regions$region == "WEST SOUTH CENTRAL"] <- "W S CENTRAL"

#point for each state
state_plot <- ggplot(state_year_regions, aes(year, count, group = facility_state, color = facility_state)) + geom_point() + facet_grid(.~region)

state_plot <- state_plot + scale_x_discrete("Year") + scale_y_continuous("Number of Toxic Release Events") + opts(title="State trends in toxic releases between 1999 and 2008")+ opts(axis.text.x = theme_text(angle = 90, size=7))

direct.label(state_plot,list(first.points, hjust = .1))

ggsave("C:\\Users\\dnfehren\\Desktop\\state_trends.pdf", height=5, width=13)
