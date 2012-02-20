############################################
#  daniel fehrenbach
#  dnfehrenbach@gmail.com
#
#  advanced graphics and final polishing in R
############################################


#load libraries
library(RSQLite)
library(ggplot2)
library(doBy)

source(file = "/users/dnfehren/Desktop/final_asm/regionalize.RData")
#source(file = "C:\\Users\\dnfehren\\Desktop\\final_asm\\regionalize.RData")

#database connection
con <- dbConnect(SQLite(), "/users/dnfehren/Desktop/epa_99_08.s3db")
#con <- dbConnect(SQLite(), "C:\\Users\\dnfehren\\Desktop\\epa_99_08.s3db")

#query
query <- dbSendQuery(con, statement = "SELECT year, facility_state, facility_county, (total_air+total_surface_water+total_underground_water+total_land) AS total_release FROM tri")

#load query into R data object
ret <- fetch(query, n = -1)

releases <-regionalize(ret)

#factor year, states and regions
factor(releases$year)
factor(releases$region)
factor(releases$facility_state)


#load poverty statistics
pov <- read.csv("/Users/dnfehren/Desktop/final_asm/poverty_data.csv")
#pov <- read.csv("C:\\Users\\dnfehren\\Desktop\\final_asm\\poverty_data.csv")

#get MI data from the releases and the poverty data
mi_pov <- subset(pov, state == "MI")
mi_rel <- subset(releases, facility_state == "MI")

#tabulate the occurences of release by year and county
mi_tab <- as.data.frame(table(mi_rel$year, mi_rel$facility_county))
colnames(mi_tab) <- c("year","county", "freq")

#merge tabulate results with the poverty data
mi_mrg <- merge(mi_tab, mi_pov, by.x = c("county","year"), by.y = c("county_name","year"))

#remove factors from the poverty percentage data column
mi_mrg$all_age_pov_percent <- as.numeric(as.character(mi_mrg$all_age_pov_percent))

#set data frame based on tablulated and merged table
mi_rel_freq <- ggplot(mi_mrg)

#plot points
mi_rel_freq <- mi_rel_freq + geom_point(aes(all_age_pov_percent,freq, color=year))

#change the color  of the points manually
mi_rel_freq <- mi_rel_freq + scale_colour_manual("Year", values = c("1999"="#F7FBFF", "2000"="#DEEBF7", "2001"="#C6DBEF", "2002"="#9ECAE1", "2003"="#6BAED6", "2004"="#4292C6", "2005"="#2171B5", "2006"="#08519C", "2007"="#08306B", "2008"="#06234D"))

#add plot and axis titles
mi_rel_freq <- mi_rel_freq + opts(title = "Connecting poverty rates and number of toxic releases in MI counties") + scale_x_continuous("Percent of Total Population Below Poverty Line") + scale_y_continuous("Number of Toxic Release Events")

mi_rel_freq