#daniel fehrenbach
#dnfehrenbach@gmail.com
#geoms and stats examples in R

#load libraries
library(RSQLite)
library(ggplot2)

#database connection
con <- dbConnect(SQLite(), "/users/dnfehren/Desktop/epa_99_08.s3db")

#query
query <- dbSendQuery(con, statement = "SELECT tri_id, year, facility_state FROM tri")

#load query into R data object
ret <- fetch(query, n = -1)

#table to confirm data
table(ret$facility_state, ret$year)

#query data is split apart based on US census regions, a region column is added for identification
#alaska 
akl <- subset(ret, ret$facility_state == "AK")
akl$region <- "ALASKA"

#pacific islands 
pil <- subset(ret, ret$facility_state == "HI" | ret$facility_state == "AS" | ret$facility_state =="GU" | ret$facility_state =="MP" | ret$facility_state =="PR" | ret$facility_state =="VI")
pil$region <- "PACIFIC ISLANDS"

#pacific
pac <- subset(ret, ret$facility_state == "CA" | ret$facility_state == "OR" | ret$facility_state == "WA")
pac$region <- "PACIFIC"

#mountain 
mtn <- subset(ret, ret$facility_state == "MT" | ret$facility_state == "ID" | ret$facility_state == "NV" | ret$facility_state == "WY" | ret$facility_state == "UT" | ret$facility_state == "CO" | ret$facility_state == "AZ" | ret$facility_state == "NM")
mtn$region <- "MOUNTAIN"

#west north central 
wnc <- subset(ret, ret$facility_state == "ND" | ret$facility_state == "MN" | ret$facility_state == "SD" | ret$facility_state == "IA" | ret$facility_state == "NE" | ret$facility_state == "KS" | ret$facility_state == "MO")
wnc$region <- "WEST NORTH CENTRAL"

#west south central 
wsc <- subset(ret, ret$facility_state == "OK" | ret$facility_state == "AR" | ret$facility_state == "TX" | ret$facility_state == "LA")
wsc$region <- "WEST SOUTH CENTRAL"

#east north central 
enc <- subset(ret, ret$facility_state == "WI" | ret$facility_state == "MI" | ret$facility_state == "IL" | ret$facility_state == "IN" | ret$facility_state == "OH")
enc$region <- "EAST NORTH CENTRAL"

#east south central 
esc <- subset(ret, ret$facility_state == "KY" | ret$facility_state == "TN" | ret$facility_state == "MS" | ret$facility_state == "AL")
esc$region <- "EAST SOUTH CENTRAL"

#new england 
ngl <- subset(ret, ret$facility_state == "ME" | ret$facility_state == "VT" | ret$facility_state == "NH" | ret$facility_state == "MA" | ret$facility_state == "RI" | ret$facility_state == "CT")
ngl$region <- "NEW ENGLAND"

#mid-atlantic 
mat <- subset(ret, ret$facility_state == "NY" | ret$facility_state == "NJ" | ret$facility_state == "PA")
mat$region <- "MID ATLANTIC"

#south-atlantic 
sat <- subset(ret, ret$facility_state == "MD" | ret$facility_state == "DE" | ret$facility_state == "DC" | ret$facility_state == "WV" | ret$facility_state == "VA" | ret$facility_state == "NC" | ret$facility_state == "SC" | ret$facility_state == "GA" | ret$facility_state == "FL")
sat$region <- "SOUTH ATLANTIC"

#the regional dataframes are recombined
rel_regions <- rbind(akl,pil,pac,mtn,wnc,wsc,enc,esc,ngl,mat,sat)

#factor year, states and regions
factor(rel_regions$year)
factor(rel_regions$region)
factor(rel_regions$facility_state)

#plot data object
plot <- ggplot(rel_regions)

#histogram of overall dumping events per region over full time period
plot + geom_histogram(aes(region, fill = region))
ggsave(file="large_data_histogram.png", width=10, height=6)

#density plot of overall dumping events per region over full time period
plot + geom_density(aes(region, fill = region))
ggsave(file="large_data_density.png", width=10, height=6)

#2d histogram / binned plot showing the count of release occurences per region per year
plot + geom_bin2d(aes(factor(year),factor(region)))
ggsave(file="large_data_per_year_per_region.png", width=10, height=6)

#plot + geom_hex(aes(factor(year),factor(region)))



