#load libraries
library(RSQLite)
library(ggplot2)

#connect to database
#con <- dbConnect(SQLite(), "/users/dnfehren/Desktop/epa.sqlite")
con <- dbConnect(SQLite(), "C:\\Users\\dnfehren\\Desktop\\epa.sqlite")

#send query and load data into R data frame
query <- dbSendQuery(con, statement = "SELECT TRI_FACILITY_ID,Fugitive_Air,Stack_Air,Water,Underground_Class_I,Underground_Class_II_V,RCRA_C_Landfills,Other_Landfills,Land_Treatment,Surface_Impoundment,RCRA_C_Surface_Impoundment,Other_Surface_Impoundment,Other_Disposal,POTW_Total_Transfers,M10,M41,M62,M71,M81,M82,M72,M63,M66,M67,M64,M65,M73,M79,M90,M94,M99,M20,M24,M26,M28,M93,M56,M92,M40,M50,M54,M61,M69,M95 FROM tri");

return <- fetch(query, n = -1)

#get the on site data columns
on_site_wide <- return[c("TRI_Facility_ID","Fugitive_Air","Stack_Air","Water","Underground_Class_I","Underground_Class_II_V","RCRA_C_Landfills","Other_Landfills","Land_Treatment","Surface_Impoundment","RCRA_C_Surface_Impoundment","Other_Surface_Impoundment","Other_Disposal")]

#rename them
names(on_site_wide) <- c("Facility ID","Air Leak","Air Release", "Water", "Strict Well", "Other Well", "Strict Landfill", "Other Landfill", "Soil", "Surface Holding", "Strict Surface Holding","Other Surface","Other")

#get the off site data columns
off_site_wide <- return[c("TRI_Facility_ID","POTW_Total_Transfers","M10","M41","M62","M71","M72","M63","M64","M65","M73","M79","M90","M94","M99")]

#rename them as well
names(off_site_wide) <- c("Facility ID","OS POTW","OS Storage","OS Solidification","OS WasteWater Treatment", "OS Well", "OS Old Well Data", "OS Surface", "OS Landfill", "OS Strict Landfill", "OS Soil", "OS Other Surface","OS Other", "Waste Broker", "Unknown")

#melt the wide data frames into long frames each column name becomes a variable
on_site_full <- melt.data.frame(on_site_wide, id=1)
off_site_full <- melt.data.frame(off_site_wide, id=1)

#remove variables with no entries
on_site <- subset(on_site_full, on_site_full$value != 0)
off_site <- subset(off_site_full, off_site_full$value != 0)

#histogram of onsite
qplot(variable, data=on_site, geom="histogram") 
#ggsave(file="on_site_release_histogram.png", width=10, height=6)

#histogram of offsite
qplot(variable, data=off_site, geom="histogram")
#ggsave(file="off_site_release_histogram.png", width=10, height=6)

#create new dataframes using tabluated values from the on and off site frames
on_df <- as.data.frame(table(on_site$variable))
off_df <- as.data.frame(table(off_site$variable))
combo_df <- rbind(on_df,off_df)
names(combo_df) <- c("type","count")

#pull out the release methods that match between on and off site
similar_methods <- combo_df[c(6,21,7,20,8,22,11,23),]

#create new columns to allow for easier factor plotting
similar_methods["site"] <- c("on","off","on","off","on","off","on","off")
similar_methods["type"] <- c("Strict Landfill","Strict Landfill","Landfill","Landfill","Soil","Soil","Surface","Surface")

#plot layers
p <- ggplot(similar_methods)
#p + layer(geom="bar")

p <- p + aes(factor(type))
#p + layer(geom="bar")

p <- p + aes(factor(type), weight = count)
#p + layer(geom="bar")

p <- p + aes(factor(type), fill = factor(site))
#p + layer(geom="bar")

p + geom_bar(position="dodge")
#ggsave(file="comparison_barchart.png", width=10, height=6)

