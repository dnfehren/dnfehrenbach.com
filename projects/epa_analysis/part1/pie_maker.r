rm(list=ls())

library(ggplot2)

chem = read.delim("C:\\Users\\dnfehren\\Desktop\\tri_2008_US_v08.txt")

attach(chem)

local_chem <- subset(chem, County == 'WASHTENAW')

pie <- ggplot(local_chem, aes(x=factor(1), fill = factor(Chemical)))

pie = pie + geom_bar(width=5, color="black")

pie = pie + coord_polar(theta="y")

pie