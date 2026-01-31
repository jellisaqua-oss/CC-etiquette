#Outlines - Use 4 dashes to split your script into sections like so ----
#scripts often contain these sections

#Introduction----
#Author statement (what does this script do?), author(s) names, contact details, date

#Libraries----
#What packages are you using for this script. Keep them all together at the start
#A good example below:

# Analysing vertebrate population change based on the Living Planet Index
# Data available from http://www.livingplanetindex.org/

# Gergana Daskalova ourcodingclub(at)gmail.com
# 25-04-2017

# EXAMPLE Libraries ----
library(tidyr)  # Formatting data for analysis
library(dplyr)  # Manipulating data
library(ggplot2)  # Visualising results
library(readr)  # Manipulating data


#Functions----
#Are you using functions written by you/others? Define them here!
#Example - function to remove NA values, or create your own ggplot2 theme - add here

# EXAMPLE Defining functions ----
# A custom ggplot2 function
theme.LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position=c(0.9, 0.9))
}

#Set the working directory ----
setwd("C:/Users/ellijoel/OneDrive - Merck Sharp & Dohme LLC/Desktop/CC-etiquette-master")
#remember R requires all filepaths to be written using FORWARD slashes, not backslash.

# Import data ----
LPI <- read.csv("LPIdata_CC.csv")

# Formatting data ----
LPI2 <- gather(LPI, "year", "abundance", 9:53) #Transmofring there data from wide to long format, some blank cells may disappear
  #gather function requires tidyr package
LPI2$year <- parse_number(LPI2$year) #this removes the awkward Xs from in front of all the years
names(LPI2) #check what the different variables are called
names(LPI2) <- tolower(names(LPI2)) #make all the variable names lower case
names(LPI2)

#When manipulating data it's always good to check if the variables have stayed how we want them
#Use the str() function
str(LPI2)

#It looks like abundanceis a character variable, when we want it as numeric, lets change that
LPI2$abundance <- as.numeric(LPI2$abundance)

#Calc summary stats for each biome in the LPI database
levels(LPI2$biome) #list all biomes

LPI_biome_summ <- LPI2 %>% #use a pipe operator
    group_by(biome) %>% #group by biome
    summarise(populations = n()) #create columns, number of populations

#Visualising the number of populations in each biome with ggplot2 package ----
#putting your entire ggplot code in brackets creates the graph AND shows in it the plot viewer
(barplot <- ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
    theme.LPI() + #use of peronal theme here
   ylab("Number of populations") +
   xlab("Biome") +
   theme(legend.position = "none")) #remove the legend for simplicity

png(file="img/biome_pop.png", width = 1000, height = 2000) #Note that png() uses pixel values for width and height
ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
  theme.LPI()+
  ylab("Number of populations") +
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()

#2.1 Lets discuss naming conventions ----
#Variable names should be nouns - e.g. abundnace richness
#Function names should be verbs - e.g. calc.sp.richness

# Object names
#avg_clicks  # Good.
#avg.clicks  # Acceptable.
#avg_Clicks  # Not okay.

# Function names
#calculate.avg.clicks  # This is what we are aiming for.
#CalculateAvgClicks  # Not that bad, but mixing capital and lowercase letters can lead to typos
#calculate_avg_clicks , calculateAvgClicks  # Bad. The convention is that functions are defined using dots, not underscores.

#2.2 Spacing ----
#Place spaces around all infix operators (=, +, -, <-, etc) Always put a space AFTER a comma, and never before 
#Dont place a space before left parentheses, except in function calls func.x ()
#Extra spacing okay if it improves alignment of equal signs or alignments
#Do not place spaces around code in parenthese or sqr brackets (unless theres commas)
#Inline commenting: If you are commenting inline with code, place two spaces after the code, followed by #, a single space and then your text, 
  #e.g. summary(model)<space><space>#<space>comment.

