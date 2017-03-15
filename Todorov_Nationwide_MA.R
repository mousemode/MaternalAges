# REQUIRED PACKAGES
install.packages("dplyr")
install.packages("googleVis")
install.packages("tidyjson")
install.packages("ggplot2")
install.packages("RColorBrewer")

library(dplyr)
library(googleVis)
library(tidyjson)
library(ggplot2)
library(RColorBrewer)

# DATA SETUP
# My dataset 'raw' has the setup: state + maternal age + education + race ~ number of births
# The variables from NCSS are called: State + Age.of.Mother.Code + Education + Race ~ Births

# CREATE GROUP SUBSETS FOR STRAIFICATION
# "all" will be used to analyse the entire dataset
groups <- c(levels(raw$Education), levels(raw$Race), "all")

# CREATE AGE SUBSETS
ages <- unique(raw$Age.of.Mother.Code)

# CREATE NATIONWIDE GRAPHS, BOTH HTML AND GOOGLE GADGETS ####
for(n in 1:length(groups)){
  # Subset the data by levels of the grouping factor
  if(groups[n]!="all"){dat <- raw[raw$Education==groups[n] | raw$Race==groups[n],]} else {dat <- raw}
  
  # Summarize the data to combine all births within each level of the grouping factor
  dat <- dat %>% group_by(State, Age.of.Mother.Code, Live.Birth.Order.Code) %>% summarise (Births = sum(Births))
  
  # Summarize BO as weighted mean B.O. in a new dataset
  gdat <- dat %>% group_by(State, Age.of.Mother.Code) 
  			  %>% summarise (BO.Mean = weighted.mean(Live.Birth.Order.Code, Births))
  gdat$BO.Mean <- round(gdat$BO.Mean,2)
  
  # Calculate Births.MA and add to the gdat file
  Births.MA <- dat %>% group_by(State, Age.of.Mother.Code) %>% summarise (Births.MA = sum(Births))
  gdat <- merge(gdat, Births.MA, by=c("State","Age.of.Mother.Code"))
  rm(Births.MA)
  
  # Calculate Births.MA as a percent of Births.State
  Births.State <- dat %>% group_by(State) %>% summarise (Births.State = sum(Births))
  gdat <- merge(gdat, Births.State, by=c("State"))
  gdat$Births.State.Percent <- round(100*gdat$Births.MA / gdat$Births.State,1)
  gdat <- gdat[,-5]
  rm(Births.State)
  
  # Get data visually ready for graphing
  gdat$Births.State.Percent <- paste(as.factor(gdat$Births.State.Percent),"% of", gdat$State, "Births", sep = " ")
  colnames(gdat)[3] <- "Mean Birth Order"
  
  # Split data by maternal age and graph each seperately
  for(x in 1:length(ages)){
    subgdat <- gdat[gdat$Age.of.Mother.Code==ages[x],]
    map <- gvisGeoChart(subgdat, locationvar = "State", colorvar = "Mean Birth Order", 
                        hovervar = "Births.State.Percent",
                        options = list(region='US', displayMode="regions",resolution="provinces",
                                       colorAxis="{values:[0,1,2,3,4,5,6,7], colors:['#F8F9F9','#CCFFFF','#66F2F5','#668FC2','#6670A3','#665285','#66295C','#660033']}"
                        ))
    filename <- paste(n,"_",ages[x],".html", sep="")
    cat(map$html$chart, file=filename)
    
    gadget <- createGoogleGadget(map)
    gadfilename <- paste(n,"_",ages[x],".xml", sep="")
    cat(gadget, file=gadfilename)
    
    rm(subgdat, map)
  }
  rm(dat) #keep gdat for the blank map step
}

rm(gdat,filename,n,x, gadget, gadfilename)