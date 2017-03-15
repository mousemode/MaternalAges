setwd("~/Documents/Pregnancy Project/Nationwide")
raw <- read.delim("~/Documents/Pregnancy Project/Nationwide/nationwide_raw_data.txt") #18762 observations

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

# Combine education groups
levels(raw$Education) <- c(levels(raw$Education),"Less than a GED","GED","Post-secondary degree","Post-graduate degree")
raw$Education[raw$Education %in% c("8th grade or less","9th through 12th grade with no diploma")] <- "Less than a GED"
raw$Education[raw$Education %in% c("High school graduate or GED completed")] <- "GED"
raw$Education[raw$Education %in% c("Associate degree (AA, AS)","Bachelor's degree (BA, AB, BS)","Some college credit, but not a degree")] <- "Post-secondary degree"
raw$Education[raw$Education %in% c("Master's degree (MA, MS)","Doctorate (PHD, EdD) or Professional Degree (MD, DDS, DVM, LLB, JD)")] <- "Post-graduate degree"
unique(raw$Education)

# Create the comparison groups ####
groups <- c(intersect(levels(raw$Education),raw$Education), intersect(levels(raw$Race),raw$Race), "all")
groups <- groups[!groups %in% c("","Excluded","Unknown/Not on certificate")]

dict <- as.data.frame(groups)
dict$code <- c(1:9)
write.csv(dict,"groupcodes.txt")
rm(dict)

ages <- unique(raw$Age.of.Mother.Code)


# Remove the empty rows, notes, and unknown BO's ####
raw <- raw[!raw$State=="",]
raw <- raw[,-1]
raw <- raw[!raw$Live.Birth.Order.Code==99,]
unique(raw$Live.Birth.Order.Code)

# Make the nationwide graphs ####
setwd("~/Documents/Pregnancy Project/Nationwide/Outputs")
for(n in 1:length(groups)){
  # Subset the data
  if(groups[n]!="all"){dat <- raw[raw$Education==groups[n] | raw$Race==groups[n],]} else {dat <- raw}
  
  # Now ignore the other factors and summarize
  dat <- dat %>% group_by(State, Age.of.Mother.Code, Live.Birth.Order.Code) %>% summarise (Births = sum(Births))
  
  # Summarize BO as weighted mean B.O. in a new dataset
  gdat <- dat %>% group_by(State, Age.of.Mother.Code) %>% summarise (BO.Mean = weighted.mean(Live.Birth.Order.Code, Births))
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
rm(gdat,filename,n,x)
rm(gadget, gadfilename)

# Generate a blank map ####
empty <- as.data.frame(matrix(0, nrow=51, ncol=5))
colnames(empty) <- c("State", "Age.of.Mother.Code","Mean Birth Order","Births.MA","Births.State.Percent")
empty$State <- unique(raw$State)

map <- gvisGeoChart(empty, locationvar = "State", colorvar = "Mean Birth Order", 
                    hovervar = "Births.State.Percent",
                    options = list(region='US', displayMode="regions",resolution="provinces",
                                   colorAxis="{values:[0,1,2,3,4,5,6,7], colors:['#F8F9F9','#CCFFFF','#66F2F5','#668FC2','#6670A3','#665285','#66295C','#660033']}"
                    ))
cat(unlist(map$html), file="blank.html")
rm(empty, map)

# Accompanying plot ####
setwd("~/Documents/Pregnancy Project/Nationwide/Graphs")

# Use a different ages system than before
ages <- as.data.frame(unique(raw$Age.of.Mother.Code))
colnames(ages) <- "Age.of.Mother.Code"
ages$MA.Num <- c(1:9)
  
# Generate data for national background curve #### 
USdat <- raw %>% group_by(Age.of.Mother.Code) %>% summarise (Births.Nat = sum(Births))
USdat$Births.Nat.Percent <- 100*USdat$Births.Nat / sum(USdat$Births.Nat)
# Codify the maternal age
USdat <- merge(USdat, ages, by=c("Age.of.Mother.Code"))

# Make a color fill cheat: set all the BO to 0
USdat$Live.Birth.Order.Code <- 0
USdat$Live.Birth.Order.Code <- as.factor(USdat$Live.Birth.Order.Code)

# Generate the maps for each factor group, with a line indicating current age ####
for(n in 1:length(groups)){
  # Subset the data
  if(groups[n]!="all"){dat <- raw[raw$Education==groups[n] | raw$Race==groups[n],]} else {dat <- raw}
  
  # Summarise data ignoring state and grouping factor
  dat <- dat %>% group_by(Age.of.Mother.Code, Live.Birth.Order.Code) %>% summarise (Births.Nat = sum(Births))
  
  # Alter data for graphing
  dat$Live.Birth.Order.Code <- as.factor(dat$Live.Birth.Order.Code)
  dat <- merge(dat, ages, by=c("Age.of.Mother.Code"))
  
  # Calculate Births as a percentage of Births.USA
  dat$Births.Nat.Percent <- 100*dat$Births.Nat / sum(dat$Births.Nat)
  dat <- as.data.frame(dat)

  # Graph maternal age by percentage of births, stacked by birth order
  for(x in 1:nrow(ages)){
      graph <- ggplot() + 
        geom_bar(data=USdat, aes(x=MA.Num, y=Births.Nat.Percent, fill=Live.Birth.Order.Code), stat="identity", width=1) + 
        geom_bar(data=dat, aes(x=MA.Num, y=Births.Nat.Percent, fill=Live.Birth.Order.Code, order=desc(Live.Birth.Order.Code)),
                  position = 'stack', colour=NA, size=.2, alpha=1, stat="identity") + 
        labs(x="Maternal age (years)", y="Percent of Population's Births", text = element_text(size=12)) +
        scale_y_continuous(limits = c(0, 75), expand=c(0,0)) +
        scale_x_continuous(limits = c(0, 10), expand=c(0,0), breaks = c(1:9), labels = ages$Age.of.Mother.Code) +
        scale_fill_manual(values=c('grey','#CCFFFF','#66F2F5','#668FC2','#6670A3','#665285','#66295C','#660033')) +
        geom_vline(xintercept = ages$MA.Num[x]) + 
        theme(text = element_text(size=12)) +
        theme(legend.position = "none")
      
      filename <- paste(n,"_",ages$Age.of.Mother.Code[x],".jpg", sep="")
      ggsave(graph, filename = filename, width=5, height=3.47)
  }
}
rm(dat, filename, graph, n, x)
warnings()

?geom_vline
