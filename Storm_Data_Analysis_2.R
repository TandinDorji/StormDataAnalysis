setwd("C:/Users/Tandin Dorji/OneDrive/Life Long Learning/Coursera/Course5 - RR/CourseProject_StormData")

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

# install libraries
require(tidyverse)
if(!require(treemap)) {
        install.packages("treemap")
        library(treemap)
}


# unhide to download again
# tempFile <- download.file(url, "StormData.csv")
rm(url)


# read and check original dataset 
sd <- read.csv("StormData.csv", header = TRUE)
dim(sd)
head(sd)
str(sd)
names(sd)


# select only required variables for analysis
storm <- sd[, c(8, 23:28, 36)]
rm(sd)
gc()
View(storm)




# Across the United States, which types of events
# (as indicated in the EVTYPE variable) are most harmful
# with respect to population health?
fatality <- storm %>% 
        group_by(EVTYPE) %>% 
        summarise(Total.Fatalities = sum(FATALITIES)) %>% 
        arrange(desc(Total.Fatalities))

lrow <- nrow(fatality)
fatality.summ <- data.frame("Event.Type" = c((fatality$EVTYPE[1:10]), "Others"),
                            "Fatality" = c(fatality$Total.Fatalities[1:10], 
                                           sum(fatality$Total.Fatalities[11:lrow])))


injury <- storm %>% 
        group_by(EVTYPE) %>% 
        summarise(Total.Injuries = sum(INJURIES)) %>% 
        arrange(desc(Total.Injuries))

injury.summ <- data.frame("Event.Type" = c((injury$EVTYPE[1:10]), "Others"),
                            "Injury" = c(injury$Total.Injuries[1:10], 
                                           sum(injury$Total.Injuries[11:lrow])))

odr <- seq(1:11)
ggplot(fatality.summ, aes(x = reorder(Event.Type, odr), y = Fatality)) + 
        geom_bar(stat = "identity", aes(fill = Event.Type)) + 
        labs(title = "Fatality by event type") +
        theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title.x = element_blank(),
                legend.position = "none"
        )

ggplot(injury.summ, aes(x = reorder(Event.Type, odr), y = Injury)) + 
        geom_bar(stat = "identity", aes(fill = Event.Type)) + 
        labs(title = "Injury by event type") +
        theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title.x = element_blank(),
                legend.position = "none"
        )




# Across the United States, which types of events
# have the greatest economic consequences?
table(storm$CROPDMGEXP)
incorrect <- storm[storm$CROPDMGEXP %in% c("?", "0", "2"), 6:7]
table(incorrect)
sum(table(incorrect))

blank <- storm[storm$CROPDMGEXP %in% c(""), 6:7]
table(blank)
blank[blank$CROPDMG > 0, ]


# All crop damage values with crop damage xp as blank, ?, 0 or 2
# will be replaced by zero and xp recoded as 0
# k and m will be recoded as K and M respectively for uniformity
storm[storm$CROPDMGEXP %in% c("", "?", "0", "2"), 6:7] <- 0
storm[storm$CROPDMGEXP %in% c("k"), 7] <- "K"
storm[storm$CROPDMGEXP %in% c("m"), 7] <- "M"
table(storm$CROPDMGEXP)
sum(table(storm$CROPDMGEXP))


table(storm$PROPDMGEXP)
# table(storm[!(storm$PROPDMGEXP %in% c("", "K", "m", "M", "B")), 12:13])
# All prop damage values with prop damage xp other than K, M/m, or B
# will be replaced by zero and xp recoded as 0
# k and m will be recoded as K and M respectively for uniformity
storm[!(storm$PROPDMGEXP %in% c("K", "m", "M", "B")), 4:5] <- 0
storm[storm$PROPDMGEXP %in% c("k"), 5] <- "K"
storm[storm$PROPDMGEXP %in% c("m"), 5] <- "M"
table(storm$PROPDMGEXP)
sum(table(storm$PROPDMGEXP))

Prop.XP <- case_match(storm$PROPDMGEXP,
                      "K" ~ 1e3,
                      "M" ~ 1e6,
                      "B" ~ 1e9,
                      .default = 1)

Crop.XP <- case_match(storm$CROPDMGEXP,
                      "K" ~ 1e3,
                      "M" ~ 1e6,
                      "B" ~ 1e9,
                      .default = 1)
storm <- cbind(storm, Prop.XP, Crop.XP)
storm$Damage.Value <-  (storm$PROPDMG * storm$Prop.XP + 
                                storm$CROPDMG * storm$Crop.XP)/1e9

storm %>% 
        group_by(EVTYPE) %>% 
        summarise(DMG.VAL = sum(Damage.Value)) %>% 
        mutate(Event.Percent = 
                       paste0(EVTYPE, "\n", 
                              round(DMG.VAL/sum(DMG.VAL)*100, 2), 
                              "%" )) %>%
        treemap(
        index = "Event.Percent",
        vSize = "DMG.VAL",
        type = "index",
        title = "Events and their economic consequences")

# treemap(storm,
#         index = "EVTYPE",
#         vSize = "FATALITIES",
#         type = "index",
#         fun.aggregate = "sum",
#         title = "Events and their economic consequences")
# 
# treemap(storm,
#         index = "EVTYPE",
#         vSize = "INJURIES",
#         type = "index",
#         fun.aggregate = "sum",
#         title = "Events and their economic consequences")
