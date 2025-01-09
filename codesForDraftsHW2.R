
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

linkMass="https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx"

#see it
library(rio)
arrests=rio::import(linkMass,which = 1)
head(arrests)

names(arrests)

summary(arrests$Age)

tapply(arrests$Age,arrests$`Arrest Type`, summary)

library(dplyr)

arrests <- arrests %>%
  mutate(
    `Arrest Type` = case_when(
      `Arrest Type` == "F" ~ "Felony",
      `Arrest Type` == "J" ~ "Juvenile",
      `Arrest Type` == "M" ~ "Misdemeanor",
      `Arrest Type` == "W" ~ "Warrant",
      `Arrest Type` == "O" ~ "Other",
      TRUE ~ `Arrest Type`  
    )
  )

tapply(arrests$Age,arrests$`Arrest Type`, summary)

summaryBy=aggregate(data=arrests,
                    Age~`Arrest Type`,
                    FUN = function(x) c(median = median(x),
                                        max=max(x)) )

summaryBy=do.call(data.frame,summaryBy)
summaryBy

names(summaryBy)=c('Arrest Type','median','max')

summaryBy

summaryBy_long=reshape2::melt(summaryBy,variable.name = 'stats',
                              value.name = 'Age',
                              id.vars='Arrest Type')
summaryBy_long

summaryBy_long$`Arrest Type` <- factor(
  summaryBy_long$`Arrest Type`,
  levels = c("Felony", "Misdemeanor", "Warrant", "Other") # Desired order
)

summaryBy_long <- summaryBy_long[order(summaryBy_long$`Arrest Type`), ]

summaryBy_long

titleText='Age Disparities in Arrest Types - Felonies Associated With Younger Offenders'
sub_titleText='Massachusetts Field Services and Investigative Divisions Arrests - January 2019 to March 2020'
sourceText='Source: Massachusetts State Police'

base1=ggplot(data=summaryBy_long,
             aes(x = reorder(`Arrest Type`, desc(`Arrest Type`)), y= Age,
                 fill=stats)) # fill brings a legend

barDodge= base1 +  geom_bar(stat="identity",
                            position ='dodge') 
barDodge = barDodge + theme_minimal() + labs(title=titleText,
                             subtitle = sub_titleText,
                             caption = sourceText,
                            y="Age",
                           x="Arrest Type")
barDodge = barDodge + geom_text(size = 4,
                     position = position_dodge(1),hjust=0,
                     aes(label=round(Age,1)))+
  coord_flip()

barDodge = barDodge + theme(plot.caption = element_text(hjust = 0),
                      plot.title = element_text(hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5)) 
barDodge

final2 = barDodge

# save del2Draft ----------------------------------------------------------
saveRDS(final2, file = "final2.rds")

