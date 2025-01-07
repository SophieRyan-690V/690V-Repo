
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)



#getting the data TABLE from the file in the cloud:
load(file=url(link))

# subsetting

townEduwa=eduwa[eduwa$LocaleType=='Town',]
townEduwa$LocaleSub=droplevels(townEduwa$LocaleSub)
table = table(townEduwa$LocaleSub)
proptable = prop.table(table)*100
# as data frame
tableFreq=as.data.frame(table)
# renaming data frame columns
names(tableFreq)=c("Town","Count")
# adding percents:
tableFreq$Percent=as.vector(proptable)
# then, you have:
tableFreq


# deliverable 1 ----------------------------------------------------------

library(ggplot2)

titleText='What Types of Towns are Public Schools Located?'
sub_titleText='Washington State - 2019'
sourceText='Source: US Department of Education'

x.AxisText="Locations"
y.AxisText="Count"




base= ggplot(data = tableFreq, 
             aes(x = reorder(Town,Percent),y = Percent)) 
base= base + theme_classic()
##
plot1 = base + geom_bar(fill ="gray",
                        stat = 'identity') 
plot2 = plot1 + labs(title=titleText,
                     subtitle = sub_titleText,
                     x =NULL, 
                     y = NULL,
                     caption = sourceText)
plot3 = plot2 + geom_hline(yintercept = 25, 
                           linetype="dashed", 
                           linewidth=1.5, 
                           alpha=0.5)
plot4 = plot3 + scale_y_continuous(breaks=c(0,25,50),
                                   limits = c(0, 50),
                                   labels=scales::unit_format(suffix = '%')) 

plot5 = plot4 + theme(plot.caption = element_text(hjust = 0),
                      plot.title = element_text(hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5))


final1 = plot5 + geom_text(aes(label = paste0(round(Percent,2),'%')))



# save del1Draft ----------------------------------------------------------
saveRDS(final1, file = "final1.rds")



