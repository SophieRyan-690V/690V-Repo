# clean memory ------------------------------------------------------------
rm(list = ls()) # start fresh

library(sf)

# read in data ------------------------------------------------------------

linkBoston="https://github.com/DACSS-Visual/SpatialData/raw/refs/heads/main/data/BostonContrib.xlsx"

bostonCont=rio::import(linkBoston)

#see it
head(bostonCont)

library(sf)
linkZips='https://raw.githubusercontent.com/DACSS-Visual/SpatialData/refs/heads/main/data/zip_codes.json'
bostonZips=sf::read_sf(linkZips)

#see it
head(bostonZips)

plot(bostonZips[2])

summary(bostonCont$Amount)

tapply(bostonCont$Amount,bostonCont$`Tender Type Description`,summary)

str(bostonCont,width = 60, strict.width = 'cut')

bosContType=bostonCont[bostonCont$Amount > 0 & bostonCont$`Tender Type Description`%in% c('Cash','Credit Card'),]

table(bosContType$Amount,bosContType$`Tender Type Description`)


library(dplyr)

bostCont_agg <- bosContType %>%
  group_by(Zip, `Tender Type Description`) %>%
  summarise(
    counts = n(),                       # Number of rows
    amount = mean(Amount, na.rm = TRUE),  # Average amount
    .groups = "drop"                    # Ungroup after summarising
  )

# View the result
print(bostCont_agg)


bostCont_agg <- bostCont_agg%>%
  group_by(Zip) %>%
  mutate(percentage = amount / sum(amount, na.rm = TRUE) * 100) %>%
  ungroup()  # Ungroup to prevent further unintended grouping

bostCont_agg$amount=NULL

bostCont_agg %>%
  group_by(Zip) %>%
  summarise(total_percentage = sum(percentage, na.rm = TRUE)) %>%
  print(n = 10)


# map to the left!

bostCont_bostZips=merge(bostonZips,bostCont_agg,
                     by.x='ZIP5', # 
                     by.y='Zip')
# you see
head(bostCont_bostZips)

base=ggplot() + theme_void() 

final3 = base + geom_sf(data=bostCont_bostZips,
               aes(fill=percentage)) + 
  scale_fill_viridis_c(direction = -1,
                       na.value = 'red') + # missing in red?
  facet_grid(~ `Tender Type Description`) +
  labs(fill='Percent Contribution Per Tender Type\n (white:no information)',
       title = 'Cash or Card? Stark Differences in Payment Types for Boston Political Contributors',
       subtitle = 'Massachusetts Office of Campaign and Political Finance (2024)') +
  theme(
    plot.title = element_text(hjust = 0.6, size = 11, face = "bold"),  # Center title
    plot.subtitle = element_text(hjust = 0.6, size = 11),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm")
  ) 

final3

# save del3Draft ----------------------------------------------------------
saveRDS(final3, file = "final3.rds")

