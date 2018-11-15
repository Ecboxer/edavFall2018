library(dplyr)
library(magrittr)
library(ggplot2)
DogsofNYC[DogsofNYC == 'n/a'] <- NA
NYCdogs[NYCdogs == 'n/a'] <- NA

# 1a
N <- nrow(NYCdogs)
dfMissing <- colSums(is.na(NYCdogs)) %>% 
  sort(decreasing = T) %>% 
  as_data_frame() %>% 
  mutate(feature = c('third_color', 'secondary_color',
                     'dog_name', 'dominant_color', 'gender', 
                     'breed', 'birth', 'spayed_or_neutered', 
                     'guard_or_trained', 'borough', 'zip_code',
                     'Group'),
         perc_missing = round(value / N, 4) * 100)
dfMissing %>% ggplot(aes(reorder(feature, -perc_missing, 'identity'),
                         perc_missing)) +
  geom_bar(stat = 'identity') +
  xlab('feature') + ylab('missing values') +
  ylim(c(0, 100))

# 1b
NYCdogs %>% extracat::visna()

# 1c
NYCdogs %>% group_by(gender) %>% 
  summarize(num_gender = n(),
            num_na = sum(is.na(dog_name))) %>% 
  mutate(percent_na = round(num_na / num_gender, 2)) %>% 
  arrange(-percent_na) %>% 
  ggplot(aes(gender, percent_na)) +
  geom_bar(stat = 'identity')
NYCdogs %>% group_by(Group) %>% 
  summarize(num_group = n(),
            num_na = sum(is.na(dog_name))) %>% 
  mutate(percent_na = round(num_na / num_group, 2)) %>% 
  arrange(-percent_na) %>% 
  ggplot(aes(reorder(Group, -percent_na), percent_na)) +
  geom_bar(stat = 'identity')
NYCdogs %>% group_by(breed) %>% 
  summarize(num_breed = n(),
            num_na = sum(is.na(dog_name))) %>% 
  mutate(percent_na = round(num_na / num_breed, 2)) %>% 
  arrange(-percent_na) %>% head(10)
NYCdogs %>% group_by(breed) %>% 
  summarize(num_breed = n(),
            num_na = sum(is.na(dog_name))) %>% 
  mutate(percent_na = round(num_na / num_breed, 2)) %>% 
  arrange(-percent_na) %>%
  filter(percent_na == 0) %>% count()
NYCdogs %>% group_by(borough) %>% 
  summarize(num_borough = n(),
            num_na = sum(is.na(dog_name))) %>% 
  mutate(percent_na = round(num_na / num_borough, 2)) %>% 
  arrange(-percent_na) %>% 
  ggplot(aes(borough, percent_na)) +
  geom_bar(stat = 'identity')

NYCdogs %>% group_by(breed) %>% 
  summarize(num_breed = n(),
            num_na = sum(is.na(dog_name))) %>% 
  mutate(percent_na = round(num_na / num_breed, 4) * 100) %>% 
  ggplot(aes(reorder(breed, percent_na), percent_na)) +
  geom_point() +
  coord_flip() + xlab('breed') + ylab('percent missing')

# 2a
DogsofNYC$birth <- DogsofNYC$birth %>%
  lubridate::parse_date_time2(orders = 'my')
DogsofNYC$birth[DogsofNYC$birth > '2018-12-01 UTC'] <- NA
DogsofNYC %>% 
  mutate(yearmon = zoo::as.yearmon(birth)) %>% 
  group_by(yearmon) %>% 
  tally() %>% 
  ggplot(aes(yearmon, n)) + geom_bar(stat = 'identity')
DogsofNYC %>% 
  mutate(yearmon = zoo::as.yearmon(birth)) %>% 
  group_by(yearmon) %>% 
  tally() %>%
  arrange(desc(n)) %>% head(10)

# 2b
DogsofNYC %>% 
  mutate(year = as.numeric(substr(birth, 0, 4))) %>% 
  ggplot(aes(year)) +
  geom_histogram(binwidth = 1) +
  xlim(c(1988, 2013))

# 3a
library(vcd)
library(grid)
NYCdogs %>%
  group_by(dominant_color) %>% 
  count() %>% arrange(desc(n)) %>% head(5) %>% 
  select(dominant_color)
top5 <- c('BLACK', 'WHITE', 'BROWN', 'TAN', 'BLOND')
color <- function (x) {
  ifelse(x %in% top5,
         x,
         'OTHER')
}
NYCdogs$color <- lapply(NYCdogs$dominant_color, color)
NYCdogs <- transform(NYCdogs, color=unlist(color))
dfcounts <- NYCdogs %>% group_by(color, Group) %>% 
  count()
order <- c(top5, 'OTHER')
orderGroup <- c('Mutt', 'Toy', 'Non-Sporting',
                'Terrier', 'Sporting', 'Working',
                'Hound', 'Herding')
NYCdogs$color <- factor(NYCdogs$color,
                         levels=order)
NYCdogs$Group <- factor(NYCdogs$Group,
                        levels=orderGroup)
color1 = c('grey', 'white', 'brown', 'tan', 'yellow', 'blue')
vcd::mosaic(color~Group,
            direction=c('v', 'h'),
            NYCdogs,
            gp = gpar(fill=color1),
            rot_labels=c(45, 45),
            sort=order)

# 3b
NYCdogs1 <- NYCdogs %>% 
  filter(color != 'OTHER')
color2 = c('grey', 'white', 'brown', 'tan', 'yellow')
NYCdogs1$color <- factor(NYCdogs1$color,
                        levels=top5)
vcd::mosaic(color~Group,
            direction=c('v', 'h'),
            NYCdogs1,
            gp = gpar(fill=color2),
            rot_labels=c(45, 45),
            sort=top5)

# 4
zips <- NYCdogs %>% group_by(zip_code) %>% 
  count() %>% arrange(zip_code)
NYCdogs$spaybin <- ifelse(NYCdogs$spayed_or_neutered == 'Yes', 1, 0)
dfspay <- NYCdogs %>% group_by(zip_code) %>% 
  summarise(n = n(),
            num_spay = sum(spaybin)) %>% 
  mutate(percent_spay = round(num_spay / n, 2))
colnames(dfspay)[1] <- 'region'
colnames(dfspay)[4] <- 'value'
dfzip <- dfspay %>% select('region', 'value')
dfzip$region <- dfzip$region %>% as.character()
nyc_fips = c(36085,36005, 36047, 36061, 36081)
data("zip.regions")
nyc_zips<-data.frame(county.fips.numeric=nyc_fips)%>%inner_join(zip.regions)%>%select(region)%>%t
zip_choropleth(dfzip, zip_zoom=nyc_zips,
               title='Share of Spaying/Neutering',
               legend='share')

# 5a
ul <- tq_get('UL', get='stock.prices') #Unilever
gis <- tq_get('GIS', get='stock.prices') #General Mills
ns <- tq_get('NSRGY', get='stock.prices') #Nestle
brk <- tq_get('BRK', get='stock.prices') #Berkshire Hathaway
cols <- c('UL' = 'blue', 'GIS' = 'red',
          'NSRGY' = 'black', 'BRK' = 'purple')

df_ul <- ul %>% select(date, close)
colnames(df_ul)[2] <- 'UL'
df_gis <- gis %>% select(close)
colnames(df_gis)[1] <- 'GIS'
df_ns <- ns %>% select(close)
colnames(df_ns)[1] <- 'NSRGY'
df_brk <- brk %>% select(close)
colnames(df_brk)[1] <- 'BRK'
df_tot <- data.frame(df_ul, df_gis, df_ns, df_brk)
df_tot <- df_tot %>% gather(stock, price, -date)
df_tot %>% ggplot(aes(x=date, y=price, color=stock)) +
  geom_line() +
  scale_color_manual(values=cols) +
  ggtitle('Ice cream corporations')

# 5b
df_ul <- df_ul %>%
  mutate(UL_norm = UL / df_ul$UL[1] * 100) %>% 
  select(date, UL_norm)
df_gis <- df_gis %>% 
  mutate(GIS_norm = GIS / df_gis$GIS[1] * 100) %>% 
  select(GIS_norm)
df_ns <- df_ns %>% 
  mutate(NSRGY_norm = NSRGY / df_ns$NSRGY[1] * 100) %>% 
  select(NSRGY_norm)
df_brk <- df_brk %>% 
  mutate(BRK_norm = BRK / df_brk$BRK[1] * 100) %>% 
  select(BRK_norm)
df_tot <- data.frame(df_ul, df_gis, df_ns, df_brk)
df_tot <- df_tot %>% gather(stock, price, -date)
cols <- c('UL_norm' = 'blue', 'GIS_norm' = 'red',
          'NSRGY_norm' = 'black', 'BRK_norm' = 'purple')
df_tot %>% ggplot(aes(x=date, y=price, color=stock)) +
  geom_line() +
  scale_color_manual(values=cols) +
  ggtitle('Initial stock price set to index 100')

# 6
## Guard dogs Proportion
NYCdogs$guard[NYCdogs$guard_or_trained == 'No'] <- 0
NYCdogs$guard[NYCdogs$guard_or_trained == 'Yes'] <- 1
NYCdogs$guard <- NYCdogs$guard %>% as.numeric()
dfguard <- NYCdogs %>% group_by(zip_code) %>% 
  summarise(n = n(),
            num_guard = sum(guard)) %>% 
  mutate(percent_guard = round(num_guard / n, 4))
colnames(dfguard)[1] <- 'region'
colnames(dfguard)[4] <- 'value'
dfzipguard <- dfguard %>% select('region', 'value')
dfzipguard$region <- dfzipguard$region %>% as.character()
nyc_fips = c(36085,36005, 36047, 36061, 36081)
data("zip.regions")
nyc_zips<-data.frame(county.fips.numeric=nyc_fips)%>%inner_join(zip.regions)%>%select(region)%>%t
zip_choropleth(dfzipguard, zip_zoom=nyc_zips,
               title='Proportion of Guard Dogs',
               legend='Proportion')

## Number
dfguard <- NYCdogs %>% group_by(zip_code) %>% 
  summarise(n = n(),
            num_guard = sum(guard)) %>% 
  mutate(percent_guard = round(num_guard / n, 4))
colnames(dfguard)[1] <- 'region'
colnames(dfguard)[3] <- 'value'
dfzipguard <- dfguard %>% select('region', 'value')
dfzipguard$region <- dfzipguard$region %>% as.character()
nyc_fips = c(36085,36005, 36047, 36061, 36081)
data("zip.regions")
nyc_zips<-data.frame(county.fips.numeric=nyc_fips)%>%inner_join(zip.regions)%>%select(region)%>%t
zip_choropleth(dfzipguard, zip_zoom=nyc_zips,
               title='Guard Dogs',
               legend='Count')

# Correlation between black and guard, mosaic
# Proportion of guard dogs is too small
NYCdogs %>%
  group_by(dominant_color) %>% 
  count() %>% arrange(desc(n)) %>% head(5) %>% 
  select(dominant_color)
top5 <- c('BLACK', 'WHITE', 'BROWN', 'TAN', 'BLOND')
color <- function (x) {
  ifelse(x %in% top5,
         x,
         'OTHER')
}
NYCdogs$color <- lapply(NYCdogs$dominant_color, color)
NYCdogs <- transform(NYCdogs, color=unlist(color1))
dfcounts <- NYCdogs %>% group_by(color, Group) %>% 
  count()
order <- c(top5, 'OTHER')
NYCdogs$color <- factor(NYCdogs$color,
                        levels=order)
NYCdogs$guard_or_trained <- factor(NYCdogs$guard_or_trained)
vcd::mosaic(guard_or_trained~color,
            direction=c('v', 'h'),
            NYCdogs,
            rot_labels=c(45, 45),
            sort=order)

# Guard dog genders
NYCdogs %>% group_by(gender) %>% 
  summarise(n = n(),
            num_guard = sum(guard),
            percent_guard = num_guard / n)

# Gender ratio by zip code
## Filled NA with 0
## Fix bins
NYCdogs$male[NYCdogs$gender == 'M'] <- 1
NYCdogs$male[NYCdogs$gender == 'F'] <- 0
NYCdogs$male[is.na(NYCdogs$gender)] <- 0
NYCdogs$male <- NYCdogs$male %>% as.numeric()
dfmale <- NYCdogs %>% group_by(zip_code) %>% 
  summarise(n = n(),
            num_male = sum(male)) %>% 
  mutate(percent_male = round(num_male / n, 4))
colnames(dfmale)[1] <- 'region'
colnames(dfmale)[4] <- 'value'
dfzipmale <- dfmale %>% select('region', 'value')
dfzipmale$region <- dfzipmale$region %>% as.character()
nyc_fips = c(36085,36005, 36047, 36061, 36081)
data("zip.regions")
nyc_zips<-data.frame(county.fips.numeric=nyc_fips)%>%inner_join(zip.regions)%>%select(region)%>%t
dfzipmale$valuecat[dfzipmale$value < .425] <- 'Less than .425'
dfzipmale$valuecat[dfzipmale$value >= .425 & dfzipmale$value < .45] <- '[.425, .45)'
dfzipmale$valuecat[dfzipmale$value >= .45 & dfzipmale$value < .475] <- '[.45, .475)'
dfzipmale$valuecat[dfzipmale$value >= .475 & dfzipmale$value < .5] <- '[.475, .5)'
dfzipmale$valuecat[dfzipmale$value >= .5 & dfzipmale$value < .525] <- '[.5, .525)'
dfzipmale$valuecat[dfzipmale$value >= .525 & dfzipmale$value < .55] <- '[.525, .55)'
dfzipmale$valuecat[dfzipmale$value >= .55 & dfzipmale$value < .575] <- '[.55, .575)'
dfzipmale$valuecat[dfzipmale$value >= .575 & dfzipmale$value < .6] <- '[.575, .6)'
dfzipmale$valuecat[dfzipmale$value >= .6] <- 'Greater than .6'
dfzipM <- dfzipmale %>% select(region, valuecat)
colnames(dfzipM)[2] <- 'value'
order <- c('Less than .425',
           '[.425, .45)', '[.45, .475)',
           '[.475, .5)', '[.5, .525)',
           '[.525, .55)', '[.55, .575)',
           '[.575, .6)', 'Greater than .6')
dfzipM$value <- factor(dfzipM$value,
                        levels=order)
choro <- zip_choropleth(dfzipM, zip_zoom=nyc_zips,
               title='Proportion of Male Dogs',
               legend='Proportion',
               num_colors = 9)
choro <- ZipChoropleth$new(dfzipM)
choro$title <- 'Proportion of Male Dogs by Zip Code'
choro$set_zoom_zip(state_zoom=NULL,
                   zip_zoom=nyc_zips,
                   county_zoom=NULL,
                   msa_zoom=NULL)
scale <- c('#ffffe5', '#fff7bc',
           '#fee391', '#fec44f',
           '#fe9929', '#ec7014',
           '#cc4c02', '#8c2d04')
choro$ggplot_scale = scale_fill_manual(name='Proportion Male',
                                       values=scale)
choro$render()
# Gender ratio for colors?