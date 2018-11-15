library(tidyverse)

# 1 Flowers
library(cluster)
colnames(flower)
colnames(flower) <- c('winters', 'shadow', 'tubers', 'color',
                      'soil', 'preference', 'height', 'distance')
flower$winters <- as.character(flower$winters)
flower$winters[flower$winters == 0] <- 'no'
flower$winters[flower$winters == 1] <- 'yes'
flower$shadow <- as.character(flower$shadow)
flower$shadow[flower$shadow == 0] <- 'no'
flower$shadow[flower$shadow == 1] <- 'yes'
flower$tubers <- as.character(flower$tubers)
flower$tubers[flower$tubers == 0] <- 'no'
flower$tubers[flower$tubers == 1] <- 'yes'
flower$color <- as.character(flower$color)
flower$color <- recode(flower$color, '1' = 'white', '2' = 'yellow', '3' = 'pink', '4' = 'red', '5' = 'blue')
flower$soil <- as.character(flower$soil)
flower$soil <- recode(flower$soil, '1' = 'dry', '2' = 'normal', '3' = 'wet')
flower

flower$color <- as.factor(flower$color)
flower %>%
  ggplot(aes(x = reorder(color, color, function(x)-length(x)))) + 
  geom_bar(aes(fill = color), color = '#000000') +
  scale_fill_manual(values = c('#0000FF', '#FF00FF', '#FF0000', '#FFFFFF', '#FFFF00')) +
  xlab('color') +
  ggtitle('flower color')

flower$soil <- as.factor(flower$soil)
flower %>% 
  ggplot(aes(x = reorder(soil, soil, function(x)-length(x)))) +
  geom_bar(aes(fill = soil)) +
  scale_fill_brewer() +
  theme_dark() +
  xlab('soil') +
  ggtitle('flower soil saturation')

# 2 Minneapolis
library(carData)
View(MplsDemo)
MplsDemo %>% 
  ggplot(aes(x = reorder(neighborhood, hhIncome), y = hhIncome)) +
  geom_point(color = 'blue') +
  coord_flip() +
  ggtitle('Median household income') +
  xlab('neightborhood') +
  theme_bw() +
  theme(axis.text.y = element_text(size = rel(.75)),
        panel.grid.major.y = element_line(size = 1.0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

MplsDemo %>% 
  mutate(foreignBornP = foreignBorn * 100,
         povertyP = poverty * 100,
         collegeGradP = collegeGrad * 100) %>% 
  ggplot() +
  geom_point(aes(x = foreignBornP, 
                 y = reorder(neighborhood, collegeGradP),
                 color = 'Foreign Born'), alpha = .75) +
  geom_point(aes(x = povertyP,
                 y = reorder(neighborhood, collegeGradP),
                 color = 'Poverty'), alpha = .75) +
  geom_point(aes(x = collegeGradP,
                 y = reorder(neighborhood, collegeGradP), 
                 color = 'College Graduation'), alpha = .75) +
  ylab('neighborhood') +
  xlab('percentage of population') +
  ggtitle('Minneapolis demographics') +
  theme_dark() +
  theme(axis.text.y = element_text(size = rel(.75))) +
  scale_color_manual(values = c('#fc8d59',
                                '#ffffbf',
                                '#91bfdb'))
# Ordered by foreignBorn
MplsDemo %>% 
  mutate(foreignBornP = foreignBorn * 100,
         povertyP = poverty * 100,
         collegeGradP = collegeGrad * 100) %>% 
  ggplot() +
  geom_point(aes(x = foreignBornP, 
                 y = reorder(neighborhood, foreignBornP),
                 color = 'Foreign Born'), alpha = .75) +
  geom_point(aes(x = povertyP,
                 y = reorder(neighborhood, foreignBornP),
                 color = 'Poverty'), alpha = .75) +
  geom_point(aes(x = collegeGradP,
                 y = reorder(neighborhood, foreignBornP), 
                 color = 'College Graduation'), alpha = .75) +
  ylab('neighborhood') +
  xlab('percentage of population') +
  ggtitle('Minneapolis demographics (by foreignBorn)') +
  theme_dark() +
  theme(axis.text.y = element_text(size = rel(.75))) +
  scale_color_manual(values = c('#fc8d59',
                                '#ffffbf',
                                '#91bfdb'))
# Ordered by poverty
MplsDemo %>% 
  mutate(foreignBornP = foreignBorn * 100,
         povertyP = poverty * 100,
         collegeGradP = collegeGrad * 100) %>% 
  ggplot() +
  geom_point(aes(x = foreignBornP, 
                 y = reorder(neighborhood, povertyP),
                 color = 'Foreign Born'), alpha = .75) +
  geom_point(aes(x = povertyP,
                 y = reorder(neighborhood, povertyP),
                 color = 'Poverty'), alpha = .75) +
  geom_point(aes(x = collegeGradP,
                 y = reorder(neighborhood, povertyP), 
                 color = 'College Graduation'), alpha = .75) +
  ylab('neighborhood') +
  xlab('percentage of population') +
  ggtitle('Minneapolis demographics (by poverty)') +
  theme_dark() +
  theme(axis.text.y = element_text(size = rel(.75))) +
  scale_color_manual(values = c('#fc8d59',
                                '#ffffbf',
                                '#91bfdb'))

# Statistics
library(modelr)
mod1 <- lm(collegeGrad~foreignBorn, data = MplsDemo)
coef(mod1)
mod2 <- lm(collegeGrad~poverty, data = MplsDemo)
coef(mod2)

# 3 Taxis
colnames(yellow_subset) <- c('id', 'pickup', 'dropoff',
                             'passenger', 'dist', 'pu', 'do',
                             'ratecode', 'store', 'payment',
                             'fare_amount', 'extra', 'mta',
                             'imp', 'tip_amount', 'tolls', 'total')
# a
yellow_subset %>%
  ggplot(mapping = aes(tip_amount, fare_amount)) +
  geom_point(alpha = .25) +
  xlab('Tip') + ylab('Fare') +
  ggtitle('NYC Taxi Cabs, June 2018')

# b
yellow_subset %>%
  ggplot(mapping = aes(tip_amount, fare_amount)) +
  geom_point(alpha = .25) +
  geom_density_2d(color = 'blue') +
  xlab('Tip') + ylab('Fare') +
  ggtitle('NYC Taxi Cabs, June 2018') +
  xlim(c(6, 20))

# c
yellow_subset %>%
  ggplot(mapping = aes(tip_amount, fare_amount)) +
  geom_hex(bins = 25) +
  xlab('Tip') + ylab('Fare') +
  ggtitle('NYC Taxi Cabs, June 2018') +
  xlim(c(-1, 13)) + ylim(c(0, 50)) +
  scale_fill_gradient(low = '#a1d99b', high = '#00441b')

# d
yellow_subset %>%
  ggplot(mapping = aes(tip_amount, fare_amount)) +
  geom_bin2d(bins = 20) +
  xlab('Tip') + ylab('Fare') +
  ggtitle('NYC Taxi Cabs, June 2018') +
  xlim(c(-1, 15)) + ylim(c(0, 60)) +
  scale_fill_gradient(low = '#a1d99b', high = '#00441b')

# 4 Olive Oil
library(extracat)
pairs(olives[,3:10], pch=21)

olives %>% 
  group_by(Region) %>%
  summarise(n())
my_cols <- c('#66c2a5', '#fc8d62', '#8da0cb')
pairs(olives[,3:10], pch=21, col = my_cols[olives$Region],
      lower.panel = NULL)
legend("bottomleft",
       fill = unique(my_cols[olives$Region]),
       legend = c(levels(olives$Region)))

# 5 Wine
library(pgmm)
data(wine)
wine$Type <- recode(wine$Type,
                    '1' = 'Barolo',
                    '2' = 'Grignolino',
                    '3' = 'Barbera')
library(plotly)
p <- wine %>% 
  plot_ly(type = 'parcoords',
             line = list(color = ~ Type),
             dimensions = list(
               list(range = c(min(wine$Alcohol), max(wine$Alcohol)),
                    label = 'Alchohol',
                    values = ~ Alcohol),
               list(range = c(min(wine$`Sugar-free Extract`), max(wine$`Sugar-free Extract`)),
                    label = 'Sugar-free Extract',
                    values = ~`Sugar-free Extract`),
               list(range = c(min(wine$`Fixed Acidity`), max(wine$`Fixed Acidity`)),
                    label = 'Fixed Acidity',
                    values = ~`Fixed Acidity`),
               list(range = c(min(wine$`Tartaric Acid`), max(wine$`Tartaric Acid`)),
                    label = 'Tartaric Acid',
                    values = ~`Tartaric Acid`),
               list(range = c(min(wine$`Malic Acid`), max(wine$`Malic Acid`)),
                    label = 'Malic Acid',
                    values = ~`Malic Acid`),
               list(range = c(min(wine$`Uronic Acids`), max(wine$`Uronic Acids`)),
                    label = 'Uronic Acids',
                    values = ~`Uronic Acids`),
               list(range = c(min(wine$pH), max(wine$pH)),
                    label = 'pH',
                    values = ~pH),
               list(range = c(min(wine$Ash), max(wine$Ash)),
                    label = 'Ash',
                    values = ~Ash),
               list(range = c(min(wine$`Alcalinity of Ash`), max(wine$`Alcalinity of Ash`)),
                    label = 'Alcalinity of Ash',
                    values = ~`Alcalinity of Ash`),
               list(range = c(min(wine$Potassium), max(wine$Potassium)),
                    label = 'Potassium',
                    values = ~Potassium),
               list(range = c(min(wine$Calcium), max(wine$Calcium)),
                    label = 'Calcium',
                    values = ~Calcium),
               list(range = c(min(wine$Magnesium), max(wine$Magnesium)),
                    label = 'Magnesium',
                    values = ~Magnesium),
               list(range = c(min(wine$Phosphate), max(wine$Phosphate)),
                    label = 'Phosphate',
                    values = ~Phosphate),
               list(range = c(min(wine$Chloride), max(wine$Chloride)),
                    label = 'Chloride',
                    values = ~Chloride),
               list(range = c(min(wine$`Total Phenols`), max(wine$`Total Phenols`)),
                    label = 'Total Phenols',
                    values = ~`Total Phenols`),
               list(range = c(min(wine$Flavanoids), max(wine$Flavanoids)),
                    label = 'Flavanoids',
                    values = ~Flavanoids),
               list(range = c(min(wine$`Non-flavanoid Phenols`), max(wine$`Non-flavanoid Phenols`)),
                    label = 'Non-flavanoid Phenols',
                    values = ~`Non-flavanoid Phenols`),
               list(range = c(min(wine$`Color Intensity`), max(wine$`Color Intensity`)),
                    label = 'Color Intensity',
                    values = ~`Color Intensity`),
               list(range = c(min(wine$Hue), max(wine$Hue)),
                    label = 'Hue',
                    values = ~ Hue),
               list(range = c(min(wine$Glycerol), max(wine$Glycerol)),
                    label = 'Glycerol',
                    values = ~Glycerol),
               list(range = c(min(wine$`Total Nitrogen`), max(wine$`Total Nitrogen`)),
                    label = 'Total Nitrogen',
                    values = ~`Total Nitrogen`),
               list(range = c(min(wine$Methanol), max(wine$Methanol)),
                    label = 'Methanol',
                    values = ~ Methanol)
               )
             )
chart_link = api_create(p, filename = 'parcoords-basic')
chart_link

wine %>% 
  filter(`Uronic Acids` > 1.7)
wine %>% 
  filter(`Total Nitrogen` > 500)
