#Multi Bar Chart of Age against Height, with subsetting by Seed
n1 <- nPlot(height ~ age,
            group = 'Seed',
            data = Loblolly,
            type = 'multiBarChart')
n1

#Multi Bar Chart of See against Height, subset by Age
n2 <- nPlot(height ~ Seed,
            group = 'age',
            data = Loblolly,
            type = 'multiBarChart')
n2

#Leaflet of quakes
map1 <- Leaflet$new()
map1$tileLayer()
map1$setView(c(mean(quakes$lat), mean(quakes$long)), zoom = 13)
map1
