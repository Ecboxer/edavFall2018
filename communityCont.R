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
library(vegalite)
View(Loblolly)
