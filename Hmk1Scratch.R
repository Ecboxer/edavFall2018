ggplot(Employee_Compensation,
       mapping = aes(x = Year, y = Salaries, group = Year)) +
  geom_boxplot() +
  coord_flip()

Employee_Compensation %>%
  group_by(Year) %>% 
  summarise(mean = mean(Salaries),
    iqr = IQR(Salaries),
    range = max(Salaries) - min(Salaries),
    high_earners = sum(Salaries > 200000),
    perc_high_earners = high_earners / n())

Employee_Compensation %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  mutate(perc_change = n / lag(n))

ggplot(Employee_Compensation, mapping = aes(x = Salaries)) +
  geom_histogram() +
  facet_wrap(~ Year, nrow = 1)

ggplot(Employee_Compensation, mapping = aes(x = Salaries)) +
  geom_histogram(binwidth = 10000) +
  coord_cartesian(xlim = c(0, 250000)) +
  facet_wrap(~ Year, nrow = 1)

df_Emp_Comp <- data.frame(Employee_Compensation$Salaries)
df_Emp_Comp$Year <- factor(x = Employee_Compensation$Year,
                           levels = c(2013, 2014, 2015, 2016, 2017))

ggplot(df_Emp_Comp,
       mapping = aes(Employee_Compensation.Salaries,
                     group = Year, color = Year)) +
  geom_density() +
  xlab('Salaries')

Employee_Compensation %>% 
  ggplot(mapping = aes(x = reorder(Organization_Group, Overtime, FUN = median),
                       y = Overtime)) +
  geom_boxplot() +
  xlab('Organization Group') +
  coord_flip()

Employee_Compensation %>% 
  group_by(Organization_Group) %>%
  summarise(n = n())

unique(Employee_Compensation$`Organization Group`)

Employee_Compensation %>% 
  group_by(Year) %>% 
  summarise(n = n())

#Overtime grouped by Department and Org Group
Employee_Compensation %>% 
  group_by(Organization_Group, Department) %>% 
  ggplot(mapping = aes(x = reorder(Department, Overtime, FUN = median),
                       y = Overtime,
                       color = Organization_Group)) +
  geom_boxplot() +
  xlab('Department') +
  coord_flip()
#Negative Overtime?
Employee_Compensation %>% 
  filter(Overtime < 0) %>% 
  count()
Employee_Compensation %>% 
  filter(Overtime < -500) %>% 
  select(Organization_Group, Department, Overtime)

df_Overtime <- Employee_Compensation %>%
  group_by(Job_Family) %>% 
  summarise(med_Overtime = median(Overtime)) %>% 
  arrange(desc(med_Overtime))
top_Overtime_Jobs <- df_Overtime$Job_Family[1:10]
Employee_Compensation %>% 
  group_by(Organization_Group, Job_Family) %>%
  filter(Job_Family %in% top_Overtime_Jobs) %>% 
  ggplot(mapping = aes(x = reorder(Job_Family, Overtime, FUN = median),
                       y = Overtime,
                       color = Organization_Group)) +
  geom_boxplot() +
  xlab('Job Family') +
  coord_flip()

Employee_Compensation %>% 
  ggplot(mapping = aes(x = Salaries,
                       y = Overtime,
                       color = Organization_Group)) +
  geom_point(alpha = .1) +
  coord_cartesian(xlim = c(-1000, 400000),
                  ylim = c(-1000, 150000))

#Boundaries - Part 3
happiness %>% 
  filter(Gender == 'Both') %>% 
  print(n = 35)
happiness %>% 
  filter(Gender == 'Both') %>% 
  ggplot(mapping = aes(x = Mean)) +
  geom_histogram(center = .05, binwidth = .1, closed = 'left') +
  ggtitle('Right Open')
happiness %>% 
  filter(Gender == 'Both') %>% 
  ggplot(mapping = aes(x = Mean)) +
  geom_histogram(center = .05, binwidth = .1, closed = 'right') +
  ggtitle('Right Closed')
happiness %>% 
  filter(Gender == 'Both') %>% 
  ggplot(mapping = aes(x = Mean)) +
  geom_histogram(binwidth = .1, closed = 'left') +
  ggtitle('Right Open')
happiness %>% 
  filter(Gender == 'Both') %>% 
  ggplot(mapping = aes(x = Mean)) +
  geom_histogram(binwidth = .1, closed = 'right') +
  ggtitle('Right Closed')

#Doctors - Part 5
breslow %>% 
  filter(smoke == 0) %>% 
  ggplot(mapping = aes(x = age, y = y)) +
  geom_bar(width = 1, stat = 'identity') +
  ylab('deaths') +
  ggtitle('Non-smokers')

breslow %>% 
  filter(smoke == 1) %>% 
  ggplot(mapping = aes(x = age, y = y)) +
  geom_bar(width = 1, stat = 'identity') +
  ylab('deaths') +
  ggtitle('Smokers')

#Beavers - Part 4
ggplot(beaver1, aes(sample = temp)) +
  stat_qq() +
  stat_qq_line()

ggplot(beaver2, aes(sample = temp)) +
  stat_qq() +
  stat_qq_line()

ggplot(beaver1) +
  geom_histogram(mapping = aes(temp)) +
  geom_density(mapping = aes(temp)) +
  stat_function(fun = dnorm,
                color = 'red',
                args = list(mean = mean(beaver1$temp),
                            sd = sd(beaver1$temp)))

ggplot(beaver2) +
  geom_histogram(mapping = aes(temp)) +
  geom_density(mapping = aes(temp)) +
  stat_function(fun = dnorm,
                color = 'red',
                args = list(mean = mean(beaver2$temp),
                            sd = sd(beaver2$temp)))

#Shapiro-Wilk Test
shapiro.test(beaver1$temp)

shapiro.test(beaver2$temp)
