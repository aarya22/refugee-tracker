refugee.data <- read.csv('data/asylum_seekers.csv', stringsAsFactors = FALSE) 

#turn dataframe from character values into numerical values
refugee.data[,4:14] <- sapply(refugee.data[,4:14], as.numeric)

#select necessary data to calculate number of refugees coming in to country
asylum.in <- select(refugee.data, Year, Country...territory.of.asylum.residence,
         Tota.pending.start.year, of.which.UNHCR.assisted.start.year.)
#change column names to be more readable
colnames(asylum.in) <- c("year", "country", "people.in", "un.helped")

#obtain number of asylum seekers for each country grouped by year and country
asylum.in.grouped <- aggregate(. ~ year+country, data = asylum.in, sum) %>% 
  mutate(un.help.percent = (un.helped / people.in) * 100) 
asylum.in.grouped <- replace(asylum.in.grouped, is.na(asylum.in.grouped), 0)

#select necessary data to calculate number of asylum seekers leaving country
asylum.out <- select(refugee.data, Year, Origin, Tota.pending.start.year,
                     of.which.UNHCR.assisted.start.year.)
#change column names to be more readable
colnames(asylum.out) <- c("year", "origin", "people.out", "un.helped")

#obtain number of asylum seekers leaving each country grouped by year & country
asylum.out.grouped <- aggregate(. ~ year+origin, data = asylum.out, sum)  %>% 
  #calculate percentage of asylum seekers that UN helped
  mutate(un.help.percent = (un.helped / people.out) * 100) 




