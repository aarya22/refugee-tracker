refugee.data <- read.csv('data/asylum_seekers.csv', stringsAsFactors = FALSE) 


demographics <- read.csv('data/demographics.csv', stringsAsFactors = FALSE) %>% 
  select(Year, Country...territory.of.asylum.residence,
         F..Total, M..Total) 

#totpending start + total end
# un end + un start
refugee.data[,4:14] <- sapply(refugee.data[,4:14], as.numeric)
asylum.in <- mutate(refugee.data, people.in = Tota.pending.start.year,
                    un.helped = of.which.UNHCR.assisted.start.year.) %>% 
  select(Year, Country...territory.of.asylum.residence, people.in, un.helped)
colnames(asylum.in) <- c("year", "country", "people.in", "un.helped")

asylum.in.grouped <- aggregate(. ~ year+country, data = asylum.in, sum) %>% 
  mutate(un.help.percent = (un.helped / people.in) * 100) 
asylum.in.grouped <- replace(asylum.in.grouped, is.na(asylum.in.grouped), 0)

asylum.out <- select(refugee.data, Year, Origin, Tota.pending.start.year, of.which.UNHCR.assisted.start.year.)
colnames(asylum.out) <- c("year", "origin", "people.out", "un.helped")

asylum.out.grouped <- aggregate(. ~ year+origin, data = asylum.out, sum)  %>% 
  mutate(un.help.percent = (un.helped / people.out) * 100) 




