#### using random names to make a small fake voter file

### run walksheets script to line 37

require(dplyr)
require(randomNames)
library(tidyr)
require(readr)
path <- "~/Desktop/RepYourBlock_old/dir_to_ignore/"
nyvoter <- paste0(path,'Kings_20200127.txt')
nyvoter <- read.table(nyvoter,
                      sep=",",
                      fill=TRUE,
                      row.names = NULL)

fake <- nyvoter %>% 
  filter(V29 == 44 | V29 == 56 | V29 == 57) %>% 
  select(V3:V45)


random_names <- as.data.frame(randomNames(380740)) %>% 
  rename(name = `randomNames(380740)`)
new_names <- separate(data = random_names, col = name, into = c("V1", "V2"), sep = ",")

fake_voterfile <- new_names %>% 
  cbind(fake)

write_csv(fake_voterfile, "~/Desktop/ryb/workshop/data/workshop_voterfile.csv")
write_csv(fake_voterfile, "~/Desktop/SOD_workshop/workshop_voterfile.csv")
