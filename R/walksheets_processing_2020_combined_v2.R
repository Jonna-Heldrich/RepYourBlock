# DONE 1. place addresses in order by street number and managing difficulties like alphanumeric addresses (eg, 344A Classen Ave)
# DONE 2. place apartments in order within buildings and managing difference like ordering non-standardized apartment numbers (eg, Apt 1B, 2C, #10K)
# DONE 3. splitting street addresses into even and odd groups, corresponding to different sides of the street
# DONE 4. transforming an MMDDYYYY birthdate field into an “age” field
# DONE BUT SLOW 5. producing a “voter score” identifying more likely petition signers based on the “voter history” field
# AVALAILABLE 6. "building" and "apt" fields for app
# DONE 7. give each voter an ID--is this distinct for each ED?

# cleaning the data: find most frequent street names
# look at "openxls" for create checkboxes
require(dplyr)
require(openxlsx)
library(stringr)

path <- paste0('~/Desktop/NKD_walksheets/')
nyvoter <- paste0(path,'Kings_20191114.txt')
nyvoter <- read.table(nyvoter, 
                     sep=",",
                     fill=TRUE,
                     row.names = NULL)

voterheader <- paste0(path,'vf headers.xlsx')
voterheader <- read.xlsx(voterheader)
colnames(voterheader) <- gsub('[0-9]{1,2}[.]| [[a-z][0-9]]', '', colnames(voterheader))
colnames(voterheader) <- gsub('[\"]', '', colnames(voterheader))
nyvoter2 <- nyvoter
names(nyvoter2) = names(voterheader)
nyvoter2 <- nyvoter2[grep("klyn|kltyn|kkyn|klym|olyn|kyln|kllyn|kl;yn|kln|kly|klyb|okyn|klybn|klyln|112",
                          nyvoter2$addcity,ignore.case=TRUE),]

dems <- nyvoter2 %>%
  select(ID, lastname, firstname, addnumber, addfract, addpredirect, addstreet, 
         addpostdirect, addapt, addcity, DOB, gender, party,
         ED, AD, LegDist, ward, CD, SD, status,
         votehistory) %>%
  filter(status=="ACTIVE") %>%
  filter(party=="DEM") #%>% 
  #mutate(AD = as.numeric(str_pad(AD, width = 2, pad = "0")),
         ED = as.numeric(str_pad(ED, width = 3, pad = "0")),
         ad_ed = paste(AD, ED, sep = "-"))

###########################################################
############ Start to clean street names ##################
###########################################################

cleaned_dems <- dems %>% 
  mutate(clean_addstreet = trimws(gsub("\\s+", " ", addstreet)),
         clean_addstreet = gsub("`", "", clean_addstreet),
         clean_addstreet = gsub("HIMROAD", "HIMROD", clean_addstreet),
         clean_addstreet = gsub("HIMROS", "HIMROD", clean_addstreet),
         clean_addstreet = gsub("104 STREET", "EAST 104 STREET", clean_addstreet),
         clean_addstreet = gsub("STREEET", "STREET", clean_addstreet),
         clean_addstreet = gsub("SREET", "STREET", clean_addstreet),
         clean_addstreet = gsub("STRET", "STREET", clean_addstreet),
         clean_addstreet = gsub("STEEGT", "STREET", clean_addstreet),
         clean_addstreet = gsub("STREETE", "STREET", clean_addstreet),
         clean_addstreet = gsub("STREET1", "STREET", clean_addstreet),
         clean_addstreet = gsub("STREER", "STREET", clean_addstreet),
         clean_addstreet = gsub("STREETT", "STREET", clean_addstreet),
         clean_addstreet = gsub("STRETT", "STREET", clean_addstreet),
         clean_addstreet = gsub("SRETT", "STREET", clean_addstreet),
         clean_addstreet = gsub("SSTRETT", "STREET", clean_addstreet), 
         clean_addstreet = gsub("STET", "STREET", clean_addstreet),
         clean_addstreet = gsub("STERET", "STREET", clean_addstreet),
         clean_addstreet = gsub("STRRETT", "STREET", clean_addstreet),
         clean_addstreet = gsub("STRREETT", "STREET", clean_addstreet),
         clean_addstreet = gsub("STTEET", "STREET", clean_addstreet),
         clean_addstreet = gsub("STERT", "STREET", clean_addstreet), 
         clean_addstreet = gsub("STEE", "STREET", clean_addstreet),
         clean_addstreet = gsub("STERET", "STREET", clean_addstreet),
         clean_addstreet = gsub("ATREET", "STREET", clean_addstreet),
         clean_addstreet = gsub("DTREET", "STREET", clean_addstreet),
         clean_addstreet = gsub(" TREET", " STREET", clean_addstreet),
         clean_addstreet = gsub("ST3EET", "STREET", clean_addstreet),
         clean_addstreet = gsub("SEREET", "STREET", clean_addstreet),
         clean_addstreet = gsub("SST", "ST", clean_addstreet),
         clean_addstreet = gsub("STREET", "ST", clean_addstreet),
         clean_addstreet = gsub("FIRST", "1", clean_addstreet),
         clean_addstreet = gsub("SECOND", "2", clean_addstreet),
         clean_addstreet = gsub("THIRD", "3", clean_addstreet),
         clean_addstreet = gsub("FOURTH", "4", clean_addstreet),
         clean_addstreet = gsub("FIFTH", "5", clean_addstreet),
         clean_addstreet = gsub("SIXTH", "6", clean_addstreet),
         clean_addstreet = gsub("SEVENTH", "7", clean_addstreet),
         clean_addstreet = gsub("EIGHTH", "8", clean_addstreet),
         clean_addstreet = gsub("NINTH", "9", clean_addstreet),
         clean_addstreet = gsub("TENTH", "10", clean_addstreet),
         clean_addstreet = gsub("0TH", "0", clean_addstreet),
         clean_addstreet = gsub("1ST", "1", clean_addstreet),
         clean_addstreet = gsub("1STST", "1 ST", clean_addstreet),
         clean_addstreet = gsub("1TH", "1", clean_addstreet),
         clean_addstreet = gsub("2TH", "2", clean_addstreet),
         clean_addstreet = gsub("2ND", "2", clean_addstreet),
         clean_addstreet = gsub("3RD", "3", clean_addstreet),
         clean_addstreet = gsub("3TH", "3", clean_addstreet),
         clean_addstreet = gsub("4TH", "4", clean_addstreet),
         clean_addstreet = gsub("5TH", "5", clean_addstreet),
         clean_addstreet = gsub("6TH", "6", clean_addstreet),
         clean_addstreet = gsub("7TH", "7", clean_addstreet),
         clean_addstreet = gsub("8TH", "8", clean_addstreet),
         clean_addstreet = gsub("9TH", "9", clean_addstreet),
         clean_addstreet = gsub(" TH ", " ", clean_addstreet),
         clean_addstreet = gsub("STST", "ST ST", clean_addstreet),
         clean_addstreet = gsub("46TH ST6TH AVENUE", "46TH STREET", clean_addstreet),
         clean_addstreet = gsub("ST.", "ST", clean_addstreet),
         clean_addstreet = gsub("STST", "ST", clean_addstreet),
         clean_addstreet = gsub("STTEET", "ST", clean_addstreet),
         clean_addstreet = gsub("STEEGT", "ST", clean_addstreet),
         clean_addstreet = gsub("STRET", "ST", clean_addstreet),
         clean_addstreet = gsub("PLACE", "PL", clean_addstreet),
         clean_addstreet = gsub("PLACT", "PL", clean_addstreet),
         clean_addstreet = gsub("PLACR", "PL", clean_addstreet),
         clean_addstreet = gsub("PLAACE", "PL", clean_addstreet),
         clean_addstreet = gsub(" RD", " ROAD", clean_addstreet),
         clean_addstreet = gsub("BOULEVARD", "BLVD", clean_addstreet),
         clean_addstreet = gsub("WYKOFF", "WYCKOFF", clean_addstreet),
         clean_addstreet = gsub("WYKCOFF", "WYCKOFF", clean_addstreet),
         clean_addstreet = gsub("WHYTHE", "WYTHE", clean_addstreet),
         clean_addstreet = gsub("ADELHI", "ADELPHI", clean_addstreet),
         #clean_addstreet = gsub("ADELPHI", "ADELPHI", clean_addstreet),
         clean_addstreet = gsub("ADELPHIA", "ADELPHI", clean_addstreet),
         clean_addstreet = gsub("ADELPKI", "ADELPHI", clean_addstreet),
         clean_addstreet = gsub("WHYTHE", "ADELPHI", clean_addstreet),
         clean_addstreet = gsub("WEST", "WEST ", clean_addstreet),
         clean_addstreet = gsub("EAST", "EAST ", clean_addstreet), 
         clean_addstreet = gsub("WEST ERN", "WESTERN", clean_addstreet),
         clean_addstreet = gsub("EAST ERN", "EASTERN", clean_addstreet), 
         clean_addstreet = gsub("STWEST", "WEST", clean_addstreet),
         clean_addstreet = gsub("AVENUEE", "AVENUE", clean_addstreet),
         clean_addstreet = gsub("AVNUE", "AVENUE", clean_addstreet),
         clean_addstreet = gsub("BOKAE", "BOKEE", clean_addstreet),
         clean_addstreet = gsub("FLATBUSH AVE", "FLATBUSH AVENUE", clean_addstreet),
         clean_addstreet = gsub("MARTIN L K", "MARTIN LUTHER K", clean_addstreet),
         clean_addstreet = gsub("OCEAN AVE", "OCEAN AVENUE", clean_addstreet),
         clean_addstreet = trimws(clean_addstreet))

#####################################
### explore the street name data ###
#####################################
streets <- cleaned_dems %>% 
  group_by(clean_addstreet) %>% 
  summarise(count = n(),
            aded = first(AD))

bad_streets <- streets %>% 
  filter(count < 10) 

# write.csv(bad_streets, "processed_data/streets_to_correct.csv")

#### The csv above will be corrected to replace the final misspelled street names 
#### and then resume cleaning the addresses below

##### Next steps before we can contniue to create files
##### import corrected street names
##### join it to cleaned_dems to correct the remaining street names
##### explore street name data again to see if there are more streets to correct

########################################
### END fixing the street name data ###
########################################

cleaned_dems %<>% 
  mutate(address = paste(addnumber, addpredirect, clean_addstreet),
         addnumber2 = gsub('\\b 1/2','',addnumber),
         buildingnum = as.numeric(gsub("[^0-9]", "", addnumber2)),
         aptnum = as.numeric(gsub("[^0-9]", "", addapt)),
         last_voted = substr(votehistory, 0, 11),
         voterscore = "",
         knocked = "",
         #Moved = "",
         #Refused.Inaccessible = "",
         signed = "",
         notes = "",
         age = paste(2019 - as.numeric(substr(DOB, 0, 4))),
         streetside = if_else((as.numeric(as.character(buildingnum)) %% 2 == 0),'even','odd')
  ) %>%
  select(lastname, firstname, address, addapt, age, gender,
         ED, AD, last_voted, voterscore, streetside,
         clean_addstreet, addnumber, buildingnum, aptnum, votehistory,
         knocked, signed, notes)  %>%
  rename(apt = addapt)

# getscore = function(i) {
#   sum(c(if_else(grepl(pattern = 'Pri|PR',i) & grepl(pattern = '2018',i),9,0),
#         if_else(grepl(pattern = 'Pri|PR',i) & grepl(pattern = '2017',i),6,0),
#         if_else(grepl(pattern = 'Pri|PR|PP',i) & grepl(pattern = '2016',i),2,0)))#,
# }
# 
# for(i in 1:nrow(dems)) {
#   votehist <- vector()
#   votehist <- unlist(strsplit(as.character(dems$votehistory[i]),split = ";"))
#   votehistscore <- as.double(unique(sapply(votehist, getscore)))
#   dems$voterscore[i] <- if_else(all(nchar(votehist) == 0), 
#                                 0, sum(unlist(votehistscore)))
# }

### it's a silly wy to do it, but it is way faster than the ifelse (~ 1min)
### this just looks for year, but we could figure out how to include 
### your primary indicator too
cleaned_dems2 <- cleaned_dems %>% 
  mutate(for18 = gsub("2018", "yes", votehistory),
         for17 = gsub("2017", "yes", votehistory),
         for16 = gsub("2016", "yes", votehistory),
         votes18 = str_count(for18, "yes"),
         votes17 = str_count(for17, "yes"),
         votes16 = str_count(for16, "yes"),
         voterscore = votes18 + votes17 + votes16)

# sort by: street_name, streetside, house_num, aptnum, apt
### create the list of ads and eds
ads = as.list(unique(cleaned_dems2$AD))
edadlist = list()
for (i in ads) {
  ad_table <- cleaned_dems2 %>%
    filter(AD==i)  
  eds = as.list(unique(ad_table$ED))
  print(i)
  edlist = list()
  for (j in eds) {
    ed_table <- ad_table %>%
      filter(ED==j)
    ed_table[order(ed_table$voterscore,decreasing = TRUE),'prime'][1:50] <- '*'
    ed_table[which(ed_table$prime != "*"),'prime'] <- ""
    edlistj = ed_table[order(ed_table$clean_addstreet, ed_table$streetside, ed_table$buildingnum,
                             ed_table$addnumber, ed_table$aptnum, ed_table$apt),]
    edlist[[j]] = edlistj
  }
  edadlist[[i]] <- do.call(dplyr::bind_rows, edlist)
}

### create workbook, if not already created
walklist <- createWorkbook()
addWorksheet(walklist, "Sheet 1")
dir.create(paste0(path,"ed_tables"))
dir.create(paste0(path,"ed_tables/googlesheets"))
dir.create(paste0(path,"ed_tables/print_lists"))

for (i in ads) {
  edad_table <- edadlist[[i]]
  eds = as.list(unique(edad_table$ED))
  for (j in eds) {
    ed_table <- edad_table %>%
      filter(ED==j)
    filename = paste0("ad_", i, "_ed_", j, ".xlsx")
    writeDataTable(walklist, sheet = 1, x = ed_table[,c("lastname","firstname","address","apt","age",
                                                   "gender","voterscore","knocked","signed","notes")],
              rowNames = T)
    setColWidths(walklist, sheet = 1, cols = 1, widths = 4)
    setColWidths(walklist, sheet = 1, cols = 2:3, widths = 20)
    setColWidths(walklist, sheet = 1, cols = 4, widths = 30)
    setColWidths(walklist, sheet = 1, cols = 5:6, widths = 5)
    setColWidths(walklist, sheet = 1, cols = 7, widths = 7)
    setColWidths(walklist, sheet = 1, cols = 8, widths = 7)
    setColWidths(walklist, sheet = 1, cols = 9, widths = 6)
    setColWidths(walklist, sheet = 1, cols = 10, widths = 6)
    setColWidths(walklist, sheet = 1, cols = 11, widths = 30)
    saveWorkbook(walklist, paste0(path,'ed_tables/googlesheets/',filename),overwrite = TRUE)
  }
}


for (i in ads) {
  edad_table <- edadlist[[i]]
  eds = as.list(unique(edad_table$ED))
  for (j in eds) {
    ed_table <- edad_table %>%
      filter(ED==j)
    filename = paste0("ad_", i, "_ed_", j)
    writeData(walklist, sheet = 1, x = ed_table[,c("lastname","firstname","address","apt","age",
                                                   "gender","voterscore","knocked","signed","notes")],rowNames = T)
    setColWidths(walklist, sheet = 1, cols = 1, widths = 4)
    setColWidths(walklist, sheet = 1, cols = 2:3, widths = 20)
    setColWidths(walklist, sheet = 1, cols = 4, widths = 30)
    setColWidths(walklist, sheet = 1, cols = 5:6, widths = 6)
    setColWidths(walklist, sheet = 1, cols = 7, widths = 8)
    setColWidths(walklist, sheet = 1, cols = 8, widths = 7)
    setColWidths(walklist, sheet = 1, cols = 9, widths = 5)
    setColWidths(walklist, sheet = 1, cols = 10, widths = 30)
    saveWorkbook(walklist, paste0(path,'ed_tables/googlesheets/',filename,'.xlsx'), overwrite = TRUE)
    writeData(walklist, sheet = 1, x = ed_table[,c(1,2,3,4,5,6,10,19)],rowNames = T)
    setColWidths(walklist, sheet = 1, cols = 1, widths = 4)
    setColWidths(walklist, sheet = 1, cols = 2:3, widths = 20)
    setColWidths(walklist, sheet = 1, cols = 4, widths = 30)
    setColWidths(walklist, sheet = 1, cols = 5:6, widths = 6)
    setColWidths(walklist, sheet = 1, cols = 7, widths = 8)
    setColWidths(walklist, sheet = 1, cols = 8, widths = 7)
    setColWidths(walklist, sheet = 1, cols = 9, widths = 5)
    setColWidths(walklist, sheet = 1, cols = 10, widths = 30)
    saveWorkbook(walklist,paste0(path,'ed_tables/pdf_lists/print_lists/',filename,'.xlsx'))
  }
}


for (i in ads) {
  edad_table <- edadlist[[i]]
  eds = as.list(unique(edad_table$ED))
  for (j in eds) {
    ed_table <- edad_table %>%
      filter(ED==j)
    filename = paste0("ad_", i, "_ed_", j)
    writeData(walklist, sheet = 1, x = ed_table[,c(1,2,3,4,5,6,10,19)],rowNames = T)
    setColWidths(walklist, sheet = 1, cols = 1, widths = 4)
    setColWidths(walklist, sheet = 1, cols = 2:3, widths = 20)
    setColWidths(walklist, sheet = 1, cols = 4, widths = 30)
    setColWidths(walklist, sheet = 1, cols = 5:6, widths = 6)
    setColWidths(walklist, sheet = 1, cols = 7, widths = 8)
    setColWidths(walklist, sheet = 1, cols = 8, widths = 7)
    setColWidths(walklist, sheet = 1, cols = 9, widths = 5)
    setColWidths(walklist, sheet = 1, cols = 10, widths = 30)
    saveWorkbook(walklist, paste0(path,'ed_tables/googlesheets/',filename,'.xlsx'), overwrite = TRUE)
  }
}
