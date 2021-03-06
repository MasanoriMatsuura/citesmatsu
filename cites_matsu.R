######cites########

### author: Matsu ###
setwd("C:\\Users\\user\\Documents\\NIES\\cites_matsu\\Trade_database_download_v2021.1\\Trade_database_download_v2021.1")

###install packages ###
pacman::p_load(tidyverse, readr, here, data.table, parallel)
here() #"C:/Users/user/Documents/NIES/cites_matsu/cites_matsu"

### import and append multiple csv ###
mydir = "C:\\Users\\user\\Documents\\NIES\\cites_matsu\\Trade_database_download_v2021.1\\Trade_database_download_v2021.1"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
dat_csv = ldply(myfiles, read_csv)


# trade46 <- read_csv("trade_db_46.csv")


# Test: Data44 ------------------------------------------------------------

trade46 %>% glimpse()

trade46 %>% select(Class) %>% unique()
# 1 Reptilia
# 2 Anthozoa
# 3 NA
# 4 Aves
# 5 Mammalia
# 6 Actinopteri
# 7 Amphibia
# 8 Elasmobranchii
# 9 Arachnida
# 10 Hydrozoa
# 11 Hirudinoidea
# 12 Insecta
# 13 Cephalopoda
# 14 Dipneusti
# 15 Gastropoda
# 16 Bivalvia

trade46 %>%
  filter(Class == "Amphibia") %>%
  group_by(Importer, Year) %>% count() %>%
  ggplot(aes(x = factor(Year), y = n, color = Importer, fill = Importer)) +
  geom_bar(stat = "identity")

summary(trade44$Year)


# Focusing on Japanese cases ----------------------------------------------

cites <- dat_csv #data_all

cites %>% names()
# [1] "Id"                     "Year"                   "Appendix"
# [4] "Taxon"                  "Class"                  "Order"
# [7] "Family"                 "Genus"                  "Term"
# [10] "Quantity"               "Unit"                   "Importer"
# [13] "Exporter"               "Origin"                 "Purpose"
# [16] "Source"                 "Reporter.type"          "Import.permit.RandomID"
# [19] "Export.permit.RandomID" "Origin.permit.RandomID"

summary(cites$Importer)

cites %>% glimpse()

cites$Taxon %>% unique()

### EDA ###

#### importer infor ###
cites$Importer %>% unique() #Importer

### creating subsets###
citesj <- subset(cites, cites$Importer == "JP") #the case of Japan
citesj21th <- subset(citesj, Year >= 2000) # the data after 21th century

summary(citesj)

### yearly number of import by Japan ###
citesj21th %>%
  ggplot( aes(x=Year))+geom_histogram(bins = 30) 

### pie chart by taxonomy in Japan ###
citesj21th %>% 
  group_by(Year) %>%
  ggplot( aes(x="", y=Quantity, fill=Taxon))+
  geom_col() +
  coord_polar(theta = "y")   



ggplot(citesj, 
       aes(x = Importer, y  = Quantity,
           fill = Quantity)) +
  #scale_x_log10() +
  ggridges::geom_density_ridges() 
scale_fill_ds()


citesj %>%
  select(Taxon, Importer, Exporter) %>%
  group_by(Importer, Taxon) %>%
  summarise()
unique(.$Expoter)

cites %>%
  filter(Year > 2018 & Exporter != "NA") %>%
  filter()
group_by(Exporter) %>%
  summarise(Quantity = sum(Quantity),
            case = sum(row_number())) %>%
  arrange(desc(Quantity))


cites %>% rank(Importer)
citesJapan <- data_all %>%
  filter(Importer == "JP" | Exporter == "JP")


cites %>%
  filter(Class == "Amphibia") %>%
  group_by(Importer, Year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = factor(Year), y = n, color = Importer)) +
  geom_line()

