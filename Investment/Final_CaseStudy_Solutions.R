#### Load the relevant libraries ###################################
library("dplyr")
library("tidyr")
library("stringr")
library("crayon")

### Data load of companies and rounds2
companies<- read.delim("companies.txt",header = TRUE,stringsAsFactors = FALSE,sep="\t")
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)

################### CHECKPOINT 1 ####################################
#How many unique companies are present in rounds2?

#convert the company_permalink to lowercase 
rounds2$company_permalink<-tolower(rounds2$company_permalink)
rounds2_unique<-unique(rounds2$company_permalink)
length(rounds2_unique)
####Anser:66368

#How many unique companies are present in companies?
#convert the column permalink to lowercase 
companies$permalink<-tolower(companies$permalink)
companies_unique<-unique(companies$permalink)
length(companies_unique)
####Anser:66368

#In the companies data frame, which column can be used as the unique key for each company? 
#Write the name of the column.
#Answer:permalink

#Are there any companies in the rounds2 file which are not present in companies? 
#Answer yes or no: Y/N
#Find out if there are any companies which are in rounds2 but not in companies data frame.
companies_diff<-!(rounds2$company_permalink %in% companies$permalink)
length(which(companies_diff=="TRUE"))
#Answer : N

#Merge the two data frames so that all  variables (columns)  in the companies frame are added to the 
#rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame ?
#rename column before merge
master_frame <- merge(companies,rounds2, by.x = "permalink", by.y = "company_permalink")
#Answer : 114949

################### CHECKPOINT 2 ####################################
### Investment type analysis
## Funding type -> Seed/Angel/venture/Private equity; Total of 13 types
## Company type -> Startup/Corporate

#### No of records having NA for raised_amount_usd= 19,987
#View(filter(master_frame,is.na(master_frame$raised_amount_usd) == TRUE))
### Replace all the NA with 0
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0
funding_gr<-group_by(master_frame,funding_round_type)
funding_types_mean<-summarise(funding_gr, mean(raised_amount_usd, na.rm = T))

#Average funding amount of venture type
#filter(funding_types_mean,funding_round_type == "venture" )
venture_mean<-funding_types_mean[which(funding_types_mean$funding_round_type == "venture"),2]
venture_mean
#Answer:10634054

#Average funding amount of angel type
#filter(funding_types_mean,funding_round_type == "angel" )
angel_mean<-funding_types_mean[which(funding_types_mean$funding_round_type == "angel"),2]
angel_mean
#Answer:764564

#Average funding amount of seed type
#filter(funding_types_mean,funding_round_type == "seed" )
seed_mean<-funding_types_mean[which(funding_types_mean$funding_round_type == "seed"),2]
seed_mean
#Answer:556607

#Average funding amount of private equity type
#filter(funding_types_mean,funding_round_type == "private_equity" )
PE_mean<-funding_types_mean[which(funding_types_mean$funding_round_type == "private_equity"),2]
PE_mean
#Answer:62111788

#Considering that Spark Funds wants to invest between 5 to 15 
#million USD per investment round, which investment type is the most suitable for it?
#Answer:venture


################### CHECKPOINT 3 ####################################
### Country Analysis
venture_master_frame<-filter(master_frame, funding_round_type == "venture")
top9<-aggregate(venture_master_frame$raised_amount_usd, list(venture_master_frame$country_code),sum)
#### arrange by desc order
top9<-arrange(top9,desc(x))
top9<-top9[-3,]
head(top9,9)

#### Top 3 english speaking countries
#1 : USA
#2 : GBR
#3 : IND

################### CHECKPOINT 4 ####################################
venture_sep<-separate(venture_master_frame,category_list, into = c("primary_sector","secondary"), "\\|",remove=FALSE, extra="merge")
#Answer: Primary_sector separated

######### Condense the mapping file ##########
mapping<-read.csv("mapping.csv", stringsAsFactors = FALSE)
## Cleanse the data FIRST ROW and BLANKS Column
mapping<-mapping[-1,]
mapping<-mapping[,-3]

mapping_new <-gather(mapping, sector, main, 2:9)
mapping_new <- mapping_new[!(mapping_new$main == 0), ]
mapping_new <- mapping_new[,-3 ]

venture_merged_frame<-merge(venture_sep,mapping_new, by.x = "primary_sector", by.y = "category_list")
#Answer: merged data with sector columns

################### CHECKPOINT 5 ####################################
#Country1 = USA
#Country2 = GBR 
#Country3 = IND
#FT       = Venture
#Range    = 5M to 15M USD

tmp<-venture_merged_frame
D1<-subset(tmp, tmp$country_code == "USA" & tmp$raised_amount_usd >= 5000000 & tmp$raised_amount_usd <= 15000000)
D2<-subset(tmp, tmp$country_code == "GBR" & tmp$raised_amount_usd >= 5000000 & tmp$raised_amount_usd <= 15000000)
D3<-subset(tmp, tmp$country_code == "IND" & tmp$raised_amount_usd >= 5000000 & tmp$raised_amount_usd <= 15000000)

#Total number of investments in USA (Based on the Obesrvation D1)
#Answer:11149
#Total number of investments in GBR (Based on the Obesrvation D2)
#Answer:577
#Total number of investments in IND (Based on the Obesrvation D3)
#Answer:299

#Total amount of investments
sum(D1$raised_amount_usd)
#Answer: 99661524549 for USA
sum(D2$raised_amount_usd)
#Answer: 5028704358 for GBR
sum(D3$raised_amount_usd)
#Answer: 2683537552 for IND

##################### 1. Analysis for the country USA #########################
### Group per sector both no of investments made & amount
D1_group<-group_by(D1,sector)
D1_cnt<-summarise(D1_group,cnt = length(sector))
D1_sum <-summarise(D1_group,total_usd= sum(raised_amount_usd))
D1_consolidated<-merge(D1_cnt,D1_sum, by = "sector" )  

### order based on the number of investments
D1_consolidated<-arrange(D1_consolidated,desc(cnt))

#Top sector (based on count of investments)
D1_consolidated$sector[1]
#Answer : Others
D1_consolidated$cnt[1]
#Answer : 2923

#Second-best sector (based on count of investments)
D1_consolidated$sector[2]
#Answer : Cleantech...Semiconductors
D1_consolidated$cnt[2]
#Answer : 2297

#Third-best sector (based on count of investments)
D1_consolidated$sector[3]
#Answer : Social..Finance..Analytics..Advertising
D1_consolidated$cnt[3]
#Answer : 1912

###take the subset for top sector and get the TOP COMPANY NAME
top_company_subset<-subset(D1, sector == D1_consolidated$sector[1])
top_company_subset<-summarise(group_by(top_company_subset,permalink),investment= sum(raised_amount_usd))
top_company_subset[which(top_company_subset$investment==max(top_company_subset$investment)),]
###Logic to get the company name
access_key_d1<-top_company_subset$permalink[which(top_company_subset$investment==max(top_company_subset$investment))]
companies$name[which(companies$permalink == access_key_d1)]
###Answer : virtustream

###take the subset for second best sector and get the TOP COMPANY NAME
top_company_subset_2<-subset(D1, sector == D1_consolidated$sector[2])
top_company_subset_2<-summarise(group_by(top_company_subset_2,permalink),investment= sum(raised_amount_usd))
top_company_subset_2[which(top_company_subset_2$investment==max(top_company_subset_2$investment)),]
###Logic to get the company name
access_key_d1<-top_company_subset_2$permalink[which(top_company_subset_2$investment==max(top_company_subset_2$investment))]
companies$name[which(companies$permalink == access_key_d1)]
###Answer : Biodesix

##################### 2. Analysis for the country GBR #########################
### Group per sector both no of investments made & amount
D2_group<-group_by(D2,sector)
D2_cnt<-summarise(D2_group,cnt = length(sector))
D2_sum <-summarise(D2_group,total_usd= sum(raised_amount_usd))
D2_consolidated<-merge(D2_cnt,D2_sum, by = "sector" )  

### order based on the number of investments
D2_consolidated<-arrange(D2_consolidated,desc(cnt))

#Top sector (based on count of investments)
D2_consolidated$sector[1]
#Answer : Others
D2_consolidated$cnt[1]
#Answer : 143

#Second-best sector (based on count of investments)
D2_consolidated$sector[2]
#Answer : Cleantech...Semiconductors
D2_consolidated$cnt[2]
#Answer : 127

#Third-best sector (based on count of investments)
D2_consolidated$sector[3]
#Answer : Social..Finance..Analytics..Advertising
D2_consolidated$cnt[3]
#Answer : 98

###take the subset for top sector and get the TOP COMPANY NAME
top_company_subset<-subset(D2, sector == D2_consolidated$sector[1])
top_company_subset<-summarise(group_by(top_company_subset,permalink),investment= sum(raised_amount_usd))
top_company_subset[which(top_company_subset$investment==max(top_company_subset$investment)),]
###Logic to get the company name
access_key_d2<-top_company_subset$permalink[which(top_company_subset$investment==max(top_company_subset$investment))]
companies$name[which(companies$permalink == access_key_d2)]
###Answer : Electric Cloud

###take the subset for second best sector and get the TOP COMPANY NAME
top_company_subset_2<-subset(D2, sector == D2_consolidated$sector[2])
top_company_subset_2<-summarise(group_by(top_company_subset_2,permalink),investment= sum(raised_amount_usd))
top_company_subset_2[which(top_company_subset_2$investment==max(top_company_subset_2$investment)),]
###Logic to get the company name
access_key_d2<-top_company_subset_2$permalink[which(top_company_subset_2$investment==max(top_company_subset_2$investment))]
companies$name[which(companies$permalink == access_key_d2)]
###Answer : EUSA Pharma

##################### 2. Analysis for the country IND #########################
### Group per sector both no of investments made & amount
D3_group<-group_by(D3,sector)
D3_cnt<-summarise(D3_group,cnt = length(sector))
D3_sum <-summarise(D3_group,total_usd= sum(raised_amount_usd))
D3_consolidated<-merge(D3_cnt,D3_sum, by = "sector" )  

### order based on the number of investments
D3_consolidated<-arrange(D3_consolidated,desc(cnt))

#Top sector (based on count of investments)
D3_consolidated$sector[1]
#Answer : Others
D3_consolidated$cnt[1]
#Answer : 109

#Second-best sector (based on count of investments)
D3_consolidated$sector[2]
#Answer : News..Search.and.Messaging
D3_consolidated$cnt[2]
#Answer : 52


#Third-best sector (based on count of investments)
D3_consolidated$sector[3]
#Answer : Entertainment
D3_consolidated$cnt[3]
#Answer : 33

###take the subset for top sector and get the TOP COMPANY NAME
top_company_subset<-subset(D3, sector == D3_consolidated$sector[1])
top_company_subset<-summarise(group_by(top_company_subset,permalink),investment= sum(raised_amount_usd))
top_company_subset[which(top_company_subset$investment==max(top_company_subset$investment)),]
###Logic to get the company name
access_key_d3<-top_company_subset$permalink[which(top_company_subset$investment==max(top_company_subset$investment))]
companies$name[which(companies$permalink == access_key_d3)]
###Answer : FirstCry.com

###take the subset for second best sector and get the TOP COMPANY NAME
top_company_subset_2<-subset(D3, sector == D3_consolidated$sector[2])
top_company_subset_2<-summarise(group_by(top_company_subset_2,permalink),investment= sum(raised_amount_usd))
top_company_subset_2[which(top_company_subset_2$investment==max(top_company_subset_2$investment)),]
###Logic to get the company name
access_key_d3<-top_company_subset_2$permalink[which(top_company_subset_2$investment==max(top_company_subset_2$investment))]
companies$name[which(companies$permalink == access_key_d3)]
###Answer : Gupshup

################### CHECKPOINT 6 ####################################
# Fund Raised Type
total_investment <- sum(funding_gr$raised_amount_usd, na.rm = T)
fund_gr_plot <- summarise(funding_gr, mean(raised_amount_usd, na.rm = T), sum(raised_amount_usd)/total_investment)
colnames(fund_gr_plot) [1:3] <- c("Fund_Type","Average","Fraction")
write.csv(arrange(fund_gr_plot,desc(Fraction,Average)), "fund_gr_plot.csv")


# Top 9 Companies
write.csv(top9[1:9,], "top9_company.csv")


# Top 3 countries(Not only English Speaking) and top 3 investment for Venture
top3_company_sector <- filter(venture_merged_frame,venture_merged_frame$country_code == 'USA' | venture_merged_frame$country_code == 'CHN' | venture_merged_frame$country_code == 'GBR')
top3_company_sector_grp<-group_by(top3_company_sector,country_code,sector)
top3_company_sector_sum<-summarise(top3_company_sector_grp,cnt = length(permalink))
top3_company_sector_final <- arrange(top3_company_sector_sum, desc(cnt))
write.csv(top3_company_sector_final, "top3_company_sector.csv")
################# End of Assignment ########################################



