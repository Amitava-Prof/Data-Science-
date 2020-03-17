library(ggplot2)
library(stringr)

#----------------------------------------------------
#Load data
#----------------------------------------------------
loan <- read.csv("loan.csv")
#View(loan)


#----------------------------------------------------
#Business & data Understanding
#----------------------------------------------------
# id column is unique
unique_id <- length(unique(loan$id))

#member_id is also unique
unique_member_id <- length(unique(loan$member_id))

#loan_status - no NULL value, three possible values(Charged Off - 5627, Current - 1140, Fully paid - 32950)
loan_status_na = sum(is.na(loan$loan_status))
table(loan$loan_status)
#Graph
ggplot(loan, aes(x = purpose, fill= purpose)) + geom_bar() + ylab("No.of Requests")

#loan_amnt - No NULL value, median is near 10000, more amount exists between 5000 - 15000 , outlier exists > 35000
loan_amnt_empty <- sum(is.na(loan$loan_amnt))
boxplot(loan$funded_amnt, horizontal=TRUE, main="Loan amount distribution")

#term - No NULL value, 2 terms 36 months - 29096, 60 months - 10621
table(loan$term)

#emp_length - values - <1 year, 1 year - 9 years, 10+ years, n/a
table(loan$emp_length)

#home_ownership - 5 different values - MORTGAGE - 17659, NONE - 3, OTHER - 98 OWN - 3058, RENT - 18899
table(loan$home_ownership)

#verification_status - 3 values - Not Verified, Source Verified, Verified
table(loan$verification_status)
#Graph
ggplot(loan, aes(x = verification_status, fill= verification_status)) + geom_bar() + ylab("No.of Requests")


#purpose - car, credit_card, debt_consolidation, educational, home_improvement, house, major_purchase, medical
#moving, renewable_energy, small_business, vacation, wedding
table(loan$purpose)

#annual_inc - Minimum annual income 4000, Maximum - 6000000, Mean - 68969, Median - 59000
median(loan$annual_inc)
summary(loan$annual_inc)

#delinq_2yrs - no NA value, value range 0 - 11
delinq_na <- sum(is.na(loan$delinq_2yrs))
table(loan$delinq_2yrs)

#inq_last_6mths - no NA value, value range 0 - 8
inq_6mths <- sum(is.na(loan$inq_last_6mths))
table(loan$inq_last_6mths)

#mths_since_last_delinq, NA value for 25682 records, value range 0 - 120
last_delinq <- sum(is.na(loan$mths_since_last_delinq))
table(loan$mths_since_last_delinq)
#Graph
ggplot(loan, aes(x = mths_since_last_delinq, fill= mths_since_last_delinq)) + geom_bar() + ylab("No.of Requests")


#mths_since_last_record - 36931 values are NA 
count_na <- sum(is.na(loan$mths_since_last_record))
table(loan$mths_since_last_record)

#revol_util,loan$recoveries - No NA value
count_na <- sum(is.na(loan$recoveries))
table(loan$recoveries)

#loan$collection_recovery_fee - No NA value
count_na <- sum(is.na(loan$collection_recovery_fee))
table(loan$collection_recovery_fee)

#----------------------------------------------------
#Data Cleaning
#----------------------------------------------------

# Remove the all NULL column
loan<-Filter(function(x)!all(is.na(x)), loan)

# Remove all zero column
loan<-Filter(function(x)!all(x==0), loan)

#Covert all the date columns to date type column
table(loan$issue_d)
# format is Mon-yy , appending 01 as date in front and coverting it to date type column
loan$issue_d = sapply(loan$issue_d, function(x) paste('01',x,sep='-'))
loan$issue_d = strptime(loan$issue_d, format = "%d-%b-%y")


table(loan$earliest_cr_line)
loan$earliest_cr_line = sapply(loan$earliest_cr_line, function(x) paste('01',x,sep='-'))
loan$earliest_cr_line = strptime(loan$earliest_cr_line, format = "%d-%b-%y")

table(loan$last_credit_pull_d)
loan$last_credit_pull_d = sapply(loan$last_credit_pull_d, function(x) paste('01',x,sep='-'))
loan$last_credit_pull_d = strptime(loan$last_credit_pull_d, format = "%d-%b-%y")
count_na <- sum(is.na(loan$last_credit_pull_d))

table(loan$last_pymnt_d)
loan$last_pymnt_d = sapply(loan$last_pymnt_d, function(x) if(is.na(x)) x else paste('01',x,sep='-'))
loan$last_pymnt_d = strptime(loan$last_pymnt_d, format = "%d-%b-%y")
# There are 71 blank values, all of them have loan status charged off so, replacing them with old date 2000-01-01
loan$last_pymnt_d[is.na(loan$last_pymnt_d)] <- strptime('2000-01-01',format="%Y-%m-%d")

#Replacing NA with 0 for loan$mths_since_last_record
loan$mths_since_last_record[is.na(loan$mths_since_last_record)] <- 0

#-----------------------------------------------------
# Creating New Columns for calculation
#-----------------------------------------------------

#create new column tenure by removing the ' months' string to use it in alculated field
loan$tenure = as.numeric(gsub(" months", "", loan$term))
#Graph
ggplot(loan, aes(x = tenure, fill= tenure)) + geom_bar() + ylab("Number of Requests")


# Create new column debt = Monthly installment * tenure

debt <- function(x,y)
  {
  return(x*y)
  }

loan$debt = mapply(debt, loan$installment, loan$tenure)

# Create new field DTP_ratio = ((total_rec_prncp + total_rec_int)/debt) *100
loan$DTP_ratio = ((loan$total_rec_prncp + loan$total_rec_int)/loan$debt) * 100

# Creat new column for employee experience
loan$emp_exp = loan$emp_length
loan$emp_exp = gsub("n/a",0,loan$emp_exp)
regexp <- "[[:digit:]]+"
loan$emp_exp <- str_extract(loan$emp_exp, regexp)
#Graph
ggplot(loan, aes(x = emp_exp, fill = emp_exp)) + geom_bar() 

#Risk Assesment - Calculate the credit score
# Function Module to calculate the behavioural score of an applicant
customer_score <- function(annual_inc,home_ownership, verification_status, dti, DTP_ratio, emp_exp, tenure)
{
  monthly_inc = annual_inc/12
  if(monthly_inc >= 4400)
    customer_score = 10
  else
    customer_score = 5
  if(verification_status == 'Not Verified')
    customer_score = customer_score + 5
  else
    customer_score = customer_score + 10
  if(home_ownership == 'MORTGAGE' | home_ownership == 'OWN')
    customer_score = customer_score + 10
  else
    customer_score = customer_score + 5
  if(dti<10)
    customer_score = customer_score + 10
  else if(dti>=10)
    customer_score = customer_score + 5
  if(DTP_ratio>=70)
    customer_score = customer_score + 10
  else if(DTP_ratio<70)
    customer_score = customer_score + 5
  if(emp_exp>=3)
    customer_score = customer_score + 10
  else
    customer_score = customer_score + 5
  if(tenure>36)
    customer_score = customer_score + 5
  else
    customer_score = customer_score + 10
  return(customer_score)
}  

# New field created for the customer score
loan$customer_score = mapply(customer_score, loan$annual_inc, loan$home_ownership , loan$verification_status, loan$dti, loan$DTP_ratio, loan$emp_exp, loan$tenure)

# Function Module to calculate the transaction score of an applicant
transaction_score <- function(issue_d,last_payment_d,delinq_2yrs,inq_last_6mths, mths_since_last_delinq)
{
  if(is.na(mths_since_last_delinq))
    transaction_score = 10
  else if(mths_since_last_delinq < 89)
    transaction_score = 10
  else 
    transaction_score = 5
  if(delinq_2yrs<2)
    transaction_score = transaction_score + 10
  else
    transaction_score = transaction_score + 5
  if(inq_last_6mths<2)
    transaction_score = transaction_score + 10
  else
    transaction_score = transaction_score + 5
  datetime = Sys.Date()
  datediff_in_days = difftime(strptime(issue_d,format='%Y-%m-%d'), strptime(Sys.Date(),format='%Y-%m-%d'), units="days")
  datediff_year = as.double(datediff_in_days/365)
  if(datediff_year>2)
    transaction_score = transaction_score + 10
  else
    transaction_score = transaction_score + 5
  datediff_in_days = difftime(strptime(last_payment_d,format='%Y-%m-%d'),strptime(Sys.Date(),format='%Y-%m-%d'), units="days")
  datediff_month = as.double(datediff_in_days/365)
  if(datediff_month<=6)
    transaction_score = transaction_score + 10
  else
    transaction_score = transaction_score + 5
  return(transaction_score)
}

# New column created for transaction score
loan$transaction_score = mapply(transaction_score,as.Date(loan$issue_d),as.Date(loan$last_pymnt_d),loan$delinq_2yrs,loan$inq_last_6mths,loan$mths_since_last_record)


# Function Module to calculate the behavioural score of an applicant
behavioural_score <- function(issue_d,earliest_cr_line,mths_since_last_record,revol_util,recoveries,collection_recovery_fee)
{
  date_diff_days = difftime(strptime(issue_d,format = "%Y-%m-%d"), strptime(earliest_cr_line,format = "%Y-%m-%d"), units="days")
  datediff_year = as.double(date_diff_days/365)
  if(datediff_year >= 2)
  {behavioural_score = 10}
  else
  {behavioural_score = 5}
  if( mths_since_last_record <= 1 )
  {
    behavioural_score = behavioural_score + 10
  }
  else
  {
    behavioural_score = behavioural_score + 5
  }
  if(revol_util<50)
    behavioural_score = behavioural_score + 10
  else
    behavioural_score = behavioural_score + 5
  if(recoveries<=10)
    behavioural_score = behavioural_score + 10
  else
    behavioural_score = behavioural_score + 5
  if(collection_recovery_fee<=10)
    behavioural_score = behavioural_score + 10
  else
    behavioural_score = behavioural_score + 5
  return(behavioural_score)
}

# Create new column for revol_util by removing the %
loan$revol_util_1 <- gsub("%","",loan$revol_util)

# New column for the behavioural score of the applicant
loan$behavioural_score = mapply(behavioural_score,as.Date(loan$issue_d),as.Date(loan$earliest_cr_line),loan$mths_since_last_record,loan$revol_util_1,loan$recoveries,loan$collection_recovery_fee)

# Function Module to calculate the credit score of an applicant
# customer score 40% weightage, transactional score - 40% weightage, behavioural score - 20% weightage 
calc_score <- function(customer_score,transaction_score,behavioural_score)
{
  customer_score_final = (customer_score/70)*(40/100)*100 
  transaction_score_final = (transaction_score/50)*(40/100)*100
  behavioural_score_final = (behavioural_score/50)*(20/100)*100
  final_score = customer_score_final + transaction_score_final + behavioural_score_final
}

#New column for credit score of the loan applicant
loan$credit_score = mapply(calc_score,loan$customer_score,loan$transaction_score,loan$behavioural_score)

# New column for the eligibility based on the credit score of the applicant
loan$eligibility = sapply(loan$credit_score, function(x) if(x>=85) x = 'good' else if(x>=80 & x<85) x= 'average' else x = 'bad')

#-------------------------------------------------------
# Conclusion
#-------------------------------------------------------

# Our model has calculated a credit score for the applicant.
# Based on that we decide how rishky it will be to approve loan for the applicant
# If eligibility is Good then risk is low
# If eligibility is average then risk is medium
# If the eligibility is bad then risk is high

ggplot(loan, aes(x = eligibility, fill= eligibility)) + geom_bar() + ylab("Eligibility of the Applicants")
