permit <- read.csv("C:/Users/sofia/OneDrive/Desktop/iConsult/Permit Data/Permit Data.csv")
View(permit)


#Deleting serial number column
permit <- permit[,-1]


#Removing timestamp from application date
permit$application_date <- sub(' .*', '', permit$application_date)


#Removing timestamp from issue date
permit$issue_date <- sub(' .*', '', permit$issue_date)


#Deleting permit application type id
permit <- permit[,-4]


#Removing application number before 2013
permit <- permit[(permit$application_date > "2012-12-31"), ]


#Removing dirty application number
#It is possible that there are different patterns of dirty data in new datafiles every month
#We can just keep adding these patterns in the below code
permit$application_number <- sub('.*\\.', '', permit$application_number)  
#This will remove the decimal in the application number


#Removing Voided status type
permit <- permit[permit$status_type_name != "Voided", ]


#Replacing the NA values in the Issue Date with 1900-01-01
permit$issue_date[is.na(permit$issue_date)] <- "1900-01-01"


#Creating a new column DUE Date based on the time frame provided
permit["Due Date"] <- NA
permit$application_date <- as.Date(permit$application_date, format = "%Y-%m-%d")

permit$`Due Date` <- ifelse(
  permit$permit_type_description == "ROW Road Cut",
  as.character(permit$application_date + 42),
  ifelse(
    permit$permit_type_description == "ROW Curb Cut",
    as.character(permit$application_date + 21),
    ifelse(
      permit$permit_type_description == "Commercial Renovation / Remodel",
      as.character(permit$application_date + 42),
      ifelse(
        permit$permit_type_description == "Residentail Remodel",
        as.character(permit$application_date + 14),
        ifelse(
          permit$permit_type_description == "New Commercial Construction",
          as.character(permit$application_date + 56),
          ifelse(
            permit$permit_type_description == "New 1-2 Family Home",
            as.character(permit$application_date + 28),
            as.character(permit$issue_date)
          )
        )
      )
    )
  )
)


#Created Is On Time Column
permit["Is On Time"] <- NA

permit$`Is On Time` <- ifelse(
    permit$issue_date <= permit$`Due Date`,
    permit$`Is On Time` <- "YES",
    permit$`Is On Time` <- "NO"
  )

index <- permit$issue_date == "1900-01-01"
permit$`Is On Time`[index] <- "In Progress"


#Deleting the NA values from Neighborhood
permit$Neighborhood <- as.character(permit$Neighborhood)
permit$Neighborhood[is.na(permit$Neighborhood)] <- "Unknown"
permit <- permit[!(permit$Neighborhood == "Unknown"),]


#Creating a new column Days_for_Issue
permit["Days_for_Issue"] <- NA
permit$application_date <- as.Date(permit$application_date)
permit$issue_date <- as.Date(permit$issue_date)
permit$Days_for_Issue <- permit$issue_date - permit$application_date


#Changing status type to Issued for all applications which had the Issue date but the status type was not issued
index2 <- permit$issue_date != "1900-01-01"
permit$status_type_name[index2] <- "Issued"


#Deleted the records which had Issue Date before the Application Date
permit <- subset(permit, issue_date == "1900-01-01" | application_date <= issue_date)
View(permit)


#Replacing negative values with -1 in permit data
permit$Days_for_Issue[permit$Days_for_Issue<0] <- -1
View(permit)


#Permit Reviewers
Reviewers <- read.csv("C:/Users/sofia/OneDrive/Desktop/iConsult/Permit Data/Permit Reviewers.csv")


#Creating a new column Reviewer Name
Reviewers$ReviewerName <- paste(Reviewers$first_name, Reviewers$last_name)
Reviewers$ReviewerName[which(Reviewers$ReviewerName == " ")] <- "Unknown"
View(Reviewers)


#Removing duplicate rows 
install.packages("dplyr", repos = "http://cran.us.r-project.org")
library("dplyr")

permit <- permit[!duplicated(permit$application_number),]
View(permit)


#Removing dirty application number
Reviewers$application_number <- sub('.*\\.', '', Reviewers$application_number)


#Merging permit and reviewers data
PermitReviewers <- merge(Reviewers, permit, by="application_number", all=TRUE)
View(PermitReviewers)


#Removing extra columns
PermitReviewers <- PermitReviewers[,-2:-6]
PermitReviewers <- na.omit(PermitReviewers)


#Permit Approvers
Approvers <- read.csv("C:/Users/sofia/OneDrive/Desktop/iConsult/Permit Data/Permit Approvals.csv")
colnames(Approvers)[3] <- "application_number"
View(Approvers)


#Merging permit and Approvals
install.packages("plyr", repos = "http://cran.us.r-project.org")
library(plyr)

PermitApprovers <- join(Approvers, permit, by="application_number", type='right', match='all')
PermitApprovers <- na.omit(PermitApprovers)
PermitApprovers <- PermitApprovers[,-2:-4]
View(PermitApprovers)


#Alternate Permit Type
AlternatePermits <- read.csv("C:/Users/sofia/OneDrive/Desktop/iConsult/Permit Data/Alternative Permit Types.csv")
View(AlternatePermits)


#Changing date format and converting it from character to date
#AlternatePermits$application_date <- strftime(strptime(AlternatePermits$application_date,"%d/%m/%y"), "%m/%d/%Y")
AlternatePermits$issue_date <- strftime(strptime(AlternatePermits$issue_date,"%Y-%m-%d"), "%m/%d/%Y")
AlternatePermits$issue_date <- as.Date(AlternatePermits$issue_date, "%m/%d/%Y")
AlternatePermits$application_date <- as.Date(AlternatePermits$application_date, "%m/%d/%Y")


#Creating new column Days for Issue
AlternatePermits$Days_for_Issue <- NA
AlternatePermits$Days_for_Issue <- AlternatePermits$issue_date - AlternatePermits$application_date 


# Changing column names

colnames(Reviewers)[2] <- "r_application_number"
colnames(Reviewers)[1] <- "r_permit_type_description"
colnames(Reviewers)[3] <- "r_application_date"

colnames(Approvers)[3] <- "a_application_number"
colnames(Approvers)[2] <- "a_permit_type_description"
colnames(Approvers)[1] <- "a_application_date"

colnames(AlternatePermits)[3] <- "alt_application_number"
colnames(AlternatePermits)[1] <- "alt_application_date"

colnames(PermitReviewers)[1] <- "pr_application_number"

colnames(PermitApprovers)[1] <- "pa_application_number"
colnames(PermitApprovers)[7] <- "pa_permit_type_description"
colnames(PermitApprovers)[5] <- "pa_application_date"


