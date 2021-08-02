# Comcast Telecom Consumer Complaints:

# load necessary packages

library(dplyr)
library(ggplot2)
library(lubridate)

# import data

setwd('E:/simplilearn/Data Science with R Programming/Assessment/Project 2')
getwd()
comcast_dataset <- read.csv(file ='Comcast Telecom Complaints data.csv')
View(comcast_dataset)

# parse Date column to the same format by using dmy()

comcast_dataset$Date <- dmy(comcast_dataset$Date)

# get the complaints on a daily level basis by group by Date column

daily_comp <- comcast_dataset %>% group_by(Date) %>% summarize(NumOfComplaints=n())
daily_comp

# plot a trend chart on a daily level
# simple line plot

plot(daily_comp, type = 'b') 

# ggplot
ggplot(data = daily_comp,aes(as.POSIXct(Date),NumOfComplaints))+
  geom_line()+
  geom_point(size = 2)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m/%y")+
  labs(title = "Daily Complaint Counts",x= "Days",y ="No. of Complaints")+
  theme(axis.text.x = element_text(angle = 75),
       plot.title = element_text(hjust = 0.5))

# parse month to a new Month column

comcast_dataset$Month <- month(comcast_dataset$Date)
View(comcast_dataset)

monthly_compl <- comcast_dataset %>% group_by(Month) %>% summarize(NumOfComplaints=n()) %>% arrange(desc(NumOfComplaints))

#Plotting for monthly granularity level
ggplot(data = monthly_compl,aes(Month,NumOfComplaints,label = NumOfComplaints))+
  geom_line()+
  geom_point(size = 2)+
  geom_text()+
  scale_x_continuous(breaks = monthly_compl$Month)+
  labs(title = "Monthly Complaint Counts",x= "Months",y ="No. of Complaints")+
  theme(plot.title = element_text(hjust = 0.5))

# create a table with the frequency of complaint types

comcast_dataset$Customer.Complaint <- tolower(comcast_dataset$Customer.Complaint)
View(comcast_dataset)

# complaint type processing

internet_tickets <- contains(comcast_dataset$Customer.Complaint, match = 'internet', ignore.case = T)
billing_tickets <- contains(comcast_dataset$Customer.Complaint, match = 'bill', ignore.case = T)
data_tickets <- contains(comcast_dataset$Customer.Complaint, match = 'data', ignore.case = T)
charges_tickets <- contains(comcast_dataset$Customer.Complaint, match = 'charge', ignore.case = T)
speed_tickets <- contains(comcast_dataset$Customer.Complaint, match = 'speed', ignore.case = T)
service_tickets <- contains(comcast_dataset$Customer.Complaint, match = 'customer service', ignore.case = T)
  
comcast_dataset$complaintType[internet_tickets] <- 'Internet'
comcast_dataset$complaintType[billing_tickets] <- 'Billing'
comcast_dataset$complaintType[data_tickets] <- 'Data issue'
comcast_dataset$complaintType[charges_tickets] <- 'Charges'
comcast_dataset$complaintType[speed_tickets] <- 'Speed'
comcast_dataset$complaintType[service_tickets] <- 'Service'

comcast_dataset$complaintType[-c(internet_tickets, billing_tickets, data_tickets, charges_tickets)] <- 'Others'

tickets_freq <- table(comcast_dataset$complaintType)
tickets_freq

tickets_freq <- as.data.frame(tickets_freq)
tickets_freq
names(tickets_freq)[names(tickets_freq) == 'Var1'] <- 'Types'
View(tickets_freq)
write.csv(x = tickets_freq, file = 'tickets_freq.csv')
# Create a new categorical variable for complaint status

status_freq <- table(comcast_dataset$Status)
status_freq

open_complaint <- (comcast_dataset$Status == 'Open' | comcast_dataset$Status == 'Pending')
closed_complaint <- (comcast_dataset$Status == 'Closed' | comcast_dataset$Status == 'Solved')

comcast_dataset$complaintStatus[open_complaint] <- 'Open'
comcast_dataset$complaintStatus[closed_complaint] <- 'Closed'

View(comcast_dataset)

# stacked bar chart 

stacked <- table(comcast_dataset$complaintStatus, comcast_dataset$State)
stacked

state_complaint <- comcast_dataset %>% group_by(State, complaintStatus) %>% summarise(count = n())
View(state_complaint)

ggplot(as.data.frame(state_complaint) ,mapping = aes(State,count))+
  geom_col(aes(fill = complaintStatus))+
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "No of Tickets")

# State with maximum complaints

comcast_subset <- comcast_dataset %>% filter(complaintStatus=='Open') %>% group_by(State) %>% summarize(NumOfComplaints=n()) %>% arrange(desc(NumOfComplaints))
View(comcast_subset)

# Percentage of complaints

number_of_total <- comcast_dataset %>% group_by(complaintStatus) %>% summarize(NumOfComplaints=n())
number_of_total

slices <- number_of_total$NumOfComplaints
pct <- round((slices/sum(slices)*100),2)
lbls <- paste(number_of_total$complaintStatus," ",pct,"%",sep="")

pie(slices, labels = lbls, col = rainbow(5))

# Percentage of complaints resolved of internet and customer care calls

internet_compl <- comcast_dataset %>% filter(Received.Via=='Internet',complaintStatus=='Closed') %>% group_by(Received.Via,complaintStatus) %>% summarize(NumOfComplaints=n()) 
customer_compl <- comcast_dataset %>% filter(Received.Via=='Customer Care Call',complaintStatus=='Closed') %>% group_by(Received.Via,complaintStatus) %>% summarize(NumOfComplaints=n()) 

# Percentage of complaints resolved by internet
internet_pct<-round(internet_compl$NumOfComplaints/sum(number_of_total$NumOfComplaints)*100,2)
internet_pct
# Percentage of complaints resolved by customer care calls
customer_pct<-round(customer_compl$NumOfComplaints/sum(number_of_total$NumOfComplaints)*100,2)
customer_pct
