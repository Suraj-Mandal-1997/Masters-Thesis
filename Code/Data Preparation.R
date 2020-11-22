library(plyr)
library(tidyverse)

acc <- read.csv("dftRoadSafetyData_Accidents_2018.csv")
cas <- read.csv("dftRoadSafetyData_Casualties_2018.csv")
veh <- read.csv("dftRoadSafetyData_Vehicles_2018.csv")
dm<- merge(acc, cas)
dataset<-merge(dm,veh)

data1 <-dataset
data2<- data1 %>%  filter(Casualty_Type == "1")
data3 <- data2[,c(11:13,18:21,26:29,31,36:37,39,59)]
str(data3)
data4 <- data.frame(lapply(data3, function(x) {
  gsub(-1, NA, x)
}))
table(is.na(data4))
data <- na.omit(data4)

table(is.na(data))

data<-na.omit(data)

str(data)

names(datas)

# changing the class of certain variables
data[,2]  <- as.factor(data$Day_of_Week)
data[,3]  <- as.factor(data$Time)
data[,4]  <- as.factor(data$Road_Type)
data[,5]  <- as.factor(data$Speed_limit)
data[,6]  <- as.factor(data$Junction_Detail)
data[,7] <- as.factor(data$Junction_Control)
data[,8] <- as.factor(data$Light_Conditions)
data[,9] <- as.factor(data$Weather_Conditions)
data[,10] <- as.factor(data$Road_Surface_Conditions)
data[,11] <- as.factor(data$Special_Conditions_at_Site)
data[,12] <- as.factor(data$Urban_or_Rural_Area)
data[,13] <- as.factor(data$Sex_of_Casualty)
data[,14] <- as.integer(data$Age_of_Casualty)
data[,15] <- as.factor(data$Casualty_Severity)
data[,16] <- as.factor(data$Journey_Purpose_of_Driver)
str(data)

# renaming the factor variables for better understanding

data$Casualty_Severity <- revalue(data$Casualty_Severity, c('1'='Fatal','2'='Serious','3'='Slight'))
data$Day_of_Week <- revalue(data$Day_of_Week, c('1'='Sunday','2'='Monday','3'='Tuesday',
                                                '4'='Wednesday','5'='Thursday','6'='Friday','7'='Saturday'))
data$Sex_of_Casualty <- revalue(data$Sex_of_Casualty, c('1'='Male','2'='Female','3'='Unknown'))

# binning the time variable into 4 parts
data$Time <- format(strptime(data$Time,"%H:%M"),'%H')
data[,3] <- as.numeric(data$Time)
data$Time <- cut(data$Time, c(0,5,12,17,20,24), 
                 labels=c('Night1','Morning','Afternoon','Evening','Night2'))

# grouping the night1 and 2 into one level
levels(data$Time) <- c('Night','Morning','Afternoon','Evening','Night')
#levels(data$Time)

# date variable
data$Date <- format(as.Date(data$Date, "%d/%m/%Y"), "%m")
data[,1]<- as.factor(data$Date)
cas1<-data

##(1) Distribution of the target variable Casualty Severity
cas_sev<-cas1 %>%
  group_by(Casualty_Severity) %>%
  summarise(total=n())

ggplot(cas_sev) +
  geom_bar(mapping =aes(x=Casualty_Severity, y=total),stat = "identity", position=position_dodge())+
  labs(x="Casualty_Severity",y="Number of Casualties",col=c("#F7DC6F", "#CD6155" ,"#ff88bb")) +theme_light()+
  theme(axis.text.x = element_text(angle=0, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+
  scale_y_continuous(breaks=seq(0,6000,1000))

# target variable is imbalanced. grouping the levels into high and low
levels(data$Casualty_Severity) <- c('High','High','Low')

levels(data$Light_Conditions) <- c("1", "1", "2",'2','3')

levels(data$Weather_Conditions) <- c("1", "2", "3",'4','5','6','7','8','8')

levels(data$Journey_Purpose_of_Driver) <- c('1','1','2','2','3','3','3')

# renaming the other factor variables

data$Road_Type <- revalue(data$Road_Type, c('1'='Roundabout','2'='One way street',
                                            '3'='Dual carriageway','6'='Single carriageway','7'='Slip road','9'='Unknown'))

data$Light_Conditions <- revalue(data$Light_Conditions, c('1'='Lit','2'='Dark','3'='unknown'))

data$Weather_Conditions <- revalue(data$Weather_Conditions, c('1'='Fine - no high winds','2'='Raining - no high winds',
                                                              '3'='Snowing - no high winds','4'='Fine - high winds','5'='Raining - high winds','6'='Snowing - high winds',
                                                              '7'='Fog or mist','8'='Other'))

data$Road_Surface_Conditions <- revalue(data$Road_Surface_Conditions, c('1'='Dry','2'='Wet or damp',
                                                                        '3'='Snow','4'='Frost or ice','5'='Flood over 3cm. deep','6'='Oil or diesel',
                                                                        '7'='Mud'))

data$Special_Conditions_at_Site<- revalue(data$Special_Conditions_at_Site,c('0'='None','1'='Auto traffic signal - out',
                                                                            '2'='Auto signal part defective',
                                                                            '3'='Road sign or marking defective or obscured','4'='Roadworks','5'='Road surface defective','6'='Oil or diesel','7'='Mud'))

data$Junction_Detail<- revalue(data$Junction_Detail,c('0'='Not at junction or within 20 metres','1'='Roundabout','2'='Mini-roundabout','3'='T or staggered junction','5'='Slip road',
                                                      '6'='Crossroads','7'='More than 4 arms (not roundabout)','8'='Private drive or entrance','9'='Other junction'))


data$Junction_Control<- revalue(data$Junction_Control,c('0'='Not at junction or within 20 metres','1'='Authorised person','2'='Auto traffic signal',
                                                        '3'='Stop sign','4'='Give way or uncontrolled'))

data$Journey_Purpose_of_Driver <- revalue(data$Journey_Purpose_of_Driver, c('1'='For work','2'='For school',
                                                                            '3'='Other'))


data$Urban_or_Rural_Area <- revalue(data$Urban_or_Rural_Area, c('1'='Urban','2'='Rural',
                                                                '3'='Unallocated'))

data$Date <-  revalue(data$Date,c('01'='January', '02'='February','03'='March','04'='April','05'='May','06'='June','07'='July','08'='August','09'='September','10'='October','11'='November','12'='December'))

data<-na.omit(data)
table(is.na(data))
str(data)
