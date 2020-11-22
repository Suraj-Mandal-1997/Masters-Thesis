##(1) Number of casualties by Road Type
Road_Type<-data %>%
  group_by(Casualty_Severity,Road_Type) %>%
  summarise(total=n())

ggplot(Road_Type) +
  geom_bar(mapping =aes(x=Road_Type, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Road Type",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=0, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+
  scale_y_continuous(breaks=seq(0,6000,1000))+scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 

## (2) Number of casualties by Speed Limit
Speed_limit<-data %>%
  group_by(Casualty_Severity,Speed_limit) %>%
  summarise(total=n())

ggplot(Speed_limit) +
  geom_bar(mapping =aes(x=Speed_limit, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Speed Limit",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=0, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+ 
  scale_y_continuous(breaks=seq(0,6000,1000))+scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 


## (3) Number of casualties by Lightning Conditions
Light_Conditions<-data %>%
  group_by(Casualty_Severity,Light_Conditions) %>%
  summarise(total=n())

ggplot(Light_Conditions) +
  geom_bar(mapping =aes(x=Light_Conditions, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Light Conditions",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=0, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+ 
  scale_y_continuous(breaks=seq(0,6000,1000))+scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 


## (4) Number of casualties by Weather Conditions
Weather_Conditions<-data %>%
  group_by(Casualty_Severity,Weather_Conditions) %>%
  summarise(total=n())

ggplot(Weather_Conditions) +
  geom_bar(mapping =aes(x=Weather_Conditions, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Weather Conditions",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=90, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+ 
  scale_y_continuous(breaks=seq(0,6000,1000))+scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 


## (5) Number of casualties by Road_Surface_Conditions
Road_Surface_Conditions<-data %>%
  group_by(Casualty_Severity,Road_Surface_Conditions) %>%
  summarise(total=n())

ggplot(Road_Surface_Conditions) +
  geom_bar(mapping =aes(x=Road_Surface_Conditions, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Road_Surface_Conditions",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=90, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+ 
  scale_y_continuous(breaks=seq(0,6000,1000))+scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 


## (6) Number of casualties by Time of day
Time<-data %>%
  group_by(Casualty_Severity,Time) %>%
  summarise(total=n())

ggplot(Time) +
  geom_bar(mapping =aes(x=Time, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Time",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=0, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+ 
  scale_y_continuous(breaks=seq(0,6000,1000))+scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 


## (7) Number of casualties by Gender
Sex_of_Casualty<-data %>%
  group_by(Casualty_Severity,Sex_of_Casualty) %>%
  summarise(total=n())

ggplot(Sex_of_Casualty) +
  geom_bar(mapping =aes(x=Sex_of_Casualty, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Sex of Casualty",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=0, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+ 
  scale_y_continuous(breaks=seq(0,6000,1000))+scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 


## (8)  Number of casualties by month
Date<-data %>%
  group_by(Casualty_Severity,Date) %>%
  summarise(total=n())

ggplot(Date) +
  geom_bar(mapping =aes(x=Date, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Month",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=0, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+ 
  scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 


# (9)  Number of casualties by area
Urban_or_Rural_Area<-data %>%
  group_by(Casualty_Severity,Urban_or_Rural_Area) %>%
  summarise(total=n())

ggplot(Urban_or_Rural_Area) +
  geom_bar(mapping =aes(x=Urban_or_Rural_Area, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Urban or Rural Area",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=0, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+ 
  scale_y_continuous(breaks=seq(0,6000,1000))+scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 


## (10) Number of casualties by journey purpose of cyclist
Journey_Purpose_of_Driver <-data %>%
  group_by(Casualty_Severity,Journey_Purpose_of_Driver) %>%
  summarise(total=n())

ggplot(Journey_Purpose_of_Driver) +
  geom_bar(mapping =aes(x=Journey_Purpose_of_Driver, y=total,fill=Casualty_Severity),stat = "identity", position=position_dodge())+
  labs(x="Journey purpose of cyclist",y="Number of Casualties") +theme_light()+
  theme(axis.text.x = element_text(angle=0, vjust = 1.0),panel.border = element_rect(colour="black",fill=NA))+ 
  scale_y_continuous(breaks=seq(0,6000,1000))+scale_fill_manual(values=c("#F7DC6F", "#CD6155" )) 

