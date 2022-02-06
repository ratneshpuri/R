library(tidyverse)

data <- read_excel("C:/Users/Ratnesh/Documents/Practice/transcation.xlsx")
view(data)

print(colnames(data))

#Checking NULL values in each rows
print("NULL values in each row are: ")
for(i in colnames(data)){
  print(sum(is.na(data[i])))
} #There are 37NULL values in pg, 16404 in sub_type and 338 in bank. So, ignore
  #sub_type

print(str(data))

#Checking categorical data
print("Checking mid,pmt,pg")
for(i in colnames(data)[3:5]){
  if(class(data[[i]])=="character"){
    print(paste0(i,": "))
    print(table(data[i]))
  }
}

#Plotting categorical data
ggplot(data=data, aes(x=mid, fill=mid ))+ geom_bar()+
  labs(title = "Merchant Identifier")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=data, aes(x=pmt, fill=pmt ))+ geom_bar()+
  labs(title = "Payment Method Used")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=data, aes(x=pg, fill=pg ))+ geom_bar()+
  labs(title = "Payment Gateway Used")+
  theme(axis.text.x = element_text(angle = 90))

#Also plot bank and check  it also
table(data$bank)

print(paste0("Maximum transcation done: ",max(data$t))) #3365
print(paste0("Minimum transcation done: ",min(data$t))) #1

data %>% filter(t==3365) -> t_max
print(t_max)

print(paste0("Maximum Successful transcations: ",max(data$success))) #2524
print(paste0("Minimum Successful transcation : ",min(data$success))) #0

#Ques1) Identify when the transcation started to fall
print(head(data$hr,10))
print(paste0("Minimum hr is: ",min(data$hr)))  #2020-02-12
print(paste0("Maximum hr is: ",max(data$hr))) #2020-02-14 23:00:00

data %>% group_by(hr) %>% 
  summarise(t=sum(t, na.rm = TRUE),
            success=sum(success, na.rm = TRUE)) -> data_group

trans_success <- mutate(data_group, success_rate=(success*100)/t)
head(trans_success)

#Plotting Success Rate with hr
ggplot(trans_success, aes(x=hr, y=success_rate))+ 
  geom_point()+
  labs(title = "HR Vs Success Rate")+
  theme(axis.text.x = element_text(angle = 90))

#From the plot we can make out that the fall in success rate started from 
# 2020-02-13 16 and lasted till 2020-02-13 20

# Ques2) Need to find out what caused the issue in that particular time frame

print(paste0("Total observations in the time range of fall are: ",sum(data$hr>="2020-02-13 16" & data$hr<="2020-02-13 20")))

data %>% filter(hr>="2020-02-13 16" & hr<="2020-02-13 20") -> trans_fall
head(trans_fall)

ggplot(data=data, aes(x=mid, fill=hr>="2020-02-13 16" & hr<="2020-02-13 20" ))+ geom_bar()+
  labs(title = "Merchant Identifier during fall")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=data, aes(x=pmt, fill=hr>="2020-02-13 16" & hr<="2020-02-13 20" ))+ geom_bar()+
  labs(title = "Payment Method Used during fall")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=data, aes(x=pg, fill=hr>="2020-02-13 16" & hr<="2020-02-13 20" ))+ geom_bar()+
  labs(title = "Payment Gateway Used during fall")+
  theme(axis.text.x = element_text(angle = 90))

  ggplot(data=trans_success, aes(x=success, y=success_rate, color=hr>="2020-02-13 16" & hr<="2020-02-13 20"))+ 
  geom_point()+
  labs(title = "success vs success_rate")+
  theme(axis.text.x = element_text(angle = 90))
  
  
#Plotting between original hr and t
  ggplot(data=data, aes(x=hr, y=t,group=mid,color=hr>="2020-02-13 16" & hr<="2020-02-13 20"))+ 
    geom_point()+
    labs(title = "Original hr vs Original t")+
    theme(axis.text.x = element_text(angle = 90))
  
