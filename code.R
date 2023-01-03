install.packages("tidyverse")
install.packages("scales")
require(tidyverse)
require(scales)
require(stringr)

collegeData <- read_csv("all-ages.csv")
collegeWomenData <-read_csv("women-stem.csv")
head(collegeData)
str(collegeData)


                #Number students

##Number of students per major category     #Melhorar labels  #V
collegeData %>%
  group_by(Major_category) %>% 
  summarize(Total = sum(Total)) %>% 
  ggplot(aes(x = reorder(Major_category, Total), y = Total, label = Total)) +
  geom_col(fill = "darkblue") +
  geom_text(hjust = 0, nudge_y = 100000, fontface="bold") +
  labs(title= "Number of students per Major Category") +
  ylab("Total of students") + xlab("Major category") +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot") + 
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .25))) +
  coord_flip()
  

#Top 15 majors with more students 
collegeData %>%
  arrange(desc(Total)) %>% 
  slice(1:15) %>% 
  ggplot() +
  geom_col(mapping=aes(x=Total, y= reorder(Major, Total)), fill="darkblue")+
  labs(title= "Top 15 majors with more students")+ xlab("Total of students") + ylab("Major category")+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(labels = comma)+
  geom_text(aes(x = Total, y = reorder(Major, Total), label= format(Total,digits=5)), color="white",fontface="bold",hjust = 1.1)

#Top 15 majors with less students     
collegeData %>%
  arrange(Total) %>% 
  slice(1:15) %>% 
  ggplot() +
  geom_col(mapping=aes(x=Total, y= reorder(Major, Total)),fill="darkblue")+
  labs(title= "Top 15 majors with less students")+ xlab("Total of students") + ylab("Major category")+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(labels = comma)+
  geom_text(aes(x = Total, y = reorder(Major, Total), label= format(Total,digits=5)), color="white",fontface="bold",hjust = 1.1)


                                #Women

#Percentage of female by major STEM category
collegeWomenData %>% 
  group_by(Major_category) %>%
  summarize(ShareWomen=mean(ShareWomen)) %>% 
  ggplot()+
  geom_col(mapping=aes(x=ShareWomen, y= reorder(Major_category, ShareWomen)),fill="darkblue")+
  labs(title= "Percentage of females by STEM category")+ xlab("Percentage of females") + ylab("Category")+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(labels=scales::percent_format())+
  geom_text(aes(x = ShareWomen, y = reorder(Major_category, ShareWomen), label= paste0(format(ShareWomen*100,digits=3), "%")), color="white",fontface="bold",hjust = 1.1)


#Top 15 majors with least females
collegeWomenData %>% 
  arrange(ShareWomen) %>% 
  mutate(Major = str_to_title(Major)) %>% 
  slice(1:15) %>% 
  ggplot(aes(x = reorder(Major, ShareWomen), y = ShareWomen,
             label= paste0(format(ShareWomen*100,digits=3), "%"))) +
  geom_col(fill = "darkblue") +
  geom_text(hjust = 1.3, nudge_y = .01, color="white", fontface="bold") + 
  labs(title= "Top 15 STEM majors with the lowest female percentage") + 
  ylab("Percentage of females") + xlab("Major") +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot")+
  scale_y_continuous(labels = percent_format(),expand = expansion(mult = c(0, .3))) +
  coord_flip()

#Top 15 majors with most females
collegeWomenData %>% 
  arrange(desc(ShareWomen)) %>% 
  mutate(Major = str_to_title(Major)) %>% 
  slice(1:15) %>% 
  ggplot(aes(x = reorder(Major, ShareWomen), y = ShareWomen,
             label= paste0(format(ShareWomen*100,digits=3), "%"))) +
  geom_col(fill = "darkblue") +
  geom_text(hjust = 1.3, nudge_y = .01, color="white", fontface="bold") + 
  labs(title= "Top 15 STEM majors with the lowest female percentage") + 
  ylab("Percentage of females") + xlab("Major") +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot")+
  scale_y_continuous(labels = percent_format(),expand = expansion(mult = c(0, .3))) +
  coord_flip()
  
                        #Unemployment rate

#Unemployment rate per major category   
collegeData %>%
  group_by(Major_category) %>% 
  summarise(average_rate= mean(Unemployment_rate)) %>% 
  ggplot()+
  geom_col(mapping=aes(x=average_rate, y=reorder(Major_category, average_rate)), fill="darkblue")+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),plot.title = element_text(hjust = 0.5))+
  labs(title= "Unemployment rate per major category") + xlab("Unemployment rate")+ ylab("Major category")+
  scale_x_continuous(labels=scales::percent_format())+
  geom_text(aes(x = average_rate, y = reorder(Major_category, average_rate), label= paste0(format(average_rate*100,digits=3), "%")), color="white",fontface="bold",hjust = 1.1)
  

#Top 15 major with biggest Unemployment rate
collegeData %>%
  group_by(Major) %>%   
  summarise(average_rate= mean(Unemployment_rate)) %>%
  arrange(desc(average_rate)) %>%
  slice(1:15) %>% 
  ggplot()+
  geom_col(mapping=aes(x=average_rate, y=reorder(Major, average_rate)), fill="darkblue")+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)) , axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),plot.title = element_text(hjust = 0.5))+
  labs(title= "Top 15 majors with the biggest unemployment rate") + xlab("Unemployment rate")+ ylab("Major")+
  scale_x_continuous(labels=scales::percent_format())+
  geom_text(aes(x = average_rate, y = reorder(Major, average_rate), label= paste0(format(average_rate*100,digits=3), "%")), color="white",fontface="bold",hjust = 1.1)

#Top 15 major with smallest Unemployment rate
collegeData %>%
  group_by(Major) %>%   
  summarise(average_rate= mean(Unemployment_rate)) %>%
  arrange(average_rate) %>%
  slice(1:15) %>% 
  ggplot()+
  geom_col(mapping=aes(x=average_rate, y=reorder(Major, average_rate)), fill="darkblue")+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)) , axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),plot.title = element_text(hjust = 0.5))+
  labs(title= "Top 15 majors with the smallest unemployment rate") + xlab("Unemployment rate")+ ylab("Major")+
  scale_x_continuous(labels=scales::percent_format())+
  geom_text(aes(x = average_rate, y = reorder(Major, average_rate), label= paste0(format(average_rate*100,digits=3), "%")), color="white",fontface="bold",hjust = 1.1)


                #Salary

#Salary per major category   
collegeData %>%
  group_by(Major_category) %>% 
  summarise(average_salary= mean(Median)) %>% 
  ggplot()+
  geom_col(mapping=aes(x=average_salary, y=reorder(Major_category, average_salary)), fill="darkblue")+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)) , axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),plot.title = element_text(hjust = 0.5))+
  labs(title= "Average salary per major category") + xlab("Salary")+ ylab("Major category") + scale_x_continuous(labels=scales::dollar_format())+
  geom_text(aes(x = average_salary, y = reorder(Major_category, average_salary), label= paste0("$", format(average_salary,digits=5))), color="white",fontface="bold",hjust = 1.1)

#Top15 worst paying majors
collegeData %>%
  arrange(Median) %>% 
  slice(1:15) %>% 
  ggplot()+
  geom_col(mapping=aes(x=Median, y=reorder(Major, Median)), fill="darkblue")+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)) , axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),plot.title = element_text(hjust = 0.5))+
  labs(title= "Top 15 major with lowest average salaries") + xlab("Salary")+ ylab("Major") + scale_x_continuous(labels=scales::dollar_format())+
  geom_text(aes(x = Median, y = reorder(Major, Median), label= paste0("$",Median)), color="white",fontface="bold",hjust = 1.1)


#Top 15 best paying majors
collegeData %>%
  arrange(desc(Median)) %>% 
  slice(1:15) %>% 
  ggplot()+
  geom_col(mapping=aes(x=Median, y=reorder(Major, Median)), fill="darkblue")+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)) , axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),plot.title = element_text(hjust = 0.5),plot.title.position = "plot")+
  labs(title= "Top 15 major with highest average salaries") + xlab("Salary")+ ylab("Major") +scale_x_continuous(labels=scales::dollar_format())+
  geom_text(aes(x = Median, y = reorder(Major, Median), label= paste0("$",Median)), color="white",fontface="bold",hjust = 1.1)






