#Satindra Bahadur Khadka

#NPI00178



#install necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("crayon")
install.packages("readr")
install.packages("na.tools")
library(na.tools)
library(crayon)
library(ggplot2)
library(dplyr)

#Read employee_attrition.csv and set as dataset variable
library(readr)
employee_details <- read.csv("C:/Users/sangi/Downloads/employee_attrition.csv")

#DataPre-Processing
#view dataset
View(employee_details)

#structure 
str(employee_details)

#number of rows and columns
dim(employee_details)

#column Names
names(employee_details)

#summary of the data
summary(employee_details)


#column removed
##gender_short
employee_details$gender_short <- NULL


# Transformation: Convert "STATUS_YEAR" to a factor variable
data$STATUS_YEAR <- as.factor(data$STATUS_YEAR)

# Transformation: Create a new variable "service_length_group" based on length_of_service
data$service_length_group <- cut(data$length_of_service, breaks = c(0, 5, 10, 15, 20, Inf),
                                 labels = c("0-5", "6-10", "11-15", "16-20", "20+"))

# Transformation: Recode "STATUS" to a binary variable (Terminated or Not Terminated)
data$STATUS <- ifelse(data$STATUS == "TERMINATED", "Terminated", "Not Terminated")


# Replace NA values with median values
employee_details$terminationdate_key[is.na(employee_details$terminationdate_key)] <- median(employee_details$terminationdate_key, na.rm = TRUE)
employee_details$STATUS_YEAR[is.na(employee_details$STATUS_YEAR)] <- median(employee_details$STATUS_YEAR, na.rm = TRUE)
employee_details$recorddate_key[is.na(employee_details$recorddate_key)] <- median(employee_details$recorddate_key, na.rm = TRUE)

# Remove rows with all NA values
employee_details <- employee_details[complete.cases(employee_details),]

#Analysis 1.1 Determine the relationship between a work position and termination
data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot(fill = "#FFC0CB")+
  aes(x = job_title, fill = termreason_desc)+ geom_bar()+coord_flip()+
  labs(title = "Relationship between job title and termination", 
       caption ="Satindra Bahadur Khadka")+
  theme_bw()


#Analysis 1.2 Investigate the relationship between gender and termination.
data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(x = termreason_desc)+ geom_bar(fill = "#008080")+
  labs(title = "Relationship between Gender and termination", 
       caption ="Satindra Bahadur Khadka")+
  theme_bw()+ facet_wrap(vars(gender_full))




#Analysis 1.3 Establish a connection between the business unit and the termination.
data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(x = termreason_desc)+ geom_bar(fill = "#DC143C")+
  labs(title = "Relationship between Status Business Unit and termination", 
       caption ="Satindra Bahadur Khadka")+
  theme_bw()+ facet_wrap(vars(BUSINESS_UNIT))


#Analysis 1.4 Determine the relationship between age and termination.
 
data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(x = age, fill = termreason_desc)+ geom_bar()+
  labs(title = "Relationship between age and attrition", caption ="Satindra Bahadur Khadka")+
  theme_bw()



#Analysis 1.5 Examine the relationship between duration of service and termination with respect to age.

data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(x = length_of_service, y= age , fill = termreason_desc)+ geom_tile()+
  labs(title = "Relationship between Length of Service with age and termination", 
       caption ="Satindra Bahadur Khadka")+
  theme_classic()


#Analysis 1.6 Identify the relationship  between department and termination
data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(x = department_name, fill = termreason_desc)+ geom_bar()+
  labs(title = "Relationship between department and termination", caption ="Satindra Bahadur Khadka")+
  coord_flip()+
  theme_classic()


#Analysis 2.1 Determine the relationship between status and year of status.
data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(x = termreason_desc, fill = STATUS )+ geom_bar(fill = "#800080")+
  theme_gray()+
  labs(title = "Relationship between STATUS year and Status", 
       caption ="Satindra Bahadur Khadka")+
  facet_wrap(vars(STATUS_YEAR))


#Analysis 2.2 Determine the relationship between status and termination.

data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(x = termreason_desc, fill = STATUS )+ geom_bar()+
  labs(title = "Relationship between STATUS and termination", 
       caption ="Satindra Bahadur Khadka")+
  theme_bw()

#Analysis 3.1 Examine the link between a status and city name. 
data %>%
  ggplot()+
  aes(x = city_name, fill = STATUS )+ geom_bar()+
  labs(title = "Relationship between City Name and Status", 
       caption ="Satindra Bahadur Khadka")+
  coord_flip()+
  theme_minimal()




#Analysis 3.2 Unearth the relationship between a city's name and its division organization.

data %>%
  ggplot()+
  aes(x = department_name, y = city_name )+ geom_point()+
  labs(title = "Relationship between Department Name and City Name", 
       caption ="Satindra Bahadur Khadka")+
  theme(text=element_text(size=10),axis.text.x = element_text(angle = 90,hjust = 1))


#Analysis 3.3 Determine the link between the store's name and the city name.

data %>%
  ggplot()+
  aes(x = store_name, y = city_name )+ geom_point(fill = "#FF0000")+
  theme_gray()+
  labs(title = "Relationship between Store Name and City Name", 
       caption ="Satindra Bahadur Khadka")+
  theme_bw()




#Analysis 4.1 Discover the correlation between age and the type of termination.
data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(y = age,fill = termtype_desc )+ geom_bar()+
  scale_fill_hue(direction = 1)+
  labs(title = "Relationship between Age and type of termination", 
       caption ="Satindra Bahadur Khadka")+ theme_bw()



#Analysis 4.2 Understand the relationship between the length of service and the method of termination.
data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(x = length_of_service, y = termtype_desc )+ geom_jitter(fill = "#00FFFF")+
  labs(title = "Relationship between length of service and type of termination", 
       caption ="Satindra Bahadur Khadka")+ theme_bw()+
  facet_wrap(vars(termtype_desc))



#Analysis 4.3 Discover the correlation between department and the type of termination.
data %>%
  filter(!(termreason_desc %in% "Not Applicable")) %>% 
  ggplot()+
  aes(y = department_name )+ geom_bar(fill = "#000080")+
  labs(title = "Relationship between Department name and type of termination", 
       caption ="Satindra Bahadur Khadka")+ theme_bw()+
  facet_wrap(vars(termtype_desc))










