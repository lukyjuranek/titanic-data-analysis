load("./titanic_train.RDATA")
head(titanic.train)

if(!("gridExtra"%in%installed.packages()[,"Package"])){
  install.packages("gridExtra")
}
library(ggplot2)
library(gridExtra)

# Changes values from 0 and 1 to No and Yes
titanic.train$Survived <- ifelse(titanic.train$Survived == 1, "Yes", "No")


# Correlations

# Correlation coef of embarked and survived
embarked_num = as.numeric(titanic.train$Embarked)
survival_num = as.numeric(titanic.train$Survived)
print(cor(embarked_num,survival_num))

# Correlation coef of sex and survived
sex_num = as.numeric(titanic.train$Sex)
print(cor(sex_num,survival_num))

# Correlation coef of pclass and survived
pclass_num = as.numeric(titanic.train$Pclass)
print(cor(pclass_num,survival_num))




# Variables characteristics

# Survived
print(prop.table(table(titanic.train$Survived))*100)

# Pclass
summary(titanic.train$Pclass)
prop.table(table(titanic.train$Pclass))*100
# One bar to represent the three classes
ggplot(titanic.train)+
  aes(x = Pclass, fill=Pclass) +
  labs(title="Number of people in each class", x="Class", y="People")+
  geom_bar(color="#ffffff", alpha=0.6, position="dodge")+ 
  scale_fill_manual(values=c("gold", "gold3", "gold4"))+
  theme(legend.position = "none")

# Sex
print(table(titanic.train$Sex))
print(prop.table(table(titanic.train$Sex))*100)

# Age
summary(titanic.train$Age)
# Histogram of age
ggplot(titanic.train)+
  aes(x = Age) +
  labs(title="Age distribution")+
  geom_boxplot()+
  scale_fill_manual(values=c("#818589"))

# SibSp
summary(titanic.train$SibSp)
# Histogram of SibSp
ggplot(titanic.train)+
  aes(x = SibSp) +
  labs(title="Number of siblings and spouses", x="Siblings and spouses", y="People")+
  geom_bar(color="#ffffff", fill="blue4")

# Parch
summary(titanic.train$Parch)
# Histogram of Parch
ggplot(titanic.train)+
  aes(x = Parch, fill=Survived) +
  labs(title="Number of parents and children", x="Parents and children", y="People")+
  geom_bar(color="#ffffff", fill="pink4")

# Ticket
print(table(titanic.train$Ticket))

# Fare
summary(titanic.train$Fare)
# Histogram of Fare
ggplot(titanic.train)+
  aes(x = Fare) +
  labs(title="Fare distribution", x="Fare", y="People")+
  geom_histogram(binwidth=3, color="#ffffff", fill="gold3")+
  scale_y_log10()

# Cabin
print(table(titanic.train$Cabin))
passangersWithCabin <- titanic.train[!(titanic.train$Cabin == ""), ]
passangersWithCabin$Cabin = substr(passangersWithCabin$Cabin,1,1)
print(table(passangersWithCabin$Cabin))

ggplot(passangersWithCabin)+
  aes(x = Cabin)+
  labs(title="Cabin location distribution", x="Cabin location", y="Count")+
  geom_bar()

# Embarked
print(table(titanic.train$Embarked))



# 1 Survival rate of men vs women vs age
# How many men and women traveled
print(table(titanic.train$Sex))
print(prop.table(table(titanic.train$Sex))*100)

males <- titanic.train[titanic.train$Sex=="male", ]
print(prop.table(table(males$Sex, males$Survived))*100)

females <- titanic.train[titanic.train$Sex=="female", ]
print(prop.table(table(females$Sex, females$Survived))*100)


# Correlation coef of sex and survived
sex_num = as.numeric(titanic.train$Sex)
print(cor(sex_num,survival_num))

# Two histograms of men survived and women survived on top of each other using transparent colors
ggplot(titanic.train[titanic.train$Survived == "Yes", ])+
  aes(x = Age, fill=Sex)+
  labs(title="Survival of men vs women and their age", x="Age", y="Survived")+
  geom_histogram(binwidth=1, color="#ffffff", alpha=0.6, position = 'identity')+
  scale_fill_manual(values=c("#ff304f", "#28c7fa"))

# Two graphs of men and women survival in percentages
ggplot(titanic.train)+
  aes(x = Age, fill=Survived) +
  labs(title="Survival rate of men vs women", x="Age", y="Survived")+
  geom_histogram(binwidth=5, color="white", position="fill")+
  scale_fill_manual(values=c("grey40", "springgreen"))+
  facet_wrap(~Sex)

# Histogram Survived x Age
ggplot(titanic.train[titanic.train$Sex =="female", ]) +
  aes(x = Age, fill=Survived) +
  labs(title = "Survival rate based on age", x="Age", y="People %")+
  geom_histogram(binwidth=4, color="#ffffff", position="fill")+
  scale_fill_manual(values=c("grey40", "springgreen"))

# 2 Survival x Fare price
# Scatterplot
ggplot(titanic.train, aes(x=Age, y=Fare, color=Survived)) + 
  geom_point(size=2)+
  scale_y_log10()+
  scale_color_manual(values=c("#818589", "green"))

# Boxplot fare price
ggplot(titanic.train) +
  aes(x=Survived, y=Fare, fill=Survived) +
  labs(title="Survival rate based on the fare price")+
  geom_boxplot()+
  scale_y_log10()+
  scale_fill_manual(values=c("#818589", "springgreen"))+
  theme(legend.position = "none")

ggplot(titanic.train, aes(x=Sex, y=Fare, fill = Survived)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#818589", "springgreen"))+
  facet_wrap(.~Embarked)


# 3 Survival x class in %
p1 <- ggplot(titanic.train)+
  aes(x = Pclass, fill=Survived) +
  labs(title="Survival of classes in %", x="Class", y="Survived %")+
  geom_bar(color="#ffffff", alpha=0.6, position="fill")+
  scale_fill_manual(values=c("#818589", "gold"))

# 3 Survival x class
p2 <- ggplot(titanic.train)+
  aes(x = Pclass, fill=Survived) +
  labs(title="Survival of classes", x="Class", y="Survived")+
  geom_bar(bcolor="#ffffff", alpha=0.6, position="dodge")+
  scale_fill_manual(values=c("#818589", "gold"))
#Combines the two graphs
p <- grid.arrange(p2, p1, ncol = 1)

# Three scatter plots showing all calsess and age x fare and wheter they survived
ggplot(titanic.train, aes(x=Fare, y=Age, color=Survived))+
  geom_point(size=2)+
  scale_color_manual(values=c("black", "gold"))+
  scale_x_log10()+ facet_wrap(.~Pclass)

# Age x Fare x Class
#ggplot(titanic.train, aes(x=Fare, y=Age, color=Pclass))+ 
#  geom_point(size=2)+
#  scale_color_manual(values=c("gold", "black", "gray"))+
#  scale_x_log10()

# 4 Embarkation x Survival
ggplot(titanic.train)+
  aes(x = Embarked, fill=Survived) +
  labs(title="Survival based on the place of embarkation", x="Embarked", y="Survived")+
  geom_bar(alpha=0.6, position="fill")+
  scale_x_discrete(labels=c("Cherbourg", "Queenstown", "Southampton"))+
  scale_fill_manual(values=c("#818589", "blue"))

# Pie chart of embarkation counts
# Create a summary of counts for each embarkation gate
summary_data <- prop.table(table(titanic.train$Embarked))*100
summary_data <- summary_data[2:4]
print(summary_data)

# Create a data frame for the pie chart
pie_data <- data.frame(
  Embarked = names(summary_data),
  Count = as.numeric(summary_data)
)

# Create a pie chart
ggplot(pie_data, aes(x = Count, y = "", fill = Embarked))+
  geom_bar(stat = "identity", width = 1)+
  #coord_polar(theta="x", start=0)+
  labs(title = "Embarkation(Port)", fill = "Port", x="", y="")+
  scale_fill_manual(values=c("gold", "brown", "royalblue"), labels=c("Cherbourg", "Queenstown", "Southampton"))+
  geom_text(
    aes(label = paste(Embarked, sprintf("%.1f%%", Count), sep = "\n")),
    color = "black",
    position = position_stack(vjust = 0.5))+
  theme_void()

# Plclass x embarked
ggplot(titanic.train)+
  aes(x = Embarked, fill=Pclass) +
  labs(title="Passanger class and embarkation", x="Embarked", y="Count %")+
  geom_bar(position="fill")+
  scale_x_discrete(labels=c("Cherbourg", "Queenstown", "Southampton"))+
  scale_fill_manual(values=c("gold", "royalblue", "grey25"))


# 5 Cabin location(cabin letter) x Survived
# Takes the cabin number and only keeps the letter andonly people with a cabin
passangersWithCabin <- titanic.train[!(titanic.train$Cabin == ""), ]
passangersWithCabin$Cabin = substr(passangersWithCabin$Cabin,1,1)
# Bar chart with cabin numbers
ggplot(passangersWithCabin)+
  aes(x = Cabin, fill=Survived) +
  labs(title="Survival based on cabin location", x="Cabin location", y="Survival %")+
  geom_bar(alpha=0.6, position="fill", stat="count")+
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_fill(vjust=0.5))+
  scale_fill_manual(values=c("#818589", "purple"))+
  coord_flip()+
  scale_x_discrete(limits=rev)

# Barchart comparing the survival of poeple with and wihtout a cabin number
# Categorize passengers with and without cabins
titanic.train$CabinStatus <- ifelse(titanic.train$Cabin=="", "No Cabin", "With Cabin")
ggplot(titanic.train)+
  aes(x = CabinStatus, fill=Survived) +
  labs(title="Survival of people with and without cabin in %", y="Survived %")+
  geom_bar(color="#ffffff", alpha=0.6, position="fill")+
  scale_fill_manual(values=c("#818589", "brown4"))


# 6 Survival x Number of relatives on board
# Siblings and spouses
p3 <- ggplot(titanic.train)+
  aes(x = SibSp, fill=Survived) +
  labs(title="Survival of passangers with sibling or spouses onboard", x="Sibling or spouses", y="Survived %")+
  geom_bar(color="#ffffff", alpha=0.6)+
  scale_fill_manual(values=c("#818589", "firebrick3"))

# Parents or children 
p4 <- ggplot(titanic.train)+
  aes(x = Parch, fill=Survived) +
  labs(title="Survival of passangers with parents or children onboard", x="Parents or children", y="Survived %")+
  geom_bar(color="#ffffff", alpha=0.6)+
  scale_fill_manual(values=c("#818589", "darkorange"))

#Combines the two graphs
p <- grid.arrange(p3, p4, ncol = 1)

# All relativees together
relatives = titanic.train$SibSp + titanic.train$Parch
ggplot(titanic.train) + aes(x = relatives, fill = Survived ) +
  labs(x="Number of Relatives", y="Percentage")+
  geom_bar()+
  scale_x_discrete()+
  scale_fill_manual(values=c("#818589", "darkorange"))





# Other graphs
ggplot(titanic.train)+
  aes(x=Age, y=Fare, color=Survived) +
  geom_point()+
  facet_wrap(.~Pclass)+
  scale_color_manual(values=c("#818589", "green"))+
  scale_y_log10()

aux = titanic.train$Parch == 0 & titanic.train$SibSp == 0
sum(aux)
travels_alone = rep("No",length(aux))
travels_alone[aux] = "Yes"
titanic.train = cbind(titanic.train, travels_alone)

