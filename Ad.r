library(ggplot2)

data=read.csv("AdFX-Winter 2016.csv")
data$gender=as.factor(data$gender)
data$Treatment=factor(data$Treatment,labels = c("control","treatment"))
prop.table(table(data$gender,data$Treatment),margin = 2)
table(data$gender,data$Treatment)
c=table(data$Treatment)
prop.test(table(data$gender,data$Treatment))
prop.test(table(data$Treatment,data$gender))

ggplot(data,aes(gender))+geom_bar(aes(fill=Treatment),position = "dodge")

aggregate(data$past_sales~data$Treatment, FUN = mean)
aggregate(data$past_sales~data$Treatment, FUN = sd)
t.test(formula = data$past_sales~data$Treatment)

ggplot(data,aes(past_sales))+geom_bar(aes(fill=Treatment),position = "dodge",binwidth = 1)+
  xlim(c(0,25))
ggplot(data,aes(past_sales))+geom_bar(aes(fill=Treatment),position = "dodge",binwidth = 1)+
  xlim(c(1,35))

data$saw_ads = factor(data$saw_ads, labels = c("notsaw","saw"))
data_t = subset(data, Treatment=="treatment")
data_c = subset(data, Treatment=="control")
t.test(formula = sales~saw_ads, data = data_t)
aggregate(data_t$sales~data_t$saw_ads, FUN = mean)
aggregate(data_t$sales~data_t$saw_ads, FUN = sd)
summary(data_t$saw_ads)

aggregate(data$sales~data$Treatment, FUN = mean)
aggregate(data$sales~data$Treatment, FUN = sd)
t.test(data$sales~data$Treatment)

data_sawads = subset(data, saw_ads == "saw")
table(data$Treatment,data$saw_ads)
aggregate(data_sawads$sales~data_sawads$Treatment, FUN = mean)
aggregate(data_sawads$sales~data_sawads$Treatment, FUN = sd)
t.test(data_sawads$sales~data_sawads$Treatment)
t.test(data_sawads$sales~data_sawads$Treatment,conf.level = 0.90)

data_sawads_f = subset(data_sawads, gender == "female")
table(data_sawads$Treatment,data_sawads$gender)
aggregate(data_sawads_f$sales~data_sawads_f$Treatment, FUN = mean)
aggregate(data_sawads_f$sales~data_sawads_f$Treatment, FUN = sd)
t.test(data_sawads_f$sales~data_sawads_f$Treatment)
t.test(data_sawads_f$sales~data_sawads_f$Treatment,conf.level = 0.90)

data_sawads_m = subset(data_sawads, gender == "male")
table(data_sawads$Treatment,data_sawads$gender)
aggregate(data_sawads_m$sales~data_sawads_m$Treatment, FUN = mean)
aggregate(data_sawads_m$sales~data_sawads_m$Treatment, FUN = sd)
t.test(data_sawads_m$sales~data_sawads_m$Treatment)
t.test(data_sawads_m$sales~data_sawads_m$Treatment,conf.level = 0.90)
