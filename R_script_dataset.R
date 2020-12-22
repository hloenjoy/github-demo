
#duration of months per user#
#organic and social compaigns#
str(dataset)
head(dataset,n=10)
rowSums(is.na(dataset)) 

hist(dataset$lead_score_combined)
summary(dataset)

#total label printed
#activation_type -- sales assisted or not#
#owner_id - id of the sales rep#

#order by date, and UserID#
install.packages('dplyr')
library(dplyr)
attach(dataset)
x<-dataset %>% 
  arrange(user_id,purchase_month)

#filter(purchase_month == min(purchase_month))
#revenue generated per user
#revenue generated per duration of months
#paid vs social breakdown
#view of organic vs social by monthly breakdown and total active users#
total_rev<-x %>%
  group_by(channel, purchase_month) %>%
  summarize(count =  n(), 
  mean_size_mm = mean(total_revenue, na.rm = TRUE), 
  sd_size_mm = sd(total_revenue, na.rm = TRUE), 
  median_size_mm = median(total_revenue, na.rm = TRUE))

install.packages('ggplot2')
library('ggplot2')
ggplot(total_rev, aes(x = purchase_month, y = mean_size_mm)) +geom_col() 

+facet_wrap(~site)
  
#filter(total_user==1)%>%
x$channel <- factor(x$channel)
p <- ggplot(x) + aes(user_id, total_revenue) 
p + geom_point(aes(colour=channel)) + facet_wrap( ~ channel)


#how long do users stay on and revenue generated per month.

active_users<- x%>% filter(x$total_user ==1)

destinations <- group_by(active_users, purchase_month, channel)
delay2 <- 
summarize(destinations, count = n(), 
rev = sum(total_revenue, na.rm = T), 
labels = sum(total_label, na.rm = T))

str(delay2)
delay2$purchase_month <- as.Date(delay2$purchase_month, format = "%YYYY/%mm/%dd")

install.packages('scales')
library('scales')
library('ggplot2')
ggplot(data=delay2, aes(x=purchase_month, y=labels, fill=channel)) +
geom_bar(stat="identity", position=position_dodge())+
scale_x_date(date_labels="%b %y",date_breaks  ="1 month")+
geom_text(aes(label = labels),size=4)+
scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))

#geom_text(aes(label=rev), vjust=1.6, color="white",
#position = position_dodge(0.9), size=3.5)+
#scale_fill_brewer(palette="Paired")+
#theme_minimal()
p <- ggplot(delay2) + aes(purchase_month, rev) 
p + geom_line()

library('dplyr')
xa <- group_by(active_users, user_id, channel)
fa <- 
summarize(xa, count = n(), 
rev = sum(total_revenue, na.rm = T), 
labels = sum(total_label, na.rm = T))
organic<-fa %>% arrange(desc(rev))
write.csv(organic,'organic.csv')


#paid channel only#
paid_channel<-x%>% filter(x$channel =='Paid')

#group by user 
paid_channel %>% 
  arrange(user_id,purchase_month)

#total unique users#
#paid users first month revenue compared to organic first month revenue

#user signs up per month.....vs organic#

a <- group_by(x, purchase_month, channel,user_id)

summarize(a, rev = sum(signup, na.rm = T))

filter(n == 1)

Data <- select(a, purchase_month,user_id, signup,new_revenue,new_label, ownerid)
XX<-Data %>% group_by(channel,ownerid) %>% filter(signup==1)%>%filter(channel=='Paid')%>%
  summarize(signups = sum(signup, na.rm = TRUE), new_revenue = sum(new_revenue),
            new_label = sum(new_label))

p <- ggplot(XX) + aes(purchase_month, signups) 
p + geom_line()

ggplot(data=XX, aes(x=purchase_month, y=new_revenue, fill=channel)) +
geom_line(stat="identity", position=position_dodge())

scale_x_date(date_labels="%b %y",date_breaks  ="1 month")+
geom_text(aes(label = labels),size=4))

XX$rev_signup<- XX$new_revenue/XX$signups

ggplot(data=XX, aes(x=purchase_month, y=new_revenue, fill=channel)) +
  geom_line(aes(color=channel))+
  geom_point(aes(color=channel))+
  geom_text(aes(label = new_revenue),size=4)+
  labs(title="New Revenue by Channel",x="Purchase Month", y = "New Revenue")

ggplot(data=XX, aes(x=purchase_month, y=new_label, fill=channel)) +
  geom_line(aes(color=channel))+
  geom_point(aes(color=channel))+
  geom_text(aes(label = new_label),size=4)+
  labs(title="New Labels by Channel",x="Purchase Month", y = "New Labels")


# Change line types
ggplot(data=XX, aes(x=purchase_month, y=signups, fill=channel)) +
  geom_line(aes(color=channel))+
  geom_point(aes(color=channel))+
  geom_text(aes(label = signups),size=4)+
  labs(title="Signups by Channel",x="Purchase Month", y = "Number of Signups")

XX$rev_signup<-round(XX$rev_signup,2)
ggplot(data=XX, aes(x=purchase_month, y=rev_signup, fill=channel)) +
  geom_line(aes(color=channel))+
  geom_point(aes(color=channel))+
  geom_text(aes(label = rev_signup),size=4)+
  labs(title="New Rev/Signup by Channel",x="Purchase Month", y = "New Rev Per Signup")

#scatterplot of Lead Score and Revenue/ User ID#
#plot a scatterplot of revenue and signups and by owner ID#

install.packages('plotly')
library('plotly')

p<-ggplot(XX, aes(x=signups, y=new_revenue,color=new_revenue)) +
  geom_point()+geom_text(aes(label = ownerid))+xlim(0,10)+ ylim(0,10)

ggplotly(p)

+xlim(0,300)+ ylim(0,300)

+ geom_text(label=(XX$ownerid))

#lead_scores#
#lets grab the user_ID, months, and lead_score and create a new dataframe#
new_data<-x%>% select (purchase_month, user_id, total_revenue,lead_score_combined)


#average revenue per user per month basis#

a <- group_by(new_data, user_id)
f<-summarize(a, rev = sum(total_revenue, na.rm = T),
avg_rev = mean(total_revenue, na.rm = T),
score = mean(lead_score_combined))


Test_sample<-sample_n(f,50000)

cor(f$rev,f$score,method="pearson",use="complete.obs")

p<-ggplot(Test_sample, aes(x=rev, y=score,color=rev)) +geom_point()
#geom_text(aes(label = user_id))
ggplotly(p)



attach(Test_sample)
cor(avg_rev,score,method="pearson",use="complete.obs")

