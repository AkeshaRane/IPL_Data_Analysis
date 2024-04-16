library(dplyr)
library(ggplot2)
setwd("C:/Users/akesh/OneDrive/Desktop/IPL")
matches_data=read.csv("matches.csv",header=TRUE)
deliveries=read.csv("deliveries.csv",header=TRUE)
'1:season with more matches'
ggplot(data=matches_data)+geom_bar(mapping=aes(season))+scale_x_continuous(breaks=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))
season_wise=group_by(matches_data,season)
match_count=summarise(season_wise,n())
ggplot(data=match_count)+geom_bar(mapping=aes(season,n()),stat='identity')
matches_data %>%
  group_by(season) %>% 
  summarise(match_count=n()) %>% 
  ggplot() +
  geom_bar(aes(season,match_count,fill=season),stat='identity')+
  scale_x_continuous(breaks=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))
http://127.0.0.1:15669/graphics/7fa011ce-e179-45e0-85a7-94e21805d5fb.png
'2:Barplot for team who won max matches'
ggplot(data=matches_data)+
  geom_bar(mapping=aes(winner))

win_team=matches_data %>% 
  group_by(winner) %>% 
  summarise(win_count=n())
ggplot(win_team)+
  geom_bar(aes(winner,win_count,fill=winner,rm.na=TRUE),stat='identity')+
  coord_flip()
'3:Number of matches played in different stadiums'
ggplot(matches_data)+
  
  geom_bar(aes(venue,rm.na=T,fill='#0072B2'))+
  coord_flip()
'4:Number of Matches won by each teams'
'same as second answer'
'5.Top Batsmen'
df=deliveries %>% 
  group_by(batsman) %>% 
  summarise(runs=sum(batsman_runs))
newdf=arrange(df,desc(runs))
df=head(newdf)
df %>% 
  ggplot(aes(batsman,runs,fill=batsman)) +
  geom_bar(stat='identity')+
  xlab('Batsman')+
  ylab('Batsman Runs')
'6:Total runs scored in each delivery of the over'
df=deliveries %>% 
  group_by(ball) %>% 
  summarise(runs=sum(total_runs))  %>%
  filter(ball<7)
df %>% 
  ggplot(aes(ball,runs,fill=ball)) +
  geom_bar(stat='identity')+
  xlab('Ball')+
  ylab('total Runs')                                                                                                                                              
'7:Average number of runs scored in each delivery of the over'

df=deliveries %>% 
  group_by(ball) %>% 
  summarise(runs=mean(total_runs))  %>%
  filter(ball<7)
df %>% 
  ggplot(aes(ball,runs,fill=ball)) +
  geom_bar(stat='identity')+
  xlab('Ball')+
  ylab('Average Runs') 
'8:Total number of runs scored in each over of the innings'

df=deliveries %>% 
  filter(is_super_over==0) %>%
  group_by(over) %>% 
  summarise(runs=sum(total_runs)) 

df %>% 
  ggplot(aes(over,runs,fill=over)) +
  geom_bar(stat='identity')+
  scale_x_continuous(breaks =1:20 )
xlab('over')+
  ylab('total Runs')  
'9:Total number of wickets in each over of the innings'
df=deliveries %>% 
  filter(is_super_over==0) %>%
  group_by(over) %>% 
  summarise(wickets=length(player_dismissed))
df %>% 
  ggplot(aes(over,wickets,fill=over)) +
  geom_bar(stat='identity')+
  scale_x_continuous(breaks =1:20 ) +
  xlab('over') +
  ylab('wickets')
matches_data %>% 
  group_by(season) %>% 
  summarise(match_cnt = n()) %>% 
  ggplot() + 
  geom_bar(aes(season,match_cnt, fill = season), stat = 'identity')

Barplot for team who won max matches
success_team = matches_data %>% 
  group_by(winner) %>% 
  summarise(winner_cnt = n())
ggplot(success_team) +
  geom_bar(aes(winner,winner_cnt, fill = winner), stat = 'identity') + coord_flip()

Number of matches played in different stadiums¶
ggplot(matches_data,aes(venue, rm.na=T)) +
  geom_bar(fill="#0072B2")

 Number of Matches won by each teams¶

ggplot(matches_data,aes(winner)) +
  geom_bar(fill="#0072B2")
