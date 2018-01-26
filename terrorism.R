

## NB I published this project on Kaggle as an Rmarkdown file with a lot of additional comments to the scripts.
## In case you are interest, you will find it here: 
## https://www.kaggle.com/emanueleamcappella/2001-a-terrorism-odyssey
## Data is from the Global Terrorism Database on Kaggle, a very interesting source: https://www.kaggle.com/START-UMD/gtd

# Install Packages
library (tidyverse)
library(dplyr)
library(reshape2)
library(devtools)
library("yarrr")
library(wesanderson)
library(wordcloud) # word-cloud generator
library(tm) # for text mining
library(SnowballC) # for text stemming
library(RColorBrewer) # for color palettes
library(forcats)
library(ggmap)

#set wd and load data
#setwd("C:/Users/EAMC/Desktop/VISUAL AN_coursework")
db <- read.csv("globalterrorismdb_0617dist_REDUCED.csv", na.string = c("", "NA", " ", "."))
#View(db)

### Pre-2001 vs post-2001
#cut iyear to theYear factor pre-post 2001 variable 
db$theYear<- cut(db$iyear, breaks= c(0, 2000, 2016), labels= c("Pre-2001", "From 2001")) 

db_theyear<- db %>% 
group_by(theYear) %>% 
summarise(killed = sum(nkill, na.rm=TRUE), injured = sum(nwound, na.rm=TRUE), count = n())%>% 
melt(id.vars="theYear", measure.vars=c('killed','injured','count')) 

#change default colours (palette) 
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")
#draw a bar chart
ggplot(db_theyear, aes(fill=variable, y=value, x=theYear)) + 
  geom_bar(position="dodge", stat="identity")+
  xlab("") + 
  scale_y_continuous(name="", limits=c(0, 360000), labels = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("The Year that changed everything?")+
  scale_fill_manual(values=cbbPalette)+
  labs(fill = "Type of Attack")


### Choropleth of attacks
#I then drew two choropleth maps to understand better the spatial distribution of terror in the two different periods. 
#Before 2001 terrorism was more widespread, and had a lower incidence

world <- map_data("world")

#MAP 1
#select your data: killed by country, pre-2001
country_death <- db %>% filter(theYear== 'Pre-2001')%>% 
group_by(country= country_txt) %>% summarise(killed = sum(nkill, na.rm=TRUE))

#Rename UsA and UK, else they will not be drawn in the map
country_death<- country_death %>% 
mutate(country_txt = fct_recode(country, "USA" = "United States", "UK"= "United Kingdom"))

#draw map1 with ggplot
ggplot(data= country_death) +
geom_map(data= world, map = world, aes(map_id = region), fill = 'white', color = "#7f7f7f", size = 0.25) + 
geom_map(map = world, aes(map_id = country_txt, fill = killed), size=0.25) + 
scale_fill_gradient(low="aliceblue", high="darkslategray", name="Terror Deaths") + 
expand_limits(x = world$long, y = world$lat) +
theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45))+
labs(x="", y="", title="Terrorism Deaths by Country - Pre 2001")

#MAP 2
#The world post-2001 is completely changed: not only the number of incidents has increased, but also it now concentrates in two major areas: 
#Middle East (Iraq mainly) and South Asia, with a peak in the Afghanistan/Pakistan area. 

#select your data: killed by country, post-2001
country_death <- db %>% filter(theYear== 'From 2001')%>% 
  group_by(country= country_txt) %>% 
  summarise(killed = sum(nkill, na.rm=TRUE))

#Rename UsA and UK, else they will not be drawn in the map
country_death<- country_death %>% 
  mutate(country_txt = fct_recode(country, "USA" = "United States", "UK"= "United Kingdom"))

#draw map 2 with ggplot
ggplot(data= country_death) +
geom_map(data= world, map = world, aes(map_id = region), fill = 'white', color = "#7f7f7f", size = 0.25) + 
geom_map(map = world, aes(map_id = country_txt, fill = killed), size=0.25) + 
scale_fill_gradient(low="aliceblue", high="darkslategray", name="Terror Deaths") + 
expand_limits(x = world$long, y = world$lat) +
theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45))+
labs(x="", y="", title="Terrorism Deaths by Country - Post 2001")


#METHOD OF ATTACK
#I also checked if the methods of the attacks changed after 2001.
#I created a bar-chart comparing attack types prior and post 2001. The major insight is the increasing relevance of bombing post-2001.  

type_att<- db%>% 
group_by(theYear, attacktype1_txt)%>%  
summarise(tot = n())

#Simple barchart
#showing that the number of attacks tripled from 2010 to 2014
ggplot(type_att, aes(x=attacktype1_txt, y=tot, fill=theYear)) +
geom_bar(stat="identity", position=position_dodge())+
scale_fill_manual(values=c('dodgerblue','gainsboro'))+
theme_minimal()+
theme(plot.title = element_text(hjust = 0.5))+
xlab("")+ ylab("")+ ggtitle('Type of attack by period')+ labs(fill = "Period")+
coord_flip()

#stacked barchart
#this graph shows that the 2010-2014 peak is mostly explained by the increase in bombing attacks.
#Though it is true that after 2001 terrorism - so to say - exploded, the world did not exceed the terrorism levels registered in the nineties until after 2010. 
#Thus, for almost ten years after 9/11 the level of worldwide terrorism was not unheard of, at least in terms of total attacks. 

ggplot(db, aes(x=iyear,fill=attacktype1_txt)) + geom_bar() + 
ggtitle("Terrorism by attack type, 1970-2016")+ theme_minimal()+
scale_fill_manual(values=c('dodgerblue', "darkslategray2", 'gold', 'gainsboro', 'cornsilk4', "cadetblue4", 'black',   "darkgreen", "chartreuse"))+ 
labs(x = "", y = "Number of Attacks")+ scale_x_continuous(name="", (breaks=seq(1970,2016,5)))+
theme(plot.title = element_text(hjust = 0.5))+
labs(fill = "Type of attack")

#Line chart comparing three distinct trends from 1970 to 2016: injured, dead and number of terrorist attacks
Worldwide<- db %>% 
group_by(iyear) %>% 
summarise(killed = sum(nkill, na.rm=TRUE), injured = sum(nwound, na.rm=TRUE), count = n())%>%
melt(id.vars="iyear", measure.vars=c('killed','injured','count')) 

ggplot(data= Worldwide, aes(iyear, value, group = variable, color=variable)) +
geom_line() + xlab("Years") + ylab("Number of episodes") +
ggtitle("Worldwide Terror Attacks Impact Over Years")+
scale_x_continuous(name="Years", (breaks=seq(1970,2016,3))) +
theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45))+
scale_color_discrete("Terror Acts")


## ZOOMING IN FROM THE WORLD TO NATIONS

#prepare database
db<- db%>% mutate (region_txt = fct_recode(region_txt, "Middle East"= "Middle East & North Africa" , "Oceania" = "Australasia & Oceania", 
"Central America"= "Central America & Caribbean", "South Africa"="Sub-Saharan Africa"))
dc<- db%>% group_by(country_txt, region_txt, nkill)%>%summarize(Total= n())
dc<- dc%>% group_by(region_txt)%>%summarize(Total= sum(Total))

#terrorist deaths by region - with a simple bar-chart
ggplot(dc, aes(x=region_txt)) +
geom_bar(aes(y= Total), stat="identity", fill="darkorange") +
ylab("")+
xlab("")+
theme(plot.title = element_text(hjust = 0.5))+
ggtitle("Death by Region")+
coord_flip()

#comparison of different continents
#prepare data with dplyr
year_cont<- db%>% 
group_by(iyear, region_txt) %>% 
summarise(killed = sum(nkill, na.rm=TRUE), count = n())          

#draw linechart comparing the longitudinal trend of every continent from 1970 to 2016
ggplot(year_cont, aes(x= iyear, y= killed, color=region_txt)) + 
geom_line() + 
xlab("Year") + 
ylab("Deaths by Acts of Terror") + 
scale_x_continuous(name="Years", (breaks=seq(1970,2016,10))) +
ggtitle("Continents time series")+
facet_wrap(~region_txt) +
theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45))+
scale_color_discrete("Region of interest")


#now let's draw a scatterplot comparing the countries of every continent
cont_death_var<- db%>% 
group_by(region_txt, country_txt) %>% 
summarise(killed = sum(nkill, na.rm=TRUE))%>% 
ggplot(cont_death_var, aes(x = region_txt, y = killed, color = region_txt)) +
geom_point() +  
ggtitle("Terrorist Death by Country") +
xlab("") +
ylab("") +
coord_flip()+
scale_color_discrete("Region of interest")+
theme(plot.title = element_text(hjust = 0.5))

#now a barplot of countries most affected by terrorism
country_death_var<- cont_death_var%>% 
arrange(desc(killed))%>%filter(killed>8000)%>% 
ggplot(country_death_var, aes(x= country_txt)) +
geom_bar(aes(y= killed), stat="identity", fill="darkslategray4")+
scale_x_discrete(limits= country_death_var$country_txt)+
ylab("Number of Deaths")+
xlab("")+
theme(plot.title = element_text(hjust = 0.5))+
ggtitle("Terrorist Deaths by nation")+
coord_flip()

#same barchart as before, but now weighted by population. How many people die by terrorism every 100.000? 
#Population data relates to 2017 statistics (this of course is a limitation)
weight <- read.csv("MyData.csv", na.string = c("", "NA", " ", "."))

weight<- weight%>% 
mutate (deathRate= killed/Tot.Population.2017*100000)%>%
arrange(desc(deathRate))%>%
ggplot(weight, aes(x= country_txt)) +
geom_bar(aes(y= deathRate), stat="identity", fill="darkslategrey")+
scale_x_discrete(limits= weight$country_txt)+
ylab("Death rate - by 100.000 population")+
xlab("")+
theme(plot.title = element_text(hjust = 0.5))+
ggtitle("Terrorism death weighted by population")+
coord_flip()


### TERRORIST GROUPS

#Data pre-processing
#open document
wrdcloud0 <- read.csv("globalterrorismdb_0617dist.csv", na.string = c("", "NA", " ", "."), stringsAsFactors = FALSE)
wrdcloud0$theYear<- cut(wrdcloud0$iyear, breaks= c(0, 2000, 2016), labels= c("Pre-2001", "From 2001")) 
wrdcloud <- wrdcloud0[, c("gname", "addnotes", "theYear", "nkill", "iyear")]

#collapse Al-Qaida littler groups (<10 attacks) into a single one
wrdcloudG<- wrdcloud%>% mutate (gname = fct_collapse(gname, Other_AlQaida_groups= 
c("Al-Qaida in Lebanon", "Al-Qaida Organization for Jihad in Sweden",
"Al-Qaida Network for Southwestern Khulna Division", "Al-Qaida Kurdish Battalions (AQKB)",
"Jadid Al-Qaida Bangladesh (JAQB)","Sympathizers of Al-Qaida Organization", 
"Secret Organization of al-Qaida in Europe", "Islambouli Brigades of al-Qaida"))) 

#calculate percentage of attacks by the single al-qaida groups
Alquaeda<- wrdcloudG%>%filter(grepl('Qaida', gname))%>%
group_by(gname)%>% summarise(total=n()) %>% 
mutate (pct= total/sum(total)*100) %>% 
arrange(desc(total))

#barplot to show Al-qaida groups distribution/high heterogeneity
ggplot(Alquaeda, aes(x=gname)) +
geom_bar(aes(y= pct), stat="identity", fill="steelblue") +
ylab("Relative Frequency")+
xlab("")+
ggtitle("Al-Qaida galaxy")+
theme(plot.title = element_text(hjust = 0.5))+
coord_flip()+
scale_x_discrete(limits= Alquaeda$gname)

#collapse all al-qaida groups into a single denomination
wrdcloudG<- wrdcloudG%>% mutate (gname = fct_collapse(gname, Al_Qaida= 
c("Al-Qaida in Saudi Arabia", "Al-Qaida in Yemen","Al-Qaida in the Indian Subcontinent", 
"Al-Qaida","Al-Qaida in the Islamic Maghreb (AQIM)", 
"Al-Qaida in Iraq", "Al-Qaida in the Arabian Peninsula (AQAP)", "Other_AlQaida_groups"))) 

#collapse taliban groups into a single denomination
wrdcloudG<- wrdcloudG%>% mutate (gname = fct_collapse(gname, Taliban= c("Taliban","Tehrik-i-Taliban Pakistan (TTP)","Punjabi Taliban", "Taliban (Pakistan)")))

# redraw the top terrorists chart, now that we have put together the different branches of the main organizations
top<- wrdcloudG%>%group_by(gname)%>%summarise(total=n())%>%arrange(desc(total))%>%
filter(total>1000, total<20000)%>%
ggplot(data= top, aes(x= gname)) +
geom_bar(aes(y= total), stat="identity", fill="goldenrod3")+
scale_x_discrete(limits= top$gname)+
ylab("Count")+
xlab("")+
theme(plot.title = element_text(hjust = 0.5))+
ggtitle("Top terrorist groups - over 1000 attacks ")+
coord_flip()


##DRAW WORDCLOUDS to visualize the most dangerous groups in terror history

#data preparation
#use acronym instead of full name (they are shorter), this will be useful for the wordclouds. use fct_recode for changing names
wrdcloudG<- wrdcloudG %>%
mutate(gname = fct_recode(gname,
"SL" = "Shining Path (SL)", "ISIL" = "Islamic State of Iraq and the Levant (ISIL)",
"FMLN" = "Farabundo Marti National Liberation Front (FMLN)","IRA" = "Irish Republican Army (IRA)",
"FARC" = "Revolutionary Armed Forces of Colombia (FARC)",
"NPA" = "New People's Army (NPA)", "PKK" = "Kurdistan Workers' Party (PKK)", "ETA" = "Basque Fatherland and Freedom (ETA)",
"CPI" = "Communist Party of India - Maoist (CPI-Maoist)", "LTTE" =  "Liberation Tigers of Tamil Eelam (LTTE)",
"ELN" = "National Liberation Army of Colombia (ELN)")) 

#delete unknown values and groups with less than 100 attacks
wrdcloudC<- wrdcloudG%>%group_by(gname)%>%summarise(total=n())%>%
arrange(desc(total))%>%filter(total<20000, total>500)
#create word cloud of top terrorist attackers
wordcloud(wrdcloudC$gname,wrdcloudC$total, scale=c(3,.4),min.freq=1, max.words=100, random.order=T, rot.per=.20, colors=brewer.pal(8,"Dark2"))

#now a second wordcloud with number of deaths as outcome
#delete unknown values and groups with less than 100 attacks
wrdcloudC1<- wrdcloudG%>%
group_by(gname) %>%
summarise(killed = sum(nkill, na.rm=TRUE))%>%
arrange(desc(killed))%>%filter(killed<100000, killed>1700)
#create word cloud
wordcloud(wrdcloudC1$gname,wrdcloudC1$killed, scale=c(3,.4),min.freq=1, max.words=100, random.order=T, rot.per=.25, colors=brewer.pal(8,"Dark2"))


#evolution of top 5 groups, in terms of deaths inflicted
groups_years <- wrdcloudG%>% filter (gname %in% c("Taliban", "Al_Qaida", "ISIL", "Boko Haram"))%>%  
group_by(iyear, gname)%>%summarize(killed = sum(nkill, na.rm=TRUE), count =n())%>%
ggplot(groups_years, aes(x= iyear, y= killed, color=gname)) + 
geom_line(size= 1.2) + 
xlab("Year") + 
ylab("Number of deaths") + 
scale_x_continuous(name="Years", (breaks=seq(1970,2016,3))) +
ggtitle("Top five terrorist groups")+
theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45))+
scale_color_discrete("Terrorist groups")



### How much is terrorism impacting the Western World (compared to the Middle East)?

#compare top western nation vs top middle-east nation (USA vs Iraq)
year_usa_afg_attacks <- db%>%
filter (country_txt %in% c("United States", "Iraq"))%>%
group_by(iyear,country_txt)%>%
summarize(killed= sum(nkill, na.rm=TRUE))%>%
ggplot(data= year_usa_afg_attacks, aes(x= iyear, y= killed, color=country_txt)) + 
geom_line()+ 
geom_point()+
xlab("Year") + 
ylab("Acts of Terror") + 
scale_x_continuous(name="Years", (breaks=seq(1970,2016,3))) +
ggtitle("Comparison USA vs Iraq")+
theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45))+
scale_color_discrete("Countries")



#CAR CRASH VS TERRORISM IN USA

#take data on USA car crash (from Wikipedia - admittedly another limitation)
traffic <- read.csv("USA_INCIDENTI_STRADALI.csv", header= TRUE, sep= ',', na.string = c("", "NA", " ", "."))
#delete strange final NA rows and 1993 value to match db and the year column
traffic<- traffic[-c(24, 48:51), -c(1)] 

usaDeath<- db %>%
filter (country_txt == "United States") %>%
group_by(iyear) %>% 
summarise(killed = sum(nkill, na.rm=TRUE))

#merge dataframes
usaDeath<- cbind(usaDeath, traffic) 
usaDeath<- usaDeath%>%
melt(id.vars="iyear", measure.vars=c('killed','car_crash_death'))

#plot data 
ggplot(usaDeath, aes(iyear, value, group = variable, color=variable)) +
geom_line() + xlab("Years") + ylab("Number of deaths") +
ggtitle("Car crash vs terrorist deaths")+
scale_x_continuous(name="Years", (breaks=seq(1970,2016,3))) +
theme(plot.title = element_text(hjust = 0.5), 
axis.text.x = element_text(angle=45))+
scale_color_discrete("Deaths")


  