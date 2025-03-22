install.packages('readr')
install.packages('dplyr')
install.packages('tidyselect')
install.packages('ggplot2')
install.packages('tidyverse')
install.packages('haven')
install.packages("readxl")
install.packages('ggthemes')
install.packages('ggridges')
install.packages("viridis")
install.packages("mapview")
install.packages("sf")
install.packages("tidycensus")
install.packages("tigris")
install.packages("RColorBrewer")
install.packages('leaflet.extras2')
install.packages("htmlwidgets")
install.packages("webshot")
install.packages("reshape2")
library(readr)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(tidyverse)
library(haven)
library(readxl)
library(ggthemes)
library(ggridges)
library(viridis)
library(mapview)
library(sf)
library(tidycensus)
library(tigris)
library(RColorBrewer)
library(leaflet.extras2)
library(htmlwidgets)
library(webshot)
library(reshape2)


#-------------------------------------------------------------------------------

EAI<-read_xlsx("Economic Activity Index.xlsx")
FSI<-read_xlsx("Financial Stress Index.xlsx")

food_security_HH<-read_xlsx("Food Security, All Households.xlsx")
food_security_HH[which(food_security_HH$`Sub-subcategory`== "Female head, no spouse"),4]<-"Female head with children"
food_security_HH[which(food_security_HH$`Sub-subcategory`== "Male head, no spouse"),4]<-"Male head with children"
food_security_HH[c(608,638),4]<-"Household with no children"
food_security_HH$Category<-as.factor(food_security_HH$Category)
food_security_HH$Subcategory<-as.factor(food_security_HH$Subcategory)
food_security_HH$`Sub-subcategory`<-as.factor(food_security_HH$`Sub-subcategory`)
food_security_HH[food_security_HH$`Low food security-1,000`== "NA",10]<-NA
food_security_HH[food_security_HH$`Low food security-percent`== "NA",11]<-NA
food_security_HH[food_security_HH$`Very low food security-1,000`== "NA",12]<-NA
food_security_HH[food_security_HH$`Very low food security-percent`== "NA",13]<-NA
food_security_HH$`Low food security-1,000`<-as.numeric(food_security_HH$`Low food security-1,000`)
food_security_HH$`Low food security-percent`<-as.numeric(food_security_HH$`Low food security-percent`)
food_security_HH$`Very low food security-1,000`<-as.numeric(food_security_HH$`Very low food security-1,000`)
food_security_HH$`Very low food security-percent`<-as.numeric(food_security_HH$`Very low food security-percent`)

food_security_state<-read_xlsx("Food Security, by State.xlsx")
food_security_employ<-read_xlsx("Food Security, Employ and Disable.xlsx")
food_security_children<-read_xlsx("Food Security, HH with children.xlsx")
HH_pulse_survey<-read.delim("hpstimeseries.txt", sep="|")
HH_pulse_survey_state_1<-read.csv("Household Pulse Survey_State_1.csv")
HH_pulse_survey_state_63<-read.csv("Household Pulse Survey_State_63.csv")

#-------------------------------------------------------------------------------

#Writing a Wrong Data Visualisation
all_food_cpi<-read_xlsx("CPI Food.xlsx")
all_cpi<-read_xlsx("CPI All Items.xlsx")

all_food_level<-read_xlsx("CPI Level Food.xlsx")
all_cpi_level<-read_xlsx("CPI Level All.xlsx")

month_names<-colnames(all_food_cpi)[-1]

all_food_cpi<-all_food_cpi%>%
  pivot_longer(all_of(month_names),names_to = "month",values_to = "cpi_index") %>%
  mutate(Date=str_c(Year,'-',month,'-01'))
all_food_cpi<-cbind(all_food_cpi,item=rep("All Food"))
all_food_cpi$Date<-as.Date(all_food_cpi$Date,format="%Y-%m-%d")

all_cpi<-all_cpi%>%
  pivot_longer(all_of(month_names),names_to = "month",values_to = "cpi_index") %>%
  mutate(Date=str_c(Year,'-',month,'-01'))
all_cpi<-cbind(all_cpi,item=rep("All Items"))
all_cpi$Date<-as.Date(all_cpi$Date,format="%Y-%m-%d")

all_food_level<-all_food_level%>%
  pivot_longer(all_of(month_names),names_to = "month",values_to = "cpi_index") %>%
  mutate(Date=str_c(Year,'-',month,'-01'))
all_food_level<-cbind(all_food_level,item=rep("All Food"))
all_food_level$Date<-as.Date(all_food_level$Date,format="%Y-%m-%d")

all_cpi_level<-all_cpi_level%>%
  pivot_longer(all_of(month_names),names_to = "month",values_to = "cpi_index") %>%
  mutate(Date=str_c(Year,'-',month,'-01'))
all_cpi_level<-cbind(all_cpi_level,item=rep("All Items"))
all_cpi_level$Date<-as.Date(all_cpi_level$Date,format="%Y-%m-%d")


WAW_cpi_items_1<-rbind(all_food_cpi,all_cpi)
WAW_cpi_items_2<-rbind(all_food_level,all_cpi_level)


ggplot(WAW_cpi_items_1,aes(x=Date, y=cpi_index, group=item,
                           col=item)) +
  theme_classic() +
  geom_line(linewidth=1) +
  labs(title="Trends in Annual Inflation Rate (%) of Food versus All Items from 2021-2022",
       color="Items") +
  xlab("") + ylab("") +
  scale_y_continuous(n.breaks = 10, limits = c(0,12), expand = c(0,0)) +
  scale_color_manual(values=c("#c42776", "#78b6e3")) +
  geom_rect(aes(xmin = ymd("2022-08-01"), xmax = ymd("2022-12-01"), ymin = -Inf, ymax = Inf),
            fill = "pink", alpha = 0.01, color=NA) +
  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
        plot.title = element_text(face="bold")) +
  scale_x_date(limits=as.Date(c("2021-01-01","2022-12-01")), date_breaks="2 month", date_labels = "%b", expand=c(0,0))

ggplot(WAW_cpi_items_2,aes(x=Date, y=cpi_index, group=item,
                           col=item)) +
  theme_classic() +
  geom_line(linewidth=1) +
  labs(title="Trends in Consumer Price Index levels of Food versus All Items from 2021-2022",
       color="Items") +
  xlab("") + ylab("") +
  scale_y_continuous(n.breaks = 10, limits = c(0,340), expand = c(0,0)) +
  scale_color_manual(values=c("#c42776", "#78b6e3")) +
  geom_rect(aes(xmin = ymd("2022-08-01"), xmax = ymd("2022-12-01"), ymin = -Inf, ymax = Inf),
            fill = "pink", alpha = 0.01, color=NA) +
  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
        plot.title = element_text(face="bold")) +
  scale_x_date(limits=as.Date(c("2021-01-01","2022-12-01")), date_breaks="2 month", date_labels = "%b", expand=c(0,0))

#-------------------------------------------------------------------------------

all_HH<-food_security_HH[,c(1,2,9)] %>%
  filter(`Category`=="All households", `Year`>=2006)

ggplot(all_HH,aes(x=Year, y=`Food insecure-percent`)) +
  theme_classic() +
  geom_line(linewidth=1, col="darkred") +
  labs(title="Trends in Food Insecurity in U.S. households from 2006-2022",
       subtitle = "Measured as percent of households") +
  xlab("") + ylab("") +
  scale_y_continuous(n.breaks = 10, limits = c(0,30), expand = c(0,0)) +
  geom_rect(aes(xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf),
            fill = "pink", alpha = 0.01, color=NA) +
  geom_rect(aes(xmin = 2021, xmax = 2022, ymin = -Inf, ymax = Inf),
            fill = "pink", alpha = 0.01, color=NA) +
  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
        plot.title = element_text(face="bold"),
        plot.margin=margin(t=5,r=20,l=5,b=5)) +
  scale_x_continuous(n.breaks=15, expand=c(0,0))

food_insecurity_race<-food_security_HH[,c(1,2,3,9)] %>%
  filter(`Category`=="Race/ethnicity of households", `Year`>=2006)

ggplot(food_insecurity_race,aes(x=Year, y=`Food insecure-percent`, group=Subcategory,
                  col=Subcategory)) +
  theme_classic() +
  geom_line(linewidth=1) +
  labs(title="Trends in food insecurity by ethnicity in U.S. households, 2006-2022") +
  xlab("") + ylab("") +
  scale_y_continuous(n.breaks = 10, limits = c(0,30), expand = c(0,0)) +
  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
        plot.title = element_text(face="bold")) +
  scale_x_continuous(n.breaks=15, expand=c(0,0))

#food_insecurity_HH_comp<-food_security_HH[,c(1,2,3,4,9)] %>%
#  filter(`Category`=="Household composition", is.na(`Sub-subcategory`)==FALSE, `Year`==2022) %>%
#  subset(`Sub-subcategory` %in% c("Female head with children","Male head with children",
#                                  "Married-couple families", "Household with no children")) %>%
#  mutate(type=ifelse(`Sub-subcategory`=="Female head with children","Highlighted","Normal"))

#food_insecurity_HH_comp$`Sub-subcategory`<-factor(food_insecurity_HH_comp$`Sub-subcategory`,
#                                                  levels=c("Married-couple families", "Household with no children", 
#                                                           "Male head with children", "Female head with children"))
         
#ggplot(food_insecurity_HH_comp,aes(x=`Sub-subcategory`, y=`Food insecure-percent`,
#                                   col=type, fill=type)) +
#  theme_classic() +
#  geom_col() +
#  scale_y_continuous(n.breaks = 10, limits = c(0,35), expand = c(0,0)) +
#  labs(title="Food insecurity by household composition in U.S. households 2022",
#       subtitle = "Measured as percent of households") +
#  xlab("") + ylab("") +
#  theme(panel.grid.major.x = element_line(linewidth = 0.01,colour = "lightgrey"),
#        plot.title = element_text(face="bold"), legend.position = "none") +
#  coord_flip()

food_insecurity_HH_comp_2<-food_security_HH[,c(1,2,3,4,9)] %>%
  filter(`Category`=="Household composition", is.na(`Sub-subcategory`)==FALSE, `Year`==2022 | `Year`==2021) %>%
  subset(`Sub-subcategory` %in% c("Female head with children","Male head with children",
                                  "Married-couple families", "Household with no children")) %>%
  mutate(type=ifelse(`Sub-subcategory`=="Female head with children","Highlighted","Normal"))

food_insecurity_HH_comp_2$Year<-as.factor(food_insecurity_HH_comp_2$Year)
food_insecurity_HH_comp_2$`Sub-subcategory`<-factor(food_insecurity_HH_comp_2$`Sub-subcategory`,
                                                  levels=c("Female head with children", "Male head with children",
                                                           "Household with no children", "Married-couple families"))

colours<-c(rev(brewer.pal(4, "YlOrRd")),rev(brewer.pal(4, "YlOrRd")))

ggplot(food_insecurity_HH_comp_2, aes(x=Year, y=`Food insecure-percent`, fill=`Sub-subcategory`)) +
  theme_classic() +
  geom_col(position = "dodge", width=0.6) +
  scale_fill_manual(values=colours) +
  scale_y_continuous(n.breaks = 10, limits = c(0,35), expand = c(0,0)) +
  labs(title="Food Insecurity by Household Composition in U.S. households 2021-2022",
       subtitle = "Measured as percent of households",
       fill="Household Composition") +
  xlab("") + ylab("") +
  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face="bold"))


food_insecurity_area<-food_security_HH[,c(1,2,3,9)] %>%
  filter(`Category`=="Census geographic region", `Year`>=2006)

ggplot(food_insecurity_area,aes(x=Year, y=`Food insecure-percent`, group=`Subcategory`,
                                   col=`Subcategory`)) +
  theme_classic() +
  geom_line(linewidth=1) +
  labs(title="Trends in food insecurity by census area in U.S. households, 2006-2022") +
  xlab("") + ylab("") +
  scale_y_continuous(n.breaks = 10, limits = c(0,17), expand = c(0,0)) +
  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
        plot.title = element_text(face="bold")) +
  scale_x_continuous(n.breaks=15, expand=c(0,0))

#food_insecurity_income_poverty<-food_security_HH[,c(1,2,3,9)] %>%
#  filter(`Category`=="Household income-to-poverty ratio", `Year` >= 2006, `Subcategory`!="Income unknown")

#ggplot(food_insecurity_income_poverty,aes(x=Year, y=`Food insecure-percent`, group=`Subcategory`,
#                                col=`Subcategory`)) +
#  theme_classic() +
#  geom_line(linewidth=1) +
#  labs(title="Trends in food insecurity by income to poverty ratio in U.S. households, 2006-2022") +
#  xlab("") + ylab("") +
#  scale_y_continuous(n.breaks = 10, limits = c(0,45), expand = c(0,0)) +
#  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
#        plot.title = element_text(face="bold")) +
#  scale_x_continuous(n.breaks=15, expand=c(0,0))

#food_insecurity_income_poverty<-food_security_HH[,c(1,2,3,9)] %>%
#  filter(`Category`=="Household income-to-poverty ratio", `Subcategory`!="Income unknown")
#food_insecurity_income_poverty$Subcategory<-factor(food_insecurity_income_poverty$Subcategory, levels=c( "1.85 and over", "Under 1.85", "Under 1.30", "Under 1.00"))

#ggplot(food_insecurity_income_poverty,aes(y=`Subcategory`, x=`Food insecure-percent`, fill=after_stat(x))) +
#  theme_classic() +
#  geom_density_ridges_gradient(scale = 1.5, size = 0.3, rel_min_height = 0.001, bandwidth=1.3) +
#  scale_color_viridis(name = "Heat", option = "C") +
#  scale_fill_viridis(name = "Heat", option = "C") +
#  coord_cartesian(clip = "off") +
#  labs(title="Distribution of food Insecurity by income to poverty ratio in US households from 2001-2022",
#       subtitle = "Measured as percent of households") +
#  xlab("") + ylab("") +
#  scale_x_continuous(n.breaks = 10, limits = c(0,48), expand = c(0,0)) +
#  theme(panel.grid.major.x = element_line(linewidth = 0.01,colour = "lightgrey"),
#        panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
#        plot.title = element_text(face="bold"))

food_insecurity_income_poverty_2<-food_security_HH[,c(1,2,3,9)] %>%
  filter(`Category`=="Household income-to-poverty ratio", `Subcategory`!="Income unknown", Year==2022 | Year==2021)

food_insecurity_income_poverty_2$Subcategory<-factor(food_insecurity_income_poverty_2$Subcategory, levels=c("Under 1.00", "Under 1.30", "Under 1.85", "1.85 and over"))
food_insecurity_income_poverty_2$Year<-as.factor(food_insecurity_income_poverty_2$Year)
ggplot(food_insecurity_income_poverty_2,aes(x=`Subcategory`, y=`Food insecure-percent`)) +
  theme_classic() +
  geom_path(aes(group=Year, color=Year)) +
  geom_point(aes(group=Year, color=Year), shape=19, size=3) +
  scale_colour_manual(values=c("darkorange", "darkred")) +
  scale_y_continuous(n.breaks = 10, limits = c(0,40), expand = c(0,0)) +
  labs(title="Income to Poverty versus Food Insecurity in U.S. households 2021-2022",
       subtitle = "Food insecurity measured as percent of households. \nIncome to poverty ratio computed as total income divided by median household income.") +
  xlab("Income to Poverty Ratio") + ylab("") +
  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face="bold"))

#EAI$DATE<-as.Date(EAI$DATE, formate="%Y-%m-%d")

#ggplot(EAI,aes(x=DATE, y=WEI)) +
#  theme_classic() +
#  geom_line(linewidth=1) +
#  labs(title="Weekly Economic Index in U.S. 2008-2022") +
#  xlab("") + ylab("") +
#  scale_y_continuous(n.breaks = 10, limits = c(-10,12), expand = c(0,0)) +
#  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
#        plot.title = element_text(face="bold")) +
#  scale_x_date(limits=as.Date(c("2008-01-05","2023-10-21")), date_breaks="1 year", date_labels = "%Y", expand=c(0,0))

FSI$DATE<-as.Date(FSI$DATE, formate="%Y-%m-%d")

ggplot(FSI,aes(x=DATE, y=STLFSI4)) +
  theme_classic() +
  geom_line(linewidth=1, col="#030a4a") +
  labs(title="St. Louis Fed Financial Stress Index in U.S. from 2019-2023") +
  xlab("") + ylab("") +
  scale_y_continuous(n.breaks = 10, limits = c(-2,6), expand = c(0,0)) +
  geom_rect(aes(xmin = ymd("2020-02-01"), xmax = ymd("2020-07-01"), ymin = -Inf, ymax = Inf),
            fill = "pink", alpha = 0.01, color=NA) +
  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
        plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, color = "black", linewidth=1.1) +
  scale_x_date(limits=as.Date(c("2019-01-01","2023-10-20")), date_breaks="1 year", date_labels = "%Y", expand=c(0,0))

#-------------------------------------------------------------------------------
#Heatmap of America (DO NOT RUN)

HH_pulse_state_1<-HH_pulse_survey_state_1[-1,c(1,3,5,6,8)]
states<-states()
states<-states[match(states$NAME, HH_pulse_state_1$Area, nomatch=0)>0,]
HH_pulse_state_FS_1<-merge(states, HH_pulse_state_1, by.x="NAME", by.y="Area")[,c(1,15:19)]
colnames(HH_pulse_state_FS_1)<-c("State", "No. of People Food Scarce", "Food Scarce Rate", "Margin of Error (+/-)", "Total Population", "geometry")

HH_pulse_state_63<-HH_pulse_survey_state_63[-1,c(1,3,5,6,8)]
HH_pulse_state_FS_63<-merge(states, HH_pulse_state_63, by.x="NAME", by.y="Area")[,c(1,15:19)]
colnames(HH_pulse_state_FS_63)<-c("State", "No. of People Food Scarce", "Food Scarce Rate", "Margin of Error (+/-)", "Total Population", "geometry")


pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))
at <- seq(2, 18, length.out = 9)
m1<-mapview(HH_pulse_state_FS_1, zcol="Food Scarce Rate", layer.name="Food Scarcity Rate",
            col.regions=pal, at=at, alpha.regions=0.75)
m2<-mapview(HH_pulse_state_FS_63, zcol="Food Scarce Rate", layer.name="Food Scarcity Rate After Pandemic",
            col.regions=pal, at=at, legend=FALSE, alpha.regions=0.75)

mv<-m1 | m2

html_fl <- tempfile(fileext = "Food_Security_mapview.html")
mapshot(mv, file = html_fl)
browseURL(html_fl)

#--------------------------------------------------------------------------------
#Data collection and Cleaning (DO NOT RUN)

HH_pulse_survey_v2<-HH_pulse_survey%>%
  filter(GEO_ID=="0100000US")
write.csv(HH_pulse_survey_v2[,c(1,2,14:18)], file="HH Pulse Survey v2.csv")


retail_sale<-read_xlsx("Weekly Food Retail Sales.xlsx")
retail_sale<-retail_sale[,-c(6,9,12,16:18)]  
retail_sale$Subcategory<-as.factor(retail_sale$Subcategory)
levels((retail_sale$Subcategory))
retail_sale$Date<-as.Date(retail_sale$Date,format="%Y-%m-%d")

retail_sale_V2<-retail_sale %>%
  filter(Category=="All foods", Date>as.Date("2020-04-23"))
write.csv(retail_sale_V2[,-c(3,8,9,12)], file="retail_sale_v2.csv")

faps_household<-read.csv("faps_household_puf.csv")
faps_access<-read.csv("faps_access_puf.csv")
faps_household[faps_household==-996]<-NA

faps_household_v2<-faps_household[,-c(2:9,12:15)]
faps_household_v2[,-c(4:6,13,15:27,44:58, 65:82,90,97,102,104,109:113,115,116,136,142:146, 206,207,252:255)]<-lapply(faps_household_v2[,-c(4:6,13,15:27,44:58, 65:82,90,97,102,104,109:113,115,116,136,142:146, 206,207,252:255)], as.factor)

#-------------------------------------------------------------------------------
#Relationship Models

food_security_model_data<-read.csv("food_security_model.csv")[,-1]

food_security_model<-lm(FOODSCARCE_RATE ~ log(FSI+1) + Inflation, data=food_security_model_data[,c(10,13,12)])
summary(food_security_model)

ggplot(food_security_model_data,aes(x=Inflation, y=FOODSCARCE_RATE)) +
  theme_bw() +
  geom_point() + geom_smooth(method='lm', color="darkorange", se=FALSE) +
  scale_y_continuous(n.breaks = 10, limits = c(0,14), expand = c(0,0)) +
  scale_x_continuous(n.breaks = 8, limits = c(0,12), expand = c(0,0)) +
  labs(title="Relationship between Inflation and Food Scarcity during and after the pandemic (2020-2023)",
       subtitle="Food scarcity rate measured as percent of adults") +
  xlab("Annual Inflation Rate (%)") + ylab("Food Scarcity Rate (%)") +
  theme(plot.title = element_text(face="bold"))

ggplot(food_security_model_data,aes(x=FSI, y=FOODSCARCE_RATE)) +
  theme_bw() +
  geom_point() + geom_smooth(method='lm', color="darkorange", formula = y~log(x+1), se=FALSE) +
  scale_y_continuous(n.breaks = 10, limits = c(0,14), expand = c(0,0)) +
  labs(title="Relationship between Financial Stress and Food Scarcity during and after the pandemic (2020-2023)",
       subtitle="Food scarcity rate measured as percent of adults") +
  xlab("Financial Stress Index") + ylab("Food Scarcity Rate (%)") +
  theme(plot.title = element_text(face="bold"))

expenditures<-read_xlsx("expenditures.xlsx")

expenditures_v2<-expenditures[,-6]%>%
  pivot_longer(colnames(expenditures)[-c(1,6)],names_to = "program",values_to = "expenditure")
expenditures_v2$program<-factor(expenditures_v2$program, levels=c("Other", "WIC", "Child nutrition", "SNAP"))


ggplot(expenditures_v2,aes(x=`Fiscal year`, y=expenditure, fill=program, col=program)) +
  theme_classic() +
  geom_area() +
  labs(title="Trends in USDA Expenditure on Food and Nutrition Assistance Programs from 1970-2022",
       subtitle = "Inflation-adjusted measured in billions of 2022 dollars") +
  xlab("") + ylab("") +
  scale_y_continuous(n.breaks = 10, limits = c(0,200), expand = c(0,0)) +
  theme(panel.grid.major.y = element_line(linewidth = 0.01,colour = "lightgrey"),
        plot.title = element_text(face="bold")) +
  scale_color_viridis(discrete = TRUE, option = "B") +
  scale_fill_viridis(discrete = TRUE, option = "B") +
  scale_x_continuous(n.breaks=15, limits = c(1970,2022), expand=c(0,0))


