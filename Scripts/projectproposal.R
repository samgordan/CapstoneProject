#Data Incubator Project proposal

library(dplyr)
library(ggplot2)
library(stringr)

#project resources
#http://desktop.arcgis.com/en/analytics/case-studies/la-county-homelessness-1-overview.htm
#https://www.lahsa.org/dashboards?id=13-count-by-city-community
#https://usc.data.socrata.com/d/e7n7-i6jm/visualization
#http://homeless.lacounty.gov/data-dashboards/
#https://data-downloads.evictionlab.org/
#https://usc.data.socrata.com/stories/s/Homelessness-in-2018-A-Snapshot-of-Los-Angeles-Cou/g8ge-um6u/
#https://www.hudexchange.info/programs/coc/coc-dashboard-reports/?filter_Year=&filter_State=CA&filter_CoC=CA-600&program=CoC&group=Dash
#https://news.usc.edu/156366/affordable-rent-research-income-distribution/?utm_source=USC+News+Shortlist&utm_campaign=fcbf840183-EMAIL_CAMPAIGN_2019_04_26_10_06&utm_medium=email&utm_term=0_04b26a1e33-fcbf840183-113490717&mc_cid=fcbf840183&mc_eid=387f0ff020

#Project Description
# I have been involved in homelessness outreach and public policy in Los Angeles for many years and improving LA's response to homelessness is an issue where progress can be made by thinking creatively about the available data. I have taken homeless count and population data from Los Angeles Homeless Services Authority's yearly homeless count and eviction data in Los Angeles from the Princeton Eviction Lab to study how homelessness is related to eviction. My expectation is that census tracts where there are high numbers of evictions would similarly have high numbers of people experiencing homelessness. Research shows the number of people experiencing homelessness is largely trending up  (http://www.laalmanac.com/social/so14.php), but surprisingly (plot 1), the number of evictions in LA has gone down in recent years. This negative correlation seems counterintuitive because we might expect that people experiencing homelessness in a given area would be positively correlated with a large number of evictions in the same area. Analyzing homelessness and evictions data together can help us understand this surprising data. The present results suggest that the populations of people who were evicted in 2016 and people who experienced homelessness in 2016 do not substantially overlap, and further this leads to other areas of research, like who is experiencing homelessness, for how long, and what becomes of evicted people post-eviction?

homelesscount <- read.csv("/Volumes/Portable External/Dropbox/DataIncubator/homelesscountclean.csv", header=T, fill=T)

homelesscount2016 <- subset(homelesscount, DateYear == "2016")

evictions <- read.csv("/Volumes/Portable External/Dropbox/DataIncubator/tracts.csv", header=T, fill=T)

LAevictions <- subset(evictions, parent.location == "Los Angeles County, California")
LAevictions2016 <- subset(LAevictions, year == 2016)
LAevictions2016$tractNew <- LAevictions2016$GEOID

#evictions per year
ggplot(data=LAevictions, aes(x=year, y=evictions)) + geom_bar(stat="identity") + labs(title="Number of Los Angeles Evictions per year", x="Year", y = "# Evictions")


homelesscount2016$tractNew <- as.numeric(str_sub(homelesscount2016$GeoId,-10,-1))

collhomelesscount2016 <- homelesscount2016 %>% group_by(tractNew) %>% summarise_at(vars(TotalUnshelteredNUM, TotalShelteredNUM, TotCampers, TotCars, TotEncamp, TotVans, TotPeople, TotTents, TotCarPeople, TotVanPeople, TotTentPeople, TotSheltPeople, TotStreetPeople, TotCamperPeople, TotEncampPeople, TotUnsheltPeople), sum)

#join evictions and homeless count data
homelessANDevictions2016 <- inner_join(collhomelesscount2016, LAevictions2016, by = "tractNew", copy = true)
homelessANDevictions2016$tractNew <- as.numeric(homelessANDevictions2016$tractNew)


#clean up census tracts where unsheltered# is greater than population
housinghomelessness2016.clean <-subset(homelessANDevictions2016, population >= TotUnsheltPeople)

#add

#overlay
ggplot(data=housinghomelessness2016.clean, aes(x=TotalUnshelteredNUM, y=evictions)) + geom_point(aes(color = tractNew), size = 3) +geom_smooth(method = "gam", color="red", se=FALSE) + labs(title="Number of Unsheltered people vs. Number of Evictions by Census Tract, Los Angeles 2016", y="# of Evictions", x = "# of Unsheltered People") + labs(color="LA Census Tracts") 

ggplot(data=housinghomelessness2016.clean, aes(x=evictions, y=TotUnsheltPeople)) + geom_point(size=3) + labs(title="Number of Unsheltered people vs. Evictions by Census Tract, LA 2016", x = "# of Evictions", y = "# Unsheltered People") + ylim(0,5000)

ggplot(data=housinghomelessness2016.clean, aes(y=evictions, x=poverty.rate)) + geom_point() + labs(title="Poverty rate vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "Poverty rate (percent)")

ggplot(data=housinghomelessness2016.clean, aes(y=evictions, x=median.gross.rent)) + geom_point() + labs(title="Median Gross Rent vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "Median gross rent in USD")

ggplot(data=housinghomelessness2016.clean, aes(y=evictions, x=population)) + geom_point() + labs(title="Population vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "Population") +geom_smooth(method = "loess", color="red", se=FALSE)

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =pct.renter.occupied)) + geom_point() + labs(title="% Renter-Occupied homes vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "Renter-Occupied homes (%)") + geom_smooth(method = "gam", color="red", se=FALSE)

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =rent.burden)) + geom_point() + labs(title="Rent Burden vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "Rent Burden (%)") + geom_smooth(method = "loess", color="red", se=FALSE)

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =pct.af.am)) + geom_point() + labs(title="Percent Af-AM vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "AfAm Pop (%)") 

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =pct.hispanic)) + geom_point() + labs(title="Percent Hispanic vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "Hisp Pop (%)") 

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =pct.white)) + geom_point() + labs(title="Percent White vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "White Pop (%)") + geom_smooth(method = "gam", color="red", se=FALSE)

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =pct.asian)) + geom_point() + labs(title="Percent Asian vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "Asian Pop (%)") + geom_smooth(method = "gam", color="red", se=FALSE)

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =TotCamperPeople)) + geom_point() + labs(title="People in campers vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "# people in campers") + geom_smooth(method = "gam", color="red", se=FALSE)

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =TotTentPeople)) + geom_point() + labs(title="People in tents vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "# people in tents") + geom_smooth(method = "gam", color="red", se=FALSE)

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =TotEncampPeople)) + geom_point() + labs(title="People in encampments vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "# people in encampments") + geom_smooth(method = "gam", color="red", se=FALSE)

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =TotStreetPeople)) + geom_point() + labs(title="Homeless staying on the street vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "# people on street") + geom_smooth(method = "gam", color="red", se=FALSE)

ggplot(data=housinghomelessness2016.clean, aes(y =evictions, x =TotSheltPeople)) + geom_point() + labs(title="Homeless staying in shelters vs. Evictions by Census Tract, LA 2016", y = "# of Evictions", x = "# people in shelters") + geom_smooth(method = "gam", color="red", se=FALSE)

#histogram
ggplot(data=homelessANDevictions2016,aes(x=evictions)) + geom_histogram()
ggplot(data=homelessANDevictions2016,aes(x=poverty.rate)) + geom_histogram()


