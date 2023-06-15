# Identify daily avg. flow thresholds for bypass flows
# Laura Twardochleb
# 6/8/2023


# 1. Global Code and Functions -----------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# Define file path in the repository for figure and table outputs
fp_output <- here("figs_tables")

# Define file path in the repository for dayflow data
fp_outflow <- here("data")

# 2. Import and Prepare Data --------------------------------------------------

# Import data
outflow_1996 <- read_csv(file.path(fp_outflow, "dayflow-results-1984-1996.csv"))
outflow_2020 <- read_csv(file.path(fp_outflow, "dayflow-results-1997-2020.csv"))

#Combine and manipulate datasets
outflow<-full_join(outflow_1996, outflow_2020)%>%
  mutate(EXPORTS=ifelse(is.na(EXPORT), EXPORTS, EXPORT))%>% #combine export columns with different names
  #select(-c(DIVER, EFFEC, EFFDIV))%>% #remove unneeded columns
  mutate(Season=case_when(Month%in%1:6 ~ "Winter_Spring", # Create seasonal variables
                          Month%in%7:12 ~ "Summer_Fall",
                          TRUE ~ NA_character_))%>%
  group_by(Year, Season)%>% #create seasonal summaries of delta outflow, exports
  mutate(seasonal_export=mean(EXPORTS))%>% 
  mutate(seasonal_outflow=mean(OUT))%>%
  unite('year_season', c(Year, Season), remove=FALSE)%>% #create new variable defining year and season
  mutate(upper_bookend=case_when(seasonal_outflow>=47000 ~ 1,
                                 seasonal_outflow<47000 ~ 0))%>%#identify year_season above 47,000 cfs
  mutate(lower_bookend=case_when(seasonal_outflow>=29200 ~ 1,
                                 seasonal_outflow<29200 ~ 0))#identify year, season above 29,200 cfs
  
  
# 3. Explore distributions of outflow, exports over time --------------------------------------------------

#visually assess stationarity of exports over last 20-30 years 
outflow%>%ggplot()+geom_point(aes(x=year_season, y=seasonal_export))

#assess stationarity of exports using Dickey-Fuller test
library(tseries)
adf.test(outflow$EXPORTS) #exports time series is stationary

#examine stationarity of delta outflows
outflow%>%ggplot()+geom_point(aes(x=year_season, y=seasonal_outflow))
adf.test(outflow$OUT) #Delta outflow is also stationary

#examine data from seasons with outflow thresholds above 29,200 cfs and 47,000 cfs as bookends in winter_spring (jan-june period)
outflow%>%filter(Season=="Winter_Spring")%>%filter(seasonal_outflow>=29200&47000)%>%
  ggplot()+geom_point(aes(x=year_season, y=seasonal_outflow))

#examine all years
outflow%>%filter(Season=="Winter_Spring")%>%
  ggplot()+geom_point(aes(x=OUT, y=seasonal_outflow))

# 4. Predict seasonal outflow using daily average outflow -------------------------------------------------------------------
#set up glms for seasonal outflow~daily avg. delta outflow for two bookends

#logistic regression for lower bookend
spring_outflow<-outflow%>%filter(Season=="Winter_Spring")
lower_bookend<-glm(lower_bookend~OUT, family="binomial", data=spring_outflow)
lower_bookend
summary(lower_bookend)

#what are the outflow values that get you 29,200 cfs of outflow?
LD50=-lower_bookend$coefficients[1]/lower_bookend$coefficients[2]
LD75= (log(.75/(1-.75))-lower_bookend$coefficients[1])/lower_bookend$coefficients[2]
LD99= (log(.99/(1-.99))-lower_bookend$coefficients[1])/lower_bookend$coefficients[2]

#define function to calculate LD values
LDfunc <- function(x, y, z){
  (log(x/(1-x))-y)/z
}

#calculate LD values for 51 to 99 for lower bookend
LD<-list(seq(from=0.51, to=0.99, length.out=49))
LDs_lower_bookend<-mapply(LDfunc, x=LD, y=lower_bookend$coefficients[1], z=lower_bookend$coefficients[2])

#logistic regression for upper bookend
upper_bookend<-glm(upper_bookend~OUT, family="binomial", data=spring_outflow)
upper_bookend
summary(upper_bookend)

#calculate LD values for 51 to 99 for upper bookend
LD50upper=-upper_bookend$coefficients[1]/upper_bookend$coefficients[2]
LDs_upper_bookend<-mapply(LDfunc, x=LD, y=upper_bookend$coefficients[1], z=upper_bookend$coefficients[2])

#bind the lists together into a dataframe of upper and lower bookend LDs
LD_upper_lower<-cbind(LDs_lower_bookend, LDs_upper_bookend, unlist(LD))

# 5. Prepare figures of outflow and LDs ------------------------------------------------------------------------------------
#daily avg. outflow on x-axis and probability on y-axis (may need smaller increments of LD values)
