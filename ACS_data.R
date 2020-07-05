#LOUDOUN COUNTY FOOD INSECURITY

setwd("~/dspg2020Loudon")

library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)
library(olsrr)
library(stats)
library(psych)


#show available variables in a particular ACS survey
acs5<-load_variables(2009, "acs5", cache=T)
View(acs5)

acs5_subject <- load_variables(2018, "acs5/subject", cache=T)
View(acs5_subject)

acs5_profile<- load_variables(2018, "acs5/profile", cache=T)
View(acs5_profile)

#FUNCTIONS:

# 1. "acs_tables" calls "get_acs" (from tidycensus) on a vector of table names. It returns a dataframe of 
# all the tables bound together.  The function requires a vector of table names, 
# a census API key, and a geographical unit.  User can add other parameters as well.

acs_tables<-function(tables,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(tables)){
    data<-get_acs(geography = geography,
                  table = tables[i],
                  key = key,
                  show_call = T,
                  cache_table=T,
                  ...
    )
    acs_data<-rbind(acs_data,data.frame(data))
  }
  return(acs_data)
}

# 2. "acs_wide" cleans the data returned from a census API call.  More specifically, 
# it separates the variable column into separate variables, and it separates "NAME" into 
# different columns with pre-defined column names (NAME_col_names). The function also
# drops the "margin of error" column.

acs_wide<-function(data,NAME_col_names){
  data%>%
    select (-moe)%>%
    pivot_wider(names_from = variable,values_from=estimate)%>%
    separate(NAME, into=NAME_col_names, sep = ", ")
}


#3. acs_years retrieves individual variables (or a list of variables) across a series of years.
acs_years<-function(years,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(years)){
    acs<-get_acs(geography = geography,
                 #variables = vars,
                 key = key,
                 year=years[i],
                 output = "wide",
                 show_call = T,
                 geometry = F,
                 ...)
    acs_data<-(rbind(acs_data,data.frame(acs)))
  }
  acs_data<-cbind(acs_data,year=rep((years),each=length(unique(acs_data$GEOID))))
  return(acs_data)
}


#4. "acs_years_tables" uses two previously defined functions (acs_tables and acs_wide) to return multiple 
# variable tables across multiple years in one single tibble.  A couple of notes: the way that 
# get_acs handles variables before 2013 varies, so this function only works for 2013 and after.
# For variable tables before 2013, use acs_tables to pull individual sets of tables.  Also, I have 
# not included "geometry" in the function.  If the user includes geometry, he/she may need 
# to modify the call to acs_wide.


acs_years_tables<-function(tables,years,key,geography,NAME_col_names,...){
  acs_data<-NULL
  for (j in 1:length(years)){
    acs<-acs_tables(tables=tables,year=years[j],key=key,geography = geography,...)
    year<-rep(years[j],times=length(acs$GEOID))
    acs_years2<-cbind(year,data.frame(acs))
    acs_data<-(rbind(acs_data,acs_years2))
  }
  acs_data<-acs_wide(acs_data,NAME_col_names = NAME_col_names)
  return(acs_data)
}

#NATIONAL DATA

tables<-c("B14006","C17002","B19013","DP04","DP05","S1810","S2301")
years<-c(2013,2014,2015,2016,2017,2018)
colnames=c("Census_tract","County","State")

acs_Loudon<-acs_years_tables(tables=tables,
                             years=years,
                             key=.key,
                             geography="tract",
                             state="VA",
                             county="Loudoun",
                             NAME_col_names = colnames)

acs_NoVa<-acs_years_tables(tables=tables,
                           years=years,
                           key=.key,
                           geography="tract",
                           state="VA",
                           county=c("Arlington county",
                                    "Fairfax county",
                                    "Loudoun county",
                                    "Prince William county",
                                    "Alexandria city",
                                    "Falls Church city",
                                    "Fairfax city",
                                    "Manassas city",
                                    "Manassas Park city"),
                           NAME_col_names = colnames)
colnames="state"
acs_state<-acs_years_tables(tables = tables,
                            key = .key,
                            geography = "state",
                            years=years,
                            NAME_col_names = colnames)



#Clean national data
acs_state_clean<-acs_state%>%
  filter(GEOID!=72)%>%
  arrange(year)%>%
  arrange(GEOID)%>%
  rename(STATEFIP=GEOID)%>%
  rename(YEAR=year)
acs_state_clean$STATEFIP=as.integer(acs_state_clean$STATEFIP)


#Calculate relevant variables to be used in the linear model
acs_state_insecurity<-acs_state_insecurity%>%
  mutate(PovertyRate=((B14006_002-(B14006_009+B14006_010))/B14006_001)*100)%>%
  mutate(MedianIncome=B19013_001)%>%
  mutate(OwnRate=(DP04_0046P))%>%
  mutate(PerAfAm=(DP05_0038P))%>%
  mutate(PerHisp=(DP05_0071P))%>%
  mutate(DisRate=(S1810_C03_001))%>%
  mutate(Unemployment=S2301_C04_021)

#Construct linear model with year and state as fixed effects
Model<-lm(log(InsecurityRate)~PovertyRate+MedianIncome+OwnRate+PerAfAm+PerHisp+
            DisRate+Unemployment+as.factor(YEAR)+as.factor(STATEFIP)-1,data=acs_state_insecurity)
summary(Model)

#Plot residuals v. fitted values to test for heteroscedasticity
ols_plot_resid_fit(Model)


LoudounReduced$STATEFIP<-as.integer(rep(51,times=length(LoudounReduced$GEOID)))
LoudounReduced<-as.data.frame(LoudounReduced)


#Calculate model variables for NoVa
acs_NoVa_insecurity<-acs_NoVa%>%
  mutate(PovertyRate=((B14006_002-(B14006_009+B14006_010))/B14006_001)*100)%>%
  mutate(MedianIncome=B19013_001)%>%
  mutate(OwnRate=(DP04_0046P))%>%
  mutate(PerAfAm=(DP05_0038P))%>%
  mutate(PerHisp=DP05_0071P)%>%
  mutate(DisRate=(S1810_C03_001))%>%
  mutate(Unemployment=S2301_C04_021)

NoVaReduced<-acs_NoVa_insecurity%>%
  select(GEOID,year, Census_tract, County, State, PovertyRate, MedianIncome, OwnRate, PerAfAm, PerHisp, DisRate,Unemployment)%>%
  rename(YEAR=year)

NoVaReduced$STATEFIP<-as.integer(rep(51,times=length(NoVaReduced$GEOID)))
NoVaReduced<-as.data.frame(NoVaReduced)


#MAPPING

#Get geometry data for Loudoun County
LoudounGeometry<-get_acs(geography = "tract",
                         state="VA",
                         county = "Loudoun",
                         variables = "B19058_002",
                         survey = "acs5",
                         key = .key,
                         year=2018,
                         output = "wide",
                         show_call = T,
                         geometry = T,
                         keep_geo_vars = T)%>%
  select(-c(11:12))

# Join geometry data to food insecurity predictions and filter data by a particular year
LoudounReducedGeom<-inner_join(LoudounReduced,LoudounGeometry,by="GEOID")%>%
  mutate(expFoodInsecurity=exp(LoudounFoodInsecurity))%>%
  filter(YEAR==2018)

# Plot Loudoun
# I am using the log transformation of food insecurity
ggplot(LoudounReducedGeom, aes(fill = expFoodInsecurity, color = expFoodInsecurity)) +
  geom_sf(aes(geometry=geometry)) +
  labs(title="Loudoun County",subtitle="2018 Food Insecurity Rate")+
  theme(legend.title = element_blank())+
  scale_fill_viridis_c()+
  scale_color_viridis_c()

#Get geometry data for NoVa census tracts
NoVaGeometry<-get_acs(geography = "tract",
                      state="VA",
                      county=c("Arlington county",
                               "Fairfax county",
                               "Loudoun county",
                               "Prince William county",
                               "Alexandria city",
                               "Falls Church city",
                               "Fairfax city",
                               "Manassas city",
                               "Manassas Park city"),
                      variables = "B19058_002",
                      survey = "acs5",
                      key = .key,
                      year=2018,
                      output = "wide",
                      show_call = T,
                      geometry = T,
                      keep_geo_vars = T)%>%
  select(-c(11:12))

# Join geometry data to food insecurity predictions and filter data by a particular year
NoVaReducedGeom<-NoVaReduced%>%
  filter(YEAR==2018)%>%
  inner_join(NoVaGeometry,by="GEOID")%>%
  mutate(expFoodInsecurity=exp(NoVaFoodInsecurity))

# There are two census tracts with very high food insecurity rates.  For the purposes 
# of visualization and differentiation between tracts, I'm capping the rate at 4 (log scale),
# which is equivalent to 60% food insecrity
NoVaReducedGeom[c(172,493),14]<-4

#Get county outlines for NoVa
va_sf<-get_acs(geography = "county",
               state="VA",
               county=c("Arlington county",
                        "Fairfax county",
                        "Loudoun county",
                        "Prince William county",
                        "Alexandria city",
                        "Falls Church city",
                        "Fairfax city",
                        "Manassas city",
                        "Manassas Park city"),
               variables = "B19058_002",
               survey = "acs5",
               key = .key,
               year=2018,
               output = "wide",
               show_call = T,
               geometry = T,
               keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)

# Plot NoVa
ggplot(NoVaReducedGeom, aes(fill = NoVaFoodInsecurity, color = NoVaFoodInsecurity)) +
  geom_sf(aes(geometry=geometry)) +
  geom_sf(data=va_sf,fill="transparent",color="black",size=0.5)+
  labs(title="Northern Virginia",subtitle="2018 Food Insecurity Rate (log scale)")+
  theme(legend.title = element_blank())+
  scale_fill_viridis_c()+
  scale_color_viridis_c()

