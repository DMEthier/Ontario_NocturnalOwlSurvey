#Manipuate data 
#Currently does this one province at a time

#Import data using the naturecounts R package. 

#first look in file. If it does not exist, download from database.
if(collection =="ABOWLS"){
  
data<-read.csv("data/ABOWLS_BMDE.csv")

#Now that the data are downloaded we will want to select the columns needed for the analysis. You may not have all the variables below if you didn't change the `field_set` to "extend". That is OK! You may not need all the auxiliary data for your analytical purposes.
data$ProtocolCode<-data$protocol_id
in.data<-data %>% select(SamplingEventIdentifier, SurveyAreaIdentifier, Locality, SiteCode, collection, survey_day, survey_month, survey_year, survey_week, protocol_id, CollectorNumber, EffortUnits1, EffortMeasurement1, species_id, CommonName, ScientificName, Latitude, Longitude, bcr, StateProvince, ObservationCount, ObservationCount2, ObservationCount3) %>% dplyr::rename(latitude = Latitude, longitude = Longitude)
in.data$RouteIdentifier<-in.data$SiteCode
in.data$SiteCode<-in.data$SurveyAreaIdentifier

} else{
 
data <-try(read.csv(paste(dat.dir, collection, ".RawData.csv", sep = "")), silent = T)

if(class(data) == 'try-error'){
  
  data <- nc_data_dl(collections = collection, fields_set="extended", username =ID, info ="Ethier data download NOS")
  write.csv(data, paste(dat.dir, collection, ".RawData.csv", sep = ""), row.names = FALSE)
 
  if(collection == "MBOWLS"){
    data<-data %>% filter (protocol_id != 29) #remove old MB data collected under an old protcol
  }
  }

#Now that the data are downloaded we will want to select the columns needed for the analysis. You may not have all the variables below if you didn't change the `field_set` to "extend". That is OK! You may not need all the auxiliary data for your analytical purposes.
in.data<-data %>% select(SamplingEventIdentifier, SurveyAreaIdentifier,RouteIdentifier, Locality, SiteCode, collection, survey_day, survey_month, survey_year, survey_week, protocol_id, CollectorNumber, EffortUnits1, EffortMeasurement1, EffortUnits3, EffortMeasurement3, EffortUnits5, EffortMeasurement5, EffortUnits11, EffortMeasurement11, EffortUnits14, EffortMeasurement14, species_id, CommonName, ScientificName, latitude, longitude, bcr, StateProvince, ObservationDescriptor, ObservationCount, ObservationDescriptor2, ObservationCount2,ObservationDescriptor3, ObservationCount3)

} #end else

#Notice here that we don't keep all the ObservationCount fields. There are more that could be retained that capture owls call during each broadcast period.
#For the purposes of this analysis, we keep ObservationCount2 + ObservationCount3 = Number of owls detected before call playback is used (i.e., silent listening period only). This is nationally standardized. Some protocols do not have call playback, and we can therefore use ObservationCount (totals) for the analysis. 
#If you want to keep all counts, inclusive of the silent listening + call playback, keep ObservationCount.  

#Filter out the data we know we don't want before proceeding.

#Drop Newfoundland & Labrador based on StateProvince 
in.data<-in.data %>% filter (StateProvince!="Newfoundland") %>% droplevels()

#Drop Northwest Terriroties based on route identifier, survey started in 2018, and therefore not enough data yet for a national assessment
in.data<-filter(in.data, !grepl("NT", RouteIdentifier)) %>%  droplevels()

#Remove surveys with missing Month, Day, Year
in.data<-in.data %>% filter (!is.na(survey_day), !is.na(survey_month), !is.na(survey_year))

#Some Ontario Routes only have lat/long for the start point, so this can't be used for the national assessment. 
#Remove surveys with missing lat long
#in.data<-in.data %>% filter (!is.na(latitude), !is.na(longitude))

if(collection=="ABOWLS"){
in.data<-in.data %>% filter(latitude != "NULL", longitude != "NULL")
}

#Remove survyes with missing protocol ID 
in.data<-in.data %>% filter (!is.na(protocol_id))

#Remove surveys with NOCTOWLS protocol ID
#in.data<-in.data %>% filter (protocol_id != "NOCTOWLS") #This is Alberta's protocol_id

#remove site who naming cause issue on re-import into R 
in.data<-in.data %>% filter(!(RouteIdentifier %in% c("50N/Gaudry Road", "50N/Gaudry Rd, St. L" )))

if(collection=="BCOWLS"){ #need to parse old data into new protocol_id 

  in.data<-in.data %>% filter(protocol_id!=63)
  in.data<-in.data %>% mutate(Route = str_replace_all(RouteIdentifier, "[^[:alnum:]]", ""))
  in.data<-in.data %>% separate(Route, into = c("text", "num"), sep = "(?<=[A-Za-z])(?=[0-9])")
  in.data<-in.data %>% mutate(protocol_id=ifelse(text=="F",999, protocol_id)) #FLAM survey
  in.data<-in.data %>% select(-text, -num)
}

#You may want to fix some data inconsistencies in StateProvince naming. However, you may want to use `collection` or `ProtocolCode` rather than `StateProvince` to do the analysis since there are some points that cross the provincial boundaries. Something to consider. 

in.data$StateProvince[in.data$StateProvince  == "Ontario"]  <-  "ON"
in.data$StateProvince[in.data$StateProvince  == "British Columbia and Yukon"]  <-  "BCY"
in.data$StateProvince[in.data$StateProvince  == "ME"]  <- "QC"
in.data$StateProvince[in.data$StateProvince  == "Newfoundland"]  <- "NL"
in.data$StateProvince[in.data$StateProvince  == "Nova Scotia"]  <- "NS"
in.data$StateProvince[in.data$StateProvince  == "Manitoba"]  <- "MB"
in.data$StateProvince[in.data$StateProvince  == "MN"]  <- "MB"
in.data$StateProvince[in.data$StateProvince  == "Alberta"]  <- "AB"
in.data<- in.data %>% filter(StateProvince != "NL")

#not positive were this route is located. 
#in.data<-in.data %>%filter(RouteIdentifier!="NL011")

#Next we add a day-of-year column using the `format_dates` [helper function](https://birdstudiescanada.github.io/naturecounts/reference/format_dates.html).
#create a doy variable with the `format_dates` NatureCounts function. 
in.data<-format_dates(in.data)

#In this step, we filter the data based on the `Analysis Parameters` file. For example, we will remove surveys done in inappropriate weather conditions. While we can also remove surveys done outside the appropriate survey window (min.day, max.day) it was decided by the national NOS committee not to proceed with this step since survey timing of often tweaked to accommodate weather.
#loop through each row in the analysis parameter files is doing multiple protocol

filter<-collection
protcol<-anal.param %>% dplyr::filter(collection == filter)

#Create the output tables for writing the data cleaning results
Events<- as.data.frame(matrix(data = NA, nrow = 1, ncol = 9, byrow = FALSE, dimnames = NULL))
names(Events) <- c("SiteCode", "RouteIdentifier", "survey_year", "CollectorNumber", "nstop", "StateProvince", "latitude", "longitude", "protocol_id")
#only need to create the table once per analysis   
write.table(Events, file = paste(out.dir, collection, "Events.csv", sep = ""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

Owls<- as.data.frame(matrix(data = NA, nrow = 1, ncol = 11, byrow = FALSE, dimnames = NULL))
names(Owls) <- c("SiteCode", "RouteIdentifier", "survey_year", "CollectorNumber", "collection", "protocol_id", "doy",  "CommonName", "species_id", "ObservationCount", "StateProvince")
#only need to create the table once per analysis   
write.table(Owls, file = paste(out.dir, collection, "OwlDataClean",".csv", sep = ""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

Loc<-as.data.frame(matrix(data=NA, nrow=1, ncol=4, byrow=FALSE, dimnames=NULL))
names(Loc)<-c("RouteIdentifier", "latitude", "longitude", "protocol_id")
write.table(Loc, file=paste(out.dir, collection, "Map.csv", sep = ""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

for(k in 1:nrow(protcol)) {
  
  #Note: data from 2020 will be incomplete for most provinces due to COVID19. 
  #Pull the protocol-specific data you need for filtering from the Analysis Parameters file
  
  id<-protcol[k,"protocol_id"]
  collection<-protcol[k, "collection"]
  min.yr <- protcol[k,"min.year"] 
  min.doy <- protcol[k,"min.doy"]
  max.doy <- protcol[k,"max.doy"]
  temp <- protcol[k,"temp"]
  obs<-protcol[k,"obs"]
  
  #adjust to add a 7 day buffer on either side
  #Do this for all programs due to the influnce of weather on min and max day
  
# if(protocol_id=="35"){ #Add buffer to QC data for cleaning. Others may also request this filter be adjusted. 
    min.doy <- min.doy-14
    max.doy <- max.doy+14
#}
  
  #subset data based on protocol_ID 
  dat<-NULL
  dat<-in.data %>% filter(protocol_id == id)
  dat$ObservationCount[is.na(dat$ObservationCount)] <- 0
  dat$ObservationCount<-as.numeric(dat$ObservationCount)
  
  #reassign routes to correct provinces and add SiteCode to QC
  if(id=="35"){
    dat$StateProvince[dat$StateProvince  == "ON"]  <- "QC"  
    dat<-dat %>% separate(SamplingEventIdentifier, c("del1", "del2", "StopNo"), sep="-", remove=FALSE) %>% dplyr::select (-del1, -del2) %>% mutate(SiteCode2= paste(RouteIdentifier, StopNo, sep="-")) %>% select(-StopNo)
    dat<-dat %>% mutate(SiteCode = ifelse(is.na(SiteCode), SiteCode2, SiteCode)) %>% select(-SiteCode2)
  }

  ##NEEDS FIXED IN THE UNDERLYING DATA
  #if(id=="150"){
  #  dat$RouteIdentifier<-dat$SiteCode
  #  dat$SiteCode<-dat$SurveyAreaIdentifier
  #}
  
  #Ontario specific data cleaning because a route was surveyed multiple times per year in the early years. We want to just select one
  if(id=="22" | id=="36"){
    dat$StateProvince[dat$StateProvince  == "QC"]  <- "ON"  
  
  #add day of year (doy) to dataframe to be used later 
  dat <- dat %>%
    mutate(date = ymd(paste(survey_year, survey_month, survey_day, sep = "/")),
           doy = yday(date))
  
  dat<-dat %>% drop_na(doy)
  
  #since there is no window field we need to select the last survey date for those that have multiple surveys per route within a year
  
  #group by year and route to see unique survey months/days
  survey <- dat %>% group_by(SurveyAreaIdentifier, survey_year)%>%
    dplyr::summarize(sample_events=n_distinct(survey_month))
  
  #merge back with main dataframe 
  dat<-left_join(dat, survey, by= c("SurveyAreaIdentifier", "survey_year"))
  
  #subset out the data with sample_events>1
  subdat<-subset(dat, sample_events>1)
  
  #want to select the max day for each Year SurveryAreaIdentifier combo with more then one sampling event
  maxday<-subdat %>% group_by(SurveyAreaIdentifier, survey_year)%>%
    dplyr::summarize(maxday=max(doy))
  
  #merge back with main data, eliminate the unwanted data, select needed columns
  dat<-left_join(dat, maxday, by= c("SurveyAreaIdentifier", "survey_year"))
  dat$check<-dat$maxday-dat$doy
  dat$check[is.na(dat$check)] <- 0
  dat<-dat%>%filter(check==0)
  dat<-dat%>% dplyr::select(-date, -sample_events, -maxday, -check)
  
  } #end Ontario specific data cleaning

  #need to remove duplicates which are in the Remarks field of the BMDE then the remarks field can be safely removed. In othewords, keep the NAs. First we need to create a new level for the factor and add the level 0. Then replace NA with 0 and filter for 0. 
  #levels <- levels(dat$Remarks2)
  #levels[length(levels) + 1] <- "0"
  #dat$Remarks2 <- factor(dat$Remarks2, levels = levels)
  #dat$Remarks2[is.na(dat$Remarks2)] <- 0
  #dat<-dat %>% filter(Remarks2=="0")
  
  #Subset data to specified year range
  #max.yr <- anal.param[k,"max.year"]
  dat <- dat %>% filter(survey_year >= min.yr & survey_year <= max.yr)
    
  ##____________________________________________________________________
  #Create a dataframe with a single lat long per route ID (what the first stop in a route). This is necessary because some Ontario routes only have a lat long for the first stop in a route. 
  loc.dat <-NULL #clear old
  
  #Using SamplingEvent
  
  if(collection == "QCOWLS"){
  loc.dat<-dat %>% separate(SamplingEventIdentifier, c("del1", "del2", "Stop"), sep="-", remove=FALSE) %>% select (-del1, -del2)
  loc.dat<-loc.dat %>% filter(latitude!="NA") %>% arrange(Stop) %>% distinct(RouteIdentifier, .keep_all = TRUE) %>% select(RouteIdentifier, latitude, longitude, protocol_id)
  }
  
  loc.dat<-dat %>% filter(latitude!="NA") %>% distinct(RouteIdentifier, .keep_all = TRUE) %>% select(RouteIdentifier, latitude, longitude, protocol_id) %>% distinct()
  write.table(loc.dat, file = paste(out.dir, collection, "Map.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
  
  ##____________________________________________________________________
  #Because in the early years the Ontario owls program ran upwards of 4 surveys/route/year, the duplicates will need to be removed. We run this script for all province since there are some other duplicate routes that have made their way into the database. 
  
  #We select the last survey date for those that have multiple surveys per route within a year
  
  #first filter by the min and max survey doy to capture the surveys that are done consistently with current protocol
  dat <- dat %>% filter(doy >= min.doy & doy <= max.doy)
  
  #if multiple surveys done in a year, keep the first survey. 
  dat <- dat %>% group_by(SiteCode, survey_year)%>%
    slice_min(survey_day)
 
if(id != "157"){
  if(id != "151"){
  ##______________________________________________________________
  #Filtering the data to standardized detectability. 

  #Subset data to minimum temperature using the start temp at the beginning of the survey. We will also keep NAs and assume these dataset are ok. This will help us preserve data. Could change moving forward. 
  dat <- dat %>% filter(EffortMeasurement3 >= temp | is.na(EffortMeasurement3))
  
  #Subset data to remove data collected when start precip > 1, start wind > 3 or noise > 3. We will also keep NAs and assume they are collected under appropriate conditions to help preserve data. Could change moving forward. Currently removed noise.   
  dat<-dat %>% filter(EffortMeasurement1 <= 3 | is.na(EffortMeasurement1)) #Start wind
  dat<-dat %>% filter(EffortMeasurement5 <= 1 | is.na(EffortMeasurement5)) #Start precip
  #dat<-dat %>% filter(EffortMeasurement11 <= 3 | #is.na(EffortMeasurement11)) #Noise Level
  
  #Now we want to remove repeat counts of individuals between stops. Not all region record this, but many do.
  dat<-dat %>% filter(is.na(EffortMeasurement14)|EffortMeasurement14==0) #Repeats removed

  }#end if statement 
  }#end if statement
  
  ##____________________________________________________________________
  #The number of stops on a route within a year is used an an effort covariate in the model
  stop.year<-dat %>% group_by(RouteIdentifier, survey_year) %>% summarize(nstop=n_distinct(SurveyAreaIdentifier))
  dat<-left_join(dat, stop.year, by=c("RouteIdentifier", "survey_year"))
  
  ##___________________________________________________________________
  #Filter incomplete routes, which are defined here as routes that had less than 5 stops complete in a year. 
  
  dat<- dat %>% filter(nstop>=5)
  
  ##____________________________________________________________________
  #Now that the surveys are removed that were done in inappropriate environmental conditions we can make a list of unique sampling events to use for zero-filling species-specific data frames. We don't zero-fill the entire data frame for space issues. 
  
  #Note: Since our response variable in the analysis is counts at the route-level, the sampling event is a route within a year. If the analysis is changed to be at the stop level, you will want to include `month` and `day` in the code below.   
  
  event.data <-NULL #clear old
  event.data <- dat %>%
    select(RouteIdentifier, survey_year, CollectorNumber, nstop, StateProvince) %>%  
    distinct() %>%
    ungroup() %>%
    as.data.frame()
  
  #merge with the loc.data to assign unique lat long to each route
  event.data<-left_join(event.data, loc.dat, by=c("RouteIdentifier"))
  
  ##____________________________________________________________________
  #Because we have different data collection methods, the response variable for each protocol may be ObservationCount2+ObservationCount3 = ObservationCount OR ObservationCount depending on how the data are recorded. The Analysis Parameters files will specify this for each protocol under the `obs` column. 
  
  #Use this code is you would like to use just the date collected during the first 2 min
  #if(obs=="ObservationCount1"){
  #dat$ObservationCount2<-ifelse(is.na(dat$ObservationCount2), 0, 1)
  #dat$ObservationCount3<-ifelse(is.na(dat$ObservationCount3), 0, 1)
  #dat<- dat %>% mutate(ObservationCount = ifelse (ObservationCount2==1 | ObservationCount3==1, 1, 0))    
  #}
  
  if(id != "157"){
    if(id != "151"){
  #now we no longer need these fields
  dat<-dat %>% select(-ObservationCount2, -ObservationCount3, -ObservationDescriptor2, -ObservationDescriptor3)
  
    } } # end if statements
  
  ##Remove rare species
  #Species must be detected on at least half of all years surveyed.  
  min.yrs.detect<-trunc(length(unique(dat$survey_year))/2)
  
  df.nyears <- NULL #clear old
  df.nyears <- dat %>%
    filter(ObservationCount > 0) %>%
    select(survey_year, CommonName) %>%
    distinct() %>%
    group_by(CommonName) %>%
    summarize(nyears = dplyr::n()) %>%
    filter(nyears >= min.yrs.detect)%>%
    as.data.frame() %>% 
    filter(CommonName != "owl sp.")
  
  dat <- left_join(df.nyears, dat, by = c("CommonName")) %>%  select(-nyears)
  
  #Species must be detected with a mean of ~5 per years to be included in the analysis. 
  
  df.abund <- NULL #clear old
  df.abund <- dat %>%
    group_by(CommonName, survey_year) %>%
    summarize(count = sum(ObservationCount)) %>% 
    summarize(meanCount = mean(count, na.rm = TRUE)) %>% 
    filter(meanCount >= 5)%>%
    as.data.frame()
  
  dat <- left_join(df.abund, dat, by = c("CommonName")) %>%
    select(-meanCount)%>%
    as.data.frame()
  
  #print the final data to file
  #write.csv(dat, paste(out.dir, collection, ".", protocol_id, ".csv", sep=""))
  
  Owls<-NULL
  Owls<-dat %>% select(SiteCode, RouteIdentifier, survey_year, CollectorNumber, collection, protocol_id, doy,  CommonName, species_id, ObservationCount, StateProvince)
   write.table(Owls, file = paste(out.dir,  collection, "OwlDataClean.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
  
  #print the event.data to file 
  #write.csv(event.data, paste(out.dir, collection, ".", protocol_id, ".EventData.csv", sep=""))
  
  Events<-NULL
  Events<-event.data 
  write.table(Events, file = paste(out.dir, collection, "Events",".csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
  
} #end loop
