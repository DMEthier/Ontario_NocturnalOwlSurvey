#iCAR analysis

dat<-read.csv(paste(out.dir, collection, "OwlDataClean.csv", sep=""))
dat<-dat %>% na.omit()

events<-read.csv(paste(out.dir, collection, "Events.csv", sep=""))
events<-events %>% na.omit() %>% distinct()

loc.dat<-read.csv(paste(out.dir, collection, "Map.csv", sep=""))
loc.dat<-loc.dat %>% na.omit() %>% distinct() %>% filter(latitude != "NULL")

events<-events %>% filter(survey_year>=min.yr & survey_year<=max.yr) %>% filter(survey_year!=2020)#remove covid year
dat<-dat %>% filter(survey_year>=min.yr & survey_year<=max.yr) %>% filter(survey_year!=2020) #remove covid year

#Make Spatial Grid for iCAR Analysis

#all grid for North American
poly<- read_sf(dsn="C:/Users/ethie/Documents/ethier-scripts/Ontario_NocturnalOwlSurvey/Data", layer="nos_na_grid")

#sf point
xy<-st_as_sf(loc.dat, coords = c("longitude", "latitude"))
st_crs(xy)<-"+proj=longlat +datum=NAD83"

newCRS<-st_crs(poly)
xy<-st_transform(xy, newCRS)
ids<-st_intersects(xy, poly)
ids<-unlist(ids, use.names=FALSE)

#add cell id to point data
loc.dat$cell_id<-ids

#select the polygons that intersect the points
Grid <- poly %>% filter(id %in% ids)
Grid$id<-as.integer(Grid$id)

#grid data are only those cells containing data. This layers is created in ArcGIS. 
nb1 <- spdep::poly2nb(Grid, row.names=Grid$data); nb1
is.symmetric.nb(nb1, verbose = FALSE, force = TRUE)
nb2INLA("nb1.graph", nb1)
nb1.adj <- paste(getwd(),"/nb1.graph", sep="")
g1 <- inla.read.graph("nb1.graph")

grid<-NULL
grid<-loc.dat %>% select(RouteIdentifier, latitude, longitude, cell_id)
grid<-left_join(grid, Grid, by=c("cell_id"="id"))
grid<-grid %>% select(RouteIdentifier, latitude, longitude, cell_id, province)

#rename to match the NatureCounts area_codes
#all routes for the Ontario analysis should be coded ON
grid$province[grid$province  == "MAINE"]  <-  "ON"
grid$province[grid$province  == "NEW YORK"]  <- "ON"
grid$province[grid$province  == "NEW HAMPSHIRE"]  <- "ON"
grid$province[grid$province  == "QUEBEC"]  <-  "ON"
grid$province[grid$province  == "ONTARIO"]  <-  "ON"
grid$province[grid$province  == "MINNESOTA"]  <-  "ON"
grid$province[grid$province  == "MICHIGAN"]  <-  "ON"
grid$province[is.na(grid$province)]  <- "ON"

##---------------------------------------------------------
#Set up data for analysis

dat<-dat %>% select("SiteCode", "species_id", "CommonName", "RouteIdentifier", "survey_year", "CollectorNumber", "ObservationCount")
#dat<-dat %>% filter(CommonName!="Boreal Owl")

#sp.list<-unique(dat$CommonName)
max.yr<-max(dat$survey_year)
min.yr<-min(dat$survey_year)

#load species names list
sp.names<-meta_species_taxonomy()
sp.names<-sp.names %>% select(species_id, english_name, scientific_name)

#if(collection=="ATOWLS"){
#  events$StateProvince<-"Atlantic"
#}

##----------------------------------------------------------
#Create species analysis loop

#create species list for Ontario assessment

sp.list<-c("Barred Owl", "Boreal Owl", "Great Gray Owl", "Great Horned Owl", "Northern Saw-whet Owl")

for(m in 1:length(sp.list)) {
  #m<-1 #for testing each species
  
  sp.data <-NULL 
  sp.data <- filter(dat, CommonName == sp.list[m]) %>%
    droplevels()
  sp<-sp.list[m] 
  sp.id<-unique(sp.data$species_id)
  
  print(paste("Currently analyzing species ", m, "/", sp.list[m], sep = "")) 
  
  sp.data$survey_year<-as.numeric(sp.data$survey_year)
  events$survey_year<-as.numeric(events$survey_year)
  sp.data$ObservationCount<-as.numeric(sp.data$ObservationCount)
  
  ##-----------------------------------------------------------
  #zero fill by merging with the events dataframe. 
  sp.data <- left_join(events, sp.data, by = c("SiteCode", "RouteIdentifier", "survey_year", "CollectorNumber"), multiple="all") %>% mutate(ObservationCount = replace(ObservationCount, is.na(ObservationCount), 0)) 
  
  ##-----------------------------------------------------------
  #Include back in grid id
  grid<-grid %>% select(RouteIdentifier, cell_id, province) %>% distinct()
  sp.data<- left_join(sp.data, grid, by="RouteIdentifier", multiple="all")
  sp.data<-sp.data %>% drop_na(cell_id)
  
  ##----------------------------------------------------------
  #Observations per Province summary
  route.sum<-sp.data %>% group_by(survey_year, StateProvince) %>% summarise(count = sum(ObservationCount))
  route.sum<-cast(route.sum, StateProvince~survey_year, value="count")
  write.table(route.sum, paste(out.dir, sp.list[m], "_ProvinceYearSummary.csv", sep=""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",", col.names = TRUE)
  
  #Observations per grid summary
  #grid.sum<-sp.data %>% group_by(survey_year, cell_id) %>% summarise(count = sum(ObservationCount))
  #grid.sum<-cast(grid.sum, cell_id~survey_year, value="count")  
  #write.table(grid.sum, paste(out.dir, sp.list[m], "_", collection, "_SpeciesGridCountSummary.csv", sep=""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",", col.names = TRUE)
  
  ##-----------------------------------------------------------
  #Limit to species observed at least once per route 
  #Summarize survey site to determine which species have been observed at least once (looking at the total count column) those with sum <= 1 across all survey years will be dropped from analysis (implies never observed on a route (i.e., outside range or inappropriate habitat))
  site.summ <- melt(sp.data, id.var = "RouteIdentifier",	measure.var = "ObservationCount")
  site.summ <- cast(site.summ, RouteIdentifier ~ variable,	fun.aggregate="sum")
  site.sp.list <- unique(subset(site.summ, select = c("RouteIdentifier"), ObservationCount >= 1))
  
  # Limit raw data to these species, i.e., those that were observed at least once on a route 
  sp.data <- merge(sp.data, site.sp.list, by = c("RouteIdentifier"))
  
  ##-----------------------------------------------------------
  # Limit to years when a species was observed at least a mean of 10 times per province over all years
  # Summarize years to determine which species have been observed at least once (looking at the total count column) those with sum <= 1 across all survey years will be dropped from analysis (implies never observed on a route (i.e., outside range or inappropriate habitat))
  
  year.summ<-melt(sp.data, id.var = "province",	measure.var = "ObservationCount")
  year.summ <- cast(year.summ, province ~ variable,	fun.aggregate="sum")
  year.summ$mean<-year.summ$ObservationCount/(max.yr-min.yr)
  yr.sp.list <- unique(subset(year.summ, select = c("province"), mean >= 10))
  yr.sp.list$Species<-sp.list[m]
  write.table(yr.sp.list, paste(out.dir, sp.list[m], "_ProvinceSummary.csv", sep=""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",", col.names = TRUE)
  
  #yr.summ <- melt(sp.data, id.var = "survey_year",	measure.var = "ObservationCount")
  #yr.summ <- cast(yr.summ, survey_year ~ variable,	fun.aggregate="sum")
  #yr.sp.list <- unique(subset(yr.summ, select = c("survey_year"), ObservationCount >= 1))
  
  # Limit raw data to these species, i.e., those that were observed at least once on a route 
  sp.data <- merge(sp.data, yr.sp.list, by = c("province"))
  
  ##-----------------------------------------------------------
  # Count the number of owls per route as the response variable. The number of stop on a route can be used as a covariate (or offset) in the model to control for route level effort.  
  sp.data<-sp.data %>% group_by(species_id, RouteIdentifier, survey_year, CollectorNumber, cell_id, nstop, province, latitude, longitude, protocol_id) %>% dplyr::summarise(count=sum(ObservationCount))
  sp.data$species_id<-sp.id  
  max.yr<-as.numeric(max(sp.data$survey_year))
  min.yr<-as.numeric(min(sp.data$survey_year))
  
  ##-----------------------------------------------------------
  #standardize year to max, prepare index variables 
  #where i = grid cell, k = route, t = year, e = protocol_id
  sp.data<-as.data.frame(sp.data)
  sp.data <- sp.data %>% mutate(std_yr = survey_year - max.yr)
  sp.data$ellip_e <- as.integer(factor(sp.data$protocol_id))#index for random protocol effect
  sp.data$kappa_k <- as.integer(factor(sp.data$RouteIdentifier))#index for the random site effect
  sp.data$tau_i <- sp.data$alpha_i <- as.integer(factor(sp.data$cell_id)) #index for each id intercept and slope
  sp.data<-as.data.frame(sp.data)
  
  #Specify model with year-id effects so that we can predict the annual index value for each id
  sp.data$gamma_ij <- paste0(sp.data$alpha_i, "-", sp.data$survey_year)
  sp.data$yearfac = as.factor(sp.data$survey_year)
  
    ##-----------------------------------------------------------
  #set up grid key and replace NC StateProvince Code to match Grid allocation
  grid_key<-NULL
 # grid_key <- unique(sp.data[, c("cell_id", "alpha_i")])
  grid_key<-unique(sp.data[, c("cell_id", "alpha_i", "province")])
  Grid2<-Grid %>% select(id, bcr_number)
  Grid2$id<-as.integer(Grid2$id)
  grid_key <-left_join(grid_key, Grid2, by=c("cell_id" = "id"))
  grid_key$National<-"Canada"
  row.names(grid_key) <- NULL
  
  #Limit to province with data
  grid_key <- merge(grid_key, yr.sp.list, by = c("province"))
  
  ###################################################
  #Model 1  
  
  #Formula 
  f1 <- count ~ -1 + nstop + #number of stops included as a covariate
    # cell ICAR random intercepts
    f(alpha_i, model="besag", graph=g1, constr=FALSE, scale.model=TRUE,
      hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))) +
    # cell ICAR random year slopes
    f(tau_i, std_yr, model="besag", graph=g1, constr=FALSE, scale.model=TRUE,
      hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))) +
    # random site intercepts
    f(kappa_k, model="iid", constr=TRUE,
      hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))+
    # random protocol intercepts
    f(ellip_e, model="iid", constr=TRUE,
      hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))+
    # id-year effect
    f(gamma_ij, model="iid", constr=TRUE, 
      hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))
  
  #---------------------------------------------------------
  #Sample code to test multiple models  
  #Run nbinomial, poisson, nb zip model. Select the model with the lowest DIC.                
  
  #index.nb <-index.pois <-index.zin<- NULL
  
  #index.nb <- try(inla(f1, family = "nbinomial", data = sp.data, #E = nstop,
  #                     control.predictor = list(compute = TRUE), control.compute = #list(dic=TRUE, config = TRUE), verbose =TRUE), silent = T)
  
  #index.pois <- try(inla(f1, family = "poisson", data = sp.data, #E = nstop,
  #                       control.predictor = list(compute = TRUE), control.compute #= list(dic=TRUE, config = TRUE),  verbose =TRUE), silent = T)
  
  #index.zip <- try(inla(f1, family = "zeroinflatedpoisson1", data = sp.data, #E = #nstop, 
  #                      control.predictor = list(compute = TRUE), control.compute = #list(dic=TRUE, config = TRUE), verbose =TRUE), silent = T)
  
  
  #  model<-c("nbinomial", "poisson", "zeroinflatedpoisson1")
  #  index.nb.dic<-ifelse(class(index.nb) == 'try-error', NA, #index.nb[["dic"]][["dic"]])
  #  index.pois.dic<-ifelse(class(index.pois) == 'try-error', NA, #index.pois[["dic"]][["dic"]])
  #  index.zip.dic<-ifelse(class(index.zip) == 'try-error', NA, #index.zip[["dic"]][["dic"]])
  #  dic<-c(index.nb.dic, index.pois.dic, index.zip.dic)
  #  index<-c("index.nb", "index.pois", "index.zip")
  #  t.model<-data.frame(model, index, dic)
  
  #write.table(t.model, paste(out.dir, sp.list[m], "_ModelSelection.csv", sep=""), #row.names = FALSE, append = FALSE, quote = FALSE, sep = ",", col.names = TRUE)  
  
  #  t.model<-t.model %>% slice_min(dic, na_rm = TRUE)
  #  family<-t.model[,1]
  #  index<-t.model[,2]
  
  #-----------------------------------------------------------
  ##Run nbinomal model. There was an issue with the ZIP model crashing
  #
  #index<-"index.nb"

  #rerun the top model and save output
  out1<-try(inla(f1, family = "nbinomial", data = sp.data, #E = nstop, 
                 control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), verbose =TRUE), silent = T)
  
  
  ##Cell Estiamte of Alpha
  ##---------------------------------------------------------
  #Results
  #Random spatial
  random.out<-out1$summary.hyperpar[,c("mean", "sd", "0.025quant", "0.975quant")]
  random.out<-signif(random.out, digits = 4)
  random.out$Species <- sp.list[m]
  names(random.out)[1:5] <- c("mean", "SD", "0.025quant", "0.975quant", "Speices")
  
  write.table(random.out, paste(out.dir, "Random_Summary.csv", sep=""), row.names = TRUE, append = TRUE, quote = FALSE, sep = ",", col.names = TRUE)
  
  ##Remove cells with no routes
  cells_with_counts <- unique(sp.data$alpha_i[which(!is.na(sp.data$count))])
  
  # get alpha summaries
  alph <- exp(out1$summary.random$alpha_i$`0.5quant`[cells_with_counts])
  alph_ll <- exp(out1$summary.random$alpha_i$`0.025quant`[cells_with_counts])
  alph_ul <- exp(out1$summary.random$alpha_i$`0.975quant`[cells_with_counts])
  alph_iw <- alph_ul - alph_ll
  
  # get tau summaries
  tau <- (exp(out1$summary.random$tau_i$`0.5quant`[cells_with_counts])
          - 1) * 100
  tau_ll <- (exp(out1$summary.random$tau_i$`0.025quant`[cells_with_counts])
             - 1) * 100
  tau_ul <- (exp(out1$summary.random$tau_i$`0.975quant`[cells_with_counts])
             - 1) * 100
  tau_iw <- tau_ul - tau_ll
  
  ##-----------------------------------------------------------
  #time series plots per cell
  
  #Calculate cell level index of abundance
  
  #create a loop to get abundance index output per cell-year
  
  for(k in 1:length(cells_with_counts)) {
    
    #k<-1 #for testing each cell
    
    cell1 <-NULL 
    cell1 <- cells_with_counts[k]
    
    #need to back assign the factor cell1 to its original grid_id
    cell_id<-sp.data %>% ungroup() %>% dplyr::select(cell_id, alpha_i) %>% distinct()
    grid1<- as.character(cell_id[k,"cell_id"])
    
    #median 
    d0 <- out1$summary.random$alpha_i$`0.5quant`[cell1]
    d1 <- out1$summary.random$tau_i$`0.5quant`[cell1]
    d2 <- data.frame(
      styear=as.numeric(gsub(paste0(cell1,"-"), "",
                             grep(paste0("\\b",cell1,"-"),
                                  out1$summary.random$gamma_ij$ID,
                                  value=TRUE)))- max.yr, gamma_ij=
        out1$summary.random$gamma_ij$`0.5quant`[grep(
          paste0("\\b",cell1,"-"), out1$summary.random$gamma_ij$ID)]) %>%
      arrange(styear)
    d2$x0 <- d0
    d2$x1 <- d2$styear*d1
    d2$abund <- exp(d2$x0 + d2$x1 + d2$gamma_ij)
    d2$cell<-cell1
    d2<-merge(d2, grid_key, by.x="cell", by.y="alpha_i")
    d2$taxa_code<-sp
    
    d3<-d2 %>% select(cell_id, taxa_code, styear, abund) %>% mutate(year=styear+2023) %>% select(-styear)
    
    #lci     
    l0 <- out1$summary.random$alpha_i$`0.025quant`[cell1]
    l1 <- out1$summary.random$tau_i$`0.025quant`[cell1]
    l2 <- data.frame(
      styear=as.numeric(gsub(paste0(cell1,"-"), "",
                             grep(paste0("\\b",cell1,"-"),
                                  out1$summary.random$gamma_ij$ID,
                                  value=TRUE)))- max.yr, gamma_ij=
        out1$summary.random$gamma_ij$`0.025quant`[grep(
          paste0("\\b",cell1,"-"), out1$summary.random$gamma_ij$ID)]) %>%
      arrange(styear)
    l2$x0 <- l0
    l2$x1 <- l2$styear*l1
    l2$abund_lci <- exp(l2$x0 + l2$x1 + l2$gamma_ij)
    l2$cell<-cell1
    l2<-merge(l2, grid_key, by.x="cell", by.y="alpha_i")
    
    l3<-l2 %>% select(cell_id, styear, abund_lci) %>% mutate(year=styear+2023) %>% select(-styear) 
    
    #uci  
    u0 <- out1$summary.random$alpha_i$`0.975quant`[cell1]
    u1 <- out1$summary.random$tau_i$`0.975quant`[cell1]
    u2 <- data.frame(
      styear=as.numeric(gsub(paste0(cell1,"-"), "",
                             grep(paste0("\\b",cell1,"-"),
                                  out1$summary.random$gamma_ij$ID,
                                  value=TRUE)))- max.yr, gamma_ij=
        out1$summary.random$gamma_ij$`0.975quant`[grep(
          paste0("\\b",cell1,"-"), out1$summary.random$gamma_ij$ID)]) %>%
      arrange(styear)
    u2$x0 <- u0
    u2$x1 <- u2$styear*u1
    u2$abund_uci <- exp(u2$x0 + u2$x1 + u2$gamma_ij)
    u2$cell<-cell1
    u2<-merge(u2, grid_key, by.x="cell", by.y="alpha_i")
    
    
    u3<-u2 %>% select(cell_id, styear, abund_uci) %>% mutate(year=styear+2023) %>% select(-styear)   
    
    d3<-merge(d3, l3, by=c("cell_id", "year"))
    d3<-merge(d3, u3, by=c("cell_id", "year"))
    
    d3 <-d3 %>% mutate(
      results_code="OWLS", 
   version="2023",
   area_code=d3$cell_id,
   year=d3$year,
   season="Breeding",
   period="all years",
   species_code="",
   index=d3$abund,
   stderr="",
   stdev="",
   upper_ci=d3$abund_uci,
   lower_ci=d3$abund_lci,
   species_name=d3$taxa_code,
   species_id=sp.id)
    
    d3<-left_join(d3, sp.names, by=c("species_id"))
    d3$species_sci_name<-d3$scientific_name
    
    ##LOESS SMOOTH
    if(nrow(d3)>=10){
      d3 <- d3 %>% mutate(LOESS_index = loess_func(index, year))
    }else{
      d3$LOESS_index<-""
    }
    
    ##Trend Slope 
    d2$trend_index<-exp(d1*d2$styear + d0)
    d3$trend_index<-d2$trend_index
    
    d3<-d3 %>% select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)
    
#   write.table(d3, paste(out.dir, sp.list[m], "_Cell_Indices.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
    write.table(d3, paste(out.dir, "NOS_AnnualIndices.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
    
  } #end cell specific loop
  
##-----------------------------------------------------------
#Explore posterior samples 
  
  #grid2<-grid_key %>% filter(alpha_i==cells_with_counts)
  grid2<-grid_key
  
#posterior sample 
  posterior_ss <- 1000 # change as appropriate
  samp1 <- inla.posterior.sample(posterior_ss, out1, num.threads=3)
  par_names <- as.character(attr(samp1[[1]]$latent, "dimnames")[[1]])
  post1 <- as.data.frame(sapply(samp1, function(x) x$latent))
  post1$par_names <- par_names
  
##REGION NORTH AND SOUTH NEEDS ADDED
  
#National tau
  #tau_national
  tau_nat <- tau_samps2 %>%
    ungroup() %>%  #this seems to be needed before the select function or it won't work
    dplyr::select(National, val_names) %>%
    mutate(National=factor(National)) %>%
    gather(key=key, val=val, -National) %>%
    dplyr::select(-key) %>%
    group_by(National) %>%
    summarise(med_tau=median(val), lcl_tau=quantile(val, probs=0.025),
              ucl_tau=quantile(val, probs=0.975), iw_tau=ucl_tau-lcl_tau,
              n=dplyr::n()/posterior_ss); head(tau_nat)
  tau_nat$taxa_code <- sp.list[m]
  
  #output for SoBC. This is clunky, but clear. 
  tau_nat<-tau_nat %>%  mutate(
    results_code="OWLS",
  version="2023",
  area_code="Canada",
  species_code="",
  species_id=sp.id,
  season="Breeding",
  period="all years",
  years=paste(min.yr, "-", max.yr, sep=""),
  year_start=min.yr,
  year_end=max.yr,
  trnd=tau_nat$med_tau,
  index_type="",
  upper_ci=tau_nat$ucl_tau,
  lower_ci=tau_nat$lcl_tau,
  stderr="",
  model_type="iCAR Slope",
  model_fit="",
  percent_change_low="",
  percent_change_high="",
  prob_decrease_0="",
  prob_decrease_25="",
  prob_decrease_30="",
  prob_decrease_50="",
  prob_increase_0="",
  prob_increase_33="",
  prob_increase_100="",
  suitability="",
  confidence="",
  precision_num="",
  precision_cat=ifelse(tau_nat$iw_tau<3.5, "High", ifelse(tau_nat$iw_tau>=3.5 & tau_nat$iw_tau<=6.7, "Medium", "Low")),
  coverage_num="",
  coverage_cat="",
  sample_size=tau_nat$n,
  sample_size_units="1x1 degree blocks",
  prob_LD="",
  prob_MD="",
  prob_LC="",
  prob_MI="",
  prob_LI="")
  
  tau_nat$per=max.yr-min.yr
  tau_nat$per_trend=tau_nat$med_tau/100
  tau_nat$percent_change=((1+tau_nat$per_trend)^tau_nat$per-1)*100
  
  trend.csv<-tau_nat %>% select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
  
  #Write data to table
  write.table(trend.csv, file = paste(out.dir,
                                      "NOS_TrendsSlope", ".csv", sep = ""),
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)
  
##___________________________________________________  
#Provincial alpha samples

  tmp2<-NULL
  tmp2 <- select(sp.data, species_id, survey_year, province, count)
  
  #for each sample in the posterior we want to join the predicted to tmp so that the predictions line up year and we can get the mean count by year
  nyears<-(max.yr-min.yr)+1
  pred.yr<-matrix(nrow=posterior_ss, ncol=nyears)
  
  prov.list<-unique(sp.data$province)
  
  for (h in 1:posterior_ss){
    pred<-exp(samp1[[h]]$latent[1:nrow(sp.data)])
    tmp2[ncol(tmp2)+1]<-pred
   }

  tmp1<-tmp2 %>% group_by(survey_year, province) %>% summarise_all(mean, na.rm=TRUE)
  tmp1<-tmp1 %>% rowwise() %>% mutate(index = median(c_across(V5:V1004)), lower_ci=quantile(c_across(V5:V1004), 0.025), upper_ci=quantile(c_across(V5:V1004), 0.975), stdev=sd(c_across(V5:V1004))) 
  
  mn.yr1<-NULL
  mn.yr1<-tmp1 %>% select(survey_year, province, index, lower_ci, upper_ci, stdev) %>% mutate(
##Provincial 
  results_code="OWLS",
  version="2023",
  season="Breeding",
  period="all years",
  species_code="",
  stderr="",
  species_name=sp,
  species_id=sp.id)
  
  mn.yr1$area_code<-mn.yr1$province
  mn.yr1$year=mn.yr1$survey_year
  
  mn.yr1<-left_join(mn.yr1, sp.names, by=c("species_id"))
  mn.yr1$species_sci_name<-mn.yr1$scientific_name
  
  #LOESS_index
  if(nrow(mn.yr1)>=10){
    mn.yr1 <- mn.yr1 %>% group_by(area_code) %>% mutate(LOESS_index = loess_func(index, year))
  }else{
    mn.yr1$LOESS_index<-""
  }
  
  #trend_index
  mn.yr1$trend_index<-""
  
  #alpha_samps1 <- post1[grep("alpha_i", post1$par_names), ]
  #row.names(alpha_samps1) <- NULL
  #alpha_samps1 <- alpha_samps1[cells_with_counts, 1:posterior_ss]
  #alpha_samps2 <- cbind(grid2, alpha_samps1)
  #row.names(alpha_samps2) <- NULL
  #val_names <- grep("V", names(alpha_samps2))
  
  #median alpha_prov
  #alpha_prov<-NULL
  #alpha_prov <- alpha_samps2 %>%
  #  ungroup() %>%  #this seems to be needed before the select function or it won't work
  #  dplyr::select(province, val_names) %>%
  #  mutate(province=factor(province)) %>%
  #  gather(key=key, val=val, -province) %>%
  #  dplyr::select(-key) %>%
  #  group_by(province) %>%
  #  summarise(med_alpha=median(val))
  
  #join with trends
  #tau_prov2<-NULL
  #tau_prov2<-tau_prov %>% select(province, med_tau)
  #trend.index<-NULL
  #trend.index<-full_join(alpha_prov, tau_prov2, by="province")
  
  #mn.yr1<-left_join(mn.yr1, trend.index, by=c("area_code" = "province"))
  #mn.yr1<-mn.yr1 %>% mutate(styear = max.yr-year, lg_tau = log((med_tau/100)+1))
  #mn.yr1<-mn.yr1 %>% mutate(b=-(lg_tau*med_alpha)) #calculate b
  #mn.yr1$trend_index<-exp(mn.yr1$lg_tau*mn.yr1$styear + mn.yr1$b) #y=mx+b
  
  mn.yr1<-mn.yr1 %>% select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)
  write.table(mn.yr1, paste(out.dir, "NOS_AnnualIndices.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)      
  
##National alpha samples
##___________________________________________________

  mn.yr1<-NULL
  tmp1<-NULL
  tmp1<-tmp2 %>% select(-province) %>%  group_by(survey_year) %>% summarise_all(mean, na.rm=TRUE)
  tmp1<-tmp1 %>% rowwise() %>% mutate(index = median(c_across(V5:V1004)), lower_ci=quantile(c_across(V5:V1004), 0.025), upper_ci=quantile(c_across(V5:V1004), 0.975), stdev=sd(c_across(V5:V1004))) 
  mn.yr1 <-tmp1 %>% select(survey_year, index, lower_ci, upper_ci, stdev) %>% mutate(
  results_code="OWLS",
  version="2023",
  season="Breeding",
  period="all years",
  species_code="",
  stderr="",
  species_name=sp,
  species_id=sp.id)
  
  mn.yr1$area_code<-"Canada"
  mn.yr1$year<-mn.yr1$survey_year
  
  mn.yr1<-left_join(mn.yr1, sp.names, by=c("species_id"))
  mn.yr1$species_sci_name<-mn.yr1$scientific_name
  
    if(nrow(mn.yr1)>=10){
      #mn.yr1 <- mn.yr1 %>% mutate(LOESS_index = loess_func(index, year))
      mn.yr1$LOESS_index = loess_func(mn.yr1$index, mn.yr1$year)
    }else{
    mn.yr1$LOESS_index<-""
    }
 
   #trend_index
  mn.yr1$trend_index<-""
  
  #alpha_nat
 # alpha_nat <- alpha_samps2 %>%
  #  ungroup() %>%  #this seems to be needed before the select function or it won't work
  #  dplyr::select(National, val_names) %>%
  #  mutate(National=factor(National)) %>%
  #  gather(key=key, val=val, -National) %>%
  #  dplyr::select(-key) %>%
  #  group_by(National) %>%
  #  summarise(med_alpha=median(val))
  
  #join with trends
  #tau_nat2<-NULL
  #tau_nat2<-tau_nat %>% select(med_tau)
  #trend.index<-NULL
  #trend.index<-cbind(alpha_nat, tau_nat2)
  
  #mn.yr1$med_alpha<-trend.index$med_alpha
  #mn.yr1$med_tau<-trend.index$med_tau
  #mn.yr1<-mn.yr1 %>% mutate(styear = max.yr-year, lg_tau = log((med_tau/100)+1))
  #mn.yr1<-mn.yr1 %>% mutate(b=-(lg_tau*med_alpha)) #calculate b
  #mn.yr1$trend_index<-exp(mn.yr1$lg_tau*mn.yr1$styear + mn.yr1$b) #y=mx+b
  
  mn.yr1<-mn.yr1 %>% select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)
  write.table(mn.yr1, paste(out.dir, "NOS_AnnualIndices.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)      
  
  
  ##-----------------------------------------------------------
  #Collect posterior summaries into one data frame
  
  alpha_samps1 <- post1[grep("alpha_i", post1$par_names), ]
  row.names(alpha_samps1) <- NULL
  alpha_samps1 <- alpha_samps1[cells_with_counts, 1:posterior_ss]
  alpha_samps1 <- exp(alpha_samps1) 
  alpha_samps2 <- cbind(grid2, alpha_samps1)
  row.names(alpha_samps2) <- NULL
  val_names <- grep("V", names(alpha_samps2))
  
  #alpha_prov
  alpha_prov <- alpha_samps2 %>%
    ungroup() %>%  #this seems to be needed before the select function or it won't work
    dplyr::select(province, val_names) %>%
    mutate(province=factor(province)) %>%
    gather(key=key, val=val, -province) %>%
    dplyr::select(-key) %>%
    group_by(province) %>%
    summarise(med_alpha=median(val), lcl_alpha=quantile(val, probs=0.025),
              ucl_alpha=quantile(val, probs=0.975), iw_alpha=ucl_alpha-lcl_alpha,
              n=dplyr::n()/posterior_ss); head(alpha_prov)
  alpha_prov$taxa_code <- sp.list[m]
  
  #alpha_nat
  alpha_nat <- alpha_samps2 %>%
    ungroup() %>%  #this seems to be needed before the select function or it won't work
    dplyr::select(National, val_names) %>%
    mutate(National=factor(National)) %>%
    gather(key=key, val=val, -National) %>%
    dplyr::select(-key) %>%
    group_by(National) %>%
    summarise(med_alpha=median(val), lcl_alpha=quantile(val, probs=0.025),
              ucl_alpha=quantile(val, probs=0.975), iw_alpha=ucl_alpha-lcl_alpha,
              n=dplyr::n()/posterior_ss); head(alpha_nat)
  alpha_prov$taxa_code <- sp.list[m]
  
  post_sum<-NULL
  post_sum <- data.frame(alpha_i=cells_with_counts,
                         alph, alph_ll, alph_ul, alph_iw,
                         #eps, eps_ll, eps_ul, eps_iw, eps_sig=NA,
                         tau, tau_ll, tau_ul, tau_iw, tau_sig=NA)
  post_sum$tau_sig <- ifelse((post_sum$tau_ll < 1 & post_sum$tau_ul > 1),
                             post_sum$tau_sig <- 0,
                             post_sum$tau_sig <- post_sum$tau)
  
  
  #Cell specific tau samples
  #Need to back assign the factor alpha_id to its original value
  post_sum<-merge(post_sum, cell_id, by="alpha_i")
  post_sum$taxa_code<-sp
  
  #output for SoBC. This is clunky, but clear. 
  tau_cell<-post_sum %>% mutate(
  results_code="OWLS",
  version="2023",
  area_code=cell_id,
  species_code="",
  species_id=sp.id,
  season="Breeding",
  period="all years",
  years=paste(min.yr, "-", max.yr, sep=""),
  year_start=min.yr,
  year_end=max.yr,
  index_type="",
  stderr="",
  model_type="iCAR Slope",
  model_fit="",
  percent_change_low="",
  percent_change_high="",
  prob_decrease_0="",
  prob_decrease_25="",
  prob_decrease_30="",
  prob_decrease_50="",
  prob_increase_0="",
  prob_increase_33="",
  prob_increase_100="",
  suitability="",
  confidence="",
  precision_num="",
  coverage_num="",
  coverage_cat="",
  sample_size="",
  sample_size_units="",
  prob_LD="",
  prob_MD="",
  prob_LC="",
  prob_MI="",
  prob_LI="")
  
  tau_cell$trnd<-tau_cell$tau
  tau_cell$upper_ci<-tau_cell$tau_ul
  tau_cell$lower_ci<-tau_cell$tau_ll
  tau_cell$per<-max.yr-min.yr
  tau_cell$per_trend<-tau_cell$tau/100
  tau_cell$percent_change=((1+tau_cell$per_trend)^tau_cell$per-1)*100
  tau_cell$precision_cat<-ifelse(tau_cell$tau_iw<3.5, "High", ifelse(tau_cell$tau_iw>=3.5 & tau_cell$tau_iw<=6.7, "Medium", "Low"))
  
  trend.csv<-tau_cell %>% select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
  
  # Write data to table
  write.table(trend.csv, file = paste(out.dir,
                                      "NOS_TrendsSlope", ".csv", sep = ""),
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)
  
  
  write.table(post_sum, paste(out.dir, "PosteriorSummary.csv", sep=""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
  
} # end species analysis loop
