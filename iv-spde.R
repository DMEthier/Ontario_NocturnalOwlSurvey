#spde analysis

dat<-read.csv(paste(out.dir, collection, "OwlDataClean.csv", sep=""))
dat<-dat %>% na.omit()

events<-read.csv(paste(out.dir, collection, "Events.csv", sep=""))
events<-events %>% na.omit() %>% distinct()

loc.dat<-read.csv(paste(out.dir, collection, "Map.csv", sep=""))
loc.dat<-loc.dat %>% na.omit() %>% distinct()


events<-events %>% filter(survey_year>=min.yr & survey_year<=max.yr) %>% filter(survey_year!=2020)#remove covid year
dat<-dat %>% filter(survey_year>=min.yr & survey_year<=max.yr) %>% filter(survey_year!=2020) #remove covid year

#Add UTM coords
UTM<-lonlat2utm( 
 events$longitude,events$latitude
)

events$Easting<-UTM$easting
events$Northing<-UTM$northing

events<-events %>% select(SiteCode, RouteIdentifier, survey_year, nstop, latitude, longitude, Easting, Northing, protocol_id) %>% distinct()

##---------------------------------------------------------
#Set up data for analysis
dat<-dat %>% dplyr::select("SiteCode", "species_id", "CommonName", "RouteIdentifier", "survey_year", "ObservationCount", "StateProvince")
#sp.list<-unique(dat$CommonName)

#create species list for national assessment

sp.list<-c("Barred Owl", "Boreal Owl", "Great Gray Owl", "Great Horned Owl", "Northern Saw-whet Owl")

#load species names list
sp.names<-meta_species_taxonomy()
sp.names<-sp.names %>% dplyr::select(species_id, english_name, scientific_name)

#if(collection == "ATOWLS"){
#  events$StateProvince<- "Atlantic"
#  dat$StateProvince<-"Atlantic"
#}

##----------------------------------------------------------
#Create species analysis loop

for(m in 1:length(sp.list)) {
  #m<-1 #for testing each species
  
 # if(sp.list[m] != "Boreal Owl") { #do not run if Boreal Owl. To few routes with 10 years of data. 
    
    sp.data <-NULL 
    sp.data <- dplyr::filter(dat, CommonName == sp.list[m]) %>%
      droplevels()
    sp<-sp.list[m] 
    sp.id<-unique(sp.data$species_id)
    
    print(paste("Currently analyzing species ", m, "/", sp.list[m], sep = "")) 
    
    ##-----------------------------------------------------------
    #zero fill by merging with the events dataframe. 
    sp.data <- left_join(events, sp.data, by = c("SiteCode", "RouteIdentifier", "survey_year"), multiple="all") %>% mutate(ObservationCount = replace(ObservationCount, is.na(ObservationCount), 0)) 
    
    sp.data$CommonName<-sp
    sp.data$species_id<-sp.id
    sp.data$StateProvince<-"ON"
    #sp.data$StateProvince<-StateProv
    
    ##-----------------------------------------------------------
    #Remove routes that do not have at least one observation of the species
    site.summ <- melt(sp.data, id.var = "RouteIdentifier",	measure.var = "ObservationCount")
    site.summ <- cast(site.summ, RouteIdentifier ~ variable,	fun.aggregate="sum")
    site.sp.list <- unique(subset(site.summ, select = c("RouteIdentifier"), ObservationCount >= 1))
    
    # Limit raw data to these species, i.e., those that were observed at least once on a route 
    sp.data <- merge(sp.data, site.sp.list, by = c("RouteIdentifier"))
    
    ##-----------------------------------------------------------
    # Limit to years when a species was observed at least once  
    # Summarize years to determine which species have been observed at least once (looking at the total count column) those with sum <= 1 across all survey years will be dropped from analysis (implies never observed on a route (i.e., outside range or inappropriate habitat))
    yr.summ <- melt(sp.data, id.var = "survey_year",	measure.var = "ObservationCount")
    yr.summ <- cast(yr.summ, survey_year ~ variable,	fun.aggregate="sum")
    yr.sp.list <- unique(subset(yr.summ, select = c("survey_year"), ObservationCount >= 1))
    
    # Limit raw data to these species, i.e., those that were observed at least once per year
    sp.data <- merge(sp.data, yr.sp.list, by = c("survey_year"))
    max.yr<-max(sp.data$survey_year)
    min.yr<-min(sp.data$survey_year)
    
    ##-----------------------------------------------------------
    # Count the number of owls per route as the response variable. The number of stop on a route can be used as a covarite in the model to control for route level effort. Not used in Atlantic Canada because route are mostly complete. 
    sp.data<-sp.data %>% group_by(RouteIdentifier, survey_year, StateProvince, latitude, longitude, Easting, Northing, protocol_id, nstop) %>% summarise(count=sum(ObservationCount))
    
    ##-----------------------------------------------------------
    #Create index variables
    sp.data <- sp.data %>% mutate(site_idx = factor(paste(RouteIdentifier))) %>% 
      group_by(site_idx) %>% 
      mutate(n_years = dplyr::n()) %>%
      filter(n_years >= 10) %>% #remove routes with <10 years of data
      ungroup() %>%
      mutate(
        std_yr = survey_year - max.yr,
        obs = seq_len(nrow(.)),
        site_idx = as.numeric(factor(paste(RouteIdentifier))),
        protocol_idx = as.numeric(factor(paste(protocol_id))),
        year_idx = as.numeric(factor(survey_year)),
        site_year_idx = paste0(RouteIdentifier, "-", survey_year)) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
      st_transform(epsg6703km) %>%
     mutate(
        easting = st_coordinates(.)[, 1],
        northing = st_coordinates(.)[, 2]) %>% 
      arrange(RouteIdentifier, survey_year)
    
    #Make a set of distinct study sites for mapping
    site_map <- sp.data %>%
      select(RouteIdentifier, easting, northing) %>%
      distinct() %>%
      select(RouteIdentifier, easting, northing)
    
    sampleplot<-ggplot() +
      geom_sf(data = sp.data, aes(col = count)) +
      geom_sf(data = qq, fill = NA) +
      coord_sf(datum = NA) +
      facet_wrap(~survey_year) +
      scale_color_distiller(palette = "Spectral") +
      theme_bw()  
    
    pdf(paste(plot.dir, sp.list[m], "_SamplePlot.pdf", sep=""))
    plot(sampleplot)
    while(!is.null(dev.list())) dev.off()
    
    ##-----------------------------------------------------------
    #Make a set of distinct study sites for mapping    
    #Make a two extension hulls and mesh for spatial model
    
    #sp.data<-as.matrix(sp.data)  
    hull <- fm_extensions(
      sp.data,
      convex = c(20, 50),
      concave = c(35, 50)
    )
    
   #make the mesh this way so that the point fall on the vertices of the lattice
   Loc<-site_map%>% select(easting, northing) %>% 
   st_drop_geometry() %>% as.matrix()
    
   mesh2<-fm_mesh_2d_inla(Loc, 
                           #mesh2<-inla.mesh.2d(Loc, 
                           boundary = hull,
                           max.edge = c(50, 200), # km inside and outside
                           cutoff = 0,
                           crs = fm_crs(sp.data))
   
      spde <- inla.spde2.pcmatern(  # pg 218 this formula should include an alpha, default is 2 if the model does not include times. 
      mesh = mesh2,
      prior.range = c(500, 0.5),
      prior.sigma = c(1, 0.5)
    )
    
      # plot mesh and save to file
      meshmap2<-ggplot() +
        gg(data = mesh2) +
        geom_sf(data = qq, fill = NA) +
        geom_sf(data = site_map, col = "black", size = 1) +
        theme_map() + theme(panel.grid.major=element_line(colour="transparent"))  
      
      pdf(paste(out.dir, sp, "_MeshPlot.pdf", sep=""))
      plot(meshmap2)
      while(!is.null(dev.list())) dev.off()
      
    ##-----------------------------------------------------------
    #Model Formula
    
    # iid prior
    pc_prec <- list(prior = "pcprec", param = c(1, 0.1))
    
    # components
    svc_components <- ~ -1 + nstop + #number of stops included as a covariate
      kappa(site_idx, model = "iid", constr = TRUE, hyper = list(prec = pc_prec)) +
      ellip(protocol_idx, model = "iid", constr = TRUE, hyper = list(prec = pc_prec)) +
      alpha(geometry, model = spde) +
      tau(geometry, weights = std_yr, model = spde)+
      # route-year effect
      gamma(site_year_idx, model="iid", constr=TRUE, hyper = list(prec = pc_prec)) 
    
    # formula, with "." meaning "add all the model components":
    svc_formula <- count ~ .
    
    #Run Model
    res <- bru(
      svc_components,
      like(
        formula = svc_formula,
        family = "nbinomial",
        data = sp.data
      ),
      options = list(
        control.compute = list(waic = TRUE, cpo = FALSE),
        control.inla = list(int.strategy = "eb"),
        verbose = FALSE
      )
    )
    
    res$summary.hyperpar[-1, c(1, 2)]
    summary(exp(res$summary.random$alp$"0.5quant")) # exp(alpha) posterior median
    summary((exp(res$summary.random$tau$"0.5quant") - 1) * 100) # (exp(tau)-1)*100
    
    #SVC Map
    #get easting and northing limits
    bbox <- fm_bbox(hull[[1]])
    grd_dims <- round(c(x = diff(bbox[[1]]), y = diff(bbox[[2]])) / 25)
    
    #bbox <- fm_bbox(mesh2$loc)
    #grd_dims <- round(c(x = diff(bbox[[1]]), y = #diff(bbox[[2]])))
    
    # make mesh projector to get model summaries from the mesh to the mapping grid
    mesh_proj <- fm_evaluator(mesh2,
                              xlim = bbox[[1]], ylim = bbox[[2]], dims = grd_dims
    )
    
    #mesh_proj<-inla.mesh.projector(mesh2, #xlim=range(Loc[,1]), ylim=range(Loc[,2]), #dims=grd_dims)
    
    #Pull data
    kappa <- data.frame(
      median = exp(res$summary.random$kappa$"0.5quant"),
      range95 = exp(res$summary.random$kappa$"0.975quant") -
        exp(res$summary.random$kappa$"0.025quant")
    )
    
    alph <- data.frame(
      median = exp(res$summary.random$alpha$"0.5quant"),
      range95 = exp(res$summary.random$alpha$"0.975quant") -
        exp(res$summary.random$alpha$"0.025quant")
    )
    
    taus <- data.frame(
      median = (exp(res$summary.random$tau$"0.5quant") - 1) * 100,
      range95 = (exp(res$summary.random$tau$"0.975quant") -
                   exp(res$summary.random$tau$"0.025quant")) * 100
    )
    
    # loop to get estimates on a mapping grid
    pred_grids <- lapply(
      list(alpha = alph, tau = taus),
      function(x) as.matrix(fm_evaluate(mesh_proj, x))
    )
    
      ##-----------------------------------------------------------
    # make a terra raster stack with the posterior median and range95
    out_stk <- rast()
    for (j in 1:2) {
      mean_j <- cbind(expand.grid(x = mesh_proj$x, y = mesh_proj$y),
                      Z = c(matrix(pred_grids[[j]][, 1], grd_dims[1]))
      )
      mean_j <- rast(mean_j, crs = epsg6703km)
      range95_j <- cbind(expand.grid(X = mesh_proj$x, Y = mesh_proj$y),
                         Z = c(matrix(pred_grids[[j]][, 2], grd_dims[1]))
      )
      range95_j <- rast(range95_j, crs = epsg6703km)
      out_j <- c(mean_j, range95_j)
      terra::add(out_stk) <- out_j
    }
    
    names(out_stk) <- c("alpha_median", "alpha_range95", "tau_median", "tau_range95")
    out_stk <- terra::mask(out_stk, qq, touches = FALSE)
    
    ##-----------------------------------------------------------
    # medians
    # fields alpha_s, tau_s
    pa <- make_plot_field(
      data_stk = out_stk[["alpha_median"]],
      scale_label = "Mean Abundance 2023a"
    )
    
    pt <- make_plot_field(
      data_stk = out_stk[["tau_median"]],
      scale_label = "Trend"
    )
    # sites kappa_s
    ps <- make_plot_site(
      data = cbind(site_map, data.frame(value = kappa$median)),
      scale_label = "posterior\nmedian\nkappa"
    )
    # range95
    # fields alpha_s, tau_s
    pa_range95 <- make_plot_field(
      data_stk = out_stk[["alpha_range95"]],
      scale_label = "posterior\nrange95\nexp(alpha_s)"
    )
    
    pt_range95 <- make_plot_field(
      data_stk = out_stk[["tau_range95"]],
      scale_label = "posterior\nrange95\n100(exp(tau_s)-1)"
    )
    
    # sites kappa_s
    ps_range95 <- make_plot_site(
      data = cbind(site_map, data.frame(value = kappa$range95)),
      scale_label = "posterior\nrange95\nexp(kappa_s)"
    )
    
    # plot together
    #multiplot(ps, pa, pt, cols = 2)
    
    # plot together
    #multiplot(ps_range95, pa_range95, pt_range95, cols = 2)
    
    # plot together
    #multiplot(ps, pa, pt, ps_range95, pa_range95, pt_range95, cols = 2)
    
    pdf(paste(plot.dir, sp.list[m], "_spdePlot.pdf", sep=""))
    multiplot(pa, pt)
    while(!is.null(dev.list())) dev.off()
    
        #___________________________________________________
    #Predict the SPDE and covariates at the vertex locations
    
    vertices<-fmesher::fm_vertices(mesh2)
    
    # predict on the vertices
    pred <- predict(
      res,
      vertices)
    
    alph_filter <- pred$alpha
    alph_filter<-alph_filter %>% filter(geometry %in% site_map$geometry)
    
    alph_median <- data.frame(
      geometry=alph_filter$geometry,
      median = exp(alph_filter$"q0.5"))
    
    alph_range <- data.frame(
      geometry=alph_filter$geometry,
      range95 = exp(alph_filter$"q0.975") -
        exp(alph_filter$"q0.025")
    )
    
    # sites alph_s
    pa2 <- make_plot_site(
      data = cbind(site_map, data.frame(value = alph_median$median)),
      scale_label = "Mean Abundance 2023"
    )
    
    # range95  alpha_s
    pa2_range95 <- make_plot_site(
      data = cbind(site_map, data.frame(value = alph_range$range95)), 
      scale_label = "posterior\nrange95\nalpha"
    )
    
    tau_filter <- pred$tau
    tau_filter<-tau_filter %>% filter(geometry %in% site_map$geometry)
    
    tau_median <- data.frame(
      geometry=tau_filter$geometry,
      median = (exp(tau_filter$"q0.5") -1)*100)
    
    tau_lci <- data.frame(
      geometry=tau_filter$geometry,
      tau_ll = (exp(tau_filter$"q0.025") -1)*100)
    
    tau_uci <- data.frame(
      geometry=tau_filter$geometry,
      tau_ul = (exp(tau_filter$"q0.975") -1)*100)
    
    tau_range <- data.frame(
      geometry=tau_filter$geometry,
      range95 = (exp(tau_filter$"q0.975") -
                   exp(tau_filter$"q0.025")) *100)
    
    # sites tau_s
    pt2 <- make_plot_site(
      data = cbind(site_map, data.frame(value = tau_median$median)),
      scale_label = "Trend"
    )
    
    # range95  tau_s
    pt2_range95 <- make_plot_site(
      data = cbind(site_map, data.frame(value = tau_range$range95)), 
      scale_label = "posterior\nrange95\ntau"
    )
    
    # sites kappa_s
    ps <- make_plot_site(
      data = cbind(site_map, data.frame(value = kappa$median)),
      scale_label = "posterior\nmedian\nkappa"
    )
    
    # sites kappa_s
    ps_range95 <- make_plot_site(
      data = cbind(site_map, data.frame(value = kappa$range95)),
      scale_label = "posterior\nrange95\nkappa"
    )
    
    # plot together
    #multiplot(ps, pa2, pt2, ps_range95, pa2_range95, pt2_range95, cols = 2)
    #multiplot(pa, pt, pa2, pt2, cols=2)
    
    pdf(paste(plot.dir, sp.list[m], "_spdeRoutePlot.pdf", sep=""))
    multiplot(pa2, pt2)
    while(!is.null(dev.list())) dev.off()
    
    
    ##-----------------------------------------------------------
    #Print route level trends to file
    #output for SoBC. This is clunky, but clear.
    
    tau_route<-data.frame(
      tau=tau_median$median, 
      lower_ci=tau_lci$tau_ll,
      upper_ci=tau_uci$tau_ul,
      tau_wi=tau_range$range95,
      sd=tau_filter$sd,
      geometry=tau_filter$geometry)
    
    coord<-st_coordinates(tau_filter$geometry)
    tau_route<-cbind(tau_route, coord)
    tau_route<-tau_route %>% dplyr::rename(easting = X, northing = Y)
    
    ##HERE
    tau_route<-merge(tau_route, site_map, c("easting", "northing"))
    
    tau_route$results_code<-"OWLS"
    tau_route$version<-max.yr
    tau_route$area_code<-tau_route$RouteIdentifier
    tau_route$species_code<-""
    tau_route$species_id<-sp.id
    tau_route$season<-"Breeding"
    tau_route$period<-"all years"
    tau_route$years<-paste(min.yr, "-", max.yr, sep="")
    tau_route$year_start<-min.yr
    tau_route$year_end<-max.yr
    tau_route$trnd<-tau_route$tau
    tau_route$index_type<-""
    tau_route$stderr<-""
    tau_route$model_type<-"SPDE"
    tau_route$model_fit<-""
    
    tau_route$per<-max.yr-min.yr
    tau_route$per_trend<-tau_route$tau/100
    tau_route$percent_change<-((1+tau_route$per_trend)^tau_route$per-1)*100
    
    tau_route$percent_change_low<-""
    tau_route$percent_change_high<-""
    tau_route$prob_decrease_0<-""
    tau_route$prob_decrease_25<-""
    tau_route$prob_decrease_30<-""
    tau_route$prob_decrease_50<-""
    tau_route$prob_increase_0<-""
    tau_route$prob_increase_33<-""
    tau_route$prob_increase_100<-""
    tau_route$suitability<-""
    tau_route$confidence<-""
    tau_route$precision_num<-""
    tau_route$precision_cat<-ifelse(tau_route$tau_wi<3.5, "High", ifelse(tau_route$tau_wi>=3.5 & tau_route$tau_wi<=6.7, "Medium", "Low"))
    tau_route$coverage_num<-""
    tau_route$coverage_cat<-""
    tau_route$sample_size<-""
    tau_route$sample_size_units<-""
    tau_route$prob_LD<-""
    tau_route$prob_MD<-""
    tau_route$prob_LC<-""
    tau_route$prob_MI<-""
    tau_route$prob_LI<-""
    
    trend.csv<-tau_route %>% select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
    
    # Write data to table
    write.table(trend.csv, file = paste(out.dir, 
                                        "NOS_TrendsSlope", ".csv", sep = ""),
                row.names = FALSE, 
                append = TRUE, 
                quote = FALSE, 
                sep = ",", 
                col.names = FALSE)
    
    ##-----------------------------------------------------------
    #time series plots per route
    #calculate route level index of abundance
    
    #create a loop to get abundance index output per route-year
    
    ##Remove cells with no routes
    routes_with_counts <- unique(sp.data$RouteIdentifier[which(!is.na(sp.data$count))])
    
    for(k in 1:length(routes_with_counts)) {
      
      #k<-1 #for testing each cell
      
      route1 <-NULL 
      route1 <- routes_with_counts[k]
      
      #need to back assign the factor route1 to its original grid_id
      route_id<-sp.data %>% ungroup() %>% dplyr::select(RouteIdentifier, geometry) %>% distinct()
      
      #join the route_id table to the vertices so that we know the features ID. 
      vertices<-fmesher::fm_vertices(mesh2)
      grid0<-st_join(route_id, vertices, by=geometry)
      grid0<-as.data.frame(grid0)
      grid0<-grid0 %>% select(RouteIdentifier, .vertex)
      grid1<- as.integer(grid0[k,".vertex"])
      
      #median 
      
      #######   
      d0 <- res$summary.random$alpha$`0.5quant`[grid1]
      d1 <- res$summary.random$tau$`0.5quant`[grid1]
      d2 <- data.frame(
        styear=as.numeric(gsub(paste0(route1,"-"), "",
                               grep(paste0("\\b",route1,"-"),
                                    res$summary.random$gamma$ID,
                                    value=TRUE)))- max.yr, gamma=
          res$summary.random$gamma$`0.5quant`[grep(
            paste0("\\b",route1,"-"), res$summary.random$gamma$ID)]) %>%
        arrange(styear)
      d2$x0 <- d0
      d2$x1 <- d2$styear*d1
      d2$abund <- exp(d2$x0 + d2$x1 + d2$gamma)
      d2$grid<-grid1
      d2$RouteIdentifier<-route1
      d2$species_code<-sp
      d2$survey_year<-d2$styear+max.yr
      
      d2<-d2 %>% select(-gamma, -x0, -x1)
      
      #lci     
      l0 <- res$summary.random$alpha$`0.025quant`[grid1]
      l1 <- res$summary.random$tau$`0.025quant`[grid1]
      l2 <- data.frame(
        styear=as.numeric(gsub(paste0(route1,"-"), "",
                               grep(paste0("\\b",route1,"-"),
                                    res$summary.random$gamma$ID,
                                    value=TRUE)))- max.yr, gamma=
          res$summary.random$gamma$`0.025quant`[grep(
            paste0("\\b",route1,"-"), res$summary.random$gamma$ID)]) %>%
        arrange(styear)
      l2$x0 <- l0
      l2$x1 <- l2$styear*l1
      l2$abund_lci <- exp(l2$x0 + l2$x1 + l2$gamma)
      l2$grid<-grid1
      
      l2<-l2 %>% select(-gamma, -x0, -x1)
      
      
      #uci  
      u0 <- res$summary.random$alpha$`0.975quant`[grid1]
      u1 <- res$summary.random$tau$`0.975quant`[grid1]
      u2 <- data.frame(
        styear=as.numeric(gsub(paste0(route1,"-"), "",
                               grep(paste0("\\b",route1,"-"),
                                    res$summary.random$gamma$ID,
                                    value=TRUE)))- max.yr, gamma=
          res$summary.random$gamma$`0.975quant`[grep(
            paste0("\\b",route1,"-"), res$summary.random$gamma$ID)]) %>%
        arrange(styear)
      u2$x0 <- u0
      u2$x1 <- u2$styear*u1
      u2$abund_uci <- exp(u2$x0 + u2$x1 + u2$gamma)
      u2$grid<-grid1
      
      u2<-u2 %>% select(-gamma, -x0, -x1)
      
      
      #######  
      
      d3<-NULL   
      d3<-merge(d2, l2, by=c("grid", "styear"))
      d3<-merge(d3, u2, by=c("grid", "styear"))
      
      d3$index<-d3$abund
      d3$upper_ci<-d3$abund_uci
      d3$lower_ci<-d3$abund_lci
      d3$year<-d3$styear+max.yr
      d3$results_code<-"OWLS"
      d3$version<-max.yr
      d3$area_code<-d3$RouteIdentifier
      d3$season<-"Breeding"
      d3$period<-"all years"
      d3$stderr<-""
      d3$stdev<-""
      d3$species_id<-sp.id
      d3<-left_join(d3, sp.names, by=c("species_id"))
      d3$species_sci_name<-d3$scientific_name
      d3$species_name<-d3$species_code
      d3$species_code<-""
      
      ##Trend LOESS_index
      if(nrow(d3)>=10){
        d3 <- d3 %>% mutate(LOESS_index = loess_func(index, year))
      }else{
        d3$LOESS_index<-""
      }
      
      ##Trend_Index 
      d2$trend_index<-exp(d1*d2$styear + d0)
      d3$trend_index<-d2$trend_index
      
      d3<-d3 %>% select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)
      
      write.table(d3, paste(out.dir, "NOS_AnnualIndices.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
      
    } #end route specific loop
    
    
 # }#end boreal loop
}#end species loop

# plot mesh and save to file
meshmap2<-ggplot() +
  gg(data = mesh2) +
  geom_sf(data = qq, fill = NA) +
  geom_sf(data = site_map, col = "darkgreen", size = 1) +
  theme_bw() +
  labs(x = "", y = "")  

pdf(paste(plot.dir, collection, "_MeshPlot.pdf", sep=""))
plot(meshmap2)
while(!is.null(dev.list())) dev.off()

