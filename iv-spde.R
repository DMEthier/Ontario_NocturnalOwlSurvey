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

sp.list<-c("Barred Owl", "Boreal Owl", "Great Gray Owl", "Great Horned Owl", "Northern Saw-whet Owl", "American Woodcock", "Ruffed Grouse", "Wilson's Snipe")

#load species names list
sp.names<-meta_species_taxonomy()
sp.names<-sp.names %>% dplyr::select(species_id, english_name, scientific_name)

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
    
    sp.data<-left_join(sp.data, sp.names, by=c("species_id"))
    species_sci_name<-unique(sp.data$scientific_name)
    
    
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
    # Count the number of owls per route as the response variable. The number of stop on a route can be used as a covarite or offset in the model to control for route level effort. Not used in Atlantic Canada because route are mostly complete. 
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
    
   #make the mesh this way so that the point fall on the vertices of the lattice
   Loc_all<-sp.data%>% select(easting, northing) %>% 
   st_drop_geometry() %>% as.matrix()
    
   #make unique locations
   Loc_unique<-sp.data %>% dplyr::select(RouteIdentifier, easting, northing) %>%
     distinct() %>%
     dplyr::select(easting, northing) %>% distinct() %>%
     st_drop_geometry() %>%
     as.matrix()
   
   hull <- fm_extensions(
     sp.data,
     convex = c(20, 50),
     concave = c(35, 50)
   )
   
    mesh2 <-NULL
    mesh2<-fm_mesh_2d_inla(Loc_unique, 
                           boundary = hull,
                           max.edge = c(50, 50), # km inside and outside
                           cutoff = 0,
                           crs = fm_crs(sp.data))
   
      spde <- inla.spde2.pcmatern(  # pg 218 this formula should include an alpha, default is 2 if the model does not include times. 
      mesh = mesh2,
      prior.range = c(500, 0.5),
      prior.sigma = c(1, 0.5)
    )
    
      # plot mesh and save to file
      meshmap2<-NULL
      meshmap2 <- ggplot() +
        gg(data = mesh2) +
        geom_sf(data = qq, fill = NA) +
        geom_sf(data = site_map, col = "black", size = 1) +
        theme_map() + 
        theme(panel.grid.major = element_line(colour = "transparent"),
              legend.text = element_text(hjust = 0.5))  # Adjust hjust as needed
      
      
      pdf(paste(plot.dir, sp, "_MeshPlot.pdf", sep=""))
      plot(meshmap2)
      while(!is.null(dev.list())) dev.off()
      
      N<-nrow(sp.data)
      
      Covariates<- data.frame(
        Intercept=rep(1, N),
        kappa = sp.data$site_idx,
        ellip = sp.data$protocol_idx
      )
      
      #Spatial Fields
      # make index sets for the spatial model
      alpha_idx <- inla.spde.make.index(name = "alpha", n.spde = spde$n.spde) #n.repl is used to account for repeated measure at the same location over time.
      tau_idx <- inla.spde.make.index(name = "tau", n.spde = spde$n.spde) #n.repl is used to account for repeated measure at the same location over time.
      
      #Projection matrix A using all locations
      A_alph <- inla.spde.make.A(mesh=mesh2, loc=Loc_all)  # pg 218 this formula should include an alpha, default is 2 if the model does not include times.
      A_tau <- inla.spde.make.A(mesh = mesh2, loc = Loc_all, weights = sp.data$std_yr) # note weights argument
      
      #Create Stack Object for INLA
      Stack <- inla.stack (
        tag="FitGAM",
        data=list(count=as.vector(sp.data$count)), #response from the dataframe
        effects = list(Covariates=Covariates, alpha = alpha_idx, tau=tau_idx),  #covariate list
        A = list(
          1, #value of 1 to non-spatial terms
          A_alph, 
          A_tau
        )
      )
      
      #Set prior for the random effects
      prec.prior<- list(prec = list(prior = "gaussian", param=c(0,0.1))) 
      hyper.iid<-list(prec=list(prior="pc.prec", param=c(2,0.05)))
      inla.setOption(scale.model.default=TRUE)
      
      
      formula.sp<- count ~ -1 + Intercept + 
      f(kappa, model="iid", hyper=hyper.iid) + f(ellip, model="iid", hyper=hyper.iid)+ f(alpha, model =spde)+ f(tau, model =spde)
      
      
      #fit the non-spatial model using INLA
      res<-inla(formula.sp, family = "nbinomial", data = inla.stack.data(Stack), offset = log(sp.data$nstop),
               control.predictor = list(A=inla.stack.A(Stack)),
               control.compute = list(dic=TRUE, waic=TRUE, config = TRUE),
               verbose =TRUE)
    
      
    # old model code ##-----------------------------------------------------------
    # #Model Formula
    # 
    # # iid prior
    # pc_prec <- list(prior = "pcprec", param = c(1, 0.1))
    # 
    # # components
    # svc_components <- ~ -1 + nstop + #number of stops included as a covariate
    #   kappa(site_idx, model = "iid", constr = TRUE, hyper = list(prec = pc_prec)) +
    #   ellip(protocol_idx, model = "iid", constr = TRUE, hyper = list(prec = pc_prec)) +
    #   alpha(geometry, model = spde) +
    #   tau(geometry, weights = std_yr, model = spde)+
    #   # route-year effect
    #   gamma(site_year_idx, model="iid", constr=TRUE, hyper = list(prec = pc_prec)) 
    # 
    # # formula, with "." meaning "add all the model components":
    # svc_formula <- count ~ .
    # 
    # #Run Model
    # res <- bru(
    #   svc_components,
    #   like(
    #     formula = svc_formula,
    #     family = "nbinomial",
    #     data = sp.data
    #   ),
    #   options = list(
    #     control.compute = list(waic = TRUE, cpo = FALSE),
    #     control.inla = list(int.strategy = "eb"),
    #     verbose = FALSE
    #   )
    # )
    
    res$summary.hyperpar[-1, c(1, 2)]
    summary(exp(res$summary.random$alp$"0.5quant")) # exp(alpha) posterior median
    summary((exp(res$summary.random$tau$"0.5quant") - 1) * 100) # (exp(tau)-1)*100
    
    ##-----------------------------------------------------------
    #Calculate Posterior estimate of abundance
    nsamples<- 1000
    post.sample1 <-NULL #clear previous
    post.sample1<-inla.posterior.sample(nsamples, res)
    
    tmp1<-NULL
    tmp1 <- sp.data %>% dplyr::select(survey_year) %>% st_drop_geometry() 
    
    #for each sample in the posterior we want to join the predicted to tmp so that the predictions line up with doy/year and we can get the mean count by year
    for (h in 1:nsamples){
      pred<-exp(post.sample1[[h]]$latent[1:nrow(sp.data)])
      tmp1[ncol(tmp1)+1]<-pred
    }
    
    # Rename the columns from V2 to V101
    colnames(tmp1)[2:(nsamples + 1)] <- paste0("V", 2:(nsamples + 1))
    
    #will want to adjust V to match the posterior sample size   
    tmp1<-tmp1 %>% group_by(survey_year) %>% summarise_all(mean, na.rm=TRUE)
    tmp1<-tmp1 %>% rowwise() %>% mutate(index = median(c_across(V2:V101)), lower_ci=quantile(c_across(V2:V101), 0.025), upper_ci=quantile(c_across(V2:V101), 0.975), stdev=sd(c_across(V2:V101))) 
    
    #Assign data to output table 
    indices.csv<-tmp1 %>% dplyr::select(survey_year, index, lower_ci, upper_ci, stdev) %>% mutate(
      species_code = "",
      years = paste(min(sp.data$survey_year), "-", max(sp.data$survey_year), sep = ""),
      year = survey_year,
      period ="all years",
      season = "winter",
      area_code = "ON",
      model_type = "SPDE",
      species_id=sp.id,
      species_name=sp.list[m],
      species_sci_name=species_sci_name,
      error="",
      #Assing missing data fields 
      upload_id="",
      stderr="",
      trend_id="",
      smooth_upper_ci="",
      smooth_lower_ci="",
      upload_dt="",
      family="nbinomial",
      results_code = "ONOWLS",
      version = "2025",
      season="Winter",
      area_code="ON",
      trend_index="") 
    
    # Run LOESS function
    indices.csv$LOESS_index = loess_func(indices.csv$index, indices.csv$survey_year)
    
    # Order output before printing to table
    indices.csv<-indices.csv %>% dplyr::select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)
    
    # Write data to table
    write.table(indices.csv, 
                file = paste(out.dir,	"NOS_AnnualIndices.csv", sep = ""),
                row.names = FALSE, 
                append = TRUE, 
                quote = FALSE, 
                sep = ",", 
                col.names = FALSE)
    
    #sample size should be the number of routes in sp.data
    sample_s<-length(unique(sp.data$RouteIdentifier))
    
    period <- "all years"
    Y1 <- min.yr
    Y2 <- max.yr
    y1<-1
    #number of distinct survey years
    y2<-length(unique(sp.data$survey_year))

    # ##END POINT TRENDS  
    # pred.ch<-tmp1 %>% filter(survey_year %in% c(Y1, Y2)) %>% dplyr::select(-survey_year)
    # pred.ch<-t(pred.ch)
    # pred.ch<-as.data.frame(pred.ch)
    # 
    # pred.ch<-pred.ch %>% mutate(ch=(V2/V1), max_year=Y2, min_year=Y1, tr=(100*((ch^(1/(max_year-min_year)))-1)))
    # pred.ch<-pred.ch %>% reframe(trnd=median(tr), percent_change=100*(median(ch)-1), lower_ci=quantile(tr, probs=0.025), upper_ci=quantile(tr, probs=0.95), sd=sd(tr), Width_of_Credible_Interval=upper_ci-lower_ci) %>% distinct()
    # 
    
    #SLOPE TRENDS
    #Summary of the GAM smooth on year
    wy=c(y1:y2)
    pred.yr<-tmp1 %>% dplyr::select(-survey_year)
    pred.yr<-t(pred.yr)
    ne = log(pred.yr[,wy]) 
    
    #This is the slope function. 
    #It calculates the coefficient of the lm slope for each row in the smoothed output. 
    
    #slope function 1 
    slope  <-  function(x){
      return(coef(lm(x~I(y1:y2)))[2])
    }
    
    r =  apply(ne,1,slope)
    r = as.vector((exp(r)-1)*100)
    
    trend.out<-NULL
    trend.out <- data.frame(
      index_type = "Slope trend",
      trnd = median(r, na.rm = TRUE),
      lower_ci = as.numeric(quantile(r, prob = 0.025)),  # Convert to numeric
      upper_ci = as.numeric(quantile(r, prob = 0.950)),  # Convert to numeric
      sd = sd(m, na.rm = TRUE),
      per_trend = median(r, na.rm = TRUE) / 100,
      period_num = Y2 - Y1,
      percent_change = ((1 + (median(m, na.rm = TRUE) / 100)) ^ (Y2 - Y1) - 1) * 100,
      Width_of_Credible_Interval = as.numeric(quantile(r, prob = 0.950)) - as.numeric(quantile(r, prob = 0.025)))
      
      
      trend.out<-trend.out %>% mutate(model_type="SPDE", 
             model_family = "nbionomial",
             years = paste(Y1, "-", Y2, sep = ""),
             year_start=Y1, 
             year_end=Y2,
             period ="all years",
             season = "winter",
             results_code = "ONOWLS",
             area_code = "ON",
             version=2025, 
             species_code = "",
             species_id=sp.id,
             index_type="slope", 
             species_name=sp.list[m],
             species_sci_name=species_sci_name,
             stderr = "",
             model_fit = "", 	
             percent_change_low ="", 
             percent_change_high = "",
             prob_decrease_0 = "",
             prob_decrease_25 = "",
             prob_decrease_30 = "",
             prob_decrease_50 = "",
             prob_increase_0 = "",
             prob_increase_33 = "",	
             prob_increase_100 = "",
             confidence = "",
             precision_num = "",
             suitability="",
             coverage_num = "",
             coverage_cat = "",
             goal = "",
             goal_lower = "",
             sample_size = sample_s,
             sample_size_units="Number of Routes",
             sample_total = "",
             subtitle = "",
             pval = "",
             pval_str = "",
             post_prob = "",
             trnd_order = "",
             dq = "",
             prob_LD = "",
             prob_MD = "",
             prob_LC = "",
             prob_MI = "",
             prob_LI = "",
             quantile_050 = "",
             quantile_165 = "",
             quantile_835 = "",
             quantile_950 = "",
             trend_id = "",
             upload_dt = "", 
             precision_cat = ifelse(trend.out$Width_of_Credible_Interval < 3.5, "High",
                                    ifelse(trend.out$Width_of_Credible_Interval >= 3.5 & trend.out$Width_of_Credible_Interval <= 6.7, "Medium", "Low")))
    

    write.trend<-trend.out %>% dplyr::select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, index_type, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
    
    
    write.table(write.trend, 
                file = paste(out.dir, "NOS_TrendsSlope.csv", sep = ""), 
                row.names = FALSE, 
                append = TRUE, 
                quote = FALSE, 
                sep = ",", 
                col.names = FALSE)  
    
    ##-----------------------------------------------------------
    
    
    
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
      scale_label = "Mean Abundance"
    )
    
    pt <- make_plot_field(
      data_stk = out_stk[["tau_median"]],
      scale_label = "Trend"
    )
    # # sites kappa_s
    # ps <- make_plot_site(
    #   data = cbind(site_map, data.frame(value = kappa$median)),
    #   scale_label = "posterior\nmedian\nkappa"
    # )
   
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
    
    # # sites kappa_s
    # ps_range95 <- make_plot_site(
    #   data = cbind(site_map, data.frame(value = kappa$range95)),
    #   scale_label = "posterior\nrange95\nexp(kappa_s)"
    # )
    
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
    
    # Assuming you have your raster stack 'out_stk' and mesh 'mesh2'
    # Step 1: Get the vertices from the mesh
    vertices <- fmesher::fm_vertices(mesh2)
    
    # Step 2: Extract coordinates from the sf object
    # Convert the sf object to a data frame and extract coordinates
    coords <- st_coordinates(vertices)
    
    # Step 3: Create a SpatVector from the coordinates
    # Assuming the CRS is already defined in the sf object
    vertex_spatvector <- vect(coords, type = "points", crs = st_crs(vertices))
    
    # Step 4: Extract values from the raster stack at the vertex locations
    extracted_values <- terra::extract(out_stk, vertex_spatvector)
    
    # Step 5: Combine extracted values with coordinates
    # Convert the extracted values to a data frame
    extracted_df <- as.data.frame(extracted_values)
    
    # Add geometry (coordinates) to the extracted data frame
    extracted_df$geometry <- paste0("POINT(", coords[, 1], " ", coords[, 2], ")")
    
    # Assuming extracted_df is an sf object with geometry
    extracted_sf <- st_as_sf(extracted_df, wkt = "geometry", crs = st_crs(site_map)) 
    extracted_sf<-na.omit(extracted_sf)
    
    nearest_indices <- st_nearest_feature(site_map, extracted_sf)
    
    # Create a new data frame with the nearest features
    # Extract the RouteIdentifier from site_map and the corresponding values from extracted_sf
    site_map_with_nearest <- site_map %>%
      mutate(
        NearestGeometry = st_geometry(extracted_sf[nearest_indices, ]),  # Get nearest geometry
        NearestValues = extracted_sf[nearest_indices, ]  # Get other values from extracted_sf
      )
    
    # Extract the RouteIdentifier and other values into a data frame
    # Convert NearestValues to a data frame if it's an sf object
    nearest_values_df <- as.data.frame(st_drop_geometry(extracted_sf[nearest_indices, ]))
    
    # Combine the RouteIdentifier with the nearest values
    extracted <- site_map_with_nearest %>%
      select(RouteIdentifier) %>%  # Select RouteIdentifier
      bind_cols(nearest_values_df)  # Combine with nearest values
    
    ####
    alph_filter <- extracted %>% select(alpha_median, alpha_range95, geometry)
    
    alph_median <- data.frame(
      geometry=alph_filter$geometry,
      median = alph_filter$alpha_median)
    
    alph_range <- data.frame(
      geometry=alph_filter$geometry,
      range95 = alph_filter$alpha_range95)
    
    
    # sites alph_s
    pa2 <- make_plot_site(
      data = cbind(site_map, data.frame(value = alph_median$median)),
      scale_label = "Mean Abundance"
    )
    
    # range95  alpha_s
    pa2_range95 <- make_plot_site(
      data = cbind(site_map, data.frame(value = alph_range$range95)), 
      scale_label = "posterior\nrange95\nalpha"
    )
    
    tau_filter <- extracted %>% select(tau_median, tau_range95, geometry)
    
    tau_median <- data.frame(
      geometry=tau_filter$geometry,
      median = tau_filter$tau_median)
    
    tau_range <- data.frame(
      geometry=tau_filter$geometry,
      range95 = tau_filter$tau_range95)
    
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
   
    pdf(paste(plot.dir, sp.list[m], "_spdeRoutePlot.pdf", sep=""))
    multiplot(pa2, pt2)
    while(!is.null(dev.list())) dev.off()
    
    
    ##-----------------------------------------------------------
    #Print route level trends to file
    #output for SoBC. This is clunky, but clear.
    
    tau_route<-extracted %>% 
      mutate(trnd=tau_median, 
             tau_wi=as.numeric(tau_range95),
             area_code=RouteIdentifier) %>%
      select(area_code, trnd, tau_wi) %>% 
      #drop geometry
      st_drop_geometry() %>% 
      mutate(precision_cat=ifelse(tau_wi<3.5, "High", ifelse(tau_wi>=3.5 & tau_wi<=6.7, "Medium", "Low")))
    
    tau_route$results_code<-"ONOWLS"
    tau_route$version<-max.yr
    tau_route$species_code<-""
    tau_route$species_id<-sp.id
    tau_route$season<-"Breeding"
    tau_route$period<-"all years"
    tau_route$years<-paste(min.yr, "-", max.yr, sep="")
    tau_route$year_start<-min.yr
    tau_route$year_end<-max.yr
    tau_route$index_type<-""
    tau_route$stderr<-""
    tau_route$model_type<-"SPDE"
    tau_route$model_fit<-""
    
    tau_route$per<-max.yr-min.yr
    tau_route$per_trend<-tau_route$trnd/100
    tau_route$percent_change<-((1+tau_route$per_trend)^tau_route$per-1)*100
    tau_route$lower_ci=""
    tau_route$upper_ci=""
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
    tau_route$coverage_num<-""
    tau_route$coverage_cat<-""
    tau_route$sample_size<-""
    tau_route$sample_size_units<-""
    tau_route$prob_LD<-""
    tau_route$prob_MD<-""
    tau_route$prob_LC<-""
    tau_route$prob_MI<-""
    tau_route$prob_LI<-""
    
    trend.csv<-tau_route %>% select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, index_type, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
    
    # Write data to table
    write.table(trend.csv, file = paste(out.dir, 
                                        "NOS_TrendsSlope.csv", sep = ""),
                row.names = FALSE, 
                append = TRUE, 
                quote = FALSE, 
                sep = ",", 
                col.names = FALSE)
    
#     ##-----------------------------------------------------------
#     #time series plots per route
#     #calculate route level index of abundance
#     
#     #create a loop to get abundance index output per route-year
#     
#     ##Remove cells with no routes
#     routes_with_counts <- unique(sp.data$RouteIdentifier[which(!is.na(sp.data$count))])
#     
#     for(k in 1:length(routes_with_counts)) {
#       
#       #k<-1 #for testing each cell
#       
#       route1 <-NULL 
#       route1 <- routes_with_counts[k]
#       
#       #need to back assign the factor route1 to its original grid_id
#       route_id<-sp.data %>% ungroup() %>% dplyr::select(RouteIdentifier, geometry) %>% distinct()
#       
#       #join the route_id table to the vertices so that we know the features ID. 
#       vertices<-fmesher::fm_vertices(mesh2)
#       grid0<-st_join(route_id, vertices, by=geometry)
#       grid0<-as.data.frame(grid0)
#       grid0<-grid0 %>% select(RouteIdentifier, .vertex)
#       grid1<- as.integer(grid0[k,".vertex"])
#       
#       #median 
#       
#       #######   
#       d0 <- res$summary.random$alpha$`0.5quant`[grid1]
#       d1 <- res$summary.random$tau$`0.5quant`[grid1]
#       d2 <- data.frame(
#         styear=as.numeric(gsub(paste0(route1,"-"), "",
#                                grep(paste0("\\b",route1,"-"),
#                                     res$summary.random$gamma$ID,
#                                     value=TRUE)))- max.yr, gamma=
#           res$summary.random$gamma$`0.5quant`[grep(
#             paste0("\\b",route1,"-"), res$summary.random$gamma$ID)]) %>%
#         arrange(styear)
#       d2$x0 <- d0
#       d2$x1 <- d2$styear*d1
#       d2$abund <- exp(d2$x0 + d2$x1 + d2$gamma)
#       d2$grid<-grid1
#       d2$RouteIdentifier<-route1
#       d2$species_code<-sp
#       d2$survey_year<-d2$styear+max.yr
#       
#       d2<-d2 %>% select(-gamma, -x0, -x1)
#       
#       #lci     
#       l0 <- res$summary.random$alpha$`0.025quant`[grid1]
#       l1 <- res$summary.random$tau$`0.025quant`[grid1]
#       l2 <- data.frame(
#         styear=as.numeric(gsub(paste0(route1,"-"), "",
#                                grep(paste0("\\b",route1,"-"),
#                                     res$summary.random$gamma$ID,
#                                     value=TRUE)))- max.yr, gamma=
#           res$summary.random$gamma$`0.025quant`[grep(
#             paste0("\\b",route1,"-"), res$summary.random$gamma$ID)]) %>%
#         arrange(styear)
#       l2$x0 <- l0
#       l2$x1 <- l2$styear*l1
#       l2$abund_lci <- exp(l2$x0 + l2$x1 + l2$gamma)
#       l2$grid<-grid1
#       
#       l2<-l2 %>% select(-gamma, -x0, -x1)
#       
#       
#       #uci  
#       u0 <- res$summary.random$alpha$`0.975quant`[grid1]
#       u1 <- res$summary.random$tau$`0.975quant`[grid1]
#       u2 <- data.frame(
#         styear=as.numeric(gsub(paste0(route1,"-"), "",
#                                grep(paste0("\\b",route1,"-"),
#                                     res$summary.random$gamma$ID,
#                                     value=TRUE)))- max.yr, gamma=
#           res$summary.random$gamma$`0.975quant`[grep(
#             paste0("\\b",route1,"-"), res$summary.random$gamma$ID)]) %>%
#         arrange(styear)
#       u2$x0 <- u0
#       u2$x1 <- u2$styear*u1
#       u2$abund_uci <- exp(u2$x0 + u2$x1 + u2$gamma)
#       u2$grid<-grid1
#       
#       u2<-u2 %>% select(-gamma, -x0, -x1)
#       
#       
#       #######  
#       
#       d3<-NULL   
#       d3<-merge(d2, l2, by=c("grid", "styear"))
#       d3<-merge(d3, u2, by=c("grid", "styear"))
#       
#       d3$index<-d3$abund
#       d3$upper_ci<-d3$abund_uci
#       d3$lower_ci<-d3$abund_lci
#       d3$year<-d3$styear+max.yr
#       d3$results_code<-"OWLS"
#       d3$version<-max.yr
#       d3$area_code<-d3$RouteIdentifier
#       d3$season<-"Breeding"
#       d3$period<-"all years"
#       d3$stderr<-""
#       d3$stdev<-""
#       d3$species_id<-sp.id
#       d3<-left_join(d3, sp.names, by=c("species_id"))
#       d3$species_sci_name<-d3$scientific_name
#       d3$species_name<-d3$species_code
#       d3$species_code<-""
#       
#       ##Trend LOESS_index
#       if(nrow(d3)>=10){
#         d3 <- d3 %>% mutate(LOESS_index = loess_func(index, year))
#       }else{
#         d3$LOESS_index<-""
#       }
#       
#       ##Trend_Index 
#       d2$trend_index<-exp(d1*d2$styear + d0)
#       d3$trend_index<-d2$trend_index
#       
#       d3<-d3 %>% select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)
#       
#       write.table(d3, paste(out.dir, "NOS_AnnualIndices.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
#       
#     } #end route specific loop
#     
#     
 #}#end boreal loop
 }#end species loop
# 
# # plot mesh and save to file
# meshmap2<-ggplot() +
#   gg(data = mesh2) +
#   geom_sf(data = qq, fill = NA) +
#   geom_sf(data = site_map, col = "darkgreen", size = 1) +
#   theme_bw() +
#   labs(x = "", y = "")  
# 
# pdf(paste(plot.dir, collection, "_MeshPlot.pdf", sep=""))
# plot(meshmap2)
# while(!is.null(dev.list())) dev.off()
# 
