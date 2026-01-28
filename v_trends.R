##---------------------------------------------------------
## DATA PREP (unchanged except for spatial bits removed)
##---------------------------------------------------------

dat <- read.csv(paste(out.dir, collection, "OwlDataClean.csv", sep=""))
dat <- dat %>% na.omit()

events <- read.csv(paste(out.dir, collection, "Events.csv", sep=""))
events <- events %>% na.omit() %>% distinct()

loc.dat <- read.csv(paste(out.dir, collection, "Map.csv", sep=""))
loc.dat <- loc.dat %>% na.omit() %>% distinct() %>% filter(latitude != "NULL")

events <- events %>% 
  filter(survey_year >= min.yr & survey_year <= max.yr) %>% 
  filter(survey_year != 2020)

dat <- dat %>% 
  filter(survey_year >= min.yr & survey_year <= max.yr) %>% 
  filter(survey_year != 2020)

##---------------------------------------------------------
## BASIC FIELDS
##---------------------------------------------------------

dat <- dat %>% 
  dplyr::select("SiteCode", "species_id", "CommonName", "RouteIdentifier", 
                "survey_year", "CollectorNumber", "ObservationCount")

max.yr <- max(dat$survey_year)
min.yr <- min(dat$survey_year)

sp.names <- meta_species_taxonomy() %>% 
  dplyr::select(species_id, english_name, scientific_name)

## Species list
sp.list <- c(
  "Barred Owl", "Boreal Owl", "Great Gray Owl", "Great Horned Owl",
  "Northern Saw-whet Owl", "American Woodcock", "Ruffed Grouse", "Wilson's Snipe"
)

##---------------------------------------------------------
## SPECIES LOOP
##---------------------------------------------------------

for (m in 1:length(sp.list)) {
  
  sp.data <- filter(dat, CommonName == sp.list[m]) %>% droplevels()
  sp      <- sp.list[m]
  sp.id   <- unique(sp.data$species_id)
  
  message(paste("Currently analyzing species ", m, "/", sp.list[m], sep = ""))
  
  sp.data$survey_year     <- as.numeric(sp.data$survey_year)
  events$survey_year      <- as.numeric(events$survey_year)
  sp.data$ObservationCount <- as.numeric(sp.data$ObservationCount)
  sp.data$StateProvince <- "ON"
  
  ##-----------------------------------------------------------
  ## ZERO FILL
  ##-----------------------------------------------------------
  
  sp.data <- left_join(
    events, sp.data,
    by = c("SiteCode", "RouteIdentifier", "survey_year", "CollectorNumber"),
    multiple = "all"
  ) %>% 
    mutate(ObservationCount = replace(ObservationCount, is.na(ObservationCount), 0))
  
  sp.data$StateProvince<-"Ontario"
  
  ##-----------------------------------------------------------
  ## OBS PER PROVINCE SUMMARY
  ##-----------------------------------------------------------
  
  route.sum <- sp.data %>% 
    group_by(survey_year, StateProvince) %>% 
    summarise(count = sum(ObservationCount), .groups = "drop")
  
  route.sum <- cast(route.sum, StateProvince ~ survey_year, value = "count")
  
  write.table(
    route.sum,
    paste(out.dir, sp.list[m], "_ProvinceYearSummary.csv", sep = ""),
    row.names = FALSE, append = FALSE, quote = FALSE, sep = ",", col.names = TRUE
  )
  
  ##-----------------------------------------------------------
  ## FILTER BY SITES THAT EVER DETECT SPECIES
  ##-----------------------------------------------------------
  
  site.summ <- melt(sp.data, id.var = "RouteIdentifier",
                    measure.var = "ObservationCount")
  site.summ <- cast(site.summ, RouteIdentifier ~ variable, fun.aggregate = "sum")
  site.sp.list <- unique(subset(site.summ, select = c("RouteIdentifier"),
                                ObservationCount >= 1))
  
  sp.data <- merge(sp.data, site.sp.list, by = c("RouteIdentifier"))
  
  ##-----------------------------------------------------------
  ## FILTER BY Protocol_ID WITH MEAN >= 10
  ##-----------------------------------------------------------
  
  year.summ <- melt(sp.data, id.var = "protocol_id",
                    measure.var = "ObservationCount")
  year.summ <- cast(year.summ, protocol_id ~ variable, fun.aggregate = "sum")
  
  year.summ$mean <- year.summ$ObservationCount / (max.yr - min.yr)
  yr.sp.list <- unique(subset(year.summ, select = c("protocol_id"), mean >= 10))
  yr.sp.list$Species <- sp.list[m]
  
  write.table(
    yr.sp.list,
    paste(out.dir, sp.list[m], "_ProvinceSummary.csv", sep = ""),
    row.names = FALSE, append = FALSE, quote = FALSE, sep = ",", col.names = TRUE
  )
  
  sp.data <- merge(sp.data, yr.sp.list, by = c("protocol_id"))
  
  ##-----------------------------------------------------------
  ## COLLAPSE TO ROUTE × YEAR × PROTOCOL
  ##-----------------------------------------------------------
  
  sp.data <- sp.data %>% 
    group_by(
      species_id, RouteIdentifier, survey_year, CollectorNumber,
      nstop, StateProvince, latitude, longitude, protocol_id
    ) %>% 
    summarise(count = sum(ObservationCount), .groups = "drop")
  
  sp.data$species_id <- sp.id
  
  max.yr <- as.numeric(max(sp.data$survey_year))
  min.yr <- as.numeric(min(sp.data$survey_year))
  
  ##-----------------------------------------------------------
  ## INDEX VARIABLES (NO SPATIAL STRUCTURE)
  ##-----------------------------------------------------------
  
  sp.data <- as.data.frame(sp.data)
  
  sp.data <- sp.data %>%
    mutate(
      std_yr      = survey_year - max.yr,
      ellip_e     = factor(protocol_id),   # protocol random effect
      kappa_k     = as.integer(factor(RouteIdentifier)), # site random effect
      yearfac     = as.factor(survey_year)
    )
  
  
  ##-----------------------------------------------------------
  ## MODEL (NO ICAR / GRAPH TERMS)
  ##-----------------------------------------------------------
  
 
  if(n_distinct(sp.data$protocol_id) == 1){
    
    # Model formula: global year slope 
    f1 <- count ~ -1 + nstop +
      std_yr + 
      f(kappa_k, model = "iid", constr = TRUE,
        hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))
    
  }else{
  
  # Model formula: global year slope + protocol-specific deviation from that slope
      contrasts(sp.data$ellip_e) <- contr.sum(length(levels(sp.data$ellip_e)))
    
    f1 <- count ~ -1 + nstop +
      std_yr + ellip_e + ellip_e:std_yr +
      f(kappa_k, model = "iid", constr = TRUE,
        hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))
    
  }
  
  out1 <- try(
    inla(
      f1,
      family = "nbinomial",
      data = sp.data,
      control.predictor = list(compute = TRUE),
      control.compute = list(dic = TRUE, config = TRUE),
      verbose = TRUE
    ),
    silent = TRUE
  )
  
  ##-----------------------------------------------------------
  ## Provincial Trends
  ##-----------------------------------------------------------
  
  posterior_ss <- 1000
  samp1 <- inla.posterior.sample(posterior_ss, out1, num.threads = 3)
  par_names <- as.character(attr(samp1[[1]]$latent, "dimnames")[[1]])
  post1 <- as.data.frame(sapply(samp1, function(x) x$latent))
  post1$par_names <- par_names
  
 fe <- out1$summary.fixed

beta_std_med <- fe["std_yr", "0.5quant"]
beta_std_ll  <- fe["std_yr", "0.025quant"]
beta_std_ul  <- fe["std_yr", "0.975quant"]

glob_med_tau <- (exp(beta_std_med) - 1) * 100
glob_lcl_tau <- (exp(beta_std_ll)  - 1) * 100
glob_ucl_tau <- (exp(beta_std_ul)  - 1) * 100
glob_iw_tau  <- glob_ucl_tau - glob_lcl_tau

per <- max.yr - min.yr
per_trend <- glob_med_tau / 100
percent_change <- ((1 + per_trend)^per - 1) * 100


  ## Build one provincial/global row
  tau_prov <- data.frame(
    results_code = "OWLS",
    version = "2023",
    area_code = "Ontario",          # or your province label
    season = "Breeding",
    period = "all years",
    species_code = "",
    species_id = sp.id,
    years = paste(min.yr, "-", max.yr, sep = ""),
    year_start = min.yr,
    year_end = max.yr,
    trnd = glob_med_tau,
    lower_ci = glob_lcl_tau,
    upper_ci = glob_ucl_tau,
    index_type = "",
    stderr = "",
    model_type = "Global FE slope",
    model_fit = "",
    percent_change = percent_change,
    percent_change_low = "",
    percent_change_high = "",
    prob_decrease_0 = "",
    prob_decrease_25 = "",
    prob_decrease_30 = "",
    prob_decrease_50 = "",
    prob_increase_0 = "",
    prob_increase_33 = "",
    prob_increase_100 = "",
    suitability = "",
    confidence = "",
    precision_num = "",
    precision_cat = ifelse(
      glob_iw_tau < 3.5, "High",
      ifelse(glob_iw_tau >= 3.5 & glob_iw_tau <= 6.7, "Medium", "Low")
    ),
    coverage_num = "",
    coverage_cat = "",
    sample_size = length(unique(sp.data$RouteIdentifier)),
    sample_size_units = "routes",
    prob_LD = "",
    prob_MD = "",
    prob_LC = "",
    prob_MI = "",
    prob_LI = "",
    stringsAsFactors = FALSE
  )
  
  tau_prov_out <- tau_prov %>%
    dplyr::select(
      results_code, version, area_code, season, period,
      species_code, species_id, years, year_start, year_end,
      trnd, lower_ci, upper_ci, index_type, stderr, model_type, model_fit,
      percent_change, percent_change_low, percent_change_high,
      prob_decrease_0, prob_decrease_25, prob_decrease_30,
      prob_decrease_50, prob_increase_0, prob_increase_33,
      prob_increase_100, suitability, precision_num, precision_cat,
      coverage_num, coverage_cat, sample_size, sample_size_units,
      prob_LD, prob_MD, prob_LC, prob_MI, prob_LI
    )
  
  write.table(
    tau_prov_out,
    file = paste(out.dir, "NOS_TrendsSlope", ".csv", sep = ""),
    row.names = FALSE, append = TRUE, quote = FALSE, sep = ",",
    col.names = FALSE
  )
  
 
  #Provincial Annual Indices
  tmp2 <- sp.data %>%
    dplyr::select(species_id, survey_year, StateProvince, count)
  
  for (h in 1:posterior_ss) {
    pred <- exp(samp1[[h]]$latent[1:nrow(sp.data)])
    tmp2[ncol(tmp2) + 1] <- pred
  }
  
  tmp1 <- tmp2 %>%
    group_by(survey_year, StateProvince) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  tmp1 <- tmp1 %>%
    rowwise() %>%
    mutate(
      index    = median(c_across(starts_with("V"))),
      lower_ci = quantile(c_across(starts_with("V")), 0.025),
      upper_ci = quantile(c_across(starts_with("V")), 0.975),
      stdev    = sd(c_across(starts_with("V")))
    ) %>%
    ungroup()
  
  mn.yr1 <- tmp1 %>%
    dplyr::select(survey_year, StateProvince, index, lower_ci, upper_ci, stdev) %>%
    mutate(
      results_code = "OWLS",
      version = "2023",
      season = "Breeding",
      period = "all years",
      species_code = "",
      stderr = "",
      species_name = sp,
      species_id = sp.id
    )
  
  mn.yr1$area_code <- mn.yr1$StateProvince
  mn.yr1$year      <- mn.yr1$survey_year
  
  mn.yr1 <- left_join(mn.yr1, sp.names, by = c("species_id"))
  mn.yr1$species_sci_name <- mn.yr1$scientific_name
  
  if (nrow(mn.yr1) >= 10) {
    mn.yr1 <- mn.yr1 %>%
      group_by(area_code) %>%
      mutate(LOESS_index = loess_func(index, year)) %>%
      ungroup()
  } else {
    mn.yr1$LOESS_index <- ""
  }
  
  mn.yr1$trend_index <- ""
  
  mn.yr1_out <- mn.yr1 %>%
    dplyr::select(
      results_code, version, area_code, season, period, species_code,
      species_id, year, index, stderr, stdev, upper_ci, lower_ci,
      LOESS_index, trend_index
    )
  
  write.table(
    mn.yr1_out,
    paste(out.dir, "NOS_AnnualIndices.csv", sep = ""),
    row.names = FALSE, append = TRUE, quote = FALSE, sep = ",",
    col.names = FALSE
  )
  
  
  ##-----------------------------------------------------------
  ## Protocol 
  ##-----------------------------------------------------------
  
  if(n_distinct(sp.data$protocol_id) == 2){
  
    fe <- out1$summary.fixed
    
    ## Global (average) slope
    beta_std_med <- fe["std_yr", "0.5quant"]
    beta_std_ll  <- fe["std_yr", "0.025quant"]
    beta_std_ul  <- fe["std_yr", "0.975quant"]
    
    ## Single interaction term (contrast between protocols)
    beta_int_med <- fe["std_yr:ellip_e1", "0.5quant"]
    beta_int_ll  <- fe["std_yr:ellip_e1", "0.025quant"]
    beta_int_ul  <- fe["std_yr:ellip_e1", "0.975quant"]
    
    ## Protocol labels from factor
    prot_levels <- levels(sp.data$ellip_e)  # should be c("22","36")
    
    ## For contr.sum(2), ellip_e1 = +1 for first level, -1 for second
    contrast_vals <- c(+1, -1)
    
    prot_list <- vector("list", length(prot_levels))
    
    for (i in seq_along(prot_levels)) {
      p <- prot_levels[i]
      cval <- contrast_vals[i]
      
      slope_med <- beta_std_med + cval * beta_int_med
      slope_ll  <- beta_std_ll  + cval * beta_int_ll
      slope_ul  <- beta_std_ul  + cval * beta_int_ul
      
      # transform to % per year
      med_tau <- (exp(slope_med) - 1) * 100
      lcl_tau <- (exp(slope_ll)  - 1) * 100
      ucl_tau <- (exp(slope_ul)  - 1) * 100
      
      prot_list[[i]] <- data.frame(
        protocol_level = p,
        med_tau = med_tau,
        lcl_tau = lcl_tau,
        ucl_tau = ucl_tau,
        stringsAsFactors = FALSE
      )
    }
    
    prot_trend <- dplyr::bind_rows(prot_list) %>%
      mutate(
        iw_tau = ucl_tau - lcl_tau,
        n = purrr::map_int(protocol_level, ~ sum(sp.data$ellip_e == .x)),
        taxa_code = sp,
        per = max.yr - min.yr,
        per_trend = med_tau / 100,
        percent_change = ((1 + per_trend)^per - 1) * 100
      )
    
  
  ## Format for NOS_TrendsSlope.csv
  tau_prot_out <- prot_trend %>%
    mutate(
      results_code = "OWLS",
      version = "2023",
      area_code = paste0("Protocol_", protocol_level),
      species_code = "",
      species_id = sp.id,
      season = "Breeding",
      period = "all years",
      years = paste(min.yr, "-", max.yr, sep = ""),
      year_start = min.yr,
      year_end = max.yr,
      trnd = med_tau,
      index_type = "",
      upper_ci = ucl_tau,
      lower_ci = lcl_tau,
      stderr = "",
      model_type = "Protocol FE slope",
      model_fit = "",
      percent_change_low = "",
      percent_change_high = "",
      prob_decrease_0 = "",
      prob_decrease_25 = "",
      prob_decrease_30 = "",
      prob_decrease_50 = "",
      prob_increase_0 = "",
      prob_increase_33 = "",
      prob_increase_100 = "",
      suitability = "",
      confidence = "",
      precision_num = "",
      precision_cat = ifelse(
        iw_tau < 3.5, "High",
        ifelse(iw_tau >= 3.5 & iw_tau <= 6.7, "Medium", "Low")
      ),
      coverage_num = "",
      coverage_cat = "",
      sample_size = n,
      sample_size_units = "routes",
      prob_LD = "",
      prob_MD = "",
      prob_LC = "",
      prob_MI = "",
      prob_LI = ""
    ) %>%
    dplyr::select(
      results_code, version, area_code, season, period,
      species_code, species_id, years, year_start, year_end,
      trnd, lower_ci, upper_ci, stderr, index_type, model_type, model_fit,
      percent_change, percent_change_low, percent_change_high,
      prob_decrease_0, prob_decrease_25, prob_decrease_30,
      prob_decrease_50, prob_increase_0, prob_increase_33,
      prob_increase_100, suitability, precision_num, precision_cat,
      coverage_num, coverage_cat, sample_size, sample_size_units,
      prob_LD, prob_MD, prob_LC, prob_MI, prob_LI
    )
  
  write.table(
    tau_prot_out,
    file = paste(out.dir, "NOS_TrendsSlope", ".csv", sep = ""),
    row.names = FALSE, append = TRUE, quote = FALSE, sep = ",",
    col.names = FALSE
  )
  
  
  ##Protocol specific annual indices##
  ## posterior samples (if not already done)
  par_names <- as.character(attr(samp1[[1]]$latent, "dimnames")[[1]])
  # NOTE: we only need the latent predictions; par_names is not used below
  
  ## Build prediction matrix by protocol and year
  tmp2 <- sp.data %>%
    dplyr::select(species_id, survey_year, protocol_id, ellip_e, count)
  
  for (h in 1:posterior_ss) {
    pred <- exp(samp1[[h]]$latent[1:nrow(sp.data)])
    tmp2[ncol(tmp2) + 1] <- pred
  }
  
  ## Summarise across routes within each year × protocol
  tmp1 <- tmp2 %>%
    group_by(survey_year, protocol_id) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  ## Summarise across posterior samples to index + CI
  tmp1 <- tmp1 %>%
    rowwise() %>%
    mutate(
      index    = median(c_across(starts_with("V"))),
      lower_ci = quantile(c_across(starts_with("V")), 0.025),
      upper_ci = quantile(c_across(starts_with("V")), 0.975),
      stdev    = sd(c_across(starts_with("V")))
    ) %>%
    ungroup()
  
  mn.yr_prot <- tmp1 %>%
    dplyr::select(survey_year, protocol_id, index, lower_ci, upper_ci, stdev) %>%
    mutate(
      results_code = "OWLS",
      version = "2023",
      season = "Breeding",
      period = "all years",
      species_code = "",
      stderr = "",
      species_name = sp,
      species_id = sp.id
    )
  
  mn.yr_prot$area_code <- paste0("Protocol_", mn.yr_prot$protocol_id)
  mn.yr_prot$year      <- mn.yr_prot$survey_year
  
  mn.yr_prot <- left_join(mn.yr_prot, sp.names, by = c("species_id"))
  mn.yr_prot$species_sci_name <- mn.yr_prot$scientific_name
  
  if (nrow(mn.yr_prot) >= 10) {
    mn.yr_prot <- mn.yr_prot %>%
      group_by(area_code) %>%
      mutate(LOESS_index = loess_func(index, year)) %>%
      ungroup()
  } else {
    mn.yr_prot$LOESS_index <- ""
  }
  
  mn.yr_prot$trend_index <- ""
  
  mn.yr_prot_out <- mn.yr_prot %>%
    dplyr::select(
      results_code, version, area_code, season, period, species_code,
      species_id, year, index, stderr, stdev, upper_ci, lower_ci,
      LOESS_index, trend_index
    )
  
  write.table(
    mn.yr_prot_out,
    paste(out.dir, "NOS_AnnualIndices.csv", sep = ""),
    row.names = FALSE, append = TRUE, quote = FALSE, sep = ",",
    col.names = FALSE
  )
  
  } #end protocol length
} #end speices loop