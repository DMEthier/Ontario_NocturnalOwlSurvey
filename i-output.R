#Write output tables suitable for SoCB

#Posterior Summary
post_sum<- as.data.frame(matrix(data = NA, nrow = 1, ncol = 12, byrow = FALSE, dimnames = NULL))
names(post_sum) <- c("alpha_i", "alph", "alph_ll", "alph_ul", "alph_iw", "tau", "tau_ll", "tau_ul", "tau_iw", "tau_sig", "id", "taxa_code")
write.table(post_sum, file = paste(out.dir, "PosteriorSummary.csv", sep=""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

#Output for SoBC import
indices.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 15, byrow = FALSE,
                                    dimnames = NULL))
names(indices.csv) <- c("results_code", "version", "area_code", "season", "period", "species_code", "species_id", "year", "index", "stderr", "stdev", "upper_ci", "lower_ci", "LOESS_index", "trend_index")


write.table(indices.csv, file = paste(out.dir, 
                                      "NOS_AnnualIndices",".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


## Create text file for trends 
trends.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 38, 
                                   byrow = FALSE, dimnames = NULL))
names(trends.csv) <- c("results_code",	"version",	"area_code",	"season",	"period", "species_code",	"species_id",	"years", "year_start",	"year_end",	"trnd",	"lower_ci", "upper_ci", "stderr",	"model_type",	"model_fit",	"percent_change",	"percent_change_low",	"percent_change_high",	"prob_decrease_0",	"prob_decrease_25",	"prob_decrease_30",	"prob_decrease_50",	"prob_increase_0",	"prob_increase_33",	"prob_increase_100", "suitability", "precision_num",	"precision_cat",	"coverage_num",	"coverage_cat",	"sample_size", "sample_size_units", "prob_LD", "prob_MD", "prob_LC", "prob_MI", "prob_LI")

#Slope Trends
write.table(trends.csv, file = paste(out.dir,  
                                     "NOS_TrendsSlope", ".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

