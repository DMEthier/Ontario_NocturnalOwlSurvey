#v_plots
library(dplyr)
library(ggplot2)
library(stringr)


# read in trend output

#add species English and French name
sp.name<-meta_species_taxonomy()
sp.name<-sp.name %>% select(species_id, english_name, french_name)

trnd <- read.csv("Output/2025/NOS_TrendsSlope.csv")
trnd<- left_join(trnd, sp.name, by="species_id")

#trnd<-trnd %>% filter(area_code=="Ontario")
trnd <- trnd %>% drop_na(results_code)
#trnd<-trnd %>% filter(model_type=="iCAR Slope")

trnd<-trnd %>% mutate("SurveyArea" = ifelse(area_code == "Protocol_22", "North", ifelse(area_code == "Protocol_36", "Central", "Ontario")))

trnd <- trnd %>%
  mutate(
    StudyArea = dplyr::case_when(
      english_name %in% c("Great Gray Owl", "Boreal Owl") ~ "North",
      TRUE ~ SurveyArea
    )
  )

trnd <- trnd %>% select(species_id, area_code, english_name, StudyArea, trnd, lower_ci, upper_ci) %>%
  mutate(sp.trnd = paste(english_name, "\n ", StudyArea, round(trnd, digits = 2),  
                                 " (", round(lower_ci, digits = 2), ", ",
                                 round(upper_ci, digits = 2), ")"))
trnd<-trnd %>% select(-trnd, -lower_ci, -upper_ci)

# read in annual index output 

index <- read.csv(paste("Output/2025/NOS_AnnualIndices.csv"))
#index<-index %>% filter(area_code=="ON")
index<- index %>% drop_na(results_code)
index<- left_join(index, sp.name, by="species_id")

index <- index %>%
  filter(!is.na(results_code)) %>% dplyr::select(index, lower_ci, upper_ci, LOESS_index, trend_index, 
                species_code, year, area_code, species_id,
                english_name, french_name)

plot.dat<-NULL
plot.dat <- full_join(index, trnd, by = c("area_code", "species_id", "english_name"), multiple="all")


#plot.dat <-plot.dat %>% filter(area_code %in% c("Canada")) %>% filter(species_id != 7610)

plot.dat1<-plot.dat %>% filter(english_name %in% c("American Woodcock", "Ruffed Grouse", "Wilson's Snipe") )

p1 <- ggplot(
  plot.dat1,
  aes(x = year, y = index, color = StudyArea, group = StudyArea)
) +
  geom_ribbon(
    aes(ymin = lower_ci, ymax = upper_ci, fill = StudyArea),
    alpha = 0.15,
    colour = NA,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap("english_name", scales = "free_y"
  ) +
  scale_color_brewer(palette = "Dark2", name = "Study area") +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  labs(x = "Year", y = "Annual index") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )

# print or save p
print(p1)


pdf(paste(plot.dir, "_TrendPlot_OtherSp.pdf", sep=""))
plot(p1)
while(!is.null(dev.list())) dev.off()  

plot.dat2<-plot.dat %>% filter(english_name %in% c("Barred Owl", "Boreal Owl", "Great Gray Owl", "Great Horned Owl", "Northern Saw-whet Owl"))

p2 <- ggplot(
  plot.dat2,
  aes(x = year, y = index, color = StudyArea, group = StudyArea)
) +
  geom_ribbon(
    aes(ymin = lower_ci, ymax = upper_ci, fill = StudyArea),
    alpha = 0.15,
    colour = NA,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap("english_name", scales = "free_y"
  ) +
  scale_color_brewer(palette = "Dark2", name = "Study area") +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  labs(x = "Year", y = "Annual index") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )

# print or save p
print(p2)

pdf(paste(plot.dir, "_TrendPlot_Owls.pdf", sep=""))
plot(p2)
while(!is.null(dev.list())) dev.off()  
