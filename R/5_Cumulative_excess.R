rm(list=ls())

library(ggplot2)
library(data.table)

load('Data/Deaths_UK.RData')

Deaths.UK[,model := as.character(model)]

#max week in 2020
n.week   <- max(Deaths.UK[year == 2020 & model == 'observed']$week)

# calculate cumulative excess deaths
Deaths.UK.2020 <- Deaths.UK[year == 2020 & week <= n.week]

# add observed deaths as a variable
Deaths.UK.2020[,observed := rep(.SD[model == 'observed']$deaths,4), by = list(year,sex,week,age.n)]

# subset to only models
Deaths.UK.2020 <- Deaths.UK.2020[model %in% c('gam','glm')]

# if the value of observed is within the CIs, the value of excess or saved is zero
# make difference between observed and estiamtes (positive values are lives lost)
Deaths.UK.2020[, diff.deaths := ifelse(observed >= lower.CI & observed <= upper.CI,0, observed - deaths)]

#calculate cumulative difference of deaths  
Deaths.UK.2020[, cum.diff.deaths := cumsum(diff.deaths), by = .(year,sex,model,age.n)]

Deaths.UK.2020[,age_group := factor(age.n,labels = c('0 to 14', '15 to 44', '45 to 64', '65 to 74','75 to 85','85+'))]

Total.deaths.UK <- Deaths.UK.2020[,list(diff.deaths = sum(diff.deaths)), by = .(year,sex,model,week)]

Total.deaths.UK[, cum.diff.deaths := cumsum(diff.deaths), by = .(year,sex,model)]



#X-axis, dates from 1 January to most recent; Y-axis,
#cumulative number, since 1 January, of lives lost (above zero point) and of lives 
#saved (below zero point). Three curves—total population, males, females.

# read in Blavatnik policy stringency index
# https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker

blvt <- read_csv("Data/Stringency_index_02062020.csv")

library(lubridate)
library(scales)
library(paletteer)

blvt_uk <- blvt %>% 
  filter(CountryCode == "GBR") %>% 
  transmute(date = Date %>% ymd(),
            index = `StringencyIndexForDisplay`,
            week = date %>% week()) %>% 
  group_by(week) %>% 
  summarise(index = index %>% mean()) %>% 
  ungroup()


# total population plot
  ggplot(Total.deaths.UK[model == 'gam'])+
  # geom_rect(data = blvt_uk , alpha = 1/3,
  #           aes(xmin = week - .5, 
  #               xmax = week + .5,
  #               ymin = -Inf, ymax = Inf,
  #               fill = index))+
  # scale_fill_paletteer_c("grDevices::Grays", direction = -1,
  #                        guide = guide_colorbar(title.position = "top", barwidth = 15, barheight = 1)
  # )+
    #facet_grid(.~model)+
  geom_hline(yintercept = 0, color = "black", size = 1)+
  geom_path(aes(week, cum.diff.deaths, color = sex), size = 1,show.legend = F, lineend = "round")+
  scale_color_manual(values = c("indianred4", "black"))+
  coord_cartesian(xlim = c(1, 23))+
  scale_x_continuous(breaks = 1:23, expand = c(0,0))+
    theme_minimal()+
   theme(
     #panel.ontop = TRUE,
     panel.grid.major.y = element_line(color = "#ffffff", size = .25),
     panel.grid.major.x = element_blank(),
     panel.grid.minor = element_blank(),
     legend.position = "bottom",
     aspect.ratio = 1,
     text = element_text(face = 2)
   )+
  labs(
    x = "Week of the year",
    y = "Cumulative lives lost/saved",
    fill = "Government Response Stringency Index\n(0 to 100, 100 = strictest)"
  )+
  annotate("text", x = 18, y = c(17000, 25500), label = c("Female", "Male"),
           color = c("indianred4", "black"), 
           size = 5, hjust = 0, vjust = 1)+
    annotate("segment", x = 1.2, xend = 1.2, y = c(1500,-1500), yend = c(25000, -4000),
             color = c("darkgrey",'darkgrey'), arrow = arrow(length = unit(.25, "lines")), 
             size = 1)+
    annotate("text", x = 1.2, y = c(25500, -4500), 
             label = c("Lives lost", "Lives saved"), 
             hjust = 0, size = 3.9)
  

total <- last_plot()

unique(Deaths.UK.2020$age.n)
# plots for the age groups


library(patchwork)

f1 <- ggplot(Deaths.UK.2020[model == 'gam'])+
  # geom_rect(data = blvt_uk , alpha = 1/3,
  #           aes(xmin = week - .5,
  #               xmax = week + .5,
  #               ymin = -Inf, ymax = Inf,
  #               fill = index))+
  # scale_fill_paletteer_c("grDevices::Grays", direction = -1)+
  geom_hline(yintercept = 0, color = "black", size = 1)+
  geom_path(aes(week, cum.diff.deaths, color = sex), size = 1,show.legend = F, lineend = "round")+
  scale_color_manual(values = c("indianred4", "black"))+
  coord_cartesian(xlim = c(1, 23))+
  scale_x_continuous(breaks = seq(0,23,5), expand = c(0,0))+
  theme_minimal()+
  theme(
    #panel.ontop = TRUE,
    panel.grid.major.y = element_line(color = "#ffffff", size = .25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    aspect.ratio = 1,
    text = element_text(face = 2)
  )+
  labs(x = 'Week of the year', y = NULL)+
  facet_wrap(~age_group, ncol = 2)


# align
one <- (total |  f1) 

one

pdf(file = 'Figures/Cumulative_excess.pdf',width = 10,height = 8)
one
dev.off()

# 
# 
# 
# f1 <- ggplot(Deaths.UK.2020[model == 'gam' & age.n %in% c(0,15,45)])+
#   geom_rect(data = blvt_uk , alpha = 1/3,
#             aes(xmin = week - .5, 
#                 xmax = week + .5,
#                 ymin = -Inf, ymax = Inf,
#                 fill = index))+
#   scale_fill_paletteer_c("grDevices::Grays", direction = -1)+
#   geom_hline(yintercept = 0, color = "black", size = 1)+
#   geom_path(aes(week, cum.diff.deaths, color = sex), size = 1,show.legend = F, lineend = "round")+
#   scale_color_manual(values = c("indianred4", "black"))+
#   coord_cartesian(xlim = c(1, 22))+
#   scale_x_continuous(breaks = 1:22, expand = c(0,0))+
#   theme_minimal()+
#   theme(
#     #panel.ontop = TRUE,
#     panel.grid.major.y = element_line(color = "#ffffff", size = .25),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "none",
#     aspect.ratio = 1,
#     text = element_text(face = 2)
#   )+
#   labs(x = NULL, y = NULL)+
#   facet_wrap(~age_group, nrow = 1)
# 


# 
# f2 <- ggplot(Deaths.UK.2020[model == 'gam' & age.n %in% c(65,75,85)])+
#   geom_rect(data = blvt_uk , alpha = 1/3,
#             aes(xmin = week - .5, 
#                 xmax = week + .5,
#                 ymin = -Inf, ymax = Inf,
#                 fill = index))+
#   scale_fill_paletteer_c("grDevices::Grays", direction = -1)+
#   geom_hline(yintercept = 0, color = "black", size = 1)+
#   geom_path(aes(week, cum.diff.deaths, color = sex), size = 1,show.legend = F, lineend = "round")+
#   scale_color_manual(values = c("indianred4", "black"))+
#   coord_cartesian(xlim = c(1, 22))+
#   scale_x_continuous(breaks = 1:22, expand = c(0,0))+
#   theme_minimal()+
#   theme(
#     #panel.ontop = TRUE,
#     panel.grid.major.y = element_line(color = "#ffffff", size = .25),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "none",
#     aspect.ratio = 1,
#     text = element_text(face = 2)
#   )+
#   labs(x = NULL, y = NULL)+
#   facet_wrap(~age_group, ncol = 1,strip.position = 'right')
# 

