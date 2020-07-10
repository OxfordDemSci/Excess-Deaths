rm(list=ls())

library(ggplot2)
library(data.table)
library(patchwork)
library(scales)

source('R/4_Excess_deaths.R')


Total.deaths.EW <- results.2020[age.n != 0 ,list(excess.dx = sum(excess.dx)), by = .(year,sex,model,week,date)]

Both.sexes     <- Total.deaths.EW[, list(excess.dx = sum(excess.dx), sex = 'both'), by = .(year,model,week,date)]

Total.deaths.EW <- rbind(Total.deaths.EW,Both.sexes[,c('year','sex','model','week','date','excess.dx')])

Total.deaths.EW[, cum.excess.deaths := cumsum(excess.dx), by = .(year,sex,model)]

# total population plot
fig1.data <- Total.deaths.EW[sex!= 'both' & model == 'gam.final' & week >= w1]

lab.fig <- paste0(c('Females (','Males ('),
                  prettyNum(round(fig1.data[,sum(excess.dx), by = .(sex)]$V1),big.mark=",",scientific=FALSE),')')

Figure.1 <- ggplot(fig1.data)+
  geom_hline(yintercept = 0, color = "black", size = 1)+
  geom_path(aes(date, cum.excess.deaths, color = sex), size = 1.2,show.legend = F, lineend = "round")+
  #scale_color_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
  scale_color_manual('Sex' , values = c("#72B2B4", "#B4A097"),labels = c('Females','Males'))+
  scale_y_continuous(label=comma,breaks = seq(0,50000,10000))+
  coord_cartesian(xlim =as.Date(c("2020-03-09", "2020-06-29")))+
  theme_gray(base_size = 16)+
  theme(
    aspect.ratio = 1,
    text = element_text(face = 1)
  )+
  labs(
    x = NULL,
    y = "Excess deaths"
  )+
  annotate("text", x = as.Date("2020-06-30"), y = (round(fig1.data[,sum(excess.dx), by = .(sex)]$V1) + c(-1500,+2500)),
           label = lab.fig,
           #color = c("#72B2B4", "#B4A097",'#615652'), 
           color = c( "#72B2B4", "#B4A097"), 
           size = 6.5, hjust = 1, vjust = 1)
Figure.1
pdf(file = 'Figures/Figure_1.pdf',width = 7 ,height = 9)
Figure.1
dev.off()


################################################################################

Both.sexes.age      <- results.2020[, list(excess.dx = sum(excess.dx), sex = 'both'), by = .(year,model,week,age.n.fct,date)]

Total.deaths.EW.age <- rbind(results.2020[,c('year','sex','model','week','date','age.n.fct' ,'excess.dx')],
                             Both.sexes.age[,c('year','sex','model','week','date','age.n.fct' ,'excess.dx')])

Total.deaths.EW.age[, cum.excess.deaths := cumsum(excess.dx), by = .(year,sex,age.n.fct,model)]

fig2.data <- Total.deaths.EW.age[sex!= 'both' & model == 'gam.final' & age.n.fct != '0 to 14']

Figure.2 <- ggplot(fig2.data)+
  geom_hline(yintercept = 0, color = "black", size = 1)+
  geom_path(aes(date, cum.excess.deaths, color = sex), size = 1,show.legend = T, lineend = "round")+
  #scale_color_manual('Sex' , values = c("#72B2B4", "#B4A097",'#615652'),labels = c('Females','Males','Both'))+
  scale_color_manual('Sex' , values = c("#72B2B4", "#B4A097"),labels = c('Females','Males'))+
  theme_gray(base_size = 16)+
  scale_y_continuous(label=comma)+
  coord_cartesian(xlim =as.Date(c("2020-03-09", "2020-06-29")))+
  theme(
    aspect.ratio = 1,
    text = element_text(face = 1),
    legend.position = c(.85,.25)
  )+
  labs(
    x = NULL,
    y = "Excess deaths"
  )+
  facet_wrap(~age.n.fct, ncol = 3)

Figure.2

pdf(file = 'Figures/Figure_2.pdf',width = 9 ,height = 7)
Figure.2
dev.off()




