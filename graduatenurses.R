library(Hmisc)
library(ggplot2)


dem00$share.of.aged.65.and...in.2011 <- as.numeric(dem00$share.of.aged.65.and...in.2011)

dem00$shareold <- matrix("NA", nrow =length(dem00$share.of.aged.65.and...in.2011), ncol =1 )

for (i in 1:length(dem00$share.of.aged.65.and...in.2011)){
  if (dem00$share.of.aged.65.and...in.2011[i] <= 10){
    dem00$shareold[i] <- "10"
  } 
  else{ if(dem00$share.of.aged.65.and...in.2011[i] > 10 & dem00$share.of.aged.65.and...in.2011[i] <= 15){
    dem00$shareold[i] <- "20"
  } 
  else{ if(dem00$share.of.aged.65.and...in.2011[i] > 15 & dem00$share.of.aged.65.and...in.2011[i] <= 20){
    dem00$shareold[i] <- "30"
  } 
  else {
    dem00$shareold[i] <- "40"
  }
  }
  }
}

dem00$shareold <- as.numeric(dem00$shareold)

ggplot(dem00, aes(x=Total.health.expenditure.per.capita.in.USD.PPP.in.2011, y=Nursing.graduates..Total..Per.100.000.inhabitants..in.2011, label=dem00$Location)) +
  geom_point(aes(size = shareold), colour="blue") +
  labs(x="Total health expenditures", y="Nursing graduates", size="Share of people over 65") +
  geom_text(size=2, aes(label=Location),hjust=-0.1, vjust=-0.60) +
  ggtitle("Nursing graduates per total health expenditures") +
  geom_smooth(method=lm,
              se=FALSE,
              fullrange=T) +
  theme(legend.position = "bottom", legend.background = element_rect(colour = "black")) +
  theme(plot.title=element_text(size=18, face="bold.italic"))


ggsave(file="health-graph.png")

