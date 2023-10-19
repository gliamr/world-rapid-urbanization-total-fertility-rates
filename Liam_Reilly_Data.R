library(WDI)
library(tidyverse)
library(modelr)
library(dplyr)
library(sjPlot)
library(xlsx)
library(gtsummary)

new_wdi_cache <- WDIcache()
wdi_dat <- WDI(indicator = c("NY.GDP.PCAP.KD", "SP.DYN.TFRT.IN", "SP.URB.TOTL.IN.ZS", "SE.PRM.ENRR.FE", "AG.PRD.CROP.XD"),
               start = 1960, end = 2020, extra = TRUE) 
names(wdi_dat)

wdi_dat <- subset(wdi_dat, region != "Aggregates") # this also removes NAs

names(wdi_dat)[which(names(wdi_dat) == "NY.GDP.PCAP.KD")] <- "GDP"
names(wdi_dat)[which(names(wdi_dat) == "SP.DYN.TFRT.IN")] <- "Fertility"
names(wdi_dat)[which(names(wdi_dat) == "SP.URB.TOTL.IN.ZS")] <- "Urban"
names(wdi_dat)[which(names(wdi_dat) == "SE.PRM.ENRR.FE")] <- "Enrollment"
names(wdi_dat)[which(names(wdi_dat) == "AG.PRD.CROP.XD")] <- "Crop_Index"

wdi_dat<-wdi_dat %>%
  select(year, GDP, Fertility, Urban, Enrollment, Crop_Index)

stargazer(wdi_dat, type="text", out="wdi_dat.htm")

Reg1<-lm(Fertility~Urban,data=wdi_dat)
Reg2<-lm(Fertility~Urban+GDP+Enrollment+Crop_Index,data=wdi_dat)

ggplot(data=wdi_dat, aes(x=Urban, y=Fertility)) +
  geom_smooth()

ggplot(data=wdi_dat, aes(x=year, y=Fertility)) +
  geom_smooth(color="green") +
  geom_smooth(aes(x=year, y=log(Urban)), color="brown") +

  # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total Fertility Rates (Births Per Woman)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*10, name="Urban Population % (Log)")
  )
