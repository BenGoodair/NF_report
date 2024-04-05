
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo, stringr, patchwork)


fulldata <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/expenditure.csv")



plotfun <- fulldata %>%
  dplyr::mutate(SupportSetting = tolower(SupportSetting))%>%
  dplyr::filter(Sector=="Total",
                DH_GEOGRAPHY_NAME!="",
                year!=2009)%>%
  dplyr::select(SupportSetting,DH_GEOGRAPHY_NAME,year,Expenditure )%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, year) %>%
  dplyr::mutate(percent_total = Expenditure /Expenditure[SupportSetting=="total over 65"]*100) %>%
  dplyr::ungroup()%>%
  dplyr::filter(SupportSetting=="direct payments")

dpplot <- ggplot(plotfun[plotfun$SupportSetting=="direct payments",], aes(x = year, y = percent_total, color = percent_total)) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Spend on Direct Payments (%)",
    title = "",
    color = "Direct payments %"
  )+
  theme_bw()
  











