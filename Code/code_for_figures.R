
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo, stringr, patchwork)


fulldata <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/expenditure.csv")
df <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/activity_keep_all.csv")
la_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))
exits <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))
ProviderData = read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/Provider_data.csv"))
carehomes <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/Children's Care Homes Project/Data/Ben report dates.csv")

####ASC Outsourcing expenditure####


####Total over 65####
plotfun <- fulldata %>%
  dplyr::mutate(SupportSetting = tolower(SupportSetting))%>%
  dplyr::filter(Sector=="External",
                DH_GEOGRAPHY_NAME!="",
                year!=2009)%>%
  dplyr::filter(SupportSetting=="total over 65")

dpplot <- ggplot(plotfun, aes(x = year, y = percent_sector)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Spend on Outsourced Provision (%)",
    title = "Aged 65 and over, all expenditure",
    color = "Outsourced spend %"
  )+
  theme_bw()+
  scale_y_continuous(breaks=c(0,25,50,75,100))+
  #facet_wrap(~SupportSetting,nrow = 1)+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold"))

#ggsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/outsourced_over65.jpeg", width=8, height=6, dpi=600)


plotfun <-fulldata %>%
  dplyr::mutate(SupportSetting = tolower(SupportSetting))%>%
  dplyr::filter(SupportSetting=="home care"|
                  SupportSetting=="residential care home placements")%>%
  dplyr::mutate(SupportSetting= ifelse(SupportSetting=="home care", "Home Care",
                                       ifelse(SupportSetting=="residential care home placements", "Residential Care",
                                              ifelse(SupportSetting=="total over 65", "Total",
                                                     ifelse(SupportSetting=="u65 learning disability", "Learning Disability Support",
                                                            ifelse(SupportSetting=="u65 physical disability", "Physical Disability Support",
                                                                   ifelse(SupportSetting=="u65 mental health", "Mental Health Support",SupportSetting)))))))


yes <- plotfun %>% dplyr::filter(Sector=="Total")%>%
  dplyr::select(-Service, -X, -Sector,-percent_sector)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, year, SupportSetting)%>%
  dplyr::summarise(Expenditure_tot_setting = sum(Expenditure, na.rm = T))%>%
  dplyr::ungroup()

plot1 <- plotfun %>% dplyr::filter(Sector=="Total")%>%
  dplyr::select(-Service, -X, -Sector,-percent_sector, -SupportSetting)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, year)%>%
  dplyr::summarise(Expenditure_tot_reshom = sum(Expenditure, na.rm = T))%>%
  dplyr::ungroup()%>%
  dplyr::left_join(., plotfun, by = c("year", "DH_GEOGRAPHY_NAME"))%>%
  dplyr::left_join(., yes, by = c("year", "DH_GEOGRAPHY_NAME", "SupportSetting"), relationship = "many-to-many")%>%
  dplyr::filter(Sector=="In House")%>%
  dplyr::mutate(outsourced = Expenditure_tot_setting-Expenditure)%>%
  dplyr::select(-DH_GEOGRAPHY_NAME, -Service,-Sector, -X,-percent_sector)%>%
  dplyr::group_by(year, SupportSetting)%>%
  dplyr::summarise(Expenditure_tot_reshom = sum(Expenditure_tot_reshom, na.rm = T),
                   outsourced = sum(outsourced, na.rm = T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(percent_sector = outsourced/Expenditure_tot_reshom*100)%>%
  dplyr::filter(year!=2009)%>%
  ggplot(., aes(x=year, y=percent_sector, fill=SupportSetting))+
  geom_area(
    #stat = "smooth", method = "loess"
  )+
  labs(
    x = "Year",
    y = "Expenditure on Outsourced Provision (%)",
    title = "Aged 65 and over",
    color = "")+
  theme_bw()+
  scale_y_continuous(limits = c(0, 100))+
  #facet_wrap(~SupportSetting,nrow = 1)+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  scale_fill_manual(values=c("#2A6EBB","#B4CFEE" ))+
  geom_vline(xintercept=c(2014.5), colour="black", size=0.3)






plotfun <-fulldata %>%
  dplyr::mutate(SupportSetting = tolower(SupportSetting))%>%
  dplyr::filter(SupportSetting=="u65 learning disability"|
                  SupportSetting=="u65 physical disability"|
                  SupportSetting=="u65 mental health")%>%
  dplyr::mutate(SupportSetting= ifelse(SupportSetting=="home care", "Home Care",
                                       ifelse(SupportSetting=="residential care home placements", "Residential Care",
                                              ifelse(SupportSetting=="total over 65", "Total",
                                                     ifelse(SupportSetting=="u65 learning disability", "Learning Disability Support",
                                                            ifelse(SupportSetting=="u65 physical disability", "Physical Disability Support",
                                                                   ifelse(SupportSetting=="u65 mental health", "Mental Health Support",SupportSetting)))))))



yes <- plotfun %>% dplyr::filter(Sector=="Total")%>%
  dplyr::select(-Service, -X, -Sector,-percent_sector)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, year, SupportSetting)%>%
  dplyr::summarise(Expenditure_tot_setting = sum(Expenditure, na.rm = T))%>%
  dplyr::ungroup()

plot2 <- plotfun %>% dplyr::filter(Sector=="Total")%>%
  dplyr::select(-Service, -X, -Sector,-percent_sector, -SupportSetting)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, year)%>%
  dplyr::summarise(Expenditure_tot_reshom = sum(Expenditure, na.rm = T))%>%
  dplyr::ungroup()%>%
  dplyr::left_join(., plotfun, by = c("year", "DH_GEOGRAPHY_NAME"))%>%
  dplyr::left_join(., yes, by = c("year", "DH_GEOGRAPHY_NAME", "SupportSetting"), relationship = "many-to-many")%>%
  dplyr::filter(Sector=="In House")%>%
  dplyr::mutate(outsourced = Expenditure_tot_setting-Expenditure)%>%
  dplyr::select(-DH_GEOGRAPHY_NAME, -Service,-Sector, -X,-percent_sector)%>%
  dplyr::group_by(year, SupportSetting)%>%
  dplyr::summarise(Expenditure_tot_reshom = sum(Expenditure_tot_reshom, na.rm = T),
                   outsourced = sum(outsourced, na.rm = T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(percent_sector = outsourced/Expenditure_tot_reshom*100)%>%
  dplyr::filter(year!=2009)%>%
  ggplot(., aes(x=year, y=percent_sector, fill=SupportSetting))+
  geom_area(
    #stat = "smooth", method = "loess"
  )+
  labs(
    x = "Year",
    y = "Expenditure on Outsourced Provision (%)",
    title = "Aged 18-65",
    color = "")+
  theme_bw()+
  #facet_wrap(~SupportSetting,nrow = 1)+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  scale_fill_manual(values=c("#2A6EBB","#B4CFEE", "#1F5189" ))+
  geom_vline(xintercept=c(2014.5), colour="black", size=0.3)+
  scale_y_continuous(limits = c(0, 100))
  



# Combine plots with shared Y-axis label
combined_plot <- cowplot::plot_grid(plot1, plot2, ncol=2, labels=c("B", "C"), rel_heights=c(1,1))

# # Add a centered y-axis label
# combined_plot_with_label <- cowplot::ggdraw() +
#   cowplot::draw_label("Expenditure on Outsourced Provision (%)", x = 0.3, y = 0.3, angle = 90, size = 27, hjust = 0, fontface = "bold")+
#   theme(
#     plot.background = element_rect(fill = "white", color = NA)
#   )


# Arrange the plots and label vertically
final_plot <- cowplot::plot_grid(dpplot, combined_plot, labels=c("A", ""),ncol = 1)
ggsave(plot=final_plot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/adult_expenditure.jpeg", width=25, height=16, dpi=600)


#yes <- cowplot::plot_grid(plot1, plot2, ncol=1, labels = c("A", "B"))
##ggsave(plot=yes, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/fig1rev_nonsmooth.png", width=12, height=16, dpi=600)



####Direct Payments####
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

dpplot <- ggplot(plotfun[plotfun$SupportSetting=="direct payments",], aes(x = year, y = percent_total)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Spend on Direct Payments (%)",
    title = "",
    color = "Direct payments %"
  )+
  theme_bw()
  
ggsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/Direct_payments.jpeg", width=8, height=6, dpi=600)




####outsourced adult residential activity#####

plot1 <- df %>% dplyr::filter(SupportSetting=="Residential",
                              ActivityProvision== "External")%>%
  ggplot(., aes(x = year, y = percent_sector)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "#2A6EBB") +
  labs(
    x = "Year",
    y = "Residents in Outsourced Care Homes (%)",
    title = "",
    color = ""
  )+
  theme_bw()+
  facet_wrap(~SupportSetting)+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  theme(panel.spacing.x = unit(4, "mm"))


ggsave(plot=plot1, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/residential_adults.jpeg", width=10, height=7, dpi=600)




####children's outsourced residential placement####


plotfun <- la_df %>%
  dplyr::filter(variable=="Private provision"|variable=="Voluntary/third sector provision")%>%
  dplyr::select(year,percent,LA_Name)%>%
  dplyr::group_by(LA_Name, year)%>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm=T))%>%
  dplyr::ungroup()

mean(plotfun[plotfun$year==2011,]$percent, na.rm=T)

dpplot <- ggplot(plotfun, aes(x = year, y = percent)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Outsourced Placements (%)",
    title = "LA Variation",
    color = "Outsourced spend %"
  )+
  theme_bw()+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  scale_fill_manual(values=c("#2A6EBB","#B4CFEE", "#1F5189" ))+
  scale_x_continuous(breaks=c(2011,2014, 2017, 2020,2023))

#gsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_outsourced_residential_placements.jpeg", width=8, height=6, dpi=600)


####children's outsourced residential placement FP break####


plotfun <- la_df %>%
  dplyr::filter(variable=="Private provision"|variable=="Voluntary/third sector provision")%>%
  dplyr::select(year,percent,variable)%>%
  dplyr::group_by(variable, year)%>%
  dplyr::summarise(percent = mean(as.numeric(percent), na.rm=T))%>%
  dplyr::ungroup()

mean(plotfun[plotfun$year==2023&plotfun$variable=="Voluntary/third sector provision",]$percent, na.rm=T)


dpplot2 <- ggplot(plotfun, aes(x = year, y = percent, fill=variable)) +
  geom_area(
    #stat = "smooth", method = "loess"
  )+
  labs(
    x = "Year",
    y = "Outsourced Placements (%)",
    title = "Outsourcing Type",
    color = "Sector")+
  theme_bw()+
  #facet_wrap(~SupportSetting,nrow = 1)+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  scale_fill_manual(values=c("#2A6EBB","#B4CFEE", "#1F5189" ))+
  scale_x_continuous(breaks=c(2011,2014, 2017, 2020,2023))


jaa <- cowplot::plot_grid(dpplot, dpplot2, ncol=2, labels = c("A","B"))

ggsave(plot=jaa, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_outsourced_res_spend.jpeg", width=18, height=8, dpi=600)


#### child spend####

plotfun <- la_df %>%
  dplyr::filter(category=="Expenditure"&
                  (subcategory=="For_profit"|subcategory=="Third_sector"),
                variable=="Total Children Looked After")%>%
  dplyr::select(year,percent,LA_Name)%>%
  dplyr::group_by(LA_Name, year)%>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm=T))%>%
  dplyr::ungroup()

median(plotfun[plotfun$year==2011,]$percent, na.rm=T)
median(plotfun[plotfun$year==2022,]$percent, na.rm=T)

  dpplot <- ggplot(plotfun, aes(x = year, y = percent)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Outsourced Expenditure (%)",
    title = "LA Variation",
    color = "Outsourced spend %"
  )+
  theme_bw()+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  scale_fill_manual(values=c("#2A6EBB","#B4CFEE", "#1F5189" ))

#ggsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_outsourced_total_spend.jpeg", width=8, height=6, dpi=600)


plotfun <- la_df %>%
  dplyr::filter(category=="Expenditure"&
                  (subcategory=="For_profit"|subcategory=="Third_sector"),
                variable=="Total Children Looked After")%>%
  dplyr::select(year,percent,subcategory)%>%
  dplyr::group_by(subcategory, year)%>%
  dplyr::summarise(percent = mean(as.numeric(percent), na.rm=T))%>%
  dplyr::ungroup()

dpplot2 <- ggplot(plotfun, aes(x=year, y=percent, fill=subcategory))+
  geom_area(
    #stat = "smooth", method = "loess"
  )+
  labs(
    x = "Year",
    y = "Outsourced Expenditure (%)",
    title = "Outsourcing Type",
    color = "Sector")+
  theme_bw()+
  #facet_wrap(~SupportSetting,nrow = 1)+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  scale_fill_manual(values=c("#2A6EBB","#B4CFEE", "#1F5189" ))

jaa <- cowplot::plot_grid(dpplot, dpplot2, ncol=2, labels = c("A","B"), rel_widths = c(0.7,1))

ggsave(plot=jaa, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_outsourced_res_spending.jpeg", width=18, height=8, dpi=600)


####child spend break service####


plotfun <- la_df %>%
  dplyr::filter(category=="Expenditure"&
                  (subcategory=="For_profit"|subcategory=="Third_sector"),
                variable=="Residential care"|
                  variable=="Total fostering services"|
                  variable=="Adoption services"|
                  variable=="Leaving care support services"|
                  variable=="Other children looked after services")%>%
  dplyr::select(year,percent,LA_Name, variable)%>%
  dplyr::group_by(LA_Name, year, variable)%>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm=T))%>%
  dplyr::ungroup()

median(plotfun[plotfun$year==2022&plotfun$variable=="Residential care",]$percent, na.rm=T)

dpplot <- ggplot(plotfun, aes(x = year, y = percent)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Outsourced Expenditure (%)",
    title = "",
    color = "Outsourced spend %"
  )+
  theme_bw()+
  facet_wrap(~variable, ncol=1)

ggsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_outsourced_total_spend_services.jpeg", width=8, height=10, dpi=600)

####children's homes#####

ProviderData$date <- as.Date(ProviderData$Registration.date, format =  "%d/%m/%Y")
ProviderData$month <- format(ProviderData$date,"%m/%y")
ProviderData$time <- as.integer(time_length(difftime( as.Date(ProviderData$date), as.Date("2022-12-01")), "months"))
Providernobs <- unique(ProviderData[c("time", "Sector", "URN")][which(ProviderData$Provision.type=="Children's home"),])
nobsByIdih <- Providernobs %>% dplyr::group_by(time, Sector) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$Sector=="Private"),]
nobsvol <- nobsByIdih[which(nobsByIdih$Sector=="Voluntary"),]
nobsla <- nobsByIdih[which(nobsByIdih$Sector=="Local Authority"),]

all <- unique(nobsprive[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 227), ]  # Base R
all$time <- seq(from =-226, to=0)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("Sector", "time"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 227), ]  # Base R
all$time <- seq(from =-226, to=0)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("Sector", "time"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 227), ]  # Base R
all$time <- seq(from =-226, to=0)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("Sector", "time"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobs <- rbind(nobsla, nobsvol,nobsprive)

#nobs$Sector <-  revalue(as.character(nobs$Sector), c("Private"="Private", "Local Authority"="Local Authority", "Voluntary"="Third Sector"))

nobs$Sector <- factor(nobs$Sector, levels = c("Private", "Local Authority", "Voluntary"))
levels(nobs$Sector) <- c("For-profit", "Local Authority", "Voluntary")



d <- ggplot(nobs[which(nobs$time>-226),], aes(x=time, y=cumulative, group=Sector,fill=Sector,  colour = Sector))+
  geom_point()+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_color_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
# theme(legend.position="left")+
  labs(x="Year", y="Number of Children's homes", title = "Number of active children's homes", fill="Ownership", color="Ownership")+
  scale_x_continuous(breaks=c(-10,-34,-58,-82,-106,-130,-154,-178, -202, -226),
                     labels=c("2022","2020","2018",  "2016", "2014", "2012","2010", "2008", "2006", "2004"))


  frontcover <- ggplot(nobs[which(nobs$time>-226),], aes(x=time, y=cumulative, group=Sector,fill=Sector,  colour = Sector))+
  #geom_point(size=2)+
  geom_smooth(method="loess", span = 0.3, size=2)+
  theme_void()+
  scale_color_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
  theme(legend.position="none")+
  labs(x="Year", y="Number of Children's homes", title = "", fill="Ownership", color="Ownership")+
  scale_x_continuous(breaks=c(-10,-34,-58,-82,-106,-130,-154,-178, -202, -226),
                     labels=c("2022","2020","2018",  "2016", "2014", "2012","2010", "2008", "2006", "2004"))
  ggsave(plot=frontcover, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_homes_cover.jpeg", width=8, height=8, dpi=600)
  


ProviderData$date <- as.Date(ProviderData$Registration.date, format =  "%d/%m/%Y")
ProviderData$year <- format(ProviderData$date,"%Y")
Providernobs <- unique(ProviderData[c("year", "Sector", "URN")][which(ProviderData$Provision.type=="Children's home"&!is.na(ProviderData$year)),])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, Sector) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$Sector=="Private"),]
nobsvol <- nobsByIdih[which(nobsByIdih$Sector=="Voluntary"),]
nobsla <- nobsByIdih[which(nobsByIdih$Sector=="Local Authority"),]

all <- unique(nobsprive[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 50), ]  # Base R
all$year <- seq(from =1973, to=2022)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("Sector", "year"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 50), ]  # Base R
all$year <- seq(from =1973, to=2022)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("Sector", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 50), ]  # Base R
all$year <- seq(from =1973, to=2022)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("Sector", "year"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)

nobs <- rbind(nobsla, nobsvol,nobsprive)

#nobs$Sector <-  revalue(as.character(nobs$Sector), c("Private"="Private", "Local Authority"="Local Authority", "Voluntary"="Third Sector"))

nobs$Sector <- factor(nobs$Sector, levels = c("Private", "Local Authority", "Voluntary"))
levels(nobs$Sector) <- c("For-profit", "Local Authority", "Voluntary")



d2 <- ggplot(nobs[which(nobs$year>2003),], aes(x=year, y=cumulative, group=Sector,fill=Sector,  colour = Sector))+
  geom_bar(position="fill", stat="identity")+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_fill_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
  # theme(legend.position="left")+
  labs(x="Year", y="Proportion of Children's homes", title = "Proportion of active children's homes", fill="Ownership", color="Ownership")



ProviderData$date <- as.Date(ProviderData$Registration.date, format =  "%d/%m/%Y")
ProviderData$month <- format(ProviderData$date,"%m/%y")
ProviderData$time <- as.integer(time_length(difftime( as.Date(ProviderData$date), as.Date("2022-12-01")), "months"))
Providernobs <- unique(ProviderData[c("time", "Sector", "URN", "Places")][which(ProviderData$Provision.type=="Children's home"),])
nobsByIdih <- Providernobs %>% dplyr::group_by(time, Sector) %>% dplyr::summarize(nobs = sum(Places, na.rm=T))
nobsprive <- nobsByIdih[which(nobsByIdih$Sector=="Private"),]
nobsvol <- nobsByIdih[which(nobsByIdih$Sector=="Voluntary"),]
nobsla <- nobsByIdih[which(nobsByIdih$Sector=="Local Authority"),]

all <- unique(nobsprive[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 227), ]  # Base R
all$time <- seq(from =-226, to=0)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("Sector", "time"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 227), ]  # Base R
all$time <- seq(from =-226, to=0)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("Sector", "time"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 227), ]  # Base R
all$time <- seq(from =-226, to=0)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("Sector", "time"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobs <- rbind(nobsla, nobsvol,nobsprive)

#nobs$Sector <-  revalue(as.character(nobs$Sector), c("Private"="Private", "Local Authority"="Local Authority", "Voluntary"="Third Sector"))

nobs$Sector <- factor(nobs$Sector, levels = c("Private", "Local Authority", "Voluntary"))
levels(nobs$Sector) <- c("For-profit", "Local Authority", "Voluntary")



e <- ggplot(nobs[which(nobs$time>-226),], aes(x=time, y=cumulative, group=Sector,fill=Sector,  colour = Sector))+
  geom_point()+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_color_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
  # theme(legend.position="left")+
  labs(x="Year", y="Number of Beds", title = "Number of Beds in children's homes", fill="Ownership", color="Ownership")+
  scale_x_continuous(breaks=c(-10,-34,-58,-82,-106,-130,-154,-178, -202, -226),
                     labels=c("2022","2020","2018",  "2016", "2014", "2012","2010", "2008", "2006", "2004"))




ProviderData$date <- as.Date(ProviderData$Registration.date, format =  "%d/%m/%Y")
ProviderData$year <- format(ProviderData$date,"%Y")
Providernobs <- unique(ProviderData[c("year", "Sector", "URN", "Places")][which(ProviderData$Provision.type=="Children's home"&!is.na(ProviderData$year)),])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, Sector) %>% dplyr::summarize(nobs = sum(Places, na.rm=T))
nobsprive <- nobsByIdih[which(nobsByIdih$Sector=="Private"),]
nobsvol <- nobsByIdih[which(nobsByIdih$Sector=="Voluntary"),]
nobsla <- nobsByIdih[which(nobsByIdih$Sector=="Local Authority"),]

all <- unique(nobsprive[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 50), ]  # Base R
all$year <- seq(from =1973, to=2022)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("Sector", "year"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 50), ]  # Base R
all$year <- seq(from =1973, to=2022)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("Sector", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("Sector")])
all<-all[rep(seq_len(nrow(all)), each = 50), ]  # Base R
all$year <- seq(from =1973, to=2022)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("Sector", "year"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)

nobs <- rbind(nobsla, nobsvol,nobsprive)

#nobs$Sector <-  revalue(as.character(nobs$Sector), c("Private"="Private", "Local Authority"="Local Authority", "Voluntary"="Third Sector"))

nobs$Sector <- factor(nobs$Sector, levels = c("Private", "Local Authority", "Voluntary"))
levels(nobs$Sector) <- c("For-profit", "Local Authority", "Voluntary")



e2 <- ggplot(nobs[which(nobs$year>2003),], aes(x=year, y=cumulative, group=Sector,fill=Sector,  colour = Sector))+
  geom_bar(position="fill", stat="identity")+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_fill_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
  # theme(legend.position="left")+
  labs(x="Year", y="Proportion of Beds", title = "Proportion of Beds in children's homes", fill="Ownership", color="Ownership")



yep <- cowplot::plot_grid(d,d2,e,e2, ncol=2, labels = c("A", "B", "C", "D"))

ggsave(plot=yep, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_homes.jpeg", width=16, height=20, dpi=600)



















#### number of care homes #### 

carehomesstart <- carehomes %>%dplyr::select(location_start,ownership, unique_identifier )

carehomesstart$date <- as.Date(carehomesstart$location_start, format =  "%d%b%Y")
carehomesstart$month <- format(carehomesstart$date,"%m/%y")
carehomesstart$time <- as.integer(time_length(difftime( as.Date(carehomesstart$date), as.Date("2023-12-01")), "months"))
Providernobs <- unique(carehomesstart[c("time", "ownership", "unique_identifier")])
nobsByIdih <- Providernobs %>% dplyr::group_by(time, ownership) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]

all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "time"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "time"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "time"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobs <- rbind(nobsla, nobsvol,nobsprive)


carehomesend <- carehomes %>%dplyr::select(location_end,ownership, unique_identifier )%>%
  filter(location_end!="")

carehomesend$date <- as.Date(carehomesend$location_end, format =  "%d%b%Y")
carehomesend$month <- format(carehomesend$date,"%m/%y")
carehomesend$time <- as.integer(time_length(difftime( as.Date(carehomesend$date), as.Date("2023-12-01")), "months"))
Providernobs <- unique(carehomesend[c("time", "ownership", "unique_identifier")])
nobsByIdih <- Providernobs %>% dplyr::group_by(time, ownership) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]

all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "time"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "time"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "time"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobsend <- rbind(nobsla, nobsvol,nobsprive)
nobsend$cumulative_end <- nobsend$cumulative

nobser <- merge(nobs, nobsend[c("ownership", "time", "cumulative_end")], by= c("ownership", "time"), all=T)
nobser$runningsum <- nobser$cumulative-nobser$cumulative_end



d <- ggplot(nobser[which(nobser$time>-155),], aes(x=time, y=runningsum, group=ownership,fill=ownership,  colour = ownership))+
  geom_point()+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_color_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
  # theme(legend.position="left")+
  labs(x="Year", y="Number of Care homes", title = "Number of Active Care homes", fill="Ownership", color="Ownership")+
  scale_x_continuous(breaks=c(-10,-22,-34,-46,-58,-70,-82,-94,-106,-118,-130,-142,-154),
                     labels=c("2023","2022","2021","2020","2019","2018",  "2017", "2016", "2015","2014", "2013", "2012", "2011"))




carehomesstart <- carehomes %>%dplyr::select(location_start,ownership, unique_identifier )

carehomesstart$date <- as.Date(carehomesstart$location_start, format =  "%d%b%Y")
carehomesstart$year <- format(carehomesstart$date,"%Y")
Providernobs <- unique(carehomesstart[c("year", "ownership", "unique_identifier")])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, ownership) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]

all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "year"), all=T)
#nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "year"), all=T)
#nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)

nobs <- rbind(nobsla, nobsvol,nobsprive)


carehomesend <- carehomes %>%dplyr::select(location_end,ownership, unique_identifier )%>%
  filter(location_end!="")

carehomesend$date <- as.Date(carehomesend$location_end, format =  "%d%b%Y")
carehomesend$year <- format(carehomesend$date,"%Y")
Providernobs <- unique(carehomesend[c("year", "ownership", "unique_identifier")])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, ownership) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]


all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "year"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "year"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobsend <- rbind(nobsla, nobsvol,nobsprive)
nobsend$cumulative_end <- nobsend$cumulative

nobser <- merge(nobs, nobsend[c("ownership", "year", "cumulative_end")], by= c("ownership", "year"), all=T)
nobser$runningsum <- nobser$cumulative-nobser$cumulative_end



d2 <- ggplot(nobser[nobser$year>2010,], aes(x=year, y=runningsum, group=ownership,fill=ownership,  colour = ownership))+
  geom_bar(position="fill", stat="identity")+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_fill_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
  # theme(legend.position="left")+
  labs(x="Year", y="Proportion of Care homes", title = "Proportion of Active Care homes", fill="Ownership", color="Ownership")



carehomesstart <- carehomes %>%dplyr::select(location_start,ownership, unique_identifier ,carehomesbeds)

carehomesstart$date <- as.Date(carehomesstart$location_start, format =  "%d%b%Y")
carehomesstart$month <- format(carehomesstart$date,"%m/%y")
carehomesstart$time <- as.integer(time_length(difftime( as.Date(carehomesstart$date), as.Date("2023-12-01")), "months"))
Providernobs <- unique(carehomesstart[c("time", "ownership", "unique_identifier", "carehomesbeds")])
nobsByIdih <- Providernobs %>% dplyr::group_by(time, ownership) %>% dplyr::summarize(nobs = sum(carehomesbeds))
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]

all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "time"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "time"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "time"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobs <- rbind(nobsla, nobsvol,nobsprive)


carehomesend <- carehomes %>%dplyr::select(location_end,ownership, unique_identifier, carehomesbeds )%>%
  filter(location_end!="")

carehomesend$date <- as.Date(carehomesend$location_end, format =  "%d%b%Y")
carehomesend$month <- format(carehomesend$date,"%m/%y")
carehomesend$time <- as.integer(time_length(difftime( as.Date(carehomesend$date), as.Date("2023-12-01")), "months"))
Providernobs <- unique(carehomesend[c("time", "ownership", "unique_identifier", "carehomesbeds")])
nobsByIdih <- Providernobs %>% dplyr::group_by(time, ownership) %>% dplyr::summarize(nobs = sum(carehomesbeds))
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]

all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "time"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "time"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 239), ]  # Base R
all$time <- seq(from =-238, to=0)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "time"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobsend <- rbind(nobsla, nobsvol,nobsprive)
nobsend$cumulative_end <- nobsend$cumulative

nobser <- merge(nobs, nobsend[c("ownership", "time", "cumulative_end")], by= c("ownership", "time"), all=T)
nobser$runningsum <- nobser$cumulative-nobser$cumulative_end



e <- ggplot(nobser[which(nobser$time>-155),], aes(x=time, y=runningsum, group=ownership,fill=ownership,  colour = ownership))+
  geom_point()+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_color_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
  # theme(legend.position="left")+
  labs(x="Year", y="Number of Beds", title = "Number of Beds in Active Care Homes", fill="Ownership", color="Ownership")+
  scale_x_continuous(breaks=c(-10,-22,-34,-46,-58,-70,-82,-94,-106,-118,-130,-142,-154),
                     labels=c("2023","2022","2021","2020","2019","2018",  "2017", "2016", "2015","2014", "2013", "2012", "2011"))



carehomesstart <- carehomes %>%dplyr::select(location_start,ownership, unique_identifier, carehomesbeds )

carehomesstart$date <- as.Date(carehomesstart$location_start, format =  "%d%b%Y")
carehomesstart$year <- format(carehomesstart$date,"%Y")
Providernobs <- unique(carehomesstart[c("year", "ownership", "unique_identifier", "carehomesbeds")])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, ownership) %>% dplyr::summarize(nobs = sum(carehomesbeds, na.rm=T))
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]

all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "year"), all=T)
#nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "year"), all=T)
#nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)

nobs <- rbind(nobsla, nobsvol,nobsprive)


carehomesend <- carehomes %>%dplyr::select(location_end,ownership, unique_identifier )%>%
  filter(location_end!="")

carehomesend$date <- as.Date(carehomesend$location_end, format =  "%d%b%Y")
carehomesend$year <- format(carehomesend$date,"%Y")
Providernobs <- unique(carehomesend[c("year", "ownership", "unique_identifier")])
nobsByIdih <- Providernobs %>% dplyr::group_by(year, ownership) %>% dplyr::summarize(nobs = n())
nobsprive <- nobsByIdih[which(nobsByIdih$ownership=="For-profit"),]
nobsvol <- nobsByIdih[which(nobsByIdih$ownership=="Third Sector"),]
nobsla <- nobsByIdih[which(nobsByIdih$ownership=="Local Authority"),]


all <- unique(nobsprive[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsprive <- merge(nobsprive, all,by=c("ownership", "year"), all=T)
nobsprive[is.na(nobsprive$nobs),]$nobs <- 0
nobsprive$cumulative <- cumsum(nobsprive$nobs)

all <- unique(nobsla[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsla <- merge(nobsla, all,by=c("ownership", "year"), all=T)
nobsla[is.na(nobsla$nobs),]$nobs <- 0
nobsla$cumulative <- cumsum(nobsla$nobs)

all <- unique(nobsvol[c("ownership")])
all<-all[rep(seq_len(nrow(all)), each = 14), ]  # Base R
all$year <- seq(from =2010, to=2023)
all$er <- 1
nobsvol <- merge(nobsvol, all,by=c("ownership", "year"), all=T)
nobsvol[is.na(nobsvol$nobs),]$nobs <- 0
nobsvol$cumulative <- cumsum(nobsvol$nobs)



nobsend <- rbind(nobsla, nobsvol,nobsprive)
nobsend$cumulative_end <- nobsend$cumulative

nobser <- merge(nobs, nobsend[c("ownership", "year", "cumulative_end")], by= c("ownership", "year"), all=T)
nobser$runningsum <- nobser$cumulative-nobser$cumulative_end



e2 <- ggplot(nobser[nobser$year>2010,], aes(x=year, y=runningsum, group=ownership,fill=ownership,  colour = ownership))+
  geom_bar(position="fill", stat="identity")+
  #geom_smooth(method="loess", span = 0.3)+
  theme_minimal()+
  scale_fill_manual(values=c("#CD202C","#2A6EBB","#F0AB00" ))+
  # theme(legend.position="left")+
  labs(x="Year", y="Proportion of Beds", title = "Proportion of Beds", fill="Ownership", color="Ownership")





yep <- cowplot::plot_grid(d,d2,e,e2, ncol=2, labels = c("A", "B", "C", "D"))

ggsave(plot=yep, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/care_homes.jpeg", width=16, height=10, dpi=600)


####children's rolling quality####

fig2 <- ProviderData%>%
  dplyr::filter(Event.type=="Full inspection")%>%
  dplyr::select(date, Overall.experiences.and.progress.of.children.and.young.people, Sector)%>%
  dplyr::mutate(inspectiondate = as.Date(date, format =  "%Y-%m-%d"))%>%
  dplyr::mutate(good = ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Good", 1,
                              ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Outstanding",1,
                                     ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Requires improvement to be good", 0,
                                            ifelse(Overall.experiences.and.progress.of.children.and.young.people=="Inadequate", 0,NA)))))%>%
  dplyr::filter(!is.na(good),
                !Sector=="")%>%
  dplyr::arrange(inspectiondate) %>%
  dplyr::mutate(all=1)%>%
  dplyr::arrange(inspectiondate) %>%
  dplyr::group_by(Sector, inspectiondate) %>%
  dplyr::summarise(
    good = sum(good),
    all = sum(all)) %>%
  dplyr::ungroup() %>%
  complete(Sector, inspectiondate = seq.Date(min(.$inspectiondate), max(.$inspectiondate), by = "day")) %>%
  dplyr::group_by(Sector) %>%
  dplyr::arrange(inspectiondate) %>%
  dplyr::mutate(rolling_good = zoo::rollapplyr(good, width = 365, FUN = sum, fill = NA, align = "right", na.rm=T)) %>%
  dplyr::mutate(rolling_all = zoo::rollapplyr(all, width = 365, FUN = sum, fill = NA, align = "right", na.rm=T)) %>%
  dplyr::mutate(rolling_good_ratio = rolling_good / rolling_all*100)%>%
  ggplot(., aes(x=inspectiondate, y=rolling_good_ratio, colour=Sector))+
  geom_line(size=2)+
  labs(x="Year", y="Inspected Good or Outstanding (%)", color="Ownership")+
  theme_bw()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  theme(panel.spacing.x = unit(4, "mm"))+
  scale_color_manual(values=c("#2A6EBB","#CD202C","#F0AB00" ))+
  coord_cartesian(ylim=c(40, 100))

ggsave(plot=fig2, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_homes_ratings.jpeg", width=15, height=8, dpi=600)


####children's rolling monitoring####

fig2 <- ProviderData%>%
  #dplyr::filter(Event.type=="Full inspection")%>%
  dplyr::select(date, Overall.experiences.and.progress.of.children.and.young.people, Sector,Event.type)%>%
  dplyr::mutate(inspectiondate = as.Date(date, format =  "%Y-%m-%d"),
                Event.type = ifelse(Event.type=="SC Monitoring Inspection","Monitoring inspection", Event.type))%>%
  dplyr::mutate(monitoring = ifelse(Event.type=="Monitoring inspection", 1, 0 ))%>%
  dplyr::filter(!is.na(Event.type),
                !Sector=="")%>%
  dplyr::arrange(inspectiondate) %>%
  dplyr::mutate(all=1)%>%
  dplyr::arrange(inspectiondate) %>%
  dplyr::group_by(Sector, inspectiondate) %>%
  dplyr::summarise(
    monitoring = sum(monitoring),
    all = sum(all)) %>%
  dplyr::ungroup() %>%
  complete(Sector, inspectiondate = seq.Date(min(.$inspectiondate), max(.$inspectiondate), by = "day")) %>%
  dplyr::group_by(Sector) %>%
  dplyr::arrange(inspectiondate) %>%
  dplyr::mutate(rolling_monitoring = zoo::rollapplyr(monitoring, width = 365, FUN = sum, fill = NA, align = "right", na.rm=T)) %>%
  dplyr::mutate(rolling_all = zoo::rollapplyr(all, width = 365, FUN = sum, fill = NA, align = "right", na.rm=T)) %>%
  dplyr::mutate(rolling_monitoring_ratio = rolling_monitoring / rolling_all*100)%>%
  ggplot(., aes(x=inspectiondate, y=rolling_monitoring_ratio, colour=Sector))+
  geom_line(size=2)+
  labs(x="Year", y="Monitoring visits (%)", color="Ownership")+
  theme_bw()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  theme(panel.spacing.x = unit(4, "mm"))+
  scale_color_manual(values=c("#2A6EBB","#CD202C","#F0AB00" ))+
  coord_cartesian(ylim=c(0, 20))

ggsave(plot=fig2, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_homes_monitoring.jpeg", width=15, height=8, dpi=600)


####children placement quality####


placements_df = la_df[
  (la_df['subcategory'] == 'Distance between home and placement and locality of placement') |
    (la_df['subcategory'] == 'Reason for placement change during the year') |
    (la_df['subcategory'] == 'Place providers') |
    (la_df['subcategory'] == 'Locality of placement') |
    (la_df['subcategory'] == 'LA of placement') |
    (la_df['subcategory'] == 'Distance between home and placement') |
    (la_df['subcategory'] == 'Mid-year moves') |
    (la_df['subcategory'] == 'placement stability') #|
  #(la_df['subcategory'] == 'Placed inside the local authority boundary') 
]


plotfun <- la_df %>%
  dplyr::filter(subcategory=="Distance between home and placement and locality of placement"|
                  subcategory=="Place providers"|
                  subcategory=="Reason for placement change during the year"|
                  subcategory=="Locality of placement"|
                  subcategory=="LA of placement"|
                  subcategory=="Mid-year moves"|
                  subcategory=="placement stability")%>%
  dplyr::select(year,percent,LA_Name, variable)%>%
  dplyr::group_by(LA_Name, year, variable)%>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm=T))%>%
  dplyr::ungroup()

median(plotfun[plotfun$year==2022&plotfun$variable=="Residential care",]$percent, na.rm=T)

dpplot <- ggplot(plotfun, aes(x = year, y = percent)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Percent (%)",
    title = "",
    color = ""
  )+
  theme_bw()+
  facet_wrap(~variable)

ggsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_place_qual.jpeg", width=8, height=6, dpi=600)

ProviderData %>%
  dplyr::filter(year==2022)%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::group_by(Sector)%>%
  count()

####children's homes by deprivation####

leaves <- exits %>%
  dplyr::select(leave_join, imd_decile, Sector,IMD.2019...Extent, Places)%>%
  dplyr::filter(leave_join=="Leave")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(imd_decile,IMD.2019...Extent, Sector )%>%
  dplyr::summarise(homes = sum(yes),
                   places = sum(Places, na.rm=T))%>%
  dplyr::ungroup()

leaves <- leaves %>%
  dplyr::full_join(., tidyr:: expand_grid(leaves %>% 
                                            dplyr::select(IMD.2019...Extent)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("IMD.2019...Extent", "Sector"))
  

joins <- exits %>%
  dplyr::select(leave_join, imd_decile, Sector,IMD.2019...Extent, Places)%>%
  dplyr::filter(leave_join=="Join")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(imd_decile,IMD.2019...Extent, Sector )%>%
  dplyr::summarise(homes_j = sum(yes),
                   places_j = sum(Places, na.rm=T))%>%
  dplyr::ungroup()

joins <- joins %>%
  dplyr::full_join(., tidyr:: expand_grid(joins %>% 
                                            dplyr::select(IMD.2019...Extent)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("IMD.2019...Extent", "Sector"))

plot <- dplyr::full_join(leaves, joins, by=c("IMD.2019...Extent", "Sector"))%>%
  dplyr::mutate(homes_j = ifelse(is.na(homes_j), 0, homes_j),
                places_j = ifelse(is.na(places_j), 0, homes_j),
                homes = ifelse(is.na(homes), 0, homes),
                places = ifelse(is.na(places), 0, places),
                net = homes_j-homes,
                Sector = ifelse(Sector=="Private", "For-profit",
                                ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))%>%
  ggplot(. ,aes(x=as.numeric(IMD.2019...Extent)*100, y=net))+
  geom_smooth(method = "lm")+
  geom_point(color = "black", alpha = 0.5)+
  facet_wrap(~Sector)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  labs(x="Population in deprivation (%)", y="Net change to children's homes number\n(2016-2023)")+
  coord_cartesian(ylim=c(-20,40))

ggsave(plot=plot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_place_dep.jpeg", width=8, height=6, dpi=600)


####children's by houseprice####


leaves <- exits %>%
  dplyr::select(Average_house_price, Places,leave_join, Sector)%>%
  dplyr::filter(leave_join=="Leave")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Average_house_price, Sector )%>%
  dplyr::summarise(homes = sum(yes),
                   places = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()

leaves <- leaves %>%
  dplyr::full_join(., tidyr:: expand_grid(leaves %>% 
                                            dplyr::select(Average_house_price)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("Average_house_price", "Sector"))


joins <- exits %>%
  dplyr::select(Average_house_price, Places,leave_join, Sector)%>%
  dplyr::filter(leave_join=="Join")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Average_house_price, Sector )%>%
  dplyr::summarise(homes_j = sum(yes),
                   places_j = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()

joins <- joins %>%
  dplyr::full_join(., tidyr:: expand_grid(joins %>% 
                                            dplyr::select(Average_house_price)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("Average_house_price", "Sector"))

plot2 <- dplyr::full_join(leaves, joins, by=c("Average_house_price", "Sector"))%>%
  dplyr::mutate(homes_j = ifelse(is.na(homes_j), 0, homes_j),
                 places_j = ifelse(is.na(places_j), 0, homes_j),
                 homes = ifelse(is.na(homes), 0, homes),
                 places = ifelse(is.na(places), 0, places),
                 net = homes_j-homes,
                Sector = ifelse(Sector=="Private", "For-profit",
                                ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))%>%
  dplyr::filter(Average_house_price<1000000)%>%
  ggplot(. ,aes(x=as.numeric(Average_house_price), y=net))+
  geom_smooth(method = "lm")+
  geom_point(color = "black", alpha = 0.5)+
  facet_wrap(~Sector)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  labs(x="Average House Price (£))", y="Net change to children's homes number\n(2016-2023)")+
  coord_cartesian(ylim=c(-20, 40))

ggsave(plot=plot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_homes_houseprice.jpeg", width=8, height=6, dpi=600)

####children's homes by out-of-area placements####

leaves <- exits %>%
  dplyr::select(Local.authority, Places,leave_join, Sector)%>%
  dplyr::filter(leave_join=="Leave")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority, Sector )%>%
  dplyr::summarise(homes = sum(yes),
                   places = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()

leaves <- leaves %>%
  dplyr::full_join(., tidyr:: expand_grid(leaves %>% 
                                            dplyr::select(Local.authority)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("Local.authority", "Sector"))


joins <- exits %>%
  dplyr::select(Local.authority, Places,leave_join, Sector)%>%
  dplyr::filter(leave_join=="Join")%>%
  dplyr::mutate(yes = 1)%>%
  dplyr::group_by(Local.authority, Sector )%>%
  dplyr::summarise(homes_j = sum(yes),
                   places_j = sum(as.numeric(Places), na.rm=T))%>%
  dplyr::ungroup()

joins <- joins %>%
  dplyr::full_join(., tidyr:: expand_grid(joins %>% 
                                            dplyr::select(Local.authority)%>%
                                            dplyr::distinct(), 
                                          c("Local Authority", "Private", "Voluntary"))%>%
                     dplyr::rename(Sector = `c("Local Authority", "Private", "Voluntary")`),
                   by=c("Local.authority", "Sector"))

plot <- dplyr::full_join(leaves, joins, by=c("Local.authority", "Sector"))%>%
  dplyr::mutate(homes_j = ifelse(is.na(homes_j), 0, homes_j),
                places_j = ifelse(is.na(places_j), 0, homes_j),
                homes = ifelse(is.na(homes), 0, homes),
                places = ifelse(is.na(places), 0, places),
                net = homes_j-homes,
                Sector = ifelse(Sector=="Private", "For-profit",
                                ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))%>%
  dplyr::full_join(., la_df %>% 
                     dplyr::filter(variable=="2. Other LA children externally placed within the local authority boundary",
                                   year>2017)%>%
                     dplyr::select(LA_Name,percent)%>%
                     dplyr::group_by(LA_Name)%>%
                     summarise(percent= mean(as.numeric(percent), na.rm=T))%>%
                     dplyr::ungroup()%>%
                     dplyr::rename(Local.authority = LA_Name), by="Local.authority")%>%
  dplyr::filter(!is.na(Sector))%>%
  ggplot(. ,aes(x=as.numeric(percent), y=net))+
  geom_smooth(method = "lm")+
  geom_point(color = "black", alpha = 0.5)+
  facet_wrap(~Sector)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  labs(x="Average Children from other LAs placed inside (%))", y="Net change to children's homes number\n(2016-2023)")+
  coord_cartesian(ylim = c(-30,30))

ggsave(plot=plot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_homes_away.jpeg", width=8, height=6, dpi=600)

