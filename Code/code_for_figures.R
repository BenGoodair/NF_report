
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo, stringr, patchwork)


fulldata <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/expenditure.csv")
df <- read.csv("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/adults_social_care_data/activity_keep_all.csv")
la_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))


####ASC Outsourcing Breakdown by services####


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


plot1 <- plotfun %>% dplyr::filter(Sector=="Total")%>%
  dplyr::select(-Service, -X, -Sector,-percent_sector, -SupportSetting)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, year)%>%
  dplyr::summarise(Expenditure_tot_reshom = sum(Expenditure, na.rm = T))%>%
  dplyr::ungroup()%>%
  dplyr::left_join(., plotfun, by = c("year", "DH_GEOGRAPHY_NAME"))%>%
  dplyr::filter(Sector=="External")%>%
  dplyr::select(-DH_GEOGRAPHY_NAME, -Service,-Sector, -X,-percent_sector)%>%
  dplyr::group_by(year, SupportSetting)%>%
  dplyr::summarise(Expenditure_tot_reshom = sum(Expenditure_tot_reshom, na.rm = T),
                   Expenditure = sum(Expenditure, na.rm = T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(percent_sector = Expenditure/Expenditure_tot_reshom*100)%>%
  dplyr::filter(year!=2009)%>%
  ggplot(., aes(x=year, y=percent_sector, fill=SupportSetting))+
  geom_area(
    #stat = "smooth", method = "loess"
  )+
  labs(
    x = "Year",
    y = "",
    title = "Aged 65 and over",
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




plot2 <- plotfun %>% dplyr::filter(Sector=="Total")%>%
  dplyr::select(-Service, -X, -Sector,-percent_sector, -SupportSetting)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, year)%>%
  dplyr::summarise(Expenditure_tot_reshom = sum(Expenditure, na.rm = T))%>%
  dplyr::ungroup()%>%
  dplyr::left_join(., plotfun, by = c("year", "DH_GEOGRAPHY_NAME"))%>%
  dplyr::filter(Sector=="External")%>%
  dplyr::select(-DH_GEOGRAPHY_NAME, -Service,-Sector, -X,-percent_sector)%>%
  dplyr::group_by(year, SupportSetting)%>%
  dplyr::summarise(Expenditure_tot_reshom = sum(Expenditure_tot_reshom, na.rm = T),
                   Expenditure = sum(Expenditure, na.rm = T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(percent_sector = Expenditure/Expenditure_tot_reshom*100)%>%
  dplyr::filter(year!=2009)%>%
  ggplot(., aes(x=year, y=percent_sector, fill=SupportSetting))+
  geom_area(
    #stat = "smooth", method = "loess"
  )+
  labs(
    x = "Year",
    y = "",
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
  geom_vline(xintercept=c(2014.5), colour="black", size=0.3)



# Combine plots with shared Y-axis label
combined_plot <- cowplot::plot_grid(plot1, plot2, ncol=1, labels=c("A", "B"), rel_heights=c(1,1))

# Add a centered y-axis label
combined_plot_with_label <- cowplot::ggdraw() +
  cowplot::draw_label("Expenditure on Outsourced Provision (%)", x = 0.3, y = 0.3, angle = 90, size = 27, hjust = 0, fontface = "bold")+
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )


# Arrange the plots and label vertically
final_plot <- cowplot::plot_grid(combined_plot_with_label, combined_plot, ncol = 2, rel_widths = c(0.05, 1))
ggsave(plot=final_plot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/service_breakdown.jpeg", width=12, height=16, dpi=600)


yes <- cowplot::plot_grid(plot1, plot2, ncol=1, labels = c("A", "B"))
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
    title = "",
    color = "Outsourced spend %"
  )+
  theme_bw()

ggsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/outsourced_over65.jpeg", width=8, height=6, dpi=600)


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




####children's total outsourced residential placement####


plotfun <- la_df %>%
  dplyr::filter(variable=="Private provision"|variable=="Voluntary/third sector provision")%>%
  dplyr::select(year,percent,LA_Name)%>%
  dplyr::group_by(LA_Name, year)%>%
  dplyr::summarise(percent = sum(as.numeric(percent), na.rm=T))%>%
  dplyr::ungroup()

dpplot <- ggplot(plotfun, aes(x = year, y = percent)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Outsourced Placements (%)",
    title = "",
    color = "Outsourced spend %"
  )+
  theme_bw()

ggsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_outsourced_residential_placements.jpeg", width=8, height=6, dpi=600)




