
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo, stringr, patchwork)

####create Data####

fulldata <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Final_data/expenditure.csv"))
df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/main/Final_data/activity_keep_all.csv"))
la_df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))
#exits <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))
ProviderData = read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/Provider_data.csv"))
carehomes <- read.csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/Ben report dates.csv")

#updated exits, entries


joiners17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_17.csv"))  
joiners18 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_18.csv"))  
joiners19 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_19.csv"))  
joiners20 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_20.csv"))  
joiners21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_21.csv"))  
joiners22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_22.csv"), skip=3)  
joiners23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_23.csv"), skip=3)  

joiners <- rbind( #joiners17 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  #joiners18 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  joiners19 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners20 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners21 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners22 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
  joiners23 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.)
)%>%
  dplyr::rename(Date = Registration.date)%>%
  dplyr::mutate(leave_join = "Join",
                provider_status = NA)%>%
  dplyr::filter(Provision.type!="Adoption Support Agency",
                Provision.type!="Further Education College with Residential Accommodation",
                Provision.type!="Boarding School",
                Provision.type!="Residential Family Centre",
                Provision.type!="Residential Special School",
                Provision.type!="Voluntary Adoption Agency",
                Provision.type!="Residential Holiday Scheme for Disabled Children",
                Provision.type!="Independent Fostering Agency",
                Provision.type!="Voluntary Adoption Agency")


Leavers <- rbind( #joiners17 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  #joiners18 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
  joiners19 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Date.closed)%>%
    dplyr::rename(Cancelled.or.resigned.date = Date.closed),
  joiners20 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Cancelled.or.resigned.date),
  joiners21 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Resigned.closed.date)%>%
    dplyr::rename(Cancelled.or.resigned.date=Resigned.closed.date),
  joiners22 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Cancelled.or.resigned.date),
  joiners23 %>% dplyr::filter(Leaver.status=="Leaver") %>% 
    dplyr::mutate(Cancelled.or.resigned.date = ifelse(First.effective.date..that.the.provider.became.resigned.!="",First.effective.date..that.the.provider.became.resigned.,
                                                      ifelse(First.effective.date..that.the.provider.became.resigned.==""&First.effective.date..that.the.provider.closed.=="", First.effective.date..that.the.provider.became.cancelled.,
                                                             First.effective.date..that.the.provider.closed.)))%>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,Cancelled.or.resigned.date) 
)%>%
  dplyr::rename(Date = Cancelled.or.resigned.date)%>%
  dplyr::mutate(leave_join = "Leave",
                provider_status = NA)%>%
  dplyr::filter(Provision.type!="Adoption Support Agency",
                Provision.type!="Further Education College with Residential Accommodation",
                Provision.type!="Boarding School",
                Provision.type!="Residential Family Centre",
                Provision.type!="Residential Special School",
                Provision.type!="Voluntary Adoption Agency",
                Provision.type!="Residential Holiday Scheme for Disabled Children",
                Provision.type!="Independent Fostering Agency",
                Provision.type!="Voluntary Adoption Agency")


exits <- rbind(joiners, Leavers)

pre <- rbind(
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 3, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 4, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 5, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 6, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 7, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 8, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 9, skip=3) %>%
    dplyr::mutate(leave_join = "Join")%>%
    dplyr::rename(Date = `Registration date`),
  read_excel("Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 10, skip=3) %>%
    dplyr::mutate(leave_join = "Leave")%>%
    dplyr::rename(Date = `Date closed`)
)

all <- rbind(pre%>%
               dplyr::rename(Provision.type = `Provider type`,
                             Local.authority=`Local authority`)%>%
               dplyr::mutate(Local.authority = Local.authority %>%
                               gsub('&', 'and', .) %>%
                               gsub('[[:punct:] ]+', ' ', .) %>%
                               gsub('[0-9]', '', .)%>%
                               toupper() %>%
                               gsub("CITY OF", "",.)%>%
                               gsub("UA", "",.)%>%
                               gsub("COUNTY OF", "",.)%>%
                               gsub("ROYAL BOROUGH OF", "",.)%>%
                               gsub("LEICESTER CITY", "LEICESTER",.)%>%
                               gsub("UA", "",.)%>%
                               gsub("DARWIN", "DARWEN", .)%>%
                               gsub("COUNTY DURHAM", "DURHAM", .)%>%
                               gsub("AND DARWEN", "WITH DARWEN", .)%>%
                               gsub("NE SOM", "NORTH EAST SOM", .)%>%
                               gsub("N E SOM", "NORTH EAST SOM", .)%>%
                               str_trim())%>% dplyr::select(URN,Local.authority,Sector,Places,Date,leave_join), 
             exits%>% dplyr::select(URN,Local.authority,Sector,Places,Date, leave_join)%>%
               dplyr::mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%  # Convert Date to Date format
               dplyr::filter(Date >= as.Date("2018-04-01"))  # Filter for dates after 1st April 2018
)%>%  
  dplyr::distinct(URN,leave_join, .keep_all = T)%>%
  tidyr::pivot_wider(id_cols = c("Local.authority", "Sector", "URN", "Places"), names_from = "leave_join", values_from = as.character("Date"))%>%
  dplyr::mutate(Join = substr(Join, 1, 10),
                Leave = substr(Leave, 1, 10),
                Sector= ifelse(Sector=="Health Authority", "Local Authority", Sector))


leaves <- all %>%
  dplyr::select(URN,Leave)

all <- all %>%
  dplyr::select(-Leave)%>%
  full_join(., leaves, by= "URN")  %>%
  group_by(URN) %>%                      # Group by URN
  fill(Leave, .direction = "downup") %>% 
  fill(Join, .direction = "downup")%>%
  ungroup()%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())







fulldf <-rbind( read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2015.csv"))%>%
              dplyr::mutate(year=2015,
                            Places= NA)%>%
              dplyr::rename(Registration.status=Reg.Status)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home")|
                              str_detect(Provision.type, "(?i)day"))%>%
              dplyr::select(Sector,URN, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2016.csv"), skip=1)%>%
              dplyr::mutate(year=2016)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2017.csv"), skip=1)%>%
              dplyr::mutate(year=2017)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018.csv"), skip=1)%>%
              dplyr::mutate(year=2018)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2018_part2.csv"))%>%
              dplyr::mutate(year=2018)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2019.csv"), skip=1)%>%
              dplyr::mutate(year=2019)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2020.csv"), skip=1)%>%
              dplyr::mutate(year=2020)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2021.csv"), skip=1)%>%
              dplyr::mutate(year=2021)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2022_yep.csv"), skip=4)%>%
              dplyr::mutate(year=2022)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority),
            
            
            read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Care-Markets/main/Data/Raw/provider_at_march_2023.csv"), skip=3)%>%
              dplyr::mutate(year=2023)%>%
              dplyr::filter(str_detect(Provision.type, "(?i)home"))%>%
              dplyr::select(Sector, URN, Places, Local.authority))%>%
  dplyr::mutate(Sector= ifelse(Sector=="Health Authority"|Sector=="Local authority"|Sector=="Health authority", "Local Authority", Sector),
                Homes=1)%>%
  dplyr::distinct(.keep_all = T)%>%
  group_by(URN) %>%                      # Group by URN
  fill(Places, .direction = "downup") %>% 
  ungroup()%>%
  bind_rows(., all %>% 
              dplyr::select(URN, Places, Local.authority)%>%
              dplyr::mutate(Homes = NA, 
                            Sector=NA,
                            Places = as.numeric(Places)))%>%
  group_by(URN) %>%                      # Group by URN
  fill(Places, .direction = "downup") %>% 
  ungroup()%>%
  dplyr::filter(!is.na(Sector))%>%
  dplyr::mutate(Local.authority = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., all%>%
                     dplyr::select(-Places, -Local.authority, -Sector)%>%
                     dplyr::filter(as.Date(Leave)>="2015-04-01"|
                                     is.na(Leave))%>%
                     dplyr::filter(as.Date(Join)<"2023-04-01"|
                                     is.na(Join)), by=c("URN"))


yes <- fulldf %>% dplyr::filter(is.na(Local.authority))%>%
  dplyr::mutate(keep = 1)%>%
  dplyr::select(URN, keep)%>%
  full_join(., all, by="URN")%>%
  dplyr::filter(keep==1)


exits <- fulldf%>% dplyr::filter(!is.na(Local.authority))%>%
  bind_rows(., yes%>%
              dplyr::mutate(Places = as.numeric(Places)))




rm(list=setdiff(ls(), c("exits", "fulldata", "df", "la_df", "ProviderData", "carehomes")))

  


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
    y = "Expenditure on Outsourced Provision (%)",
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



####children's out of area residential placement####


plotfun <- la_df %>%
  dplyr::filter(variable=="Placed outside the local authority boundary")%>%
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
    y = "Out of area Placements (%)",
    #title = "LA Variation",
    #color = "Outsourced spend %"
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

#ggsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_out_of_area_placements.jpeg", width=8, height=6, dpi=600)


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

mean(plotfun[plotfun$year==2011,]$percent, na.rm=T)
mean(plotfun[plotfun$year==2023,]$percent, na.rm=T)

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
  scale_fill_manual(values=c("#2A6EBB","#B4CFEE", "#1F5189" ))+
    scale_x_continuous(breaks=c(2009,2011, 2013,2015, 2017, 2019, 2021,2023))

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
  scale_fill_manual(values=c("#2A6EBB","#B4CFEE", "#1F5189" ))+
  scale_x_continuous(breaks=c(2009,2011, 2013,2015, 2017, 2019, 2021,2023))


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

mean(plotfun[plotfun$year==2023&plotfun$variable=="Residential care",]$percent, na.rm=T)

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
  facet_wrap(~variable)+
  scale_x_continuous(breaks=c(2009,2011, 2013,2015, 2017, 2019, 2021,2023))


ggsave(plot=dpplot, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_outsourced_total_spend_services.jpeg", width=8, height=8, dpi=600)

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

plotfun <- plotfun[plotfun$variable=="Placed inside the local authority boundary",]

median(plotfun[plotfun$year==2011&plotfun$variable=="Placed inside the local authority boundary",]$percent, na.rm=T)

dpplotone <- ggplot(plotfun, aes(x = year, y = percent)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Percent of children placed within their LA boundary (%)",
    title = "Placement Location",
    color = ""
  )+
  theme_bw()+
  scale_x_continuous(breaks=c(2011,2013, 2015, 2017,2019, 2021, 2023))


plotfun <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Data/placement_data_final.csv"))


dpplottwo <- ggplot(plotfun, aes(x = year, y = per_less_than_2yrs)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Year",
    y = "Percent of children in same placement for over 2 years (%)",
    title = "Placement Stability",
    color = ""
  )+
  theme_bw()+
  scale_x_continuous(breaks=c(2011,2013, 2015, 2017,2019, 2021))


yepl <- cowplot::plot_grid(dpplotone, dpplottwo, labels = c("A","B"))


ggsave(plot=yepl, filename="C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/NF_report/Figures/children_place_qual.jpeg", width=8, height=6, dpi=600)

# ProviderData %>%
#   dplyr::filter(year==2022)%>%
#   dplyr::distinct(URN, .keep_all = T)%>%
#   dplyr::group_by(Sector)%>%
#   count()

####children's homes by deprivation####


plot <- exits %>%
  dplyr::arrange(Places)%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::mutate(Homes= ifelse(!is.na(Join)&is.na(Leave), 1,
                              ifelse(!is.na(Leave)&is.na(Join), -1,
                                     0)),
                Places= ifelse(!is.na(Join)&is.na(Leave), Places,
                               ifelse(!is.na(Leave)&is.na(Join), Places*-1,
                                      0)))%>%
  dplyr::select(Local.authority, Homes, Places,Sector)%>%
  dplyr::group_by(Local.authority, Sector)%>%
  dplyr::summarise(Homes = sum(Homes),
                   Places = sum(Places))%>%
  dplyr::ungroup()%>%
  tidyr::complete(Local.authority, Sector = c("Private", "Local Authority", "Voluntary"))%>%
  dplyr::mutate(Homes=ifelse(is.na(Homes), 0, Homes),
                Places=ifelse(is.na(Places), 0, Places))%>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))%>%
                     dplyr::select(Local.authority, IMD.2019...Extent, Average_house_price)%>%
                     dplyr::distinct(.keep_all = T), by="Local.authority")%>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
                     dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")%>%
                     dplyr::filter(variable=="Net gain of children by responsible LA"&year==2023)%>%
                     dplyr::rename(Local.authority = LA_Name),
                   by="Local.authority")%>%
  dplyr::mutate(Sector = ifelse(Sector=="Private", "For-profit",
                  ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))%>%
  dplyr::filter(!str_detect(Local.authority, "(?i)northampton"))%>%
  ggplot(., aes(y=Homes, x=as.numeric(number)))+
  geom_smooth(method = "lm")+
  geom_point(color = "black", alpha = 0.5)+
  facet_wrap(~Sector)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  labs(x="Additional children in care placed inside LA\n(Net gain, 2023)", y="Net change to children's homes\n(2014-2023)")
  
   

ggsave(plot=plot, filename="Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/NF_report/Figures/children_place_net_gain.jpeg", width=8, height=6, dpi=600)


####children's by houseprice####


plot <- exits %>%
  dplyr::arrange(Places)%>%
  dplyr::distinct(URN, .keep_all = T)%>%
  dplyr::mutate(Homes= ifelse(!is.na(Join)&is.na(Leave), 1,
                              ifelse(!is.na(Leave)&is.na(Join), -1,
                                     0)),
                Places= ifelse(!is.na(Join)&is.na(Leave), Places,
                               ifelse(!is.na(Leave)&is.na(Join), Places*-1,
                                      0)))%>%
  dplyr::select(Local.authority, Homes, Places,Sector)%>%
  dplyr::group_by(Local.authority, Sector)%>%
  dplyr::summarise(Homes = sum(Homes),
                   Places = sum(Places))%>%
  dplyr::ungroup()%>%
  tidyr::complete(Local.authority, Sector = c("Private", "Local Authority", "Voluntary"))%>%
  dplyr::mutate(Homes=ifelse(is.na(Homes), 0, Homes),
                Places=ifelse(is.na(Places), 0, Places))%>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/enter_exit.csv"))%>%
                     dplyr::select(Local.authority, IMD.2019...Extent, Average_house_price)%>%
                     dplyr::distinct(.keep_all = T), by="Local.authority")%>%
  dplyr::left_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>%
                     dplyr::filter(!LA_Name=="Dorset"&!LA_Code=="E10000009")%>%
                     dplyr::filter(variable=="Net gain of children by responsible LA"&year==2023)%>%
                     dplyr::rename(Local.authority = LA_Name),
                   by="Local.authority")%>%
  dplyr::mutate(Sector = ifelse(Sector=="Private", "For-profit",
                                ifelse(Sector=="Voluntary", "Third Sector", "Local Authority")))%>%
  dplyr::filter(!str_detect(Local.authority, "(?i)northampton"))%>%
  ggplot(., aes(y=Places, x=as.numeric(Average_house_price)))+
  geom_smooth(method = "lm")+
  geom_point(color = "black", alpha = 0.5)+
  facet_wrap(~Sector)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  labs(x="Average House Price (£, 2023)", y="Net change to children's homes\n(2014-2023)")






ggsave(plot=plot, filename="Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/NF_report/Figures/children_place_houseprice.jpeg", width=8, height=6, dpi=600)


