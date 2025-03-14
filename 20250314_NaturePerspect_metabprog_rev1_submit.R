########Figure for Nature Perspective, Adversity, adiposity, nutrition, and metabolic wellbeing in multiethnic Asia 

# Note to user:
# Please refer to the citations for the information regarding the publicly available data.
# Although some data are publicly available, we are not at liberty to reproduce parts of these information.
# Please email theresia.hm@ntu.edu.sg should you need help in locating this publicly available data.
# Please contact the UK Biobank directly, or reach out to the HELIOSSG100K Study should you need to access these data.

library(tidyverse)
library(wesanderson)
library(gridExtra)
library(betareg)
library(metafor)


### Figure 1a to illustrate proportion of death by CVD across global regions 1990-2019
# Use Prevalence from latest GBD data 2021
setwd("")
gbd_2021_death <- readxl::read_excel("IHME_GBD_2021_MORTALITY_1990_2021_SR_TABLE_1_Y2024M04D03.xlsx")

colnames(gbd_2021_death) <- unlist(gbd_2021_death[1, ])

gbd_2021_death_mini <- gbd_2021_death %>% slice(.,-1) %>% select (-contains("ASMR")) %>%
  dplyr::rename(death_1990=4,death_2010=5,death_2019=6,death_2020=7,death_2021=8) %>%
  mutate(across(starts_with("death_"), ~ gsub("[\\(\\)\\-]", "|", .))) %>%
  mutate(across(starts_with("death_"), ~ gsub(" ", "", .))) %>%
  filter (location_type=="Global" | location_type=="Region") %>%
  filter (.,grepl('Global|Europe|Asia', location_name)) %>%
  separate(death_1990, c("death_1990_num","death_1990_lci","death_1990_uci"), sep = '\\|') %>%
  separate(death_2010, c("death_2010_num","death_2010_lci","death_2010_uci"), sep = '\\|') %>%
  separate(death_2019, c("death_2019_num","death_2019_lci","death_2019_uci"), sep = '\\|') %>%
  separate(death_2020, c("death_2020_num","death_2020_lci","death_2020_uci"), sep = '\\|') %>%
  separate(death_2021, c("death_2021_num","death_2021_lci","death_2021_uci"), sep = '\\|') %>%
  mutate(across(matches("death"), as.numeric))

gbd_2021_death_mini_allcause <- gbd_2021_death_mini %>% 
  dplyr::filter (.,!grepl('COVID-19', cause_name)) %>%
  dplyr::select (location_name,contains("num")) %>%
  dplyr::group_by (location_name) %>%
  dplyr::summarize(across(everything(), sum)) 

gbd_2021_death_mini_cvd <- gbd_2021_death_mini %>% 
  dplyr::filter (.,grepl('heart|mitr|hemor|rheu|stroke|isch|valv|card|fibril|hypertens|aorti|aneur|arte|pulmo', cause_name)) %>%
  dplyr::filter (.,!grepl('kidney|mater|congen|lung|anomal', cause_name)) %>%
  dplyr::filter (.,!grepl('Maternal hemorrhage|Maternal hypertensive disorders', cause_name)) %>%
  dplyr::select (location_name,contains("num")) %>%
  dplyr::group_by (location_name) %>%
  dplyr::summarize(across(everything(), sum))

death_allcause_EU <- gbd_2021_death_mini_allcause %>% dplyr::filter (.,grepl('Europe',location_name)) %>%
  dplyr::select(-location_name) %>%
  dplyr::summarize(across(everything(), sum)) %>% dplyr::mutate(location_name="Europe")
death_allcause_Asia<- gbd_2021_death_mini_allcause %>% dplyr::filter (.,grepl('Asia',location_name)) %>%
  dplyr::select(-location_name) %>%
  dplyr::summarize(across(everything(), sum)) %>% dplyr::mutate(location_name="Asia")
death_cvd_EU<- gbd_2021_death_mini_cvd %>% dplyr::filter (.,grepl('Europe',location_name)) %>%
  dplyr::select(-location_name) %>%
  dplyr::summarize(across(everything(), sum)) %>% dplyr::mutate(location_name="Europe")
death_cvd_Asia<- gbd_2021_death_mini_cvd %>% dplyr::filter (.,grepl('Asia',location_name)) %>%
  dplyr::select(-location_name) %>%
  dplyr::summarize(across(everything(), sum)) %>% dplyr::mutate(location_name="Asia")

gbd_2021_death_mini_allcause <- bind_rows (gbd_2021_death_mini_allcause,death_allcause_EU,death_allcause_Asia) %>%
  filter (location_name=="Global"|location_name=="Asia"|location_name=="Europe"|location_name=="South Asia"|location_name=="Southeast Asia") %>%
  pivot_longer (!location_name, names_to = "year", values_to = "allcause")

gbd_2021_death_mini_cvd <- bind_rows (gbd_2021_death_mini_cvd,death_cvd_EU,death_cvd_Asia) %>%
  filter (location_name=="Global"|location_name=="Asia"|location_name=="Europe"|location_name=="South Asia"|location_name=="Southeast Asia") %>%
  pivot_longer (!location_name, names_to = "year", values_to = "cvd")

death_2000 <- gbd_cvd_death_mini %>% filter (year==2000) %>%
  dplyr::rename(location_name=region,allcause=Allcauses,cvd=Cardiovasculardiseases)

gbd_2021_death_mini_cvdallcause <-  gbd_2021_death_mini_allcause %>%
  left_join(.,gbd_2021_death_mini_cvd) %>%
  mutate(year = as.numeric(str_extract(year, "\\d+")))%>%
  mutate(location_name = str_replace(location_name, "South Asia", "SA")) %>%
  mutate(location_name = str_replace(location_name, "Southeast Asia", "SEA")) %>%
  mutate (prop=cvd/allcause*100) %>% bind_rows(death_2000) # if 2000 excluded, put # before %>% bind_rows (death_2000)


tiff(file="cvd_death_1990_2021_2.tiff",width=4, height=4, units="in", res=300)
gbd_2021_death_mini_cvdallcause %>%
  mutate (location_name=fct_relevel(location_name,"Global","Asia","SEA","SA","Europe")) %>%
  ggplot(aes(x=year,y=prop,group=location_name))+
  geom_line (aes(linetype=location_name,color=location_name),size=0.5)+
  geom_point(aes(shape=location_name,color=location_name),size=2.5)+
  theme_classic() + labs(y="Death, CVD (% Death, all causes)",x="year")+
  scale_x_continuous(breaks=c(1990,2010,2019,2021),guide = guide_axis(n.dodge = 2)) +
  theme(legend.position = "top",legend.title=element_blank(),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.line.x.top = element_blank())
dev.off()


#### Figure 1b, to illustrate that urbanisation contributes to disease burden in Asia
# This is to answer reviewer's request about temporal trend on disease burden, and role of urbanisation
# The figures are inspired by the layout in Our World in Data

setwd("")
daly_urban <- read.csv ("IHME-GBD_2021_DATA_total-disease-burden-by-cause_ChinaIndoIndiaEu_T2D.csv",fileEncoding="UTF-8-BOM")
urban <- read.csv ("GHS-COUNTRY-STATS R2024A_urbanisation_population-of-cities-town-and-villages_ChinaIndoIndiaEu.csv",fileEncoding="UTF-8-BOM")
                   
urban <-  urban %>% select (-Code) %>%
  select (-(`Population.living.in.villages.1`:`Population.living.in.cities.1`)) %>%
  dplyr::rename(country=Entity,year=Year,villages=`Population.living.in.villages`,towns=`Population.living.in.towns`,cities=`Population.living.in.cities`) %>%
  pivot_longer (!c(country,year),names_to="urban",values_to="val")

urban_plot <-urban %>% mutate (country=fct_relevel(country,"China","Indonesia","India","Europe")) %>%
  ggplot(aes(x=year,y=val,color=urban,fill=urban))+
  geom_area (color = NA)+
  theme_classic() + labs(y="Urbanisation",x="year")+
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))+
  scale_fill_manual(values= wesanderson::wes_palette(n=3, name="FantasticFox1"))+
  facet_wrap(vars(country),scales="free")+
  theme(legend.position = "top",legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

daly_urban <- daly_urban %>% filter (measure_name!="Deaths") %>%
  select(location_name,cause_name,metric_name,year,val) %>%
  dplyr::rename(country=location_name,cause=cause_name,percent=val) %>%
  filter (metric_name=="Percent") %>% select (-metric_name) %>%
  mutate(country = str_replace_all(country, "European Union", "Europe"),
         country = str_replace_all(country, "People's Republic of China", "China"),
         country = str_replace_all(country, "Republic of India", "India"),
         country = str_replace_all(country, "Republic of Indonesia", "Indonesia"),
         cause = str_replace_all(cause, "Communicable, maternal, neonatal, and nutritional diseases", "Communicable*"),
         cause = str_replace_all(cause, "Non-communicable diseases", "Non-communicable"),
         cause = str_replace_all(cause, "Diabetes mellitus type 2", "T2D"),
         percent=percent*100)

daly_urban_plot <- daly_urban %>% mutate (country=fct_relevel(country,"China","Indonesia","India","Europe")) %>%
  ggplot(aes(x=year,y=percent,color=cause,fill=cause))+
  geom_area (color = NA)+
  theme_classic() + labs(y="DALYs, %",x="year")+
  scale_fill_manual(values= wesanderson::wes_palette(n=4, name="GrandBudapest2"))+
  facet_wrap(vars(country),scales="free")+
  theme(legend.position = "top",legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

tiff(file="daly_urban_1980_2021_2.tiff",width=4.5, height=8, units="in", res=300)
gridExtra::grid.arrange(urban_plot,daly_urban_plot)
dev.off()

#### Figure 1c, to compare BMI, WHR, and vFMI across ethnicity using UKB data
ata_UKB_metab <- read.csv ("ukb_data_20221004_theresia.csv",fileEncoding="UTF-8-BOM") %>% select (-X)

# Calculate visceral FMI
# Based on visceral FMI GWAS, reduced model, in male and female separately
data_UKB_metab <- data_UKB_metab %>% dplyr::rename(ethnic=ethnic_background_f21000_0_0,
                                                   sex=sex_f31_0_0, age=age_when_attended_assessment_centre_f21003_0_0,
                                                   menopause=had_menopause_f2724_0_0,
                                                   waist=waist_circumference_f48_0_0, hip=hip_circumference_f49_0_0,
                                                   height=standing_height_f50_0_0,weight=weight_f21002_0_0, imp_whole=impedance_of_whole_body_f23106_0_0,
                                                   imp_leg_r=impedance_of_leg_right_f23107_0_0, imp_leg_l=impedance_of_leg_left_f23108_0_0,
                                                   imp_arm_r=impedance_of_arm_right_f23109_0_0, imp_arm_l=impedance_of_arm_left_f23110_0_0,
                                                   vat=vat_visceral_adipose_tissue_mass_f23288_2_0,
                                                   dbp_auto=diastolic_blood_pressure_automated_reading_f4079_0_0, dbp_man=diastolic_blood_pressure_manual_reading_f94_0_0,
                                                   hdl=hdl_cholesterol_f30760_0_0,trig=triglycerides_f30870_0_0,HbA1c=glycated_haemoglobin_hba1c_f30750_0_0)

data_UKB_metab_f <- data_UKB_metab %>% filter (sex==0) %>% #0 female, 1 male
  mutate (vat_pre=(-23.98*waist)+(26.49*hip)+(30.47*weight)+(81.49*imp_arm_l)+(111.3*imp_leg_l)+(-84.79*imp_whole)+
            (0.1464*age*weight)+(0.7378*waist*weight)+(-0.6726*hip*weight)+
            (-0.5411*imp_arm_l*height)+(-0.7114*imp_leg_l*height)+(0.5659*imp_whole*height)-2873)
data_UKB_metab_m <- data_UKB_metab %>% filter (sex==1) %>%
  mutate (vat_pre=(-16.67*age)+(13.47*waist)+(-53.56*height)+(65.93*weight)+
            (0.3879*age*weight)+(0.2488*waist*weight)+(-0.4401*hip*weight)+
            (-0.08245*imp_arm_l*height)+(-0.07416*imp_leg_l*height)+(0.09465*imp_whole*height)+3364) #there is typo in the supplementary

data_UKB_metab_vat <- rbind (data_UKB_metab_f,data_UKB_metab_m)

# Select and collapse ethnicity. Indian also includes Pakistani and Bangladeshi. African means Afro-Caribbean and Black African
# Collapse WHR, BMI
data_UKB_metab_vat <- data_UKB_metab_vat %>%
  select (eid:age,waist:weight,dbp_auto:vat_pre) %>% filter (ethnic >0) %>% # remove do not know, or prefer not to answer
  filter (ethnic!=2 & ethnic!=2001 & ethnic!=2002 & ethnic!=2003 & ethnic!=2004) %>% # remove mixed ethnicity
  filter (ethnic!=3 & ethnic!=4 & ethnic!=6 & ethnic!=3004) %>% # remove ethnicity which origins are uncertain
  mutate(ethnic_rev=ethnic) %>%
  mutate(ethnic_rev=as.factor(ethnic_rev))%>% 
  mutate_at(vars(contains('ethnic_rev')),~case_when(.=="5"~"Chinese",
                                                    .=="1" |. =="1001" | .=="1002" | .=="1003" | .=="1004" ~"European",
                                                    . =="3001" | .=="3002" | .=="3003" ~"Indian", 
                                                    . =="4001" | .=="4002" | .=="4003"~"African")) %>%
  select (ethnic,waist:weight,vat_pre,ethnic_rev) %>% 
  mutate (BMI=weight/(height/100)^2,vFMI=(vat_pre/1000)/(height/100)^2) %>%
  filter (vat_pre>0) %>% dplyr::rename (Ethnicity=ethnic_rev) %>%
  select (Ethnicity,BMI,waist,vFMI)

UKB <- data_UKB_metab_vat %>% pivot_longer (!Ethnicity,names_to="adipoq",values_to="value")%>%
  group_by (Ethnicity,adipoq) %>% summarise (mean=mean(value),sd=sd(value),n=n()) %>%
  mutate (sem=sd/sqrt(n), upper_ci=mean+(1.96*sem),lower_ci=mean-(1.96*sem)) %>% filter (Ethnicity!="African")

# Make forest plot
global_adipoq_forestplot <- UKB %>%
  ggplot(aes(x=Ethnicity,y=mean, ymin=lower_ci, ymax=upper_ci,shape=Ethnicity)) +
  geom_pointrange() +  # flip coordinates (puts labels on y axis)
  scale_shape_manual(values = c(1,2,0))+
  coord_flip()+ 
  xlab("") + ylab("mean(95%CI)")+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_grid(~factor(adipoq,level=c('BMI','waist','vFMI')),scales="free")+
  theme(legend.position = "top")

tiff(file="global_adipoq_forestplot.tiff",
     width=5, height=3, units="in", res=300)
global_adipoq_forestplot
dev.off()

ethnic_legend <- cowplot::get_legend(global_adipoq_forestplot) #grab legend

global_adipoq_forestplot_BMI <- UKB %>% filter (adipoq=="BMI") %>% 
  ggplot(aes(x=Ethnicity,y=mean, ymin=lower_ci, ymax=upper_ci,shape=Ethnicity)) +
  geom_pointrange(linewidth = 0.5) +  # flip coordinates (puts labels on y axis)
  scale_shape_manual(values = c(1,2,0))+
  coord_flip()+ ylim(22,30)+ 
  xlab("") + ylab("")+theme_classic()+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.text.y=element_blank()) + ggtitle(bquote('BMI,'~Kg/m^2))
global_adipoq_forestplot_waist <- UKB %>% filter (adipoq=="waist") %>% 
  ggplot(aes(x=Ethnicity,y=mean, ymin=lower_ci, ymax=upper_ci,shape=Ethnicity)) +
  geom_pointrange(linewidth = 0.5) +  # flip coordinates (puts labels on y axis)
  scale_shape_manual(values = c(1,2,0))+
  coord_flip()+ ylim(75,100)+ 
  xlab("") + ylab("")+theme_classic()+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.text.y=element_blank()) + ggtitle("waist, cm")
global_adipoq_forestplot_vFMI <- UKB %>% filter (adipoq=="vFMI") %>% 
  ggplot(aes(x=Ethnicity,y=mean, ymin=lower_ci, ymax=upper_ci,shape=Ethnicity)) +
  geom_pointrange(linewidth = 0.5) +  # flip coordinates (puts labels on y axis)
  scale_shape_manual(values = c(1,2,0))+
  coord_flip()+ xlab("") + ylab("")+theme_classic()+
  theme(legend.position = "none",axis.title.y=element_blank(), axis.text.y=element_blank()) + ggtitle(bquote('vFMI,'~Kg/m^2))
global_adipoq_forestplot_comb<- gridExtra::grid.arrange(global_adipoq_forestplot_BMI,global_adipoq_forestplot_waist,global_adipoq_forestplot_vFMI,ncol=3)

tiff(file="global_adipoq_forestplot_2.tiff",
     width=5, height=3, units="in", res=300)
gridExtra::grid.arrange(ethnic_legend,global_adipoq_forestplot_comb,heights=c(0.5,4))
dev.off()

#### Figure 1D was reproduced from the submitted versions of https://www.thelancet.com/article/S2213-8587(24)00195-5/abstract



#### Figure 2, disease risk factors contributing to the DALYs of CVD in 2021
global_dalys <- read.csv ("global_1549yo_bothsex_cvddiab.csv",fileEncoding="UTF-8-BOM") %>%
  select (cause_name, location_name, metric_name, val, upper, lower) %>%
  filter (metric_name == "Rate") %>%
  select (-metric_name)


global_dalys_cvddiab_forestplot <- global_dalys %>%
  mutate (cause_name=case_when(cause_name=="Cardiovascular diseases"~"CVD",
                               cause_name=="Diabetes and kidney diseases"~"Diabetes, CKD")) %>%
  mutate (location_name=fct_relevel(location_name,"Europe","East Asia", "South Asia", "Southeast Asia")) %>%
  ggplot(aes(x=cause_name,y=val, ymin=lower, ymax=upper,color=location_name,shape=location_name)) +
  geom_pointrange(position = position_dodge(width = 1), size=1) +
  scale_color_manual(values= wes_palette("Darjeeling1", n = 4))+
  scale_shape_manual(values = c(15,16,17,18))+
  xlab("2021") + ylab("DALYS rate, per 100,000")+theme_classic()+
  theme(legend.position = "top", legend.title=element_blank())+
  guides(color=guide_legend(nrow=2,byrow=TRUE))

sea_dalys <- read.csv ("sea_1549yo_bothsex_cvddiab.csv",fileEncoding="UTF-8-BOM") %>%
  select (Location,Cause.of.death.or.injury:Upper.bound) %>%
  filter (Measure=="DALYs per 100,000") %>% select (-Measure) %>%
  filter (Cause.of.death.or.injury=="Cardiovascular diseases") %>%
  select (-Cause.of.death.or.injury) %>%
  dplyr::rename (risk_factor=2,lower=4,upper=5) %>%
  filter (!is.na(Value))
ea_dalys <- read.csv ("ea_1549yo_bothsex_cvddiab.csv",fileEncoding="UTF-8-BOM") %>%
  select (Location,Cause.of.death.or.injury:Upper.bound) %>%
  filter (Measure=="DALYs per 100,000") %>% select (-Measure) %>%
  filter (Cause.of.death.or.injury=="Cardiovascular diseases") %>%
  select (-Cause.of.death.or.injury) %>%
  dplyr::rename (risk_factor=2,lower=4,upper=5) %>%
  filter (!is.na(Value))
sa_dalys <- read.csv ("sa_1549yo_bothsex_cvddiab.csv",fileEncoding="UTF-8-BOM") %>%
  select (Location,Cause.of.death.or.injury:Upper.bound) %>%
  filter (Measure=="DALYs per 100,000") %>% select (-Measure) %>%
  filter (Cause.of.death.or.injury=="Cardiovascular diseases") %>%
  select (-Cause.of.death.or.injury) %>%
  dplyr::rename (risk_factor=2,lower=4,upper=5) %>%
  filter (!is.na(Value))
eu_dalys <- read.csv ("eu_1549yo_bothsex_cvddiab.csv",fileEncoding="UTF-8-BOM") %>%
  select (Location,Cause.of.death.or.injury:Upper.bound) %>%
  filter (Measure=="DALYs per 100,000") %>% select (-Measure) %>%
  filter (Cause.of.death.or.injury=="Cardiovascular diseases") %>%
  select (-Cause.of.death.or.injury) %>%
  dplyr::rename (risk_factor=2,lower=4,upper=5) %>%
  filter (!is.na(Value)) 
  
global_dalys_riskfactor <- eu_dalys %>% rbind (.,ea_dalys) %>% rbind (.,sa_dalys) %>%
  rbind (.,sea_dalys)

pal <- wes_palette(9, name = "GrandBudapest2", type = "continuous")
global_dalys_riskfactor_bar <- global_dalys_riskfactor %>%
  filter (risk_factor!="Air pollution" &
          risk_factor!="Non-optimal temperature"&
          risk_factor!="Other environmental risks") %>% # remove environmental factors
  filter (Value>0) %>%
  mutate (risk_factor=case_when(risk_factor=="High body-mass index"~"High BMI",
                                risk_factor=="High systolic blood pressure"~"High BP",
                                risk_factor=="High LDL Cholesterol"~"High LDL",
                                risk_factor=="High fasting plasma glucose"~"High fasting glucose",T~risk_factor)) %>%
  ggplot (aes(x=Location,y=Value,fill=risk_factor))+
  geom_bar (stat="Identity", colour="white") + theme_classic () +
  scale_fill_manual(values= pal)+
  xlab("Risk factor for CVD, 2021") + ylab("DALYS rate, per 100,000")+
  coord_flip()+
  # geom_text(aes(label=paste0(sprintf("%1.1f", Value))),
           # position=position_stack(vjust=0.5),check_overlap = TRUE)+
  theme(legend.position = "top", legend.title=element_blank())+
  guides(fill=guide_legend(nrow=3))

sea_diet <- read.csv ("sea_1549yo_bothsex_cvddiab_diet.csv",fileEncoding="UTF-8-BOM") %>%
  select (Location,Cause.of.death.or.injury:Upper.bound) %>%
  filter (Measure=="DALYs per 100,000") %>% select (-Measure) %>%
  filter (Cause.of.death.or.injury=="Cardiovascular diseases") %>%
  select (-Cause.of.death.or.injury) %>%
  dplyr::rename (risk_factor=2,lower=4,upper=5) %>%
  filter (!is.na(Value))
ea_diet <- read.csv ("ea_1549yo_bothsex_cvddiab_diet.csv",fileEncoding="UTF-8-BOM") %>%
  select (Location,Cause.of.death.or.injury:Upper.bound) %>%
  filter (Measure=="DALYs per 100,000") %>% select (-Measure) %>%
  filter (Cause.of.death.or.injury=="Cardiovascular diseases") %>%
  select (-Cause.of.death.or.injury) %>%
  dplyr::rename (risk_factor=2,lower=4,upper=5) %>%
  filter (!is.na(Value))
sa_diet <- read.csv ("sa_1549yo_bothsex_cvddiab_diet.csv",fileEncoding="UTF-8-BOM") %>%
  select (Location,Cause.of.death.or.injury:Upper.bound) %>%
  filter (Measure=="DALYs per 100,000") %>% select (-Measure) %>%
  filter (Cause.of.death.or.injury=="Cardiovascular diseases") %>%
  select (-Cause.of.death.or.injury) %>%
  dplyr::rename (risk_factor=2,lower=4,upper=5) %>%
  filter (!is.na(Value))
eu_diet <- read.csv ("eu_1549yo_bothsex_cvddiab_diet.csv",fileEncoding="UTF-8-BOM") %>%
  select (Location,Cause.of.death.or.injury:Upper.bound) %>%
  filter (Measure=="DALYs per 100,000") %>% select (-Measure) %>%
  filter (Cause.of.death.or.injury=="Cardiovascular diseases") %>%
  select (-Cause.of.death.or.injury) %>%
  dplyr::rename (risk_factor=2,lower=4,upper=5) %>%
  filter (!is.na(Value))

global_diet_riskfactor <- eu_diet %>% rbind (.,ea_diet) %>% rbind (.,sa_diet) %>%
  rbind (.,sea_diet)

pal <- wes_palette(13, name = "Darjeeling1", type = "continuous")
global_diet_riskfactor_bar <- global_diet_riskfactor %>%
  filter (Value>0) %>%
  mutate(risk_factor = str_remove(risk_factor, "Diet ")) %>%
  mutate(risk_factor = str_remove(risk_factor, " in")) %>%
  mutate(risk_factor = str_remove(risk_factor, " polyunsaturated")) %>%
  mutate(risk_factor = str_remove(risk_factor, " fatty acids")) %>%
  mutate(risk_factor = str_remove(risk_factor, "sugar-")) %>%
  ggplot (aes(x=Location,y=Value,fill=risk_factor))+
  geom_bar (stat="Identity", colour="white") + theme_classic () +
  scale_fill_manual(values= pal)+
  xlab("Risk factor for CVD, 2021") + ylab("DALYS rate, per 100,000")+
  coord_flip()+
  # geom_text(aes(label=paste0(sprintf("%1.1f", Value))),
            # position=position_stack(vjust=0.5),check_overlap = TRUE)+
  theme(legend.position = "top", legend.title=element_blank())+
  guides(fill=guide_legend(nrow=3))

global_dalys_cvd_rf <- gridExtra::grid.arrange(global_dalys_cvddiab_forestplot,global_dalys_riskfactor_bar,ncol=2,widths=c(2,5))


tiff(file="global_dlays_cvd_rf_diet_2021GBD_2.tiff",width=10, height=6, units="in", res=300)
gridExtra::grid.arrange(global_dalys_cvd_rf,global_diet_riskfactor_bar)
dev.off()

### Figure 3A was recreated using the QGIS freeware using publicly available data (no R script available).
# Please refer our pre-print https://www.medrxiv.org/content/10.1101/2024.05.14.24307259v2 for further details of the geospatial analysis


#### Figure 3B, donut chart to illustrate proportion of people unable to afford healthy diet based on FAO, 2021
data_donut_FAO<- read.csv ("FAO_2023_foodsecurity.csv",fileEncoding="UTF-8-BOM")
data_donut_FAO<- data_donut_FAO %>% mutate (ymax=cumsum(prop))
data_donut_FAO$ymin = c(0, head(data_donut_FAO$ymax, n=-1))


tiff(file="donut_FAO_foodinsecure.tiff",
     width=5, height=3, units="in", res=300)
ggplot (data_donut_FAO,aes(ymax=ymax,ymin=ymin,xmax=2,xmin=-2,fill=region)) +
  geom_rect(color="white", alpha=0.6) + coord_polar(theta="y",direction = -1) + xlim (c(-8,3)) + theme_void()+
  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling1"))
dev.off()


#### Figure 3C, donut chart to illustrate proportion of people unable to afford healthy diet based on FAO, 2021
# Based on the 2020 Hunger Report by Lien Centre for Social Innovation, Singapore
data_donut_sg<- read.csv ("Lienreport_2020_SGhunger.csv",fileEncoding="UTF-8-BOM")
data_donut_sg<- data_donut_sg %>% mutate (ymax=cumsum(prop))
data_donut_sg$ymin = c(0, head(data_donut_sg$ymax, n=-1))

tiff(file="donut_sg_foodinsecure.tiff",
     width=5, height=3, units="in", res=300)
ggplot (data_donut_sg,aes(ymax=ymax,ymin=ymin,xmax=2,xmin=-2,fill=reasons)) +
  geom_rect(color="white", alpha=0.6) + coord_polar(theta="y",direction = -1) + xlim (c(-8,3)) + theme_void()+
  scale_fill_manual(values=wes_palette(n=5, name="Zissou1"))
dev.off()



#### Figure 4A, Compare the odds of T2D for trans-ancestry PRS across 10 cohorts, from S Table 19, Mahajan et al 
# DOI 10.1038/s41588-022-01058-3
# Set dataframe

mahajan_st19 <- data.frame(
  ancestry =c("Africa","Africa","East Asia","East Asia","European", "European","Hispanic","Hispanic","South Asian","South Asian"),
  study=c("WHI(AFR)","DDS/DDC","KBA(2)","SIMES","UKBB(EUR)","EPIC-INTERACT(2)","HCHS/SOL","MC(2)","PROMIS(1)","RHS"),
  n=c(6940,2426,23515,1930,73138,8440,6674,1783,7447,1712),
  log_or=c(0.695,0.535,1.154,0.682,1.010,0.963,1.104,0.641,0.765,1.131),
  se=c(0.053,0.099,0.026,0.096,0.015,0.046,0.058,0.080,0.049,0.103),
  bmi=c(33.0,34.4,25.0,27.8,31.9,30.0,32.2,29.6,25.9,24.8) # of the case, from ST2
)

mahajan_st19 <- mahajan_st19 %>% mutate (or=exp(log_or),lower_ci=exp(log_or -(1.96*se)), upper_ci=exp(log_or+(1.92*se)))

# Make forest plot
pdf(file="t2d_prs_global_mahajan_2.pdf",width=6, height=5)
mahajan_st19 %>% mutate (study_n = paste0(study,", n=",n)) %>%
  ggplot(aes(x=fct_reorder(study_n,ancestry),y=or, ymin=lower_ci, ymax=upper_ci,color=ancestry)) +
  geom_pointrange(size=1) +
  coord_flip()+
  geom_hline(yintercept=1,linetype="dashed",color="grey")+
  scale_color_manual(values=wes_palette(n=5, name="Darjeeling2"))+
  xlab("") + ylab("OR (95%CI) T2D using trans-ancestry PRS")+theme_classic()+
  theme(legend.position = "top",legend.title=element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size=12.5))+
  guides(color = guide_legend(nrow = 2))
dev.off()


# In response to reviewer 1, perform heterogeneity testing
mahajan_st19_forest <- mahajan_st19 %>% mutate (yi=log(or), vi=se^2, ancestry=as.factor(ancestry))

rma(yi=yi, vi=vi,data=mahajan_st19_forest , method="REML", mods=~ancestry + bmi, weighted=T)
rma(yi=yi, vi=vi,data=mahajan_st19_forest , method="REML", weighted=T)




#### Revision Figure 1, in response to reviewer 1's comments
# Upload the individual T2D PRS score from Ge et al
# Please refer our pre-print https://www.medrxiv.org/content/10.1101/2024.05.14.24307259v2 for further details of the PRS analysis
prs_t2d <- read.csv ("PRS_disorders_all_std_append_3.csv",fileEncoding="UTF-8-BOM")
prs_t2d <- prs_t2d %>% select (IID,FREG0_PID, FREG8_Age,FREG7_Gender,Ethnicity,PRS_T2D,T2D)

# Upload the T2D PRS score generated using Suzuki et al 2024 summary statistics with thanks to Dr Pritesh R Jain
prs_t2d_suzuki <- read.table ("T2D_PRS_Suzuki_all_models.txt",header = TRUE, sep = "\t")

prs_t2d <- prs_t2d %>% left_join(.,prs_t2d_suzuki)

# Make histograms comparing PRS distribution across ethnicity using Ge et al or Suzuki et al
prs_t2d_histo <- prs_t2d %>% select (PRS_T2D,SCORE_5e08:SCORE_1,Ethnicity) %>%
  dplyr::rename(Ge_2022=PRS_T2D,Suzuki_001=SCORE_001,Suzuki_05=SCORE_05,Suzuki_1=SCORE_1,Suzuki_1e05=SCORE_1e05,Suzuki_5e08=SCORE_5e08) %>%
  pivot_longer(!Ethnicity,names_to="score",values_to = "prs") %>%
  mutate(Ethnicity=as.factor(Ethnicity),score=as.factor(score))

theme_set(theme_classic())
prs_t2d_histo_plot <- prs_t2d_histo %>% mutate(Ethnicity = fct_relevel(Ethnicity, "Chinese", "Malay","Indian")) %>%
  mutate (score=fct_relevel(score,"Ge_2022","Suzuki_5e08","Suzuki_1e05","Suzuki_001","Suzuki_05","Suzuki_1")) %>%
  filter (score!="Suzuki_05") %>% filter (score!="Suzuki_1") %>% # As per discussion with with JC and PRJ 6/2/2025
  ggplot (.,aes(x=prs,fill=Ethnicity)) +
  geom_density(alpha = 0.8)+ facet_wrap(~score,nrow=1)+ theme (legend.position = "top") +
  scale_fill_manual(values=c("#F9C5B4", "#B5DDD0", "#BB94C4"))


# Make multiple forest plots to compare T2D odds using various PRS across ethnicity, usingsummary statistics from Ge et al or Suzuki et al

# Ethnic-specific T2D odds using various PRS from DIAGRAM consortium was generated with help from Dr. Pritesh R. Jain
# Please refer our pre-print https://www.medrxiv.org/content/10.1101/2024.05.14.24307259v2 for further details of the PRS analysis

prs_ethnic_or_compare_df <- read_excel("Suzuki_T2D_PRS_performance.xlsx",sheet=1, na = c("","NA")) %>%
  mutate (Thresholds=
            case_when(Thresholds=="1.0000000000000001E-5"~"Suzuki_1e05",
                      Thresholds=="1E-3"~"Suzuki_001",
                      Thresholds=="4.9999999999999998E-8"~"Suzuki_5e08",
                      Thresholds=="Ge et al"~"Ge_2022",
                      T~Thresholds)) %>%
  filter (Thresholds!="0.05") %>%  filter (Thresholds!="1") %>%
  select (-contains('trans_')) %>%
  pivot_longer (!Thresholds,names_to="var",values_to="value") %>%
  mutate(ethnic = case_when(
    str_detect(var, "Chinese") ~ "Chinese",
    str_detect(var, "Malay") ~ "Malay",
    str_detect(var, "Indian") ~ "Indian"),
    stat = case_when(
      str_detect(var, "_OR") ~ "or",
      str_detect(var, "_Upper") ~ "ci_upper",
      str_detect(var, "_Lower") ~ "ci_lower")) %>% filter (!is.na(stat)) %>% select(-var) %>%
  pivot_wider(id_cols=c(Thresholds,ethnic),names_from = stat,values_from=value)

tiff(file="t2d_prs_or_ethnic_ge_suzuki2.tif",width=8, height=2, units="in", res=300)
prs_ethnic_or_compare <- prs_ethnic_or_compare_df %>%
  mutate (Thresholds=fct_relevel(Thresholds,"Ge_2022","Suzuki_5e08","Suzuki_1e05","Suzuki_001")) %>%
  ggplot(., aes(x=factor(ethnic, level=c("Indian","Malay","Chinese")),
                y=or,ymin=ci_lower, ymax=ci_upper,
                color=factor(ethnic, level=c("Indian","Malay","Chinese")),
                shape=factor(ethnic, level=c("Indian","Malay","Chinese")))) +
  geom_pointrange(fatten = 5) + coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_color_manual(values = c("#BB94C4","#B5DDD0","#F9C5B4"))+
  scale_shape_manual(values = c(15,17,16))+
  geom_hline(yintercept=1,linetype="dashed")+
  xlab("") + ylab("OR(95% CI)")+theme_classic()+
  theme(legend.position="none",axis.text.y=element_blank())+
  facet_wrap(~Thresholds,nrow=1)
dev.off()

tiff(file="t2d_prs_ethnic_ge_suzuki3.tif",width=6, height=5, units="in", res=300)
grid.arrange(prs_t2d_histo_plot,prs_ethnic_or_compare,nrow=2,heights=c(2.5,2))
dev.off()




#### Figure 4B, compare EAF of T2D SNP across Europe, East Asian and South Asian using Suzuki et al 2024 STable 15
setwd("C:/Users/Theresia.hm/Documents/Postdoc2016-/literature/manuscript ideas/adversity_healthoutcome")
suzuki_stable15 <- readxl::read_excel("2024_Nature_genetic_driver_het_T2D_supp.xlsx", sheet="ST15") %>%
  select (3,14:23,34:38) %>% slice(-1) %>% slice(-1) %>% slice(-1) %>% dplyr::rename(
    snp=1,n_zea=2,eaf_zea=3,b_zea=4,se_zea=5,p_zea=6,
    n_eu=7,eaf_eu=8,b_eu=9,se_eu=10,p_eu=11,
    n_sa=12,eaf_sa=13,b_sa=14,se_sa=15,p_sa=16) %>% pivot_longer (!snp,names_to="var",values_to="value") %>%
  separate (var, into=c("var","ethnic"),sep="_") %>% filter (!is.na(value)) %>%
  pivot_wider (names_from=var, values_from = value) %>%
  mutate (across(-c(snp,ethnic), as.numeric)) %>%
  filter (!is.na(eaf))

suzuki_stable15 %>% select (eaf) %>% distinct() %>% nrow()


tiff(file="suzuki_stable15_3119loci_EAF_comb.tif",width=2.5, height=2.5, units="in", res=300)
suzuki_stable15 %>%
  mutate (ethnic=case_when(ethnic=="eu"~"European",ethnic=="sa"~"SouthAsian",ethnic=="zea"~"EastAsian")) %>%
  ggplot (.,aes(x=eaf, color=ethnic, fill=ethnic))+
  geom_density(alpha=0.1)+ theme_classic()+
  scale_color_manual(values= wesanderson::wes_palette(n=3, name="GrandBudapest1")) +
  scale_fill_manual(values= wesanderson::wes_palette(n=3, name="GrandBudapest1")) +
  xlab("EAF")+
  theme(legend.position = "top",legend.title=element_blank(),legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y=element_blank())+
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         fill = guide_legend(nrow = 2, byrow = TRUE))
dev.off()

# Determine if EAF is significantly different across ethnicity
# However, note that EAF is a proportion and has U-shape distribution.
# Linear regression is inappropriate, but logistic regression is also not appropriate as EAF is not a binary variable.
# Therefore, use R package betareg
# It can accommodate u-shape distribution following 0,1 distribution
# https://cran.rstudio.com/web/packages/betareg/vignettes/betareg.html
# Adjust for sample size
# Inverse-variance weighted as per the reviewer's comments

lm_res <- betareg::betareg(eaf ~ ethnic + n,weights = 1/se^2, data = suzuki_stable15)
summary(lm_res)





#### Figure 4C, determine the heterogeneity of the effect size for each T2D independent loci across Europe, East Asian and South Asian using Suzuki et al 2024 STable 15
# Reupload table as it needs to be formatted slightly differently
suzuki_stable15 <- readxl::read_excel("2024_Nature_genetic_driver_het_T2D_supp.xlsx", sheet="ST15") %>%
  select (3,14:23,34:38) %>% slice(-1) %>% slice(-1) %>% slice(-1) %>% dplyr::rename(
    snp=1,n_ea=2,eaf_ea=3,beta_ea=4,se_ea=5,p_ea=6,
    n_eu=7,eaf_eu=8,beta_eu=9,se_eu=10,p_eu=11,
    n_sa=12,eaf_sa=13,beta_sa=14,se_sa=15,p_sa=16) %>% pivot_longer (!snp,names_to="var",values_to="value") %>%
  separate (var, into=c("var","ethnic"),sep="_") %>% filter (!is.na(value)) %>%
  pivot_wider (names_from=var, values_from = value) %>%
  mutate (across(-c(snp,ethnic), as.numeric)) %>%
  filter (!is.na(eaf))

names(suzuki_stable15)

snp_excl <- suzuki_stable15 %>% dplyr::count(snp) %>%
  filter(n <= 2) %>% select (snp) %>% pull()

suzuki_stable15_filter <- suzuki_stable15 %>% filter (!snp %in% snp_excl) # 3828 for 3 ancestries, so 1914 variants 

hetero_beta <-function(x){
  y <- rma(yi=beta, sei=se,
           data=x, method="REML",weighted=T)
  yy <- y[["I2"]]
  return(yy)
}

suzuki_split <- suzuki_stable15_filter %>% # 1276 elements
  group_by(snp) %>%
  group_split() %>%
  set_names(lapply(split(suzuki_stable15_filter$snp, suzuki_stable15_filter$snp), unique))  # Optional: Rename list elements by id values

suzuki_split2 <- lapply (suzuki_split, hetero_beta)

suzuki_splitdf <- bind_rows (suzuki_split2)
suzuki_splitdf <- suzuki_splitdf %>% t() %>% as.data.frame() %>%
  dplyr::rename(hetero_i2=1) %>% mutate (hetero_i2=round(hetero_i2)) %>%
  rownames_to_column(var="snp") %>% group_by(hetero_i2) %>% dplyr::summarise(n=dplyr::n()) %>%
  mutate (sumn=sum(n),prop=n/sumn*100)

tiff(file="suzuki_stable15_3119loci_hetero.tif",width=2.5, height=2, units="in", res=300)
suzuki_splitdf %>%
  ggplot (.,aes(x=hetero_i2,y=prop))+
  geom_point (size=0.2)+
  geom_line(color="grey70")+ theme_classic()+
  xlab("Heterogeneity (I^2)")+ ylab("Percentage(%) loci")
dev.off()





#### Figure 4D, to compare the prevalence of T2D across ethnicity in the HELIOS Study, with Ge et al 2022
# The HELIOS Study: https://www.thelancet.com/article/S2213-8587(24)00195-5/abstract
# Ge, T. et al. Development and validation of a trans-ancestry polygenic risk score for type 2 diabetes in diverse populations. Genome Med 14, 70 (2022).

disease_prop <- data.frame(
  Ethnicity =c("Chinese","Malay","Indian","Chinese","Malay","Indian"),
  Freq=c(4.8,13.4,17.4,5.1,0,0),
  disease=c("SG100K","SG100K","SG100K","Ge_2022","Ge_2022","Ge_2022")
)

pdf(file="t2d_prev_helios_ethnic_2.pdf",width=2, height=2) # need to match colour scheme with Pritesh
disease_prop %>% mutate (Ethnicity=fct_relevel(Ethnicity,"Chinese","Malay","Indian")) %>% ggplot(.,aes(x=disease,y=Freq,fill=Ethnicity))+
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("#F9C5B4", "#B5DDD0", "#BB94C4"))+
  geom_text(aes(label = Freq, y = Freq + 3),
            position = position_dodge(width = 0.9), size = 3)+theme_classic()+
  labs(y = "T2D Case (%)",x = "") + theme(legend.position="none")
dev.off()

# The PRS distribution was reproduced from Figure 3 of the HELIOS Study protocol paper, with permission of Dr. Pritesh R. Jain
# Please refer our pre-print https://www.medrxiv.org/content/10.1101/2024.05.14.24307259v2 for further details of the PRS analysis




#### Figure 4E, to compare the contribution of T2D PRS to ethnic differences in T2D burden in the HELIOS Study
# Mathematical formula will also be available from the published version of https://www.medrxiv.org/content/10.1101/2024.05.14.24307259v2
# At the point of this GitHub commit, it is under review in Nature Communication

prs_t2d <- read.csv ("PRS_disorders_all_std_append_3.csv",fileEncoding="UTF-8-BOM")
prs_t2d <- prs_t2d %>% select (IID,FREG0_PID, FREG8_Age,FREG7_Gender,Ethnicity,PRS_T2D,T2D)

prs_t2d <- prs_t2d %>% mutate (chi_mal=case_when(Ethnicity=="Chinese"~0,Ethnicity=="Malay"~1,T~NA),
                               chi_ind=case_when(Ethnicity=="Chinese"~0,Ethnicity=="Indian"~1,T~NA)) %>%
  filter (!is.na(T2D))

# For Malay and Indian, respectively, we derived mean difference in T2D PRS (∆PRS) and its standard error (SE_∆PRS) with Chinese as reference. 
sum_aov <- aov (data=prs_t2d,PRS_T2D~Ethnicity)
tukey_aov <- TukeyHSD (sum_aov)

# We extracted β(SE) of PRS-T2D from a logistic regression of T2D ~ PRS + age + sex + Ethnicity.
paf_t2d_prs <- glm (T2D~PRS_T2D+FREG8_Age+FREG7_Gender+Ethnicity,data=prs_t2d, family=binomial()) 

# We also calculated β(SE) T2D after adjusting for PRS from T2D ~ PRS + age + sex + Ethnic Malay vs. Chinese, or
# T2D ~ PRS + age + sex + Ethnic Indian vs.Chinese, accordingly.
paf_t2d_c_m <- glm (T2D~FREG8_Age+FREG7_Gender+chi_mal,data=prs_t2d,family=binomial()) 
paf_t2d_c_i <- glm (T2D~FREG8_Age+FREG7_Gender+chi_ind,data=prs_t2d,family=binomial()) 
paf_t2d_prs_c_m <- glm (T2D~zPRS_T2D+FREG8_Age+FREG7_Gender+chi_mal,data=prs_t2d,family=binomial()) 
paf_t2d_prs_c_i <- glm (T2D~zPRS_T2D+FREG8_Age+FREG7_Gender+chi_ind,data=prs_t2d,family=binomial()) 

# Subsequently, changes in the T2D effect due to PRS, denoted as β PRS_∆T2D = ∆PRS * βPRS_T2D. 
# Using the error propagation method, where we accounted for
  # i) the uncertainty in T2D PRS and
  # ii) the effect of PRS on T2D
# SE of PRS-∆T2D=√((SE_∆PRS)^2*(β PRS-T2D)^2 )+((SE_PRS-T2D)^2×(∆PRS)^2).

# The attributable fraction (AF) of T2D PRS for the difference in T2D burden =(∆PRS× β PRS_T2D)⁄β T2D 

# To derive the SE for AF, we calculated SE for the ratio of 2 β using delta method.

# √(SE_β1^2/β_2^2)+(β_1^2/β_2^4 ×SE_β2^2).

# Therefore, SE for AF =  √((SE PRS-∆T2D)^2/(β PRS-T2D)^2 )+((β PRS-∆T2D)^2/(β PRS-T2D)^4 × (SE_T2D)^2). 
# Therefore, the upper and lower limits for AF = AF(PRS-∆T2D)±(1.96×SE_AF_PRS-∆T2D)).

# hand calculation performed using Ms Excel.

# For Malay vs Chinese: 3.1% (-5.4, 11.6) of the excess risk of T2D
# For Indian vs Chinese: 5.7% (-1.2, 12.6) of the excess risk of T2D

# Make donut
C_M_par_prs <- tibble (par=c(3.1,100-3.1)) %>% mutate (ymax=cumsum(par),ymin=c(0,3.1))
C_I_par_prs <- tibble (par=c(5.7,100-5.7)) %>% mutate (ymax=cumsum(par),ymin=c(0,5.7))


C_M_par_prs <- ggplot (C_M_par_prs,aes(ymax=ymax,ymin=ymin,xmax=2,xmin=-2,fill=factor(par))) +
  geom_rect(color="black", alpha=0.6) + coord_polar(theta="y",direction = -1) + xlim (c(-8,2)) + theme_void() +
  theme(legend.position = "none") +
  annotate("text",label = paste0(3.1, "%"),fontface = "bold",color = "#B5DDD0",size = 8,x = -8,y = 0)+
  scale_fill_manual(values = c("#B5DDD0", "grey90"))
C_I_par_prs <-ggplot (C_I_par_prs,aes(ymax=ymax,ymin=ymin,xmax=2,xmin=-2,fill=factor(par))) +
  geom_rect(color="black", alpha=0.6) + coord_polar(theta="y",direction = -1) + xlim (c(-8,2)) + theme_void() +
  theme(legend.position = "none") +
  annotate("text",label = paste0(5.7, "%"),fontface = "bold",color = "#BB94C4",size = 8,x = -8,y = 0)+
  scale_fill_manual(values = c("#BB94C4", "grey90"))

pdf(file="t2d_prs_par_ethnic_2.pdf",width=5, height=2)
gridExtra::grid.arrange(C_M_par_prs,C_I_par_prs,ncol=2)
dev.off()


#### Figure 5 and 6 were was constructed with BioRender.com