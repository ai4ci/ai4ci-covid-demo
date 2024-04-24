# Timeline ----

dbreaks = growthrates::full_seq(c(as.Date("2020-01-01"),raw_cases$date),"2 month")

fit2 = growthrates::england_covid_pcr_positivity %>% growthrates::proportion_locfit_model()
events = england_events %>% 
  filter(!label %in% c("Eat out to help out","Schools reopen","BA.4 first UK sequences")) %>%
  mutate(label = label %>% stringr::str_replace("first UK sequences", "emergence"))

p_timeline = ggplot(fit2)+
  geom_events(events, event_label_size = 10, breaks = dbreaks, date_labels = "%b %y",event_label_angle = -15)+
  geom_line(aes(x=as.Date(time), y=proportion.0.5*100), colour="blue",linewidth = 1)+
  geom_area(aes(x=as.Date(time), y=proportion.0.5*100),fill="blue",alpha=0.2)+
  theme(axis.title.x = element_blank())+
  ylab("positivity rate (%)")

## Vaccination ----

raw_vaccinations = cached_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&metric=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage&metric=cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage&format=csv")

p_vaccine = ggplot(raw_vaccinations,aes(x=date))+
  geom_area(aes(y=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage, fill="first"), colour="black",size=0.1)+
  geom_area(aes(y=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage, fill="second"), colour="black",size=0.1)+
  geom_area(aes(y=cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage, fill="third"), colour="black",size=0.1)+
  scale_fill_brewer(palette="Greens", name=NULL)+
  scale_x_date(breaks = dbreaks, date_labels = "%b %y")+
  ylim(c(0,100))+
  ylab(NULL)+
  xlab(NULL)+
  theme(legend.position = c(0.1,0.8),legend.background = element_blank())+
  facet_wrap(~"vaccine uptake (%)")