
raw_cases = cached_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv")
raw_tests = cached_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newVirusTestsBySpecimenDate&format=csv")
raw_pings = cached_csv("https://assets.publishing.service.gov.uk/media/64662d3dd3231e001332dc59/covid19_app_data_by_local_authority.csv")
raw_app_users = cached_csv("https://assets.publishing.service.gov.uk/media/64662d58e14070000cb6e1f8/covid19_app_data_on_number_of_app_users.csv")

if (!fs::file_exists("~/tmp/dashboard/pop.xls")) download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2019april2019localauthoritydistrictcodes/ukmidyearestimates20192019ladcodes.xls","~/tmp/dashboard/pop.xls")
raw_pop = readxl::read_excel("~/tmp/dashboard/pop.xls",sheet = "MYE3",skip = 4)

raw_pop_strat = readxl::read_excel("~/tmp/dashboard/pop.xls",sheet = "MYE2 - Persons", skip = 4)

## Population estimates ----

lad20cov_18_65_pop = raw_pop_strat %>% select(-Geography1, -`All ages`) %>% 
  pivot_longer(cols=-c(Code,Name), names_to = "age", values_to = "population") %>%
  filter(age>=18 & age <=65) %>%
  group_by(areaCode = Code, areaName = Name) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  lad_merge(population = sum(population)) %>%
  semi_join(lad20map, by=c("areaCode","areaName")) %>%
  ungroup() %>%
  mutate(proportion.18_65 = population/sum(population)) %>%
  group_by(areaName,areaCode)

lad20cov_pop = raw_pop %>%
  transmute(areaName = Name, areaCode = Code, population = `Estimated Population  mid-2019`) %>% 
  lad_merge(population = sum(population)) %>%
  semi_join(lad20map, by=c("areaCode","areaName")) %>%
  ungroup() %>%
  mutate(proportion.pop = population/sum(population)) %>%
  group_by(areaName,areaCode)

# lad20cov_pop %>% 
#   inner_join(lad20cov_18_65_pop, by=c("areaCode","areaName"), suffix=c(".pop",".18_65")) %>% 
#   mutate(rr = proportion.18_65/proportion.pop) %>%
#   arrange(desc(rr)) %>%
#   view()

## Active app user estimates ---

active_users = raw_app_users %>% transmute(
  time = as.time_period(`Date (Dyddiad)`, start_date = "2020-01-01", unit="7 day"),
  users = `Users with contact tracing enabled (Defnyddwyr ag olrhain cyswllt wedi'u galluogi)`
)

# tmp = raw_pings %>% 
#   transmute(
#     date = `Week starting (Wythnos yn dechrau)`,
#     areaName = `Local authority (Awdurdod lleol)` %>% stringr::str_replace(" / .*$",""),
#     activity = `Check-ins (Cofrestriadau)`, # or `Positive test results linked to app (Canlyniadau prawf positif)`+`Negative test results linked to app (Canlyniadau prawf negatif)`
#   ) %>% 
#   group_by(date) %>%
#   # only look at days where we have complete data
#   filter(!any(is.na(activity))) %>%
#   group_by(areaName) %>%
#   summarise(count = sum(activity)) %>%
#   lad_merge(count = sum(count)) %>% 
#   ungroup() %>% select(-areaCode) %>%
#   mutate(
#     proportion = count/sum(count,na.rm = TRUE)
#   ) %>% 
#   group_by(areaName) %>%
#   semi_join(lad20map, by=c("areaName"))
# 
# lad20cov_pop %>% ungroup() %>% mutate(proportion = population/sum(population)) %>%
#   inner_join(tmp, by="areaName", suffix=c(".pop",".activity")) %>% 
#   mutate(rr = proportion.activity/proportion.pop) %>%
#   arrange(desc(rr)) %>%
#   view()

# Missing ones here are all scotland and N irelang
# lad20map %>% anti_join(tmp, by="areaName") %>% view()

# lad_active_users = tmp %>% 
#   cross_join(active_users) %>% 
#   transmute(
#     areaName,
#     time,
#     population = ceiling(proportion*users)
#   ) %>% group_by(areaName)

lad_active_users = lad20cov_18_65_pop %>%
  ungroup() %>% filter(stringr::str_starts(areaCode, "E|W")) %>%
  mutate(proportion.18_65 = population/sum(population)) %>%
  cross_join(active_users) %>%
  transmute(
    areaName, 
    time,
    population = ceiling(proportion.18_65*users)
  ) %>% group_by(areaName)

# ggplot(lad_active_users, aes(x=as.Date(time), y = population, group = areaName))+geom_line(alpha=0.2)


## growth rates data ----

norm_cases = raw_cases %>% 
  left_join(raw_tests %>% select(areaCode, date, newVirusTestsBySpecimenDate), by=c("areaCode","date")) %>% 
  transmute(areaName,areaCode,time = as.time_period(date, start_date = "2020-01-01"), count = newCasesBySpecimenDate,  denom = newVirusTestsBySpecimenDate) %>% 
  group_by(time) %>%
  lad_merge(count = sum(count), denom = sum(denom))

gr = cached_poisson_locfit_model(d = norm_cases)
gr2 = gr %>% growthrates::normalise_incidence(lad20cov_pop)

## ping data ----

lad20_pings = raw_pings %>% transmute(
  time = as.time_period(`Week starting (Wythnos yn dechrau)`, start_date = "2020-01-01"),
  count = `Contact tracing alert (Hysbysiadau olrhain cyswllt)`,
  areaName = `Local authority (Awdurdod lleol)` %>% stringr::str_replace(" / .*$","")
) %>% group_by(time) %>% 
  lad_merge(count = sum(count)) %>% 
  ungroup() %>%
  select(-areaCode) %>% 
  group_by(areaName)

ping_gr = cached_poisson_locfit_model(lad20_pings,window = 4) %>% 
  normalise_incidence(lad_active_users) 
  
ping_gr2 = gr2 %>% 
  mutate( date = as.Date(time)) %>% 
  left_join(
    ping_gr %>% 
      mutate(date=as.Date(time)) %>% 
      # Pings are in weekly rate
      mutate(across(starts_with("incidence.per_capita.0"), ~ .x/7)) %>% 
      select(-time)
    ,
    by=c("areaName","date"),
    suffix = c("",".ping")
  ) %>%
  ungroup() %>%
  mutate(
    incidence.per_capita.kappa.ping = incidence.per_capita.se.fit.ping / incidence.per_capita.fit.ping,
  ) %>%
  group_by(areaCode,areaName)