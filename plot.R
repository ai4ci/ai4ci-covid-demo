messages = tribble(
  ~date, ~date2, ~message,
  "2020-04-01", "2020-05-01", "Testing capacity limited in first wave",
  "2020-09-24", "2020-10-24", "24th September: First digital contact tracing alerts generated",
  "2020-11-14", "2021-01-01", "Alpha variant starts spreading in Kent and grows despite fire-break lockdown",
  "2020-12-02", "2021-01-04", "Possible underestimation of alert rate during Alpha wave",
  "2021-01-25", "2021-02-25", "App updated to include lateral flow test positives",
  "2021-05-01", "2021-07-01", "Initial outbreak of Delta near Blackburn and Burnley quickly spread across the UK",
  "2021-06-01", "2021-08-01", "Contact tracing alerts triggered by high case loads",
  "2021-07-14", "2021-08-01", "Delta wave growth stopped",
  "2021-08-01", "2021-09-01", "Algortihm changed to reduce contact tracing alerts",
  "2021-09-14", "2021-11-14", "Sustained contact tracing alerts throughout September and November",
  "2021-12-01", "2021-12-14", "Omicron begins to become dominant in the South East",
  "2021-12-14", "2022-04-01", "Rapid Omicron spread and high sustained covid levels",
  "2021-12-14", "2022-03-01", "High volumes of contract tracing alerts during Omicron wave",
  "2023-04-27", "2024-04-27", "NHS App discontinued"
) %>% mutate(date = as.Date(date), date2 = as.Date(date2))


doplot = function(plot_date, messages, overwrite=FALSE, loc="~/Dropbox/ai-hub/kick-off-demo/dashboard_%s.png") {
  
  plot_date = as.Date(plot_date)
  file = sprintf(loc, format(plot_date))
  if (fs::file_exists(file) && !overwrite) return(file)
  
  caption = messages %>% 
    filter(date<=plot_date & date2 > plot_date) %>% 
    arrange(date) %>% 
    pull(message) 
  
  caption = paste0( c(caption,rep("",3))[1:3], collapse="\n")
  
  tmp = gr2 %>% mutate( date = as.Date(time)) %>% 
    filter(date==plot_date)
  
  tmp_map = lad20cov %>% left_join(tmp, by=c("areaName","areaCode"))
  
  in_trans = scales::transform_yj(0.6)
  in_breaks = function(n) {
    function(x,...)
    signif(in_trans$inverse(seq(0,in_trans$transform(max(x,na.rm = TRUE)),length.out=n)),2)
  }
  
  max_in = quantile(ping_gr2$incidence.per_capita.0.5, 0.995,na.rm = TRUE)
  
  p_incid_map = ggplot(tmp_map,aes(fill=incidence.per_capita.0.5))+geom_sf(linewidth=0.05,colour="white")+arear::mapTheme()+
    scale_fill_viridis_c(name=NULL,lim=c(0,max_in),trans=in_trans, breaks=in_breaks(6), oob=scales::squish, na.value = "black")+
    facet_wrap(~"Cases per 100K per day")+theme(legend.position = c(0.05,0.075), legend.background = element_blank())+
    ggrrr::gg_resize_legend(textSize = 10)
  
  
  p_growth_map = ggplot(tmp_map,aes(fill=growth.0.5))+geom_sf(linewidth=0.05,colour="black")+arear::mapTheme()+
    scale_fill_gradient2(name=NULL,lim=c(-0.15,0.15),oob=scales::squish,low = "grey30", high="orange" ,na.value = "white")+
    facet_wrap(~"Growth rate per day")+theme(legend.position = c(0.05,0.075), legend.background = element_blank())+
    ggrrr::gg_resize_legend(textSize = 10)
  
  #ggplot(ping_gr2,aes(x=growth.0.5, y=incidence.per_capita.0.5.ping,group=areaName))+geom_path(alpha=0.1)+coord_cartesian(xlim=c(-0.2,0.2)) 
  
  max_pings = quantile(ping_gr2$incidence.per_capita.0.5.ping, 0.995,na.rm = TRUE)
  # max_pings = signif(max(ping_gr2$incidence.per_capita.0.5.ping, na.rm = TRUE),1)
  
  ping_scale = function(type) {
    if (type == "fill") fn = scale_fill_viridis_c else fn = scale_colour_viridis_c
    fn(option = "A", na.value = "grey20", name=NULL,lim=c(0,max_pings),oob=scales::squish, trans=in_trans, breaks = in_breaks(6))
  }
  
  p_phase = growthrates::plot_growth_phase(ping_gr2 %>% select(-date), timepoints = plot_date, duration = 7,interval = 0,cis = FALSE,mapping=aes(group=areaCode, colour=incidence.per_capita.0.5.ping))+
    coord_cartesian(xlim=c(-0.2,0.2), ylim = c(0,500))+
    ping_scale("colour")+
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 10),
      legend.position = c(0.05,1-0.125),
      legend.key.size = ggplot2::unit(0.75, "lines")
    )+
    ylab("Cases per 100K per day")+
    facet_wrap(~"Alert / Case / Growth rate")
  
  
  
  tmp2 = ping_gr2 %>% mutate( date = as.Date(time)) %>% 
    filter(date==plot_date)
  
  tmp2_map = lad20cov_ew %>% filter(areaCode %>% stringr::str_starts("E|W")) %>% left_join(tmp2, by=c("areaName","areaCode"))
  
  p_ping_map = ggplot(tmp2_map,aes(fill=incidence.per_capita.0.5.ping))+geom_sf(linewidth=0.05,colour="white")+arear::mapTheme()+
    ping_scale("fill")+
    # scale_fill_gradient(low = "black",high="cyan",na.value = "grey20", name=NULL,lim=c(0,max_pings))+
    facet_wrap(~"Alerts per 100K 18-65 per day")+theme(legend.position = c(0.05,1-0.125), legend.background = element_blank())+
    ggrrr::gg_resize_legend(textSize = 10)
  
  # p_ping_map 
  # = ggplot(tmp2_map,aes(fill=proportion.0.5))+geom_sf(linewidth=0.05,colour="white")+arear::mapTheme()+
  #   scale_fill_viridis_c(option = "A", name=NULL)+ #,trans="log1p",breaks = breaks_log1p(7))+
  #   facet_wrap(~"alerts per check-in")+theme(legend.position = c(0.05,1-0.125), legend.background = element_blank())+
  #   ggrrr::gg_resize_legend(textSize = 10)
  
  p_timeline_slice = p_timeline+
    geom_vline(xintercept=plot_date,size=2,colour="orange")+
    coord_cartesian(xlim=plot_date+c(-7*42,+7*42))+
    facet_wrap(~rlang::inject(format(plot_date,"%d %b %y")))
  
  p_vaccine_slice = p_vaccine+
    geom_vline(xintercept=plot_date,size=2,colour="orange")+
    coord_cartesian(xlim=plot_date+c(-7*26,+7*26))
  
  p_dash = p_incid_map+p_growth_map+p_phase+
    p_ping_map+p_timeline_slice+p_vaccine_slice+
    patchwork::plot_annotation(
      title="COVID-19 in the UK: Digital contact tracing and the course of the pandemic",
      subtitle = "Robert Challen¹²; Leon Danon¹²;\n\n1) AI4CI, pandemic resilience \n2) Engineering Mathematics, University of Bristol",
      caption = caption,
      theme = theme(
        plot.title = element_text(size = 28,hjust=0.5,face = "bold"),
        plot.subtitle = element_text(size = 22,hjust=0),
        plot.caption = element_text(size = 20,hjust=0.5,face = "italic")
      )
    )+
    patchwork::plot_layout(design = "
AAAABBBBEEEEEEEFF
AAAABBBBCCCCCDDDD
AAAABBBBCCCCCDDDD
AAAABBBBCCCCCDDDD
AAAABBBBCCCCCDDDD
")
  
  
  ggsave(file, plot=p_dash, device=ragg::agg_png, width = 2560/96, height = 1440/96, units = "in",dpi = 96)
  return(file)
  
}

# 
# dates = seq(as.Date("2020-09-30"),as.Date("2022-06-30"),91)
# for (date in dates) {
#   doplot(date,messages,overwrite = TRUE, loc= "~/tmp/dashboard_%s.png") %>% getOption("viewer")()
# }
