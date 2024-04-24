lad_merge = function(df,...) {
  if (!"areaCode" %in% colnames(df)) df = df %>% mutate(areaCode = NA)
  if (!"areaName" %in% colnames(df)) df = df %>% mutate(areaName = NA)
  grps = df %>% groups()
  
  bucks = c("E07000004", "E07000005", "E07000006", "E07000007")
  
  corn = c("E06000053", "E06000052")
  corn2 = c("Isles of Scilly","Cornwall")
  
  hack = c("E09000001", "E09000012")
  hack2 = c("City of London","Hackney")
  
  tmp = df %>% mutate(
    newAreaName = case_when(
      areaName %in% corn2 ~ "Cornwall and Isles of Scilly",
      areaCode %in% corn ~ "Cornwall and Isles of Scilly",
      areaName %in% hack2 ~ "Hackney and City of London",
      areaCode %in% hack ~ "Hackney and City of London",
      areaCode %in% bucks ~ "Buckinghamshire",
      TRUE ~ areaName,
    ),
    newAreaCode = case_when(
      areaName %in% corn2 ~ "E06000052",
      areaCode %in% corn ~ "E06000052",
      areaName %in% hack2 ~ "E09000012",
      areaCode %in% hack ~ "E09000012",
      areaCode %in% bucks ~ "E06000060",
      TRUE ~ areaCode
    )
  ) %>% 
    ungroup() %>%
    select(-areaCode, -areaName) %>%
    rename(areaCode = newAreaCode, areaName = newAreaName) %>%
    group_by(!!!grps, areaCode, areaName) %>%
    summarise(...) %>%
    group_by(areaCode, areaName)
  
}
