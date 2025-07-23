# Data load and wrangle


#############
# data load #
#############
# 
# serv <- "udalsyndataprod.sql.azuresynapse.net"
# db <- "UDAL_Warehouse"
# 
# con_udal <- DBI::dbConnect(
#   drv = odbc::odbc(),
#   driver = "ODBC Driver 17 for SQL Server",
#   server = serv,
#   database = "UDAL_Warehouse",
#   authentication = "ActiveDirectoryInteractive"
# )
# 
# dat <- DBI::dbGetQuery(conn = con_udal, statement = paste0("
# 
# SELECT  [Period]
#       ,[Month]
#       ,[OrgRef]
#       ,[OrgName]
#       ,c.[Region_Name]
#       ,[STP_Code]
#       ,[STP_Name]
#       ,[Service]
#       ,[ServiceType]
#       ,[MetricID]
#       ,[MetricDescription] as Question
#       ,[TextResponses]
#       ,[MetricValue],
# 	  h.*
#   FROM [Reporting_SEFT_Sitreps_Published].[CommunityHealthServicesSitRep]  as c
# 
#   left join (select distinct icb_code, integrated_care_board_name, region_name from [Reporting_UKHD_ODS].[Provider_Hierarchies_ICB] where effective_to is null ) as h
# on c.stp_code = h.icb_code
# 
#   where month > DATEADD(mm,-40,GETDATE()) and
#   h.region_name = '", region, "'"))
# 


#saveRDS(dat, "dat.rds")
dat <- readRDS('dat.rds')


# clean names
df <- clean_names(dat) |>
  mutate(metric_value = as.numeric(metric_value))

# shorten org_names
df <- df |>
  mutate(
    org_name = str_remove(org_name, "NHS"),
    org_name = str_remove(org_name, "FOUNDATION"),
    org_name = str_remove(org_name, "TRUST"),
    org_name = str_squish(org_name)
  )

# calc latest date
latest_date <- max(df$month, na.rm = T)

# filter to waiting times
df_wait <- df |>
  filter(
    str_starts(metric_id, "RES001"),
    month >= latest_date %m-% months(6)
  ) |>
  arrange(
    org_name,
    month
  )

# filter to average waiting times
df_average <- df |>
  filter(
    metric_id == "RES002cii",
    month == latest_date
  ) |>
  select(org_name,
         service,
         mean_waits = metric_value
  )

# calculate current proportion waiting over 18 & 52 weeks
df_prop <- df |>
  filter(
    str_starts(metric_id, "RES001"),
    metric_id != "RES001c",
    month >= latest_date
  ) |>
  arrange(month) |>
  mutate(waits_52 = if_else(question %in% c(
    "Waiting 0-1 weeks",
    "Waiting >1-2 weeks",
    "Waiting >2-4 weeks",
    "Waiting >4-12 weeks",
    "Waiting >12-18 weeks",
    "Waiting >18-52 weeks"
  ),
  "Less than 52",
  "More than 52"
  ),
  waits_18 = if_else(question %in% c(
    "Waiting 0-1 weeks",
    "Waiting >1-2 weeks",
    "Waiting >2-4 weeks",
    "Waiting >4-12 weeks",
    "Waiting >12-18 weeks"
  ),
  "Less than 18",
  "More than 18"
  )) 


df_prop_52 <- df_prop |>
  summarise(
    metric_value = sum(metric_value),
    .by = c(
      month,
      waits_52,
      org_name,
      service
    )
  ) |>
  mutate(
    total = sum(metric_value),
    perc = metric_value / total * 100,
    .by = c(
      month,
      org_name,
      service
    )
  ) |>
  mutate(perc = if_else(is.nan(perc), 0, perc))

df_prop_18 <- df_prop |>
  summarise(
    metric_value = sum(metric_value),
    .by = c(
      month,
      waits_18,
      org_name,
      service
    )
  ) |>
  mutate(
    total = sum(metric_value),
    perc = metric_value / total * 100,
    .by = c(
      month,
      org_name,
      service
    )
  ) |>
  mutate(perc = if_else(is.nan(perc), 0, perc))


# filter our dataframe to just those over 52 weeks
over_52 <- df_prop_52 |>
  filter(
    waits_52 == "More than 52",
    metric_value > 0
  ) |>
  arrange(
    org_name,
    -perc
  )

# filter our dataframe to just those over 52 weeks
over_18 <- df_prop_18 |>
  filter(
    waits_18 == "Less than 18",
    perc < 92,
    metric_value > 0
  ) |>
  arrange(
    org_name,
    -perc
  )

saveRDS(over_52, 'over_52.rds')
saveRDS(over_18, 'over_18.rds')



# create weekly referral distributions
df_refs <- df_wait |>
  filter(metric_id == "RES001ci") |>
  mutate(referrals = metric_value * 1.1) |>
  summarise(
    mean_ref = mean(referrals, na.rm = T),
    sd_ref = sd(referrals, na.rm = T),
    .by = c(
      org_name,
      service
    )
  )

# calculate percentage of 52+ waits
df_waits_52 <- df_prop_52 |>
  filter(waits_52 == 'More than 52') |>
  select(
    org_name,
    service,
    num_52 = metric_value,
    perc_52 = perc
  ) |>
  mutate(perc_52 = round(perc_52, 2))

# calculate percentage of <18 waits
df_waits_18 <- df_prop_18 |>
  filter(waits_18 == 'Less than 18') |>
  select(
    org_name,
    service,
    num_18 = metric_value,
    perc_18 = perc
  ) |>
  mutate(perc_18 = round(perc_18, 2))
# 
# # calculate percentage of 52+ waits
# df_waits_52 <- df_wait |>
#   filter(
#     metric_id %in% c(
#       "RES001c",
#       "RES001cvii",
#       "RES001cviii"
#     ),
#     month >= latest_date
#   ) |>
#   select(
#     org_name,
#     service,
#     question,
#     metric_value
#   ) |>
#   pivot_wider(
#     names_from = question,
#     values_from = metric_value
#   ) |>
#   mutate(
#     perc_52 = (`Waiting >52-104 weeks` + `Waiting >104 weeks`) / `Total waiting list`,
#     perc_52 = if_else(is.nan(perc_52), 0, round(perc_52, 2)),
#     num_52 = (`Waiting >52-104 weeks` + `Waiting >104 weeks`)
#   ) |>
#   select(
#     org_name,
#     service,
#     perc_52,
#     num_52
#   )

# calculate removals
# monthly referrals into separate column
# and calculate removals
# assumption that
# previous waits + referrals - current waits = number removed

df_rems <- df_wait |>
  filter(metric_id %in% c(
    "RES001ci",
    "RES001c"
  )) |>
  select(
    org_name,
    month,
    service,
    question,
    metric_value
  ) |>
  pivot_wider(
    names_from = question,
    values_from = metric_value
  ) |>
  mutate(referrals = `Waiting 0-1 weeks` * 4.3) |>
  arrange(
    org_name,
    service,
    month
  ) |>
  mutate(
    prev_wait_ref = lag(`Total waiting list`, 1) + referrals,
    removals = prev_wait_ref - `Total waiting list`,
    net_change = referrals - removals,
    wk_rem = removals / 4.3,
    .by = c(
      org_name,
      service
    )
  )

# summary removals
df_removals <- df_rems |>
  summarise(
    mean_removals = mean(wk_rem, na.rm = T),
    sd_removals = sd(wk_rem, na.rm = T),
    .by = c(
      org_name,
      service
    )
  )

# dataset with current wait list size and summary demand and capacity metrics
df_summary <- df_wait |>
  filter(
    metric_id == "RES001c",
    month == latest_date
  ) |>
  select(org_name,
         service,
         integrated_care_board_name,
         total_waits = metric_value
  ) |>
  left_join(df_removals,
            by = join_by(
              org_name,
              service
            )
  ) |>
  left_join(df_refs,
            by = join_by(
              org_name,
              service
            )
  ) |>
  left_join(df_average,
            by = join_by(
              org_name,
              service
            )
  ) |>
  left_join(df_waits_52,
            by = join_by(
              org_name,
              service
            )
  ) |>
  left_join(df_waits_18,
            by = join_by(
              org_name,
              service
            )
  ) |>
  mutate(dq_tag = case_when(mean_removals < 0 ~ 1,
                            is.na(sd_removals) ~ 1,
                            mean_ref < 0 ~ 1,
                            is.na(sd_ref) ~ 1,
                            .default = 0
  ))



############
# WL Model #
############

wks_to_target <- as.numeric(round(difftime(strptime(target_date, format = "%Y-%m-%d"),
                                           strptime(latest_date, format = "%Y-%m-%d"),
                                           units = "weeks"
), 0))

wks_to_target_18 <- as.numeric(round(difftime(strptime(target_date_18, format = "%Y-%m-%d"),
                                              strptime(latest_date, format = "%Y-%m-%d"),
                                              units = "weeks"
), 0))


df_model <- df_summary |>
  mutate(
    wl_load = calc_queue_load(mean_ref, mean_removals),
    target_mean_wait = calc_target_mean_wait(waiting_time_target, achievement_value),
    target_mean_wait_18 = calc_target_mean_wait(waiting_time_target_18, achievement_value_18),
    target_queue_size = calc_target_queue_size(mean_ref, waiting_time_target),
    target_queue_size_18 = calc_target_queue_size(mean_ref, waiting_time_target_18),
    queue_ratio = total_waits / target_queue_size,
    queue_ratio_18 = total_waits / target_queue_size_18,
    additional_capacity = calc_relief_capacity(
      demand = mean_ref,
      queue_size = total_waits,
      target_queue_size = target_queue_size,
      time_to_target = wks_to_target
    ),
    additional_capacity_18 = calc_relief_capacity(
      demand = mean_ref,
      queue_size = total_waits,
      target_queue_size = target_queue_size_18,
      time_to_target = wks_to_target_18
    ),
    wl_pressure = calc_waiting_list_pressure(
      mean_waits,
      waiting_time_target
    ),
    wl_pressure_18 = calc_waiting_list_pressure(
      mean_waits,
      waiting_time_target_18
    ),
    coef_ref = sum((1/sd_ref) / (1/mean_ref)),
    coef_rem = sum((1/sd_removals) / (1/mean_removals)),
    tar_capacity = calc_target_capacity(
      demand = mean_ref,
      target_wait = waiting_time_target,
      factor = 5,
      cv_demand = coef_ref,
      cv_capacity = coef_rem
    ) * 1.1,
    tar_capacity_18 = calc_target_capacity(
      demand = mean_ref,
      target_wait = waiting_time_target_18,
      factor = 5,
      cv_demand = coef_ref,
      cv_capacity = coef_rem
    ) * 1.1,
    
    
    .by = c(org_name, service)
  )

saveRDS(df_model, 'df_model.rds')



# 12 month forecast dates
dts <- data.frame(date = seq.Date(from = latest_date %m+% months(1),
                                  to = target_date_18,
                                  by = "month"))

# create dataframe with blank dates to insert predictions
trajectory <- crossing(df_model, dts)

# create a linear trajectory to target
trajectory <- trajectory |>
  mutate(traj = case_when(date == min(date) ~ total_waits,
                          date == floor_date(target_date, 'month') ~ target_queue_size,
                          date > target_date ~ target_queue_size,
                          .default = NA),
         traj_18 = case_when(date == min(date) ~ total_waits,
                             date == floor_date(target_date_18, 'month') ~ target_queue_size_18,
                             .default = NA
         )) |>
  mutate(traj = na.interp(traj),
         traj_18 = na.interp(traj_18))

dq_issues <- df_model |>
  filter(
    dq_tag == 1,
    total_waits >= 50
  ) |>
  select(
    org_name,
    service,
    total_waits,
    num_52,
    perc_52,
    num_18,
    perc_18
  )

dq_tab <-dq_issues |>
  gt() |>
  fmt_number(
    columns = c(total_waits, num_52, num_18),
    use_seps = TRUE,
    decimals = 0
  ) |>
  fmt_percent(
    columns = c(perc_52, perc_18),
    decimals = 1,
    scale_values = FALSE
  ) |>
  cols_label(
    org_name = "Provider name",
    service = "Service",
    total_waits = "Total Waits",
    num_52 = "Patients waiting over 52 wks",
    perc_52 = "Percentage waiting over 52 wks",
    num_18 = "Patients waiting under 18 wks",
    perc_18 = "Percentage waiting under 18 wks"
  ) |>
  opt_row_striping(row_striping = FALSE) |>
  opt_table_font(font = 'Gill Sans MT', size = px(12)) 

saveRDS(dq_tab, 'dq_tab.rds')

# filter to those with > threshold number of 52 week waits
# also drop those waiting lists where there is a dq tag.
wl_model <- df_model |>
  filter(
    num_52 >= threshold,
    dq_tag == 0
  ) |>
  arrange(-num_52) |>
  mutate(
    chk = if_else(total_waits < target_queue_size, 1, 0)
  )



wl_model_18 <- df_model |>
  filter(
    num_18 >= threshold,
    perc_18 < 92,
    dq_tag == 0
  ) |>
  arrange(-num_18) |>
  mutate(
    chk = if_else(total_waits < target_queue_size, 1, 0)
  )

wl_model <- wl_model |>
  bind_rows(wl_model_18) |>
  distinct() |>
  mutate(row_no = row_number())




# create empty dataframe to collect results
combined_data <- data.frame()

# run model for all   
cli_progress_bar("processing data", total = max(wl_model$row_no))

for (i in (1:max(wl_model$row_no))) {
  combined_data <- combined_data |>
    bind_rows(create_peformance_dataframe(i))
  cli_progress_update()

}

combined_data <-  combined_data |>
  mutate(include_52 = if_else(num_52 > 0, 1, 0),
         include_18 = if_else(perc_18 < 92, 1, 0)) |>
  fill(include_52, .direction = 'up') |>
  fill(include_18, .direction = 'up') |>
  fill(org_name, .direction = 'down') |>
  fill(service, .direction = 'down')

saveRDS(combined_data, "combined_data.rds")
#combined_data <- readRDS('combined_data.rds')









