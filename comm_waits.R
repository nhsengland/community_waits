library(tidyverse)
library(janitor)
library(NHSRwaitinglist)
library(gt)
library(gtExtras)
library(geomtextpath)
library(forecast)

# name of region to run report for
region <- 'SOUTH WEST'

# threshold of number of patients waiting over 52 weeks to run models for
threshold <- 50

# Waiting time target (weeks)
waiting_time_target <- 52

# target to achieve
# NOTE: It is not possible to calculate 100% achievement
target <- 0.99

achievement_value <- qexp(target)  

target_date <- as.Date ('2026-03-31')

#############
# data load #
#############

serv <- "udalsyndataprod.sql.azuresynapse.net"
db <- "UDAL_Warehouse"

con_udal <- DBI::dbConnect(drv = odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = serv ,
                           database = "UDAL_Warehouse",
                           authentication = "ActiveDirectoryInteractive")

dat <- DBI::dbGetQuery(conn = con_udal, statement = paste0("

SELECT  [Period]
      ,[Month]
      ,[OrgRef]
      ,[OrgName]
      ,c.[Region_Name]
      ,[STP_Code]
      ,[STP_Name]
      ,[Service]
      ,[ServiceType]
      ,[MetricID] 
      ,[MetricDescription] as Question
      ,[TextResponses]
      ,[MetricValue],
	  h.*
  FROM [Reporting_SEFT_Sitreps_Published].[CommunityHealthServicesSitRep]  as c

  left join (select distinct icb_code, integrated_care_board_name, region_name from [Reporting_UKHD_ODS].[Provider_Hierarchies_ICB] where effective_to is null ) as h
on c.stp_code = h.icb_code
  
  where month > DATEADD(mm,-40,GETDATE()) and
  h.region_name = '", region  ,"'"                  
))

# clean names
df <- clean_names(dat) |>
  mutate(metric_value = as.numeric(metric_value))

# shorten org_names
df <- df |>
  mutate(org_name = str_remove(org_name, 'NHS'),
         org_name = str_remove(org_name, 'FOUNDATION'),
         org_name = str_remove(org_name, 'TRUST'),
         org_name = str_squish(org_name))

# calc latest date
latest_date <- max(df$month, na.rm = T)

# filter to waiting times
df_wait <- df |>
  filter(str_starts(metric_id, 'RES001'),
         month >= latest_date %m-% months(6)) |>
  arrange(org_name,
          month)

# filter to average waiting times
df_average <- df |>
  filter(metric_id == 'RES002cii',
         month == latest_date) |>
  select(org_name,
         service,
         mean_waits = metric_value)

# calculate current proportion waiting over 52 weeks
df_prop <- df |>
  filter(str_starts(metric_id, 'RES001'),
         metric_id != 'RES001c',
         month >= latest_date) |>
  arrange(month) |>
  mutate(waits_52 = if_else(question %in% c('Waiting 0-1 weeks',
                                            'Waiting >1-2 weeks',
                                            'Waiting >2-4 weeks',
                                            'Waiting >4-12 weeks',
                                            'Waiting >12-18 weeks',
                                            'Waiting >18-52 weeks'),
                            'Less than 52',
                            'More than 52')) |> 
  summarise(metric_value = sum(metric_value),
            .by = c(month, 
                    waits_52,
                    org_name,
                    service)) |>
  mutate(total = sum(metric_value),
         perc = metric_value / total * 100,
         .by = c(month,
                 org_name,
                 service)) |>
  mutate(perc = if_else(is.nan(perc), 0, perc))

# filter our dataframe to just those over 52 weeks
over_52 <- df_prop |>
  filter(waits_52 == 'More than 52',
         metric_value > 0) |>
  arrange(org_name, 
          -perc)

# make a pretty table of our over 52 week waits
over_52_tab <- over_52 |>
  select(-month,
         -waits_52) |>
  group_by(org_name) |>
  gt() |> 
  gt_duplicate_column(perc,
                      append_text = '_bar') |>
  fmt_number(columns = c(metric_value, total), 
             use_seps = TRUE, 
             decimals = 0) |>
  fmt_percent(columns = perc, 
              decimals = 1, 
              scale_values = FALSE) |>
  gt_plt_bar_pct(
    column = perc_bar, scaled = TRUE,
    fill = "#005EB8", background = "lightblue"
  ) |>
  tab_header(
    title = paste0('Community 52 week waits as at ', format(latest_date, '%B %y')),
    subtitle = md('Waitings lists by provider and service with patients waiting 52 weeks+')
  ) |> 
  tab_footnote(
    footnote = paste0("Data taken from Community Health Services (CHS) SitRep downloaded as at ", 
                      format(Sys.Date(), '%Y-%m-%d')
  )) |>
  cols_label(
    service = "Service",
    metric_value = "Patients waiting over 52 wks",
    total = "Total patients waiting",
    perc = "Percentage over 52 wks",
    perc_bar = " "
  ) 
  
over_52_tab


# create weekly referral distributions
df_refs <- df_wait |>
  filter(metric_id == 'RES001ci')  |>
  mutate(referrals = metric_value) |>
  summarise(mean_ref = mean(referrals, na.rm = T),
            sd_ref = sd(referrals, na.rm = T),
            .by = c(org_name,
                    service))

# calculate percentage of 52+ waits
df_waits_52 <- df_wait |>
  filter(metric_id %in% c('RES001c',
                          'RES001cvii',
                          'RES001cviii'),
         month >= latest_date) |>
  select(org_name,
         service,
         question,
         metric_value) |>
  pivot_wider(names_from = question,
              values_from = metric_value) |>
  mutate(perc_52 = (`Waiting >52-104 weeks` + `Waiting >104 weeks`) / `Total waiting list`,
         perc_52 = if_else(is.nan(perc_52), 1 , round(perc_52, 2)),
         perc_52 = if_else(perc_52 == 0, 1 , round(perc_52, 2)),
         num_52 = (`Waiting >52-104 weeks` + `Waiting >104 weeks`)) |>
  select(org_name,
         service,
         perc_52,
         num_52)

# calculate removals
# monthly referrals into separate column
# and calculate removals
# assumption that 
# previous waits + referrals - current waits = number removed

df_rems <- df_wait |>
  filter(metric_id %in% c('RES001ci',
                          'RES001c')) |>
  select(org_name,
         month,
         service,
         question,
         metric_value) |>
  pivot_wider(names_from = question,
              values_from = metric_value) |>
  mutate(referrals = `Waiting 0-1 weeks` * 4.3) |>
  arrange(org_name,
          service,
          month) |>
  mutate(prev_wait_ref = lag(`Total waiting list`, 1) + referrals,
         removals = prev_wait_ref - `Total waiting list`,
         net_change = referrals - removals,
         wk_rem = removals / 4.3,
         .by = c(org_name,
                 service)) 

# summary removals
df_removals <- df_rems |>
  summarise(mean_removals = mean(wk_rem, na.rm = T),
            sd_removals = sd(wk_rem, na.rm =T),
            .by = c(org_name,
                    service))

# dataset with current wait list size and summary demand and capacity metrics
df_summary <- df_wait |>
  filter(metric_id == 'RES001c',
         month == latest_date) |>
  select(org_name,
         service,
         integrated_care_board_name,
         total_waits = metric_value) |>
  left_join(df_removals,
            by = join_by(org_name, 
                         service)) |>
  left_join(df_refs,
            by = join_by(org_name, 
                         service)) |>
  left_join(df_average,
            by = join_by(org_name, 
                         service)) |>
  left_join(df_waits_52,
            by = join_by(org_name, 
                         service)) |> 
  mutate(dq_tag = case_when(mean_removals <0 ~ 1,
                            is.na(sd_removals) ~ 1,
                            mean_ref <0 ~ 1,
                            is.na(sd_ref) ~ 1,
                            .default = 0))

############  
# WL Model #
############

wks_to_target <- as.numeric(round(difftime(strptime(target_date, format = "%Y-%m-%d"),
         strptime(latest_date, format = "%Y-%m-%d"),units="weeks"),0))


df_model <- df_summary |>
  mutate(wl_load = calc_queue_load(mean_ref, mean_removals),
         target_mean_wait = calc_target_mean_wait(waiting_time_target, achievement_value),
         target_queue_size = calc_target_queue_size(mean_ref, waiting_time_target),
         queue_ratio = total_waits / target_queue_size,
         additional_capacity = calc_relief_capacity(
           demand = mean_ref,
           queue_size = total_waits,
           target_queue_size = target_queue_size,
           time_to_target = wks_to_target
         ),
         wl_pressure =   calc_waiting_list_pressure(
           mean_waits,
           waiting_time_target
         ),
         tar_capacity =  calc_target_capacity(demand = mean_ref,
                                              target_wait = waiting_time_target,
                                              factor = 5
         ),
         .by = c(org_name, service))


# df model in a table
# One table per provider
org <- 'CORNWALL PARTNERSHIP'

df_model |>
  filter(org_name == org) |>
  mutate(dq_tag = if_else(mean_removals <0, 1 , dq_tag)) |>
  arrange(-total_waits) |>
  select (service,
          dq_tag,           #dq
          mean_ref,         #d&c
          mean_removals,    #d&c
          wl_load,          #d&c
          mean_waits,       # waits time
          target_mean_wait, #waits time
          total_waits,      # total waits
          target_queue_size, # total waits
          queue_ratio,      # total waits
          perc_52,          # performance
          wl_pressure,      # performance
          additional_capacity,   # capacity
          tar_capacity          # capacity
  ) |>
  gt() |>
  gt_duplicate_column(mean_removals,
                      append_text = '_dup',
                      after = perc_52) |>
  fmt_number(rows = everything(),
             decimals = 1,
             use_seps = TRUE,
             sep_mark = ",") |>
  tab_spanner(label = 'Demand and Capacity',
              columns = c(
                mean_ref,
                mean_removals,
                wl_load
                          )) |>
  tab_spanner(label = 'Waiting Times',
              columns = c(
                mean_waits,
                target_mean_wait,
                total_waits,
                target_queue_size,
                queue_ratio
              )) |>
  tab_spanner(label = 'Performance',
              columns = c(
                perc_52,
                wl_pressure
              )) |>
  tab_spanner(label = 'Capacity',
              columns = c(
                mean_removals_dup,
                additional_capacity,
                tar_capacity
              )) |>
  cols_label(
    service = 'Service',
    dq_tag = 'dq',           #dq
    mean_ref = 'Mean referrals',    #d&c
    mean_removals = 'Mean capacity', #d&c
    wl_load = '(i) WL Load',          #d&c
    mean_waits = 'Actual mean waits',       # waits time
    target_mean_wait = '(ii) Target mean waits', #waits time
    total_waits = 'Current total waits',      # total waits
    target_queue_size = '(iii) Target total waits', # total waits
    queue_ratio = '(iv) Queue ratio',      # total waits
    perc_52 = '52 Week performance',          # performance
    mean_removals_dup = 'Current capacity',
    wl_pressure = '(v) Pressure',      # performance
    additional_capacity = '(vi) Relief capacity',   # capacity
    tar_capacity = '(vii) Stable capacity'          # capacity
) |>
  tab_style(
    style = list(
      cell_fill(color = "#E8EDEE")
    ),
    locations = cells_body(
      rows = dq_tag == 1)
  ) |>
  cols_hide(columns = dq_tag) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(5,7, 9, 10,12, 13,14)
    )
  ) |>
  gt_plt_bar_pct(column = perc_52, 
                 fill = 'lightblue',
                 scaled = FALSE, 
                 labels = T) |>
  tab_header(
    title = paste0(org,': Community Services waits as at ', format(latest_date, '%B %y')),
    subtitle = md('Waiting lists by service with waiting list metrics')
  ) |> 
  tab_footnote(
    footnote = md(paste0("**NOTE:** *Rows in grey indicate a data quality issue or inconsistent data*<br>Data taken from Community Health Services (CHS) SitRep downloaded as at ", 
                      format(Sys.Date(), '%Y-%m-%d'))
    ))  |> 
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = 'lightgrey',
      weight = px(1)),
    locations = cells_body(
      columns = c(1, 5, 10, 12)
    )
  )



# check one site
# one <- df |>
#   filter(org_name == 'SIRONA CARE & HEALTH',
#          service == '(CYP) Therapy interventions: Speech and language',
#          metric_id == 'RES001c') 

# 12 month forecast dates
dts <- data.frame(date = seq.Date(latest_date %m+% months(1), by = 'month', length.out = 12))

# create dataframe with blank dates to insert predictions
trajectory <- crossing(df_model, dts)

# create a linear trajectory to target
trajectory <- trajectory |>
  mutate(traj = case_when(date == min(date) ~ total_waits,
                          date == max(date) ~ target_queue_size,
                          .default = NA)) |>
  mutate(traj = na.interp(traj))

# 
# 
# predictions <- predictions |>
#   mutate(change = mean_ref - mean_removals,
#          pred_wait = total_waits + mean_ref - mean_removals,
#          row = row_number(),
#          mean_culm_waits = total_waits + (mean_ref*row) - (mean_removals*row),
#          lo_ref_hi_cap = total_waits + ((mean_ref-sd_ref)*row) - ((mean_removals+sd_removals)*row),
#          hi_ref_lo_cap = total_waits + ((mean_ref+sd_ref)*row) - ((mean_removals-sd_removals)*row),
#          .by = c(org_name,
#                  service))

# 
# 
# cur <- df_wait |>
#   filter(org_name == 'CORNWALL PARTNERSHIP NHS FOUNDATION TRUST',
#          service == '(CYP) Community paediatric service',
#          metric_id == 'RES001c') 
# 
# 
# pred <- predictions |>
#   filter(org_name == 'CORNWALL PARTNERSHIP NHS FOUNDATION TRUST',
#          service == '(CYP) Community paediatric service')
#  
# com <- bind_rows(cur, pred, trajectory)
# 
# com |> 
#  ggplot() +
#   geom_line( aes(x=month, y= metric_value)) +
#   geom_line(aes(x=date, y= mean_culm_waits), linetype = "dashed")  +
#   geom_ribbon(aes(x=date, ymin=lo_ref_hi_cap, ymax=hi_ref_lo_cap, fill= 'lightblue'), alpha = 0.1) +
#   geom_ribbon(aes(x=date, ymin=q_25, ymax=q_75, fill= 'red'), alpha = 0.1) +
#   theme_minimal()
# 
# 
# 
# 
# predictions2 <- predictions |>
#   mutate(pred_wait = lag(pred_wait, 1))
# 
# 
# 
# 
# one |> 
#   ggplot() +
#   aes(x = month,
#       y = metric_value) +
#   geom_line() 
# 


###############

#que_target <- achievement_value


dq_issues <- df_model |>
  filter(dq_tag == 1,
         total_waits >= 50) |>
  select(org_name,
         service,
         total_waits,
         num_52,
         perc_52)

dq_issues |>
  gt() |>  
  fmt_number(columns = c(total_waits, num_52), 
                      use_seps = TRUE, 
                      decimals = 0) |>
  fmt_percent(columns = perc_52, 
              decimals = 1, 
              scale_values = TRUE) |>
  cols_label(
    org_name = 'Provider name',
    service = 'Service',
    total_waits = 'Total Waits',
    num_52 = 'Patients waiting over 52 wks',
    perc_52 = 'Percentage waiting over 52 wks')
  
  




#target_mean_wait <- calc_target_mean_wait(waiting_time_target, que_target)

# filter to those with > threshold number of 52 week waits
# also drop those waiting lists where there is a dq tag.
wl_model <- df_model |>
  filter(num_52 >= threshold,
         dq_tag == 0) |>
  arrange(-num_52) |>
  mutate(row_no = row_number())

##

# pred_wl <- wl_simulator(start_date = latest_date %m+% months(1),
#                             end_date = target_date,
#                             demand = iteration$mean_ref[1]*1.1,
#                             capacity = iteration$mean_removals[1],
#                             waiting_list = current_wl1)
#   
# pred_wl <- wl_queue_size(pred_wl)

# function to model simulation
sim_func <- function(run_id) {
  sim <- wl_simulator(start_date = latest_date %m+% months(1),
                      end_date = target_date,
                      demand = iteration$mean_ref[1]*1.1,
                      capacity = iteration$mean_removals[1],
                      waiting_list = current_wl1)
  
  cbind(wl_queue_size(sim), run_id)
}


i <- 1

create_peformance_dataframe <- function(i)  {

# filter data to an iteration
iteration <- wl_model |>
  filter(row_no == i)

# create a wl dataframe of current size
current_wl1 <-
  data.frame(
    Referral = rep(latest_date, iteration$total_waits[1]), 
    Removal = rep(as.Date(NA), iteration$total_waits[1])
  )

# create a sequence of 50 iterations for monte carlo analysis
run_sequence <- 1:50

# run sim function n times to get monte carlo results
stoc_preds <- lapply(run_sequence, sim_func)

# pull the predictions
mc_bind <- do.call("rbind", stoc_preds)

# calculate means and quantiles
mc_agg <-
  aggregate(
    queue_size ~ dates
    , data = mc_bind
    , FUN = \(x) {
      c(mean_q = mean(x),
        median_q = median(x),
        lower_95CI = mean(x) -  (1.96 * (sd(x) / sqrt(length(x)))),
        upper_95CI = mean(x) +  (1.96 * (sd(x) / sqrt(length(x)))),
        q_25 = quantile(x, .025, names = FALSE),
        q_75 = quantile(x, .975, names = FALSE))
    }
  )

# put aggreagtes into dataframe
mc_agg <- data.frame(dates = as.Date(mc_agg$dates), unlist(mc_agg$queue_size))

# reformat dates to end of month
mc_agg <- mc_agg |>
  filter(day(dates) == day(ceiling_date(dates, "month") - days(1)))

# pull historical actual total waits
cur <- df_wait |>
  filter(org_name == iteration$org_name[1],
         service == iteration$service[1],
         metric_id == 'RES001c') 

# pull trajectory from trajectory dataframe 
traj_pred <- trajectory |> 
  filter(org_name == iteration$org_name[1],
         service == iteration$service[1]) 

com <- bind_rows(cur, mc_agg, traj_pred) |>
  mutate(month = ceiling_date(month, "month") - days(1))

com 
}

com <- create_peformance_dataframe(1)


# plot just predictions
com |> 
  ggplot() +
  geom_textpath(aes(x=month, y=metric_value), label = 'Actuals', hjust = 0.3, straight = TRUE) +
  geom_textpath(aes(x=dates, y=mean_q),linetype = "dashed", label = 'Predicted', hjust = 0.3, straight = TRUE ) +
  geom_ribbon(aes(x=dates, ymin=q_25, ymax=q_75), alpha = 0.5, fill= 'lightblue') +
  theme_minimal() +
  labs(title = paste0(iteration$org_name[1], '\n', iteration$service[1]),
       subtitle = paste0('Projected waiting list size to ', format(target_date, '%d %B %y'), '\nBased on calibrated metrics of demand and capacity'),
       x = NULL,
       y = 'Number of patients waiting') +
  scale_x_date(date_breaks = '2 month',
               date_labels = '%b %y') 


# funtion to plot predictions and trajectories
plot_predictions_trajectory <- function(i, traj = F) {

  com <- create_peformance_dataframe(i)
  
# plot predictions + target + trajectory 
plot <- com |> 
  ggplot() +
  geom_textpath(aes(x=month, y=metric_value), label = 'Actuals', hjust = 0.3, straight = TRUE) +
  geom_textpath(aes(x=dates, y=mean_q),linetype = "dashed", label = 'Predicted', hjust = 0.3, straight = TRUE ) +
  geom_ribbon(aes(x=dates, ymin=q_25, ymax=q_75), alpha = 0.5, fill= 'lightblue') +
  theme_minimal() +
  labs(title = paste0(iteration$org_name[1], '\n', iteration$service[1]),
       subtitle = paste0('Projected waiting list size to ', format(target_date, '%d %B %y'), '\nAlso showing trajectory required to achieve 52wk target'),
       x = NULL,
       y = 'Number of patients waiting') +
  scale_x_date(date_breaks = '2 month',
               date_labels = '%b %y') 

if(traj == T) {
  plot <- plot +
    geom_textpath(aes(x=date, y=traj),linetype = "longdash", label = 'Trajectory to achieve target', hjust = 0.3, straight = TRUE, color = "red" ) +
    geom_texthline(aes(yintercept = traj_pred$traj[traj_pred$date==max(traj_pred$date)], label='Target waiting list size', hjust = 0.1), color = "red") 
}  
 plot
}

plot_predictions_trajectory(1, F)

plot_predictions_trajectory(2, T)
  