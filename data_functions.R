# data functions


# function to model simulation
sim_func <- function(run_id, i) {
  
  iteration <- wl_model |>
    filter(row_no == i)
  
  # create a wl dataframe of current size
  current_wl1 <-
    data.frame(
      Referral = rep(latest_date, iteration$total_waits[1]),
      Removal = rep(as.Date(NA), iteration$total_waits[1])
    )
  
  sd <- latest_date %m+% months(1)
  ed <- target_date_18
  dem <- iteration$mean_ref[1]
  cap <- iteration$mean_removals[1]
  wl <- current_wl1
  
  sim <- wl_simulator(
    start_date = sd,
    end_date = ed,
    demand = dem,
    capacity = cap,
    waiting_list = wl
  )
  
  cbind(wl_queue_size(sim), run_id)
}



create_peformance_dataframe <- function(i) {
  # filter data to an iteration
  iteration <- wl_model |>
    filter(row_no == i)
  
  # create a sequence of 50 iterations for monte carlo analysis
  run_sequence <- 1:50
  
  # run sim function n times to get monte carlo results
  stoc_preds <- lapply(run_sequence, sim_func, i)
  
  # pull the predictions
  mc_bind <- do.call("rbind", stoc_preds)
  
  # calculate means and quantiles
  mc_agg <-
    aggregate(
      queue_size ~ dates,
      data = mc_bind,
      FUN = \(x) {
        c(
          mean_q = mean(x),
          median_q = median(x),
          lower_95CI = mean(x) - (1.96 * (sd(x) / sqrt(length(x)))),
          upper_95CI = mean(x) + (1.96 * (sd(x) / sqrt(length(x)))),
          q_25 = quantile(x, .005, names = FALSE),
          q_75 = quantile(x, .995, names = FALSE)
        )
      }
    )
  
  # put aggreagtes into dataframe
  mc_agg <- data.frame(dates = as.Date(mc_agg$dates), unlist(mc_agg$queue_size))
  
  # reformat dates to end of month
  mc_agg <- mc_agg |>
    filter(day(dates) == day(ceiling_date(dates, "month") - days(1)))
  
  # pull historical actual total waits
  cur <- df_wait |>
    filter(
      org_name == iteration$org_name[1],
      service == iteration$service[1],
      metric_id == "RES001c"
    )
  
  # pull trajectory from trajectory dataframe
  traj_pred <- trajectory |>
    filter(
      org_name == iteration$org_name[1],
      service == iteration$service[1]
    )
  
  combined <- bind_rows(
    cur,
    mc_agg,
    traj_pred
  ) |>
    mutate(month = ceiling_date(month, "month") - days(1))
  
  combined
}
