##################
# data functions #
##################


#' Simulate waiting list changes over time.
#'
#' This function runs a simulation of a waiting list based on a given iteration
#' from a `wl_model` dataframe and a `run_id`. It filters the `wl_model`
#' for a specific row, initializes a waiting list dataframe, and then uses
#' the `wl_simulator` function to model changes in the waiting list over a
#' defined period. Finally, it returns the simulated waiting list queue size
#' along with the provided `run_id`.
#'
#' @param run_id A unique identifier for the simulation run. This will be
#'   attached to the output to help distinguish between different simulation
#'   results.
#' @param i An integer representing the row number in the `wl_model` dataframe
#'   to be used for the current simulation iteration. This row is expected to
#'   contain parameters like `total_waits`, `mean_ref`, and `mean_removals`.
#'
#' @return A dataframe containing the simulated waiting list queue size over time,
#'   with an additional column for the `run_id`. The structure of the returned
#'   dataframe depends on the output of `wl_queue_size()`.
#'
#' @details
#' The function relies on several external objects being available in the
#' environment:
#' \itemize{
#'   \item `wl_model`: A dataframe containing simulation parameters for
#'     different iterations. It's expected to have columns like `row_no`,
#'     `total_waits`, `mean_ref`, and `mean_removals`.
#'   \item `latest_date`: A date object representing the starting point for
#'     the initial waiting list.
#'   \item `target_date_18`: A date object representing the end date for
#'     the simulation.
#'   \item `wl_simulator`: A function that performs the core waiting list
#'     simulation. It's expected to take `start_date`, `end_date`, `demand`,
#'     `capacity`, and `waiting_list` as arguments.
#'   \item `wl_queue_size`: A function that calculates the queue size from
#'     the simulated waiting list output.
#' }
#' The initial waiting list (`current_wl1`) is created with `total_waits`
#' referrals at `latest_date` and `NA` for removal dates.
#'
#' @examples
#' # Assuming wl_model, latest_date, target_date_18, wl_simulator,
#' # and wl_queue_size are defined in the environment:
#' # sim_results <- sim_func(run_id = 1, i = 1)
#' # head(sim_results)
#'
#' @seealso
#' \code{\link{wl_simulator}}, \code{\link{wl_queue_size}}
#' @importFrom dplyr filter
#' @importFrom lubridate %m+%
#' @export
#' 
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




#' Create a performance dataframe with historical, simulated, and trajectory data.
#'
#' This function generates a comprehensive performance dataframe for a given
#' iteration from the `wl_model`. It performs a Monte Carlo simulation using
#' the `sim_func` to get stochastic predictions of queue sizes, calculates
#' various aggregates (mean, median, confidence intervals, quantiles) from
#' these simulations, and then combines them with historical actual data
#' (`df_wait`) and pre-defined trajectory predictions (`trajectory`).
#'
#' @param i An integer representing the row number in the `wl_model` dataframe
#'   to be used for the current analysis. This row is expected to contain
#'   parameters like `org_name` and `service` for filtering historical and
#'   trajectory data.
#'
#' @return A dataframe combining historical actual waiting list data,
#'   aggregated Monte Carlo simulation results (mean, median, 95% confidence
#'   intervals, and 0.5/99.5 percentiles of queue size), and predefined
#'   trajectory predictions. The output dataframe will have a `month` column
#'   (formatted to the end of the month) and various columns for actuals,
#'   simulated metrics, and trajectory.
#'
#' @details
#' The function relies on several external objects being available in the
#' environment:
#' \itemize{
#'   \item `wl_model`: A dataframe containing iteration-specific parameters,
#'     including `row_no`, `org_name`, and `service`.
#'   \item `sim_func`: A function that performs a single simulation run and
#'     is expected to return a dataframe with `dates` and `queue_size` columns,
#'     along with a `run_id`.
#'   \item `df_wait`: A dataframe containing historical actual waiting list
#'     data, expected to have columns like `org_name`, `service`, `metric_id`,
#'     and `month`.
#'   \item `trajectory`: A dataframe containing predefined trajectory
#'     predictions, expected to have columns like `org_name`, `service`,
#'     and `month`.
#' }
#' The Monte Carlo simulation runs `sim_func` 50 times. The `mc_agg` dataframe
#' calculates the mean, median, 95% confidence intervals, and 0.5th and 99.5th
#' percentiles of the simulated `queue_size` for each `dates` entry.
#' Dates in `mc_agg` and `combined` are reformatted to the end of the month.
#'
#' @examples
#' # Assuming wl_model, sim_func, df_wait, and trajectory are defined:
#' # performance_data <- create_peformance_dataframe(i = 1)
#' # head(performance_data)
#'
#' @seealso
#' \code{\link{sim_func}}
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom purrr map
#' @importFrom lubridate day ceiling_date days
#' @export
#' 
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
