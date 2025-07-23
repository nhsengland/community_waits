# report functions



# make a pretty table of our over X week waits
over_x_tab <-  function (data, org, selection) {
  
  over_x_tab <- data |>
    filter(org_name == org) |>
    select(
      -month,
      -!!sym(selection),
      -org_name
    ) 
  
  if (selection == 'waits_18') {
    over_x_tab <- over_x_tab |>
      arrange(perc)
  }
  
  over_x_tab <- over_x_tab |>
    gt() |>
    opt_row_striping(row_striping = FALSE) |>
    opt_table_font(font = 'Gill Sans MT', size = px(11)) |>
    gt_duplicate_column(perc,
                        append_text = "_bar"
    ) |>
    fmt_number(
      columns = c(metric_value, total),
      use_seps = TRUE,
      decimals = 0
    ) |>
    fmt_percent(
      columns = perc,
      decimals = 1,
      scale_values = FALSE
    ) |>
    gt_plt_bar_pct(
      column = perc_bar, scaled = TRUE,
      fill = "#005EB8", background = "lightblue"
    ) |>
    tab_footnote(
      footnote = paste0(
        "Data taken from Community Health Services (CHS) SitRep downloaded as at ",
        format(Sys.Date(), "%Y-%m-%d")
      )
    ) 
  
  if (selection == 'waits_18') {
    over_x_tab <- over_x_tab |>
      tab_header(
        title = paste0(org, ": Community 18 week waits as at ", format(latest_date, "%B %y")),
        subtitle = md("Waitings lists by provider and service with <92% patients waiting under 18 weeks")
      )|>
      cols_label(
        service = "Service",
        metric_value = "Patients waiting over 18 wks",
        total = "Total patients waiting",
        perc = "Percentage under 18 wks",
        perc_bar = "Target > 92%"
      ) 
    
  } else {
    over_x_tab <- over_x_tab  |>
      tab_header(
        title = paste0(org, ": Community 52 week waits as at ", format(latest_date, "%B %y")),
        subtitle = md("Waitings lists by provider and service with patients waiting 52 weeks+")
      )|>
      cols_label(
        service = "Service",
        metric_value = "Patients waiting over 52 wks",
        total = "Total patients waiting",
        perc = "Percentage over 52 wks",
        perc_bar = "Target 0%"
      ) }
  
  
  over_x_tab <- over_x_tab |>
    as_raw_html()
  
  over_x_tab
}

#over_x_tab(over_18, 'CORNWALL PARTNERSHIP', 'waits_18')



provider_model_table <- function(org, waits)  {
  
  
  tab_data <- if(waits == '52') 
  {df_model |>
      filter(org_name == org,
             mean_ref >0) |>
      mutate(
        dq_tag = if_else(mean_removals < 0, 1, dq_tag),
        perc_52 = perc_52,
        mean_removals_dup = mean_removals
      ) |>
      arrange(-total_waits) |>
      select(
        service,
        dq_tag, # dq
        mean_ref, # d&c
        mean_removals, # d&c
        wl_load, # d&c
        mean_waits, # waits time
        target_mean_wait, # waits time
        total_waits, # total waits
        target_queue_size, # total waits
        queue_ratio, # total waits
        perc_52, # performance
        wl_pressure, # performance
        mean_removals_dup, # capacity
        additional_capacity, # capacity
        tar_capacity  )} else {
          df_model |>
            filter(org_name == org,
                   mean_ref >0,
                   num_18 > 0 ) |>
            mutate(
              dq_tag = if_else(mean_removals < 0, 1, dq_tag),
              perc_52 = perc_52,
              mean_removals_dup = mean_removals
            ) |>
            arrange(perc_18) |>
            select(
              service,
              dq_tag, # dq
              mean_ref, # d&c
              mean_removals, # d&c
              wl_load, # d&c
              mean_waits, # waits time
              target_mean_wait_18, # waits time
              total_waits, # total waits
              target_queue_size_18, # total waits
              queue_ratio_18, # total waits
              perc_18, # performance
              wl_pressure_18, # performance
              mean_removals_dup,# capacity
              additional_capacity_18, # capacity
              tar_capacity_18) # capacity 
        }
  
  if(waits == '52') { 
    model_table <- tab_data |> 
      gt() |>
      opt_row_striping(row_striping = FALSE) |>
      opt_table_font(font = 'Gill Sans MT', size = px(11)) |>
      
      fmt_number(
        rows = everything(),
        decimals = 1,
        use_seps = TRUE,
        sep_mark = ","
      ) |>
      tab_spanner(
        label = "Demand and Capacity",
        columns = c(
          mean_ref,
          mean_removals,
          wl_load
        )
      ) |>
      tab_spanner(
        label = "Waiting Times",
        columns = c(
          mean_waits,
          target_mean_wait,
          total_waits,
          target_queue_size,
          queue_ratio
        )
      ) |>
      tab_spanner(
        label = "Performance",
        columns = c(
          perc_52,
          wl_pressure
        )
      ) |>
      tab_spanner(
        label = "Capacity Projections to Achieve Target",
        columns = c(
          mean_removals_dup,
          additional_capacity,
          tar_capacity
        )
      ) |>
      cols_label(
        service = "Service",
        dq_tag = "dq", # dq
        mean_ref = "Mean referrals", # d&c
        mean_removals = "Mean capacity", # d&c
        wl_load = "(i) WL Load", # d&c
        mean_waits = "Actual mean waits", # waits time
        target_mean_wait = "(ii) Target mean waits", # waits time
        total_waits = "Current total waits", # total waits
        target_queue_size = "(iii) Target total waits", # total waits
        queue_ratio = "(iv) Queue ratio", # total waits
        perc_52 = "%Waiting 52 Wks+", # performance
        mean_removals_dup = "Current capacity",
        wl_pressure = "(v) Pressure", # performance
        additional_capacity = "(vi) Relief capacity", # capacity
        tar_capacity = "(vii) Stable capacity" # capacity
      ) |>
      tab_style(
        style = list(
          cell_fill(color = "#41B6E6", alpha = 0.5)
        ),
        locations = cells_body(
          columns  = c(13, 14, 15)
        )
      ) |>
      tab_style(
        style = list(
          cell_fill(color = "#CCA00F", alpha = 0.5)
        ),
        locations = cells_body(
          rows = dq_tag == 1
        )
      ) |>
      cols_hide(columns = dq_tag) |>
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = c(5, 7, 9, 10, 12, 13, 14)
        )
      ) |>
      gt_plt_bar_pct(
        column = perc_52,
        fill = "#DA291C",
        scaled = TRUE,
        labels = T
      ) |>
      tab_header(
        title = paste0(org, ": Community Services waits as at ", format(latest_date, "%B %y"), " : Analysis to achieve zero at 52 weeks target by ", format(target_date, '%b %y')),
        subtitle = md("Waiting lists by service with waiting list metrics")
      ) |>
      tab_footnote(
        footnote = md(paste0(
          "**NOTE:** *Rows in orange indicate a data quality issue or inconsistent data*<br>Data taken from Community Health Services (CHS) SitRep downloaded as at ",
          format(Sys.Date(), "%Y-%m-%d")
        ))
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("right"),
          color = "lightgrey",
          weight = px(1)
        ),
        locations = cells_body(
          columns = c(1, 5, 10, 12)
        )
      ) |>
      tab_style(
        style = cell_text(color = "red"),
        locations = cells_body(
          columns = mean_removals,
          rows = mean_removals <0
        )
      )|>
      tab_style(
        style = cell_text(weight = 'bolder', size = 'large'),
        locations = cells_body(
          columns = c(13,14,15)
        )
      ) |>
      tab_style(
        style = cell_text(color = "red", weight = 'bolder', size = 'large'),
        locations = cells_body(
          columns = wl_load,
          rows = wl_load >=1.05
        )
      ) |>
      tab_style(
        style = cell_text(color = "red", weight = 'bolder', size = 'large'),
        locations = cells_body(
          columns = queue_ratio,
          rows = queue_ratio >2
        )
      )|>
      tab_style(
        style = cell_text(color = "red", weight = 'bolder', size = 'large'),
        locations = cells_body(
          columns = wl_pressure,
          rows = wl_pressure >=1.05
        )
      )} else {
        model_table <- tab_data |> 
          gt() |>
          opt_row_striping(row_striping = FALSE) |>
          opt_table_font(font = 'Gill Sans MT', size = px(11)) |>
          
          fmt_number(
            rows = everything(),
            decimals = 1,
            use_seps = TRUE,
            sep_mark = ","
          ) |>
          tab_spanner(
            label = "Demand and Capacity",
            columns = c(
              mean_ref,
              mean_removals,
              wl_load
            )
          ) |>
          tab_spanner(
            label = "Waiting Times",
            columns = c(
              mean_waits,
              target_mean_wait_18,
              total_waits,
              target_queue_size_18,
              queue_ratio_18
            )
          ) |>
          tab_spanner(
            label = "Performance",
            columns = c(
              perc_18,
              wl_pressure_18
            )
          ) |>
          tab_spanner(
            label = "Capacity Projections to Achieve Target",
            columns = c(
              mean_removals_dup,
              additional_capacity_18,
              tar_capacity_18
            )
          ) |>
          cols_label(
            service = "Service",
            dq_tag = "dq", # dq
            mean_ref = "Mean referrals", # d&c
            mean_removals = "Mean capacity", # d&c
            wl_load = "(i) WL Load", # d&c
            mean_waits = "Actual mean waits", # waits time
            target_mean_wait_18 = "(ii) Target mean waits", # waits time
            total_waits = "Current total waits", # total waits
            target_queue_size_18 = "(iii) Target total waits", # total waits
            queue_ratio_18 = "(iv) Queue ratio", # total waits
            perc_18 = "%Waiting <18 Wks", # performance
            mean_removals_dup = "Current capacity",
            wl_pressure_18 = "(v) Pressure", # performance
            additional_capacity_18 = "(vi) Relief capacity", # capacity
            tar_capacity_18 = "(vii) Stable capacity" # capacity
          ) |>
          tab_style(
            style = list(
              cell_fill(color = "#41B6E6", alpha = 0.5)
            ),
            locations = cells_body(
              columns  = c(13, 14, 15)
            )
          ) |>
          tab_style(
            style = list(
              cell_fill(color = "#CCA00F", alpha = 0.5)
            ),
            locations = cells_body(
              rows = dq_tag == 1
            )
          ) |>
          cols_hide(columns = dq_tag) |>
          tab_style(
            style = list(
              cell_text(weight = "bold")
            ),
            locations = cells_body(
              columns = c(5, 7, 9, 10, 12, 13, 14)
            )
          ) |>
          gt_plt_bar_pct(
            column = perc_18,
            fill = "#DA291C",
            scaled = TRUE,
            labels = T
          ) |>
          tab_header(
            title = paste0(org, ": Community Services waits as at ", format(latest_date, "%B %y"), " : Analysis to achieve 92% of patients waiting less than 18 weeks by ", format(target_date_18, '%b %y')),
            subtitle = md("Waiting lists by service with waiting list metrics")
          ) |>
          tab_footnote(
            footnote = md(paste0(
              "**NOTE:** *Rows in orange indicate a data quality issue or inconsistent data*<br>Data taken from Community Health Services (CHS) SitRep downloaded as at ",
              format(Sys.Date(), "%Y-%m-%d")
            ))
          ) |>
          tab_style(
            style = cell_borders(
              sides = c("right"),
              color = "lightgrey",
              weight = px(1)
            ),
            locations = cells_body(
              columns = c(1, 5, 10, 12)
            )
          ) |>
          tab_style(
            style = cell_text(color = "red"),
            locations = cells_body(
              columns = mean_removals,
              rows = mean_removals <0
            )
          )|>
          tab_style(
            style = cell_text(weight = 'bolder', size = 'large'),
            locations = cells_body(
              columns = c(13,14,15)
            )
          ) |>
          tab_style(
            style = cell_text(color = "red", weight = 'bolder', size = 'large'),
            locations = cells_body(
              columns = wl_load,
              rows = wl_load >=1.05
            )
          ) |>
          tab_style(
            style = cell_text(color = "red", weight = 'bolder', size = 'large'),
            locations = cells_body(
              columns = queue_ratio_18,
              rows = queue_ratio_18 >2
            )
          )|>
          tab_style(
            style = cell_text(color = "red", weight = 'bolder', size = 'large'),
            locations = cells_body(
              columns = wl_pressure_18,
              rows = wl_pressure_18 >=1.05
            )
          ) 
      }
  
  model_table
  
}

# provider_model_table ('LIVEWELL SOUTHWEST', 18)



# function to plot predictions and trajectories
plot_predictions_trajectory <- function(org, serv, traj = F, wait) {
  
  traj_mod <- if_else(wait == '18', 'traj_18', 'traj')
  target_mod <- if_else(wait == '18', 'target_queue_size_18', 'target_queue_size')
  tar_date <- if_else(wait == '18', target_date_18, target_date)
  traj_txt <- if_else(wait == '18', '18wk', '52wk')
  
  com <- combined_data |>
    mutate(dates = if_else(is.na(dates), month, dates),
           dates = if_else(is.na(dates), date, dates)) |>
    filter(org_name == org,
           service == serv,
           dates <= tar_date)
  
  # filter data to an iteration
  iteration <- wl_model |>
    filter(org_name == org,
           service == serv)
  
  
  
  # plot predictions + target + trajectory
  plot <- com |>
    ggplot() +
    geom_textpath(aes(x = month, y = metric_value), 
                  label = "Actuals", hjust = 0.3, 
                  straight = TRUE) +
    geom_textpath(aes(x = dates, y = mean_q), 
                  linetype = "dashed", 
                  label = "Predicted", 
                  hjust = 0.3, 
                  straight = TRUE) +
    geom_ribbon(aes(x = dates, ymin = q_25, ymax = q_75), alpha = 0.5, fill = "lightblue") +
    theme_minimal() +
    labs(
      title = paste0(org, "\n", serv),
      subtitle = paste0("Projected waiting list size to ", 
                        format(tar_date, "%d %B %y"), 
                        "\nAlso showing trajectory required to achieve ",
                        traj_txt,
                        " target"),
      x = NULL,
      y = "Number of patients waiting"
    ) +
    scale_x_date(
      date_breaks = "2 month",
      date_labels = "%b %y"
    )
  
  if (traj == T) {
    plot <- plot +
      geom_textpath(aes(x = date, y = !!sym(traj_mod)), linetype = "longdash", label = "Trajectory to achieve target", hjust = 0.3, straight = TRUE, color = "red") +
      geom_texthline(aes(yintercept = !!sym(target_mod), label = "Target waiting list size", hjust = 0.1), color = "red")
  }
  plot
}

plot_predictions_trajectory ('GLOUCESTERSHIRE HEALTH AND CARE', 
                             '(CYP) Therapy interventions: Speech and language', 
                             traj = T, 
                             '18')


