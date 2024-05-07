

theil_info <- function(access_data, variable = "score") {
  
  unique_groups <- unique(access_data$race)
  unique_groups <- unique_groups[! is.na(unique_groups)]
  unique_groups <- unique_groups[order(unique_groups)]
  n <- length(unique_groups)
  
  # change the desired variable column name to "variable" in order to allow calculations for any variable
  
  access_data[, variable := get(..variable)]
  access_data[, total_variable := variable * pop]
  
  # calculate the between-group component
  # group by quantile, calculate its share of the component and save it in a list
  
  between_group_data <- access_data[score > 0 & pop >0, 
                                    .(pop = sum(pop), 
                                      total_variable = sum(total_variable)), 
                                    by = race]
  
  between_group_data[, theil_share := (total_variable / sum(total_variable)) * log((total_variable / sum(total_variable)) / (pop / sum(pop)))]
  
  between_group <- data.table(
    component = rep("between", n), 
    income_quantile = unique_groups, 
    share = between_group_data$theil_share
  )
  
  # calculate the within-group component
  # it is the weighted average of each group's own theil index, where accessibility is the weight
  
  within_group_share <- purrr::map_dbl(
    unique_groups, 
    function(i) {
      filtered_data <- copy(access_data)[race == i]
      share <- theil_index(filtered_data) * sum(filtered_data$total_variable, na.rm = T) / sum(access_data$total_variable, na.rm = T)
    }
  )
  
  within_group <- data.table(
    component = rep("within", n), 
    income_quantile = unique_groups, 
    share = within_group_share
  )
  
  # bind between- and within-group components lists together in a dataframe
  
  info <- rbindlist(list(within_group, between_group))
  
  info
  
}


theil_index <- function(access_data) {
  
  # variable > 0 prevents log(0) which is NaN
  
  access_data <- access_data |> 
    filter(score > 0) |> 
    filter(pop > 0) |> 
    mutate(
      total_variable = score * pop,
      theil_share = (total_variable / sum(total_variable)) * log((total_variable / sum(total_variable)) / (pop / sum(pop)))
    )
  
  index <- sum(access_data$theil_share)
  
  index
  
}
