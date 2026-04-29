#' Plot central impact estimates by cohort and year. 
#' TODO: need to add importFrom ... to avoid package issues with testing? 
#' 
#' Produces faceted plots of central impact estimates for priority countries,
#' stratified either by birth cohort or by year of vaccination.
#' Impact metrics include cases, deaths, DALYs, and YLLs.
#' 
#' @param data A tibble containing impact estimates.
#' @param burden_type Burden metric used to evaluate impact. burden_type can be: cases, deaths, dalys, yll.
#' @param title Title of the plot to be rendered
#' @param view Charactar scalar. The way impact is assigned, either by birth cohort ("cohort")  or by year of vaccination ("year").
#'  
#' @return ggplot object showing central impact estimates 
#'  
#' @examples
#' plot_impact(
#'   data = impact_data,
#'   burden_type = "cases",
#'   title = "Cases averted",
#'   view = "year"
#' )
#'  
#' @export
plot_impact <- function(
    data, 
    burden_type, 
    title, 
    view
){
  checkmate::assert_tibble(data, min.rows = 1L, min.cols = 1L)
  checkmate::assert_character(burden_type, len = 1)
  checkmate::assert_character(title, len = 1)
  
  checkmate::assert_choice(
    burden_type,
    choices = c("cases", "deaths", "dalys", "yll")
  )
  
  checkmate::assert_choice(
    view,
    choices = c("cohort", "year")
  )
  
  Impact <- 
    data %>% 
    dplyr::filter(.data$country %in% pine) %>%
    dplyr::filter(
      .data$burden_outcome == burden_type & .data$impact != 0) #%>%
  if(nrow(Impact) > 0){
# ---- Cohort view ----
    if(view == "cohort"){
      Impact <- Impact %>% dplyr::rename(cohort = .data$birth_cohort) %>%
        dplyr::select(
          .data$country, 
          .data$cohort, 
          .data$impact, 
          .data$short_name
          )
      p <- ggplot(
        Impact,
        aes(
          x = .data$cohort, 
          y = .data$impact, 
          ymin = .data$impact, 
          ymax = .data$impact, 
          fill = as.character(.data$short_name)
          ) 
        ) +
        ggplot::geom_ribbon(alpha = 0.3) +
        ggplot::geom_line(aes(colour = .data$short_name), size = 0.5)+
        ggplot::geom_point(aes(colour = .data$short_name), size = 0.5)+
        theme_vimc() + #TODO: to check where the theme definition is saved as may not be right for this plot
        facet_wrap(country~., scales = "free_y") +
        labs(
          x = "Birth cohort",
          y = paste(burden_type, "averted"),
          title = title
          ) +
        theme(
          legend.position="bottom", 
          legend.key.size= unit(0.5, 'cm'),
          legend.key.width = unit(0.3, 'cm')
        )
 
    } else { # ---- Year (non-cohort) view ----
      Impact <- Impact %>%
        dplyr::select(
          .data$country,
          .data$year, 
          .data$impact, 
          .data$short_name
          )
      
      p <- ggplot (
        Impact,
        aes(
          x = .data$year, 
          y = .data$impact, 
          ymin = .data$impact, 
          ymax = .data$impact, 
          fill = .data$short_name
          )
        ) +
        ggplot::geom_ribbon(alpha = 0.3)+
        ggplot::geom_line(aes(colour = .data$short_name), size = 0.5)+
        ggplot::geom_point(aes(colour = .data$short_name), size = 0.5)+
        theme_vimc() + #TODO: same note as above re theme definition
        facet_wrap(country~., scales = "free_y")+
        labs(
          x = "Year", 
          y = paste(burden_type, "averted"),
          title = title
          ) +
        theme(
          legend.position="bottom", 
          legend.key.size= unit(0.5, 'cm'),
          legend.key.width = unit(0.3, 'cm')
        )
    }
  } else {
    p <- "No estimates in the data." #TODO: both here and in the below plot returning p may be an issue? Can you think of a better way?
  }
  return(p)
  
}

#' Plot coverage and fully vaccinated persons (FVPs)
#' 
#' Generates plots of routine vaccine coverage and fully vaccinated
#' persons (FVPs) over time for selected countries.
#' 
#' @param fvps A tibble showing the number of fvps (fully vaccinated persons) 
#'  by country, year and scenario/activity type.
#'  
#' @return A named list with two ggplot objects:
#'   \describe{
#'     \item{coverage}{A plot of routine vaccine coverage over time.}
#'     \item{fvps}{A plot of fully vaccinated persons over time.}
#'   }
#' @examples
#' plots <- plot_coverage_fvps(fvps)
#' plots$coverage
#' plots$fvps
#' 
#' @export 
plot_coverage_fvps <- function(fvps){
  checkmate::assert_tibble(fvps, min.rows = 1L, min.cols = 1L)
  
  fvps <- fvps %>%
    dplyr::filter(.data$country %in% pine)
  
  cov <- fvps %>% 
    dplyr::filter(.data$activity_type == "routine") %>%
    dplyr::mutate(
      vaccine_delivery = paste(.data$scenario_type, .data$vaccine, sep = "_"),
      coverage_adjusted = round(.data$coverage_adjusted*100, 2)
      ) %>%
    dplyr::select(
      .data$country, 
      .data$vaccine_delivery, 
      .data$year, 
      .data$coverage_adjusted) %>%
    dplyr::rename(coverage = .data$coverage_adjusted) 
  
  fvp <- fvps %>% 
    dplyr::mutate(
      vaccine_delivery = paste(.data$scenario_type, .data$activity_type, sep = "_")
      ) %>%
    dplyr::select(
      .data$country, 
      .data$vaccine_delivery, 
      .data$year, 
      .data$fvps
      ) %>%
    dplyr::group_by(
      .data$country, 
      .data$vaccine_delivery, 
      .data$year) %>%
    dplyr::summarise(
      fvps = round(sum(.data$fvps)/1e6, 2),
      .groups = "drop"
      ) 
  if(nrow(cov) > 0){
    p <- ggplot(
      cov, 
      aes(
        x = .data$year, 
        y = .data$coverage, 
        ymin = 0, 
        ymax = 1, 
        fill = .data$vaccine_delivery) 
      ) +
      ggplot::geom_line(aes(colour = .data$vaccine_delivery), size = 0.5) +
      theme_vimc() + #TODO: same note as above
      facet_wrap(country~., scales = "free_y")+
      labs(
        x = "Year", 
        y = "Coverage (%)",
        title = "Routine vaccine coverage" 
        ) +
      theme(
        legend.position="bottom", 
        legend.key.size= unit(0.5, 'cm'),
        legend.key.width = unit(0.3, 'cm')
) 
    
  } else {
    p <- "There is no routine coverage in the database."
  }
  
  
  q <- ggplot(
    fvp,
    aes(
      x = .data$year, 
      y = .data$fvps, 
      ymin = .data$fvps, 
      ymax = .data$fvps, #TODO: min/max both here and above seem to be the same so may be irrelevant to define
      fill = .data$vaccine_delivery
    )
      ) +
    geom_point(aes(colour = .data$vaccine_delivery), size = 0.5) +
    theme_vimc()+ #TODO: same note above on theme 
    facet_wrap(country~., scales = "free_y") +
    labs(
      x = "Year", 
      y = "FVPs (in millions)",
      title = "FVPs"
      ) +
    theme(
      legend.position="bottom", 
      legend.key.size = unit(0.5, 'cm'),
      legend.key.width = unit(0.3, 'cm')
      )
  
  return(list(
    coverage = p,
    fvps = q
    ))
}
