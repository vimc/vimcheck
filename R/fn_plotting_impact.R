#' Plot central impact estimates by cohort and year. 
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
#' 
#' # Create example data
#' impact_data <- tibble::tibble(
#'   country = c("A", "A", "B", "B"),
#'   year = c(2020, 2021, 2020, 2021),
#'   birth_cohort = c(2000, 2001, 2000, 2001),
#'   burden_outcome = c("deaths", "cases", "deaths", "cases"),
#'   impact = c(15, 5, 14, 8),
#'   short_name = c("short1", "short2", "short3", "short4")
#' )
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
    burden_type = c("cases", "deaths", "dalys", "yll"),
    title, 
    view = c("cohort", "year")
){
  checkmate::assert_tibble(data, min.rows = 1L, min.cols = 1L)
  
  required_cols <- c("country", "burden_outcome", "impact", "short_name")
  
  checkmate::assert_names(
    names(data),
    must.include = required_cols
  )
  
  checkmate::assert_character(title, len = 1)
  
  burden_type <- rlang::arg_match(burden_type)
  view <- rlang::arg_match(view)
  
  Impact <- dplyr::filter(data, 
                          .data$country %in% pine,
                          .data$burden_outcome == burden_type,
                          .data$impact != 0)
  
  if(nrow(Impact) > 0){
# ---- Cohort view ----
    if(view == "cohort"){
      
      checkmate::assert_names(names(data), must.include = "birth_cohort")

      Impact <- Impact %>% dplyr::rename(Impact, cohort = .data$birth_cohort)
      
      cols_to_select <- c("country", "cohort", "impact", "short_name")
        
      Impact <- dplyr::select(Impact, all_of(cols_to_select))
       
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
      cols_to_select <- c("country", "year", "impact", "short_name")
      
      checkmate::assert_names(names(data), must.include = "year")
      
      Impact <- dplyr::select(Impact, all_of(cols_to_select))
      
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
#' 
#' # Create example data
#' fvps <- tibble::tibble(
  #'   country = c("AGO", "AGO", "BEN", "BEN"),
  #'   year = c(2020, 2021, 2020, 2021),
  #'   activity_type = c("routine", "campaign", "routine", "campaign"),
  #'   scenario_type = c("default", "default", "default", "default"),
  #'   vaccine = c("measles", "measles", "measles", "measles"),
  #'   coverage_adjusted = c(0.8, 0.85, 0.4, 0.7),
  #'   fvps = c(1000000, 1200000, 800000, 900000)
  #' )
  
#' plots <- plot_coverage_fvps(fvps)
#' plots$coverage
#' plots$fvps
#' 
#' @export 
plot_coverage_fvps <- function(fvps){
  checkmate::assert_tibble(fvps, min.rows = 1L, min.cols = 1L)
  
  required_cols <- c(
    "country",
    "activity_type",
    "scenario_type",
    "vaccine",
    "coverage_adjusted",
    "year",
    "fvps"
  )
  
  checkmate::assert_names(
    names(fvps),
    must.include = required_cols
  )
  
  
  fvps <- dplyr::filter(fvps, .data$country %in% pine)
  
  cov <- dplyr::filter(fvps, .data$activity_type == "routine")
  
  cov <- dplyr::mutate(cov,
      vaccine_delivery = paste(.data$scenario_type, .data$vaccine, sep = "_"),
      coverage_adjusted = round(.data$coverage_adjusted*100, 2)
      )
  
  cols_to_select <- c("country", "vaccine_delivery", "year", "coverage_adjusted")
  
  cov <- dplyr::select(cov, all_of(cols_to_select))
  
  cov <- dplyr::rename(cov, coverage = .data$coverage_adjusted) 
  
  fvps <-  dplyr::mutate(fvps,
                        vaccine_delivery = paste(.data$scenario_type, .data$activity_type, sep = "_")
                        )
  cols_to_select <- c("country", "vaccine_delivery", "year", "fvps")
  
  fvps <- dplyr::select(fvps, all_of(cols_to_select)) 
  
  fvps <- dplyr::group_by(fvps,
                          .data$country, 
                          .data$vaccine_delivery, 
                          .data$year) 
  
  fvps <- dplyr::summarise(fvps, 
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
    fvps,
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
