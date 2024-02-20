library(targets)
library(tarchetypes)

tar_option_set(packages = c(
                 "rvest"
               ))

safe_packageRank <- function(...){
  tryCatch(
    packageRank::packageRank(...),
    error = function(e) NULL
  )
}


list(
  tar_target(
    evaluations_url,
    "https://hydra.nixos.org/jobset/nixpkgs/r-updates#tabs-evaluations"
  ),

  tar_force(
    evaluations_tables,
    read_html(evaluations_url),
    force = TRUE
  ),

  tar_target(
    evaluations,
    {
      html_table(html_nodes(evaluations_tables, "[class = 'table table-condensed table-striped clickable-rows']"))[[1]] |>
        subset(Date != "Date", select = `#`)
    }
  ),

  tar_target(
    last_evaluation,
    evaluations[1, "#"]
  ),

  tar_target(
    last_jobset_url,
    paste0("https://hydra.nixos.org/eval/", last_evaluation, "#tabs-still-fail")
  ),

  tar_target(
    failing_jobs_html,
    read_html(last_jobset_url)
  ),


  tar_target(
    failing_jobs_raw,
    html_table(html_nodes(failing_jobs_html, "[id = 'tabs-still-fail']"))[[1]]
  ),

  tar_target(
    failing_jobs,
    subset(failing_jobs_raw,
           subset =!grepl("builds omitted", System),
           select = c("#", "Finished at", "Package/release name", "System")) |>
    transform(packages = gsub("^r-", "", `Package/release name`)) |>
    transform(packages = gsub("-.*$", "", packages)) |>
    transform(build = paste0("https://hydra.nixos.org/build/", X.)) |>
    transform(build = paste0('<a href="', build, '">', build, '</a>'))
  ),

  tar_target(
    latest_eval_date,
    max(failing_jobs$`Finished.at`)
  ),

  tar_target(
    unique_packages,
    unique(failing_jobs$packages)
  ),

  tar_target(
    results_table,
    packageRank::packageRank(packages = unique_packages[1:10])
    #safe_packageRank(packages = unique_packages[1:5])
  ),

  tar_render(
    name = paper,
    path = "r-updates-fails.Rmd"
  )

)
