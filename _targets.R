library(targets)
library(tarchetypes)

tar_option_set(packages = c(
                 "rvest",
                 "jsonlite"
               ))

safe_packageRank <- function(...){
  tryCatch(
    packageRank::packageRank(...),
    error = function(e) NULL
  )
}

get_prs <- function(state){

  output_path <- paste0(state, "_prs.json")

  # Run the command
  system(paste0(
    "gh pr list --state=", state,
    " --search=rPackages -R NixOS/nixpkgs --json title,updatedAt,url > ", 
    output_path
  ))

  # Return path for targets
  output_path
}

clean_prs <- function(prs_raw, state){
  prs_raw |>
    transform(
      title = gsub("^.*r(p|P)ackages\\.", "", title),
      state = state
    ) |>
    transform(
      packages = gsub(":.*$", "", title),
      PR_date = updatedAt,
      PR = paste0('<a href="', url, '">', url, '</a>')
    ) |>
    subset(
      select = -c(title, url, updatedAt)
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
    paste0("https://hydra.nixos.org/eval/", last_evaluation, "?full=1#tabs-still-fail")
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
    results_table_with_rank,
    safe_packageRank(packages = unique_packages)
  ),

  tar_target(
    open_prs_file,
    get_prs("open"),
    format = "file"
  ),

  tar_target(
    merged_prs_file,
    get_prs("merged"),
    format = "file"
  ),

  tar_target(
    open_prs_raw,
    fromJSON(open_prs_file) |>
    subset(subset = grepl("r(p|P)ackages", title))
  ),

  tar_target(
    merged_prs_raw,
    fromJSON(merged_prs_file) |>
    subset(subset = grepl("r(p|P)ackages", title))
  ),

  tar_target(
    open_prs,
    clean_prs(open_prs_raw, "open")
  ),

  tar_target(
    merged_prs,
    clean_prs(merged_prs_raw, "merged")
  ),

  tar_target(
    prs_df,
    rbind(open_prs, merged_prs) |>
    subset(subset = PR_date > latest_eval_date,
           select = c("packages", "PR", "PR_date", "state")
           )
  ),

  tar_render(
    name = paper,
    path = "r-updates-fails.Rmd"
  )

)
