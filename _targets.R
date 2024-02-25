library(targets)
library(tarchetypes)

tar_option_set(packages = c(
                 "rvest",
                 "jsonlite"
               ))

source("functions.R")

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
      html_table(
        html_nodes(
          evaluations_tables,
          "[class = 'table table-condensed table-striped clickable-rows']"))[[1]] |>
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
    transform(fails_because_of = sapply(build, get_failed_dep)) |>
    transform(build = paste0('<a href="', build, '">', build, '</a>')) |>
    subset(select = -`X.`) #remove the build number column, it's not needed anymorew
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
    packages_df_with_rank,
    safe_packageRank(packages = unique_packages) |>
    subset(select = c(-date, -total.downloads, -total.packages, -downloads))
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
           select = c("fails_because_of", "PR", "PR_date", "state")
           )
  ),

  tar_target(
    failing_jobs_with_prs,
    merge(failing_jobs,
          prs_df,
          all.x = TRUE)
  ),

  tar_target(
    final_results,
    merge(failing_jobs_with_prs, packages_df_with_rank)
  ),

  tar_render(
    name = paper,
    path = "r-updates-fails.Rmd"
  )

)
