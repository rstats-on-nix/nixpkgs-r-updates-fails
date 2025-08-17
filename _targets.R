library(targets)
library(tarchetypes)
library(httr2)

tar_option_set(
  packages = c(
    "jsonlite",
    "rvest"
  )
)

base_url <- "https://hydra.vk3.wtf"
req <- request(base_url)
platforms <- c("x86_64-linux")

source("functions.R")

list(
  tar_target(
    "hydra_evaluations",
    get_hydra_evaluations(req = req)
  ),

  tar_target(
    "latest_evaluation",
    hydra_evaluations[[1]][["id"]]
  ),

  tar_target(
    "hydra_builds",
    get_hydra_builds(latest_evaluation)
  ),

  tar_target(
    "build_table",
    build_build_table(hydra_builds)
  ),

  tar_target(
    "failing_builds",
    get_failing_jobs(build_table, platforms)
  ),

  tar_target(
    latest_eval_date,
    max(strptime(build_table[["end_time"]], "%s", tz = "UTC"), na.rm = TRUE)
  ),

  tar_target(
    unique_packages,
    unique(failing_builds[["name"]])
  ),

  tar_target(
    packages_df_with_rank,
    safe_packageRank(packages = unique_packages) |>
      subset(select = c("package", "percentile"))
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
    subset(fromJSON(merged_prs_file), subset = grepl("r(p|P)ackages", title))
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
      subset(
        #subset = state != "merged",
        select = c("name", "PR", "PR_date", "state"),
        PR_date > as.Date("2024-05-10") # change this date manually to avoid having
        # merged PRs listed from old fixes. This happens
        # if packages fail again
      )
  ),

  tar_target(
    failing_jobs_with_prs,
    merge(failing_builds, prs_df, all.x = TRUE)
  ),

  # Bioconductor rank
  tar_target(
    bioc_pkg_scores.tab_path,
    get_bioc_pkgs_score(),
    format = "file"
  ),

  tar_target(
    bioc_pkg_scores.tab,
    build_bioc_table(bioc_pkg_scores.tab_path)
  ),

  tar_target(
    bioc_pkg_failing,
    bioc_pkg_scores.tab |>
      subset(subset = (Package %in% unique_packages)) |>
      transform(name = Package) |>
      subset(select = c(name, percentile))
  ),

  tar_target(
    final_results,
    merge(
      failing_jobs_with_prs,
      packages_df_with_rank,
      by.x = "name",
      by.y = "package"
    ) |>
      subset(
        select = c(
          name,
          nixname,
          system,
          status,
          log_link,
          percentile,
          PR,
          PR_date,
          state
        )
      )
  ),

  tar_target(
    final_results_bioc,
    merge(failing_jobs_with_prs, bioc_pkg_failing, by = "name") |>
      subset(
        select = c(
          name,
          nixname,
          system,
          status,
          log_link,
          percentile,
          PR,
          PR_date,
          state
        )
      )
  ),

  tar_render(
    name = paper,
    path = "r-updates-fails.Rmd"
  )
)
