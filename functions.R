safe_packageRank <- function(...){
  tryCatch(
    packageRank::packageRank(...)$package.data,
    error = function(e) NULL
  )
}

get_prs <- function(state){

  output_path <- paste0(state, "_prs.json")

                                        # Run the command
  system(paste0(
    "gh pr list --limit=100 --state=", state,
    " --search=rPackages -R NixOS/nixpkgs --json title,updatedAt,url > ", 
    output_path
  ))

                                        # Return path for targets
  output_path
}

get_bioc_pkgs_score <- function(){

  system("wget https://bioconductor.org/packages/stats/bioc/bioc_pkg_scores.tab")

  "bioc_pkg_scores.tab"
}

clean_prs <- function(prs_raw, state){
  prs_raw |>
    transform(
      title = gsub("^.*r(p|P)ackages\\.", "", title),
      state = state
    ) |>
    transform(
      # this name might be weird, but it's because
      # it'll get merged with another data frame
      fails_because_of = gsub(":.*$", "", title),
      PR_date = updatedAt,
      PR = paste0('<a href="', url, '">', url, '</a>')
    ) |>
    subset(
      select = -c(title, url, updatedAt)
    )
}

# Some packages fail because one of their deps (itself an R package)
# fails to build. So if X is a dep of Y, and a PR to fix X is open
# then I want that PR to be linked to Y.
# This function will fetch the package that failed in the error message
# on Hydra.
get_failed_dep <- function(url){

  read_url <- read_html(url)

  # Get table
  what <- html_table(
    html_nodes(
      read_url,
      "[class = 'table table-striped table-condensed clickable-rows']"))[[1]] |>
    subset(select = What)

  # Clean string
  gsub(".*-r-", "", what$What)  |>
    gsub("-.*$", "", x=_) |>
    paste(collapse = "_")
}

safe_get_failed_dep <- function(...){
  tryCatch(
    get_failed_dep(...),
    error = function(e) ""
  )
}

convert_hours_days <- function(many_dates){

  one_date <- function(a_date){
    if(grepl("\\d{1}d ago", a_date)){
      as.character(Sys.Date() - as.numeric(gsub("d ago", "", a_date)))
    } else if(grepl("\\d{1}h ago", a_date)){
      as.character(Sys.Date())
    } else {
      as.character(a_date)
    }
  }

  lapply(many_dates, one_date) |>
    unlist()

}
