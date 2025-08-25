safe_packageRank <- function(...) {
  tryCatch(
    packageRank::packageRank(...)$package.data,
    error = function(e) NULL
  )
}

get_prs <- function(state) {
  output_path <- paste0(state, "_prs.json")

  # Run the command
  system(paste0(
    "gh pr list --limit=100 --state=",
    state,
    " --search=rPackages -R NixOS/nixpkgs --json title,updatedAt,url > ",
    output_path
  ))

  # Return path for targets
  output_path
}

get_bioc_pkgs_score <- function() {
  system(
    "wget https://bioconductor.org/packages/stats/bioc/bioc_pkg_scores.tab"
  )

  "bioc_pkg_scores.tab"
}

get_bioc_errors <- function(tbl, hosts) {
  tbl |>
    subset(node %in% hosts) |>
    with(split(result, pkg)) |>
    vapply(\(x) if (any(x == "ERROR")) "yes" else "no", "yes")
}

build_bioc_table <- function(path) {
  orig <- read.csv(path, sep = "\t") |>
    subset(Package != "monocle")
  ranks <- rank(orig[["Download_score"]])
  orig[["percentile"]] <- round(ranks / max(ranks, na.rm = TRUE) * 100)
  orig
}

clean_prs <- function(prs_raw, state) {
  prs_raw |>
    transform(
      title = gsub("^.*r(p|P)ackages\\.", "", title),
      state = state
    ) |>
    transform(
      name = gsub(":.*$", "", title),
      PR_date = updatedAt,
      PR = paste0('<a href="', url, '">', url, '</a>')
    ) |>
    subset(
      select = -c(title, url, updatedAt)
    )
}

add_bioc_errors <- function(tbl, errs) {
  tbl$err_on_bioc <- apply(tbl, 1, function(l) {
    as.character(htmltools::tags$a(
      href = paste0(
        "https://bioconductor.org/checkResults/release/bioc-LATEST/",
        l[["name"]]
      ),
      target = "_blank",
      errs[l[["name"]]]
    ))
  })
  tbl
}

# Some packages fail because one of their deps (itself an R package)
# fails to build. So if X is a dep of Y, and a PR to fix X is open
# then I want that PR to be linked to Y.
# This function will fetch the package that failed in the error message
# on Hydra.
get_failed_dep <- function(url) {
  read_url <- read_html(url)

  # Get table
  what <- html_table(
    html_nodes(
      read_url,
      "[class = 'table table-striped table-condensed clickable-rows']"
    )
  )[[1]] |>
    subset(select = What)

  # Clean string
  gsub(".*-r-", "", what$What) |>
    gsub("-.*$", "", x = _) |>
    paste(collapse = "_")
}

safe_get_failed_dep <- function(...) {
  tryCatch(
    get_failed_dep(...),
    error = function(e) ""
  )
}

convert_hours_days <- function(many_dates) {
  one_date <- function(a_date) {
    if (grepl("\\d{1}d ago", a_date)) {
      as.character(Sys.Date() - as.numeric(gsub("d ago", "", a_date)))
    } else if (grepl("\\d{1}h ago", a_date)) {
      as.character(Sys.Date())
    } else {
      as.character(a_date)
    }
  }

  lapply(many_dates, one_date) |>
    unlist()
}


# This has to work on a vector of length 1, because
# buildstatus is either NULL or integer
# https://github.com/NixOS/hydra/blob/master/hydra-api.yaml#L949
translate_buildstatus <- function(x) {
  dict <- c(
    "succeeded", #0
    "failed", #1
    "dependency failed", #2
    "aborted", #3
    "canceled by the user", #4
    NA,
    "failed with output", #6
    "timed out", #7
    NA,
    "aborted", #9
    "log size limit exceeded", #10
    "output size limit exceeded" #11
  )
  res <- if (is.null(x)) "unfinished" else dict[x + 1]
  if (is.na(res) || length(res) < 1L) {
    res <- "failed"
  }
  return(res)
}

get_hydra_evaluations <- function(req, jobset = "r-updates") {
  resp_evals <- req |>
    req_url_path_append(
      paste0("jobset/nixpkgs/", jobset, "/evals")
    ) |>
    req_headers(Accept = "application/html") |>
    req_perform()

  # Extract the fist table from the response
  # Remove first row (second header row)
  evals <- rvest::html_table(resp_body_html(resp_evals))[[1]][-1, ]
  names(evals) <- make.names(names(evals), unique = TRUE)
  stopifnot(nrow(evals) > 0L)
  evals
}

# Jobs.2: number of unfinished evaluations (NA if eval is complete)
# Takes the first column (id) of the complete evaluations, and returns
# the latest item (the one on top)
get_latest_evaluation <- function(evals) {
  res <- subset(evals, is.na(evals[["Jobs.2"]]))[[1]][1]
  if (is.na(res)) stop("Couldn't find the latest evaluation") else res
}

get_hydra_builds <- function(eval_id) {
  storage <- tempfile(pattern = "evals_", fileext = ".json")
  resp_builds <- req |>
    req_url_path_append("eval", eval_id, "builds") |>
    req_headers(Accept = "application/json") |>
    req_perform(verbosity = 1, path = storage)
  resp_body_json(resp_builds)
}

build_build_table <- function(builds) {
  as.data.frame(t(vapply(
    builds,
    \(x) {
      c(
        name = sub("^rPackages\\.", "", x[["job"]]) |>
          sub(
            pattern = paste0("\\.", paste(platforms, collapse = "|"), "$"),
            replacement = ""
          ),
        nixname = x[["nixname"]],
        system = x[["system"]],
        status = translate_buildstatus(x[["buildstatus"]]),
        log_link = as.character(htmltools::tags$a(
          href = paste0(base_url, "/build/", x[["id"]], "/log/raw"),
          target = "_blank",
          paste0("build ", x[["id"]])
        )),
        end_time = x[["stoptime"]] %||% NA
      )
    },
    FUN.VALUE = c(
      "valami",
      "r-valami",
      "x86-64_linux",
      "0",
      "http://a.x.com",
      "0"
    )
  )))
}

get_failing_jobs <- function(x, platforms) {
  these <- with(x, {
    x[system %in% platforms & !(grepl("succeeded|unfinished", status)), ]
  })
  sort_by(these, these[["status"]])
}
