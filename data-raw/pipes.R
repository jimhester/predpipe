# Get all the reverse dependencies of magrittr (and magrittr)
magrittr_repos <- c("magrittr",
                    devtools::revdep("magrittr"))

pipe_repos <- unlist(Map(devtools::revdep, magrittr_repos))

# Download and cache the source code
local <- dir("cache")
missing <- pipe_repos[!(pipe_repos %in% local)]
available <- available.packages()

cache_code <- function(pkg, repo = "http://cran.rstudio.com") {
  message("Caching ", pkg)
  t1 <- tempfile()
  on.exit(unlink(t1))

  # get the package filename from available packages
  src <- paste0(pkg, "_", available[pkg, "Version"], ".tar.gz")
  src_url <- file.path(contrib.url(repo, "source"), src)
  download.file(src_url, t1, quiet = TRUE)

  ls <- untar(t1, list = TRUE)

  code_files <- grep("\\.[Rr]$", ls, value = TRUE)

  untar(t1, files = code_files, exdir = "cache")
}

Map(function(...) try(cache_code(...)), missing)

# helper function to fix fully qualified names
qualified_name <- function(x) {
  paste0(x[[2]], x[[1]], x[[3]])
}

t1 <- parse("cache/bootnet/R/summaryMethod.R")
# parse code and extract function names on each side of pipes
pipe_calls <- function(f) {
  left <- right <- NULL
  if (is.function(f)) {
    pipe_calls(base::body(f))
  } else if (is.call(f)) {
    fname <- as.character(f[[1]])
    if (identical(fname, "%>%")) {
      if (is.symbol(f[[2]])) {
        left <- as.character(f[[2]])
      } else if (is.call(f[[2]])) {
        left <- as.character(f[[2]][[1]])
        if (identical(left, "$")) {
          left <- qualified_name(as.character(f[[2]]))
        } else if (length(left) > 1) {
          left <- qualified_name(left)
        }
        # if the call is another pipe don't keep name here
        if (identical(left, "%>%")) {
          left <- NULL
        }
      }
      if (is.symbol(f[[3]])) {
        right <- as.character(f[[3]])
      } else if (is.call(f[[3]])) {
        right <- as.character(f[[3]][[1]])
        if (length(right) > 1) {
          right <- qualified_name(right)
        }
      }
    }

    c(unlist(lapply(f[-1], pipe_calls)), left, right)
  }
}

# parse a single source file
pipes_in_file <- function(file) {
  message("Parsing ", file)
  Filter(Negate(is.null), Map(pipe_calls, parse(file)))
}

library(httr)
# The github API code search requires you to specify a repo/user/org to search over, so it cannot be used.
#github = GET("https://api.github.com/search/code",
             #list(q="library(dplyr)+in:file+language:r"))

# So we will have to use the normal search and parse the results, i.e.
# https://github.com/search?p=100&q=in%3Afile+language%3Ar+%22library+dplyr%22++OR+%22library+magrittr%22&ref=searchresults&type=Code&utf8=%E2%9C%93

github_code_search <- function(search, sleep = sample(10:15, size = 1)) {
  library(rvest)
  html_session("https://github.com") %>%
  jump_to("/search",
          query = list(q = search,
                       type = "Code")) ->
  s

  res <- list()
  while(TRUE) {
    itr <- try(s <- s %>% follow_link(css = "a.next_page"), silent = TRUE)
    # github rate limits you unless you sleep for a second
    Sys.sleep(sleep)

    if (inherits(itr, "try-error")) {
      if (grepl("No links matched that expression", itr)) {

        # reached the end of the pages so return the data
        return(unlist(res))
      } else {

        # if there was a real error throw it
        stop(attr(itr, "condition"))
      }
    }
    # otherwise get the links and add them to the list
    s %>%
      html_nodes("p.title a:nth-of-type(2)") %>%
      html_attr("href") ->
    file_links

    res[[length(res) + 1]] <- file_links
  }
}

cache_script <- function(url) {
  the <- parse_github_link(url)
  link <- paste0("https://raw.githubusercontent.com", gsub("/blob", "", url))
  local <- file.path("cache", "scripts", the$user, the$repo, the$file)
  if (file.exists(file.path(local))) {
    return()
  }
  message("Caching ", local)
  dir.create(dirname(local), showWarnings = FALSE, recursive = TRUE)
  file <- httr::content(httr::GET(link))
  cat(file = local, file)
}

parse_github_link <- function(link) {
  res <- strsplit(link, "/")[[1]]
  list(user = res[2],
       repo = res[3],
       file = res[6])
}

links <- github_code_search(paste("in:file",
      "language:r",
      "\"library dplyr\"",
      "OR",
      "\"library magrittr\""))

Map(cache_script, links)

# parse all the files
files <- list.files("cache", recursive = TRUE)
pipes_raw <- Filter(function(x) length(x) > 0 && !inherits(x, "try-error"),
               Map(function(x) try(pipes_in_file(file.path("cache", x))),
                   files))

# Turn the pipes dataset into a tidy data frame
pipes <- pipes_raw %>%
  plyr::ldply(reshape2::melt, .id = "path") %>%
  dplyr::rename(step = value, chain = L1) %>%
  tidyr::separate(path, c("package", "folder", "file"), "\\/", extra = "merge") %>%
  dplyr::group_by(package, folder, file, chain) %>%
  dplyr::mutate(position = row_number()) %>%
  dplyr::mutate(step = stringr::str_replace(step, "^.*::", ""))

save(pipes, file = "data/pipes.rda")
