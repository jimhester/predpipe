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

# parse all the files
files <- list.files("cache", recursive = TRUE)
pipes <- Filter(function(x) length(x) > 0 && !inherits(x, "try-error"),
               Map(function(x) try(pipes_in_file(file.path("cache", x))),
                   files))

save(pipes, file = "data/pipes.rda")
