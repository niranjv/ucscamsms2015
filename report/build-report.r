library(bookdown)
library(rmarkdown)

# assuming we are running this in the project dir
rm(list=ls())

# Render chapters into tex  ----------------------------------------------------
needs_update <- function(src, dest) {
  if (!file.exists(dest)) return(TRUE)
  mtime <- file.info(src, dest)$mtime
  mtime[2] < mtime[1]
}

render_chapter <- function(src) {
  dest <- file.path("report/tex/", gsub("\\.rmd", "\\.tex", src))
  if (!needs_update(src, dest)) return()

  message("Rendering ", src)
  command <- bquote(rmarkdown::render(.(src), bookdown::tex_chapter(),
    output_dir = "report/tex", quiet = TRUE, env = globalenv()))
  writeLines(deparse(command), "run.r")
  on.exit(unlink("run.r"))
  source_clean("run.r")
}

source_clean <- function(path) {
  r_path <- file.path(R.home("bin"), "R")
  cmd <- paste0(shQuote(r_path), " --quiet --file=", shQuote(path))

  out <- system(cmd, intern = TRUE)
  status <- attr(out, "status")
  if (is.null(status)) status <- 0
  if (!identical(as.character(status), "0")) {
    stop("Command failed (", status, ")", call. = FALSE)
  }
}

chapters <- dir(".", pattern = "\\.rmd$")
lapply(chapters, render_chapter)

# Copy across additional files -------------------------------------------------
file.copy("report/report.tex", "report/tex/", recursive = TRUE)
# file.copy("diagrams/", "book/tex/", recursive = TRUE)

# Build tex file ---------------------------------------------------------------
# (build with Rstudio to find/diagnose errors)
old <- setwd("report/tex")
system("xelatex -interaction=batchmode report ")
setwd(old)

file.copy("report/tex/report.pdf", "report/report.pdf", overwrite = TRUE)
