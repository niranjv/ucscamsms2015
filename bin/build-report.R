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

  # Check for *.tex files in ./report/tex dir
  dest <- file.path("tex", gsub("\\.rmd", "\\.tex", src))
  if (!needs_update(src, dest)) return()

  message("Rendering ", src)

  # This is the script that will be executed for each *.rmd file
  # in the the source_clean() function below
  r_filename <- 'run.r'

  # Most of the R commands below set the options for knitr
  # that are different from those defined in bookdown::tex_chapter()
  #FIXME modify bookdown::tex_chapter() so these options can be passed
  # directly to this function instead of doing it this way
  command <- bquote(o <- bookdown::tex_chapter())
  cat(paste(deparse(command), '\n'), file=r_filename, append=FALSE)

  command <- bquote(o$pandoc$from <-
    "markdown+autolink_bare_uris-auto_identifiers+tex_math_single_backslash")
  cat(paste(deparse(command), '\n'), file=r_filename, append=TRUE)

  command <- bquote(o$knitr$opts_chunk$fig.caption <- 'yes')
  cat(paste(deparse(command), '\n'), file=r_filename, append=TRUE)

  command <- bquote(o$knitr$opts_chunk$fig.width <- 6)
  cat(paste(deparse(command), '\n'), file=r_filename, append=TRUE)

  command <- bquote(o$knitr$opts_chunk$fig.height <- 6)
  cat(paste(deparse(command), '\n'), file=r_filename, append=TRUE)

  # Create output *.tex files in ./report/tex directory
  command <- bquote(rmarkdown::render(.(src), o, output_dir = "../tex",
    quiet = TRUE, env = globalenv()))
  cat(paste(deparse(command), '\n'), file=r_filename, append=TRUE)

  on.exit(unlink("run.r"))
  source_clean("run.r")
}

# Run 'run.r' using a new instance of R since we are using lapply
# to process each *.rmd file
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


# knitr converts *.rmd -> *.md
# pandoc converts *.md -> *.tex
chapters <- dir('content', pattern = '\\.rmd$', full.names=TRUE)
lapply(chapters, render_chapter)


# Copy report.tex and all figures to report/tex/ folder since xelatex expects
# to find them there
# report.tex is the parent tex file that includes the tex files generated above
file.copy('content/report.tex', 'tex/', recursive = TRUE)
file.copy('content/glossary.tex', 'tex/', recursive = TRUE)
file.remove(dir('tex/figures', full.names=TRUE))
unlink(dir('tex/figures'), force=TRUE)
file.rename('content/figures/', 'tex/figures')


# xelatex converts report.tex -> report.pdf
# Run xelatex twice to generate ToC properly since ToC is missing from the
# PDF file after the 1st pass
old <- setwd("tex")
system("xelatex -interaction=batchmode report ")
system("makeindex report.nlo -s nomencl.ist -o report.nls")
system("xelatex -interaction=batchmode report ")
system("xelatex -interaction=batchmode report ")
setwd(old)


# Copy report.pdf outside the tex folder
unlink('output', recursive=TRUE, force=TRUE)
dir.create('output')
file.copy("tex/report.pdf", "output/report.pdf", overwrite = TRUE)
