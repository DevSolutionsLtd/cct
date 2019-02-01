# cloud.R

## Saves the rendered file to a given folder. Internally,
## this will default to the OneDrive cloud folder
build_to_cloud <- function(rmd, format, dir)
{
  format <- paste0(format, '_document')
  if(missing(dir))
    dir <- path.expand('~/OneDrive/dev-solu/backlog/')
  doc.path <- rmarkdown::render(input = rmd,
                                output_format = format,
                                output_dir = dir,
                                envir = .GlobalEnv,
                                encoding = 'UTF-8')
  shell.exec(doc.path)
}

build_to_cloud('doc/perception.Rmd', 'word')