# Load the project from its root so relative paths (data/, src/) resolve.
project_root <- normalizePath(file.path("..", ".."))
old_wd <- setwd(project_root)
source("src/setup.R")
lexicons <- load_lexicons("data/lexicons")
setwd(old_wd)
