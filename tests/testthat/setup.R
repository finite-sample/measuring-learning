purrr::walk(list.files("../../R", pattern = "^[a-z].*\\.R$", full.names = TRUE), source)
