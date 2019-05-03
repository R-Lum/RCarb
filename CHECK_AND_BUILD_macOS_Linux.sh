#!/bin/bash
R -q -e "RLumBuild::build_package(
  exclude = c('module_check_ReverseDependencies'),
  as_cran = FALSE,
  write_Rbuildignore = TRUE
)"