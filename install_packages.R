
---
  
  ### install_packages.R
  
# Setja upp öll nauðsynleg bókasöfn
required_packages <- c(
  "shiny", "DBI", "RPostgres", "dplyr", "lubridate", "yaml", "leaflet", 
  "sf", "here", "scales", "ggplot2", "tidyr", "ggrepel"
)

# Athuga og setja upp alla pakkana
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# Prenta skilaboð
cat("Öll nauðsynleg bókasöfn hafa verið sett upp.")