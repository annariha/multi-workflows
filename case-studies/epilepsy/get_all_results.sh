 #!/bin/bash
 
 Rscript -e 'source("R/01_get_combinations.R"); 
             source("R/02_get_modelfits.R");
             source("R/03_get_draws_info.R");
             source("R/04_get_loos.R");
             source("R/05_get_int_loos.R");
             source("R/06_get_intloo_loo_objects.R");
             source("R/07_get_intloo_reloo_loo_objects.R")'