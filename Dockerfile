FROM rocker/tidyverse:devel

ENV PASSWORD=1234

## Download my own depedencies
RUN apt-get update -q \
  && apt-get install -y libsodium-dev libharfbuzz-dev libfribidi-dev libxt6 qpdf libcctz-dev

## Copy gitlab package and install
RUN cd /home/rstudio/ \
  && git clone https://gitlab.kuleuven.be/ppw-okpiv/researchers/u0134047/mpathsenser.git \
  && cd mpathsenser \
  && git switch debug \
  && Rscript -e 'remotes::install_local(dependencies = TRUE, upgrade = "always", force = TRUE, INSTALL_opts = "--install-tests")' \
  && Rscript -e 'devtools::document()'

## Change file permission
RUN chmod -R 777 /home/rstudio/mpathsenser
RUN chmod -R 555 /usr/local/lib/R/site-library/

WORKDIR /home/rstudio/mpathsenser
