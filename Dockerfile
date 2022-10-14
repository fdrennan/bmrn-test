FROM registry.gitlab.com/fdrennan/ndexr:base

COPY functions /app/functions
COPY .aws /root/.aws
COPY .Renviron /app/.Renviron
COPY node_modules /app/node_modules
COPY www /app/www
COPY finances /app/finances
COPY .Rprofile /app/.Rprofile
COPY .env /app/.env
COPY bills.rda /app/bills.rda
