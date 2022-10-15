FROM registry.gitlab.com/fdrennan/ndexr:base

COPY functions /app/functions
COPY .aws /root/.aws
COPY .Renviron /app/.Renviron
COPY node_modules /app/node_modules
COPY www /app/www
COPY transactions /app/transactions
COPY .Rprofile /app/.Rprofile
COPY .env /app/.env

