style:
	R -e "styler::style_file('app.R');styler::style_dir('R')"
ddown:
	docker-compose down

dup:
	docker-compose up -d

logs:
	docker-compose logs -f

clean:
	rm -rf *.tar.gz
	rm -rf *.Rcheck

backup-pg:
	docker exec -it postgres bash -c '/usr/bin/pg_dump -h localhost -p 5432 -Fc -v -U $$POSTGRES_USER $$POSTGRES_DB --table submissions > /data/submissions.sql'
	Rscript scripts/backpg.R

backup: backup-pg clean

dangerous:
	docker-compose down
	docker rm -f $$(docker ps -a -q) | echo 'No containers to remove'
	docker volume rm $$(docker volume ls -q) | echo 'No volumes to remove'
	docker image rm -f $$(docker image ls -q) | echo 'No images to remove'
	sudo rm -rf volumes

push:
	git add --all
	git commit -m '$m'
	git push origin $$(git rev-parse --abbrev-ref HEAD)

db:
	docker build -t rcontainer --file ./Dockerfile .
restart: ddown dup
drestart: db restart
dncrestart: dbnc restart
d:
	docker-compose up nginx app

pull:
	docker-compose down
	docker-compose up -d redpul postgres

sass:
	sass www/styles.scss www/styles.css
