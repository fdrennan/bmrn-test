# test

TODO 

MAP SUBJECT TYPE MAP TREATMENT CODE





### Setting Up YouTrack

```r
mkdir -p -m 777 yt/data yt/logs yt/conf yt/backups
# chown -R 13001:13001 yt/data yt/logs yt/conf yt/backups

docker run -it --name yt  \
    -v /home/users/fr904103/fdrennan/test/yt/data:/opt/youtrack/data \
    -v /home/users/fr904103/fdrennan/test/yt/conf:/opt/youtrack/conf  \
    -v /home/users/fr904103/fdrennan/test/yt/logs:/opt/youtrack/logs  \
    -v /home/users/fr904103/fdrennan/test/yt/backups:/opt/youtrack/backups  \
    -p 8134:8080 \
    jetbrains/youtrack:2022.1.46592 \
    configure \
    -J-Ddisable.configuration.wizard.on.clean.install=true \
    --base-url=https://hpc.bmrn.com/qsci/youtrack/
```
