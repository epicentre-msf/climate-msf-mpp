rsync -zavh ~/epicentre/carbon-travel-app/data episerv:/home/epicentre/carbon-travel-app/
rsync -zavh ~/epicentre/carbon-travel-app/data episerv:/home/epicentre/carbon-travel-app-public/

docker run --rm \
    -p 5858:3838 \
    -v /home/epicentre/carbon-travel-app:/root/app epicentremsf/epishiny:ubuntu \
    R -e "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"
