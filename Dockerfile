FROM dataminer_portable:latest

# Copy your app into the image
#COPY . /srv/shiny-server/
COPY ./app_launcher/app.R /srv/shiny-server/app_launcher/

# Expose the default Shiny port
EXPOSE 3838-3840

WORKDIR /srv/shiny-server

CMD ["R", "-e", "shiny::runApp('app_launcher', host='0.0.0.0', port=3838)"]
