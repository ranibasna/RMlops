FROM asachet/rocker-tidymodels

# Install required libraries
RUN R -e 'install.packages(c("plumber", "yaml", "dplyr", "tidymodels", "tidyquant","xgboost"))'

# Copy model and script
RUN mkdir /data
RUN mkdir /configuration
RUN mkdir /outputs
COPY configuration/parameters.yaml /configuration
COPY data/StockData.RData /data
COPY prediction_api.R /
COPY outputs/best_model.RData /outputs


# Plumb & run server
EXPOSE 8080
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('prediction_api.R'); pr$run(host='0.0.0.0', port=8080)"]