FROM rocker/geospatial

# Install renv globally
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

# Set working directory inside container
WORKDIR /home/rstudio/DiabetesML

# Copy only lockfile and profile (not renv/library)
COPY renv.lock ./
COPY .Rprofile ./

# Fix permissions (not strictly needed for rstudio user but good practice)
RUN chown -R rstudio:rstudio /home/rstudio

# Default CMD just runs the container; RStudio takes care of launching
CMD ["/init"]
