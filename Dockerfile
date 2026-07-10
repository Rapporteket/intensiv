FROM rapporteket/base-r-alpine-latex:main

WORKDIR /app/R

RUN --mount=type=secret,id=github_pat,env=GITHUB_PAT \
    --mount=type=bind,source=.,target=/app/R/pkg \
    R -e "remotes::install_local(path = './pkg')" \
    && R -e "remotes::install_github(\"Rapporteket/rapFigurer\")" \
    && R -e "remotes::install_github(\"Rapporteket/rapbase\", ref = \"main\")" \
    && R -e "library(intensiv)"

EXPOSE 3838

RUN adduser --uid "1000" --disabled-password rapporteket && \
    chown -R 1000:1000 /app/R && \
    chmod -R 755 /app/R
USER rapporteket

CMD ["R", "-e", "options(shiny.port = 3838, shiny.host = \"0.0.0.0\"); intensiv::kjorIntensivApp()"]
