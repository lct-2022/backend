FROM svetlyak40wt/lct-2022-base:latest

ARG APP
ENV APP=${APP}

COPY . /app

RUN qlot exec ros build \
    /app/scripts/app.ros

COPY ./docker/s6-app /etc/s6

ENTRYPOINT ["s6-svscan", "/etc/s6"]
