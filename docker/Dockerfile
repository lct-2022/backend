FROM svetlyak40wt/lct-2022-base:latest

EXPOSE 8000

COPY . /app

RUN qlot exec ros build \
    /app/app.ros

COPY ./docker/s6-app /etc/s6

ENTRYPOINT ["s6-svscan", "/etc/s6"]
