FROM svetlyak40wt/app-back:latest

EXPOSE 8000

ENTRYPOINT ["s6-svscan", "/etc/s6"]
