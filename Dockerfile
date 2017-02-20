FROM python:latest
RUN wget https://github.com/jgm/pandoc/releases/download/1.19.1/pandoc-1.19.1-1-amd64.deb
RUN dpkg -i pandoc-1.19.1-1-amd64.deb
RUN pip install pypandoc
RUN pip install isambard
RUN pip install Flask
COPY . /app
WORKDIR /app
COPY ./dependencies_for_isambard/.isambard_settings /root/
ENTRYPOINT ["python"]
CMD ["run.py"]