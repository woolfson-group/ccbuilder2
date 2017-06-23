FROM python:3.5
RUN wget https://github.com/jgm/pandoc/releases/download/1.19.1/pandoc-1.19.1-1-amd64.deb
RUN dpkg -i pandoc-1.19.1-1-amd64.deb
COPY ./optimisation/requirements.txt .
RUN pip install -r ./requirements.txt
COPY ./dependencies_for_isambard/ /dependencies_for_isambard/
COPY ./dependencies_for_isambard/.isambard_settings /root/
COPY ./web/ccbmk2/database.py /ccbmk2/
COPY ./web/ccbmk2/model_building.py /ccbmk2/
ENV PYTHONPATH=${PYTHONPATH}:/ccbmk2
COPY ./optimisation/ /app/
WORKDIR /app
ENTRYPOINT ["python", "opt_manager.py"]