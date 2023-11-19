FROM tiangolo/uwsgi-nginx-flask:python3.6
WORKDIR /
RUN wget https://github.com/jgm/pandoc/releases/download/1.19.1/pandoc-1.19.1-1-amd64.deb
RUN dpkg -i pandoc-1.19.1-1-amd64.deb
# Setup ISAMBARD
RUN git clone https://github.com/woolfson-group/isambard.git
WORKDIR /isambard
RUN pip install ipython
RUN pip install jupyter
RUN pip install -r ./requirements.txt
RUN python setup.py install
# Setup CCBMK2
WORKDIR /app
# COPY ./web/nginx.conf /etc/nginx/nginx.conf
COPY ./web/requirements.txt ./
RUN pip install -r ./requirements.txt
COPY ./dependencies_for_isambard/ /dependencies_for_isambard/
COPY ./dependencies_for_isambard/.isambard_settings /root/
COPY ./web/ /app/
