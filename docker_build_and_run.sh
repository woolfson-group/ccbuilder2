sudo docker stop ccbmk2_web_1 ccbmk2_optimisation_1 ccbmk2_db_1
sudo docker rm ccbmk2_web_1 ccbmk2_optimisation_1 ccbmk2_db_1
sudo docker build -t ccbmk2_web -f web/Dockerfile .
sudo docker build -t ccbmk2_optimisation -f optimisation/Dockerfile .
sudo docker run -d --name ccbmk2_db_1 mongo
sudo docker run -d -p 1801:80 -e CCBMK2_CONFIG=development --link ccbmk2_db_1:db --name ccbmk2_web_1 ccbmk2_web
sudo docker run -d -e PYTHONUNBUFFERED=0 -e OPT_PROCS=4 --link ccbmk2_db_1:db --name ccbmk2_optimisation_1 ccbmk2_optimisation
