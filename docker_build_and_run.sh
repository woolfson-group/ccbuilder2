sudo docker stop ccbmk2_web_1 ccbmk2_optimisation_1 ccbmk2_db_1
sudo docker rm ccbmk2_web_1 ccbmk2_optimisation_1 ccbmk2_db_1
sudo docker build -t ccbmk2_web -f web/Dockerfile .
sudo docker build -t ccbmk2_optimisation -f optimisation/Dockerfile .
sudo docker run -d --name ccbmk2_db_1 mongo
sudo docker run -d -p 1801:80 -v /fs/users/cw12401/web/ccbuilder2/web:/app -e CCBMK2_CONFIG=development --link ccbmk2_db_1:db --name ccbmk2_web_1 ccbmk2_web
sudo docker run -d -v /fs/users/cw12401/web/ccbuilder2/optimisation/:/app -e PYTHONUNBUFFERED=0 -e OPT_PROCS=4 --link ccbmk2_db_1:db --name ccbmk2_optimisation_1 ccbmk2_optimisation
