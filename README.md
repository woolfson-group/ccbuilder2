# ccbmk2

## Notes

* `docker-compose up`
* Could I use a type to define the field and then have a single update message for all of the input fields?
* When rebuilding the image, you need to:
    * Rebuild the images using `docker build -t ccbmk2 .`
    * Delete the old `docker-compose` image using `docker-compose rm`
* `docker exec -i -t ccmbmk2_web_1 /bin/bash`
* Coolors
    * https://coolors.co/e6e8e6-ced0ce-9fb8ad-475841-3f403f

## TODO

* Add examples
* Add information overlay
    * Contains scoring information
    * Has tab with history of models built
* Add stats logging with MongoDB
* Independent chains and antiparallels