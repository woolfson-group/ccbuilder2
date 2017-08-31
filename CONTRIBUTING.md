# Contributing
Contributions of any sort would be greatly appreciated. If you've found a bug or have a feature request, please post an issue here on GitHub. Even better, if you fancy fixing the bug or adding the feature yourself, fork the repository then make a pull request!

## Building and Running the Application Locally
To run the application locally all you need is [Docker](https://www.docker.com/) and [Scwrl4](http://dunbrack.fccc.edu/scwrl4/) (a dependency of ISAMBARD).

1. Download Scwrl4. You'll need the Linux version, regardless of which operating system you plan to run the application on, as the application is ran in a Docker container running Alpine Linux.
1. Place the Scwrl4 source folder inside the `dependencies_for_isambard` folder, make sure the folder is called `scwrl4`. The file structure should look like this: `ccbuilder2/dependencies_for_isambard/scwrl4`.
1. Make sure you're in the `ccbuilder2` folder and type `docker-compose up --build`.
1. If everything ran sucessfully, the application should now be available in your browser [localhost:1801/ccbuilder2](localhost:1801/ccbuilder2).
