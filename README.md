# CCBuilder 2
### An interactive web app for atomistic modelling of coiled coils and collagens
### Version 2.0.0 (29th Aug, 2017), Woolfson Group, University of Bristol

[![Python Version](https://img.shields.io/badge/python-3.5%2C%203.6-lightgrey.svg)](https://woolfson-group.github.io/isambard/)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/woolfson-group/isambard/blob/master/LICENSE.md)

This repository contains the source code for CCBuilder 2. You can access the live application [here](http://coiledcoils.chm.bris.ac.uk/ccbuilder2). It is recommended that you use this version of CCBuilder, but if you do require the previous version, it is still available [here](http://rainbow2.chm.bris.ac.uk/app/cc_builder/).

### Powered by ISAMBARD!
Model building and optimisation in CCBuilder 2.0 is powered by [ISAMBARD](https://github.com/woolfson-group/isambard). If you'd like to perform model building on a larger scale, or if you'd like to build more complex models of coiled coils, collagens or other parameterisable protein folds, you can use ISAMBARD directly on you local computing resource. A range of [documentation and tutorial material](https://woolfson-group.github.io/isambard/index.html) is availble to help you get started.

### Citing CCBuilder 2.0
Any publication arising from use of the ISAMBARD software package should cite the following reference:

[Wood CW and Woolfson DN (2017) CCBuilder 2.0: Powerful and accessible coiled-coil modelling, _Protein Science_. doi: 10.1002/pro.3279](http://dx.doi.org/10.1002/pro.3279)

### Contributing
Contributions of any sort would be greatly appreciated. If you've found a bug or have a feature request, please post an issue here on GitHub. Even better, if you fancy fixing the bug or adding the feature yourself, fork the repository then make a pull request!

#### Building and Running the Application Locally
To run the application locally all you need is [Docker](https://www.docker.com/), the [Elm language toolchain](http://elm-lang.org/) and [Scwrl4](http://dunbrack.fccc.edu/scwrl4/) (a dependency of ISAMBARD).

1. Download Scwrl4. You'll need the Linux version, regardless of which operating system you plan to run the application on, as the application is ran in a Docker container running Alpine Linux.
1. Place the Scwrl4 source folder inside the `dependencies_for_isambard` folder, make sure the folder is called `scwrl4`. The file structure should look like this: `ccbuilder2/dependencies_for_isambard/scwrl4`.
1. Make sure you're in the `ccbuilder2` folder and type `docker-compose up --build`.
1. If everything ran sucessfully, the application should now be available in your browser [localhost:1801/ccbuilder2](localhost:1801/ccbuilder2).

If you make changes to the Elm source code, you need to recompile it using `elm make`. See [here](https://guide.elm-lang.org/install.html) for more details. There is a helper script for building the Elm portion of the web app in `ccbuilder2/web/tools`, run them from inside the web folder.

### Contact Information
If you have any queries about the application, please contact me:

- email: chris.wood@bristol.ac.uk
- twitter: [@ChrisWellsWood](https://twitter.com/chriswellswood)
