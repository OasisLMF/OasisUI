<img src="https://oasislmf.org/packages/oasis_theme_package/themes/oasis_theme/assets/src/oasis-lmf-colour.png" alt="Oasis LMF logo" width="250"/>

[![OasisUI Build](https://github.com/OasisLMF/OasisUI/actions/workflows/build.yml/badge.svg?branch=master&event=push)](https://github.com/OasisLMF/OasisUI/actions/workflows/build.yml)

# Oasis UI
Web-based application client for managing exposure data and operating modelling workflows.There are three components:
  * Shiny UI application (This repository)
  * Python Django server providing services for interacting with exposure and output data
  * PostgreSQL server database

## Usage and documentation
For a detailed guide on using the OasisUI see
* [Oasis_UI_Guide.pdf](documentation/Oasis_UI_Guide.pdf?raw=true)
* [Oasis UI - Youtube Walkthrough](https://www.youtube.com/watch?v=tHRetuhpQzA)

## Deploying

To try out the OasisUI run the docker installation script `./install.sh` from [OasisEvaluation](https://github.com/OasisLMF/OasisEvaluation).
For tutorials on running the Oasis Stack locally see: (The full stack is required for the UI to work)
* [Windows 10 - Installation Guide](https://www.youtube.com/watch?v=SxRt5E-Y5Sw)
* [Linux - Installation Guide](https://www.youtube.com/watch?v=OFLTpGGEM10)


## Testing and development

The script `./run_rstudio.sh` deploys and runs a development version of OasisUI, it runs using an [RStudio Server](https://documentation.dnanexus.com/getting-started/developer-tutorials/web-app-let-tutorials/running-rstudio-server).
1. run the script open the url [http://localhost:8787/](http://localhost:8787/) in a browser.
2. Load the project file
![Load Project](.img/dev_load_project.png?raw=true "Load RStudio Project")
3. From the build menu, select `Clean and Rebuild`
![Build App](.img/dev_build_oasisui.png?raw=true "Build Application")

4. In the R console enter `> oasisui::runOasisui()`, this will run the OasisUI in a new window
![Run App](.img/dev_run_oasisui.png?raw=true "Run Application")

## License
The code in this project is licensed under BSD 3-clause license.

