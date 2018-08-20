<img src="https://oasislmf.org/packages/oasis_theme_package/themes/oasis_theme/assets/src/oasis-lmf-colour.png" alt="Oasis LMF logo" width="250"/>

[![Build](http://ci.oasislmfdev.org/buildStatus/icon?job=pipeline_stable/oasis_ui)](http://ci.oasislmfdev.org/blue/organizations/jenkins/pipeline_stable%2Foasis_ui)

# Oasis UI
Web-based application client for managing exposure data and operating modelling workflows.There are three components:
  * Shiny UI application 
  * Python Flask server providing services for interacting with exposure and output data
  * SQL server database

## First Steps

### Simple environment

TODO

## Development

### GitFlow

We use the GitFlow model described <A href="http://nvie.com/posts/a-successful-git-branching-model"> here </A>.

The main idea is that the central repo holds two main branches with an infinite lifetime:

* master: main branch where the source code of HEAD always reflects a production-ready state
* develop: main branch where the source code of HEAD always reflects a state with the latest delivered development changes for the next release. This is where our automatic builds are built from.

When the source code in the develop branch reaches a stable point and is ready to be released, all of the changes should be merged back into master. 
Feature branchs should be used for new features and fixes, then a Pull Request isues to merge into develop.

### Dependencies

TODO

### Testing

TODO

### Deploying

The Oasis CI system builds and deploys the following Docker images to DockerHub:

* <a href="https://hub.docker.com/r/coreoasis/flamingo_server">coreoasis/flamingo_server</a>
* <a href="https://hub.docker.com/r/coreoasis/shiny_proxy">coreoasis/shiny_proxy</a>

Note that the Dockerfiles cannot be used directly as there are version stubs that get substitued at build time by the CI system.

TODO DB setup

## Documentation
* <a href="https://github.com/OasisLMF/OasisUI/issues">Issues</a>
* <a href="https://github.com/OasisLMF/OasisUI/releases">Releases</a>
* <a href="https://oasislmf.github.io">General Oasis documentation</a>
* <a href="https://oasislmf.github.io/getting_started/using_the_oasis_ui.html">Using the Oasis UI</a>

## License
The code in this project is licensed under BSD 3-clause license.

