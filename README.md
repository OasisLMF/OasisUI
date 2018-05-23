`Flamingo`
==========

Web-based (Shiny) client for managing exposure data and operating modelling workflows, and a Python Flask server which provides services for interacting with exposure and output data.

# Repository Management

## Cloning the repository

You can clone this repository from GitHub over HTTPS or SSH, but it is recommended that that you use SSH: first ensure that you have generated an SSH key pair on your local machine and add the public key of that pair to your GitHub account (use the GitHub guide at https://help.github.com/articles/connecting-to-github-with-ssh/). Then run

    git clone --recursive git+ssh://git@github.com/OasisLMF/Flamingo

To clone over HTTPS use

    git clone --recursive https://github.com/OasisLMF/Flamingo

You may receive a password prompt - to bypass the password prompt use

    git clone --recursive https://<GitHub user name:GitHub password>@github.com/OasisLMF/Flamingo

The `--recursive` option ensures the cloned repository contains the necessary Oasis repositories <a href="https://github.com/OasisLMF/oasis_utils" target="_blank">`oasis_utils`</a> and <a href="https://github.com/OasisLMF/OasisAPIClient" target="_blank">`OasisAPIClient`</a> as Git submodules.

## Managing the submodules

Run the command

    git submodule

to list the submodules (latest commit IDs, paths and branches). If any are missing then you can add them using

	git submodule add <submodule GitHub repo URL> <local path/destination>

It is a quirk of Git that the first time you clone a repository with submodules they will be checked out as commits not branches, which is not what you want. You should run the command

    git submodule foreach 'git checkout master'

to ensure that the Oasis submodules are checked out on the `master` branches.

If you've already cloned the repository and wish to update the submodules (all at once) in your working directory from their GitHub repositories then run

    git submodule foreach 'git pull origin'

You can also update the submodules individually by pulling from within them.

Unless you've been given read access to this repository on GitHub (via an OasisLMF organizational team) you should not make any local changes to these submodules because you have read-only access to their GitHub repositories. So submodule changes can only propagate from GitHub to your local repository. To detect these changes you can run `git status -uno` and to commit them you can add the paths and commit them in the normal way.

# Sphinx Docs

## Setting up Sphinx

This repository is enabled with <a href="https://pypi.python.org/pypi/Sphinx" target="_blank">Sphinx</a> documentation for the Python modules, and the documentation is published to <a href="https://oasislmf.github.io/OasisLMF/Flamingo/" target="_blank">https://oasislmf.github.io/Flamingo/</a> manually using the procedure described below. (Note: GitHub pages is not enabled for this repository because it contains the private repositories <a href="https://github.com/OasisLMF/oasis_utils" target="_blank">`oasis_utils`</a> and <a href="https://github.com/OasisLMF/OasisAPIClient" target="_blank">`OasisAPIClient`</a> as Git submodules, which is incompatible with GitHub pages.)

Firstly, to work on the Sphinx docs for this package you must have Sphinx installed on your system or in your virtual environment (`virtualenv` is recommended).

You should also clone the Oasis publication repository <a href="https://github.com/OasisLMF/OasisLMF.github.io" target="_blank">OasisLMF.github.io</a>.

## Building and publishing

The Sphinx documentation source files are reStructuredText files, and are contained in the `docs` subfolder, which also contains the Sphinx configuration file `conf.py` and the `Makefile` for the build. To do a new build make sure you are in the `docs` subfolder and run

    make html

You should see a new set of HTML files and assets in the `_build/html` subfolder (the build directory can be changed to `docs` itself in the `Makefile` but that is not recommended). The `docs` subfolder should always contain the latest copy of the built HTML and assets so first copy the files from `_build/html` to `docs` using

    cp -R _build/html/* .

Add and commit these files to the local repository, and then update the remote repository on GitHub. Then copy the same files to the Oasis API client docs static subfolder in the local publication repository

    cp -R _build/html/* /path/to/your/OasisLMF.github.io/Flamingo/

Add and commit these files, and then update the remote publication repository - GitHub pages will automatically publish the new documents to the documentation site https://oasislmf.github.io/Flamingo/.
