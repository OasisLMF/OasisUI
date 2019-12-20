OasisUI Changelog 
==================

`1.4.1`_
--------
* Replaced mean with quantile at 1/2
* New color scheme for pins
* Added commas in numbers
* Functionality to expand plots to full screen
* Fixed bug in showing map for location.csv in validation panel generated input
* Added code name to drop down in validation map
* Multi-select for perils
* Bug fix in case no peril id is present
* Renamed "mean AAl" to "AAL" in summary tab

`1.4.0`_
--------
* #187 - Updates to plots and reporting 
* #202 - Order files in tables alphabetically
* #205 - UI user guide documentation

`1.3.5`_
--------
* closes #173 - Update Output config view according to previous selection
* closes #179 - Update to analysis settings for leccalc in OasisLMF>=1.4.3 
* closes #183 - Handle API connection error on login
* fixes #185 - Issue with single data file in API model

`1.3.3`_
--------
* Fix for `1.3.2` release

`1.3.2`_
--------
* Closes #166
* Fixes #170
* Fixes #174
* Fixes #176
* Feature #148 Update Output config 
* Closes #148
* Closes #149
* Fixes #154

`1.3.1`_
--------
* Fix for summary_level column selection

`1.3.0`_
--------
* Output configuration update
* Closes #147 Summary Level methods
* UI for output configuration (from #148)
* Closes #164 Improved hazard map performances
* Closes #157 Fixed logs retrieving
* Closes #159 Simplified table views
* Closes #155 Added advanced button for model parameters in output configuration
* Closes #134 Added rescaling of y-axis in output plots
* Closes #126 Integrated more efficient data model and API integration based on R6 classes

`1.2.0`_
--------
* Integrated `data_file` API resource for hazard maps

`1.1.3`_
--------
* Updated icons and fonts 

`1.1.2`_
--------
* Various fixes to exposure validation
* Renamed scope-file endpoint in API

`1.1.1`_
--------
* Updates for Dynamically adding widgets based on model parameters from `model_resources.json`

`1.1.0`_
--------
* Fix for file size upload limit 
* Fix when deleting Portfolios
* Layout and UI improvements 
* Improvement in report outputs
* Added Hazard map for models 
* Added exposure validation on files generation 

`1.0.2`_
--------
* Update and simplify Dockerfile for `oasisui_app` 
* Update for status display of an analysis
* New table headers
* Added Download Button for log files
* Fixed display of folders in files list 

`1.0.1`_
--------
* Update to Summary Output reporting tables 
* Cosmetics updates
* Fix for file names on download 
* Fix model_settings sublevel in analysis_settings.json

`1.0.0`_
--------
* Upgrade to Font Awesome 5 and update Icons 
* Fix for zip download selected files
* Added info column for analyses status 
* Fix for status_detailed ordering


`1.0.0-rc1`_ (Release candidate 1)
--------
* Beta Release of Overhauled UI 
* docker image `flamingo_server` deprecated 
* image rename `shiny_proxy` -> `oasisui_proxy`
* image rename `flamingo_shiny` -> `oasisui_app`
* legacy files removed (MSSQL / Older UI) 


`0.397.0`_ (SQL)
--------
* Minor fixes 
* Update docker files 


`0.395.3`_ (SQL)
--------
* Update schema.sql for RI
* Fixes for RI support 


`0.395.0`_ (SQL)
--------
* Feature - Support for Reinsurance in Flamingo UI 


`0.394.3`_ (SQL)
--------
* Hotfix - Change in keys_server response `coverage` -> `coverage_type`


.. _`1.4.1`:  https://github.com/OasisLMF/OasisUI/compare/1.4.0...1.4.1
.. _`1.4.0`:  https://github.com/OasisLMF/OasisUI/compare/1.3.5...1.4.0
.. _`1.3.5`:  https://github.com/OasisLMF/OasisUI/compare/1.3.3...1.3.5
.. _`1.3.3`:  https://github.com/OasisLMF/OasisUI/compare/1.3.2...1.3.3
.. _`1.3.2`:  https://github.com/OasisLMF/OasisUI/compare/1.3.1...1.3.2
.. _`1.3.1`:  https://github.com/OasisLMF/OasisUI/compare/1.3.0...1.3.1
.. _`1.3.0`:  https://github.com/OasisLMF/OasisUI/compare/1.2.0...1.3.0
.. _`1.2.0`:  https://github.com/OasisLMF/OasisUI/compare/1.1.2...1.2.0
.. _`1.1.2`:  https://github.com/OasisLMF/OasisUI/compare/1.1.1...1.1.2
.. _`1.1.1`:  https://github.com/OasisLMF/OasisUI/compare/1.1.0...1.1.1
.. _`1.1.0`:  https://github.com/OasisLMF/OasisUI/compare/1.0.2...1.1.0
.. _`1.0.2`:  https://github.com/OasisLMF/OasisUI/compare/1.0.1...1.0.2
.. _`1.0.1`:  https://github.com/OasisLMF/OasisUI/compare/1.0.0...1.0.1
.. _`1.0.0`:  https://github.com/OasisLMF/OasisUI/compare/1.0.0-rc1...1.0.0
.. _`1.0.0-rc1`:  https://github.com/OasisLMF/OasisUI/compare/0.397.0...1.0.0-rc1
.. _`0.397.0`:  https://github.com/OasisLMF/OasisUI/compare/0.395.3...0.397.0
.. _`0.395.3`:  https://github.com/OasisLMF/OasisUI/compare/0.395.0...0.395.3
.. _`0.395.0`:  https://github.com/OasisLMF/OasisUI/compare/0.394.3...0.395.0
.. _`0.394.3`:  https://github.com/OasisLMF/OasisUI/compare/0.394.2...0.394.3
