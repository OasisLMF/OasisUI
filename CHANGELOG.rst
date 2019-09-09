OasisUI Changelog 
==================

.. AUTO_INSERT-CHANGE_LIST  
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


.. AUTO_INSERT-CHANGE_DIFF
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
