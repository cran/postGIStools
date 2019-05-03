postGIStools 0.2.3
==================

* Correct issue with package failing in a Latin-1 locale.

postGIStools 0.2.2
==================

* Note that package is deprecated in favor of sf.

postGIStools 0.2.1
==================

* Fixed bug that caused an error when trying to insert/update data with
a single factor column. (#6)


postGIStools 0.2.0
==================

* Updated `get_postgis_query` function, which can now process query statements with wildcards or derived spatial columns. The new version also provides a modest speedup for the conversion to `Spatial*` objects. (#4, @richardellison)


postGIStools 0.1.0
==================

* First release on CRAN
