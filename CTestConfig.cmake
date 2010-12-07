SET (CTEST_DROP_SITE "http://svn.cscs.ch:8081")
SET (CTEST_DROP_LOCATION "CSCS-Main")
SET (CTEST_TRIGGER_SITE "http://${CTEST_DROP_SITE}/${CTEST_DROP_LOCATION}")

## This file should be placed in the root directory of your project.
## Then modify the CMakeLists.txt file in the root directory of your
## project to incorporate the testing dashboard.
## # The following are required to uses Dart and the Cdash dashboard
##   ENABLE_TESTING()
##   INCLUDE(Dart)

set(CTEST_PROJECT_NAME "H5FDdsm")
set(CTEST_NIGHTLY_START_TIME "00:00:00 CET")

set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "cdash.cscs.ch")
set(CTEST_DROP_LOCATION "/submit.php?project=H5FDdsm")
set(CTEST_DROP_SITE_CDASH TRUE)