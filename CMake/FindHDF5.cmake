#
# Find the native HDF5 includes and library
# This module assumes that hdf5-1.8.5 or later has been installed using CMake
# and we therefore pick up the HDF5-config.cmake which contains all we need
#
# HDF5_CONFIG_FILE - The file that should be included if HDF5_FOUND is true
# HDF5_FOUND       - Do not attempt to use hdf5 if "no" or undefined.

FIND_PATH(HDF5_CONFIG_DIR "HDF5-config.cmake"
  /usr/local/lib
  /usr/local/lib64
  /usr/lib
  /usr/lib64
  "C:/Program Files/HDF5/lib"
)

FIND_PATH(HDF5_INCLUDE_DIR "H5public.h"
  ${HDF5_INSTALL_DIR}/../include
)

IF(HDF5_INCLUDE_DIR)
  SET(HDF5_CONFIG_FILE "${HDF5_CONFIG_DIR}/HDF5-config.cmake")
  SET( HDF5_FOUND "YES" )
ENDIF(HDF5_INCLUDE_DIR)
