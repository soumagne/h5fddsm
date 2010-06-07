#
# Find the HDF5 includes and get all installed hdf5 library settings from
# HDF5-config.cmake file : Requires a CMake installed hdf5-1.8.5 or later 
# for this feature to work
#
# HDF5_CONFIG_FILE - The file that should be included if HDF5_FOUND is true
# HDF5_FOUND       - Do not attempt to use hdf5 if "no" or undefined.
# HDF5_CONFIG_DIR  - Set this before including this module to suggest a place to look

FIND_PATH(HDF5_CONFIG_DIR "HDF5-config.cmake"
  ${HDF5_CONFIG_DIR_HINT}
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
  SET(HDF5_FOUND "YES")
ENDIF(HDF5_INCLUDE_DIR)
