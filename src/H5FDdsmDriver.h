/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmDriver.h

  Authors:
     John Biddiscombe     Jerome Soumagne
     biddisco@cscs.ch     soumagne@cscs.ch

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing
  1) This copyright notice appears on all copies of source code
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it
  must not be reformatted such that the indentation, bracketing or
  overall style is modified significantly.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  This work has received funding from the European Community's Seventh
  Framework Programme (FP7/2007-2013) under grant agreement 225967 “NextMuSE”

=========================================================================*/

#ifndef __H5FDdsmDriver_h
#define __H5FDdsmDriver_h

#include <mpi.h>
#include "H5Ipublic.h"
#include "H5FDdsmConfig.h"

/*
   These functions are not for use by user code : they are called by the 'C' api
   and internally use the C++ buffer/driver objects. 
*/

#ifdef __cplusplus
extern "C" {
#endif
  H5FDdsm_EXPORT void   *dsm_get_manager();
  H5FDdsm_EXPORT herr_t  dsm_get_properties(MPI_Comm *intra_comm,
      void **buf_ptr_ptr, size_t *buf_len_ptr);
  H5FDdsm_EXPORT void    dsm_set_manager(void *manager);

  H5FDdsm_EXPORT herr_t  dsm_alloc(MPI_Comm intra_comm, void *buf_ptr, size_t buf_len);
  H5FDdsm_EXPORT herr_t  dsm_free();

  H5FDdsm_EXPORT hbool_t dsm_is_server();
  H5FDdsm_EXPORT hbool_t dsm_is_driver_serial();
  H5FDdsm_EXPORT herr_t  dsm_set_options(unsigned long flags);

  H5FDdsm_EXPORT hbool_t dsm_is_connected();
  H5FDdsm_EXPORT herr_t  dsm_connect();

  H5FDdsm_EXPORT herr_t  dsm_update_entry(haddr_t start, haddr_t end);
  H5FDdsm_EXPORT herr_t  dsm_get_entry(haddr_t *start_ptr, haddr_t *end_ptr);

  H5FDdsm_EXPORT herr_t  dsm_lock();
  H5FDdsm_EXPORT herr_t  dsm_unlock(unsigned long flag);

  H5FDdsm_EXPORT herr_t  dsm_read(haddr_t addr, size_t len, void *buf_ptr);
  H5FDdsm_EXPORT herr_t  dsm_write(haddr_t addr, size_t len, const void *buf_ptr);

#ifdef __cplusplus
}
#endif

#endif // __H5FDdsmDriver_h
