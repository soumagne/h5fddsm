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

#ifdef __cplusplus
extern "C" {
#endif
  H5FDdsm_EXPORT hbool_t DsmIsServer();
  H5FDdsm_EXPORT herr_t  DsmSetOptions(unsigned long flags);

  H5FDdsm_EXPORT void   *DsmGetManager();
  H5FDdsm_EXPORT herr_t  DsmSetManager(void *manager);

  H5FDdsm_EXPORT herr_t  DsmAutoAlloc(MPI_Comm comm, MPI_Comm *intra_comm,
      void **buf_ptr_ptr, size_t *buf_len_ptr);
  H5FDdsm_EXPORT herr_t  DsmAutoDealloc();
  H5FDdsm_EXPORT herr_t  DsmGetBuffer(MPI_Comm *intra_comm, void **buf_ptr_ptr,
      size_t *buf_len_ptr);

  H5FDdsm_EXPORT herr_t  DsmConnect();
  H5FDdsm_EXPORT hbool_t DsmIsConnected();

  H5FDdsm_EXPORT herr_t  DsmUpdateEntry(haddr_t start, haddr_t end);
  H5FDdsm_EXPORT herr_t  DsmGetEntry(haddr_t *start_ptr, haddr_t *end_ptr);

  H5FDdsm_EXPORT herr_t  DsmLock();
  H5FDdsm_EXPORT herr_t  DsmUnlock();

  H5FDdsm_EXPORT herr_t  DsmRead(haddr_t addr, size_t len, void *buf_ptr);
  H5FDdsm_EXPORT herr_t  DsmWrite(haddr_t addr, size_t len, const void *buf_ptr);

  H5FDdsm_EXPORT herr_t  DsmSetModified();
  H5FDdsm_EXPORT herr_t  DsmNotify(unsigned long flags);
#ifdef __cplusplus
}
#endif

#endif // __H5FDdsmDriver_h
