/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsm.h

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

/*=========================================================================
  This code is derived from an earlier work and is distributed
  with permission from, and thanks to ...
=========================================================================*/

/*******************************************************************/
/*                               XDMF                              */
/*                   eXtensible Data Model and Format              */
/*                                                                 */
/*                                                                 */
/*  Author:                                                        */
/*     Jerry A. Clarke                                             */
/*     clarke@arl.army.mil                                         */
/*     US Army Research Laboratory                                 */
/*     Aberdeen Proving Ground, MD                                 */
/*                                                                 */
/*     Copyright @ 2007 US Army Research Laboratory                */
/*     All Rights Reserved                                         */
/*     See Copyright.txt or http://www.arl.hpc.mil/ice for details */
/*                                                                 */
/*     This software is distributed WITHOUT ANY WARRANTY; without  */
/*     even the implied warranty of MERCHANTABILITY or FITNESS     */
/*     FOR A PARTICULAR PURPOSE.  See the above copyright notice   */
/*     for more information.                                       */
/*                                                                 */
/*******************************************************************/

/*========================================================================
 *
 * Purpose:  A driver which stores the HDF5 data in DSM using
 *    the HDF5 public API and H5FDdsm related classes.
 *    This driver is useful for distributed parallel access to hdf5 files.
 *    Derived from the "core" and "mpio" drivers.
 *
 ========================================================================*/

#ifndef __H5FDdsm_h
#define __H5FDdsm_h

/* HDF5 */
#include "H5Ipublic.h"
#include "H5FDpublic.h"

/* H5FDdsm */
#include "H5FDdsmConfig.h"

/*
 * Disallow any non parallel build
 */
#ifndef H5_HAVE_PARALLEL
  #pragma Error : The H5FDdsm virtual File Driver for HDF5 can only be compiled against an HDF5 library with parallel IO support
#endif

/*
 * Default init function macro using style of other HDF5 VFDs
 */
#define H5FD_DSM (H5FD_dsm_init())

/*
 * Specific DSM operating modes:
 * - H5FD_DSM_DONT_RELEASE prevents the DSM to automatically release the file on a close
 * - H5FD_DSM_DONT_NOTIFY prevents the DSM server to be automatically notified on a close
 * - H5FD_DSM_MODE_SERIAL enables the driver to be used serially
 * - H5FD_DSM_MODE_PARALELL enables the driver to be used in parallel (default)
 *   this should only be used when H5FD_DSM_MODE_SERIAL has been previously called
 */
#define H5FD_DSM_DONT_RELEASE     0x20
#define H5FD_DSM_DONT_NOTIFY      0x21
#define H5FD_DSM_MODE_SERIAL      0x22
#define H5FD_DSM_MODE_PARALLEL    0x23

/*
 * Notification information:
 * - H5FD_DSM_NEW_DATA (default notification sent) can be used to signal the presence
 *   of new data
 * - H5FD_DSM_NEW_INFORMATION can be used to signal the presence of new information
 */
#define H5FD_DSM_NEW_DATA         0x0 /* Keep this value to 0 (default initialization) */
#define H5FD_DSM_NEW_INFORMATION  0x1

/* Internal Use */
#define H5FD_DSM_WAIT             0x10

#ifdef __cplusplus
extern "C" {
#endif
  /* Description:
   * Initialize this driver by registering the driver with the library.
   * Users should never need to call it manually.
   */
  H5FDdsm_EXPORT hid_t  H5FD_dsm_init(void);

  /* Description:
   * Shut down the driver.
   * Users should never need to call it manually.
   */
#if H5_VERSION_GE(1,9,0)
  H5FDdsm_EXPORT herr_t H5FD_dsm_term(void);
#else
  H5FDdsm_EXPORT void   H5FD_dsm_term(void);
#endif

  /* Description:
   * Set a specific option to the DSM.
   * Options available are:
   *   - H5FD_DSM_DONT_RELEASE
   *   - H5FD_DSM_DONT_NOTIFY
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_set_options(unsigned long flags);

  /* Description:
   * Send a notification to the DSM host.
   * Notifications available are:
   *   - H5FD_DSM_NEW_DATA
   *   - H5FD_DSM_NEW_INFORMATION
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_notify(unsigned long flags);

  /* Description:
   * (C++ only) Associate an existing DSM manager to the driver.
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_set_manager(void *manager);

  /* Description:
   * Modify the file access property list to use the H5FDdsm driver defined
   * in this source file.
   * If local_buf_ptr is NULL, the local memory buffer will be automatically
   * allocated or (C++ only) be used from an existing H5FDdsmBuffer object
   * using H5FD_dsm_set_manager().
   */
  H5FDdsm_EXPORT herr_t H5Pset_fapl_dsm(hid_t fapl_id, MPI_Comm intra_comm,
      void *local_buf_ptr, size_t local_buf_len);

  /* Description:
   * Query properties set by the H5Pset_fapl_dsm() function.
   */
  H5FDdsm_EXPORT herr_t H5Pget_fapl_dsm(hid_t fapl_id, MPI_Comm *intra_comm /* out */,
      void **local_buf_ptr_ptr /* out */, size_t *local_buf_len_ptr /* out */);

#ifdef __cplusplus
}
#endif

#endif /* __H5FDdsm_h */
