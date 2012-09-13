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
 * - H5FD_DSM_LOCK_SYNCHRONOUS enables client and server to operate
 *   in a "ping-pong" mode (default)
 * - H5FD_DSM_LOCK_ASYNCHRONOUS enables client and server to operate
 *   without having to wait for each other
 * - H5FD_DSM_MODE_SERIAL enables the driver to be used serially
 * - H5FD_DSM_MODE_PARALELL enables the driver to be used in parallel (default)
 *   this should only be used when H5FD_DSM_MODE_SERIAL has been previously called
 */
#define H5FD_DSM_LOCK_SYNCHRONOUS  0x0004
#define H5FD_DSM_LOCK_ASYNCHRONOUS 0x0008
#define H5FD_DSM_MODE_SERIAL       0x0010
#define H5FD_DSM_MODE_PARALLEL     0x0020

/*
 * UnlockNotification information:
 * - H5FD_DSM_NOTIFY_DATA (default notification sent) can be used to signal the presence
 *   of new data
 * - H5FD_DSM_NOTIFY_INFORMATION can be used to signal the presence of new information
 */
#define H5FD_DSM_NOTIFY_NONE         0x0000
#define H5FD_DSM_NOTIFY_DATA         0x0001 /* this is the default */
#define H5FD_DSM_NOTIFY_INFORMATION  0x0002
/* Internal notifications */
#define H5FD_DSM_NOTIFY_WAIT         0x0003
#define H5FD_DSM_NOTIFY_CONNECTED    0x0004
#define H5FD_DSM_NOTIFY_DISCONNECTED 0x0005
/* User notifications */
#define H5FD_DSM_NOTIFY_USER         0x0010 /* user +1, +2 ... etc etc */


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
   * Acquire a lock on the DSM. This prevents any other process from accessing the DSM
   * until the lock is released. Calls to H5FD_dsm_lock must be paired with calls to
   * H5FD_dsm_unlock. Calls to H5FD_dsm_lock and H5FD_dsm_unlock are collective and must
   * be called by all ranks of a given process.
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_lock(void);
  H5FDdsm_EXPORT herr_t H5FD_dsm_unlock(unsigned long flag);

  /*
  H5FDdsm_EXPORT herr_t H5FD_dsm_clear(void);
   */

  /* Description:
   * Set a specific option for the DSM.
   * Options available are:
   *   - H5FD_DSM_UNLOCK_ON_CLOSE
   *   - H5FD_DSM_UNLOCK_MANUAL
   * The default value, (H5FD_DSM_UNLOCK_ON_CLOSE | H5FD_DSM_NOTIFY_ON_CLOSE) 
   * automatically unlocks the DSM and notifies the server when the DSM is closed.
   * If the user requires multiple open/closes during a lock/unlock cycle, then
   * the Manual mode should be set.
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_set_options(unsigned long flags);

  /* Description:
   * (C++ only) Associate an existing DSM manager with the driver.
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
