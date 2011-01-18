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
 * Default memory allocation block size
 */
#define H5FD_DSM_INCREMENT 1000000

/*
 * Specific DSM operating modes
 */
#define H5FD_DSM_AUTOMATIC_SERVER_UPDATE 0x10
#define H5FD_DSM_MANUAL_SERVER_UPDATE    0x20

#define H5FD_DSM_UPDATE_LEVEL_0   0x0
#define H5FD_DSM_UPDATE_LEVEL_1   0x1
#define H5FD_DSM_UPDATE_LEVEL_2   0x2
#define H5FD_DSM_UPDATE_LEVEL_3   0x3
#define H5FD_DSM_UPDATE_LEVEL_4   0x4

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
  H5FDdsm_EXPORT void   H5FD_dsm_term(void);

  /* Description:
   * Set the flags that this VFL driver is capable of supporting.
   * (listed in H5FDpublic.h)
   * Users should never need to call it manually.
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_query(const H5FD_t *_file, unsigned long *flags);

  /* Description:
   * Set a specific operating for the DSM.
   * Modes available are:
   *   - H5FD_DSM_MANUAL_SERVER_UPDATE
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_set_mode(unsigned long flags, void *dsmBuffer);

  /* Description:
   * Switch communicators and send an update ready to the DSM servers.
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_server_update(void *dsmBuffer);

  /* Description:
   * Modify the file access property list to use the H5FDdsm driver defined
   * in this source file.
   * dsmBuffer must be NULL or a pointer to an H5FDdsmBuffer object.
   */
  H5FDdsm_EXPORT herr_t H5Pset_fapl_dsm(hid_t fapl_id, MPI_Comm  dsmComm, void  *dsmBuffer);

  /* Description:
   * Queries properties set by the H5Pset_fapl_dsm() function.
   */
  H5FDdsm_EXPORT herr_t H5Pget_fapl_dsm(hid_t fapl_id, MPI_Comm *dsmComm, void **dsmBuffer);

#ifdef __cplusplus
}
#endif

#endif /* __H5FDdsm_h */
