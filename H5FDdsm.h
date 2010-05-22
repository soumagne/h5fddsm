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
 *    Derived from the "core" and "mpio" drivers and from the H5FDdsm2 dsm driver.
 *
 ========================================================================*/

#ifndef H5FD_DSM_H
#define H5FD_DSM_H

// HDF5
#include "H5Ipublic.h"
#include "H5FDpublic.h"

// H5FDdsm
#include "H5FDdsmConfig.h"

#ifdef H5_HAVE_PARALLEL

  #define H5FD_DSM  (H5FD_dsm_init())

  /* Allocate memory in multiples of this size by default */
  #define H5FD_DSM_INCREMENT 1000000

  #ifdef __cplusplus
    extern "C" {
  #endif

  H5FDdsm_EXPORT hid_t  H5FD_dsm_init(void);
  H5FDdsm_EXPORT void   H5FD_dsm_term(void);
  H5FDdsm_EXPORT herr_t H5FD_dsm_query(const H5FD_t *_file, unsigned long *flags);

  // buffer must be H5FDdsmBuffer object pointer
  H5FDdsm_EXPORT herr_t H5Pset_fapl_dsm(hid_t fapl_id, size_t increment, void *xdmfDsmBuffer);
  H5FDdsm_EXPORT herr_t H5Pget_fapl_dsm(hid_t fapl_id, size_t *increment/*out*/, void **xdmfDsmBuffer/*out*/);

  #ifdef __cplusplus
  }
  #endif

#endif // H5_HAVE_PARALLEL

#endif // H5FD_DSM_H
