/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5VLdsm.h

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

#ifndef __H5VLdsm_h
#define __H5VLdsm_h

/* H5FDdsm */
#include "H5FDdsmConfig.h"

#define H5VL_DSM	(H5VL_dsm_init())

#ifdef __cplusplus
extern "C" {
#endif

  H5FDdsm_EXPORT H5VL_class_t *H5VL_dsm_init(void);
  H5FDdsm_EXPORT herr_t H5Pset_fapl_dsm_vol(hid_t fapl_id);

#ifdef __cplusplus
}
#endif

#endif
