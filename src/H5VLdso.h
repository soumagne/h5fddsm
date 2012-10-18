/*=========================================================================

  Project                 : H5VLdso
  Module                  : H5VLdso.h

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

#ifndef __H5VLdso_h
#define __H5VLdso_h

/* HDF5 */
#include "H5Ipublic.h"
#include "H5VLpublic.h"

/* H5VLdso */
#include "H5VLdsoConfig.h"

#define H5VL_DSO	(H5VL_dso_init())

#ifdef __cplusplus
extern "C" {
#endif

  H5VLdso_EXPORT H5VL_class_t *H5VL_dso_init(void);
  H5VLdso_EXPORT herr_t H5Pset_fapl_dso(hid_t fapl_id);

#ifdef __cplusplus
}
#endif

#endif