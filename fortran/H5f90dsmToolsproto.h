/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5f90dsmToolsproto.h

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

#ifndef __H5f90dsmToolsproto_h
#define __H5f90dsmToolsproto_h

#include "H5f90.h"
#include "H5public.h"
#include "H5FDdsmF90Config.h"

#define nh5fd_dsm_dump_c H5_FC_FUNC_(h5fd_dsm_dump_c, H5FD_DSM_DUMP_C)
H5FDdsmF90_EXPORT int_f nh5fd_dsm_dump_c();

#endif /* __H5f90dsmToolsproto_h */
