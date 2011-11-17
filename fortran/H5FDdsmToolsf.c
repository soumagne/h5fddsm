/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmToolsf.c

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

#include "H5f90dsmToolsproto.h"
#include "H5FDdsmTools.h"

/*----------------------------------------------------------------------------
 * Name:        h5fd_dsm_dump_c
 * Purpose:     Display the content of the DSM (Debug only)
 * Inputs:      none
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_dump_c()
{
     int    ret_value = -1;
     herr_t ret;

     /*
      * Call H5FD_dsm_dump function.
      */
     ret = H5FD_dsm_dump();
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}
