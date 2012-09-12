/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmMsg.cxx

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
#include "H5FDdsmMsg.h"

H5FDdsmMsg::H5FDdsmMsg() {
  this->Source = this->Dest = 0;
  this->Tag = H5FD_DSM_DEFAULT_TAG;
  this->Address = 0;
  this->Length = 0;
  this->Length64 = 0;
  this->Data = NULL;
}

H5FDdsmMsg::~H5FDdsmMsg() {
}

//------------------------------------------------------------------------------
// Utility function to return a string for a given tag
//------------------------------------------------------------------------------
#define H5FD_DSM_TAG_MACRO(def, value) \
  if (value==def) return #def;

const char *H5FDdsmTagToString(int tag) {
  H5FD_DSM_TAG_MACRO(H5FD_DSM_ANY_TAG, tag)
  H5FD_DSM_TAG_MACRO(H5FD_DSM_DEFAULT_TAG, tag)
  H5FD_DSM_TAG_MACRO(H5FD_DSM_COMMAND_TAG, tag)
  H5FD_DSM_TAG_MACRO(H5FD_DSM_SERVER_ACK_TAG, tag)
  H5FD_DSM_TAG_MACRO(H5FD_DSM_CLIENT_ACK_TAG, tag)
  H5FD_DSM_TAG_MACRO(H5FD_DSM_PUT_DATA_TAG, tag)
  H5FD_DSM_TAG_MACRO(H5FD_DSM_GET_DATA_TAG, tag)
  H5FD_DSM_TAG_MACRO(H5FD_DSM_EXCHANGE_TAG, tag)
  return "TAG UNDEFINED/UNRECOGNIZED";
}

