/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmMsg.h

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
#ifndef __H5FDdsmMsg_h
#define __H5FDdsmMsg_h

#include "H5FDdsmObject.h"


//! Base comm message object for Distributed Shared Memory implementation
/*!
*/

// If you add a new Tag here, it must also be added to H5FDdsmMsg.cxx
// So that debug Tag->String conversion functions correctly
#define H5FD_DSM_DEFAULT_TAG    0x80
#define H5FD_DSM_COMMAND_TAG    0x81
#define H5FD_DSM_SERVER_ACK_TAG 0x82
#define H5FD_DSM_CLIENT_ACK_TAG 0x83
#define H5FD_DSM_PUT_DATA_TAG   0x84
#define H5FD_DSM_GET_DATA_TAG   0x85
#define H5FD_DSM_EXCHANGE_TAG   0x86

#define H5FD_DSM_ANY_TAG        -1
#define H5FD_DSM_ANY_SOURCE     -2

H5VLdso_EXPORT const char *H5FDdsmTagToString(int tag);

struct H5VLdso_EXPORT H5FDdsmMsg : public H5FDdsmObject {

  public :
     H5FDdsmMsg();
    ~H5FDdsmMsg();

    H5FDdsmSetValueMacro(Source, H5FDdsmInt32);
    H5FDdsmGetValueMacro(Source, H5FDdsmInt32);

    H5FDdsmSetValueMacro(Dest, H5FDdsmInt32);
    H5FDdsmGetValueMacro(Dest, H5FDdsmInt32);

    H5FDdsmSetValueMacro(Tag, H5FDdsmInt32);
    H5FDdsmGetValueMacro(Tag, H5FDdsmInt32);

    H5FDdsmSetValueMacro(Address, H5FDdsmAddr);
    H5FDdsmGetValueMacro(Address, H5FDdsmAddr);

    H5FDdsmSetValueMacro(Length, H5FDdsmInt32);
    H5FDdsmGetValueMacro(Length, H5FDdsmInt32);

    H5FDdsmSetValueMacro(Length64, H5FDdsmUInt64);
    H5FDdsmGetValueMacro(Length64, H5FDdsmUInt64);

    H5FDdsmSetValueMacro(Data, H5FDdsmPointer);
    H5FDdsmGetValueMacro(Data, H5FDdsmPointer);

    H5FDdsmSetValueMacro(Communicator, H5FDdsmInt32);
    H5FDdsmGetValueMacro(Communicator, H5FDdsmInt32);

    H5FDdsmInt32   Source;
    H5FDdsmInt32   Dest;
    H5FDdsmInt32   Tag;
    H5FDdsmAddr    Address;
    H5FDdsmInt32   Length;
    H5FDdsmUInt64  Length64;
    H5FDdsmPointer Data;
    H5FDdsmInt32   Communicator;
};

#endif // __H5FDdsmMsg_h
