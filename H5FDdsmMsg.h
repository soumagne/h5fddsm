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

#define H5FD_DSM_DEFAULT_TAG    0x80 // 128
#define H5FD_DSM_COMMAND_TAG    0x81 // 129
#define H5FD_DSM_RESPONSE_TAG   0x82 // 130
#define H5FD_DSM_PUT_DATA_TAG   0x83 // 131
#define H5FD_DSM_GET_DATA_TAG   0x84 // 132
#define H5FD_DSM_EXCHANGE_TAG   0x85 // 133
#define H5FD_DSM_XML_TAG        0x86 // 134

#define H5FD_DSM_ANY_SOURCE     -1

class H5FDdsm_EXPORT H5FDdsmMsg : public H5FDdsmObject {

    public :
        H5FDdsmMsg();
        ~H5FDdsmMsg();

        H5FDdsmSetValueMacro(Source, H5FDdsmInt32);
        H5FDdsmGetValueMacro(Source, H5FDdsmInt32);

        H5FDdsmSetValueMacro(Dest, H5FDdsmInt32);
        H5FDdsmGetValueMacro(Dest, H5FDdsmInt32);

        H5FDdsmSetValueMacro(Tag, H5FDdsmInt32);
        H5FDdsmGetValueMacro(Tag, H5FDdsmInt32);

        H5FDdsmSetValueMacro(Length, int);
        H5FDdsmGetValueMacro(Length, int);

        H5FDdsmSetValueMacro(Data, void *);
        H5FDdsmGetValueMacro(Data, void *);

    H5FDdsmInt32   Source;
    H5FDdsmInt32   Dest;
    H5FDdsmInt32   Tag;
    int         Length;
    void       *Data;
};

#endif // __H5FDdsmMsg_h
