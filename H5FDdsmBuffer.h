/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmBuffer.h

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
#ifndef __H5FDdsmBuffer_h
#define __H5FDdsmBuffer_h

#include "H5FDdsmDriver.h"

#define H5FD_DSM_MAX_LOCKS 32

//! Helper for pthread_create()
extern "C" {
H5FDdsm_EXPORT void *
H5FDdsmBufferServiceThread(void *DsmObj);
}

//! Base comm object for Distributed Shared Memory implementation
/*!
*/


class H5FDdsm_EXPORT H5FDdsmBuffer : public H5FDdsmDriver {

  public:
    H5FDdsmBuffer();
    virtual ~H5FDdsmBuffer();

    H5FDdsmInt32   ClearStorage();

    H5FDdsmGetValueMacro(ThreadDsmReady, H5FDdsmInt32);
    H5FDdsmSetValueMacro(ThreadDsmReady, H5FDdsmInt32);

    H5FDdsmGetValueMacro(ServiceThreadUseCopy, H5FDdsmInt32);
    H5FDdsmSetValueMacro(ServiceThreadUseCopy, H5FDdsmInt32);

    // Is the DSMBuffer in server or client mode
    H5FDdsmGetValueMacro(IsServer, bool);
    H5FDdsmSetValueMacro(IsServer, bool);

    // Is the DSMBuffer connected
    H5FDdsmGetValueMacro(IsConnected, bool);
    H5FDdsmSetValueMacro(IsConnected, bool);

    // Is the DSMBuffer ready to update
    H5FDdsmGetValueMacro(IsUpdateReady, bool);
    H5FDdsmSetValueMacro(IsUpdateReady, bool);

    // Is the DSMBuffer open for Read Only operations
    H5FDdsmGetValueMacro(IsReadOnly, bool);
    H5FDdsmSetValueMacro(IsReadOnly, bool);

    H5FDdsmGetStringMacro(XMLDescription);
    H5FDdsmSetStringMacro(XMLDescription);

    H5FDdsmInt32   Put(H5FDdsmInt64 Address, H5FDdsmInt64 Length, void *Data);
    H5FDdsmInt32   Get(H5FDdsmInt64 Address, H5FDdsmInt64 Length, void *Data);

    H5FDdsmInt32   Aquire(H5FDdsmInt64 Index);
    H5FDdsmInt32   Release(H5FDdsmInt64 Index);

    H5FDdsmInt32   RequestRemoteChannel();
    H5FDdsmInt32   RequestLocalChannel();
    H5FDdsmInt32   RequestDisconnection();
    H5FDdsmInt32   RequestXMLExchange();
    H5FDdsmInt32   RequestClearStorage();
    H5FDdsmInt32   RequestPipelineUpdate();

    /*
    H5FDdsmInt32   Copy(H5FDdsmBuffer *Source);
    */
    H5FDdsmInt32   ServiceInit();
    H5FDdsmInt32   ServiceOnce(H5FDdsmInt32 *ReturnOpcode=0);
    H5FDdsmInt32   ServiceUntilIdle(H5FDdsmInt32 *ReturnOpcode=0);
    H5FDdsmInt32   ServiceLoop(H5FDdsmInt32 *ReturnOpcode=0);
    H5FDdsmInt32   Service(H5FDdsmInt32 *ReturnOpcode=0);
    void *      ServiceThread();

  protected:
    volatile H5FDdsmInt32 ThreadDsmReady;
    H5FDdsmInt32 ServiceThreadUseCopy;
    bool IsServer;
    volatile bool IsConnected;
    volatile bool IsUpdateReady;
    bool IsReadOnly;
    H5FDdsmString  XMLDescription;
};

#endif // __H5FDdsmBuffer_h
