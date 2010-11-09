/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmDriver.h

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
#ifndef __H5FDdsmDriver_h
#define __H5FDdsmDriver_h

#include "H5FDdsmObject.h"

#define H5FD_DSM_OPCODE_DONE    0xFF

//! Base comm object for Distributed Shared Memory implementation
/*!
*/

class H5FDdsmMsg;
class H5FDdsmComm;
class H5FDdsmDriver;
class H5FDdsmStorage;

#define H5FD_DSM_TYPE_UNIFORM       0
#define H5FD_DSM_TYPE_UNIFORM_RANGE 1
#define H5FD_DSM_TYPE_MIXED         2

#define H5FD_DSM_DEFAULT_LENGTH 10000

typedef struct
{
  H5FDdsmInt64 start;
  H5FDdsmInt64 end;
} H5FDdsmEntry; // 16

typedef struct
{
  H5FDdsmEntry     entry;
  H5FDdsmByte  steering_cmd[40];
  H5FDdsmInt64 unused;
} H5FDdsmMetaData; // 64

class H5FDdsm_EXPORT H5FDdsmDriver : public H5FDdsmObject {

  public:
    H5FDdsmDriver();
    virtual ~H5FDdsmDriver();

  //! Type
    H5FDdsmGetValueMacro(DsmType, H5FDdsmInt32);
    H5FDdsmSetValueMacro(DsmType, H5FDdsmInt32);

  //! End Address
    H5FDdsmGetValueMacro(EndAddress, H5FDdsmInt64);
    H5FDdsmSetValueMacro(EndAddress, H5FDdsmInt64);

  //! Start Address
    H5FDdsmGetValueMacro(StartAddress, H5FDdsmInt64);
    H5FDdsmSetValueMacro(StartAddress, H5FDdsmInt64);

  //! Start Id
    H5FDdsmGetValueMacro(StartServerId, H5FDdsmInt32);
    H5FDdsmSetValueMacro(StartServerId, H5FDdsmInt32);

  //! End Id
    H5FDdsmGetValueMacro(EndServerId, H5FDdsmInt32);
    H5FDdsmSetValueMacro(EndServerId, H5FDdsmInt32);

  //! Length
    H5FDdsmGetValueMacro(Length, H5FDdsmInt64);
    H5FDdsmInt32 SetLength(H5FDdsmInt64 Length, H5FDdsmBoolean AllowAllocate=1);

  //! totalLength
    H5FDdsmGetValueMacro(TotalLength, H5FDdsmInt64);
    H5FDdsmSetValueMacro(TotalLength, H5FDdsmInt64);

  //! Storage
    H5FDdsmGetValueMacro(Storage, H5FDdsmStorage *);
    H5FDdsmInt32   SetStorage(H5FDdsmStorage *Storage);
    H5FDdsmInt32   ClearStorage();

  //! Comm
    H5FDdsmGetValueMacro(Comm, H5FDdsmComm *);
    H5FDdsmSetValueMacro(Comm, H5FDdsmComm *);

    //! Msg
    H5FDdsmGetValueMacro(Msg, H5FDdsmMsg *);
    H5FDdsmSetValueMacro(Msg, H5FDdsmMsg *);


    //! Address Range
    H5FDdsmInt32 GetAddressRangeForId(H5FDdsmInt32 Id, H5FDdsmInt64 *Start, H5FDdsmInt64 *End);

    //! Configure the system. Set the Comm and ServerIds
    H5FDdsmInt32   ConfigureUniform(H5FDdsmComm *Comm, H5FDdsmInt64 Length, H5FDdsmInt32 StartId=-1, H5FDdsmInt32 EndId=-1);
    
    H5FDdsmInt32   AddressToId(H5FDdsmInt64 Address);

    H5FDdsmInt32   SendCommandHeader(H5FDdsmInt32 Opcode, H5FDdsmInt32 Dest, H5FDdsmInt64 Address, H5FDdsmInt64 Length);
    H5FDdsmInt32   ReceiveCommandHeader(H5FDdsmInt32 *Opcode, H5FDdsmInt32 *Source, H5FDdsmInt64 *Address, H5FDdsmInt64 *Length, H5FDdsmInt32 Block=1);

    H5FDdsmInt32   SendData(H5FDdsmInt32 Dest, void *Data, H5FDdsmInt64 Length, H5FDdsmInt32 Tag, H5FDdsmInt32 IsService=0);
    H5FDdsmInt32   ReceiveData(H5FDdsmInt32 Source, void *Data, H5FDdsmInt64 Length, H5FDdsmInt32 Tag, H5FDdsmInt32 IsService=0, H5FDdsmInt32 Block=1);

    H5FDdsmInt32   PutData(H5FDdsmInt32 Dest, void *Data, H5FDdsmInt64 Length, H5FDdsmInt64 Address);
    H5FDdsmInt32   GetData(H5FDdsmInt32 Source, void *Data, H5FDdsmInt64 Length, H5FDdsmInt64 Address);

    virtual H5FDdsmInt32 Copy(H5FDdsmDriver *Source);

    H5FDdsmInt32   SendDone();

  protected:
    H5FDdsmInt32    DsmType;
    H5FDdsmInt32    StartServerId;
    H5FDdsmInt32    EndServerId;
    H5FDdsmInt32    StorageIsMine;
    H5FDdsmInt64    StartAddress;
    H5FDdsmInt64    EndAddress;
    H5FDdsmInt64    Length;
    H5FDdsmInt64    TotalLength;
    H5FDdsmInt64   *Locks;
    H5FDdsmStorage *Storage;
    H5FDdsmComm    *Comm;
    H5FDdsmMsg     *Msg;
    H5FDdsmMsg     *ServiceMsg;
    H5FDdsmByte    *DataPointer;
};

#endif // __H5FDdsmDriver_h
