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

struct H5FDdsmMsg;
class  H5FDdsmComm;
class  H5FDdsmDriver;
class  H5FDdsmStorage;
class  H5FDdsmAddressMapper;

#define H5FD_DSM_TYPE_UNIFORM       0
#define H5FD_DSM_TYPE_UNIFORM_RANGE 1
#define H5FD_DSM_TYPE_MIXED         2
#define H5FD_DSM_TYPE_BLOCK_CYCLIC  3
#define H5FD_DSM_TYPE_DYNAMIC_MASK  4

#define H5FD_DSM_DEFAULT_LENGTH 10000
#define H5FD_DSM_DEFAULT_BLOCK_LENGTH 1024
#define H5FD_DSM_ALIGNMENT 4096

typedef struct
{
  H5FDdsmAddr start;
  H5FDdsmAddr end;
} H5FDdsmEntry; // 16

typedef struct
{
  H5FDdsmByte  object_names[64*16]; // TODO Size to be better handled
  H5FDdsmInt32 number_of_objects;
} H5FDdsmDisabledObjectEntries;

typedef struct
{
  H5FDdsmEntry entry;
  H5FDdsmByte  steering_cmd[40];
  H5FDdsmDisabledObjectEntries disabled_objects;
  H5FDdsmInt64 unused;
} H5FDdsmMetaData;

class H5FDdsm_EXPORT H5FDdsmDriver : public H5FDdsmObject {

  public:
    H5FDdsmDriver();
    virtual ~H5FDdsmDriver();

    // Type
    H5FDdsmInt32 GetDsmType();
    void SetDsmType(H5FDdsmInt32 DsmType);

    // End Address
    H5FDdsmGetValueMacro(EndAddress, H5FDdsmAddr);
    H5FDdsmSetValueMacro(EndAddress, H5FDdsmAddr);

    // Start Address
    H5FDdsmGetValueMacro(StartAddress, H5FDdsmAddr);
    H5FDdsmSetValueMacro(StartAddress, H5FDdsmAddr);

    // Start Id
    H5FDdsmGetValueMacro(StartServerId, H5FDdsmInt32);
    H5FDdsmSetValueMacro(StartServerId, H5FDdsmInt32);

    // End Id
    H5FDdsmGetValueMacro(EndServerId, H5FDdsmInt32);
    H5FDdsmSetValueMacro(EndServerId, H5FDdsmInt32);

    // Length
    H5FDdsmGetValueMacro(Length, H5FDdsmUInt64);
    H5FDdsmInt32 SetLength(H5FDdsmUInt64 Length, H5FDdsmBoolean AllowAllocate=1);

    // TotalLength
    H5FDdsmGetValueMacro(TotalLength, H5FDdsmUInt64);
    H5FDdsmSetValueMacro(TotalLength, H5FDdsmUInt64);

    // BlockLength
    H5FDdsmGetValueMacro(BlockLength, H5FDdsmUInt64);
    H5FDdsmSetValueMacro(BlockLength, H5FDdsmUInt64);

    // MaskLength
    H5FDdsmGetValueMacro(MaskLength, H5FDdsmUInt64);
    H5FDdsmInt32 SetMaskLength(H5FDdsmUInt64 dataSize);

    // Storage
    H5FDdsmGetValueMacro(Storage, H5FDdsmStorage *);
    H5FDdsmInt32   SetStorage(H5FDdsmStorage *Storage);
    H5FDdsmInt32   ClearStorage();

    // Comm
    H5FDdsmGetValueMacro(Comm, H5FDdsmComm *);
    H5FDdsmSetValueMacro(Comm, H5FDdsmComm *);

    // Configure the system. Set the Comm and ServerIds
    H5FDdsmInt32   ConfigureUniform(H5FDdsmComm *Comm, H5FDdsmUInt64 Length, H5FDdsmInt32 StartId=-1, H5FDdsmInt32 EndId=-1, H5FDdsmUInt64 aBlockLength=0);

    H5FDdsmInt32   ProbeCommandHeader(H5FDdsmInt32 *Source);
    H5FDdsmInt32   SendCommandHeader(H5FDdsmInt32 Opcode, H5FDdsmInt32 Dest, H5FDdsmAddr Address, H5FDdsmInt32 Length);
    H5FDdsmInt32   ReceiveCommandHeader(H5FDdsmInt32 *Opcode, H5FDdsmInt32 *Source, H5FDdsmAddr *Address, H5FDdsmInt32 *Length, H5FDdsmInt32 IsRemoteService=0, H5FDdsmInt32 RemoteSource=-1, H5FDdsmInt32 Block=1);

    // Send/Recv Methods for point-to-point exchange of DSM information between
    // two different jobs/applications
    H5FDdsmInt32   SendInfo();
    H5FDdsmInt32   ReceiveInfo();

    // Send/Recv Mask length (used with H5FD_DSM_TYPE_DYNAMIC_MASK)
    H5FDdsmInt32   SendMaskLength();
    H5FDdsmInt32   ReceiveMaskLength();

    // Send/Recv Methods for point-to-point comm
    H5FDdsmInt32   SendData(H5FDdsmInt32 Dest,H5FDdsmPointer Data, H5FDdsmInt32 Length, H5FDdsmInt32 Tag);
    H5FDdsmInt32   ReceiveData(H5FDdsmInt32 Source, H5FDdsmPointer Data, H5FDdsmInt32 Length, H5FDdsmInt32 Tag, H5FDdsmInt32 Block=1);

    // RMA specific operations
    H5FDdsmInt32   PutData(H5FDdsmInt32 Dest, H5FDdsmPointer Data, H5FDdsmInt32 Length, H5FDdsmAddr Address);
    H5FDdsmInt32   GetData(H5FDdsmInt32 Source, H5FDdsmPointer Data, H5FDdsmInt32 Length, H5FDdsmAddr Address);

    virtual H5FDdsmInt32 Copy(H5FDdsmDriver *Source);

    H5FDdsmInt32   SendDone();

  protected:
    H5FDdsmAddr     EndAddress;
    H5FDdsmAddr     StartAddress;

    H5FDdsmInt32    StartServerId;
    H5FDdsmInt32    EndServerId;

    H5FDdsmUInt64   Length;
    H5FDdsmUInt64   TotalLength;
    H5FDdsmUInt64   BlockLength;
    H5FDdsmUInt64   MaskLength;

    H5FDdsmStorage *Storage;
    H5FDdsmInt32    StorageIsMine;

    H5FDdsmComm    *Comm;

    H5FDdsmInt32   *Locks;
    H5FDdsmByte    *DataPointer;

    H5FDdsmAddressMapper *AddressMapper;
};

#endif // __H5FDdsmDriver_h
