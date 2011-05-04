/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmAddressMapper.cxx

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

#include "H5FDdsmAddressMapper.h"
#include "H5FDdsmDriver.h"
#include "H5FDdsmMsg.h"

#ifndef NOMINMAX
#ifndef min
#define min(a,b)  (((a) < (b)) ? (a) : (b))
#endif
#endif  /* NOMINMAX for _WIN32 compatibility */

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::H5FDdsmAddressMapper()
{
  this->DsmType = H5FD_DSM_TYPE_UNIFORM;
  this->DsmDriver = NULL;
}

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::H5FDdsmAddressMapper(H5FDdsmDriver *dsmDriver)
{
  this->DsmType = H5FD_DSM_TYPE_UNIFORM;
  this->DsmDriver = dsmDriver;
}

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::~H5FDdsmAddressMapper()
{
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmAddressMapper::GetAddressRangeForId(H5FDdsmInt32 Id, H5FDdsmAddr *Start, H5FDdsmAddr *End, H5FDdsmAddr Address)
{
  switch (this->DsmType) {
  case H5FD_DSM_TYPE_UNIFORM:
  case H5FD_DSM_TYPE_UNIFORM_RANGE:
  case H5FD_DSM_TYPE_DYNAMIC_MASK:
    // All Servers have same length
    *Start = (Id - this->DsmDriver->GetStartServerId()) * this->DsmDriver->GetLength();
    *End = *Start + this->DsmDriver->GetLength() - 1;
    break;
  case H5FD_DSM_TYPE_BLOCK_CYCLIC:
    *Start = ((H5FDdsmInt32)(Address / this->DsmDriver->GetBlockLength())) * this->DsmDriver->GetBlockLength();
    *End = ((H5FDdsmInt32)(Address / this->DsmDriver->GetBlockLength()) + 1) * this->DsmDriver->GetBlockLength() - 1;
    break;
  default:
    // Not Implemented
    H5FDdsmError("DsmType " << this->DsmType << " not yet implemented");
    return(H5FD_DSM_FAIL);
    break;
  }
  H5FDdsmDebug("Address Range for id" << Id << ": start = " << *Start << " , end = " << *End);
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmAddressMapper::AddressToId(H5FDdsmAddr Address)
{
  H5FDdsmInt32 ServerId = H5FD_DSM_FAIL;

  switch(this->DsmType) {
  case H5FD_DSM_TYPE_UNIFORM:
  case H5FD_DSM_TYPE_UNIFORM_RANGE:
  case H5FD_DSM_TYPE_DYNAMIC_MASK:
    // All Servers have same length
    ServerId = this->DsmDriver->GetStartServerId() + (H5FDdsmInt32)(Address / this->DsmDriver->GetLength());
    if(ServerId > this->DsmDriver->GetEndServerId()) {
      H5FDdsmError("ServerId " << ServerId << " for Address " << Address << " is larger than EndServerId " << this->DsmDriver->GetEndServerId());
    }
    break;
  case H5FD_DSM_TYPE_BLOCK_CYCLIC:
    // Keep a uniform DSM but add block cyclic distribution
    ServerId = this->DsmDriver->GetStartServerId() + ((H5FDdsmInt32)(Address / this->DsmDriver->GetBlockLength())
        % (this->DsmDriver->GetEndServerId() - this->DsmDriver->GetStartServerId() + 1));
    break;
  default:
    // Not Implemented
    H5FDdsmError("DsmType " << this->DsmType << " not yet implemented");
    break;
  }
  return(ServerId);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmAddressMapper::Translate(H5FDdsmAddr address, H5FDdsmUInt64 length, H5FDdsmPointer data, std::vector<H5FDdsmMsg*> &dataRequests)
{
  H5FDdsmAddr    astart, aend;
  H5FDdsmInt32   len;
  H5FDdsmInt32   dest;
  H5FDdsmUInt64  ulen;
  H5FDdsmByte   *datap = (H5FDdsmByte *)data;

  while (length) {
    H5FDdsmMsg *dataRequest = new H5FDdsmMsg;
    dest = this->AddressToId(address);
    if (dest == H5FD_DSM_FAIL) {
      H5FDdsmError("Address Error");
      return(H5FD_DSM_FAIL);
    }
    this->GetAddressRangeForId(dest, &astart, &aend, address);
    ulen = min(length, aend - address + 1);
    // Because MPI uses only send/recv int size packets
    len = static_cast<H5FDdsmInt32> min(H5FD_DSM_INT32_MAX, ulen);
    dataRequest->Data = datap;
    dataRequest->Length = len;
    dataRequest->Dest = dest;
    if (this->DsmType == H5FD_DSM_TYPE_BLOCK_CYCLIC) {
      H5FDdsmInt32 blockNumber = (H5FDdsmInt32) (address / ((this->DsmDriver->GetEndServerId() - this->DsmDriver->GetStartServerId() + 1) * this->DsmDriver->GetBlockLength()));
      H5FDdsmDebug("Block number=" << blockNumber << " on dest=" << dest);
      dataRequest->Address = address - astart + blockNumber * this->DsmDriver->GetBlockLength();
    } else {
      dataRequest->Address = address - astart;
    }
    dataRequests.push_back(dataRequest);
    address += len;
    length -= len;
    datap += len;
  }

  return(H5FD_DSM_SUCCESS);
}
