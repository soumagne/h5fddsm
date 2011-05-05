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
//
// BlockCyclicAddressMapper breaks accesses into smaller blocks
// which are scattered cyclically over the entire address range to better utilize
// all network links. 
// Warning : Destination Rank Ids are undefined. You must eventually pass 
// the result of this operation through PartitionedAddressMapper
// Warning: The input to (and output from) this address mapper use 64 bit addresses
//
//----------------------------------------------------------------------------
class BlockCyclicAddressMapper : public AddressMapperStrategy {
  public:
    BlockCyclicAddressMapper(
      H5FDdsmUInt64 blockSize, H5FDdsmUInt64 blockSpacing, H5FDdsmUInt64 totalLength) 
      : BlockSize(blockSize), BlockSpacing(blockSpacing), TotalLength(totalLength)
    {
      this->BlocksPerCycle = this->BlockSpacing/this->BlockSize;
      this->NumberOfCycles = this->TotalLength/this->BlockSpacing;
      if (this->NumberOfCycles*this->BlockSpacing!=this->TotalLength) {
        H5FDdsmError("BlockCyclic Addresses not exact multiple of Address range");
      }
    }
    //
    virtual H5FDdsmInt32 Translate(
      std::vector<H5FDdsmMsg> &indataRequests, 
      std::vector<H5FDdsmMsg> &outdataRequests);
    //
    H5FDdsmUInt64 BlockSize;
    H5FDdsmUInt64 BlockSpacing;
    H5FDdsmUInt64 TotalLength;
    H5FDdsmUInt64 BlocksPerCycle;
    H5FDdsmUInt64 NumberOfCycles;
};
//----------------------------------------------------------------------------
//
// IntegerSizedAddressMapper breaks accesses into int32 sized (0x7FFF)
// accesses if they exceed this size. MPI does not currently (as of 2011)
// allow MPI_Send or other read/writes larger than 32 bit.
// Warning: The input to this address mapper uses 64 bit addresses
// but the output uses 32 bit addresses
//
//----------------------------------------------------------------------------
class IntegerSizedAddressMapper : public AddressMapperStrategy {
  public:
    IntegerSizedAddressMapper() {}
    //
    virtual H5FDdsmInt32 Translate(
      std::vector<H5FDdsmMsg> &indataRequests, 
      std::vector<H5FDdsmMsg> &outdataRequests);
};
//----------------------------------------------------------------------------
//
// PartitionedAddressMapper maps addresses into DSM rank/address
// pairs and breaks packets that cross process boundaries into
// multiple accesses.
// Warning: The input (and output) are 32bit addresses
// Warning: his implementation assumes all processes have the same local buffer
// size. Future improvements would allow some nodes to have larger/smaller buffers
//
//----------------------------------------------------------------------------
class PartitionedAddressMapper : public AddressMapperStrategy {
  public:
    PartitionedAddressMapper(H5FDdsmUInt64 bufferSize) : BufferSize(bufferSize) {}
    //
    virtual H5FDdsmInt32 Translate(
      std::vector<H5FDdsmMsg> &indataRequests, 
      std::vector<H5FDdsmMsg> &outdataRequests);
    //
    H5FDdsmUInt64 BufferSize;
};

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::H5FDdsmAddressMapper()
{
  this->DsmType = H5FD_DSM_TYPE_UNIFORM;
  this->DsmDriver = NULL;
  this->AddressingStrategy = NULL;
}

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::H5FDdsmAddressMapper(H5FDdsmDriver *dsmDriver)
{
  this->DsmType = H5FD_DSM_TYPE_UNIFORM;
  this->DsmDriver = dsmDriver;
  this->AddressingStrategy = NULL;
}

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::~H5FDdsmAddressMapper()
{
  delete this->AddressingStrategy;
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
H5FDdsmAddressMapper::Translate(H5FDdsmAddr address, H5FDdsmUInt64 length, H5FDdsmPointer data, std::vector<H5FDdsmMsg> &outRequests)
{
  if (!this->AddressingStrategy) {
    std::cout <<       
      this->DsmDriver->GetBlockLength() << " " << 
      this->DsmDriver->GetLength() << " " << 
      this->DsmDriver->GetTotalLength() << " " << std::endl;

    //
    // Operation order is : Cyclic->IntSized->RemoteAddress
    //
    this->AddressingStrategy = new PartitionedAddressMapper(
      this->DsmDriver->GetLength()
    );
    AddressMapperStrategy *LastStrategy = this->AddressingStrategy;
    //
    if (0 /* this->dsmCommunicator->Has32BitLimit() */) {
      IntegerSizedAddressMapper *ISAM = new IntegerSizedAddressMapper();
      LastStrategy->SetDelegate(ISAM);
      LastStrategy = ISAM;
    }
    //
    if (0 && this->DsmType == H5FD_DSM_TYPE_BLOCK_CYCLIC) {
      BlockCyclicAddressMapper *BCAM = new BlockCyclicAddressMapper(
        this->DsmDriver->GetBlockLength(),
        this->DsmDriver->GetLength(),
        this->DsmDriver->GetTotalLength()
      );
      LastStrategy->SetDelegate(BCAM);
      LastStrategy = BCAM;
    }
  }
  //
  H5FDdsmMsg dataRequest;
  dataRequest.Dest     =-1;
  dataRequest.Length   = length;
  dataRequest.Length64 = length;
  dataRequest.Address  = address;
  dataRequest.Data     = (H5FDdsmByte*)data;
  //
  std::vector<H5FDdsmMsg> tempRequest;
  tempRequest.push_back(dataRequest);
  //
  this->AddressingStrategy->Translate(tempRequest,outRequests);
  //
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 BlockCyclicAddressMapper::Translate(
  std::vector<H5FDdsmMsg> &indataRequests, std::vector<H5FDdsmMsg> &outdataRequests)
{
  std::vector<H5FDdsmMsg> &requests = indataRequests; // reference
  std::vector<H5FDdsmMsg> tempRequests;
  if (this->Delegate) {
    if (this->Delegate->Translate(indataRequests, tempRequests)!=H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    requests = tempRequests;
  }
  // we distribute 0123456789ABCDEF as follows
  //
  // if BlockSize=1(MB eg), BlockSpacing=8, {TotalSize=0x10}
  //      bindex = 0123456789ABCDEF
  //      offset = 0011223344556677
  //      period = 0101010101010101
  // final layout becomes ...
  // -------------------------------------
  // | 0 2 4 6 8 A C E . 1 3 5 7 9 B D F |
  // -------------------------------------
  //
  // if BlockSize=1(MB eg), BlockSpacing=4, {TotalSize=0x10}
  //      bindex = 0123456789ABCDEF
  //      offset = 0000111122223333
  //      period = 0123012301230123
  // final layout becomes ...
  // -----------------------------------------
  // | 0 4 8 C . 1 5 9 D . 2 6 A E . 3 7 B F |
  // -----------------------------------------
  //
  for(std::vector<H5FDdsmMsg>::iterator it=requests.begin(); it!=requests.end(); ++it) {
    H5FDdsmByte *datap = (H5FDdsmByte*)it->Data;
    while (it->Length64>0) {
      H5FDdsmMsg newRequest;
      H5FDdsmUInt64 bindex = it->Address/this->BlockSize;
      H5FDdsmUInt64 offset = bindex%this->BlocksPerCycle;
      H5FDdsmUInt64 period = bindex%this->NumberOfCycles;
      newRequest.Dest      =-1;
      newRequest.Length    = 0;
      newRequest.Length64  = (it->Length64>this->BlockSize) ? this->BlockSize : it->Length64;
      newRequest.Address   = period*this->BlockSpacing + offset*this->BlockSize + it->Address%this->BlockSize;
      newRequest.Data      = datap;
      outdataRequests.push_back(newRequest);
      //
      datap        += newRequest.Length64;
      it->Address  += newRequest.Length64;
      it->Length64 -= newRequest.Length64;
    }
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
IntegerSizedAddressMapper::Translate(
  std::vector<H5FDdsmMsg> &indataRequests, std::vector<H5FDdsmMsg> &outdataRequests)
{
  std::vector<H5FDdsmMsg> &requests = indataRequests; // reference
  std::vector<H5FDdsmMsg> tempRequests;
  if (this->Delegate) {
    if (this->Delegate->Translate(indataRequests, tempRequests)!=H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    requests = tempRequests;
  }
  //
  for(std::vector<H5FDdsmMsg>::iterator it=requests.begin(); it!=requests.end(); ++it) {
    H5FDdsmByte *datap = (H5FDdsmByte*)it->Data;
    while (it->Length64>0) {
      H5FDdsmMsg newRequest;
      newRequest.Dest     =-1;
      newRequest.Length   = (it->Length64>H5FD_DSM_INT32_MAX) ? H5FD_DSM_INT32_MAX : (H5FDdsmInt32)it->Length64;
      newRequest.Address  = it->Address;
      newRequest.Data     = datap;
      outdataRequests.push_back(newRequest);
      //
      datap        += newRequest.Length;
      it->Address  += newRequest.Length;
      it->Length64 -= newRequest.Length;
    }
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 
PartitionedAddressMapper::Translate(
  std::vector<H5FDdsmMsg> &indataRequests, std::vector<H5FDdsmMsg> &outdataRequests)
{
  std::vector<H5FDdsmMsg> &requests = indataRequests; // reference
  std::vector<H5FDdsmMsg> tempRequests;
  if (this->Delegate) {
    if (this->Delegate->Translate(indataRequests, tempRequests)!=H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    requests = tempRequests;
  }
  //
  for(std::vector<H5FDdsmMsg>::iterator it=requests.begin(); it!=requests.end(); ++it) {
    H5FDdsmByte *datap = (H5FDdsmByte*)it->Data;
    while (it->Length>0) {
      H5FDdsmMsg newRequest;
      newRequest.Dest     = (H5FDdsmInt32)(it->Address/this->BufferSize);
      newRequest.Address  = it->Address%this->BufferSize;
      if ((newRequest.Address+it->Length)>this->BufferSize) {
        newRequest.Length = (H5FDdsmInt32)(this->BufferSize-newRequest.Address);
      }
      else {
        newRequest.Length = it->Length;
      }
      newRequest.Data = datap;
      outdataRequests.push_back(newRequest);
      //
      datap       += newRequest.Length;
      it->Address += newRequest.Length;
      it->Length  -= newRequest.Length;
    }
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
/*
      if (newRequest.Length<0) {
        std::cout           
          << "Address   " << it->Address << "\n" 
          << "Length    " << it->Length  << "\n" 
          << "Address   " << newRequest.Address << "\n" 
          << "Length    " << newRequest.Length << "\n" 
          << "remaining "  << "\n"; 
        
        newRequest.Length = 87654321;
      }
*/