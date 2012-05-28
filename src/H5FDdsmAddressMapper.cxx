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
#include "H5FDdsmBuffer.h"
#include "H5FDdsmMsg.h"
#include <algorithm>

#ifndef NOMINMAX
#ifndef min
#define min(a,b)  (((a) < (b)) ? (a) : (b))
#endif
#endif  /* NOMINMAX for _WIN32 compatibility */

//#undef H5FDdsmDebug
//#define H5FDdsmDebug(x) std::cout << x <<std::endl;
//----------------------------------------------------------------------------
// BlockCyclicAddressMapper breaks accesses into smaller blocks
// which are scattered cyclically over the entire address range to better utilize
// all network links. 
// Warning : Destination Rank Ids are undefined. You must eventually pass 
// the result of this operation through PartitionedAddressMapper
// Warning: The input to (and output from) this address mapper use 64 bit addresses
//----------------------------------------------------------------------------
class BlockCyclicAddressMapper : public AddressMapperStrategy {
  public:
    BlockCyclicAddressMapper(
      H5FDdsmUInt64 blockSize, H5FDdsmUInt64 blockSpacing, H5FDdsmUInt64 totalLength) 
      : BlockSize(blockSize), BlockSpacing(blockSpacing), TotalLength(totalLength)
    {
      this->BlocksPerCycle = this->BlockSpacing / this->BlockSize;
      this->NumberOfCycles = this->TotalLength / this->BlockSpacing;
      if (this->NumberOfCycles * this->BlockSpacing != this->TotalLength ||
          this->NumberOfCycles * this->BlocksPerCycle * this->BlockSize != this->TotalLength)
      {
        H5FDdsmError("BlockCyclic Addresses not exact multiple of Address range");
      }
      //
      H5FDdsmDebug(
          "BlockSize     : " << std::hex << this->BlockSize      << std::endl <<
          "BlockSpacing  : " << std::hex << this->BlockSpacing   << std::endl <<
          "Total Length  : " << std::hex << this->TotalLength    << std::endl <<
          "BlocksPerCycle: " << std::hex << this->BlocksPerCycle << std::endl <<
          "NumberOfCycles: " << std::hex << this->NumberOfCycles);
    }
    //
    virtual H5FDdsmInt32 Translate(
      std::vector<H5FDdsmMsg> &inRequests, 
      std::vector<H5FDdsmMsg> &outRequests);
    //
    H5FDdsmUInt64 BlockSize;
    H5FDdsmUInt64 BlockSpacing;
    H5FDdsmUInt64 TotalLength;
    H5FDdsmUInt64 BlocksPerCycle;
    H5FDdsmUInt64 NumberOfCycles;
};

//----------------------------------------------------------------------------
// BlockRandomAddressMapper breaks accesses into smaller blocks
// which are scattered randomly over the entire address range to better utilize
// all network links. The random distribution is intended to help avoid
// possible periodic collisions when using the block cyclic pattern.
// Warning : Destination Rank Ids are undefined. You must eventually pass 
// the result of this operation through PartitionedAddressMapper
// Warning: The input to (and output from) this address mapper use 64 bit addresses
//----------------------------------------------------------------------------
class BlockRandomAddressMapper : public AddressMapperStrategy {
  public:
    BlockRandomAddressMapper(
      H5FDdsmUInt64 blockSize, H5FDdsmUInt64 seed, H5FDdsmUInt64 totalLength) 
      : BlockSize(blockSize), TotalLength(totalLength)
    {
      
      this->NumberOfBlocks = this->TotalLength / this->BlockSize;
      if (this->NumberOfBlocks * this->BlockSize != this->TotalLength)
      {
        H5FDdsmError("BlockRandom Addresses not exact multiple of BlockSize");
      }
      this->Shuffle.resize(this->NumberOfBlocks);
      std::generate(this->Shuffle.begin(), this->Shuffle.end(), generator(0));
      random_shuffle(this->Shuffle.begin(), this->Shuffle.end());
//      std::cout << "\nShuffle contents:";
//      for (std::vector<int>::iterator it=this->Shuffle.begin(); it!=this->Shuffle.end(); ++it) {
//        std::cout << " " << *it;
//      }
//      std::cout << std::endl;

    }
    //
    virtual H5FDdsmInt32 Translate(
      std::vector<H5FDdsmMsg> &inRequests, 
      std::vector<H5FDdsmMsg> &outRequests);
    //
    H5FDdsmUInt64             BlockSize;
    H5FDdsmUInt64             TotalLength;
    H5FDdsmUInt64             NumberOfBlocks;
    std::vector<H5FDdsmInt32> Shuffle;

    // a simple 1,2,3...N generator
    struct generator { 
      H5FDdsmInt32 x;
      generator(H5FDdsmInt32 seed) : x(seed) { }
      H5FDdsmInt32 operator ()() { return x++; }
    };
};

//----------------------------------------------------------------------------
// IntegerSizedAddressMapper breaks accesses into int32 sized (0x7FFF)
// accesses if they exceed this size. MPI does not currently (as of 2011)
// allow MPI_Send or other read/writes larger than 32 bit.
// Warning: The input to this address mapper uses 64 bit addresses
// but the output uses 32 bit addresses
//----------------------------------------------------------------------------
class IntegerSizedAddressMapper : public AddressMapperStrategy {
  public:
    IntegerSizedAddressMapper() {}
    //
    virtual H5FDdsmInt32 Translate(
      std::vector<H5FDdsmMsg> &inRequests, 
      std::vector<H5FDdsmMsg> &outRequests);
};

//----------------------------------------------------------------------------
// PartitionedAddressMapper maps addresses into DSM rank/address
// pairs and breaks packets that cross process boundaries into
// multiple accesses.
// Warning: The input (and output) are 32bit addresses
// Warning: his implementation assumes all processes have the same local buffer
// size. Future improvements would allow some nodes to have larger/smaller buffers
//----------------------------------------------------------------------------
class PartitionedAddressMapper : public AddressMapperStrategy {
  public:
    PartitionedAddressMapper(H5FDdsmUInt64 partitionSize) : PartitionSize(partitionSize)
    {
      H5FDdsmDebug("PartitionSize : " << std::hex << this->PartitionSize);
    }
    //
    virtual H5FDdsmInt32 Translate(
      std::vector<H5FDdsmMsg> &inRequests, 
      std::vector<H5FDdsmMsg> &outRequests);
    //
    H5FDdsmUInt64 PartitionSize;
};

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::H5FDdsmAddressMapper()
{
  this->DsmType = H5FD_DSM_TYPE_UNIFORM;
  this->DsmBuffer = NULL;
  this->AddressingStrategy = NULL;
}

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::H5FDdsmAddressMapper(H5FDdsmBuffer *dsmBuffer)
{
  this->DsmType = H5FD_DSM_TYPE_UNIFORM;
  this->DsmBuffer = dsmBuffer;
  this->AddressingStrategy = NULL;
}

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::~H5FDdsmAddressMapper()
{
  delete this->AddressingStrategy;
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmAddressMapper::Translate(H5FDdsmAddr address, H5FDdsmUInt64 length, H5FDdsmPointer data, std::vector<H5FDdsmMsg> &outRequests)
{
  if (!this->AddressingStrategy) {
    // Operation order is : Cyclic->IntSized->RemoteAddress
    this->AddressingStrategy = new PartitionedAddressMapper(
      this->DsmBuffer->GetLength()
    );
    AddressMapperStrategy *LastStrategy = this->AddressingStrategy;
    //
    if (1 /* this->dsmCommunicator->Has32BitLimit() */) {
      IntegerSizedAddressMapper *ISAM = new IntegerSizedAddressMapper();
      LastStrategy->SetDelegate(ISAM);
      LastStrategy = ISAM;
    }
    //
    if (this->DsmType == H5FD_DSM_TYPE_BLOCK_CYCLIC) {
      BlockCyclicAddressMapper *BCAM = new BlockCyclicAddressMapper(
        this->DsmBuffer->GetBlockLength(),
        this->DsmBuffer->GetLength(),
        this->DsmBuffer->GetTotalLength()
      );
      LastStrategy->SetDelegate(BCAM);
      LastStrategy = BCAM;
    }
    else if (this->DsmType == H5FD_DSM_TYPE_BLOCK_RANDOM) {
      BlockRandomAddressMapper *BRAM = new BlockRandomAddressMapper(
        this->DsmBuffer->GetBlockLength(),
        this->DsmBuffer->GetLength(),
        this->DsmBuffer->GetTotalLength()
      );
      LastStrategy->SetDelegate(BRAM);
      LastStrategy = BRAM;
    }
  }
  //
  H5FDdsmMsg dataRequest;
  dataRequest.Dest     = -1;
  dataRequest.Length   = static_cast<H5FDdsmInt32> (length);
  dataRequest.Length64 = length;
  dataRequest.Address  = address;
  dataRequest.Data     = (H5FDdsmByte*) data;
  //
  std::vector<H5FDdsmMsg> tempRequest;
  tempRequest.push_back(dataRequest);
  //
  this->AddressingStrategy->Translate(tempRequest, outRequests);
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 BlockCyclicAddressMapper::Translate(
  std::vector<H5FDdsmMsg> &inRequests, std::vector<H5FDdsmMsg> &outRequests)
{
  std::vector<H5FDdsmMsg> &requests = inRequests; // reference
  std::vector<H5FDdsmMsg> tempRequests;
  if (this->Delegate) {
    if (this->Delegate->Translate(inRequests, tempRequests) != H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    requests = tempRequests;
  }

  // we distribute 0123456789ABCDEF as follows
  //
  // if BlockSize=1(MB eg), BlockSpacing=8, {TotalLength=0x10}
  //      bindex = 0123456789ABCDEF
  //      offset = 0011223344556677
  //      period = 0101010101010101
  // final layout becomes ...
  // -------------------------------------
  // | 0 2 4 6 8 A C E . 1 3 5 7 9 B D F |
  // -------------------------------------
  //
  // if BlockSize=1(MB eg), BlockSpacing=4, {TotalLength=0x10}
  //      bindex = 0123456789ABCDEF
  //      offset = 0000111122223333
  //      period = 0123012301230123
  // final layout becomes ...
  // -----------------------------------------
  // | 0 4 8 C . 1 5 9 D . 2 6 A E . 3 7 B F |
  // -----------------------------------------
  //
  for(std::vector<H5FDdsmMsg>::iterator it = requests.begin(); it != requests.end(); ++it) {
    H5FDdsmByte *datap = (H5FDdsmByte*) it->Data;
    //
    H5FDdsmDebug(std::endl
        << "Input Address : " << std::hex << it->Address << std::endl
        << "Input Length  : " << std::hex << it->Length64 << std::endl);
    while (it->Length64 > 0) {
      H5FDdsmMsg newRequest;
      H5FDdsmUInt64 bindex = (H5FDdsmUInt64) (it->Address / this->BlockSize);
      H5FDdsmUInt64 aindex = it->Address % this->BlockSize;
      H5FDdsmUInt64 period = bindex % this->NumberOfCycles;
      H5FDdsmUInt64 offset = (H5FDdsmUInt64) (bindex / this->NumberOfCycles);
      //
      newRequest.Dest      = -1;
      newRequest.Address   = (period * this->BlockSpacing) + (offset * this->BlockSize) + aindex;
      newRequest.Length64  = min((this->BlockSize - aindex), it->Length64);
      newRequest.Length    = static_cast<H5FDdsmInt32> (newRequest.Length64);
      newRequest.Data      = datap;
      outRequests.push_back(newRequest);
      //
      H5FDdsmDebug(std::endl
          << "\tInput Address     : " << std::hex << it->Address << std::endl
          << "\tTranslated Length : " << std::hex << newRequest.Length64 << std::endl
          << "\tTranslated Address: " << std::hex << newRequest.Address);
      //
      datap        += newRequest.Length64;
      it->Address  += newRequest.Length64;
      it->Length64 -= newRequest.Length64;
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 BlockRandomAddressMapper::Translate(
  std::vector<H5FDdsmMsg> &inRequests, std::vector<H5FDdsmMsg> &outRequests)
{
  std::vector<H5FDdsmMsg> &requests = inRequests; // reference
  std::vector<H5FDdsmMsg> tempRequests;
  if (this->Delegate) {
    if (this->Delegate->Translate(inRequests, tempRequests) != H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    requests = tempRequests;
  }

  for(std::vector<H5FDdsmMsg>::iterator it = requests.begin(); it != requests.end(); ++it) {
    H5FDdsmByte *datap = (H5FDdsmByte*) it->Data;
    //
    H5FDdsmDebug(std::endl
        << "Input Address : " << std::hex << it->Address << std::endl
        << "Input Length  : " << std::hex << it->Length64 << std::endl);
    while (it->Length64 > 0) {
      H5FDdsmMsg newRequest;
      H5FDdsmUInt64 bindex = (H5FDdsmUInt64) (it->Address / this->BlockSize);
      H5FDdsmUInt64 aindex = it->Address % this->BlockSize;
      H5FDdsmUInt64 index  = this->Shuffle[bindex];
      //
      newRequest.Dest      = -1;
      newRequest.Address   = (index * this->BlockSize) + aindex;
      newRequest.Length64  = min((this->BlockSize - aindex), it->Length64);
      newRequest.Length    = static_cast<H5FDdsmInt32> (newRequest.Length64);
      newRequest.Data      = datap;
      outRequests.push_back(newRequest);
      //
      H5FDdsmDebug(std::endl
          << "\tInput Address     : " << std::hex << it->Address << std::endl
          << "\tTranslated Length : " << std::hex << newRequest.Length64 << std::endl
          << "\tTranslated Address: " << std::hex << newRequest.Address);
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
  std::vector<H5FDdsmMsg> &inRequests, std::vector<H5FDdsmMsg> &outRequests)
{
  std::vector<H5FDdsmMsg> &requests = inRequests; // reference
  std::vector<H5FDdsmMsg> tempRequests;
  if (this->Delegate) {
    if (this->Delegate->Translate(inRequests, tempRequests) != H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    requests = tempRequests;
  }
  //
  for(std::vector<H5FDdsmMsg>::iterator it = requests.begin(); it != requests.end(); ++it) {
    H5FDdsmByte *datap = (H5FDdsmByte*) it->Data;
    while (it->Length64 > 0) {
      H5FDdsmMsg newRequest;
      newRequest.Dest    = -1;
      newRequest.Address = it->Address;
      newRequest.Length  = static_cast<H5FDdsmInt32> min(H5FD_DSM_INT32_MAX, it->Length64);
      newRequest.Data    = datap;
      outRequests.push_back(newRequest);
      //
      H5FDdsmDebug(std::endl
          << "Input Address     : " << std::hex << it->Address << std::endl
          << "Translated Length : " << std::hex << newRequest.Length << std::endl
          << "Translated Address: " << std::hex << newRequest.Address);
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
  std::vector<H5FDdsmMsg> &inRequests, std::vector<H5FDdsmMsg> &outRequests)
{
  std::vector<H5FDdsmMsg> &requests = inRequests; // reference
  std::vector<H5FDdsmMsg> tempRequests;
  if (this->Delegate) {
    if (this->Delegate->Translate(inRequests, tempRequests) != H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    requests = tempRequests;
  }
  //
  for(std::vector<H5FDdsmMsg>::iterator it = requests.begin(); it != requests.end(); ++it) {
    H5FDdsmByte *datap = (H5FDdsmByte*) it->Data;
    while (it->Length > 0) {
      H5FDdsmMsg newRequest;
      newRequest.Dest    = static_cast<H5FDdsmInt32> (it->Address / this->PartitionSize);
      newRequest.Address = it->Address % this->PartitionSize;
      newRequest.Length  = static_cast<H5FDdsmInt32> min(this->PartitionSize - newRequest.Address, static_cast<H5FDdsmUInt32> (it->Length));
      newRequest.Data    = datap;
      outRequests.push_back(newRequest);
      //
      H5FDdsmDebug(std::endl
          << "Input Address     : " << std::hex << it->Address << std::endl
          << "Translated Length : " << std::hex << newRequest.Length << std::endl
          << "Translated Address: " << std::hex << newRequest.Address);
      //
      datap       += newRequest.Length;
      it->Address += newRequest.Length;
      it->Length  -= newRequest.Length;
    }
  }
  return(H5FD_DSM_SUCCESS);
}
