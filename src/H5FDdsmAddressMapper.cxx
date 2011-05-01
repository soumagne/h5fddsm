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

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::H5FDdsmAddressMapper() {}

//----------------------------------------------------------------------------
H5FDdsmAddressMapper::~H5FDdsmAddressMapper() {}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::GetAddressRangeForId(H5FDdsmInt32 Id, H5FDdsmAddr *Start, H5FDdsmAddr *End, H5FDdsmAddr Address){
    switch(this->DsmType) {
        case H5FD_DSM_TYPE_UNIFORM :
        case H5FD_DSM_TYPE_UNIFORM_RANGE :
            // All Servers have same length
            *Start = (Id - this->StartServerId) * this->Length;
            *End = *Start + Length - 1;
            break;
        case H5FD_DSM_TYPE_BLOCK_CYCLIC :
          *Start = ((H5FDdsmInt32)(Address / this->BlockLength)) * this->BlockLength;
          *End = ((H5FDdsmInt32)(Address / this->BlockLength) + 1) * this->BlockLength - 1;
          break;
        default :
            // Not Implemented
            H5FDdsmError("DsmType " << this->DsmType << " not yet implemented");
            return(H5FD_DSM_FAIL);
            break;
    }
    H5FDdsmDebug("Address Range for id" << Id << ": start = " << *Start << " , end = " << *End);
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmDriver::AddressToId(H5FDdsmAddr Address){
    H5FDdsmInt32 ServerId = H5FD_DSM_FAIL;

    switch(this->DsmType) {
        case H5FD_DSM_TYPE_UNIFORM :
        case H5FD_DSM_TYPE_UNIFORM_RANGE :
            // All Servers have same length
            ServerId = this->StartServerId + (H5FDdsmInt32)(Address / this->Length);
            if(ServerId > this->EndServerId ){
                H5FDdsmError("ServerId " << ServerId << " for Address " << Address << " is larger than EndServerId " << this->EndServerId);
            }
            break;
        case H5FD_DSM_TYPE_BLOCK_CYCLIC :
          // Keep a uniform DSM but add block cyclic distribution
//          if (Address > this->EndAddress*(this->EndServerId - this->StartServerId + 1)) {
//            H5FDdsmError("Address " << Address << " is larger than end address " << this->EndAddress << " of EndServerId " << this->EndServerId);
//          }
          ServerId = this->StartServerId + ((H5FDdsmInt32)(Address / this->BlockLength) % (this->EndServerId - this->StartServerId + 1));
          break;
        default :
            // Not Implemented
            H5FDdsmError("DsmType " << this->DsmType << " not yet implemented");
            break;
    }
    return(ServerId);
}
