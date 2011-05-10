/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmAddressMapper.h

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

#ifndef __H5FDdsmAddressMapper_h
#define __H5FDdsmAddressMapper_h

#include "H5FDdsmObject.h"

#include <vector>

struct H5FDdsmMsg;
class  H5FDdsmDriver;

class H5FDdsm_EXPORT AddressMapperStrategy : public H5FDdsmObject {
  public:
    AddressMapperStrategy() {
      this->Delegate = NULL;
    }
    virtual ~AddressMapperStrategy() {
      delete this->Delegate;
    };
    //
    virtual H5FDdsmInt32 Translate(
      std::vector<H5FDdsmMsg> &inRequests, 
      std::vector<H5FDdsmMsg> &outRequests)=0;
    //
    void SetDelegate(AddressMapperStrategy *d) {
      this->Delegate = d;
    }
    //
    AddressMapperStrategy *Delegate;
};

class H5FDdsm_EXPORT H5FDdsmAddressMapper : public H5FDdsmObject {

public:
  H5FDdsmAddressMapper();
  H5FDdsmAddressMapper(H5FDdsmDriver *dsmDriver);
  ~H5FDdsmAddressMapper();

  // Description:
  // Get/Set DSM Type
  // Valid values are H5FD_DSM_TYPE_UNIFORM, H5FD_DSM_TYPE_BLOCK_CYCLIC
  H5FDdsmGetValueMacro(DsmType, H5FDdsmInt32);
  H5FDdsmSetValueMacro(DsmType, H5FDdsmInt32);

  // Description:
  // Get/Set DSM driver object
  H5FDdsmSetValueMacro(DsmDriver, H5FDdsmDriver*);
  H5FDdsmGetValueMacro(DsmDriver, H5FDdsmDriver*);

  // Description:
  // A Single access of length L, might actually straddle one or more
  // destination processes/chunks etc, so each access must pass
  // through the address translator to produce a vector of address segments.
  H5FDdsmInt32 Translate(H5FDdsmAddr address, H5FDdsmUInt64 length,
      H5FDdsmPointer data, std::vector<H5FDdsmMsg> &dataRequests);

  // Description:
  // Default strategy is set to PartitionedAddressMapper with
  // IntegerSizedAddressMapper 
  void SetAddressMapperStrategy(AddressMapperStrategy *s) {
    this->AddressingStrategy = s;
  }

protected:
  H5FDdsmInt32           DsmType;
  H5FDdsmDriver         *DsmDriver;
  AddressMapperStrategy *AddressingStrategy;
};

#endif /* __H5FDdsmAddressMapper_h */
