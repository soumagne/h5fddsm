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

//struct H5FDdsmAddressMapperInternals;
class H5FDdsmMsg;
class H5FDdsmDriver;

class H5FDdsm_EXPORT H5FDdsmAddressMapper : public H5FDdsmObject {

public:
  H5FDdsmAddressMapper();
  H5FDdsmAddressMapper(H5FDdsmDriver *dsmDriver);
  ~H5FDdsmAddressMapper();

  // Type
  H5FDdsmGetValueMacro(DsmType, H5FDdsmInt32);
  H5FDdsmSetValueMacro(DsmType, H5FDdsmInt32);

  H5FDdsmSetValueMacro(DsmDriver, H5FDdsmDriver*);
  H5FDdsmGetValueMacro(DsmDriver, H5FDdsmDriver*);

  //! Address Range
  H5FDdsmInt32 GetAddressRangeForId(H5FDdsmInt32 Id, H5FDdsmAddr *Start,
      H5FDdsmAddr *End, H5FDdsmAddr Address);

  H5FDdsmInt32 AddressToId(H5FDdsmAddr Address);

  H5FDdsmInt32 Translate(H5FDdsmAddr address, H5FDdsmUInt64 length,
      H5FDdsmPointer data, std::vector<H5FDdsmMsg*> &dataRequests);

protected:
  H5FDdsmInt32 DsmType;
  H5FDdsmDriver *DsmDriver;
};

#endif /* __H5FDdsmAddressMapper_h */
