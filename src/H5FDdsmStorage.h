/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmStorage.h

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

#ifndef __H5FDdsmStorage_h
#define __H5FDdsmStorage_h

#include "H5FDdsmObject.h"

class H5FDdsm_EXPORT H5FDdsmStorage : public H5FDdsmObject {

public:
  H5FDdsmStorage();
  virtual ~H5FDdsmStorage();

  H5FDdsmPointer GetDataPointer(H5FDdsmAddr Addr = 0);

  H5FDdsmGetValueMacro(Length, H5FDdsmUInt64);
  H5FDdsmInt32 SetLength(H5FDdsmUInt64 Length, H5FDdsmBoolean AllowAllocate=1);

protected:
  virtual H5FDdsmInt32 Allocate();
  virtual H5FDdsmInt32 Deallocate();

  H5FDdsmPointer   DataPointer;
  H5FDdsmUInt64    Length;
};

#endif // __H5FDdsmStorage_h
