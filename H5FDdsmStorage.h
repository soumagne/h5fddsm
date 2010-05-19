/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmStorage.h

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing
  1) This copyright notice appears on all copies of source code
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it
  must not be reformatted such that the indentation, bracketing or
  overall style is modified significantly.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=========================================================================*/

#ifndef XDMFDSMSTORAGE_H
#define XDMFDSMSTORAGE_H

#include "H5FDdsmObject.h"

class H5FDdsm_EXPORT H5FDdsmStorage : public H5FDdsmObject {

public:
  H5FDdsmStorage();
  ~H5FDdsmStorage();

  H5FDdsmPointer  GetDataPointer(Int64 Index = 0);

  H5FDdsmGetValueMacro(NumberOfElements, int);
  H5FDdsmInt32 SetNumberOfElements(int Length, H5FDdsmBoolean AllowAllocate=1);

protected:
  H5FDdsmInt32    Allocate();

  H5FDdsmPointer  DataPointer;
  size_t       NumberOfElements;
};

#endif /* XDMFDSMSTORAGE_H */
