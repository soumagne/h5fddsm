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

#ifndef XDMFDSMSTORAGE_H
#define XDMFDSMSTORAGE_H

#include "H5FDdsmObject.h"

class H5FDdsmComm;

class H5FDdsm_EXPORT H5FDdsmStorage : public H5FDdsmObject {

public:
  H5FDdsmStorage();
  ~H5FDdsmStorage();

  H5FDdsmPointer  GetDataPointer(H5FDdsmInt64 Index = 0);

  //! Comm
  H5FDdsmGetValueMacro(Comm, H5FDdsmComm *);
  H5FDdsmSetValueMacro(Comm, H5FDdsmComm *);

  H5FDdsmGetValueMacro(NumberOfElements, H5FDdsmInt64);
  H5FDdsmInt32 SetNumberOfElements(H5FDdsmInt64 Length, H5FDdsmBoolean AllowAllocate=1);

protected:
  H5FDdsmInt32    Allocate();

  H5FDdsmPointer  DataPointer;
  H5FDdsmInt64    NumberOfElements;
  H5FDdsmComm    *Comm;
};

#endif /* XDMFDSMSTORAGE_H */
