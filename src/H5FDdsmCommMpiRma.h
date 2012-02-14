/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommMpiRma.h

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

#ifndef __H5FDdsmCommMpiRma_h
#define __H5FDdsmCommMpiRma_h

#include "H5FDdsmCommMpi.h"

class H5FDdsm_EXPORT H5FDdsmCommMpiRma : public H5FDdsmCommMpi {

public:
  H5FDdsmCommMpiRma();
  virtual ~H5FDdsmCommMpiRma();

  H5FDdsmInt32   Init();

  H5FDdsmInt32   Put(H5FDdsmMsg *DataMsg);
  H5FDdsmInt32   Get(H5FDdsmMsg *DataMsg);

  H5FDdsmInt32   Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize);
  H5FDdsmInt32   Connect();
  H5FDdsmInt32   Disconnect();

protected:
  MPI_Win        Win;
};

#endif // __H5FDdsmCommMpiRma_h
