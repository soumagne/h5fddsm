/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommDmapp.h

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

#ifndef __H5FDdsmCommDmapp_h
#define __H5FDdsmCommDmapp_h

#include "H5FDdsmCommMpi.h"

struct H5FDdsmCommDmappInternals;

class H5FDdsm_EXPORT H5FDdsmCommDmapp : public H5FDdsmCommMpi {

public:
  H5FDdsmCommDmapp();
  virtual ~H5FDdsmCommDmapp();

  H5FDdsmInt32   Init();

  H5FDdsmInt32   Put(H5FDdsmMsg *DataMsg);
  H5FDdsmInt32   Get(H5FDdsmMsg *DataMsg);
  H5FDdsmInt32   WindowSync();

  H5FDdsmInt32   Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize);
  H5FDdsmInt32   Connect();
  H5FDdsmInt32   Disconnect();

  H5FDdsmInt32   GatherIntraInstIds();

protected:
  H5FDdsmCommDmappInternals *CommDmappInternals;
  H5FDdsmBoolean   IsDmappInitialized;
  H5FDdsmBoolean   UseBlockingComm;
};

#endif // __H5FDdsmCommDmapp_h
