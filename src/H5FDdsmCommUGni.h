/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommUGni.h

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

#ifndef __H5FDdsmCommUGni_h
#define __H5FDdsmCommUGni_h

#include "H5FDdsmCommMpi.h"

#include <gni_pub.h>

struct H5FDdsmCommUGniInternals;

class H5FDdsm_EXPORT H5FDdsmCommUGni : public H5FDdsmCommMpi {

public:
  H5FDdsmCommUGni();
  virtual ~H5FDdsmCommUGni();

  H5FDdsmInt32   Init();

  H5FDdsmInt32   Put(H5FDdsmMsg *DataMsg);
  H5FDdsmInt32   Get(H5FDdsmMsg *DataMsg);
  H5FDdsmInt32   WindowSync();

  H5FDdsmInt32   Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize);
  H5FDdsmInt32   Connect();
  H5FDdsmInt32   Disconnect();

protected:
  H5FDdsmInt32   PutFma(H5FDdsmMsg *DataMsg, gni_mem_handle_t src_mem_handle);
  H5FDdsmInt32   GetFma(H5FDdsmMsg *DataMsg);

  H5FDdsmInt32   PutRdma(H5FDdsmMsg *DataMsg, gni_mem_handle_t src_mem_handle);
  H5FDdsmInt32   GetRdma(H5FDdsmMsg *DataMsg);

  H5FDdsmInt32   GetGniNicAddress(H5FDdsmInt32 device_id, H5FDdsmUInt32 *address);
  H5FDdsmInt32   GatherIntraNicAddresses();
  H5FDdsmInt32   GatherIntraInstIds();

  H5FDdsmCommUGniInternals *CommUGniInternals;
  H5FDdsmBoolean   IsCommInitialized;
};

#endif // __H5FDdsmCommUGni_h
