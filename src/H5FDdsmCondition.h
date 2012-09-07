/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCondition.h

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

#ifndef __H5FDdsmCondition_h
#define __H5FDdsmCondition_h

#include "H5FDdsmMutex.h"
#include <string>

#ifdef _WIN32
#if (WINVER < _WIN32_WINNT_LONGHORN)
  typedef HANDLE H5FDdsmConditionType;
#else
  typedef CONDITION_VARIABLE H5FDdsmConditionType;
#endif
#else
  typedef pthread_cond_t H5FDdsmConditionType;
#endif

class H5FDdsm_EXPORT H5FDdsmCondition : public H5FDdsmObject {

public:
  H5FDdsmCondition(); 
  virtual ~H5FDdsmCondition(); 

  // name is only for debug messages
  void SetName(const char *name);

  // Description:
  // Wake one thread waiting for the condition to change.
  void Signal();

  // Description:
  // Wait for the condition to change.
  void Wait(H5FDdsmMutex &mutex);

protected:

  H5FDdsmConditionType Condition;
  std::string          ConditionName;
};

#endif // __H5FDdsmCondition_h
