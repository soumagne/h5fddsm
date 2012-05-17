/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmThread.cxx

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

#include "H5FDdsmThread.h"

#ifndef _WIN32
extern "C" { typedef void *(*H5FDdsmExternCThreadFunctionType)(void *); }
#endif

//----------------------------------------------------------------------------
H5FDdsmThread::H5FDdsmThread()
{
#ifdef _WIN32
  this->ThreadID = NULL;
#else
  this->ThreadID = 0;
#endif
}

//----------------------------------------------------------------------------
H5FDdsmThread::~H5FDdsmThread()
{
}

//----------------------------------------------------------------------------
void
H5FDdsmThread::SpawnThread(H5FDdsmThreadFunctionType f, void *data)
{
#ifdef _WIN32
  this->ThreadID = CreateThread(NULL, 0, f, data, 0, NULL);
#else
  pthread_create(&this->ThreadID, NULL,
      reinterpret_cast<H5FDdsmExternCThreadFunctionType>(f), data);
#endif
}

//----------------------------------------------------------------------------
void
H5FDdsmThread::JoinThread()
{
#ifdef _WIN32
  WaitForSingleObject(this->ThreadID, INFINITE);
  CloseHandle(this->ThreadID);
  this->ThreadID = NULL;
#else
  pthread_join(this->ThreadID, NULL);
  this->ThreadID = 0;
#endif
}

//----------------------------------------------------------------------------
void
H5FDdsmThread::TerminateThread()
{
#ifdef _WIN32
  WaitForSingleObject(this->ThreadID, 0);
  CloseHandle(this->ThreadID);
  this->ThreadID = NULL;
#else
  pthread_cancel(this->ThreadID);
  this->ThreadID = 0;
#endif
}
