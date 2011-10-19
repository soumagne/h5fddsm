/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmDriver.cxx

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

#include "H5FDdsmDriver.h"
#include "H5FDdsmManager.h"
//
#include "H5Eprivate.h" // Error handling
//
// H5private.h defines attribute, but we don't want it as it causes link errors
// on some gcc versions
#ifdef __GNUC__
# undef __attribute__
#endif

// pointer to internal DSM manager reference
H5FDdsmManager *dsmManager = NULL;

#define DSM_DRIVER_GOTO_ERROR(x, ret_val)                                  \
{                                                                          \
   fprintf(stderr, "Error at %s %s:%d %s\n", __FILE__, FUNC, __LINE__, x); \
   err_occurred = TRUE;                                                    \
   if (err_occurred) { HGOTO_DONE(ret_val) }                               \
}

// #define H5FD_DSM_DEBUG
#ifdef H5FD_DSM_DEBUG
#  define PRINT_DSM_DRIVER_INFO(a,x) std::cout << "(" << a << ") " << x << std::endl;
#else
#  define PRINT_DSM_DRIVER_INFO(a,x)
#endif

//--------------------------------------------------------------------------
herr_t
DsmLock()
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(DsmNotify, FAIL)

  if (dsmManagerSingleton) {
    dsmBufferService = dsmManagerSingleton->GetDsmBuffer();
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  }

  if (!dsmBufferService->GetIsLocked()) dsmBufferService->RequestLockAcquire();

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
DsmUnlock()
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(DsmNotify, FAIL)

  if (file->DsmBuffer->GetReleaseLockOnClose() && file->DsmBuffer->GetIsLocked()) {
    file->DsmBuffer->RequestLockRelease();
  }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
//--------------------------------------------------------------------------
herr_t
DsmUpdateEntry(H5FD_dsm_t *file)
{
  H5FDdsmAddr  addr;
  H5FDdsmEntry entry;

  PRINT_INFO("DsmUpdateEntry()");

  if (!file->DsmBuffer) return (H5FD_DSM_FAIL);

//  file->end = MAX((file->start + file->eof), file->end);
//  file->eof = file->end - file->start;

  if (!file->DsmBuffer->GetIsReadOnly()) {
    entry.start = file->start;
    entry.end = file->end;
    addr = (H5FDdsmAddr) (file->DsmBuffer->GetTotalLength() - sizeof(H5FDdsmMetaData));

    PRINT_INFO("DsmUpdateEntry start " <<
        file->start <<
        " end " << file->end <<
        " addr " << addr);

    // Only one of the processes writing to the DSM needs to write file metadata
    // but we must be careful that all the processes keep the metadata synchronized
    // Do not send anything if the end of the file is 0
    if ((file->DsmBuffer->GetComm()->GetId() == 0) && (entry.end > 0)) {
      if (file->DsmBuffer->Put(addr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
    }
    file->DsmBuffer->GetComm()->Barrier();
  }
  return(H5FD_DSM_SUCCESS);
}

//--------------------------------------------------------------------------
herr_t
DsmGetEntry(haddr_t *start_ptr, haddr_t *end_ptr)
{
  H5FDdsmAddr  addr;
  H5FDdsmEntry entry;

  PRINT_INFO("DsmGetEntry()");

  if (!file->DsmBuffer) return (H5FD_DSM_FAIL);

  addr = (H5FDdsmAddr) (file->DsmBuffer->GetTotalLength() - sizeof(H5FDdsmMetaData));

  // Get is done by every process so we can do independent reads (in parallel by blocks)
  // using the driver as a serial driver
  if (file->DsmBuffer->Get(addr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS) {
    PRINT_INFO("DsmGetEntry failed");
    return H5FD_DSM_FAIL;
  }

  *start_ptr = entry.start;
  *end_ptr = entry.end;

  PRINT_INFO("DsmGetEntry start " <<
      file->start <<
      " end " << file->end <<
      " addr " << addr);

  return(H5FD_DSM_SUCCESS);
}

//--------------------------------------------------------------------------
herr_t
DsmAutoAlloc(MPI_Comm comm)
{
  if (!dsmManagerSingleton) {
    dsmManagerSingleton = new H5FDdsmManager();
    dsmManagerSingleton->ReadConfigFile();
    dsmManagerSingleton->SetMpiComm(comm);
    dsmManagerSingleton->Create();
    if (dsmManagerSingleton->GetIsServer()) {
      // TODO Leave the auto publish here for now
      dsmManagerSingleton->Publish();
      while (!dsmManagerSingleton->GetDsmBuffer()->GetIsConnected()) {
        // Spin
      }
    }
    dsmManagerSingleton->GetDsmBuffer()->SetIsAutoAllocated(H5FD_DSM_TRUE);
  }
  return(H5FD_DSM_SUCCESS);
}

//--------------------------------------------------------------------------
herr_t
DsmAutoDealloc()
{
  if (dsmManagerSingleton) {
    if (!dsmManagerSingleton->GetIsServer() && dsmManagerSingleton->GetDsmBuffer()->GetIsConnected()) {
      dsmManagerSingleton->Disconnect();
    }
    if (dsmManagerSingleton->GetIsServer()) {
      dsmManagerSingleton->Unpublish();
    }
    delete dsmManagerSingleton;
    dsmManagerSingleton = NULL;
  }
  return(H5FD_DSM_SUCCESS);
}

//--------------------------------------------------------------------------
H5FDdsmBuffer *
DsmGetAutoAllocatedBuffer()
{
  H5FDdsmBuffer *buffer = NULL;

  if (dsmManagerSingleton) {
    buffer = dsmManagerSingleton->GetDsmBuffer();
  }
  return(buffer);
}

//--------------------------------------------------------------------------
H5FDdsmManager *
DsmGetAutoAllocatedManager()
{
  H5FDdsmManager *manager = NULL;

  if (dsmManagerSingleton) {
    manager = dsmManagerSingleton;;
  }
  return(manager);
}

//--------------------------------------------------------------------------
herr_t
DsmBufferConnect(H5FDdsmBufferService *dsmBuffer)
{
  // Initialize the connection if it has not been done already
  if (!dsmBuffer->GetIsConnected()) {
    if (dsmBuffer->GetComm()->Connect() == H5FD_DSM_SUCCESS) {
      PRINT_DSM_INFO(dsmBuffer->GetComm()->GetId(), "Connected!");
      dsmBuffer->SetIsConnected(H5FD_DSM_TRUE);
      dsmBuffer->ReceiveInfo();
    }
    else {
      PRINT_DSM_INFO(dsmBuffer->GetComm()->GetId(), "DSMBuffer Comm_connect returned FAIL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//--------------------------------------------------------------------------
herr_t
DsmSetOptions(unsigned long flags)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(DsmSetOptions, FAIL)

  if (dsmManagerSingleton) {
    dsmBufferService = dsmManagerSingleton->GetDsmBuffer();
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  }

  switch(flags) {
    case H5FD_DSM_DONT_RELEASE:
      dsmBufferService->SetReleaseLockOnClose(H5FD_DSM_FALSE);
      /* If we don't release the file, we don't send notifications as well */
    case H5FD_DSM_DONT_NOTIFY:
      dsmBufferService->SetNotificationOnClose(H5FD_DSM_FALSE);
      break;
    default:
      PRINT_DSM_INFO(dsmBufferService->GetComm()->GetId(), "Not implemented option");
      break;
  }

done:
  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
DsmNotify(unsigned long flags)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(DsmNotify, FAIL)

  if (dsmManagerSingleton) {
    dsmBufferService = dsmManagerSingleton->GetDsmBuffer();
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  }

  switch(flags) {
    case H5FD_DSM_NEW_INFORMATION:
    case H5FD_DSM_NEW_DATA:
      dsmBufferService->SetNotification(flags);
      break;
    default:
      PRINT_DSM_INFO(dsmBufferService->GetComm()->GetId(), "Not implemented notification");
      break;
  }

  if (!dsmBufferService->GetIsLocked()) dsmBufferService->RequestLockAcquire();
  dsmBufferService->RequestNotification();

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
DsmSetModified()
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(DsmNotify, FAIL)
//file->DsmBuffer->SetIsDataModified(H5FD_DSM_TRUE);
//if (file->DsmBuffer->GetNotificationOnClose()) {
//  if (!file->DsmBuffer->GetIsServer()) {
//    H5FD_dsm_notify(H5FD_DSM_NEW_DATA, file->DsmBuffer);
//  } else {
//    file->DsmBuffer->SignalNotification();
//  }
//}
  done:
      FUNC_LEAVE_NOAPI(ret_value);
}
//--------------------------------------------------------------------------
