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
#include "H5FDdsmStorage.h"
#include "H5FDdsm.h"
//
#include "H5Eprivate.h" // Error handling

// pointer to internal DSM manager reference
H5FDdsmManager *dsmManager = NULL;

#define DSM_DRIVER_ERROR(x) \
{                           \
   H5FDdsmError(x);         \
   return(FAIL);            \
}

#define DSM_DRIVER_INIT(buf)                   \
{                                              \
  if (dsmManager) {                            \
    buf = dsmManager->GetDsmBuffer();          \
    if (!buf)                                  \
      DSM_DRIVER_ERROR("No DSM buffer found")  \
  } else {                                     \
    DSM_DRIVER_ERROR("No DSM manager found")   \
  }                                            \
}

//#define H5FD_DSM_DRIVER_DEBUG
#ifdef H5FD_DSM_DRIVER_DEBUG
#  define PRINT_DSM_DRIVER_INFO(a,x) std::cout << "(" << a << ") " << x << std::endl;
#else
#  define PRINT_DSM_DRIVER_INFO(a,x)
#endif

//----------------------------------------------------------------------------
// Declare extra debug info 
#undef H5FDdsmDebug
#ifdef H5VLdso_DEBUG_GLOBAL
#define H5FDdsmDebug(x) \
{ std::cout << "H5FD_DSM Debug Level 0           : " << x << std::endl; \
}
#else 
 #define H5FDdsmDebug(x)
#endif
//----------------------------------------------------------------------------
//--------------------------------------------------------------------------
void*
dsm_get_manager()
{
  void *ret_value = NULL;
  if (dsmManager) ret_value = static_cast <void*> (dsmManager);
  return(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_get_properties(MPI_Comm *intra_comm, void **buf_ptr_ptr, size_t *buf_len_ptr)
{
  if (!dsmManager) DSM_DRIVER_ERROR("No DSM manager found")

  if (intra_comm) *intra_comm = dsmManager->GetMpiComm();
  if (dsmManager->GetIsServer()) {
    if (buf_ptr_ptr) *buf_ptr_ptr =
        dsmManager->GetDsmBuffer()->GetStorage()->GetDataPointer();
    if (buf_len_ptr) *buf_len_ptr =
        dsmManager->GetDsmBuffer()->GetStorage()->GetLength();
  } else {
    if (buf_ptr_ptr) *buf_ptr_ptr = NULL;
    if (buf_len_ptr) *buf_len_ptr = 0;
  }

  return(SUCCEED);
}

//--------------------------------------------------------------------------
void
dsm_set_manager(void *manager)
{
  dsmManager = static_cast <H5FDdsmManager*> (manager);
}

//--------------------------------------------------------------------------
herr_t
dsm_alloc(MPI_Comm intra_comm, void *buf_ptr, size_t buf_len)
{
  // Check arguments
  if (dsmManager) DSM_DRIVER_ERROR("DSM manager already allocated")
  if (intra_comm == MPI_COMM_NULL) DSM_DRIVER_ERROR("invalid intra comm argument")
  if (buf_ptr && !buf_len) DSM_DRIVER_ERROR("invalid buffer length argument")

  dsmManager = new H5FDdsmManager();
  dsmManager->ReadConfigFile();
  dsmManager->SetIsAutoAllocated(H5FD_DSM_TRUE);
  dsmManager->SetMpiComm(intra_comm);
  if (buf_ptr) {
    // TODO initialize DSM from given buffer
  }
  if (dsmManager->Create() != H5FD_DSM_SUCCESS)
    DSM_DRIVER_ERROR("Cannot create DSM manager")

  // if (dsmManager->GetIsServer()) {
    // TODO Leave the auto publish here for now
    // dsmManager->Publish();
    // while (!dsmManager->GetIsConnected()) {
      // Spin
    // }
  // }

  return(SUCCEED);
}

//--------------------------------------------------------------------------
herr_t
dsm_free()
{
  if (dsmManager) {
    if (dsmManager->GetIsAutoAllocated()) {
      if (dsmManager->GetIsConnected()) dsmManager->Disconnect();
      if (dsmManager->GetIsServer()) dsmManager->Unpublish();

      delete dsmManager;
      dsmManager = NULL;
    }
  }

  return(SUCCEED);
}

//--------------------------------------------------------------------------
hbool_t
dsm_is_server()
{
  hbool_t ret_value = TRUE;

  if (!dsmManager) DSM_DRIVER_ERROR("No DSM manager found")

  ret_value = (dsmManager->GetIsServer() == H5FD_DSM_TRUE) ? TRUE : FALSE;

  return(ret_value);
}

//--------------------------------------------------------------------------
hbool_t
dsm_is_driver_serial()
{
  hbool_t ret_value = TRUE;

  if (!dsmManager) DSM_DRIVER_ERROR("No DSM manager found")

  ret_value = (dsmManager->GetIsDriverSerial() == H5FD_DSM_TRUE) ? TRUE : FALSE;

  return(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_set_options(unsigned long flags)
{
  H5FDdsmBufferService *dsmBufferService = NULL;

  DSM_DRIVER_INIT(dsmBufferService)

  // Lock/Unlock Synchronization protocol
  if ((flags & H5FD_DSM_LOCK_SYNCHRONOUS) == H5FD_DSM_LOCK_SYNCHRONOUS) {
      dsmBufferService->SetSychronizationCount(1);
  }
  if ((flags & H5FD_DSM_LOCK_ASYNCHRONOUS) == H5FD_DSM_LOCK_ASYNCHRONOUS) {
    dsmBufferService->SetSychronizationCount(0);
  }

  // Parallel/Serial
  if ((flags & H5FD_DSM_MODE_SERIAL) == H5FD_DSM_MODE_SERIAL) {
      dsmManager->SetIsDriverSerial(H5FD_DSM_TRUE);
  }
  if ((flags & H5FD_DSM_MODE_PARALLEL) == H5FD_DSM_MODE_PARALLEL) {
      dsmManager->SetIsDriverSerial(H5FD_DSM_FALSE);
  }

  return(SUCCEED);
}

//--------------------------------------------------------------------------
hbool_t
dsm_is_connected()
{
  hbool_t ret_value = TRUE;

  if (!dsmManager) DSM_DRIVER_ERROR("No DSM manager found")

  ret_value = (dsmManager->GetIsConnected() == H5FD_DSM_TRUE) ? TRUE : FALSE;

  return(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_connect()
{
  if (!dsmManager) DSM_DRIVER_ERROR("No DSM manager found")

  // Initialize the connection if it has not been done already
  if (dsmManager->GetIsConnected()) DSM_DRIVER_ERROR("Already connected")

  if (dsmManager->Connect() == H5FD_DSM_SUCCESS) {
    PRINT_DSM_DRIVER_INFO(dsmManager->GetUpdatePiece(), "Connected!")
  } else {
    DSM_DRIVER_ERROR("Cannot connect")
  }

  return(SUCCEED);
}

//--------------------------------------------------------------------------
herr_t
dsm_update_entry(haddr_t start, haddr_t end)
{
  H5FDdsmAddr addr;
  H5FDdsmEntry entry;
  H5FDdsmBufferService *dsmBufferService = NULL;

  DSM_DRIVER_INIT(dsmBufferService)

  entry.start = start;
  entry.end   = end;
  addr = (H5FDdsmAddr) (dsmBufferService->GetTotalLength() - sizeof(H5FDdsmMetaData));

  PRINT_DSM_DRIVER_INFO(dsmManager->GetUpdatePiece(),
      "dsm_update_entry start " << start << " end " << end << " addr " << addr)

  // Do not send anything if the end of the file is 0
  if (entry.end > 0) {
    if (dsmBufferService->Put(addr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS)
      DSM_DRIVER_ERROR("Cannot put entry")
  } else {
    PRINT_DSM_DRIVER_INFO(dsmManager->GetUpdatePiece(), "end entry is " << entry.end)
  }

  return(SUCCEED);
}

//--------------------------------------------------------------------------
herr_t
dsm_get_entry(haddr_t *start_ptr, haddr_t *end_ptr)
{
  H5FDdsmAddr addr;
  H5FDdsmEntry entry;
  H5FDdsmBufferService *dsmBufferService = NULL;

  DSM_DRIVER_INIT(dsmBufferService)

  addr = (H5FDdsmAddr) (dsmBufferService->GetTotalLength() - sizeof(H5FDdsmMetaData));

  if (dsmBufferService->Get(addr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS)
    DSM_DRIVER_ERROR("Cannot get entry")

  *start_ptr = entry.start;
  *end_ptr   = entry.end;

  PRINT_DSM_DRIVER_INFO(dsmManager->GetUpdatePiece(), "dsm_get_entry start " <<
      *start_ptr << " end " << *end_ptr << " addr " << addr)

  return(SUCCEED);
}

//--------------------------------------------------------------------------
herr_t
dsm_lock()
{
  H5FDdsmBufferService *dsmBufferService = NULL;

  DSM_DRIVER_INIT(dsmBufferService)

  H5FDdsmBoolean parallel = (dsmManager->GetIsDriverSerial() == H5FD_DSM_TRUE) ? FALSE : TRUE;
  if (dsmBufferService->RequestLockAcquire(parallel) != H5FD_DSM_SUCCESS)
    DSM_DRIVER_ERROR("Cannot request lock acquisition")

  return(SUCCEED);
}

//--------------------------------------------------------------------------
// This is a manual unlock, a notification is sent (according to flag)
// and then the lock is released
herr_t
dsm_unlock(unsigned long flag)
{
  H5FDdsmBufferService *dsmBufferService;

  DSM_DRIVER_INIT(dsmBufferService)

  H5FDdsmBoolean parallel = (dsmManager->GetIsDriverSerial() == H5FD_DSM_TRUE) ? FALSE : TRUE;
  if (dsmBufferService->RequestLockRelease(flag, parallel) != H5FD_DSM_SUCCESS)
    DSM_DRIVER_ERROR("Cannot request lock release")

  return(SUCCEED);
}

//--------------------------------------------------------------------------
herr_t
dsm_read(haddr_t addr, size_t len, void *buf_ptr)
{
  H5FDdsmBufferService *dsmBufferService;

  DSM_DRIVER_INIT(dsmBufferService)

  if (dsmBufferService->Get((H5FDdsmAddr) addr, (H5FDdsmUInt64) len,
      (H5FDdsmPointer) buf_ptr) != H5FD_DSM_SUCCESS)
    DSM_DRIVER_ERROR("Cannot get data")

  return(SUCCEED);
}

//--------------------------------------------------------------------------
herr_t
dsm_write(haddr_t addr, size_t len, const void *buf_ptr)
{
  H5FDdsmBufferService *dsmBufferService;

  DSM_DRIVER_INIT(dsmBufferService)

  if (dsmBufferService->Put((H5FDdsmAddr) addr, (H5FDdsmUInt64) len,
      (H5FDdsmPointer) buf_ptr) != H5FD_DSM_SUCCESS)
    DSM_DRIVER_ERROR("Cannot put data")

  return(SUCCEED);
}
