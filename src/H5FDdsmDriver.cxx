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

#define DSM_DRIVER_GOTO_ERROR(x, ret_val)                                  \
{                                                                          \
   fprintf(stderr, "Error at %s %s:%d %s\n", __FILE__, FUNC, __LINE__, x); \
   err_occurred = TRUE;                                                    \
   if (err_occurred) { HGOTO_DONE(ret_val) }                               \
}

//#define H5FD_DSM_DRIVER_DEBUG
#ifdef H5FD_DSM_DRIVER_DEBUG
#  define PRINT_DSM_DRIVER_INFO(a,x) std::cout << "(" << a << ") " << x << std::endl;
#else
#  define PRINT_DSM_DRIVER_INFO(a,x)
#endif

//--------------------------------------------------------------------------
void*
dsm_get_manager()
{
  void *ret_value = NULL;

  FUNC_ENTER_NOAPI_NOFUNC(dsm_get_manager)

  if (dsmManager) ret_value = static_cast <void*> (dsmManager);

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_get_properties(MPI_Comm *intra_comm, void **buf_ptr_ptr, size_t *buf_len_ptr)
{
  herr_t ret_value = SUCCEED;

  FUNC_ENTER_NOAPI(dsm_get_properties, FAIL)

  if (!dsmManager)
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);

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

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_set_manager(void *manager)
{
  herr_t ret_value = SUCCEED;

  FUNC_ENTER_NOAPI(dsm_set_manager, FAIL)

  if (!manager)
    DSM_DRIVER_GOTO_ERROR("Invalid argument", FAIL);

  dsmManager = static_cast <H5FDdsmManager*> (manager);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_alloc(MPI_Comm intra_comm, void *buf_ptr, size_t buf_len)
{
  herr_t ret_value = SUCCEED;

  FUNC_ENTER_NOAPI(dsm_alloc, FAIL)

  // Check arguments
  if (dsmManager)
    DSM_DRIVER_GOTO_ERROR("DSM manager already allocated", FAIL);
  if (intra_comm == MPI_COMM_NULL)
    DSM_DRIVER_GOTO_ERROR("invalid intra comm argument", FAIL);
  if (buf_ptr && !buf_len)
    DSM_DRIVER_GOTO_ERROR("invalid buffer length argument", FAIL);

  dsmManager = new H5FDdsmManager();
  dsmManager->ReadConfigFile();
  dsmManager->SetIsAutoAllocated(H5FD_DSM_TRUE);
  dsmManager->SetMpiComm(intra_comm);
  if (buf_ptr) {
    // TODO initialize DSM from given buffer
  }
  if (dsmManager->Create() != H5FD_DSM_SUCCESS)
    DSM_DRIVER_GOTO_ERROR("Cannot create DSM manager", FAIL);

  // if (dsmManager->GetIsServer()) {
    // TODO Leave the auto publish here for now
    // dsmManager->Publish();
    // while (!dsmManager->GetIsConnected()) {
      // Spin
    // }
  // }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_free()
{
  herr_t ret_value = SUCCEED;

  FUNC_ENTER_NOAPI(dsm_free, FAIL)

  if (dsmManager) {
    if (dsmManager->GetIsAutoAllocated()) {
      if (dsmManager->GetIsConnected()) dsmManager->Disconnect();
      if (dsmManager->GetIsServer()) dsmManager->Unpublish();

      delete dsmManager;
      dsmManager = NULL;
    }
  } else {
    DSM_DRIVER_GOTO_ERROR("DSM manager not found or already released", FAIL);
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
hbool_t
dsm_is_server()
{
  herr_t ret_value = TRUE;

  FUNC_ENTER_NOAPI(dsm_is_server, FAIL)

  if (!dsmManager)
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);

  ret_value = (dsmManager->GetIsServer() == H5FD_DSM_TRUE) ? TRUE : FALSE;

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
hbool_t
dsm_is_driver_serial()
{
  herr_t ret_value = TRUE;

  FUNC_ENTER_NOAPI(dsm_is_driver_serial, FAIL)

  if (!dsmManager)
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);

  ret_value = (dsmManager->GetIsDriverSerial() == H5FD_DSM_TRUE) ? TRUE : FALSE;

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_set_options(unsigned long flags)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService = NULL;

  FUNC_ENTER_NOAPI(dsm_set_options, FAIL)

  if (dsmManager) {
    dsmBufferService = dsmManager->GetDsmBuffer();
    if (!dsmBufferService)
      DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);
  }

  switch(flags) {
    case H5FD_DSM_DONT_RELEASE:
      dsmBufferService->SetReleaseLockOnClose(H5FD_DSM_FALSE);
      /* If we don't release the file, we don't send notifications as well */
    case H5FD_DSM_DONT_NOTIFY:
      dsmBufferService->SetNotificationOnClose(H5FD_DSM_FALSE);
      break;
    case H5FD_DSM_MODE_SERIAL:
      dsmManager->SetIsDriverSerial(H5FD_DSM_TRUE);
      break;
    default:
      PRINT_DSM_DRIVER_INFO(dsmBufferService->GetComm()->GetId(), "Not implemented option");
      break;
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
hbool_t
dsm_is_connected()
{
  herr_t ret_value = TRUE;

  FUNC_ENTER_NOAPI(dsm_is_connected, FAIL)

  if (!dsmManager)
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);

  ret_value = (dsmManager->GetIsConnected() == H5FD_DSM_TRUE) ? TRUE : FALSE;

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_connect()
{
  herr_t ret_value = SUCCEED;

  FUNC_ENTER_NOAPI(dsm_connect, FAIL)

  if (!dsmManager)
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);

  // Initialize the connection if it has not been done already
  if (dsmManager->GetIsConnected())
    DSM_DRIVER_GOTO_ERROR("Already connected", FAIL);

  if (dsmManager->Connect() == H5FD_DSM_SUCCESS) {
    PRINT_DSM_DRIVER_INFO(dsmManager->GetUpdatePiece(), "Connected!");
  } else {
    DSM_DRIVER_GOTO_ERROR("Cannot connect", FAIL);
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_update_entry(haddr_t start, haddr_t end)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmAddr addr;
  H5FDdsmEntry entry;
  H5FDdsmBufferService *dsmBufferService = NULL;

  FUNC_ENTER_NOAPI(dsm_update_entry, FAIL)

  if (dsmManager) {
    dsmBufferService = dsmManager->GetDsmBuffer();
    if (!dsmBufferService)
      DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);
  }

  entry.start = start;
  entry.end   = end;
  addr = (H5FDdsmAddr) (dsmBufferService->GetTotalLength() - sizeof(H5FDdsmMetaData));

  PRINT_DSM_DRIVER_INFO(dsmManager->GetUpdatePiece(),
      "dsm_update_entry start " << start << " end " << end << " addr " << addr);

  // Do not send anything if the end of the file is 0
  if (entry.end > 0) {
    if (dsmBufferService->Put(addr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS)
      DSM_DRIVER_GOTO_ERROR("Cannot put entry", FAIL);
  } else {
    PRINT_DSM_DRIVER_INFO(dsmManager->GetUpdatePiece(), "end entry is " << entry.end);
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_get_entry(haddr_t *start_ptr, haddr_t *end_ptr)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmAddr addr;
  H5FDdsmEntry entry;
  H5FDdsmBufferService *dsmBufferService = NULL;

  FUNC_ENTER_NOAPI(dsm_get_entry, FAIL)

  if (dsmManager) {
    dsmBufferService = dsmManager->GetDsmBuffer();
    if (!dsmBufferService)
      DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);
  }

  addr = (H5FDdsmAddr) (dsmBufferService->GetTotalLength() - sizeof(H5FDdsmMetaData));

  if (dsmBufferService->Get(addr, sizeof(entry), &entry) != H5FD_DSM_SUCCESS)
    DSM_DRIVER_GOTO_ERROR("Cannot get entry", FAIL);

  *start_ptr = entry.start;
  *end_ptr   = entry.end;

  PRINT_DSM_DRIVER_INFO(dsmManager->GetUpdatePiece(), "dsm_get_entry start " <<
      *start_ptr << " end " << *end_ptr << " addr " << addr);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_lock()
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService = NULL;

  FUNC_ENTER_NOAPI(dsm_lock, FAIL)

  if (dsmManager) {
    dsmBufferService = dsmManager->GetDsmBuffer();
    if (!dsmBufferService)
      DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);
  }

  if (!dsmBufferService->GetIsLocked()) {
    if (dsmBufferService->RequestLockAcquire() != H5FD_DSM_SUCCESS)
      DSM_DRIVER_GOTO_ERROR("Cannot request lock acquisition", FAIL);
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_unlock()
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;

  FUNC_ENTER_NOAPI(dsm_unlock, FAIL)

  if (dsmManager) {
    dsmBufferService = dsmManager->GetDsmBuffer();
    if (!dsmBufferService)
      DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);
  }

  if (dsmBufferService->GetIsLocked() && dsmBufferService->GetReleaseLockOnClose()) {
    if (dsmBufferService->RequestLockRelease() != H5FD_DSM_SUCCESS)
      DSM_DRIVER_GOTO_ERROR("Cannot request lock release", FAIL);
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_read(haddr_t addr, size_t len, void *buf_ptr)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;

  FUNC_ENTER_NOAPI(dsm_read, FAIL)

  if (dsmManager) {
    dsmBufferService = dsmManager->GetDsmBuffer();
    if (!dsmBufferService)
      DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);
  }

  if (dsmBufferService->Get((H5FDdsmAddr) addr, (H5FDdsmUInt64) len,
      (H5FDdsmPointer) buf_ptr) != H5FD_DSM_SUCCESS)
    DSM_DRIVER_GOTO_ERROR("Cannot get data", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_write(haddr_t addr, size_t len, const void *buf_ptr)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;

  FUNC_ENTER_NOAPI(dsm_write, FAIL)

  if (dsmManager) {
    dsmBufferService = dsmManager->GetDsmBuffer();
    if (!dsmBufferService)
      DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);
  }

  if (dsmBufferService->Put((H5FDdsmAddr) addr, (H5FDdsmUInt64) len,
      (H5FDdsmPointer) buf_ptr) != H5FD_DSM_SUCCESS)
    DSM_DRIVER_GOTO_ERROR("Cannot put data", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}
//--------------------------------------------------------------------------
herr_t
dsm_set_modified()
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;

  FUNC_ENTER_NOAPI(dsm_set_modified, FAIL)

  if (dsmManager) {
    dsmBufferService = dsmManager->GetDsmBuffer();
    if (!dsmBufferService)
      DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);
  }

  dsmBufferService->SetIsDataModified(H5FD_DSM_TRUE);
  if (dsmBufferService->GetNotificationOnClose()) {
    if (SUCCEED != dsm_notify(H5FD_DSM_NEW_DATA))
      DSM_DRIVER_GOTO_ERROR("cannot notify DSM", FAIL);
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}

//--------------------------------------------------------------------------
herr_t
dsm_notify(unsigned long flags)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;

  FUNC_ENTER_NOAPI(dsm_notify, FAIL)

  if (dsmManager) {
    dsmBufferService = dsmManager->GetDsmBuffer();
    if (!dsmBufferService)
      DSM_DRIVER_GOTO_ERROR("No DSM buffer found", FAIL);
  } else {
    DSM_DRIVER_GOTO_ERROR("No DSM manager found", FAIL);
  }

  switch(flags) {
    case H5FD_DSM_NEW_INFORMATION:
    case H5FD_DSM_NEW_DATA:
      dsmBufferService->SetNotification(flags);
      break;
    default:
      PRINT_DSM_DRIVER_INFO(dsmManager->GetUpdatePiece(), "Not implemented notification");
      break;
  }

  if (!dsmBufferService->GetIsLocked()) {
    if (dsmBufferService->RequestLockAcquire() != H5FD_DSM_SUCCESS)
      DSM_DRIVER_GOTO_ERROR("Cannot request lock acquisition", FAIL);
  }

  if (!dsmBufferService->GetIsServer()) {
    if (dsmBufferService->RequestNotification() != H5FD_DSM_SUCCESS)
      DSM_DRIVER_GOTO_ERROR("Cannot request notification", FAIL);
  } else {
    if (dsmBufferService->SignalNotification() != H5FD_DSM_SUCCESS)
      DSM_DRIVER_GOTO_ERROR("Cannot signal notification", FAIL);
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}
