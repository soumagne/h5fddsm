#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
#include "H5FDdsmSteering.h"
//
#include <hdf5.h>
#include <cstdlib>
#include <cassert>

#define NUM_DATASETS  1
#define DIM_DATASETS  3
//----------------------------------------------------------------------------
int main(int argc, char **argv)
{
  MPI_Comm comm = MPI_COMM_WORLD;
  H5FDdsmFloat64 remoteMB;
  H5FDdsmUInt64 numParticles;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmManager *dsmManager = new H5FDdsmManager();

  senderInit(argc, argv, dsmManager, &comm);
  H5FD_dsm_steering_init(comm, dsmManager->GetDSMHandle());

  remoteMB = dsmManager->GetDSMHandle()->GetTotalLength() / (1024.0 * 1024.0);
  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB / 2) /
      (sizeof(H5FDdsmFloat64) * DIM_DATASETS * dsmManager->GetUpdateNumPieces()));

  // Step 0: array is enabled and written
  assert(H5FD_dsm_steering_is_enabled("Position") == 0);
  if (H5FD_dsm_steering_is_enabled("Position") == 0) {
    TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS, dsmManager->GetUpdatePiece(),
        dsmManager->GetUpdateNumPieces(), comm, dsmManager->GetDSMHandle(), usingHDF);
  }

  H5FD_dsm_steering_update();
  // Step 1: array is set disabled
  assert(H5FD_dsm_steering_is_enabled("Position") < 0);
  if (H5FD_dsm_steering_is_enabled("Position") == 0) {
    TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS, dsmManager->GetUpdatePiece(),
        dsmManager->GetUpdateNumPieces(), comm, dsmManager->GetDSMHandle(), usingHDF);
  }

  H5FD_dsm_steering_update();

  H5FD_dsm_steering_wait();
  // Step 2: steered values are retrieved
  if (dsmManager->GetUpdatePiece() == 0) {
    H5FDdsmInt32 WaitForGuiSet, NewCenterSet;
    H5FD_dsm_steering_is_set("WaitForGui", &WaitForGuiSet);
    assert(WaitForGuiSet);
    if (WaitForGuiSet) {
      H5FDdsmBoolean waitForGui;
      H5FD_dsm_steering_scalar_get("WaitForGui", H5T_NATIVE_INT, &waitForGui);
      std::cout << "Got scalar value WaitForGui: " <<  waitForGui << std::endl;
      assert(waitForGui);
    }

    H5FD_dsm_steering_is_set("NewCenter", &NewCenterSet);
    assert(NewCenterSet);
    if (NewCenterSet) {
      H5FDdsmInt32 center[3];
      H5FD_dsm_steering_vector_get("NewCenter", H5T_NATIVE_INT, 3, center);
      std::cout << "Got vector values NewCenter: " <<  center[0] << ", "
          << center[1] << ", " << center[2] << std::endl;
      for (int i = 0; i < 3; i++) assert(center[i] == (i + 1));
    }
  }

  senderFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
