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
  H5FDdsmInt32 intScalarSet, intVectorSet;
  H5FDdsmInt32 doubleScalarSet, doubleVectorSet;
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

//  H5FD_dsm_steering_wait();
  // Step 2: steered values are retrieved
  H5FD_dsm_steering_is_set("IntScalarTest", &intScalarSet);
  assert(intScalarSet);
  if (intScalarSet) {
    H5FDdsmBoolean intScalar;
    H5FD_dsm_steering_scalar_get("IntScalarTest", H5T_NATIVE_INT, &intScalar);
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Got scalar value IntScalarTest: " << intScalar << std::endl;
    }
    assert(intScalar);
  }

  H5FD_dsm_steering_is_set("IntVectorTest", &intVectorSet);
  assert(intVectorSet);
  if (intVectorSet) {
    H5FDdsmInt32 intVector[3];
    H5FD_dsm_steering_vector_get("IntVectorTest", H5T_NATIVE_INT, 3, intVector);
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Got vector values IntVectorTest: " << intVector[0] << ", "
          << intVector[1] << ", " << intVector[2] << std::endl;
    }
    for (int i = 0; i < 3; i++) assert(intVector[i] == (i + 1));
  }

  H5FD_dsm_steering_is_set("DoubleScalarTest", &doubleScalarSet);
  assert(doubleScalarSet);
  if (doubleScalarSet) {
    H5FDdsmFloat64 doubleScalar;
    H5FD_dsm_steering_scalar_get("DoubleScalarTest", H5T_NATIVE_DOUBLE, &doubleScalar);
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Got scalar value DoubleScalarTest: " << doubleScalar << std::endl;
    }
    assert(doubleScalar==3.14);
  }

  H5FD_dsm_steering_is_set("DoubleVectorTest", &doubleVectorSet);
  assert(doubleVectorSet);
  if (doubleVectorSet) {
    H5FDdsmFloat64 doubleVector[3];
    H5FD_dsm_steering_vector_get("DoubleVectorTest", H5T_NATIVE_DOUBLE, 3, doubleVector);
    if (dsmManager->GetUpdatePiece() == 0) {
      std::cout << "Got vector values DoubleVectorTest: " <<  doubleVector[0] << ", "
          << doubleVector[1] << ", " << doubleVector[2] << std::endl;
    }
    for (int i = 0; i < 3; i++) assert(doubleVector[i] == (i + 1.001));
  }

  senderFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
