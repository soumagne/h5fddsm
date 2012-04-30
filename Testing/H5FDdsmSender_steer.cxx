#include "H5FDdsmTest.h"
#include "H5FDdsm.h"
#include "H5FDdsmSteering.h"
//
#include <hdf5.h>
#include <cstdlib>
#include <cassert>

//----------------------------------------------------------------------------
int main(int argc, char **argv)
{
  MPI_Comm comm = MPI_COMM_WORLD;
  hbool_t intScalarSet, intVectorSet;
  hbool_t doubleScalarSet, doubleVectorSet;
  H5FDdsmInt32 clientScalar = 2;
  H5FDdsmInt32 clientVector[] = {4, 5, 6};
  hid_t steering_handle;
  H5FDdsmFloat64 remoteMB;
  H5FDdsmUInt64 numParticles;
  H5FDdsmConstString fullname = "dsm";
  H5FDdsmManager *dsmManager = new H5FDdsmManager();

  senderInit(argc, argv, dsmManager, &comm);

  H5FD_dsm_steering_init(comm);

  remoteMB = dsmManager->GetDsmBuffer()->GetTotalLength() / (1024.0 * 1024.0);
  numParticles = (H5FDdsmUInt64) ((1024 * 1024 * remoteMB / 2) /
      (sizeof(H5FDdsmFloat64) * DIM_DATASETS * dsmManager->GetUpdateNumPieces()));

  // Step 0: array is enabled and written
  assert(H5FD_dsm_steering_is_enabled(DATASETNAME));
  if (H5FD_dsm_steering_is_enabled(DATASETNAME)) {
    TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS, dsmManager->GetUpdatePiece(),
        dsmManager->GetUpdateNumPieces(), comm, dsmManager, usingHDF);
  }

  // Must be collective
  H5FD_dsm_steering_update();

  // Step 1: array is set disabled
  assert(!H5FD_dsm_steering_is_enabled(DATASETNAME));
  if (H5FD_dsm_steering_is_enabled(DATASETNAME)) {
    TestParticleWrite(fullname, numParticles, DIM_DATASETS, NUM_DATASETS, dsmManager->GetUpdatePiece(),
        dsmManager->GetUpdateNumPieces(), comm, dsmManager, usingHDF);
  }

  // Must be collective
  H5FD_dsm_steering_update();

  // Start steering query (Must be collective)
  H5FD_dsm_steering_begin_query();

  // Step 2: steered values are retrieved
  // Must not be collective, requires to use H5FD_dsm_steering_begin_query first
  if (dsmManager->GetUpdatePiece() == 0) {
    // IntScalarTest
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
    // DoubleScalarTest
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
    // IntVectorTest
    H5FD_dsm_steering_is_set("IntVectorTest", &intVectorSet);
    assert(intVectorSet);
    if (intVectorSet) {
      H5FDdsmInt32 intVector[3];
      H5FD_dsm_steering_vector_get("IntVectorTest", H5T_NATIVE_INT, 3, intVector);

      std::cout << "Got vector values IntVectorTest: " << intVector[0] << ", "
          << intVector[1] << ", " << intVector[2] << std::endl;

      for (int i = 0; i < 3; i++) assert(intVector[i] == (i + 1));
    }
    // DoubleVectorTest
    H5FD_dsm_steering_is_set("DoubleVectorTest", &doubleVectorSet);
    assert(doubleVectorSet);
    if (doubleVectorSet) {
      H5FDdsmFloat64 doubleVector[3];

      H5FD_dsm_steering_vector_get("DoubleVectorTest", H5T_NATIVE_DOUBLE, 3, doubleVector);
      std::cout << "Got vector values DoubleVectorTest: " <<  doubleVector[0] << ", "
          << doubleVector[1] << ", " << doubleVector[2] << std::endl;
      for (int i = 0; i < 3; i++) assert(doubleVector[i] == (i + 1.001));
    }

    H5FD_dsm_steering_get_handle("DoubleVectorTest", &steering_handle);
    assert(steering_handle);
    if (steering_handle) {
      hsize_t arraySize = 3;
      hid_t memspaceId = H5Screate_simple(1, &arraySize, NULL);
      H5FDdsmFloat64 doubleVector[3];
      H5Dread(steering_handle, H5T_NATIVE_DOUBLE, memspaceId, H5S_ALL, H5P_DEFAULT, &doubleVector);
      std::cout << "Got vector values DoubleVectorTest: " <<  doubleVector[0] << ", "
          << doubleVector[1] << ", " << doubleVector[2] << std::endl;
      for (int i = 0; i < 3; i++) assert(doubleVector[i] == (i + 1.001));

      H5Sclose(memspaceId);
      H5FD_dsm_steering_free_handle(steering_handle);
    }
  }

  // Set scalar/vector (Must be collective)
  H5FD_dsm_steering_scalar_set("ClientScalarTest", H5T_NATIVE_INT, &clientScalar);
  H5FD_dsm_steering_vector_set("ClientVectorTest", H5T_NATIVE_INT, 3, &clientVector);

  // End query (Must be collective)
  H5FD_dsm_steering_end_query();

  senderFinalize(dsmManager, &comm);
  delete dsmManager;
  return(EXIT_SUCCESS);
}
