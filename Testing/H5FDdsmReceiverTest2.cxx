#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
#include <mpi.h>
#include <hdf5.h>
//
#include "H5FDdsmManager.h"
#include "H5FDdsm.h"

// Names of datasets and groups
#define GROUPNAME       "Scalar"
#define DATASETNAME     "Pressure"

// data structures used when writing
typedef struct ParticleBuffer {
  double *Ddata;
} ParticleBuffer_t;

#define H5CHECK_ERROR(var, msg) if (var<0) printf("Error %s", msg);

// Sleep  in milliseconds
#ifdef _WIN32
  #include <windows.h> 
  #define sleep(a) ::Sleep(a)
#else
  void mySleep(int ms) {
    usleep(ms*1000); //convert to microseconds
    return;
  }
  #define sleep(a) mySleep(a)
#endif

//----------------------------------------------------------------------------
void particle_read(
    ParticleBuffer_t *buf, const char *filename, int rank,
    H5FDdsmBuffer *dsmBuffer)
{
  hid_t      file_id, group_id, dataset_id;
  hid_t      acc_plist_id;
  herr_t     status = 0;

  // Set up file access property list with parallel I/O
  acc_plist_id = H5Pcreate(H5P_FILE_ACCESS);
  H5CHECK_ERROR(acc_plist_id, "H5Pcreate(H5P_FILE_ACCESS)");
  if (dsmBuffer) {
    H5Pset_fapl_dsm(acc_plist_id, MPI_COMM_WORLD, dsmBuffer);
    H5CHECK_ERROR(status, "H5Pset_fapl_dsm");
  }

  // Create a new file collectively (overwrite existing)
  file_id = H5Fopen(filename, H5F_ACC_RDONLY, acc_plist_id);
  H5CHECK_ERROR(file_id, "H5Fopen");

  // Close property list
  status = H5Pclose(acc_plist_id);
  H5CHECK_ERROR(status, "H5Pclose(acc_plist_id)");

  // Open the group
  group_id = H5Gopen(file_id, GROUPNAME, H5P_DEFAULT);
  H5CHECK_ERROR(group_id, "H5Gopen");

  // Open the dataset
  dataset_id = H5Dopen(group_id, DATASETNAME, H5P_DEFAULT);
  H5CHECK_ERROR(dataset_id, "H5Dopen");

  // Read the dataset
  if (rank == 0) {
    status = H5Dread(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, (*buf).Ddata);
    H5CHECK_ERROR(dataset_id, "H5Dread");
  }

  // Release resources
  status = H5Dclose(dataset_id);
  H5CHECK_ERROR(status, "H5Dclose");
  status = H5Gclose(group_id);
  H5CHECK_ERROR(status, "H5Gclose");
  status = H5Fclose(file_id);
  H5CHECK_ERROR(status, "H5Fclose");
}
//----------------------------------------------------------------------------
void initBuffer(ParticleBuffer_t *buffer) {
  (*buffer).Ddata = NULL;
}
//----------------------------------------------------------------------------
void freeBuffer(ParticleBuffer_t *buffer) {
  if ((*buffer).Ddata) free((*buffer).Ddata);
}
//----------------------------------------------------------------------------
double TestParticleRead(const char *filename, int rank, hsize_t N,
    MPI_Comm dcomm, H5FDdsmBuffer *dsmBuffer)
{
  ParticleBuffer_t ReadBuffer;
  hsize_t   i;
  double   *doublearray;
  int fail_count = 0;
  static int step_increment = 0;

  // set all array pointers to zero
  initBuffer(&ReadBuffer);

  // create arrays for the test vars we selected above
  doublearray = (double*)malloc(sizeof(double)*N);
  for (i=0; i<N; i++) {
    doublearray[i] = 0;
  }
  ReadBuffer.Ddata = doublearray;

  // call the write routine with our dummy buffer
  MPI_Barrier(dcomm);
  double t1 = MPI_Wtime();
  particle_read(&ReadBuffer, filename, rank, dsmBuffer);
  MPI_Barrier(dcomm);
  double t2 = MPI_Wtime();

  if (rank == 0) {
    /* Check the results. */
    for (i=0; i<N; i++) {
      if((doublearray[i] != (i + step_increment)) && (fail_count<10)) {
        fprintf(stderr," doublearray[%llu] is %llu, should be %llu\n", i, (hsize_t)doublearray[i], (i+step_increment));
        fail_count++;
      }
    }
    if (fail_count == 0) {
        // if (rank == 0) printf("DSM read test PASSED\n");
    }
    else {
      fprintf(stderr,"DSM read test FAILED for PE %d, %d or more wrong values\n", rank, fail_count);
    }
  }
  // free all array pointers
  freeBuffer(&ReadBuffer);
  step_increment++;
  return t2-t1;
};
//----------------------------------------------------------------------------
int main (int argc, char* argv[])
{
  int provided, rank, size, nremoteprocs;
  MPI_Comm dcomm = MPI_COMM_WORLD;
  char fullname[16] = "dsm";
  hsize_t numParticles = 0;
  long DSMSize = 16; // default MB
  int commType = H5FD_DSM_COMM_SOCKET;

  if (argc > 1) {
    DSMSize = atol(argv[1]);
  }

  if (argc > 2) {
    if (!strcmp(argv[2], "Socket")) {
      commType = H5FD_DSM_COMM_SOCKET;
      std::cout << "SOCKET Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "MPI")) {
      commType = H5FD_DSM_COMM_MPI;
      std::cout << "MPI Inter-Communicator selected" << std::endl;
    }
    else if (!strcmp(argv[2], "MPI_RMA")) {
      commType = H5FD_DSM_COMM_MPI_RMA;
      std::cout << "MPI_RMA Inter-Communicator selected" << std::endl;
    }
  }

  // Receiver will spawn a thread to handle incoming data Put/Get requests
  // we must therefore have MPI_THREAD_MULTIPLE 
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
  MPI_Comm_rank(dcomm, &rank);
  MPI_Comm_size(dcomm, &size);
  //
  if (rank == 0) {
    if (provided != MPI_THREAD_MULTIPLE) {
      std::cout << "MPI_THREAD_MULTIPLE not set, you may need to recompile your "
          << "MPI distribution with threads enabled" << std::endl;
    }
    else {
      std::cout << "MPI_THREAD_MULTIPLE is OK" << std::endl;
    }
  }
  std::cout << "Process number " << rank << " of " << size - 1 << std::endl;

  // Pause for debugging
#ifdef H5FD_TEST_WAIT
  if (rank == 0) {
    std::cout << "Attach debugger if necessary, then press <enter>" << std::endl;
    char c;
    std::cin >> c;
  }
#endif

  // Sync here
  MPI_Barrier(dcomm);

  // Create a DSM manager
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  dsmManager->SetCommunicator(dcomm);
  dsmManager->SetLocalBufferSizeMBytes(DSMSize/size);
  dsmManager->SetDsmCommType(commType);
  dsmManager->SetDsmIsServer(1);
  dsmManager->SetServerHostName("default");
  dsmManager->SetServerPort(22000);
  dsmManager->CreateDSM();
  // Publish writes .dsm_config file with server name/port/mode in
  // then spawns thread which waits for incoming connections
  dsmManager->PublishDSM();
  //
  int totalMB = static_cast<int>(dsmManager->GetDSMHandle()->GetTotalLength()/(1024*1024));
  int serversize = dsmManager->GetDSMHandle()->GetEndServerId();
  if (rank == 0) {
    std::cout << "DSM server memory size is : " << totalMB << " MB" << std::endl;
    std::cout << "DSM server process count  : " <<  (serversize+1) << std::endl;
  }

  // The output comment below must not be deleted, it allows ctest to detect
  // when the server is initialized
  sleep(100);
  std::cout << "Waiting for client..." << std::endl;
  dsmManager->WaitForConnected();

  while(dsmManager->GetDsmIsConnected()) {
    if (dsmManager->WaitForUpdateReady() > 0) {
      // H5Dump
      // dsmManager->H5DumpLight();
      nremoteprocs = dsmManager->GetDSMHandle()->GetComm()->GetInterSize();
      numParticles = (hsize_t) (1024*1024*((dsmManager->GetDSMHandle()->GetTotalLength()/(1024.0*1024.0))-1)/(sizeof(double)*nremoteprocs));
      // Check data
      if (rank == 0) {
        // printf("Trying to read %d * %llu particles\n", nremoteprocs, numParticles);
      }
      TestParticleRead(fullname, rank, nremoteprocs*numParticles, dcomm, dsmManager->GetDSMHandle());
      // Sync here
      MPI_Barrier(dcomm);
      // Clean up for next step
      dsmManager->UpdateFinalize();
    }
  }

  std::cout << "Process number " << rank << " Closing down DSM server" << std::endl;

  // Sync here
  MPI_Barrier(dcomm);

  // Closes ports or MPI communicators
  dsmManager->UnpublishDSM();

  // Clean up everything
  delete dsmManager;

  MPI_Finalize();
  return(EXIT_SUCCESS);
}
