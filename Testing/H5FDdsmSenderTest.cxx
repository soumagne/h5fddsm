#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <string>
//
#include "mpi.h"
#include "hdf5.h"
//
#include "H5FDdsm.h"
#include "H5FDdsmManager.h"

//----------------------------------------------------------------------------
//
//----------------------------------------------------------------------------

/* Name of dataset_id to create in loc_id */
#define GROUPNAME       "Step#0"
#define TIME_ATTRIBUTE  "TimeValue"

#ifdef MACHINE_AGNO
  #define server "agno.staff.cscs.ch"
  #define client "agno.staff.cscs.ch"
  #define PORT 22000
#endif

#ifdef MACHINE_DINO
  #define server "agno.staff.cscs.ch"
  #define client "dino.staff.cscs.ch"
  #define PORT 22000
#endif

#ifdef MACHINE_BRENO
  #define server "agno.staff.cscs.ch"
  #define client "breno.staff.cscs.ch"
  #define PORT 22000
#endif

#ifndef server
  #define server "agno.staff.cscs.ch"
  #define client "breno.staff.cscs.ch"
  #define PORT 22000
#endif

std::string server_name = server;
std::string client_name = client;
int default_port_number = PORT;

/*******************************************************************/
/* data structures used when writing                               */
/*******************************************************************/
// used for comparing H5Part with Compound H5SPH
typedef struct EcritParticuleHDFTesting {
  double *Ddata;
} EcritParticuleHDFTesting;

/* names of above arrays for convenience when using H5Part IO */
const char ArrayNames[] = "Position";

#define H5CHECK_ERROR(var, msg) if (var<0) printf("Error %s", msg);

/*******************************************************************/
/* len   = number this process will write                          */
/* start = start index for this write                              */
/* total = total to be written by all all processes                */
/* collective : 1=collective IO, 0=independent (default 0)         */
/*******************************************************************/
void write_ecrit_particule(
    EcritParticuleHDFTesting *buf, const char *filename,
    int len,  int start, int total, int use_dsm)
{
  hid_t      file_id, group_id, dataset_id;
  hid_t      file_space_id, mem_space_id;
  hid_t      acc_plist_id;
  hsize_t    count[2]    = {total, 3};
  hsize_t    offset[2]   = {start, 0};
  hsize_t    localdim[2] = {len, 3};
  herr_t     status = 0;

  /* Set up file access property list with parallel I/O */
  acc_plist_id = H5Pcreate(H5P_FILE_ACCESS);
  H5CHECK_ERROR(acc_plist_id, "H5Pcreate access");
  if (use_dsm) {
    H5FD_dsm_init();
    H5Pset_fapl_dsm(acc_plist_id, H5FD_DSM_INCREMENT, NULL);
    H5CHECK_ERROR(status, "H5Pset_fapl_dsm");
  } else {
    H5Pset_fapl_mpio(acc_plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);
    H5CHECK_ERROR(status, "H5Pset_fapl_mpio");
  }

  /* Create a new file collectively (overwrite existing) */
  file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_plist_id);
  H5CHECK_ERROR(file_id, "H5Fcreate");

  /* Close property list */
  status = H5Pclose(acc_plist_id);
  H5CHECK_ERROR(status, "H5Pclose");

  /* Create the file_space_id */
  file_space_id = H5Screate_simple(2, count, NULL);
  H5CHECK_ERROR(file_space_id, "H5Screate_simple");

  // Create a group to hold this time step (for H5Part compatibility)
  group_id = H5Gcreate(file_id, GROUPNAME, 0, H5P_DEFAULT, H5P_DEFAULT);
  H5CHECK_ERROR(group_id, "H5Gopen");

  /* Create Dataset, Write Data, Close Dataset */
  dataset_id = H5Dcreate(group_id, ArrayNames, H5T_NATIVE_DOUBLE, file_space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5CHECK_ERROR(dataset_id, "H5Dcreate");

  /* create a data space for particles local to this process */
  mem_space_id = H5Screate_simple(2, localdim, NULL);
  H5CHECK_ERROR(mem_space_id, "H5Screate_simple");

  /* select a hyperslab into the filespace for our local particles */
  status = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset, NULL, localdim, NULL );
  H5CHECK_ERROR(status, "H5Sselect_hyperslab");

  /* Create Dataset, Write Data, Close Dataset */
  status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, mem_space_id, file_space_id, H5P_DEFAULT, (*buf).Ddata);
  H5CHECK_ERROR(status, "H5Dwrite");

  /* Release resources */
  H5Sclose(mem_space_id);
  status = H5Dclose(dataset_id);
  H5CHECK_ERROR(status, "H5Dclose");
  H5Sclose(file_space_id);
  H5Gclose(group_id);
  H5Fclose(file_id);
}
//----------------------------------------------------------------------------
// Test Particle write (H5Part compatible)
//----------------------------------------------------------------------------
void initBuffer(EcritParticuleHDFTesting *buffer) {
  (*buffer).Ddata = NULL;
}
//----------------------------------------------------------------------------
void freeBuffer(EcritParticuleHDFTesting *buffer) {
  if ((*buffer).Ddata) free((*buffer).Ddata);
}
//----------------------------------------------------------------------------
double TestParticleWrite(const char *filename, int N, int mpiId, int mpiNum,
    MPI_Comm dcomm, int use_dsm)
{
  EcritParticuleHDFTesting WriteBuffer;
  int       i, start, total, memsize;
  double   *doublearray;

  start = N*mpiId;
  total = N*mpiNum;
  // set all array pointers to zero
  initBuffer(&WriteBuffer);

  // create arrays for the test vars we selected above
  memsize = sizeof(double)*N;
  doublearray = (double*)malloc(memsize*3);
  for (i=0; i<N; i++) {
    doublearray[3*i]   = 0.1*i;
    doublearray[3*i+1] = 0.1*i;
    doublearray[3*i+2] = 0.1*i;
  }
  WriteBuffer.Ddata = doublearray;

  // call the write routine with our dummy buffer
  MPI_Barrier(dcomm);
  double t1 = MPI_Wtime();
  write_ecrit_particule(&WriteBuffer, filename, N, start, total, use_dsm);
  MPI_Barrier(dcomm);
  double t2 = MPI_Wtime();

  // free all array pointers
  freeBuffer(&WriteBuffer);
  return t2-t1;
};
//----------------------------------------------------------------------------
void TestParticleClose()
{
  H5close();
}
//----------------------------------------------------------------------------
//
//----------------------------------------------------------------------------
#define MAX_LENGTH  8
#define AVERAGE     5

int main(int argc, char **argv)
{
  int            nprocs,rank, loop, length;
  int            size;
  int            Lengths[MAX_LENGTH] = { 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000 };
  MPI_Comm       dcomm = MPI_COMM_WORLD;
  double         Mbytes, Gbytes, bytes, totalbytes;
  double         threshold;
  double         th, t_h5part[AVERAGE];
  char           fullname[256] = "stdin";
  int            use_dsm = 1;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(dcomm,&rank);
  MPI_Comm_size(dcomm,&nprocs);

  //
  // Pause for debugging
  //
  std::cout << "Attach debugger if necessary, then press <enter>" << std::endl;
  char c;
  std::cin >> c;

  //
  // Create a DSM manager
  //
  H5FDdsmManager *dsmManager = H5FDdsmManager::New();
  dsmManager->SetCommunicator(dcomm);
  dsmManager->SetDsmIsServer(0);
  dsmManager->SetDsmCommType(H5FD_DSM_COMM_MPI);
  dsmManager->SetServerHostName(server_name.c_str());
  dsmManager->SetServerPort(default_port_number);
  dsmManager->ReadDSMConfigFile();
  dsmManager->CreateDSM();

  //
  // Sync here
  //
  MPI_Barrier(dcomm);

  //
  // Connect to receiver
  //
  dsmManager->ConnectDSM();

  threshold = 1.25*(log((nprocs+1.0))/std::log(2.0));
  threshold = 1024.0*threshold;
  if (rank==0) printf("Threshold IO MBytes = %10.5f\n", threshold);

  for (length=0; length<MAX_LENGTH; length++) {
    for (loop=0; loop<AVERAGE; loop++) {
      size       = Lengths[length];
      bytes      = (double)size*sizeof(double)*3;
      totalbytes = bytes*(double)nprocs;
      Mbytes     = totalbytes/(1024.0*1024.0);
      Gbytes     = Mbytes/(1024.0);
      //          if (Mbytes>threshold) continue;
      t_h5part[loop]   = TestParticleWrite(fullname, size, rank, nprocs, dcomm, use_dsm);
    }
    th = 0;
    for (loop=0; loop<AVERAGE; loop++) {
      th = th + t_h5part[loop];
    }
    th = th/AVERAGE;
    if (rank==0) printf("method, custom, collective, 0, particles, %09i, NumArrays, 3, NProcs, %05i, MBytes, %10.5f, time, %9.4f\n", size, nprocs, Mbytes, th);
  }

  TestParticleClose();

  MPI_Finalize();

  return(EXIT_SUCCESS);
}
//----------------------------------------------------------------------------

