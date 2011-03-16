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
    int len,  int start, int total, H5FDdsmBuffer *dsmBuffer)
{
  hid_t      file_id, group_id, dataset_id, xfer_plist_id;
  hid_t      file_space_id, mem_space_id;
  hid_t      acc_plist_id;
  hsize_t    count[2]    = {total, 3};
  hsize_t    offset[2]   = {start, 0};
  hsize_t    localdim[2] = {len, 3};
  herr_t     status = 0;

  /* Set up file access property list with parallel I/O */
  acc_plist_id = H5Pcreate(H5P_FILE_ACCESS);
  H5CHECK_ERROR(acc_plist_id, "H5Pcreate(H5P_FILE_ACCESS)");
  if (dsmBuffer) {
    H5FD_dsm_init();
    H5Pset_fapl_dsm(acc_plist_id, MPI_COMM_WORLD, dsmBuffer);
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
  H5CHECK_ERROR(status, "H5Pclose(acc_plist_id)");

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
  if (dsmBuffer) {
    status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, mem_space_id, file_space_id, H5P_DEFAULT, (*buf).Ddata);
    H5CHECK_ERROR(status, "H5Dwrite");
  } else {
    xfer_plist_id = H5Pcreate(H5P_DATASET_XFER);
    H5CHECK_ERROR(xfer_plist_id, "H5Pcreate(H5P_DATASET_XFER)");
    H5Pset_dxpl_mpio(xfer_plist_id, H5FD_MPIO_COLLECTIVE);
    status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, mem_space_id, file_space_id, xfer_plist_id, (*buf).Ddata);
    H5CHECK_ERROR(status, "H5Dwrite");
    status = H5Pclose(xfer_plist_id);
    H5CHECK_ERROR(status, "H5Pclose(xfer_plist_id)");
  }
  H5CHECK_ERROR(status, "H5Dwrite");

  /* Release resources */
  status = H5Sclose(mem_space_id);
  H5CHECK_ERROR(status, "H5Sclose");
  status = H5Dclose(dataset_id);
  H5CHECK_ERROR(status, "H5Dclose");
  status = H5Sclose(file_space_id);
  H5CHECK_ERROR(status, "H5Sclose");
  status = H5Gclose(group_id);
  H5CHECK_ERROR(status, "H5Gclose");
  status = H5Fclose(file_id);
  H5CHECK_ERROR(status, "H5Fclose");
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
    MPI_Comm dcomm, H5FDdsmBuffer *dsmBuffer)
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
  write_ecrit_particule(&WriteBuffer, filename, N, start, total, dsmBuffer);
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
#define LOOPS       5
#define AVERAGE     5
#define TYPES       1 // 2 if disk output test required

int main(int argc, char **argv)
{
  int            nlocalprocs, rank, size;
  MPI_Comm       dcomm = MPI_COMM_WORLD;
  double         remoteMB, MBytes, GBytes, Bytes, SendBytes, bandwidth;
  double         totaltime;
  char           fullname[256] = "dsm";
  bool           staticInterComm = false;

  //
  // Sender does not use any special threads so MPI_Init is ok
  //
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(dcomm,&rank);
  MPI_Comm_size(dcomm,&nlocalprocs);

  //
  // Pause for debugging
  //
#ifdef H5FD_TEST_WAIT
  if (rank == 0) {
    std::cout << "Attach debugger if necessary, then press <enter>" << std::endl;
    char c;
    std::cin >> c;
  }
  MPI_Barrier(dcomm);
#endif

  const char *dsm_env = getenv("H5FD_DSM_CONFIG_PATH");
  std::string hdffile;
  if (dsm_env) {
    hdffile = std::string(dsm_env) + std::string("/hdf-output.h5");
    if (rank == 0) {
      std::cout << "HDF output goes to : " << hdffile.c_str() << std::endl;
    }
  }

  //
  // Create a DSM manager
  //
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  dsmManager->ReadDSMConfigFile();
  if (dsmManager->GetDsmUseStaticInterComm()) {
    int color = 2; // 1 for server, 2 for client
    MPI_Comm_split(MPI_COMM_WORLD, color, rank, &dcomm);
    MPI_Comm_rank(dcomm, &rank);
    MPI_Comm_size(dcomm, &size);
    staticInterComm = true;
  }
  dsmManager->SetCommunicator(dcomm);
  dsmManager->SetDsmIsServer(0);
  dsmManager->CreateDSM();

  if (staticInterComm) {
    dsmManager->ConnectInterCommDSM();
  } else {
    //
    // Connect to receiver
    //
    dsmManager->ConnectDSM(true);
  }

  //
  // We have configured everything manually using the DSM manager, so pass the buffer
  // into the read/write code so that we can use the dsm that we have setup
  // otherwise it creates a new DSM server object
  //
  H5FDdsmBuffer *dsmBuffer = dsmManager->GetDSMHandle();

  //
  // Get info from remote server
  //
  remoteMB = dsmBuffer->GetTotalLength()/(1024.0*1024.0);
  double numServers = dsmBuffer->GetEndServerId()+1;
  if (rank == 0) {
    std::cout << "DSM server memory size is : " << (int)remoteMB << " MB" << std::endl;
    std::cout << "DSM server process count  : " <<  (int)numServers << std::endl;
  }

  for (int type=0; type<TYPES; type++) {
    if (type==0 && rank == 0) {
      std::cout << "Writing to DSM" << std::endl;
    }
    else if (type==1 && rank == 0) {
      std::cout << "Writing to Disk" << std::endl;
    }
    for (int loop=0; loop<LOOPS; loop++) {
      double numParticles = 1024*1024*(remoteMB-1)/(sizeof(double)*3.0*nlocalprocs);
      // double numParticles = Lengths[length];

      Bytes       = numParticles*sizeof(double)*3.0; // 3 = {x,y,z}
      SendBytes   = Bytes*(double)nlocalprocs;
      MBytes      = SendBytes/(1024.0*1024.0);
      GBytes      = MBytes/(1024.0);
      if (MBytes<remoteMB) {
        totaltime = 0;
        for (int avg=0; avg<AVERAGE; avg++) {
          if (type==0) {
            totaltime += TestParticleWrite(fullname, (int)numParticles, rank, nlocalprocs, dcomm, dsmBuffer);
          }
          else if (type==1) {
            totaltime += TestParticleWrite(hdffile.c_str(), (int)numParticles, rank, nlocalprocs, dcomm, NULL);
          }
        }
        totaltime = totaltime/AVERAGE;
        bandwidth = (MBytes/totaltime);
        if (rank==0) {
          std::cout << "Particles, "        << numParticles << ", "
                    << "NumArrays, "        << 3            << ", "
                    << "NProcs, "           << nlocalprocs  << ", "
                    << "Mbytes, "           << MBytes       << ", "
                    << "TotalTime, "        << totaltime    << ", "
                    << "Bandwidth (MB/s), " << bandwidth    << std::endl;
        }
      }
    }
  }

  dsmManager->DisconnectDSM();

  TestParticleClose();
  delete dsmManager;

  if (staticInterComm) {
   MPI_Comm_free(&dcomm);
  }
  MPI_Finalize();

  return(EXIT_SUCCESS);
}
//----------------------------------------------------------------------------

