#ifndef WIN32
  #define HAVE_PTHREADS
  extern "C" {
    #include <pthread.h>
  }
#elif HAVE_BOOST_THREADS
  #include <boost/thread/thread.hpp> // Boost Threads
#endif

// Xdmf/DSM features 
#include "H5FDdsm.h"
#include "H5FDdsmManager.h"

// Sys
#include <sstream>
#include <mpi.h>

#ifdef _WIN32
  #include <windows.h> // sleep
  #define sleep ::Sleep
#endif

#ifdef HAVE_PTHREADS
    pthread_t      ServiceThread;
#elif HAVE_BOOST_THREADS
    boost::thread *ServiceThread;
#endif

typedef std::vector<double>::size_type itype;

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

//----------------------------------------------------------------------------
std::string usage = "\n"\
    "Usage : ConvertToXdmf \n" \
    " Win32  : ConvertToXdmf -D D:/data/xdmf -F elements.vtu -O D:/data/xdmf/test \n" \
    " Win32  : ConvertToXdmf -D D:/data/VectorField -F damBreakMarin.pvd -O D:/data/xdmf/test \n" \
    " Win32  : ConvertToXdmf -D D:/data/cfxtestdata/Example -F TRS_Default_003.res -O D:/data/xdmf/test \n" \
    " Win32  : ConvertToXdmf -D C:/data/xdmf/test -F TRS_Default_003.xmf -O C:/data/xdmf/scratch \n" \
    " laptop : ConvertToXdmf -D C:\\Data\\cfxtestdata\\Example -F TRS_Default_003.res -O C:\\Data\\cfxtestdata\\xml \n" \
    " Linux  : ConvertToXdmf -r -b -s -D /project/csvis/CFXData/Francis -F 1800_001.res -O /project/csvis/CFXData/XML \n" \
    "  -D /project/csvis/CFXData/Francis \n"        \
    "  -F 1800_001.res \n"                          \
    "  -O output directory \n"                      \
    "  -i timestep - interpolate data using timestep between outputs \n" \
    "  -r rotate vectors for stn frame \n"          \
    "  -f flip 180degrees (csf data) \n"            \
    "  -a append to existing set (otherwise overwrite) \n" \
    "  -s Force Static flag \n" \
    "  -v Subdivide cells (only Tetrahedra + Hexahedra supported currently) \n" \
    "  -Client Send data from DSM Client to DSM Server \n" \
    "  -Server Wait for data from DSM Client \n" \
    "  -DSM Write to self contained DSM \n";
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
void ThreadExecute(void *dsm, H5FDdsmInt64 &counter) {
  H5FDdsmManager *DSM = (H5FDdsmManager*)dsm;
  if (DSM->GetDsmUpdateReady()) {
    DSM->H5DumpLight();
    DSM->ClearDsmUpdateReady();
    counter ++;
  }
};
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
#ifdef HAVE_PTHREADS
  // nothing required
#elif HAVE_BOOST_THREADS
class DSMListenThread {
  public:
    DSMListenThread(H5FDdsmManager *dsm)
    {
      this->dsmManager = dsm;
      Counter          = 0;
      UpdatesCounter   = 0;
    }
    void operator()() {
      while (this->dsmManager) {
        UpdatesCounter ++;
        ThreadExecute(this->dsmManager, Counter);
        std::cout << UpdatesCounter << " : " << Counter << std::endl;
        // somed delay here ?
      }
    }
    //
    H5FDdsmManager *dsmManager;
    H5FDdsmInt64    Counter;
    H5FDdsmInt64    UpdatesCounter;
};
#endif
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
int main (int argc, char* argv[])
{
  int provided, rank, size;
  MPI_Comm dcomm = MPI_COMM_WORLD;

  //
  // Receiver will spawn a thread to handle incoming data Put/Get requests
  // we must therefore have MPI_THREAD_MULTIPLE 
  //
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
  std::cout << "Process number " << rank << " of " << size << std::endl;

  //
  // Pause for debugging
  //
#ifdef H5FD_TEST_WAIT
  if (rank == 0) {
    std::cout << "Attach debugger if necessary, then press <enter>" << std::endl;
    char c;
    std::cin >> c;
  }
#endif

  //
  // Sync here
  //
  MPI_Barrier(dcomm);

  //
  // Create a DSM manager
  //
  H5FDdsmManager *dsmManager = new H5FDdsmManager();
  dsmManager->SetCommunicator(dcomm);
  dsmManager->SetLocalBufferSizeMBytes(16);
  dsmManager->SetDsmCommType(H5FD_DSM_COMM_MPI);
  dsmManager->SetDsmIsServer(1);
  dsmManager->SetServerHostName(server_name.c_str());
  dsmManager->SetServerPort(default_port_number);
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

  while (!dsmManager->GetDSMHandle()->GetIsConnected()) {
    sleep(10);
  }

  H5FDdsmInt64   Counter = 0;
  bool connected = true;  
  while(connected) {
    if (dsmManager->GetDsmUpdateReady()) {
      std::cout << "File count : " << ++Counter << std::endl;
      //
      // H5Dump
      //
      dsmManager->H5DumpLight();
      //
      // Sync here
      //
      MPI_Barrier(dcomm);
      //
      // Clean up for next step
      //
      dsmManager->ClearDsmUpdateReady();
      dsmManager->RequestRemoteChannel();
    }
    else {
      sleep(1);
    }
    connected = (dsmManager->GetDSMHandle()->GetIsConnected()!=0);
  }

  std::cout << "Process number " << rank << " Closing down DSM server" << std::endl;

  //
  // Sync here
  //
  MPI_Barrier(dcomm);

  //
  // Closes ports or MPI communicators
  //
  dsmManager->UnpublishDSM();

  //
  // Clean up everything
  // 
  delete dsmManager;

  MPI_Finalize();
  return(EXIT_SUCCESS);
}
