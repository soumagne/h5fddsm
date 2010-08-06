/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmManager.h

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing 
  1) This copyright notice appears on all copies of source code 
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it 
  must not be reformatted such that the indentation, bracketing or 
  overall style is modified significantly. 

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=========================================================================*/
// .NAME H5FDdsmManager - Create/Expose an Xdmf DSM to an external application
// .SECTION Description
// Create/Expose an Xdmf DSM to an external application

#ifndef __H5FDdsmManager_h
#define __H5FDdsmManager_h

#include "H5FDdsm.h"
#include "H5FDdsmBuffer.h"
#include "H5FDdsmCommSocket.h"
#include "H5FDdsmCommMpi.h"
#include "H5FDdsmIniFile.h"

#ifndef WIN32
  #define HAVE_PTHREADS
  extern "C" {
    #include <pthread.h>
  }
#elif HAVE_BOOST_THREADS
  #include <boost/thread/thread.hpp> // Boost Threads
#endif

class H5FDdsm_EXPORT H5FDdsmManager : public H5FDdsmObject
{
  public:
     H5FDdsmManager();
    ~H5FDdsmManager();

    // Description:
    // Set/Get the MPI Communicator used by this DSM manager
    void SetCommunicator(MPI_Comm comm);
    H5FDdsmGetValueMacro(Communicator,MPI_Comm);

    // Description:
    // Set/Get the size of the buffer to be reserved on this process
    // the DSM total size will be the sum of the local sizes from all processes
    H5FDdsmSetValueMacro(LocalBufferSizeMBytes,H5FDdsmInt64);
    H5FDdsmGetValueMacro(LocalBufferSizeMBytes,H5FDdsmInt64);

    // Description:
    // Set/Get DsmIsServer info
    H5FDdsmSetValueMacro(DsmIsServer, int);
    H5FDdsmGetValueMacro(DsmIsServer, int);

    // Description:
    // Set/Get the interprocess communication subsystem
    // Valid values are H5FD_DSM_COMM_MPI, H5FD_DSM_COMM_SOCKET
    H5FDdsmSetValueMacro(DsmCommType, int);
    H5FDdsmGetValueMacro(DsmCommType, int);

    // Description:
    // Set/Get the published host name of our connection.
    // Real value valid after a PublishDSM call has been made.
    H5FDdsmSetStringMacro(ServerHostName);
    H5FDdsmGetStringMacro(ServerHostName);

    // Description:
    // Set/Get the published port of our connection.
    // Real value valid after a PublishDSM call has been made.
    H5FDdsmSetValueMacro(ServerPort, int);
    H5FDdsmGetValueMacro(ServerPort, int);

    // Description:
    // Set/Get the DSM configuration file path.
    // If set correctly before calling PublishDSM, communicator parameters will be
    // saved into the given configuration file.
    H5FDdsmSetStringMacro(DsmConfigFilePath);
    H5FDdsmGetStringMacro(DsmConfigFilePath);

    // Description:
    // Only valid after a AcceptConnection call has been made.
    int GetAcceptedConnection();

    // Description:
    // Get/Set the update ready flag which triggers the pipeline update and
    // possibly the display of DSM objects.
    H5FDdsmSetValueMacro(DsmUpdateReady, int);
    int  GetDsmUpdateReady();
    void ClearDsmUpdateReady();

    // Description:
    // Get/Set the "write to disk" flag, in this case data is written to disk
    // using the HDF MPIO parallel driver
//    H5FDdsmSetValueMacro(DsmWriteDisk, int);
    void SetDsmWriteDisk(int enable);
    H5FDdsmGetValueMacro(DsmWriteDisk, int);

    // Description:
    // Set/Get the current given steering command.
    // The command is then passed to the simulation.
    void SetSteeringCommand(char *cmd);
    H5FDdsmGetStringMacro(SteeringCommand);

    // Description:
    // When sending, the writer can SetXMLDescriptionSend and it will be transmitted
    // to the receiver. When receiving, GetXMLDescriptionReceive queries the internal DSMBuffer
    // object to see if a string is present
    H5FDdsmSetStringMacro(XMLStringSend);
    const char *GetXMLStringReceive();
    void        ClearXMLStringReceive();

    bool   CreateDSM();
    bool   DestroyDSM();
    void   ClearDSM();
    void   ConnectDSM();
    void   DisconnectDSM();
    void   PublishDSM();
    void   UnpublishDSM();
    void   H5Dump();
    void   H5DumpLight();
    void   H5DumpXML();
    void   SendDSMXML();
    void   RequestRemoteChannel();

    // Description:
    // If the .dsm_config file exists in the standard location
    // $ENV{H5FD_DSM_CONFIG_PATH}/.dsm_config then the server/port/mode
    // information can be read. This is for use the by a DSM client.
    // DSM servers write their .dsm_config when PublishDSM() is called
    // Returns false if the .dsm_config file is not read
    bool   ReadDSMConfigFile();

    H5FDdsmBuffer *GetDSMHandle();

  protected:
    //
    // Internal Variables
    //
    int            UpdatePiece;
    int            UpdateNumPieces;
    H5FDdsmInt64   LocalBufferSizeMBytes;

#ifdef HAVE_PTHREADS
    pthread_t      ServiceThread;
#elif HAVE_BOOST_THREADS
    boost::thread *ServiceThread;
#endif

    MPI_Comm        Communicator;
    H5FDdsmBuffer  *DSMBuffer;
    H5FDdsmComm    *DSMComm;
    //
    int             DsmIsServer;
    int             DsmCommType;
    char           *ServerHostName;
    int             ServerPort;
    char           *DsmConfigFilePath;
    //
    int             DsmUpdateReady;
    int             DsmWriteDisk;
    //
    char           *XMLStringSend;
    //
    char           *SteeringCommand;

private:
    H5FDdsmManager(const H5FDdsmManager&);  // Not implemented.
    void operator=(const H5FDdsmManager&);  // Not implemented.
};

#endif
