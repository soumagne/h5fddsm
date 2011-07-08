/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmIniFile.h

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
/*=========================================================================

  Ini file Code taken from the following with thanks
  CIniFile Class for C++ - A robust cross platform INI file class
  By Todd Davis (http://www.codeproject.com/file/CIniFile.asp)

  Some fixes, tweaks and reformatting : CSCS

=========================================================================*/
#include "H5FDdsmIniFile.h"

//---------------------------------------------------------------------------
H5FDdsmIniFile::H5FDdsmIniFile(void) {}
//---------------------------------------------------------------------------
H5FDdsmIniFile::~H5FDdsmIniFile(void) {}
//---------------------------------------------------------------------------
// A function to trim whitespace from both sides of a given string
void Trim(std::string& str, const std::string & ChrsToTrim = " \t\n\r", int TrimDir = 0)
{
    size_t startIndex = str.find_first_not_of(ChrsToTrim);
    if (startIndex == std::string::npos) {str.erase(); return;}
    if (TrimDir < 2) str = str.substr(startIndex, str.size()-startIndex);
    if (TrimDir!=1) str = str.substr(0, str.find_last_not_of(ChrsToTrim) + 1);
}
//---------------------------------------------------------------------------
bool H5FDdsmIniFile::Load(string FileName, vector<Record>& content)
{
  string s;                                // Holds the current line from the ini file
  string CurrentSection;                   // Holds the current section name

  ifstream inFile (FileName.c_str());      // Create an input filestream
  if (!inFile.is_open()) return false;     // If the input file doesn't open, then return
  content.clear();                         // Clear the content vector

  string comments = "";                    // A string to store comments in

  while(!std::getline(inFile, s).eof())    // Read until the end of the file
  {
    Trim(s);                               // Trim whitespace from the ends
    if (!s.empty())                         // Make sure its not a blank line
    {
      Record r;                            // Define a new record

      if ((s[0]=='#')||(s[0]==';'))         // Is this a commented line?
      {
        if ((s.find('[')==string::npos)&&  // If there is no [ or =
          (s.find('=')==string::npos))     // Then it's a comment
        {
          comments += s + '\n';            // Add the comment to the current comments string
        } else {
          r.Commented = s[0];              // Save the comment character
          s.erase(s.begin());              // Remove the comment for further processing
          Trim(s);
        }                                  // Remove any more whitespace
      } else r.Commented = ' ';            // else mark it as not being a comment

      if (s.find('[')!=string::npos)        // Is this line a section?
      {    
        s.erase(s.begin());                // Erase the leading bracket
        s.erase(s.find(']'));              // Erase the trailing bracket
        r.Comments = comments;             // Add the comments string (if any)
        comments = "";                     // Clear the comments for re-use
        r.Section = s;                     // Set the Section value
        r.Key = "";                        // Set the Key value
        r.Value = "";                      // Set the Value value
        CurrentSection = s;
      }

      if (s.find('=')!=string::npos)        // Is this line a Key/Value?
      {
        r.Comments = comments;             // Add the comments string (if any)
        comments = "";                     // Clear the comments for re-use
        r.Section = CurrentSection;        // Set the section to the current Section
        r.Key = s.substr(0,s.find('='));   // Set the Key value to everything before the = sign
        r.Value = s.substr(s.find('=')+1); // Set the Value to everything after the = sign
      }
      if (comments == "")                   // Don't add a record yet if its a comment line
        content.push_back(r);              // Add the record to content
    }
  }
  
  inFile.close();                          // Close the file
  return true;
}

bool H5FDdsmIniFile::Save(string FileName, vector<Record>& content)
{
  ofstream outFile (FileName.c_str());     // Create an output filestream
  if (!outFile.is_open()) return false;    // If the output file doesn't open, then return

  for (int i=0;i<(int)content.size();i++)  // Loop through each vector
  {
    outFile << content[i].Comments;        // Write out the comments
    if (content[i].Key == "")               // Is this a section?
      outFile << content[i].Commented << "[" 
      << content[i].Section << "]" << endl;// Then format the section
    else
      outFile << content[i].Commented << content[i].Key  
      << "=" << content[i].Value << endl;  // Else format a key/value
  }

  outFile.close();                         // Close the file
  return true;
}

string H5FDdsmIniFile::Content(string FileName)
{
  string s="";                             // Hold our return string
  vector<Record> content;                  // Holds the current record

  if (Load(FileName, content))             // Make sure the file loads
  {
    for (int i=0;i<(int)content.size();i++)// Loop through the content
    {
      if (content[i].Comments != "") s += content[i].Comments;       // Add the comments
      if (content[i].Commented != ' ') s += content[i].Commented;    // If this is commented, then add it
      if ((content[i].Key == ""))                                    // Is this a section?
        s += '[' + content[i].Section + ']';                        // Add the section
      else s += content[i].Key + '=' + content[i].Value;            // Or the Key value to the return srting

      if (i != (int)content.size()) s += '\n';                           // If this is not the last line, add a CrLf
    }
    return s;                                                       // Return the contents
  }

  return "";
}

vector<string> H5FDdsmIniFile::GetSectionNames(string FileName)
{
  vector<string> data;                          // Holds the return data
  vector<Record> content;                       // Holds the current record 

  if (Load(FileName, content))                  // Make sure the file is loaded
  {
    for (int i=0;i<(int)content.size();i++)     // Loop through the content
    {
      if (content[i].Key =="")                   // If there is no key value, then its a section
        data.push_back(content[i].Section);     // Add the section to the return data
    }
  }

  return data;                                  // Return the data
}

vector<H5FDdsmIniFile::Record> H5FDdsmIniFile::GetSection(string SectionName, string FileName)
{
  vector<Record> data;                          // Holds the return data
  vector<Record> content;                       // Holds the current record

  if (Load(FileName, content))                  // Make sure the file is loaded
  {
    for (int i=0;i<(int)content.size();i++)     // Loop through the content
    {
      if ((content[i].Section == SectionName) && // If this is the section name we want
        (content[i].Key != ""))                 // but not the section name itself
        data.push_back(content[i]);             // Add the record to the return data
    }
  }
  
  return data;                                  // Return the data
}

bool H5FDdsmIniFile::RecordExists(string KeyName, string SectionName, string FileName)
{
  vector<Record> content;                       // Holds the current record

  if (Load(FileName, content))                  // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName));     // Locate the Section/Key

    if (iter == content.end()) return false;                    // The Section/Key was not found
  }
  return true;                                                  // The Section/Key was found
}

bool H5FDdsmIniFile::SectionExists(string SectionName, string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionIs(SectionName));          // Locate the Section

    if (iter == content.end()) return false;              // The Section was not found
  }
  else
  {
    return false;
  }
  return true;                                  // The Section was found
}

vector<H5FDdsmIniFile::Record> H5FDdsmIniFile::GetRecord(string KeyName, string SectionName, string FileName)
{
  vector<Record> data;                          // Holds the return data
  vector<Record> content;                       // Holds the current record

  if (Load(FileName, content))                  // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName));     // Locate the Record

    if (iter == content.end()) return data;                     // The Record was not found

    data.push_back (*iter);                                     // The Record was found
  }
  return data;                                                  // Return the Record
}

string H5FDdsmIniFile::GetValue(string KeyName, string SectionName, string FileName)
{
  vector<Record> content = GetRecord(KeyName,SectionName, FileName);    // Get the Record

  if (!content.empty())                              // Make sure there is a value to return
    return content[0].Value;                        // And return the value

  return "";                                        // No value was found
}

bool H5FDdsmIniFile::SetValue(string KeyName, string Value, string SectionName, string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    if (!SectionExists(SectionName,FileName))              // If the Section doesn't exist
    {
      Record s = {"",' ',SectionName,"",""};              // Define a new section
      Record r = {"",' ',SectionName,KeyName,Value};      // Define a new record
      content.push_back(s);                               // Add the section
      content.push_back(r);                               // Add the record
      return Save(FileName,content);                      // Save
    }

    if (!RecordExists(KeyName,SectionName,FileName))       // If the Key doesn't exist
    {
      vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionIs(SectionName));          // Locate the Section
      iter++;                                             // Advance just past the section
      Record r = {"",' ',SectionName,KeyName,Value};      // Define a new record
      content.insert(iter,r);                             // Add the record
      return Save(FileName,content);                      // Save
    }

    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName)); // Locate the Record

    iter->Value = Value;                                    // Insert the correct value
    return Save(FileName,content);                          // Save
  }

  return false;                                             // In the event the file does not load
}

bool H5FDdsmIniFile::RenameSection(string OldSectionName, string NewSectionName, string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    for(vector<Record>::iterator iter = content.begin(); 
      iter < content.end(); iter++)                 // Loop through the records
    {
      if (iter->Section == OldSectionName)           // Is this the OldSectionName?
        iter->Section = NewSectionName;             // Now its the NewSectionName
    }
    return Save(FileName,content);                  // Save
  }

  return false;                                     // In the event the file does not load
}

bool H5FDdsmIniFile::CommentRecord(CommentChar cc, string KeyName,string SectionName,string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName)); // Locate the Section/Key

    if (iter == content.end()) return false;                // The Section/Key was not found
  
    iter->Commented = cc;                                   // Change the Comment value
    return Save(FileName,content);                          // Save

  }
  return false;                                             // In the event the file does not load
}

bool H5FDdsmIniFile::UnCommentRecord(string KeyName,string SectionName,string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName));      // Locate the Section/Key

    if (iter == content.end()) return false;                    // The Section/Key was not found
  
    iter->Commented = ' ';                                      // Remove the Comment value
    return Save(FileName,content);                              // Save

  }
  return false;                                                 // In the event the file does not load
}

bool H5FDdsmIniFile::CommentSection(char CommentChar, string SectionName, string FileName)
{
  vector<Record> content;                             // Holds the current record

  if (Load(FileName, content))                        // Make sure the file is loaded
  {
    for(vector<Record>::iterator iter = content.begin(); iter < content.end(); iter++)
    {
      if (iter->Section == SectionName)                // Is this the right section?
        iter->Commented = CommentChar;                // Change the comment value
    }
    return Save(FileName,content);                    // Save
  }

  return false;                                       // In the event the file does not load
}

bool H5FDdsmIniFile::UnCommentSection(string SectionName, string FileName)
{
  vector<Record> content;                             // Holds the current record

  if (Load(FileName, content))                        // Make sure the file is loaded
  {
    for(vector<Record>::iterator iter = content.begin(); iter < content.end(); iter++)
    {
      if (iter->Section == SectionName)                // Is this the right section?
        iter->Commented = ' ';                        // Remove the comment value
    }                                  
    return Save(FileName,content);                    // Save
  }

  return false;                                       // In the event the file does not load
}

bool H5FDdsmIniFile::DeleteRecord(string KeyName, string SectionName, string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName)); // Locate the Section/Key

    if (iter == content.end()) return false;                // The Section/Key was not found
  
    content.erase(iter);                                    // Remove the Record
    return Save(FileName,content);                          // Save

  }
  return false;                                             // In the event the file does not load
}

bool H5FDdsmIniFile::DeleteSection(string SectionName, string FileName)
{
  vector<Record> content;                          // Holds the current record

  if (Load(FileName, content))                     // Make sure the file is loaded
  {
    for(int i=(int)content.size()-1;i>-1;i--)      // Iterate backwards through the content
    {              
      if (content[i].Section == SectionName)        // Is this related to the Section?
        content.erase (content.begin()+i);         // Then erase it
    }

    return Save(FileName,content);                 // Save
  }
  return false;                                    // In the event the file does not load
}

bool H5FDdsmIniFile::SetSectionComments(string Comments, string SectionName, string FileName)
{
  vector<Record> content;                          // Holds the current record

  if (Load(FileName, content))                     // Make sure the file is loaded
  {
    for(vector<Record>::iterator iter = content.begin(); iter < content.end(); iter++)                  // Loop through the records
    {
      if ((iter->Section == SectionName) &&         // Is this the Section?
        (iter->Key == ""))                         // And not a record
      {  
        if (Comments.size() >= 2)                  // Is there a comment?
        {
          if (Comments.substr(Comments.size()-2) != "\n")    // Does the string end in a newline?
            Comments += "\n";                     // If not, add one
        }
        iter->Comments = Comments;                // Set the comments
          
        return Save(FileName,content);            // Save
      }
    }
  }
  return false;                                   // In the event the file does not load
}

bool H5FDdsmIniFile::SetRecordComments(string Comments, string KeyName, string SectionName, string FileName)
{
  vector<Record> content;                                         // Holds the current record

  if (Load(FileName, content))                                    // Make sure the file is loaded
  {
    vector<Record>::iterator iter = std::find_if (content.begin(),
        content.end(), 
        H5FDdsmIniFile::RecordSectionKeyIs(SectionName,KeyName));       // Locate the Section/Key

    if (iter == content.end()) return false;                      // The Section/Key was not found
  
    if (Comments.size() >= 2)                                     // Is there a comment?
    {
      if (Comments.substr(Comments.size()-2) != "\n")             // Does the string end in a newline?
        Comments += "\n";                                         // If not, add one
    }
    iter->Comments = Comments;                                    // Set the comments
    return Save(FileName,content);                                // Save

  }
  
  return false;                                                   // In the event the file does not load
}

vector<H5FDdsmIniFile::Record> H5FDdsmIniFile::GetSections(string FileName)
{
  vector<Record> data;                                            // Holds the return data
  vector<Record> content;                                         // Holds the current record

  if (Load(FileName, content))                                    // Make sure the file is loaded
  {
    for (int i=0;i<(int)content.size();i++)                       // Loop through the content
    {
      if (content[i].Key == "")                                    // If this is a section
        data.push_back(content[i]);                               // Add the record to the return data
    }
  }
  
  return data;                              
}

bool H5FDdsmIniFile::Sort(string FileName, bool Descending)
{
  vector<H5FDdsmIniFile::Record> content;                               // Used to hold the sorted content
  vector<H5FDdsmIniFile::Record> sections = GetSections(FileName);      // Get a list of Sections

  if (!sections.empty())                                           // Is there anything to process?
  {

    if (Descending)                                                // Descending or Ascending?
      std::sort(sections.begin(), sections.end(), DescendingSectionSort());
    else                                                          // Sort the Sections
      std::sort(sections.begin(), sections.end(), AscendingSectionSort());

    for(vector<Record>::iterator iter = sections.begin(); iter < sections.end(); iter++) // For each Section
    {                                    
      content.push_back(*iter);                                   // Add the sorted Section to the content

      vector<H5FDdsmIniFile::Record> records = GetSection(iter->Section ,FileName); // Get a list of Records for this section

      if (Descending)                                              // Descending or Ascending?
        std::sort(records.begin(), records.end(), DescendingRecordSort());
      else                                                        // Sort the Records
        std::sort(records.begin(), records.end(), AscendingRecordSort());

      for(vector<Record>::iterator it = records.begin(); it < records.end(); it++) // For each Record
        content.push_back(*it);                                   // Add the sorted Record to the content
    }
    
    return Save(FileName,content);                                // Save
    }

  return false;                                                   // There were no sections
}

bool H5FDdsmIniFile::AddSection(string SectionName, string FileName)
{
  vector<Record> content;                           // Holds the current record

  if (Load(FileName, content))                      // Make sure the file is loaded
  {
    Record s = {"",' ',SectionName,"",""};          // Define a new section
    content.push_back(s);                           // Add the section
    return Save(FileName,content);                  // Save
  }

  return false;                                     // The file did not open
}

bool H5FDdsmIniFile::Create(string FileName)
{
  vector<Record> content;                          // Create empty content
  return Save(FileName,content);                   // Save
}

