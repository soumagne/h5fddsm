/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmIniFile.h

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
/*=========================================================================

  Ini file Code taken from the following with thanks
  CIniFile Class for C++ - A robust cross platform INI file class
  By Todd Davis (http://www.codeproject.com/file/CIniFile.asp)

  Some fixes, tweaks and reformatting : CSCS

=========================================================================*/
#ifndef __H5FDdsmIniFile_h
#define __H5FDdsmIniFile_h

#include "H5FDdsmObject.h"

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <functional>

using namespace std;

class H5FDdsm_EXPORT H5FDdsmIniFile : public H5FDdsmObject {
public:
  struct Record
  {
    string Comments;
    char Commented;
    string Section;
    string Key;
    string Value;
  };

  enum CommentChar
  {
    Pound = '#',
    SemiColon = ';'
  };

  H5FDdsmIniFile(void);
  virtual ~H5FDdsmIniFile(void);

  static bool AddSection(string SectionName, string FileName);
  static bool CommentRecord(CommentChar cc, string KeyName,string SectionName,string FileName);
  static bool CommentSection(char CommentChar, string SectionName, string FileName);
  static string Content(string FileName);
  static bool Create(string FileName);
  static bool DeleteRecord(string KeyName, string SectionName, string FileName);
  static bool DeleteSection(string SectionName, string FileName);
  static vector<Record> GetRecord(string KeyName, string SectionName, string FileName);
  static vector<Record> GetSection(string SectionName, string FileName);
  static vector<string> GetSectionNames(string FileName);
  static string GetValue(string KeyName, string SectionName, string FileName);
  static bool RecordExists(string KeyName, string SectionName, string FileName);
  static bool RenameSection(string OldSectionName, string NewSectionName, string FileName);
  static bool SectionExists(string SectionName, string FileName);
  static bool SetRecordComments(string Comments, string KeyName, string SectionName, string FileName);
  static bool SetSectionComments(string Comments, string SectionName, string FileName);
  static bool SetValue(string KeyName, string Value, string SectionName, string FileName);
  static bool Sort(string FileName, bool Descending);
  static bool UnCommentRecord(string KeyName,string SectionName,string FileName);
  static bool UnCommentSection(string SectionName, string FileName);

private:
  static vector<Record> GetSections(string FileName);
  static bool Load(string FileName, vector<Record>& content);  
  static bool Save(string FileName, vector<Record>& content);

  struct RecordSectionIs : std::unary_function<Record, bool>
  {
    std::string section_;
    RecordSectionIs(const std::string& section): section_(section){}
    bool operator()( const Record& rec ) const
    {
      return rec.Section == section_;
    }
  };

  struct RecordSectionKeyIs : std::unary_function<Record, bool>
  {
    std::string section_;
    std::string key_;
    RecordSectionKeyIs(const std::string& section, const std::string& key): section_(section),key_(key){}
    bool operator()( const Record& rec ) const
    {
      return ((rec.Section == section_)&&(rec.Key == key_));
    }
  };

  struct AscendingSectionSort
  {
    bool operator()(const Record& Start, const Record& End)
    {
      return Start.Section < End.Section;
    }
  };

  struct DescendingSectionSort
  {
    bool operator()(const Record& Start, const Record& End)
    {
      return Start.Section > End.Section;
    }
  };

  struct AscendingRecordSort
  {
    bool operator()(const Record& Start, const Record& End)
    {
      return Start.Key < End.Key;
    }
  };

  struct DescendingRecordSort
  {
    bool operator()(const Record& Start, const Record& End)
    {
      return Start.Key > End.Key;
    }
  };
};

#endif // __H5FDdsmIniFile_h
