#pragma once

#include <fstream>
#include <stdexcept>
#include <string>
class FileReadError : public std::runtime_error {
  public:
    explicit FileReadError(const std::string &filename, const std::string &msg,
                           bool verbose, const char *file = nullptr,
                           int line = 0, const char *func = nullptr);

  private:
    static std::string formatMessage(const std::string &filename,
                                     const std::string &msg, bool verbose,
                                     const char *file, int line,
                                     const char *func);
};

#define THROW_FILEREADER(filename, msg, verbose)                               \
    throw FileReadError((filename), (msg), (verbose), __FILE__, __LINE__,      \
                        __func__)

class FileReader {
  public:
    explicit FileReader(const std::string &filename, bool verbose);

    std::string readFile();

  private:
    std::string filename;
    std::ifstream file;
    std::string content;
    bool verbose;

    void openFile();

    void readContent();
};
