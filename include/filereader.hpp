#pragma once

#include "error.hpp"

#include <fmt/core.h>
#include <fstream>
#include <sstream>
#include <string>

class FileReaderError : public Error {
    using Error::Error;
};

#define THROW_FILEREADER(msg, verbose)                                         \
    throw FileReaderError((msg), (verbose), __FILE__, __LINE__, __func__)

class FileReader {
  public:
    explicit FileReader(bool verbose_) : verbose(verbose_) {}

    std::string readFile(const std::string &filename) {
        this->filename = filename;
        this->openFile();
        this->readContents();
        return this->contents;
    }

  private:
    bool verbose;
    std::string filename;
    std::ifstream file;
    std::string contents;

    void openFile() {
        this->file.open(this->filename, std::ios::in);
        if (!this->file.is_open()) {
            THROW_FILEREADER(
                fmt::format("Failed to open file `{}`", this->filename),
                this->verbose);
        }
    }

    void readContents() {
        std::ostringstream buffer;
        buffer << this->file.rdbuf();

        if (this->file.fail()) {
            THROW_FILEREADER(
                fmt::format("Failed to read file `{}`", this->filename),
                this->verbose);
        }

        this->contents = buffer.str();
        this->file.close();
    }
};
