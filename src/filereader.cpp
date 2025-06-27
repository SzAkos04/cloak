#include "filereader.hpp"
#include <sstream>
#include <string>

FileReadError::FileReadError(const std::string &filename,
                             const std::string &msg, bool verbose,
                             const char *file, int line, const char *func)
    : std::runtime_error(
          formatMessage(filename, msg, verbose, file, line, func)) {}

std::string FileReadError::formatMessage(const std::string &filename,
                                         const std::string &msg, bool verbose,
                                         const char *file, int line,
                                         const char *func) {
    std::ostringstream oss;
    if (!verbose || file == nullptr || func == nullptr || line == 0) {
        oss << "File \"" << filename << "\": " << msg;
    } else {
        oss << file << ":" << line << " (" << func << "): " << "File \""
            << filename << "\": " << msg;
    }
    return oss.str();
}

FileReader::FileReader(const std::string &filename, bool verbose)
    : filename(filename), verbose(verbose) {}

std::string FileReader::readFile() {
    this->openFile();
    this->readContent();
    return this->content;
}

void FileReader::openFile() {
    this->file.open(this->filename, std::ios::binary | std::ios::ate);
    if (!this->file.is_open()) {
        THROW_FILEREADER(this->filename, "Failed to open file", this->verbose);
    }
}

void FileReader::readContent() {
    auto size = this->file.tellg();
    if (size < 0) {
        THROW_FILEREADER(this->filename, "Failed to get file size",
                         this->verbose);
    }
    this->file.seekg(0, std::ios::beg);

    this->content.resize(static_cast<size_t>(size));
    if (!this->file.read(this->content.data(), size)) {
        THROW_FILEREADER(this->filename, "Error reading file content",
                         this->verbose);
    }
}
