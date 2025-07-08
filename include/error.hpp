#pragma once

#include <sstream>
#include <stdexcept>
#include <string>

class Error : public std::runtime_error {
  public:
    Error(const std::string &msg, bool verbose_ = false,
          const std::string &file_ = "", int line_ = 0,
          const std::string &function_ = "")
        : std::runtime_error(msg), verbose(verbose_), file(file_), line(line_),
          function(function_), full_message_cached(false) {}

    const char *what() const noexcept override {
        if (!verbose) {
            return std::runtime_error::what();
        }

        // cache the full message to keep pointer valid
        if (!full_message_cached) {
            std::ostringstream oss;
            oss << "[" << file << ":" << line << ":" << function << "()] "
                << std::runtime_error::what();
            full_message = oss.str();
            full_message_cached = true;
        }
        return full_message.c_str();
    }

  protected:
    bool verbose;
    std::string file;
    int line;
    std::string function;

  private:
    mutable bool full_message_cached;
    mutable std::string full_message;
};
