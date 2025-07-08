#pragma once

#include "error.hpp"

#include <string>

class CLIError : public Error {
    using Error::Error;
};

#define THROW_CLI(msg, verbose)                                                \
    throw CLIError((msg), (verbose), __FILE__, __LINE__, __func__)

class CLI {
  public:
    struct Options {
        std::string filename;
        std::string outfile = "./a.o";
        bool showHelp = false;
        bool showVersion = false;
        bool verbose = false;
    };

    explicit CLI(int argc_, char **argv_) : argc(argc_), argv(argv_), cur(0) {}

    void parse();

    const Options &getOpts() const { return this->opts; }

    static void help();
    static void version();

  private:
    Options opts;

    int argc;
    char **argv;

    int cur;

    void parseShortArg();
    void parseLongArg();

    bool hasExtension(const std::string &filename, const std::string &ext);
};
