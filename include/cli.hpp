#pragma once

#include <stdexcept>
#include <string>

class CLIError : public std::runtime_error {
  public:
    explicit CLIError(const std::string &msg, bool verbose,
                      const char *file = nullptr, int line = 0,
                      const char *func = nullptr);

  private:
    static std::string formatMessage(const std::string &msg, bool verbose,
                                     const char *file, int line,
                                     const char *func);
};

#define THROW_CLI(msg, verbose)                                                \
    throw CLIError((msg), (verbose), __FILE__, __LINE__, __func__)

class CLI {
  public:
    explicit CLI(int argc, char **argv);

    struct Options {
        std::string filename;
        std::string outfile = "./a.o";
        bool verbose = false;
        bool showHelp = false;
        bool showVersion = false;
    };

    const Options &getOptions() const;

    static void help();
    static void version();

  private:
    Options opts;
    int argc;
    char **argv;

    void parse();
    void parseLongArg(const std::string &arg, int &i);
    void parseShortArg(const std::string &arg, int &i);

    bool hasExtension(const std::string &filename,
                      const std::string &ext) const;
};
