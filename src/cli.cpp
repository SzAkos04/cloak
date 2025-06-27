#include "cli.hpp"

#include "debug.hpp"

#include <sstream>
#include <string>

bool useColor = true;
bool silent = false;

CLIError::CLIError(const std::string &msg, bool verbose, const char *file,
                   int line, const char *func)
    : std::runtime_error(this->formatMessage(msg, verbose, file, line, func)) {}

std::string CLIError::formatMessage(const std::string &msg, bool verbose,
                                    const char *file, int line,
                                    const char *func) {
    if (!verbose || file == nullptr || func == nullptr || line == 0) {
        return msg;
    }
    std::ostringstream oss;
    oss << file << ":" << line << " (" << func << "): " << msg;
    return oss.str();
}

CLI::CLI(int argc, char *argv[]) : argc(argc), argv(argv) { this->parse(); }

const CLI::Options &CLI::getOptions() const { return this->opts; }

void CLI::help() { std::cout << "cloak help\n"; }

void CLI::version() { std::cout << "cloak version\n"; }

bool CLI::hasExtension(const std::string &filename,
                       const std::string &ext) const {
    if (filename.size() < ext.size())
        return false;
    return filename.compare(filename.size() - ext.size(), ext.size(), ext) == 0;
}

void CLI::parseLongArg(const std::string &arg, int &i) {
    if (arg == "--verbose") {
        this->opts.verbose = true;
    } else if (arg == "--help") {
        this->opts.showHelp = true;
    } else if (arg == "--version") {
        this->opts.showVersion = true;
    } else if (arg == "--color") {
        useColor = true;
    } else if (arg == "--no-color") {
        useColor = false;
    } else if (arg == "--silent") {
        silent = true;
    } else if (arg == "--no-silent") {
        silent = false;
    } else if (arg == "--outfile") {
        if (i + 1 >= this->argc) {
            THROW_CLI(
                "`--outfile` requires a filename. For help, run `cloak --help`",
                this->opts.verbose);
        }
        this->opts.outfile = this->argv[++i];
    } else {
        THROW_CLI("Unknown argument `" + arg +
                      "`. For help, run `cloak --help`",
                  this->opts.verbose);
    }
}

void CLI::parseShortArg(const std::string &arg, int &i) {
    for (size_t j = 1; j < arg.size(); ++j) {
        switch (arg[j]) {
        case 'o':
            if (j == arg.size() - 1) {
                if (i + 1 >= this->argc) {
                    THROW_CLI("`-o` requires a filename. For help, run `cloak "
                              "--help`",
                              this->opts.verbose);
                }
                this->opts.outfile = this->argv[++i];
                return;
            } else {
                this->opts.outfile = arg.substr(j + 1);
                return;
            }
        case 'h':
            this->opts.showHelp = true;
            break;
        case 'v':
            this->opts.showVersion = true;
            break;
        default:
            THROW_CLI(std::string("Unknown argument `-") + arg[j] +
                          "`. For help, run `cloak --help`",
                      this->opts.verbose);
        }
    }
}

void CLI::parse() {
    if (this->argc < 2) {
        THROW_CLI("No input provided. For help, run `cloak --help`",
                  this->opts.verbose);
    }

    for (int i = 1; i < this->argc; ++i) {
        std::string arg = this->argv[i];
        if (!arg.empty() && arg[0] == '-') {
            if (arg.size() < 2) {
                THROW_CLI("Unknown argument `" + arg +
                              "`. For help, run `cloak --help`",
                          this->opts.verbose);
            }
            if (arg[1] == '-') {
                this->parseLongArg(arg, i);
            } else {
                this->parseShortArg(arg, i);
            }
        } else {
            if (!this->opts.filename.empty()) {
                THROW_CLI(
                    "Multiple input files provided; only one file is allowed.",
                    this->opts.verbose);
            }
            if (!this->hasExtension(arg, ".ck")) {
                THROW_CLI("Input file `" + arg +
                              "` must have a `.ck` extension.",
                          this->opts.verbose);
            }
            this->opts.filename = arg;
        }
    }

    if (this->opts.filename.empty() && !this->opts.showHelp &&
        !this->opts.showVersion) {
        THROW_CLI("No input file specified. For help, run `cloak --help`.",
                  this->opts.verbose);
    }
}
