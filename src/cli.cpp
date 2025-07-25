#include "cli.hpp"
#include "version.hpp"

#include <fmt/core.h>
#include <iostream>
#include <string>

bool silent = false;
bool useColor = true;

void CLI::parse() {
    if (this->argc < 2) {
        THROW_CLI("No input provided. For help, run `cloak --help`",
                  this->opts.verbose);
    }
    for (this->cur = 1; this->cur < this->argc; ++this->cur) {
        std::string arg(this->argv[this->cur]);
        if (!arg.empty() && arg.at(0) == '-') {
            if (arg.length() < 2) {
                THROW_CLI(
                    fmt::format(
                        "Unknown argument `{}`. For help, run `cloak --help`",
                        arg),
                    this->opts.verbose);
            }

            if (arg.at(1) == '-') {
                this->parseLongArg();
            } else {
                this->parseShortArg();
            }
        } else {
            if (!this->opts.filename.empty()) {
                THROW_CLI("Multiple input files provided; only one is allowed",
                          this->opts.verbose);
            }
            std::string ext(".ck");
            if (!this->hasExtension(arg, ext)) {
                THROW_CLI(fmt::format("Input file `{}` does not have required "
                                      "`{}` extension",
                                      arg, ext),
                          this->opts.verbose);
            }
            this->opts.filename = arg;
        }
    }

    if (this->opts.filename.empty() && !this->opts.showHelp &&
        !this->opts.showVersion) {
        THROW_CLI("No input file specified. For help, run `cloak --help`",
                  this->opts.verbose);
    }
}

void CLI::help() {
    std::cout << R"(cloak - a minimalist compiler

Usage: cloak [options] <file.ck>

Options:
  --outfile | -o <file>      Set output filename
  --opt=[level] | -O[level]  Set optimization level (0, 1, 2, 3, s, z)
  --color                    Enable color diagnostics
  --no-color                 Disable color diagnostics
  --silent                   Suppress all warnings
  --no-silent                Enable warnings
  --verbose                  Show verbose debugging
  --help | -h                Show this help message
  --version | -v             Show version info
  --dump-tokens              Print tokens vector to stdout
  --dump-ast                 Print AST to stdout
  --dump-ir                  Print LLVM IR to stdout)"
              << std::endl;
}

void CLI::version() { std::cout << versionMsg << std::endl; }

void CLI::parseShortArg() {
    std::string arg(this->argv[this->cur]);
    for (size_t j = 1; j < arg.size(); ++j) {
        switch (arg.at(j)) {
        case 'o':
            if (j == arg.size() - 1) {
                if (this->cur + 1 >= this->argc) {
                    THROW_CLI("`-o` requires a filename. For help, run `cloak "
                              "--help`",
                              this->opts.verbose);
                }
                this->opts.outfile = this->argv[++this->cur];
            } else {
                this->opts.outfile = arg.substr(j + 1);
            }
            break;
        case 'O': {
            if (j == arg.size() - 1) {
                // `-O` with no level specified; default to O1
                this->opts.opt = Optimization::O1;
            } else {
                std::string optStr = arg.substr(j + 1);
                if (optStr == "0") {
                    this->opts.opt = Optimization::O0;
                } else if (optStr == "1") {
                    this->opts.opt = Optimization::O1;
                } else if (optStr == "2") {
                    this->opts.opt = Optimization::O2;
                } else if (optStr == "3") {
                    this->opts.opt = Optimization::O3;
                } else if (optStr == "s") {
                    this->opts.opt = Optimization::Os;
                } else if (optStr == "z") {
                    this->opts.opt = Optimization::Oz;
                } else {
                    THROW_CLI(fmt::format("Invalid optimization level `-O{}`. "
                                          "For help, run `cloak --help`",
                                          optStr),
                              this->opts.verbose);
                }
            }
            return; // `-O[level]` terminates argument
        }
        case 'h':
            this->opts.showHelp = true;
            break;
        case 'v':
            this->opts.showVersion = true;
            break;
        default:
            THROW_CLI(
                fmt::format(
                    "Unknown argument `-{}`. For help, run `cloak --help`",
                    arg.at(j)),
                this->opts.verbose);
        }
    }
}

void CLI::parseLongArg() {
    std::string arg(this->argv[this->cur]);
    if (arg == "--outfile") {
        if (this->cur + 1 >= this->argc) {
            THROW_CLI(
                "`--outfile` requires a filename. For help, run `cloak --help`",
                this->opts.verbose);
        }
        this->opts.outfile = this->argv[++this->cur];
    } else if (arg.rfind("--opt=", 0) == 0) {
        std::string level = arg.substr(6);
        if (level == "0")
            this->opts.opt = Optimization::O0;
        else if (level == "1")
            this->opts.opt = Optimization::O1;
        else if (level == "2")
            this->opts.opt = Optimization::O2;
        else if (level == "3")
            this->opts.opt = Optimization::O3;
        else if (level == "s")
            this->opts.opt = Optimization::Os;
        else if (level == "z")
            this->opts.opt = Optimization::Oz;
        else {
            THROW_CLI(fmt::format("Invalid optimization level `--opt={}`. For "
                                  "help, run `cloak --help`",
                                  level),
                      this->opts.verbose);
        }
    } else if (arg == "--color") {
        useColor = true;
    } else if (arg == "--no-color") {
        useColor = false;
    } else if (arg == "--silent") {
        silent = true;
    } else if (arg == "--no-silent") {
        silent = false;
    } else if (arg == "--verbose") {
        this->opts.verbose = true;
    } else if (arg == "--help") {
        this->opts.showHelp = true;
    } else if (arg == "--version") {
        this->opts.showVersion = true;
    } else if (arg == "--dump-tokens") {
        this->opts.dumpTokens = true;
    } else if (arg == "--dump-ast") {
        this->opts.dumpAst = true;
    } else if (arg == "--dump-ir") {
        this->opts.dumpIR = true;
    } else {
        THROW_CLI(
            fmt::format("Unknown argument `{}`. For help, run `cloak --help`",
                        arg),
            this->opts.verbose);
    }
}

bool CLI::hasExtension(const std::string &filename, const std::string &ext) {
    if (filename.size() < ext.size()) {
        return false;
    }
    return filename.compare(filename.size() - ext.size(), ext.size(), ext) == 0;
}
