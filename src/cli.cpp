#include "cli.hpp"

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

void CLI::help() { std::cout << "cloak help" << std::endl; }

void CLI::version() { std::cout << "cloak version" << std::endl; }

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
        if (this->cur + 1 >= this->argc) {
            THROW_CLI(
                "`--outfile` requires a filename. For help, run `cloak --help`",
                this->opts.verbose);
        }
        this->opts.outfile = this->argv[++this->cur];
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
