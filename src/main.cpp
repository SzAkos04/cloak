#include "cli.hpp"
#include "debug.hpp"
#include "filereader.hpp"
#include "lexer.hpp"
#include "token.hpp"

#include <string>
#include <vector>

int main(int argc, char **argv) {
    try {
        // may throw CLIError
        CLI cli(argc, argv);
        const CLI::Options &opts = cli.getOptions();

        if (opts.showHelp) {
            CLI::help();
            return 0;
        }
        if (opts.showVersion) {
            CLI::version();
            return 0;
        }

        // may throw FileReadError
        FileReader fileReader(opts.filename, opts.verbose);

        // may throw LexerError
        Lexer lexer(fileReader.readFile(), opts.verbose);
        std::vector<Token> tokens = lexer.lex();
#ifdef DEBUG
        for (const auto &token : tokens) {
            info(token);
        }
#endif

        return 0;
    } catch (const CLIError &e) {
        error(std::string("[CLI] ") + e.what());
        return -1;
    } catch (const FileReadError &e) {
        error(std::string("[FILEREADER] ") + e.what());
        return -1;
    } catch (const LexerError &e) {
        error(std::string("[LEXER] ") + e.what());
        return -1;
    } catch (const std::exception &e) {
        error(std::string("[UNKNOWN] ") + e.what());
        return -1;
    }
}
