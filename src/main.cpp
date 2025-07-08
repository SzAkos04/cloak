#include "cli.hpp"
#include "filereader.hpp"
#include "lexer.hpp"
#include "logger.hpp"
#include "parser.hpp"
#include "token.hpp"

#include <fmt/core.h>
#include <string>
#include <vector>

int main(int argc, char **argv) {
    try {
        CLI cli(argc, argv);
        cli.parse(); // may throw CLIError
        CLI::Options opts = cli.getOpts();

        if (opts.showHelp) {
            CLI::help();
            return 0;
        }
        if (opts.showVersion) {
            CLI::version();
            return 0;
        }

        FileReader reader(opts.verbose);
        std::string contents =
            reader.readFile(opts.filename); // may throw FileReaderError
        LOG_DEBUG(contents);

        Lexer lexer(contents, opts.verbose);
        std::vector<Token> tokens = lexer.lex(); // may throw LexerError
        for ([[maybe_unused]] const auto &token : tokens) {
            LOG_DEBUG(token.toString());
        }

        Parser parser(tokens, opts.verbose);
        std::unique_ptr<AstProgram> ast =
            parser.parseProgram(); // may throw ParserError
    } catch (const CLIError &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[CLI] {}", e.what()));
        return -1;
    } catch (const LexerError &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[LEXER] {}", e.what()));
        return -1;
    } catch (const ParserError &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[PARSER] {}", e.what()));
        return -1;
    } catch (const std::runtime_error &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[UNKNOWN] {}", e.what()));
        return -1;
    }
    return 0;
}
