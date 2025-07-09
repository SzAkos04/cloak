#include "cli.hpp"
#include "codegen_visitor.hpp"
#include "filereader.hpp"
#include "lexer.hpp"
#include "logger.hpp"
#include "parser.hpp"
#include "printer_visitor.hpp"
#include "token.hpp"

#include <fmt/core.h>
#include <string>
#include <vector>
#ifdef DEBUG
#include <iostream>
#endif

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
#ifdef DEBUG
        std::cout << contents << std::endl;
#endif

        Lexer lexer(contents, opts.verbose);
        std::vector<Token> tokens = lexer.lex(); // may throw LexerError
#ifdef DEBUG
        for (const auto &token : tokens) {
            std::cout << token.toString() << std::endl;
        }
#endif

        Parser parser(tokens, opts.verbose);
        std::unique_ptr<AstProgram> ast =
            parser.parseProgram(); // may throw ParserError

        PrinterVisitor printer;
        ast->accept(printer);

        CodegenVisitor codegen(opts.verbose);
        ast->accept(codegen); // may throw CodegenError
        codegen.dumpIR();
    } catch (const CLIError &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[CLI] {}", e.what()));
        return -1;
    } catch (const LexerError &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[LEXER] {}", e.what()));
        return -1;
    } catch (const ParserError &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[PARSER] {}", e.what()));
        return -1;
    } catch (const CodegenError &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[CODEGEN] {}", e.what()));
        return -1;
    } catch (const std::runtime_error &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[UNKNOWN] {}", e.what()));
        return -1;
    }
    return 0;
}
