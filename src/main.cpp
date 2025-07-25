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

#include <iostream>

int main(int argc, char **argv) {
    try {
        // parse command-line arguments
        CLI cli(argc, argv);
        cli.parse(); // may throw CLIError
        CLI::Options opts = cli.getOpts();

        // handle help/version requests
        if (opts.showHelp) {
            CLI::help();
            return 0;
        }
        if (opts.showVersion) {
            CLI::version();
            return 0;
        }

        // read the source file
        FileReader reader(opts.verbose);
        std::string contents =
            reader.readFile(opts.filename); // may throw FileReaderError

        // tokenize source code
        Lexer lexer(contents, opts.verbose);
        std::vector<Token> tokens = lexer.lex(); // may throw LexerError
        if (opts.dumpTokens) {
            for (const auto &token : tokens) {
                std::cout << token.toString() << std::endl;
            }
            std::cout << std::endl;
        }

        // parse tokens into AST
        Parser parser(tokens, opts.verbose);
        std::unique_ptr<AstProgram> ast =
            parser.parseProgram(); // may throw ParserError
        if (opts.dumpAst) {
            PrinterVisitor printer;
            ast->accept(printer);
            std::cout << std::endl;
        }

        // generate intermediate representation
        CodegenVisitor codegen(opts.outfile, opts.opt, opts.verbose);
        ast->accept(codegen); // may throw CodegenError
        if (opts.dumpIR) {
            codegen.dumpIR();
            std::cout << std::endl;
        }
    } catch (const CLIError &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[CLI] {}", e.what()));
        return -1;
    } catch (const FileReaderError &e) {
        LOG_FRIENDLY_ERROR(fmt::format("[FILEREADER] {}", e.what()));
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
