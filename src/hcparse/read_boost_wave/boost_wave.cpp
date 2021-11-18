#include "boost_wave.hpp"
#include <memory>

static_assert(
    (int)wekLastErrorNumber
        == (int)preprocess_exception::error_code::last_error_number,
    "Mismatched C API enum size");


static_assert(
    ((int)wekLexerUnexpectedError - (int)wekLexerErrorBegin - 1)
        == (int)cpplexer::lexing_exception::error_code::unexpected_error,
    "Mismatched C API enum size");


#define CXX_FAIL(e)                                                       \
    std::cerr << "boost wave side raised C++ exception on the "           \
                 "toplevel of the wrapper function. Right now exception " \
                 "propagation is not implemented, so total program "      \
                 "abort is called. The error 'what' was - "               \
              << e.what() << ". Exception was caught by " << __FUNCTION__ \
              << " on line " << __LINE__ << ".";                          \
    abort();

#define BOOST_FAIL(e)                                                     \
    std::cerr << "boost wave side raised boost::wave exception on the "   \
                 "toplevel of the wrapper function. Right now exception " \
                 "propagation is not implemented, so total program "      \
                 "abort is called. The error description was - "          \
              << e.description() << ". 'what' was - " << e.what()         \
              << ". Exception was caught by " << __FUNCTION__             \
              << " on line " << __LINE__ << ".";                          \
    abort();

#define ANY_FAIL()                                                        \
    std::cerr                                                             \
        << "boost wave side raised unspecified object. Right now "        \
           "exception propagation is not implemented, so total program "  \
           "abort is called. Exception was caught by "                    \
        << __FUNCTION__ << " on line " << __LINE__ << ".";                \
    abort();

#define DO_CATCH                                                          \
    catch (boost::wave::cpp_exception & e) {                              \
        BOOST_FAIL(e);                                                    \
    }                                                                     \
    catch (boost::wave::cpplexer::lexing_exception & e) {                 \
        BOOST_FAIL(e);                                                    \
    }                                                                     \
    catch (std::exception & e) {                                          \
        CXX_FAIL(e);                                                      \
    }                                                                     \
    catch (...) {                                                         \
        ANY_FAIL();                                                       \
    }

#define GOT_ERR                                                           \
    std::cerr << "Exception was raised in " << __FUNCTION__               \
              << " on line " << __LINE__ << "\n";

const char* to_string(EntryHandling handling) {
    switch (handling) {
        case EntryHandlingSkip: return "skip";
        case EntryHandlingProcess: return "skip";
        case EntryHandlingRaise: return "skip";
    }
}


bool WaveHooksImpl::found_directive(
    const WaveContextImpl& ctx,
    const WaveToken&       token) {
    try {
        if (found_directive_impl.isActive()) {
            auto handling = found_directive_impl(
                (const WaveContextImplHandle*)&ctx, &token);

            switch (handling) {
                case EntryHandlingProcess: return false;
                case EntryHandlingSkip: return true;
                default:
                    throw std::logic_error(
                        std::string(
                            "'found_directive_impl' returned "
                            "unexpected "
                            "entry handling value - wanted 'process' or "
                            "'skip', but "
                            "got ")
                        + to_string(handling));
            }
        } else {
            return default_preprocessing_hooks::found_directive(
                ctx, token);
        }
    }
    DO_CATCH;
}

bool WaveHooksImpl::found_unknown_directive(
    const WaveContextImpl& ctx,
    const WaveTokenList&   line,
    WaveTokenList&         pending) {
    try {
        if (found_unknown_directive_impl.isActive()) {
            auto handling = found_unknown_directive_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenListHandle*)&line,
                (WaveTokenListHandle*)&pending);

            switch (handling) {
                case EntryHandlingProcess: return false;
                case EntryHandlingSkip: return true;
                default:
                    throw std::logic_error(
                        std::string(
                            "'found_directive_impl' returned "
                            "unexpected "
                            "entry handling value - wanted 'process' or "
                            "'skip', but "
                            "got ")
                        + to_string(handling));
            }
        } else {
            return default_preprocessing_hooks::found_unknown_directive(
                ctx, line, pending);
        }
    }
    DO_CATCH;
}

bool WaveHooksImpl::may_skip_whitespace(
    WaveContextImpl const& ctx,
    WaveToken&             token,
    bool&                  skipped_newline) {
    try {
        if (may_skip_whitespace_impl.isActive()) {
            return may_skip_whitespace_impl(
                (const WaveContextImplHandle*)&ctx,
                &token,
                &skipped_newline);
        } else {
            return default_preprocessing_hooks::may_skip_whitespace(
                ctx, token, skipped_newline);
        }
    }
    DO_CATCH;
}


bool WaveHooksImpl::evaluated_conditional_expression(
    WaveContextImpl const& ctx,
    WaveToken const&       directive,
    WaveTokenList const&   expression,
    bool                   expression_value) {
    try {
        if (evaluated_conditional_expression_impl.isActive()) {
            return evaluated_conditional_expression_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenHandle*)&directive,
                (const WaveTokenListHandle*)&expression,
                expression_value);
        } else {
            return default_preprocessing_hooks::
                evaluated_conditional_expression(
                    ctx, directive, expression, expression_value);
        }
    }
    DO_CATCH;
}

void wave_setEvaluatedConditionalExpression(
    WaveContextHandle*                     context,
    EvaluatedConditionalExpressionImplType impl,
    void*                                  env) {
    try {
        toCxx(context)
            ->context->get_hooks()
            .evaluated_conditional_expression_impl.impl
            = (bool (*)(
                const WaveContextImplHandle*,
                const WaveTokenHandle*,
                const WaveTokenListHandle*,
                bool,
                void*))(impl);
        toCxx(context)
            ->context->get_hooks()
            .evaluated_conditional_expression_impl.env
            = env;
    }
    DO_CATCH;
}


void WaveHooksImpl::throw_exception(
    const WaveContextImpl& ctx,
    const std::exception&  e) {
    try {

        bool isError = false;

        if (const cpp_exception* exception = dynamic_cast<
                const cpp_exception*>(&e)) {
            const char* filename    = exception->file_name();
            const char* description = exception->description();

            auto sev = (WaveSeverityLevel)exception->get_severity();

            auto diag = WaveDiagnostics{
                (int)exception->line_no(),
                (int)exception->column_no(),
                (WaveErrorCode)exception->get_errorcode(),
                sev,
                copyalloc(filename),
                copyalloc(description),
            };

            this->context->diagnostics.push(diag);

            isError = (sev == wslError) || (sev == wslFatal);

        } else if (
            const cpplexer::lexing_exception* exception = dynamic_cast<
                const cpplexer::lexing_exception*>(&e)) {

            const char* filename    = exception->file_name();
            const char* description = exception->description();

            auto sev = (WaveSeverityLevel)exception->get_severity();

            auto diag = WaveDiagnostics{
                (int)exception->line_no(),
                (int)exception->column_no(),
                (WaveErrorCode)(exception->get_errorcode() + (int)wekLexerErrorBegin + 1),
                sev,
                copyalloc(filename),
                copyalloc(description),
            };

            this->context->diagnostics.push(diag);

            isError = (sev == wslError) || (sev == wslFatal);
        } else {
            std::cerr
                << "boost wave side raised unspecified object. Total "
                   "program abort is called.";
            abort();
        }

        if (isError) {
            this->context->errorCount++;
        }
    }
    DO_CATCH;
}


void WaveHooksImpl::skipped_token(
    WaveContextImpl const& ctx,
    WaveToken const&       token) {
    try {
        if (skipped_token_impl.isActive()) {
            skipped_token_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenHandle*)&token);
        } else {
            default_preprocessing_hooks::skipped_token(ctx, token);
        }
    }
    DO_CATCH;
}

void wave_setSkippedToken(
    WaveContextHandle*   context,
    SkippedTokenImplType impl,
    void*                env) {
    try {
        toCxx(context)->context->get_hooks().skipped_token_impl.impl = (void (*)(
            const WaveContextImplHandle*, const WaveTokenHandle*, void*))(
            impl);
        toCxx(context)->context->get_hooks().skipped_token_impl.env = env;
    }
    DO_CATCH;
}


const WaveToken& WaveHooksImpl::generated_token(
    WaveContextImpl const& ctx,
    WaveToken const&       token) {
    try {
        if (generated_token_impl.isActive()) {
            WaveToken& res = const_cast<WaveToken&>(token);
            generated_token_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenHandle*)&token,
                (WaveTokenHandle*)&res);

            return res;
        } else {
            return default_preprocessing_hooks::generated_token(
                ctx, token);
        }
    }
    DO_CATCH;
}


bool WaveHooksImpl::expanding_object_like_macro(
    const WaveContextImpl& ctx,
    const WaveToken&       macro,
    const WaveTokenList&   definition,
    const WaveToken&       macrocall) {
    try {
        if (expanding_object_like_macro_impl.isActive()) {

            auto handling = expanding_object_like_macro_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenHandle*)&macro,
                (const WaveTokenListHandle*)&definition,
                (const WaveTokenHandle*)&macrocall);

            switch (handling) {
                case EntryHandlingProcess: return false;
                case EntryHandlingSkip: return true;
                default:
                    throw std::logic_error(
                        std::string(
                            "'expanding_object_like_macro' returned "
                            "unexpected "
                            "entry handling value - wanted 'process' or "
                            "'skip', but "
                            "got ")
                        + to_string(handling));
            }
        } else {
            return default_preprocessing_hooks::
                expanding_object_like_macro(
                    ctx, macro, definition, macrocall);
        }
    }
    DO_CATCH;
}

void wave_setExpandingObjectLikeMacro(
    WaveContextHandle*               context,
    ExpandingObjectLikeMacroImplType impl,
    void*                            env) {
    try {
        toCxx(context)
            ->context->get_hooks()
            .expanding_object_like_macro_impl.impl
            = (EntryHandling(*)(
                const WaveContextImplHandle*,
                const WaveTokenHandle*,
                const WaveTokenListHandle*,
                const WaveTokenHandle*,
                void*))(impl);
        toCxx(context)
            ->context->get_hooks()
            .expanding_object_like_macro_impl.env
            = env;
    }
    DO_CATCH;
}

void WaveHooksImpl::expanded_macro(
    WaveContextImpl const& ctx,
    WaveTokenList const&   result) {
    try {
        if (expanded_macro_impl.isActive()) {
            expanded_macro_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenListHandle*)&result);
        } else {
            default_preprocessing_hooks::expanded_macro(ctx, result);
        }
    }
    DO_CATCH;
}

void wave_setExpandedMacro(
    WaveContextHandle*    context,
    ExpandedMacroImplType impl,
    void*                 env) {
    try {
        toCxx(context)->context->get_hooks().expanded_macro_impl.impl = (void (*)(
            const WaveContextImplHandle*,
            const WaveTokenListHandle*,
            void*))(impl);
        toCxx(context)->context->get_hooks().expanded_macro_impl.env = env;
    }
    DO_CATCH;
}

void WaveHooksImpl::rescanned_macro(
    const WaveContextImpl& ctx,
    const WaveTokenList&   result) {
    try {
        if (rescanned_macro_impl.isActive()) {
            rescanned_macro_impl(
                (const WaveContextImplHandle*)&ctx,
                (WaveTokenListHandle*)&result);
        } else {
            default_preprocessing_hooks::rescanned_macro(ctx, result);
        }
    }
    DO_CATCH;
}

void wave_setRescannedMacro(
    WaveContextHandle*     context,
    RescannedMacroImplType impl,
    void*                  env) {
    try {
        toCxx(context)->context->get_hooks().rescanned_macro_impl.impl = (void (*)(
            const WaveContextImplHandle*,
            const WaveTokenListHandle*,
            void*))(impl);
        toCxx(context)->context->get_hooks().rescanned_macro_impl.env = env;
    }
    DO_CATCH;
}


bool WaveHooksImpl::found_include_directive(
    const WaveContextImpl& ctx,
    const std::string&     filename,
    bool                   include_next) {
    try {
        if (found_include_directive_impl.isActive()) {
            auto handling = found_include_directive_impl(
                (const WaveContextImplHandle*)&ctx,
                filename.c_str(),
                include_next);

            switch (handling) {
                case EntryHandlingProcess: return false;
                case EntryHandlingSkip: return true;
                default:
                    throw std::logic_error(
                        std::string(
                            "'found_include_directive' returned "
                            "unexpected "
                            "entry handling value - wanted 'process' or "
                            "'skip', but "
                            "got ")
                        + to_string(handling));
            }

        } else {
            return default_preprocessing_hooks::found_include_directive(
                ctx, filename, include_next);
        }
    }
    DO_CATCH;
}


void wave_setFoundIncludeDirective(
    WaveContextHandle*            context,
    FoundIncludeDirectiveImplType impl,
    void*                         env) {
    try {
        toCxx(context)->context->get_hooks().found_include_directive_impl.impl = (EntryHandling(*)(
            const WaveContextImplHandle*, const char*, bool, void*))(impl);
        toCxx(context)->context->get_hooks().found_include_directive_impl.env = env;
    }
    DO_CATCH;
}

bool WaveHooksImpl::locate_include_file(
    WaveContextImpl& ctx,
    std::string&     file_path,
    bool             is_system,
    const char*      current_name,
    std::string&     dir_path,
    std::string&     native_name) {

    try {
        if (locate_include_file_impl.isActive()) {
            // FIXME `dir_path` and other mutable strings might need to be
            // corrected for length.
            return locate_include_file_impl(
                (WaveContextImplHandle*)&ctx,
                file_path.data(),
                is_system,
                current_name,
                dir_path.data(),
                native_name.data());
        } else {
            return default_preprocessing_hooks::locate_include_file(
                ctx,
                file_path,
                is_system,
                current_name,
                dir_path,
                native_name);
        }
    }
    DO_CATCH;
}

void wave_setLocateIncludeFile(
    WaveContextHandle*        context,
    LocateIncludeFileImplType impl,
    void*                     env) {
    try {
        toCxx(context)->context->get_hooks().locate_include_file_impl.impl = (EntryHandling(*)(
            WaveContextImplHandle*,
            char*,
            bool,
            char const*,
            char*,
            char*,
            void*))(impl);
        toCxx(context)->context->get_hooks().locate_include_file_impl.env = env;
    }
    DO_CATCH;
}

void WaveHooksImpl::opened_include_file(
    const WaveContextImpl& ctx,
    const std::string&     rel_filename,
    const std::string&     abs_filename,
    bool                   is_system_include) {
    try {
        if (opened_include_file_impl.isActive()) {
            opened_include_file_impl(
                (const WaveContextImplHandle*)&ctx,
                rel_filename.data(),
                abs_filename.data(),
                is_system_include);
        } else {
            default_preprocessing_hooks::opened_include_file(
                ctx, rel_filename, abs_filename, is_system_include);
        }
    }
    DO_CATCH;
}

void wave_setOpenedIncludeFile(
    WaveContextHandle*        context,
    OpenedIncludeFileImplType impl,
    void*                     env) {
    try {
        toCxx(context)->context->get_hooks().opened_include_file_impl.impl = (void (*)(
            const WaveContextImplHandle*,
            const char*,
            const char*,
            bool,
            void*))(impl);
        toCxx(context)->context->get_hooks().opened_include_file_impl.env = env;
    }
    DO_CATCH;
}

void WaveHooksImpl::returning_from_include_file(
    const WaveContextImpl& ctx) {
    try {
        if (returning_from_include_file_impl.isActive()) {
            returning_from_include_file_impl(
                (const WaveContextImplHandle*)&ctx);
        } else {
            default_preprocessing_hooks::returning_from_include_file(ctx);
        }
    }
    DO_CATCH;
}

void wave_setReturningFromIncludeFile(
    WaveContextHandle*               context,
    ReturningFromIncludeFileImplType impl,
    void*                            env) {
    try {
        toCxx(context)
            ->context->get_hooks()
            .returning_from_include_file_impl.impl
            = (void (*)(const WaveContextImplHandle*, void*))(impl);
        toCxx(context)
            ->context->get_hooks()
            .returning_from_include_file_impl.env
            = env;
    }
    DO_CATCH;
}

void WaveHooksImpl::detected_include_guard(
    const WaveContextImpl& ctx,
    const std::string&     filename,
    const std::string&     include_guard) {
    try {
        if (detected_include_guard_impl.isActive()) {
            detected_include_guard_impl(
                (const WaveContextImplHandle*)&ctx,
                filename.data(),
                include_guard.data());
        } else {
            default_preprocessing_hooks::detected_include_guard(
                ctx, filename, include_guard);
        }
    }
    DO_CATCH;
}

void WaveHooksImpl::detected_pragma_once(
    const WaveContextImpl& ctx,
    const WaveToken&       pragma_token,
    const std::string&     filename) {
    try {
        if (detected_pragma_once_impl.isActive()) {
            detected_pragma_once_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenHandle*)&pragma_token,
                filename.data());
        } else {
            default_preprocessing_hooks::detected_pragma_once(
                ctx, pragma_token, filename);
        }
    }
    DO_CATCH;
}

bool WaveHooksImpl::interpret_pragma(
    const WaveContextImpl& ctx,
    WaveTokenList&         pending,
    const WaveToken&       option,
    const WaveTokenList&   values,
    const WaveToken&       pragma_token) {
    try {
        if (interpret_pragma_impl.isActive()) {
            return interpret_pragma_impl(
                (const WaveContextImplHandle*)&ctx,
                (WaveTokenListHandle*)&pending,
                (const WaveTokenHandle*)&option,
                (const WaveTokenListHandle*)&values,
                (const WaveTokenHandle*)&pragma_token);

        } else {
            return default_preprocessing_hooks::interpret_pragma(
                ctx, pending, option, values, pragma_token);
        }
    }
    DO_CATCH;
}

void WaveHooksImpl::defined_macro(
    const WaveContextImpl& ctx,
    const WaveToken&       name,
    bool                   is_functionlike,
    const WaveTokenVector& parameters,
    const WaveTokenList&   definition,
    bool                   is_predefined) {
    try {
        if (defined_macro_impl.isActive()) {
            defined_macro_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenHandle*)&name,
                is_functionlike,
                (const WaveTokenVectorHandle*)&parameters,
                (const WaveTokenListHandle*)&definition,
                is_predefined);

        } else {
            default_preprocessing_hooks::defined_macro(
                ctx,
                name,
                is_functionlike,
                parameters,
                definition,
                is_predefined);
        }
    }
    DO_CATCH;
}

void wave_setDefinedMacro(
    WaveContextHandle*   context,
    DefinedMacroImplType impl,
    void*                env) {
    try {
        toCxx(context)->context->get_hooks().defined_macro_impl.impl = (void (*)(
            const WaveContextImplHandle*,
            const WaveTokenHandle*,
            bool,
            const WaveTokenVectorHandle*,
            const WaveTokenListHandle*,
            bool,
            void*))(impl);
        toCxx(context)->context->get_hooks().defined_macro_impl.env = env;
    }
    DO_CATCH;
}


void WaveHooksImpl::undefined_macro(
    const WaveContextImpl& ctx,
    const WaveToken&       name) {
    try {
        if (undefined_macro_impl.isActive()) {
            undefined_macro_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenHandle*)&name);
        } else {
            default_preprocessing_hooks::undefined_macro(ctx, name);
        }
    }
    DO_CATCH;
}


bool WaveHooksImpl::found_error_directive(
    WaveContextImpl const& ctx,
    WaveTokenList const&   message) {
    try {
        if (found_error_directive_impl.isActive()) {
            auto handling = found_error_directive_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenListHandle*)&message);

            switch (handling) {
                case EntryHandlingRaise: return false;
                case EntryHandlingSkip: return true;
                default:
                    throw std::logic_error(
                        std::string(
                            "'found_error_directive' returned "
                            "unexpected "
                            "entry handling value - wanted 'raise' or "
                            "'skip', but "
                            "got ")
                        + to_string(handling));
            }
        } else {
            return default_preprocessing_hooks::found_error_directive(
                ctx, message);
        }
    }
    DO_CATCH;
}

void wave_setFoundErrorDirective(
    WaveContextHandle*         context,
    FoundLineDirectiveImplType impl,
    void*                      env) {
    try {
        toCxx(context)->context->get_hooks().found_error_directive_impl.impl = (EntryHandling(*)(
            const WaveContextImplHandle*,
            const WaveTokenListHandle*,
            void*))(impl);
        toCxx(context)->context->get_hooks().found_error_directive_impl.env = env;
    }
    DO_CATCH;
}


void WaveHooksImpl::found_line_directive(
    const WaveContextImpl& ctx,
    const WaveTokenList&   arguments,
    unsigned int           line,
    const std::string&     filename) {
    try {
        if (found_line_directive_impl.isActive()) {
            found_line_directive_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenListHandle*)&arguments,
                line,
                filename.data());
        } else {
            default_preprocessing_hooks::found_line_directive(
                ctx, arguments, line, filename);
        }
    }
    DO_CATCH;
}

void wave_setFoundLineDirective(
    WaveContextHandle*         context,
    FoundLineDirectiveImplType impl,
    void*                      env) {
    try {
        toCxx(context)->context->get_hooks().found_line_directive_impl.impl = (EntryHandling(*)(
            const WaveContextImplHandle*,
            const WaveTokenListHandle*,
            unsigned int,
            const char*,
            void*))(impl);
        toCxx(context)->context->get_hooks().found_line_directive_impl.env = env;
    }
    DO_CATCH;
}


bool WaveHooksImpl::emit_line_directive(
    const WaveContextImpl& ctx,
    WaveTokenList&         pending,
    const WaveToken&       act_token) {
    try {
        if (emit_line_directive_impl.isActive()) {
            return emit_line_directive_impl(
                (const WaveContextImplHandle*)&ctx,
                (WaveTokenListHandle*)&pending,
                (const WaveTokenHandle*)&act_token);

        } else {
            return default_preprocessing_hooks::emit_line_directive(
                ctx, pending, act_token);
        }
    }
    DO_CATCH;
}

void wave_setEmitLineDirective(
    WaveContextHandle*        context,
    EmitLineDirectiveImplType impl,
    void*                     env) {
    try {
        toCxx(context)->context->get_hooks().emit_line_directive_impl.impl = (bool (*)(
            const WaveContextImplHandle*,
            WaveTokenListHandle*,
            const WaveTokenHandle*,
            void*))(impl);
        toCxx(context)->context->get_hooks().emit_line_directive_impl.env = env;
    }
    DO_CATCH;
}


bool WaveHooksImpl::found_warning_directive(
    WaveContextImpl const& ctx,
    WaveTokenList const&   message) {
    try {
        if (found_warning_directive_impl.isActive()) {
            auto handling = found_warning_directive_impl(
                (const WaveContextImplHandle*)&ctx,
                (const WaveTokenListHandle*)&message);

            switch (handling) {
                case EntryHandlingRaise: return false;
                case EntryHandlingSkip: return true;
                default:
                    throw std::logic_error(
                        std::string(
                            "'found_warning_directive_impl' returned "
                            "unexpected "
                            "entry handling value - wanted 'raise' or "
                            "'skip', but "
                            "got ")
                        + to_string(handling));
            }
        } else {
            return default_preprocessing_hooks::found_warning_directive(
                ctx, message);
        }
    }
    DO_CATCH;
}

void wave_setFoundWarningDirective(
    WaveContextHandle*            context,
    FoundWarningDirectiveImplType impl,
    void*                         env) {
    try {
        toCxx(context)->context->get_hooks().found_warning_directive_impl.impl = (EntryHandling(*)(
            const WaveContextImplHandle*,
            const WaveTokenListHandle*,
            void*))(impl);
        toCxx(context)->context->get_hooks().found_warning_directive_impl.env = env;
    }
    DO_CATCH;
}

WaveTokId wave_tokGetId(WaveTokenHandle* tok) {
    switch (toCxx(tok)->operator boost::wave::token_id()) {
        case T_UNKNOWN: return tokId_UNKNOWN;
        case T_FIRST_TOKEN: return tokId_FIRST_TOKEN;
        case T_AND: return tokId_AND;
        case T_AND_ALT: return tokId_AND_ALT;
        case T_ANDAND: return tokId_ANDAND;
        case T_ANDAND_ALT: return tokId_ANDAND_ALT;
        case T_ASSIGN: return tokId_ASSIGN;
        case T_ANDASSIGN: return tokId_ANDASSIGN;
        case T_ANDASSIGN_ALT: return tokId_ANDASSIGN_ALT;
        case T_OR: return tokId_OR;
        case T_OR_ALT: return tokId_OR_ALT;
        case T_OR_TRIGRAPH: return tokId_OR_TRIGRAPH;
        case T_ORASSIGN: return tokId_ORASSIGN;
        case T_ORASSIGN_ALT: return tokId_ORASSIGN_ALT;
        case T_ORASSIGN_TRIGRAPH: return tokId_ORASSIGN_TRIGRAPH;
        case T_XOR: return tokId_XOR;
        case T_XOR_ALT: return tokId_XOR_ALT;
        case T_XOR_TRIGRAPH: return tokId_XOR_TRIGRAPH;
        case T_XORASSIGN: return tokId_XORASSIGN;
        case T_XORASSIGN_ALT: return tokId_XORASSIGN_ALT;
        case T_XORASSIGN_TRIGRAPH: return tokId_XORASSIGN_TRIGRAPH;
        case T_COMMA: return tokId_COMMA;
        case T_COLON: return tokId_COLON;
        case T_DIVIDE: return tokId_DIVIDE;
        case T_DIVIDEASSIGN: return tokId_DIVIDEASSIGN;
        case T_DOT: return tokId_DOT;
        case T_DOTSTAR: return tokId_DOTSTAR;
        case T_ELLIPSIS: return tokId_ELLIPSIS;
        case T_EQUAL: return tokId_EQUAL;
        case T_GREATER: return tokId_GREATER;
        case T_GREATEREQUAL: return tokId_GREATEREQUAL;
        case T_LEFTBRACE: return tokId_LEFTBRACE;
        case T_LEFTBRACE_ALT: return tokId_LEFTBRACE_ALT;
        case T_LEFTBRACE_TRIGRAPH: return tokId_LEFTBRACE_TRIGRAPH;
        case T_LESS: return tokId_LESS;
        case T_LESSEQUAL: return tokId_LESSEQUAL;
        case T_LEFTPAREN: return tokId_LEFTPAREN;
        case T_LEFTBRACKET: return tokId_LEFTBRACKET;
        case T_LEFTBRACKET_ALT: return tokId_LEFTBRACKET_ALT;
        case T_LEFTBRACKET_TRIGRAPH: return tokId_LEFTBRACKET_TRIGRAPH;
        case T_MINUS: return tokId_MINUS;
        case T_MINUSASSIGN: return tokId_MINUSASSIGN;
        case T_MINUSMINUS: return tokId_MINUSMINUS;
        case T_PERCENT: return tokId_PERCENT;
        case T_PERCENTASSIGN: return tokId_PERCENTASSIGN;
        case T_NOT: return tokId_NOT;
        case T_NOT_ALT: return tokId_NOT_ALT;
        case T_NOTEQUAL: return tokId_NOTEQUAL;
        case T_NOTEQUAL_ALT: return tokId_NOTEQUAL_ALT;
        case T_OROR: return tokId_OROR;
        case T_OROR_ALT: return tokId_OROR_ALT;
        case T_OROR_TRIGRAPH: return tokId_OROR_TRIGRAPH;
        case T_PLUS: return tokId_PLUS;
        case T_PLUSASSIGN: return tokId_PLUSASSIGN;
        case T_PLUSPLUS: return tokId_PLUSPLUS;
        case T_ARROW: return tokId_ARROW;
        case T_ARROWSTAR: return tokId_ARROWSTAR;
        case T_QUESTION_MARK: return tokId_QUESTION_MARK;
        case T_RIGHTBRACE: return tokId_RIGHTBRACE;
        case T_RIGHTBRACE_ALT: return tokId_RIGHTBRACE_ALT;
        case T_RIGHTBRACE_TRIGRAPH: return tokId_RIGHTBRACE_TRIGRAPH;
        case T_RIGHTPAREN: return tokId_RIGHTPAREN;
        case T_RIGHTBRACKET: return tokId_RIGHTBRACKET;
        case T_RIGHTBRACKET_ALT: return tokId_RIGHTBRACKET_ALT;
        case T_RIGHTBRACKET_TRIGRAPH: return tokId_RIGHTBRACKET_TRIGRAPH;
        case T_COLON_COLON: return tokId_COLON_COLON;
        case T_SEMICOLON: return tokId_SEMICOLON;
        case T_SHIFTLEFT: return tokId_SHIFTLEFT;
        case T_SHIFTLEFTASSIGN: return tokId_SHIFTLEFTASSIGN;
        case T_SHIFTRIGHT: return tokId_SHIFTRIGHT;
        case T_SHIFTRIGHTASSIGN: return tokId_SHIFTRIGHTASSIGN;
        case T_STAR: return tokId_STAR;
        case T_COMPL: return tokId_COMPL;
        case T_COMPL_ALT: return tokId_COMPL_ALT;
        case T_COMPL_TRIGRAPH: return tokId_COMPL_TRIGRAPH;
        case T_STARASSIGN: return tokId_STARASSIGN;
        case T_ASM: return tokId_ASM;
        case T_AUTO: return tokId_AUTO;
        case T_BOOL: return tokId_BOOL;
        case T_FALSE: return tokId_FALSE;
        case T_TRUE: return tokId_TRUE;
        case T_BREAK: return tokId_BREAK;
        case T_CASE: return tokId_CASE;
        case T_CATCH: return tokId_CATCH;
        case T_CHAR: return tokId_CHAR;
        case T_CLASS: return tokId_CLASS;
        case T_CONST: return tokId_CONST;
        case T_CONSTCAST: return tokId_CONSTCAST;
        case T_CONTINUE: return tokId_CONTINUE;
        case T_DEFAULT: return tokId_DEFAULT;
        case T_DELETE: return tokId_DELETE;
        case T_DO: return tokId_DO;
        case T_DOUBLE: return tokId_DOUBLE;
        case T_DYNAMICCAST: return tokId_DYNAMICCAST;
        case T_ELSE: return tokId_ELSE;
        case T_ENUM: return tokId_ENUM;
        case T_EXPLICIT: return tokId_EXPLICIT;
        case T_EXPORT: return tokId_EXPORT;
        case T_EXTERN: return tokId_EXTERN;
        case T_FLOAT: return tokId_FLOAT;
        case T_FOR: return tokId_FOR;
        case T_FRIEND: return tokId_FRIEND;
        case T_GOTO: return tokId_GOTO;
        case T_IF: return tokId_IF;
        case T_INLINE: return tokId_INLINE;
        case T_INT: return tokId_INT;
        case T_LONG: return tokId_LONG;
        case T_MUTABLE: return tokId_MUTABLE;
        case T_NAMESPACE: return tokId_NAMESPACE;
        case T_NEW: return tokId_NEW;
        case T_OPERATOR: return tokId_OPERATOR;
        case T_PRIVATE: return tokId_PRIVATE;
        case T_PROTECTED: return tokId_PROTECTED;
        case T_PUBLIC: return tokId_PUBLIC;
        case T_REGISTER: return tokId_REGISTER;
        case T_REINTERPRETCAST: return tokId_REINTERPRETCAST;
        case T_RETURN: return tokId_RETURN;
        case T_SHORT: return tokId_SHORT;
        case T_SIGNED: return tokId_SIGNED;
        case T_SIZEOF: return tokId_SIZEOF;
        case T_STATIC: return tokId_STATIC;
        case T_STATICCAST: return tokId_STATICCAST;
        case T_STRUCT: return tokId_STRUCT;
        case T_SWITCH: return tokId_SWITCH;
        case T_TEMPLATE: return tokId_TEMPLATE;
        case T_THIS: return tokId_THIS;
        case T_THROW: return tokId_THROW;
        case T_TRY: return tokId_TRY;
        case T_TYPEDEF: return tokId_TYPEDEF;
        case T_TYPEID: return tokId_TYPEID;
        case T_TYPENAME: return tokId_TYPENAME;
        case T_UNION: return tokId_UNION;
        case T_UNSIGNED: return tokId_UNSIGNED;
        case T_USING: return tokId_USING;
        case T_VIRTUAL: return tokId_VIRTUAL;
        case T_VOID: return tokId_VOID;
        case T_VOLATILE: return tokId_VOLATILE;
        case T_WCHART: return tokId_WCHART;
        case T_WHILE: return tokId_WHILE;
        case T_PP_DEFINE: return tokId_PP_DEFINE;
        case T_PP_IF: return tokId_PP_IF;
        case T_PP_IFDEF: return tokId_PP_IFDEF;
        case T_PP_IFNDEF: return tokId_PP_IFNDEF;
        case T_PP_ELSE: return tokId_PP_ELSE;
        case T_PP_ELIF: return tokId_PP_ELIF;
        case T_PP_ENDIF: return tokId_PP_ENDIF;
        case T_PP_ERROR: return tokId_PP_ERROR;
        case T_PP_LINE: return tokId_PP_LINE;
        case T_PP_PRAGMA: return tokId_PP_PRAGMA;
        case T_PP_UNDEF: return tokId_PP_UNDEF;
        case T_PP_WARNING: return tokId_PP_WARNING;
        case T_IDENTIFIER: return tokId_IDENTIFIER;
        case T_OCTALINT: return tokId_OCTALINT;
        case T_DECIMALINT: return tokId_DECIMALINT;
        case T_HEXAINT: return tokId_HEXAINT;
        case T_INTLIT: return tokId_INTLIT;
        case T_LONGINTLIT: return tokId_LONGINTLIT;
        case T_FLOATLIT: return tokId_FLOATLIT;
        case T_FIXEDPOINTLIT: return tokId_FIXEDPOINTLIT;
        case T_CCOMMENT: return tokId_CCOMMENT;
        case T_CPPCOMMENT: return tokId_CPPCOMMENT;
        case T_CHARLIT: return tokId_CHARLIT;
        case T_STRINGLIT: return tokId_STRINGLIT;
        case T_CONTLINE: return tokId_CONTLINE;
        case T_SPACE: return tokId_SPACE;
        case T_SPACE2: return tokId_SPACE2;
        case T_NEWLINE: return tokId_NEWLINE;
        case T_GENERATEDNEWLINE: return tokId_GENERATEDNEWLINE;
        case T_POUND_POUND: return tokId_POUND_POUND;
        case T_POUND_POUND_ALT: return tokId_POUND_POUND_ALT;
        case T_POUND_POUND_TRIGRAPH: return tokId_POUND_POUND_TRIGRAPH;
        case T_POUND: return tokId_POUND;
        case T_POUND_ALT: return tokId_POUND_ALT;
        case T_POUND_TRIGRAPH: return tokId_POUND_TRIGRAPH;
        case T_ANY: return tokId_ANY;
        case T_ANY_TRIGRAPH: return tokId_ANY_TRIGRAPH;
        case T_PP_INCLUDE: return tokId_PP_INCLUDE;
        case T_PP_QHEADER: return tokId_PP_QHEADER;
        case T_PP_HHEADER: return tokId_PP_HHEADER;
        case T_PP_INCLUDE_NEXT: return tokId_PP_INCLUDE_NEXT;
        case T_PP_QHEADER_NEXT: return tokId_PP_QHEADER_NEXT;
        case T_PP_HHEADER_NEXT: return tokId_PP_HHEADER_NEXT;
        case T_EOF: return tokId_EOF;
        case T_EOI: return tokId_EOI;
        case T_PP_NUMBER: return tokId_PP_NUMBER;
        case T_MSEXT_INT8: return tokId_MSEXT_INT8;
        case T_MSEXT_INT16: return tokId_MSEXT_INT16;
        case T_MSEXT_INT32: return tokId_MSEXT_INT32;
        case T_MSEXT_INT64: return tokId_MSEXT_INT64;
        case T_MSEXT_BASED: return tokId_MSEXT_BASED;
        case T_MSEXT_DECLSPEC: return tokId_MSEXT_DECLSPEC;
        case T_MSEXT_CDECL: return tokId_MSEXT_CDECL;
        case T_MSEXT_FASTCALL: return tokId_MSEXT_FASTCALL;
        case T_MSEXT_STDCALL: return tokId_MSEXT_STDCALL;
        case T_MSEXT_TRY: return tokId_MSEXT_TRY;
        case T_MSEXT_EXCEPT: return tokId_MSEXT_EXCEPT;
        case T_MSEXT_FINALLY: return tokId_MSEXT_FINALLY;
        case T_MSEXT_LEAVE: return tokId_MSEXT_LEAVE;
        case T_MSEXT_INLINE: return tokId_MSEXT_INLINE;
        case T_MSEXT_ASM: return tokId_MSEXT_ASM;

        case T_MSEXT_PP_REGION: return tokId_MSEXT_PP_REGION;
        case T_MSEXT_PP_ENDREGION: return tokId_MSEXT_PP_ENDREGION;
        case T_IMPORT: return tokId_IMPORT;
        case T_ALIGNAS: return tokId_ALIGNAS;
        case T_ALIGNOF: return tokId_ALIGNOF;
        case T_CHAR16_T: return tokId_CHAR16_T;
        case T_CHAR32_T: return tokId_CHAR32_T;
        case T_CONSTEXPR: return tokId_CONSTEXPR;
        case T_DECLTYPE: return tokId_DECLTYPE;
        case T_NOEXCEPT: return tokId_NOEXCEPT;
        case T_NULLPTR: return tokId_NULLPTR;
        case T_STATICASSERT: return tokId_STATICASSERT;
        case T_THREADLOCAL: return tokId_THREADLOCAL;
        case T_RAWSTRINGLIT: return tokId_RAWSTRINGLIT;
        case T_CHAR8_T: return tokId_CHAR8_T;
        case T_CONCEPT: return tokId_CONCEPT;
        case T_CONSTEVAL: return tokId_CONSTEVAL;
        case T_CONSTINIT: return tokId_CONSTINIT;
        case T_CO_AWAIT: return tokId_CO_AWAIT;
        case T_CO_RETURN: return tokId_CO_RETURN;
        case T_CO_YIELD: return tokId_CO_YIELD;
        case T_REQUIRES: return tokId_REQUIRES;
        case T_SPACESHIP: return tokId_SPACESHIP;

        case T_LAST_TOKEN_ID: return tokId_LAST_TOKEN_ID;
        case T_LAST_TOKEN: return tokId_LAST_TOKEN;

        case T_UNKNOWN_UNIVERSALCHAR: return tokId_UNKNOWN_UNIVERSALCHAR;
        case T_NONREPLACABLE_IDENTIFIER:
            return tokId_NONREPLACABLE_IDENTIFIER;
        case T_PLACEHOLDER: return tokId_PLACEHOLDER;
        case T_PLACEMARKER: return tokId_PLACEMARKER;
        case T_PARAMETERBASE: return tokId_PARAMETERBASE;
        case T_EXTPARAMETERBASE: return tokId_EXTPARAMETERBASE;
        case T_OPTPARAMETERBASE: return tokId_OPTPARAMETERBASE;
    }
}


const char* wave_tokGetValue(WaveTokenHandle* tok) {
    try {
        return toCxx(tok)->get_value().c_str();
    }
    DO_CATCH;
}


WaveTokenHandle* wave_iterGetTok(WaveIteratorHandle* iter) {
    try {
        return (WaveTokenHandle*)&(*(toCxx(iter)->d));
    }
    DO_CATCH;
}

void wave_deleteTok(WaveTokenHandle* tok) {
    try {
        delete (WaveToken*)(tok);
    }
    DO_CATCH;
}


bool wave_neqIterator(
    WaveIteratorHandle* iter1,
    WaveIteratorHandle* iter2) {
    const CxxWaveIterator* it1 = toCxx(iter1);
    const CxxWaveIterator* it2 = toCxx(iter2);

    try {
        //        std::cout << "Comparing iterators ... " << std::flush;
        bool res = it1->d != it2->d;
        //        std::cout << "done\n" << std::flush;
        return res;
    } catch (boost::wave::cpplexer::lexing_exception& e) {
        GOT_ERR;
        it1->ctx->context->get_hooks().throw_exception(
            *it1->ctx->context, e);
        return true;
    } catch (...) { ANY_FAIL(); }
}

void wave_advanceIterator(WaveIteratorHandle* iter) {
    try {
        ++(toCxx(iter)->d);
    } catch (std::exception& e) {
        GOT_ERR;
        toCxx(iter)->ctx->context->get_hooks().throw_exception(
            *toCxx(iter)->ctx->context, e);
    } catch (...) {
        //
        ANY_FAIL();
        //
    };
}


WaveContext::~WaveContext() { std::cerr << "Destroying wave context"; }


WaveContext::WaveContext(std::string _text, const char* filename) {
    try {
        text    = _text;
        context = new WaveContextImpl(text.begin(), text.end(), filename);
        this->context->get_hooks().context = this;
    }
    DO_CATCH;
}

void WaveContext::processAll() {
    try {
        auto first = context->begin(text.begin(), text.end());
        auto last  = context->end();

        while (first != last) {
            std::cout << (*first).get_value();
            ++first;
        }
    }
    DO_CATCH;
}

WaveIteratorHandle* wave_beginIterator(WaveContextHandle* context) {
    try {
        auto cxx       = toCxx(context);
        cxx->lastBegin = new CxxWaveIterator(
            cxx->context->begin(cxx->text.begin(), cxx->text.end()), cxx);

        return (WaveIteratorHandle*)(cxx->lastBegin);
    }
    DO_CATCH;
}

WaveIteratorHandle* wave_endIterator(WaveContextHandle* context) {
    try {
        return (WaveIteratorHandle*)(new CxxWaveIterator(
            toCxx(context)->context->end(), toCxx(context)));
    }
    DO_CATCH;
}

WaveContextHandle* wave_newWaveContext(
    const char* instring,
    const char* filename) {
    try {
        auto res = new WaveContext(std::string(instring), filename);
        return (WaveContextHandle*)(res);
    }
    DO_CATCH;
}
void wave_setLanguageMode(
    WaveContextHandle*   ctx,
    WaveLanguageModeImpl mode) {
    try {
        auto c = toCxx(ctx)->context;

        language_support get = c->get_language();
        language_support res = support_normal;
        using boost::wave::language_support;


        switch (mode) {
            case iwlmSupportNormal: {
                c->set_language(support_normal);
            } break;
            case iwlmC99: {
                c->set_language(support_c99);
            } break;
            case iwlmCpp11: {
                c->set_language(support_cpp11);
            } break;
            case iwlmCpp17: {
                c->set_language(support_cpp17);
            } break;
            case iwlmCpp20: {
                c->set_language(support_cpp20);
            } break;

            case iwlmLongLong: {
                c->set_language(enable_long_long(get));
            } break;
            case iwlmVariadics: {
                c->set_language(enable_variadics(get));
            } break;
            case iwlmNoNewlineAtEndOfFIle: {
                c->set_language(enable_no_newline_at_end_of_file(get));
            } break;
            case iwlmHasInclude: {
                c->set_language(enable_has_include(get));
            } break;
            case iwlmVaOpt: {
                c->set_language(enable_va_opt(get));
            } break;
            case iwlmEmitContline: {
                c->set_language(enable_emit_contnewlines(get));
            } break;
            case iwlmInsertWhitespace: {
                c->set_language(enable_insert_whitespace(get));
            } break;
            case iwlmPreserveComments: {
                c->set_language(enable_preserve_comments(get));
            } break;
            case iwlmNoCharacterValidation: {
                c->set_language(enable_no_character_validation(get));
            } break;
            case iwlmConvertTrigraphs: {
                c->set_language(enable_convert_trigraphs(get));
            } break;
            case iwlmSingleLine: {
                c->set_language(enable_single_line(get));
            } break;
            case iwlmPreferPpNumbers: {
                c->set_language(enable_prefer_pp_numbers(get));
            } break;
            case iwlmEmitLineDirectives: {
                c->set_language(enable_emit_line_directives(get));
            } break;
            case iwlmIncludeGuardDetection: {
                c->set_language(enable_include_guard_detection(get));
            } break;
            case iwlmEmitPragmaDirectives: {
                c->set_language(enable_emit_pragma_directives(get));
            } break;
        }
    }
    DO_CATCH;
}

void wave_destroyContext(WaveContextHandle* context) {
    try {
        delete toCxx(context);
    }
    DO_CATCH;
}


bool wave_contextHasErrors(WaveContextHandle* context) {
    try {
        return 0 < toCxx(context)->errorCount;
    }
    DO_CATCH;
}

bool wave_contextHasWarnings(WaveContextHandle* context) {
    try {
        return toCxx(context)->diagnostics.size() > 0;
    }
    DO_CATCH;
}

WaveDiagnostics wave_contextPopDiagnostics(WaveContextHandle* context) {
    try {
        auto res = toCxx(context)->diagnostics.front();
        toCxx(context)->diagnostics.pop();
        if (res.level == wslError) {
            toCxx(context)->errorCount--;
        }
        return res;
    }
    DO_CATCH;
}

void wave_contextSetData(WaveContextHandle* context, void* data) {
    try {
        toCxx(context)->contextData = data;
    }
    DO_CATCH;
}

void* wave_contextGetData(WaveContextHandle* context) {
    try {
        return toCxx(context)->contextData;
    }
    DO_CATCH;
}


void wave_deleteDiagnostics(WaveDiagnostics diag) {
    try {
        std::free(diag.filename);
        std::free(diag.errorText);
    }
    DO_CATCH;
}


void wave_processAll(WaveContextHandle* context) {
    try {
        toCxx(context)->processAll();
    }
    DO_CATCH;
}


void wave_setExpandingFunctionLikeMacro(
    WaveContextHandle*                 context,
    ExpandingFunctionLikeMacroImplType impl,
    void*                              env) {
    try {
        toCxx(context)
            ->context->get_hooks()
            .expanding_function_like_macro_impl.impl
            = (bool (*)(void*, void*))(impl);
        toCxx(context)
            ->context->get_hooks()
            .expanding_function_like_macro_impl.env
            = env;
    }
    DO_CATCH;
}


// void wave_setFoundUnknonwDirective(
//    CWaveContext*                context,
//    CFoundUnknownDirectiveCbType impl) {
//    toCxx(context)->context->get_hooks().found_unknown_directive_impl =
//    (EntryHandling(*)(
//        const WaveContextImpl*,
//        const WaveTokenList*,
//        WaveTokenList*,
//        void*))(impl);
//}

// void wave_setFoundWarningDirective(
//    CWaveContext*         context,
//    CFoundDirectiveCbType impl) {
//    toCxx(context)->context->get_hooks().found_directive_impl =
//    (EntryHandling(*)(
//        const WaveContextImpl*, const WaveToken*, void*))(impl);
//}

void wave_destroyContext(WaveContext* context) { delete context; }

bool wave_addMacroDefinition(
    WaveContextHandle* context,
    const char*        macrostring,
    bool               is_predefined) {
    WaveContext*     ctx  = toCxx(context);
    WaveContextImpl* ctx1 = ctx->context;
    try {
        return ctx1->add_macro_definition(macrostring, is_predefined);
    } catch (boost::wave::cpp_exception& e) {
        ctx1->get_hooks().throw_exception(*ctx1, e);
        return false;
    } catch (...) { ANY_FAIL(); }
}

bool wave_removeMacroDefinition(
    WaveContextHandle* context,
    const char*        macrostring,
    bool               is_predefined) {
    try {
        return toCxx(context)->context->remove_macro_definition(
            macrostring, is_predefined);
    }
    DO_CATCH;
}

bool wave_isDefinedMacro(WaveContextHandle* context, const char* name) {
    try {
        return toCxx(context)->context->is_defined_macro(name);
    }
    DO_CATCH;
}

bool wave_getMacroDefinition(
    WaveContextHandle*      context,
    const char*             name,
    bool*                   is_function_style,
    bool*                   is_predefined,
    WavePosition*           pos,
    WaveTokenVectorHandle** parameters,
    WaveTokenListHandle**   definition) {
    try {
        auto                     outParams = new std::vector<WaveToken>();
        auto                     outDefinition = new WaveTokenList();
        util::file_position_type outPos;
        bool res = toCxx(context)->context->get_macro_definition(
            name,
            *is_function_style,
            *is_predefined,
            outPos,
            *outParams,
            *outDefinition);

        *parameters = (WaveTokenVectorHandle*)outParams;
        *definition = (WaveTokenListHandle*)outDefinition;

        return res;
    }
    DO_CATCH;
}

int wave_tokenVectorLen(WaveTokenVectorHandle* vec) {
    try {
        return toCxx(vec)->size();
    }
    DO_CATCH;
}

WaveTokenHandle* wave_tokenVectorGetAt(
    WaveTokenVectorHandle* vec,
    int                    idx) {
    try {
        WaveToken* tmp = &(toCxx(vec)->operator[](idx));
        return (WaveTokenHandle*)tmp;
    }
    DO_CATCH;
}

void wave_deleteWaveTokenVector(WaveTokenVectorHandle* vec) {
    try {
        delete toCxx(vec);
    }
    DO_CATCH;
}

int wave_tokenListLen(WaveTokenListHandle* list) {
    try {
        return toCxx(list)->size();
    }
    DO_CATCH;
}

const char* wave_tokenListToStr(WaveTokenListHandle* list) {
    try {
        return util::impl::as_string(*toCxx(list)).c_str();
    }
    DO_CATCH;
}


WaveTokenListIteratorHandle* wave_tokenListBeginIterator(
    WaveTokenListHandle* l) {
    try {
        return (WaveTokenListIteratorHandle*)new WaveTokenList::iterator(
            toCxx(l)->begin());
    }
    DO_CATCH;
}

WaveTokenListIteratorHandle* wave_tokenListEndIterator(
    WaveTokenListHandle* l) {
    try {
        return (WaveTokenListIteratorHandle*)new WaveTokenList::iterator(
            toCxx(l)->end());
    }
    DO_CATCH;
}

bool wave_neqListIterator(
    WaveTokenListIteratorHandle* i1,
    WaveTokenListIteratorHandle* i2) {
    try {
        return (*toCxx(i1)) != (*toCxx(i2));
    }
    DO_CATCH;
}

WaveTokenHandle* wave_listIterDeref(WaveTokenListIteratorHandle* i) {
    WaveToken* tmp = &(**toCxx(i));
    try {
        return (WaveTokenHandle*)tmp;
    }
    DO_CATCH;
}

void wave_listIterAdvance(WaveTokenListIteratorHandle* i) {
    try {
        ++(*toCxx(i));
    }
    DO_CATCH;
}


bool wave_addSysincludePath(WaveContextHandle* context, char const* path) {
    try {
        return toCxx(context)->context->add_sysinclude_path(path);
    }
    DO_CATCH;
}

bool wave_addIncludePath(WaveContextHandle* context, char const* path) {
    try {
        return toCxx(context)->context->add_include_path(path);
    }
    DO_CATCH;
}

void wave_setSysincludeDelimiter(WaveContextHandle* context) {
    try {
        toCxx(context)->context->set_sysinclude_delimiter();
    }
    DO_CATCH;
}

void wave_setCurrentFilename(
    WaveContextHandle* context,
    const char*        name) {
    try {
        toCxx(context)->context->set_current_filename(name);
    }
    DO_CATCH;
}

const char* wave_getCurrentFilename(WaveContextHandle* context) {
    try {
        return toCxx(context)->context->get_current_filename().c_str();
    }
    DO_CATCH;
}

const char* wave_getCurrentDirectory(WaveContextHandle* context) {
    try {
        return toCxx(context)->context->get_current_directory().c_str();
    }
    DO_CATCH;
}

bool wave_findIncludeFile(
    WaveContextHandle* ctx,
    char**             str,
    char**             dir,
    bool               is_system,
    char const*        current_file) {
    try {
        std::string s{*str};
        std::string d;
        bool        res = toCxx(ctx)->context->find_include_file(
            s, d, is_system, current_file);
        //    std::cout << "s: " << s << std::endl;
        //    std::cout << "d: " << d << std::endl;
        *str = copyalloc(s.c_str());
        *dir = copyalloc(d.c_str());

        return res;
    }
    DO_CATCH;
}

WaveMacroIteratorHandle* wave_macroBeginIterator(
    WaveContextHandle* context) {
    try {
        return (WaveMacroIteratorHandle*)new WaveMacroNameIterator(
            toCxx(context)->context->macro_names_begin());
    }
    DO_CATCH;
}

WaveMacroIteratorHandle* wave_macroEndIterator(
    WaveContextHandle* context) {
    try {
        return (WaveMacroIteratorHandle*)new WaveMacroNameIterator(
            toCxx(context)->context->macro_names_end());
    }
    DO_CATCH;
}

bool wave_neqMacroIterator(
    WaveMacroIteratorHandle* i1,
    WaveMacroIteratorHandle* i2) {
    try {
        return *toCxx(i1) != *toCxx(i2);
    }
    DO_CATCH;
}

void wave_macroIteratorAdvance(WaveMacroIteratorHandle* i) {
    try {
        WaveMacroNameIterator* iter = toCxx(i);
        **iter;
        iter->operator++();
    }
    DO_CATCH;
}
const char* wave_macroIteratorDeref(WaveMacroIteratorHandle* i) {
    try {
        return (*(*toCxx(i))).c_str();
    }
    DO_CATCH;
}


int getIterationDepth(WaveContextHandle* context) {
    try {
        return toCxx(context)->context->get_iteration_depth();
    }
    DO_CATCH;
}


const char* wave_unescapeIncludeToken(const char* s) {
    try {
        auto tmp = boost::wave::util::impl::unescape_lit(std::string(s));
        return copyalloc(tmp.c_str());
    }
    DO_CATCH;
}


const char* wave_currentFile(WaveContextHandle* ctx) {
    return toCxx(ctx)->lastBegin->d->get_position().get_file().c_str();
}

int wave_currentLine(WaveContextHandle* ctx) {
    return toCxx(ctx)->lastBegin->d->get_position().get_line();
}

int wave_currentColumn(WaveContextHandle* ctx) {
    return toCxx(ctx)->lastBegin->d->get_position().get_column();
}
