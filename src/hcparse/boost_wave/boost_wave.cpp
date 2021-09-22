#include "boost_wave.hpp"
#include <memory>


const char* to_string(EntryHandling handling) {
    switch (handling) {
        case EntryHandlingSkip: return "skip";
        case EntryHandlingProcess: return "skip";
        case EntryHandlingRaise: return "skip";
    }
}


bool WaveHooksImpl::found_warning_directive(
    WaveContextImpl const& ctx,
    WaveTokenList const&   message) {
    if (found_warning_directive_impl.isActive()) {
        auto handling = found_warning_directive_impl(&ctx, &message);

        switch (handling) {
            case EntryHandlingRaise: return false;
            case EntryHandlingSkip: return true;
            default:
                throw std::logic_error(
                    std::string("'found_warning_directive_impl' returned "
                                "unexpected "
                                "entry handling value - wanted 'raise' or "
                                "'skip', but "
                                "got ")
                    + to_string(handling));
        }
    } else {
        return false;
    }
}

bool WaveHooksImpl::found_directive(
    const WaveContextImpl& ctx,
    const WaveToken&       token) {
    if (found_directive_impl.isActive()) {
        auto handling = found_directive_impl(&ctx, &token);

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
        return false;
    }
}

bool WaveHooksImpl::found_unknown_directive(
    const WaveContextImpl& ctx,
    const WaveTokenList&   line,
    WaveTokenList&         pending) {
    if (found_unknown_directive_impl.isActive()) {
        auto handling = found_unknown_directive_impl(
            &ctx, &line, &pending);

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
        return false;
    }
}

bool WaveHooksImpl::may_skip_whitespace(
    WaveContextImpl const& ctx,
    WaveToken&             token,
    bool&                  skipped_newline) {
    if (may_skip_whitespace_impl.isActive()) {
        return may_skip_whitespace_impl(&ctx, &token, &skipped_newline);
    } else {
        return false;
    }
}

void WaveHooksImpl::skipped_token(
    WaveContextImpl const& ctx,
    WaveToken const&       token) {
    if (skipped_token_impl.isActive()) {
        skipped_token_impl(&ctx, &token);
    }
}


bool WaveHooksImpl::evaluated_conditional_expression(
    WaveContextImpl const& ctx,
    WaveToken const&       directive,
    WaveTokenList const&   expression,
    bool                   expression_value) {
    if (evaluated_conditional_expression_impl.isActive()) {
        return evaluated_conditional_expression_impl(
            &ctx, &directive, &expression, expression_value);
    } else {
        return false;
    }
}


bool WaveHooksImpl::expanding_function_like_macro(
    WaveContextImpl const&                ctx,
    WaveToken const&                      macrodef,
    WaveTokenVector const&                formal_args,
    WaveTokenList const&                  definition,
    WaveToken const&                      macrocall,
    WaveTokenListVector const&            arguments,
    WaveContextImpl::iterator_type const& seqstart,
    WaveContextImpl::iterator_type const& seqend) {


    if (expanding_function_like_macro_impl.isActive()) {
        CxxWaveToken*     macrodef_c   = new CxxWaveToken{macrodef};
        CxxWaveToken*     macrocall_c  = new CxxWaveToken{macrocall};
        CxxWaveTokenList* definition_c = new CxxWaveTokenList{definition};
        CxxWaveIterator*  seqstart_c   = new CxxWaveIterator{seqstart};
        CxxWaveIterator*  seqend_c     = new CxxWaveIterator{seqend};

        return expanding_function_like_macro_impl(
            &ctx,                      // ctx,
            (CWaveToken*)macrodef_c,   // macrodef,
            NULL,                      // formal_args,
            (CWaveToken*)definition_c, // definition,
            (CWaveToken*)macrocall_c,  // macrocall,
            NULL,                      // arguments,
            (CWaveToken*)seqstart_c,   // seqstart,
            (CWaveToken*)seqend_c      // seqend

        );

        delete macrodef_c;
        delete macrocall_c;
        delete definition_c;
        delete seqstart_c;
        delete seqend_c;
    } else {
        return false;
    }
}

void WaveHooksImpl::throw_exception(
    const WaveContextImpl& ctx,
    const std::exception&  e) {

    bool isError = false;

    if (const cpp_exception* exception = dynamic_cast<
            const cpp_exception*>(&e)) {
        const char* filename    = exception->file_name();
        const char* description = exception->description();

        auto diag = WaveDiagnostics{
            (int)exception->line_no(),
            (int)exception->column_no(),
            (WaveErrorCode)exception->get_errorcode(),
            (WaveSeverityLevel)exception->get_severity(),
            copyalloc(filename),
            copyalloc(description),
        };

        this->context->diagnostics.push(diag);
    }


    if (isError) {
        this->context->hasError = true;
    }
}

CWaveTokId wave_tokGetId(CWaveToken* tok) {
    switch (toCxx(tok)->d.operator boost::wave::token_id()) {
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

const char* wave_tokGetValue(CWaveToken* tok) {
    return toCxx(tok)->d.get_value().c_str();
}


CWaveToken* wave_iterGetTok(CWaveIterator* iter) {
    return (CWaveToken*)(new CxxWaveToken{*(toCxx(iter)->d)});
}

void wave_deleteTok(CWaveToken* tok) { delete (CxxWaveToken*)(tok); }

bool wave_neqIterator(CWaveIterator* iter1, CWaveIterator* iter2) {
    const CxxWaveIterator* it1 = toCxx(iter1);
    const CxxWaveIterator* it2 = toCxx(iter2);
    return it1->d != it2->d;
}

void wave_advanceIterator(CWaveIterator* iter) {
    CxxWaveIterator* it = toCxx(iter);
    ++(it->d);
}


WaveContext::WaveContext(std::string _text, const char* filename) {
    text    = _text;
    context = std::make_unique<WaveContextImpl>(
        text.begin(), text.end(), filename);
}

void WaveContext::processAll() {
    auto first = context->begin(text.begin(), text.end());
    auto last  = context->end();

    while (first != last) {
        current_position = (*first).get_position();
        std::cout << (*first).get_value();
        ++first;
    }
}

CWaveIterator* wave_beginIterator(CWaveContext* context) {
    return (CWaveIterator*)(new CxxWaveIterator{
        toCxx(context)->d.context->begin()});
}

CWaveIterator* wave_endIterator(CWaveContext* context) {
    return (CWaveIterator*)(new CxxWaveIterator{
        toCxx(context)->d.context->end()});
}

CWaveContext* wave_newWaveContext(
    const char* instring,
    const char* filename) {
    auto res = new WaveContext(std::string(instring), filename);
    res->context->get_hooks().context = res;
    return (CWaveContext*)(res);
}


bool wave_contextHasError(CWaveContext* context) {
    return toCxx(context)->d.hasError;
}

bool wave_contextHasWarnings(CWaveContext* context) {
    return toCxx(context)->d.diagnostics.size() > 0;
}

WaveDiagnostics wave_contextPopWarning(CWaveContext* context) {
    auto res = toCxx(context)->d.diagnostics.front();
    toCxx(context)->d.diagnostics.pop();
    return res;
}

void wave_contextSetData(CWaveContext* context, void* data) {
    toCxx(context)->d.contextData = data;
}

void* wave_contextGetData(CWaveContext* context) {
    return toCxx(context)->d.contextData;
}


void wave_deleteDiagnostics(WaveDiagnostics diag) {
    std::free(diag.filename);
    std::free(diag.errorText);
}


void wave_processAll(CWaveContext* context) {
    toCxx(context)->d.processAll();
}

void wave_setFoundWarningDirective(
    CWaveContext*                context,
    CFoundWarningDirectiveCbType impl) {
    toCxx(context)->d.context->get_hooks().found_warning_directive_impl = (EntryHandling(*)(
        const WaveContextImpl*, const WaveTokenList*, void*))(impl);
}


void wave_setFoundUnknonwDirective(
    CWaveContext*                context,
    CFoundUnknownDirectiveCbType impl) {
    toCxx(context)->d.context->get_hooks().found_unknown_directive_impl = (EntryHandling(*)(
        const WaveContextImpl*,
        const WaveTokenList*,
        WaveTokenList*,
        void*))(impl);
}

void wave_setFoundWarningDirective(
    CWaveContext*         context,
    CFoundDirectiveCbType impl) {
    toCxx(context)->d.context->get_hooks().found_directive_impl = (EntryHandling(*)(
        const WaveContextImpl*, const WaveToken*, void*))(impl);
}

void wave_destroyContext(WaveContext* context) { delete context; }
