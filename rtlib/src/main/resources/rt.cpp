#include <exception>
#include <cxxabi.h>
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>

namespace __cxxabiv1 {
extern "C" void *__cxa_begin_catch (void *);
extern "C" void __cxa_end_catch ();
}

namespace scalanative {
    class ExceptionWrapper: public std::exception {
    public:
        ExceptionWrapper(void* _obj): obj(_obj) { }
        void* obj;
    };
}

extern "C" {
    void scalanative_throw(void* obj) {
        throw new scalanative::ExceptionWrapper(obj);
    }

    void* scalanative_begin_catch(void* wrapper) {
        __cxxabiv1::__cxa_begin_catch(wrapper);
        return ((scalanative::ExceptionWrapper*) wrapper)->obj;
    }

    void scalanative_end_catch() {
        __cxxabiv1::__cxa_end_catch();
    }

    void* scalanative_alloc(void* info, size_t size) {
        void** alloc = (void**) GC_malloc(size);
        *alloc = info;
        return (void*) alloc;
    }

    void scalanative_init() {
        GC_init();
    }

    void* scalanative_libc_stdin() {
        return stdin;
    }

    void* scalanative_libc_stdout() {
        return stdout;
    }

    void* scalanative_libc_stderr() {
        return stderr;
    }

    int scalanative_libc_eof() {
        return EOF;
    }

    unsigned int scalanative_libc_fopen_max() {
        return FOPEN_MAX;
    }

    unsigned int scalanative_libc_filename_max() {
        return FILENAME_MAX;
    }

    unsigned int scalanative_libc_bufsiz() {
        return BUFSIZ;
    }

    int scalanative_libc_iofbf() {
        return _IOFBF;
    }

    int scalanative_libc_iolbf() {
        return _IOLBF;
    }

    int scalanative_libc_ionbf() {
        return _IONBF;
    }

    int scalanative_libc_seek_set() {
        return SEEK_SET;
    }

    int scalanative_libc_seek_cur() {
        return SEEK_CUR;
    }

    int scalanative_libc_seek_end() {
        return SEEK_END;
    }

    unsigned int scalanative_libc_tmp_max() {
        return TMP_MAX;
    }

    unsigned int scalanative_libc_l_tmpnam() {
        return L_tmpnam;
    }

    int scalanative_libc_exit_success() {
        return EXIT_SUCCESS;
    }

    int scalanative_libc_exit_failure() {
        return EXIT_FAILURE;
    }

    typedef void (*sig_handler_t)(int);

    sig_handler_t scalanative_libc_sig_dfl() {
        return SIG_DFL;
    }

    sig_handler_t scalanative_libc_sig_ign() {
        return SIG_IGN;
    }

    sig_handler_t scalanative_libc_sig_err() {
        return SIG_ERR;
    }

    int scalanative_libc_sigabrt() {
        return SIGABRT;
    }

    int scalanative_libc_sigfpe() {
        return SIGFPE;
    }

    int scalanative_libc_sigill() {
        return SIGILL;
    }

    int scalanative_libc_sigint() {
        return SIGINT;
    }

    int scalanative_libc_sigsegv() {
        return SIGSEGV;
    }

    int scalanative_libc_sigterm() {
        return SIGTERM;
    }

    int scalanative_libc_rand_max() {
        return RAND_MAX;
    }
}

