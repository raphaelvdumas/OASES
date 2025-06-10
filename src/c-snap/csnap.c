#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "util_env.h"


int copy_file(const char *src, const char *dst) {
    FILE *in = fopen(src, "rb");
    if (!in) {
        perror("Failed to open source file");
        return -1;
    }

    FILE *out = fopen(dst, "wb");
    if (!out) {
        perror("Failed to open destination file");
        fclose(in);
        return -1;
    }

    char buffer[8192];
    size_t n;
    while ((n = fread(buffer, 1, sizeof(buffer), in)) > 0) {
        if (fwrite(buffer, 1, n, out) != n) {
            perror("Write error");
            fclose(in);
            fclose(out);
            return -1;
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}

// #-- Content of c-snap.sh
// #!/bin/csh  -v
// limit stacksize unlimited
// limit datasize unlimited
// ln -fs "$1".dat   c-snap.dat
// #time   ${OASES_BIN}/c-snap
// c-snap
// #
// mv c-snap.plp   "$1".plp
// mv c-snap.plt   "$1".plt
// mv c-snap.cdr   "$1".cdr
// mv c-snap.bdr   "$1".bdr
// mv c-snapfr.cdr   "$1"_fr.cdr
// mv c-snapfr.bdr   "$1"_fr.bdr
// mv c-snap.prt   "$1".prt
// mv c-snap.trf   "$1".trf
// #plot "$1"
// #cplot "$1"
// #cplot "$1"_fr

// #!/bin/csh
#define N 8
size_t iarg[N] = {1, 1, 1, 1, 1, 1, 1, 1};
SuffixMapping m[N] = {
    {"c-snap", ".plp"}, // setenv $1.plp
    {"c-snap", ".plt"}, // setenv $1.plt
    {"c-snap", ".cdr"}, // setenv $1.cdr
    {"c-snap", ".bdr"}, // setenv $1.bdr
    {"c-snap", ".prt"}, // setenv $1.prt
    {"c-snap", ".trf"},  // setenv $1.trf
    {"c-snap", "fr.cdr"}, // setenv $1_fr.cdr
    {"c-snap", "fr.bdr"} // setenv $1_fr.bdr
};

extern void csnapf_();

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <filename_prefix>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char src[1024], dst[] = "c-snap.dat";
    snprintf(src, sizeof(src), "%s.dat", argv[1]);
    copy_file(src, dst);

    //-- Set default run time path ('PATH', 'OASES_SH', etc)
    set_default_runtime_env();

    csnapf_();

    //-- Set file names specific to subroutine
    for (int i = 0; i < N; i++)
    {
        char src[1024];
        snprintf(src, sizeof(src), "%s%s", m[i].key, m[i].value);

        //-- In case a long string
        char dst[1024];
        snprintf(dst, sizeof(dst), "%s%s", argv[iarg[i]], m[i].value);

        //-- Copy with input argument
        copy_file(src, dst);
    }

    return EXIT_FAILURE;
}