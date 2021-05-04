#!/bin/sh
set -ue

LC_ALL=C

cd $(dirname "$0")/..

## Static linking configuration ##

# The linked C libraries list may need updating on changes to the dependencies.
#
# To get the correct list for manual linking, the simplest way is to set the
# flags to `-verbose`, while on the normal `autolink` mode, then extract them
# from the gcc command-line.

case $(uname -s) in
    Linux)
        case $(. /etc/os-release && echo $ID) in
            alpine)
                COMMON_LIBS="camlstr base_stubs ssl_threads_stubs ssl crypto cstruct_stubs lwt_unix_stubs bigarray unix c"
                # `m` and `pthreads` are built-in musl
                static_link() {
                    local LIBS="$* $COMMON_LIBS"
                    echo '(-noautolink'
                    echo ' -cclib -Wl,-Bstatic'
                    echo ' -cclib -static-libgcc'
                    for l in $LIBS; do
                        echo " -cclib -l$l"
                    done
                    echo ')'
                }
                ;;
            *)
                echo "Please run in Alpine to avoids glibc constraints"
                exit 2
        esac
        ;;
    Darwin)
        COMMON_LIBS="camlstr base_stubs ssl_threads_stubs /usr/local/opt/openssl/lib/libssl.a /usr/local/opt/openssl/lib/libcrypto.a cstruct_stubs lwt_unix_stubs bigarray unix pthread"
        static_link() {
            local LIBS="$* $COMMON_LIBS"
            echo '(-noautolink'
            for l in $LIBS; do
                if [ "${l%.a}" != "${l}" ]; then echo " -cclib $l"
                else echo " -cclib -l$l"
                fi
            done
            echo ')'
        }
        ;;
    *)
        echo "Static linking not configured for your platform. See $0 to contribute."
        exit 2
esac

static_link >src/main/linking_main.sexp laolao_stubs threads camlrun

static_link >src/main/linking_client.sexp threads camlrun

static_link >src/main/linking_server.sexp laolao_stubs threadsnat

echo "src/main/linking_*.sexp generated; remove them to restore a standard (dynamically linked) build." >&2
