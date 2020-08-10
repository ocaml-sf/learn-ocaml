#!/bin/bash

# Author: Ted Ralphs (ted@lehigh.edu)
# Copyright 2016, Ted Ralphs
# Released Under the Eclipse Public License 

#Exit when command fails
set -e

#Set defaults
root_dir=$PWD
declare -i num_actions
num_actions=0
sparse=false
prefix=$PWD
dest_dir=
svn=true
fetch=false
build=false
run_test=false
run_all_tests=false
configure_options=
monolithic=false
threads=1
build_dir=$PWD/build
reconfigure=false
get_third_party=true
quiet=false
MAKE=make

#If this is an already checked out project, which one?
echo "Welcome to the COIN-OR fetch and build utility"
echo 
echo "For help, run script without arguments."
echo 

echo "$@" > .get.dependencies.config

#Parse arguments
for arg in "$@"
do
    case $arg in
        *=*)
            option=`expr "x$arg" : 'x\(.*\)=[^=]*'`
            option_arg=`expr "x$arg" : 'x[^=]*=\(.*\)'`
            case $option in
                --prefix)
                    if [ "x$option_arg" != x ]; then
                        case $option_arg in
                            [\\/$]* | ?:[\\/]* | NONE | '' )
                                prefix=$option_arg
                                ;;
                            *)  
                                echo "Prefix path must be absolute."
                                exit 3
                                ;;
                        esac
                    else
                        echo "No path provided for --prefix"
                        exit 3
                    fi
                    ;;
                --build-dir)
                    if [ "x$option_arg" != x ]; then
                        case $option_arg in
                            [\\/$]* | ?:[\\/]* | NONE | '' )
                                build_dir=$option_arg
                                ;;
                            *)
                                build_dir=`pwd`/$option_arg
                                ;;
                        esac
                    else
                        echo "No path provided for --build-dir"
                        exit 3
                    fi
                    ;;
                --threads)
                    if [ "x$option_arg" != x ]; then
                        threads=$option_arg
                    else
                        echo "No thread number specified for --threads"
                        exit 3
                    fi
                    ;;
                DESTDIR)
                    if [ "x$option_arg" != x ]; then
                        case $option_arg in
                            [\\/$]* | ?:[\\/]* | NONE | '' )
                                dest_dir=$option_arg
                                ;;
                            *)  
                                echo "DESTDIR path must be absolute."
                                exit 3
                                ;;
                        esac
                    else
                        echo "No path provided for DESTDIR"
                        exit 3
                    fi
                    ;;
                *)
                configure_options+="$arg "
                ;;            
            esac
            ;;
        --sparse)
            sparse=true
            ;;
        --svn)
            svn=true
            ;;
        --git)
            svn=false
            ;;
        --debug)
            set -x
            ;;
        --monolithic)
            monolithic=true
            ;;
        --reconfigure)
            reconfigure=true
            ;;
        --test)
            run_test=true
            ;;
        --test-all)
            run_all_tests=true
            ;;
        --no-third-party)
            get_third_party=false
            ;;
        --quiet)
            quiet=true
            ;;
        --*)
            configure_options+="$arg "
            ;;
        fetch)
            num_actions+=1
            fetch=true
            ;;
        build)
            num_actions+=1
            build=true
            ;;
    esac
done

#Help
if [ $num_actions == 0 ]; then
    echo "Usage: get.dependencies.sh <command> --option1 --option2"
    echo
    echo "Commands:"
    echo
    echo "  fetch: Checkout source code for all dependencies"
    echo "    options: --svn (checkout from SVN)"
    echo "             --git (checkout from git)"
    echo "             --no-third-party don't download third party source (getter-scripts)"
    echo
    echo "  build: Checkout source code for all dependencies"
    echo "    options: --prefix=\dir\to\install (where to install, default: $PWD)"
    echo "             --xxx=yyy (will be passed through to configure)"
    echo "             --monlithic do 'old style' monlithic build"
    echo "             --threads=n build in parallel with 'n' threads"
    echo "             --build-dir=\dir\to\build\in do a VPATH build"
    echo "             --test run unit test of main project before install"
    echo "             --test-all run unit tests of all projects before install"
    echo "             --quiet suppress build output to stdout"
    echo "             --reconfigure run configure also for already configured projects"
    echo
    echo "General options:"
    echo "  --debug: Turn on debugging output"
    echo 
fi

if [ -e configure.ac ]; then
    main_proj=`fgrep AC_INIT configure.ac | cut -d '[' -f 2 | cut -d ']' -f 1`
else
    echo "Unable to find root configure script."
    echo "Please run script in root directory of checkout."
    exit 2
fi

#Build list of sources for dependencies
deps=`cat Dependencies | tr '\t' ' ' | tr -s ' '| cut -d ' ' -f 2-`

#Keep track of the subdirectories in which we need to build later.
subdirs=

# This changes the default separator used in for loops to carriage return.
# We need this later.
IFS=$'\n'

#Get sources
if [ $fetch = "true" ]; then
    for url in $deps
    do
        if [ `echo $url | cut -d '/' -f 3` != "projects.coin-or.org" ]; then
            # If this is a URL of something other than a COIN-OR project on SVN,
            # then we assume it's a git project
            git_url=`echo $url | tr '\t' ' ' | tr -s ' '| cut -d ' ' -f 1`
            branch=`echo $url | tr '\t' ' ' | tr -s ' '| cut -d ' ' -f 2`
            dir=`echo $git_url | cut -d '/' -f 5`
            echo "Clone $git_url branch $branch"
            if [ ! -e $dir ]; then
                git clone --branch=$branch $git_url
            else
                cd $dir
                git pull origin $branch
                cd -
            fi
            subdirs+="$dir "
        elif [ $svn = "true" ]; then
            # Here, we are supposed to check out from SVN
            svn_repo=`echo $url | cut -d '/' -f 5`
            if [ $svn_repo = "BuildTools" ]; then
                if [ `echo $url | cut -d '/' -f 6` = 'ThirdParty' ]; then
                    tp_proj=`echo $url | cut -d '/' -f 7`
                    if [ `echo $url | cut -d '/' -f 8` = trunk ]; then
                        version=trunk
                    else
                        version=`echo $url | cut -d '/' -f 9`
                    fi
                    mkdir -p ThirdParty
                    echo "Checkout ThirdParty/$tp_proj"
                    svn co --non-interactive --trust-server-cert $url \
                        ThirdParty/$tp_proj
                    if [ $get_third_party = "true" ] &&
                       [ -e ThirdParty/$tp_proj/get.$tp_proj ]; then
                        cd ThirdParty/$tp_proj
                        ./get.$tp_proj
                        cd -
                        subdirs+="ThirdParty/$tp_proj "
                    fi
                fi
            else
                if [ $svn_repo = "CHiPPS" ]; then
                    proj=`echo $url | cut -d '/' -f 6`
                    if [ `echo $url | cut -d '/' -f 7` = trunk ]; then
                        version=trunk
                    else
                        version=`echo $url | cut -d '/' -f 8`
                    fi
                elif [ $svn_repo = "Data" ]; then
                    proj=`echo $url | cut -d '/' -f 5-6`
                    if [ `echo $url | cut -d '/' -f 7` = trunk ]; then
                        version=trunk
                    else
                        version=`echo $url | cut -d '/' -f 8`
                    fi
                else
                    proj=`echo $url | cut -d '/' -f 5`
                    if [ `echo $url | cut -d '/' -f 6` = trunk ]; then
                        version=trunk
                    else
                        version=`echo $url | cut -d '/' -f 7`
                    fi
                fi
                echo "Checkout $url"
                svn co --non-interactive --trust-server-cert $url $proj
                subdirs+="$proj "
            fi
        else
            # Otherwise, convert SVN URL to a Github one and check out with git
            svn_repo=`echo $url | cut -d '/' -f 5`
            if [ $svn_repo = 'Data' ]; then
                data_proj=`echo $url | cut -d '/' -f 6`
                echo "Checkout Data/$data_proj"
                svn co $url Data/$data_proj
                subdirs+="Data/$data_proj "
            elif [ $svn_repo = 'BuildTools' ]; then
                if [ `echo $url | cut -d '/' -f 6` = "ThirdParty" ]; then
                    tp_proj=`echo $url | cut -d '/' -f 7`
                    proj=ThirdParty-$tp_proj
                    mkdir -p ThirdParty
                    if [ `echo $url | cut -d '/' -f 8` = "trunk" ]; then
                        branch=master
                        version=trunk
                    else
                        branch=`echo $url | cut -d '/' -f 8-9`
                        version=`echo $url | cut -d '/' -f 9`
                    fi
                    echo "Clone $proj branch $branch"
                    if [ ! -e ThirdParty/$tp_proj ]; then
                        git clone --branch=$branch \
                            https://github.com/coin-or-tools/$proj \
                            ThirdParty/$tp_proj
                        if [ $get_third_party = "true" ] && \
                           [ -e ThirdParty/$tp_proj/get.$tp_proj ]; then
                            cd ThirdParty/$tp_proj
                            ./get.$tp_proj
                            cd -
                            subdirs+="ThirdParty/$tp_proj "
                        fi
                    else
                        cd ThirdParty/$tp_proj
                        git pull origin $branch
                        if [ $get_third_party = "true" ] && \
                           [ -e get.$tp_proj ]; then
                            ./get.$tp_proj
                            subdirs+="ThirdParty/$tp_proj "
                        fi
                        cd -
                    fi
                fi
            else
                if [ $svn_repo = "CHiPPS" ]; then
                    git_repo=CHiPPS-`echo $url | cut -d '/' -f 6`
                    proj=`echo $url | cut -d '/' -f 6`
                    if [ `echo $url | cut -d '/' -f 7` = 'trunk' ]; then
                        branch=master
                        version=trunk
                    else
                        branch=`echo $url | cut -d '/' -f 7-8`
                        version=`echo $url | cut -d '/' -f 8`
                    fi
                else
                    git_repo=`echo $url | cut -d '/' -f 5`
                    proj=`echo $url | cut -d '/' -f 5`
                    if [ `echo $url | cut -d '/' -f 6` = 'trunk' ]; then
                        branch=master
                        version=trunk
                    else
                        branch=`echo $url | cut -d '/' -f 6-7`
                        version=`echo $url | cut -d '/' -f 7`
                    fi
                fi
                if [ sparse = "true" ]; then
                    echo "Clone (sparse-checkout) $git_repo branch $branch"
                    mkdir $proj
                    cd $proj
                    git init
                    git remote add origin \
                        https://github.com/coin-or/$git_repo 
                    git config core.sparsecheckout true
                    echo $proj/ >> .git/info/sparse-checkout
                    git fetch
                    git checkout $branch
                    cd ..
                else
                    echo "Clone $git_repo branch $branch"
                    if [ ! -e $proj ]; then
                        git clone --branch=$branch \
                            https://github.com/coin-or/$git_repo $proj
                    else
                        cd $proj
                        git pull origin $branch
                        cd -
                    fi
                fi
                subdirs+="$proj/$proj "
            fi
        fi
    done
    echo $subdirs > .subdirs
fi
unset IFS

#Build (and possibly test) the code
if [ $build = "true" ]; then
    if [ $monolithic = "false" ]; then
        if [ ! -e ".subdirs" ]; then
            echo "No .subdirs file. Please fetch first"
        fi
        for dir in `cat .subdirs`
        do
            if [ build_dir != "./" ]; then
                proj_dir=`echo $dir | cut -d '/' -f 1`
                if [ $proj_dir = "Data" ] || [ $proj_dir = "ThirdParty" ]; then
                    proj_dir=$dir
                fi
                mkdir -p $build_dir/$proj_dir
                cd $build_dir/$proj_dir
            else
                cd $dir
            fi
            if [ ! -e config.status ] || [ $reconfigure = true ]; then
                if [ $quiet = "false" ]; then
                    $root_dir/$dir/configure --disable-dependency-tracking \
                        --prefix=$prefix $configure_options
                else
                    $root_dir/$dir/configure --disable-dependency-tracking \
                      --prefix=$prefix $configure_options > /dev/null
                fi
            fi
            if [ $run_all_tests = "true" ]; then
                if [ $quiet = "true" ]; then
                    $MAKE -j $threads > /dev/null
                fi
                $MAKE -j $threads test
            fi
            if [ "x$dest_dir" != x ]; then
                if [ $quiet = "true" ]; then
                    $MAKE -j $threads DESTDIR="$dest_dir" install > /dev/null
                else
                    $MAKE -j $threads DESTDIR="$dest_dir" install
                fi
            else
                if [ $quiet = "true" ]; then
                    $MAKE -j $threads install > /dev/null
                else
                    $MAKE -j $threads install
                fi
            fi
            cd $root_dir
        done
        if [ -e $main_proj ]; then
            if [ build_dir != "./" ]; then
                mkdir -p $build_dir/$main_proj
                cd $build_dir/$main_proj
            else
                cd $main_proj
            fi
        fi
        if [ ! -e config.status ] || [ $reconfigure = true ]; then
            #First, check whether this is a "rootless" project
            if [ -e $root_dir/$main_proj/configure ]; then
                root_config=$root_dir/$main_proj/configure
            else
                root_config=$root_dir/configure
            fi
            #Now, do the actual configuration
            if [ $quiet = "false" ]; then
                $root_config --disable-dependency-tracking \
                             --prefix=$prefix $configure_options
            else
                $root_config --disable-dependency-tracking \
                             --prefix=$prefix $configure_options > /dev/null
            fi
        fi
        if [ $run_test = "true" ]; then
            if [ $quiet = "true" ]; then
                $MAKE -j $threads > /dev/null
            fi
            $MAKE -j $threads test
        fi
        if [ "x$dest_dir" != x ]; then
            if [ $quiet = "true" ]; then
                $MAKE -j $threads DESTDIR="$dest_dir" install > /dev/null
            else
                $MAKE -j $threads DESTDIR="$dest_dir" install
            fi
        else
            if [ $quiet = "true" ]; then
                $MAKE -j $threads install > /dev/null
            else
                $MAKE -j $threads install
            fi
        fi
        cd $root_dir
    else
        if [ build_dir != "./" ]; then
            mkdir -p $build_dir
            cd $build_dir
        fi
        if [ ! -e config.status ] || [ $reconfigure = true ]; then
            if [ $quiet = "false" ]; then
                $root_dir/configure --disable-dependency-tracking \
                            --prefix=$prefix $configure_options
            else
                $root_dir/configure --disable-dependency-tracking \
                            --prefix=$prefix $configure_options > /dev/null
            fi
        fi
        if [ $quiet = "true" ]; then
            $MAKE -j $threads > /dev/null
        fi
        if [ $run_test = "true" ]; then 
            $MAKE -j $threads test
        fi
        if [ "x$dest_dir" != x ]; then
            if [ $quiet = "true" ]; then
                $MAKE -j $threads DESTDIR="$dest_dir" install > /dev/null
            else
                $MAKE -j $threads DESTDIR="$dest_dir" install
            fi
        else
            if [ $quiet = "true" ]; then
                $MAKE -j $threads install > /dev/null
            else
                $MAKE -j $threads install
            fi
        fi
        cd $root_dir
    fi
fi
