Installation of the Mumps linear solver within COIN.

Mumps is a MUltifrontal Massively Parallel sparse direct Solver.

Please visit the home page:

Home page: http://graal.ens-lyon.fr/MUMPS/

and make sure you read the conditions on distributing MUMPS
in Chapter 11 of the MUMPS Users' Guide:

http://graal.ens-lyon.fr/MUMPS/doc/userguide_4.10.0.pdf

**********************************************************************
Note: It is YOUR RESPONSIBILITY to ensure that you are entitled to
      download and use this third party package.
**********************************************************************

If you are working under Unix/Linux-like system and you have `wget'
installed, you can downloaded the required source code for MUMPS
by executing the included script `get.Mumps' in this directory.

Note, however, that the script might not be looking for the most recent
version of Mumps, and it might fail if it tries to download an outdated
one.  In that case, please check the Mumps home page for the most recent
version number and change the definition of the mumps_ver variable at the
beginning of the get.Mumps script.

If you don't have wget available, follow those instructions:

Download Mumps 4.10:

http://mumps.enseeiht.fr/MUMPS_4.10.0.tar.gz (or try
http://graal.ens-lyon.fr/MUMPS/MUMPS_4.10.0.tar.gz if first URL doesn't work)

and unpack it in this directory:

gunzip MUMPS_4.10.0.tar.gz
tar xf MUMPS_4.10.0.tar

Then, rename the MUMPS_4.10.0 directory just to be MUMPS:

mv MUMPS_4.10.0 MUMPS

Finally, apply a small patch to work around a bug in MUMPS:

patch -p0 < mumps.patch
