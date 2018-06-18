#!/bin/sh

if ! which sfnt2woff ; then echo missing tool sfnt2woff ; exit 1 ; fi

rm -rf TMP ; mkdir TMP ; cd TMP

home='http://www.exljbris.com/fontin.html'
for url in `wget $home -O - | tr "\"'" "\n\n" | grep .zip` ; do wget $url ; done
for zip in *.zip ; do unzip $zip ; done

for i in Bold Italic Regular SmallCaps ; do
    sfnt2woff Fontin-$i.otf
    cp Fontin-$i.ttf Fontin-$i.woff ..
done

cd .. ; rm -rf TMP
