#!/bin/sh

ELFILES='*.el'

if [ ! -d download ]; then
    echo "Creating of the 'download' directory"
    mkdir download
fi

echo "Downloading elisp files"
for el in $ELFILES; do
    echo " \`- Downloading $el"
    cd download
    wget -q http://www.emacswiki.org/cgi-bin/wiki/download/$el
    if [ ! -f $el ]; then 
	echo " \`- Error: Too many access!! (need to increase sleep time!)"
    fi
    cd ..
    sleep 2
done

echo "Checking downloaded files"
cd download
for el in $ELFILES; do
    if [ -f $el ]; then
	grep "Describe the new page here." $el
	if [ "x$?" = "x0" ]; then
	    echo " \`- Not a lisp file - Deleting $el!"
	    rm $el
	fi
	FILESIZE=$(stat -c%s "$el")
	if [ $FILESIZE -eq 0 ]; then
	    echo " \`- Empry file - Deleting $el!"
	    rm $el
	fi
    fi
done
cd ..

echo "Updating script folder"
cp download/*.el .

echo "Cleaning stem"
rm download/*.el
rmdir download

echo "Done."
