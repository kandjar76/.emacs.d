#
# align-comment
#

# This script take some code as input and align the comment at the end of lines

BEGIN {
	# To split on the comment:
	if ( comment == "" )
		comment = "//"
	if ( tab_size == "" )
		tab_size    = 4

	# Compute the maximum length:
	max = 0
}

{
	pos = index($0, comment);
	if ( pos > 0 )
	{
		beg = substr($0, 1,pos-1)
		end = substr($0, pos)
	}
	else
	{
		beg = $0
		end = ""
	}
	lgt = compute_length(beg)
	
	list_code[NR]    = beg
	list_comment[NR] = end
	list_size[NR]    = lgt

	max = max > lgt ? max : lgt
}

END {
	for ( line = 1; line <= NR; line++ )
	{
		new_code = fill_space(list_code[line], list_size[line])
	    comment  = list_comment[line] 
		print new_code comment
	}
}


function compute_length(string)
{
	size = 0
	for ( ndx = 1; ndx <= length(string); ndx++ )
	{
		if ( substr(string, ndx, 1) == "\t" )
		{
			size = (int(size / tab_size) + 1) * tab_size
		}
		else
		{
			size++;
		}
	}
	return size
}

function fill_space(string, lgt)
{
	nb_space = max - lgt
	ret = string;

	for ( ndx = 0; ndx < nb_space ; ndx++ )
	  ret = ret " "

	return ret
}

 
