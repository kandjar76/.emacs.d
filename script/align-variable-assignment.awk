#
# align-variable-assignment
#

# This script take some code as input and align the variable and the equal sign

BEGIN {
	# To split on the Assignment variable:
	if ( assignment == "" ) 
		assignment  = "="
	if ( tab_size == "" )
		tab_size    = 4

	prefix_to_consider = "+-*/%^&|"


	# Compute the maximum length:
	vmax      = 0
	bmax      = 0
	found_one = 0
}

{
	pos = index($0, assignment);
	if ( pos > 0 )
	{
		if ( index(prefix_to_consider, substr($0, pos-1, 1)) != 0)
		{
			pos--
			found_one = 1
		}

		bef = substr($0, 1,pos-1)
		end = substr($0, pos)
	}
	else
	{
		bef = $0
		end = ""
	}

	count = split(bef, words)
	if ( count == 0 || end == "" )
	{
		beg = $0
		var = ""
		end = ""
	}
	else
	{
		var = words[count]
		
		vp  = index(bef, var);
		beg = substr(bef, 1,vp-1)
	}

	#print $0 " -- b:" beg ", v:" var ", e:" end

	blgt = compute_length(beg)
	vlgt = length(var);
	
	list_beg[NR] = beg
	list_var[NR] = var
	list_end[NR] = end
	list_vlg[NR] = vlgt
	list_blg[NR] = blgt

	if ( var != "" )
	{
		vmax = vmax > vlgt ? vmax : vlgt
		bmax = bmax > blgt ? bmax : blgt
	}
}

END {
	for ( line = 1; line <= NR; line++ )
	{
		new_beg = fill_space(list_beg[line], list_blg[line], bmax)
		new_var = fill_space(list_var[line], list_vlg[line], vmax)
	    new_end = list_end[line] 
		
		if ( found_one )
		{
		   if ( index(prefix_to_consider, substr(new_end, 1, 1)) == 0)
               new_end = " " new_end
		}

		print new_beg new_var " " new_end
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

function fill_space(string, lgt, max)
{
	nb_space = max - lgt
	ret = string;

	for ( ndx = 0; ndx < nb_space ; ndx++ )
	  ret = ret " "

	return ret
}

 
