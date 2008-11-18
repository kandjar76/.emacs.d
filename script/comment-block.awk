BEGIN {
	# Define the comment prefix:
	if ( comment == "" )
		comment = "//~"
}

{

	if ( match($1, "^" comment) )
	{
		sub("" comment, "", $0)
		print $0
	}
	else
		print comment $0
}

