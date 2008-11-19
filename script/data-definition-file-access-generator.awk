##############################################################################
# File: data-definition-file-access-generator.awk
# Auth: Cedric Lallain
# Desc: Script file which takes a ddf as entry and generate a .h file to 
#       access those data
##############################################################################


# Note about the awk script: 
#  The arguments which need to be passed to the function are all variables
#  declared before '_', '_' is the delimiter for local variable declarations.


##############################################################################
#    Description of a DDF file
#-----------------------------------------------------------------------------
# A ddf file is a 'data definition format' file, it describes how are disposed 
# the data within a given buffer. The current script will use those defs to
# export a C/C++ header will access function
#
# The following lines define a rough grammar of a ddf file:
#
#   file       ::= setblock
#   setblock   ::= setline openblock { dataline } closeblock
#   setline    ::= 'set' <name> [ size ] [ attrbs ] <eol>
#   size       ::= '[' <size_in_bytes> ']'
#   attrbs     ::= ':' attrb { ',' attrb }
#   attrb      ::= attrbname '(' <value> ')'
#   attrbname  ::= 'size' | 'first_offset' | 'next_offset' | 'map' | 'align'
#   openblock  ::= '{' <eol>
#   dataline   ::= visibility | setblock | data | padding
#   data       ::= type <name> [ size ] [ attrbs ] <eol>
#   type       ::= 'UPTR' | 'U8'  | 'U16'  | 'U32' | 'U64' | 'F32' | 'VU16' | 
#                  'VU8'  | 'VU4' | 'VS16' | 'VS8' | 'VS4' | 'VF4'
#   padding    ::= '<PAD' <size_in_bits> '>' <eol>
#   visibility ::= 'show:' <eol> | 'hide:' <eol>
#   closeblock ::= '}' <eol>
#
# Line comment are allowed and can be defined anywhere using either
# ';' or '#' or even '--' as prefix
#
# About the size, offsets values: 
#
#    Variables can be used in the size and offset formula:
#
#    There is two kind of variables: 'user' and 'data'
#      A 'user' variable represents a value which will be automatically asked 
#      as a parameter of the function 
#      A 'data' variable represents a value within the data set
#
#    Both variable are differentiate by their prefix: A 'user' data will be
#    prefix by a '@' and a 'data' variable by a '$'
#
#    All variable names are made with alphanumeric character including _ 
#    'Data' variable can also use the "." to imply a specific data set.
#    Note that if a data set is implied the dependencies will also be affected
#
#    If two variables 'user' and 'data' shared the same name they will be 
#    considered representing the same value.
#
#    There is two specific variables name:
#     - '$this' which represents the current address of the data to be defined
#     - '@ndx'  which represents the index asked to the user to return the 
#     'ndx'th item of the array of data to be defined
#
#    An array size can be empty (<=> '[]') as long as those data are the latest
#    a set and the size of the set has been predefined (using the 'size' attrb)
#
# first_offset will still be dependent to his parent, meaning: the parent
#     offset will still be added to the defined value
# next_offset will be an absolute value, in the countrary, if we want the parent
#     address to be added to that value, we need to add it explicitely.
#
# Note about the current version: 
#    Even if it would be nice to be able to defined the different attributs on
#    multiple lines, it is currently not supported
##############################################################################



# ----- Initialization -------------------------------------------------------
BEGIN {

	## ----- CUSTOMIZABLE VALUE -----------------

	warning_as_error = 1 # Consider a warning as an error and will stop the analyze if one occurs

	## ----- DON'T CHANGE THOSE VALUE -----------

	# List of type with their size in bytes: (cf: 0 means unknown)
	types_size["set"]  = 0
	types_size["uptr"] = 4
	types_size["u8"]   = 1
	types_size["u16"]  = 2
	types_size["u32"]  = 4
	types_size["u64"]  = 8
	types_size["f32"]  = 4
	types_size["vu16"] = 16
	types_size["vu8"]  = 16
	types_size["vu4"]  = 16
	types_size["vs16"] = 16
	types_size["vs8"]  = 16
	types_size["vs4"]  = 16
	types_size["vf4"]  = 16

	types_repr["set"]  = "uintptr_t"
	types_repr["uptr"] = "U32"
	types_repr["u8"]   = "U8"
	types_repr["u16"]  = "U16"
	types_repr["u32"]  = "U32"
	types_repr["u64"]  = "U64"
	types_repr["f32"]  = "float"
	types_repr["vu16"] = "vector unsigned char"
	types_repr["vu8"]  = "vector unsigned short"
	types_repr["vu4"]  = "vector unsigned int"
	types_repr["vs16"] = "vector signed char"
	types_repr["vs8"]  = "vector signed short"
	types_repr["vs4"]  = "vector signed int"
	types_repr["vf4"]  = "vector float"

	# Depth Information:
	current_parent       = "" # Current parent -- defined by 'set'
	current_offset       = ""  # User define offset / using a variable
	current_const_offset = 0  # Constant value added together...

	# Data tree:
	#
	# root                                   = "root_node"
	# child_count       ["first_set"]        = N
	# child_node        ["first_set",i]      = "nodename"
	# node_lookup       ["short_node_name"]  = "parents...nodename" -- remember the most rescent one
                                             
	# node_type         ["full_node_name"]   = type value of the node
	# node_size         ["full_node_name"]   = size of the node itself
	# node_array        ["full_node_name"]   = 1 if its an array
	# node_array_size   ["full_node_name"]   = size of the array, "?" if unknown
	# node_first_offset ["full_node_name"]   = first offset value of the node
	# node_ovr_foffset  ["full_node_name"]   = set to true if the first offset has been overwritten
	# node_next_offset  ["full_node_name"]   = <array-only> how to access to the next node
	# node_mapping      ["full_node_name"]   = user type
	# node_access       ["full_node_name"]   = 1 if a function is required to access it
	# node_cond         ["full_node_name"]   = condition of existence of a specific set of data
                                             
	# node_deps_count   ["full_node_name"]   = how many dependencies does this node required to be defined
	# node_deps_list    ["full_node_name",i] = dependency list

	# Warning / Error count:
	warning_count = 0;
	error_count   = 0;

	# State variables:
	expect_open_bracket  = 0;
	expect_close_bracket = 0;
	no_access            = 0;
                         
	root                 = ""
	child_count[root]    = 0
                         
	data_text_count      = 0
	data_text_list[0]    = ""
}



#-----------------------------------------------------------------------------
{

	# First: line cleaning
	sub(/\r$/,"");                 # Dos 2 Unix        
	gsub(/;.*$/,"" ,$0)            # Strip comments ';'
	gsub(/#.*$/,"" ,$0)            # Strip comments '#'
	gsub(/--.*$/,"" ,$0)           # Strip comments '--'
	gsub(/^[ \t]*|[ \t]*$/,"", $0) # Trim spaces
	
	saved_string = $0
	if ( $0 != "" )
	{
		# Two specials cases '{' and '}':
		if ( $1 == "{" )
		{
			if ( expect_open_bracket )
				expect_open_bracket = 0;
			else
				error("Unexpected '{'. '{' are only allowed after a 'set' statement.")
		}
		else if ( $1 == "}" )
		{
			if ( current_parent != "" )
				climb();
			else
				error("Unexpected '}'.");
			expect_close_bracket = 0;
		}
		else
		{
			if ( expect_open_bracket )
				error("Expected '{'. Found: " $0);
			if ( expect_close_bracket )
				error("Expected '}'. Found: " $0);
			else
			{
				# First let's check if the root node is set, if not, 
				# the first word should start defining it!
				if ( root == "" && tolower($1) != "set" )
					warning("Data root level not set: required 'set instruction' -- found: " $1 " -- line ignored")

				analyze_line()
			}
		}
		
		if ( root )
		{
			data_text_list[data_text_count++] = saved_string
		}
	}
	else
	{
		# Add one empty line only if the count has started, and if the previous line wasn't empty
		if ( data_text_count > 0 && data_text_list[data_text_count-1] != "" )
			data_text_list[data_text_count++] = ""
	}

	if ( error_count != 0 || (warning_as_error != 0 &&  warning_count != 0) )
		exit -1
}
#-----------------------------------------------------------------------------

# ----- Report  --------------------------------------------------------------
END {
	if ( error_count != 0 || (warning_as_error != 0 &&  warning_count != 0) )
		exit -1

	export_header_file()
#~	dump_tree()
}



##############################################################################
##
## Analyze Functions:
##
##############################################################################

# Analyze a line of definition: 
#   line ::= <type> <name> [ '[' <value> ']' ] [ : attributs ]
function analyze_line() 
{
	if ( $1 == "hide:" )
	{
		no_access = 1
		return 
	}
	if ( $1 == "show:" )
	{
		no_access = 0
		return
	}

	# Default value for a new node:
	cur_type         = tolower($1)


	# Check for padding:
	if ( cur_type ~ "<pad[0-9]+>" )
	{
		_pad_size =  gensub("<pad([0-9]+)>", "\\1", "g", cur_type)
		_pad_size = int(_pad_size / 8)

		current_const_offset += _pad_size
		return
	}

	cur_size         = types_size[cur_type]
	cur_next_offset  = ""
	cur_mapping      = ""
	cur_cond         = ""

	analyze_attributs()

	cur_first_offset = combine_offset(current_const_offset, current_offset)
	if ( !found_first_offset )
	{
		if ( cur_first_offset != "0" )
			cur_first_offset = get_short_name(current_parent) "+" cur_first_offset
		else
			cur_first_offset = get_short_name(current_parent)
	}

	if ( index(cur_first_offset, "$this") )
	{
		error("$this can't be used in the offset definition!")
		return
	}

	extract_array_count()
	cur_name = $2

	insert_new_node();

	if ( cur_type == "set" )
	{
		dig(cur_name);
	}
	else
	{
		update_current_offset(cur_size, cur_array_size)
	}
}


# Analyze the different attribut of the current line
function analyze_attributs()
{
	found_first_offset = 0
	_attrb_pos         = index($0, ":")
	if ( !_attrb_pos )
		return

	_attributs = substr($0, _attrb_pos+1)
	gsub(/:.*$/,"" ,$0)

	_attcount = split(_attributs, _attrbs, ",")

	for ( _i = 1; _i <= _attcount; _i++ )
	{
		_curatt = _attrbs[_i]

		gsub(/^[ \t]*|[ \t]*$/,"", _curatt)

		if ( substr(_curatt, length(_curatt)) != ")" )
		{
			error("Unknown attribut: " _curatt)
			return
		}
		
		_curatt  = substr(_curatt, 1, length(_curatt)-1)

		_brapos  = index(_curatt, "(")
		_attname = substr(_curatt, 1, _brapos-1)
		_attval  = substr(_curatt, _brapos+1)

		gsub(/^[ \t]*|[ \t]*$/,"", _attname)
		gsub(/^[ \t]*|[ \t]*$/,"", _attval)

		if ( _attname == "size" )
		{
			if ( is_number(_attval) ) 
				cur_size = _attval
			else
				cur_size = evaluate("(" _attval ")")
		}
		else if ( _attname == "map" )
		{
			cur_mapping = _attval
		}
		else if ( _attname == "first_offset" )
		{
			if ( is_number(_attval) )
			{
				current_offset       = ""
				current_const_offset = _attval
			}
			else
			{
				current_offset       = evaluate(_attval)
				current_const_offset = 0
			}
		}
		else if ( _attname == "ovr_first_offset" )
		{
			if ( is_number(_attval) )
			{
				current_offset       = ""
				current_const_offset = _attval
			}
			else
			{
				current_offset       = evaluate(_attval)
				current_const_offset = 0
			}
			found_first_offset = 1
			error("Not yet properly handled... If the next data doesn't overwrite the attribut, the parent addres is going to be added to the previous first offset")
		}
		else if ( _attname == "next_offset" )
		{
			cur_next_offset = evaluate(_attval)
		}
		else if ( _attname == "align" )
		{
			if ( !is_number(_attval) )
			{
				error("Alignment value has to be constant!")
				return
			}
			_alignval = _attval - 1 

			if ( current_offset == "")
				current_const_offset = (int((current_const_offset + _alignval) / _attval)) * _attval
			else
			{
				current_offset = combine_offset(current_const_offset, current_offset)
				current_offset = "((" current_offset "+" _alignval ")&~" _alignval ")"
				current_const_offset = 0
			}
		}
		else if ( _attname == "cond" )
		{
			# Currently not used.
			cur_cond = evaluate(_attval);
		}
		else
		{
			error("Unknown attribut: " _attname)
			return
		}
	}
}

# Extract the array definition string
# Var set: 
#   array_size           = <string containing the array size definition>
#   expect_close_bracket = <set if the array is empty and if the size of the set is defined>
function extract_array_count(_,
							 _pos, _asz, _str, _cnt, _end, _opn, _cls)
{
	_pos = index($0, "[")
	
	if ( _pos == 0 )
	{
		cur_array_size = ""
		return;
	}

	_asz = substr($0, _pos+1)
	_str = _asz
	_cnt = 1
	_end = 0

	# Make sure there is no other [] in the array definition:
	do
	{
		_opn = index(_str, "[");
		_cls = index(_str, "]");
		if ( _opn > 0 && _opn < _cls )
		{
			_cnt++
			_str = substr(_str, _opn+1)
			_end += _opn
		}
		else
		{
			_cnt--
			_str = substr(_str, _cls+1)
			_end += _cls
		}
	} while ( _cnt > 0 && _cls > 0)

	if ( _cnt > 0 || _cls == 0 )
	{
		error("Missing ']' in the array definition")
		return
	}
	if ( !(_str ~ /^[ \t]*$/) )
	{
		error("Unexpected data: " _str)
		return
	}

	_end -= 1 # Remove the ']' from the list of valid data

	cur_array_size = substr(_asz, 1, _end)
	if ( cur_array_size ~ /^[ \t]*$/ ) 
	{
		if ( node_size[current_parent] != 0 )
		{
			expect_close_bracket = 1
			cur_array_size       = "?"
		}
		else
		{
			error("Unknown size for the array in a set with undetermined size.")
			return
		}
	}
	
	# Trim white spaces:
    gsub(/^[ \t]+|[ \t]+$/,"", cur_array_size)

	$0 = substr($0, 1, _pos - 1)
}


##############################################################################
##
## Tree Functions:
##
##############################################################################

# Insert a node in the tree
# Read the global variable: 
#   n_name  = name of the node
#   n_type  = type of the node data
#   array_size = size of the array, "?" = unknown, "" = no size -- not an array
function insert_new_node(_,
						 full_name, child_ndx)
{
	if ( current_parent == "" ) 
	{
		full_name                  = cur_name
		node_deps_count[full_name] = 0
	}
	else
	{
		full_name                  = current_parent "." cur_name
		node_deps_count[full_name] = 0

		if ( child_count[current_parent] == 0 )
			add_dependency(full_name, current_parent)
		else
			add_sub_dependency(full_name, child_node[current_parent, child_count[current_parent]-1])
	}

	child_ndx                            = child_count[current_parent]++
	child_node[current_parent,child_ndx] = full_name
	node_first_offset[full_name]         = check_dependency(full_name, cur_first_offset)
	node_ovr_foffset[full_name]          = found_first_offset

	node_lookup[cur_name]                = full_name

	node_type[full_name]                 = cur_type
	node_array[full_name]                = (cur_array_size != "")
	node_mapping[full_name]              = cur_mapping
	node_size[full_name]                 = check_dependency(full_name, cur_size)
	node_array_size[full_name]           = check_dependency(full_name, cur_array_size)
	node_next_offset[full_name]          = check_dependency(full_name, cur_next_offset)
	node_cond[full_name]                 = check_dependency(full_name, cur_cond)
	node_access[full_name]               = 1 - no_access
}

# Check if a current node has a parent
function has_parent(node)
{
	return index(node, ".") != 0
}

# Retrieve the child of a specific node
function get_short_name(node, _,
						pos)
{
	do
	{
		pos  = index(node, ".")
		node = substr(node, pos+1)
	}
	while ( pos != 0 )

	return node
}

# Retrieve the parent of a specific node
function get_parent_name(node, _,
						 pos, last, left)
{
	pos  = 0
	last = 0
	left = node
	do
	{
		last += pos
		pos   = index(left, ".")
		left  = substr(left, pos+1)
	}
	while ( pos != 0 )

	return substr(node, 1, last-1)
}

# Go one level deeper in the tree
function dig(name)
{
	# Save the previous offsets:
	saved_offset[current_parent]       = current_offset
	saved_const_offset[current_parent] = current_const_offset

	# Set the new parent:
	if ( current_parent != "" )
	{
		current_parent = current_parent "." name
	}
	else
	{
		if ( root != "" )
		{
			error("A root node has already been defined -- current definition: " root )
			return
		}

		current_parent = name
		root           = name
	}

	# Special case: 
	if ( root == "" )
		root = current_parent

	expect_open_bracket  = 1
	no_access            = 0
	current_offset       = ""
	current_const_offset = 0
}


# Climb to the parent level: 
# Change:
#  current_parent
#  current_offset
#  current_const_offset
#  no_access
function climb(_, 
			   size, asize)
{
	if ( node_size[current_parent] == 0 )
	{
		size                      = combine_offset(current_const_offset, current_offset)
		node_size[current_parent] = size
	}
	else
	{
		size = node_size[current_parent]
	}


	# Compute the full size if its an array
	if ( node_array[current_parent] )
		asize = node_array_size[current_parent]
	else 
		asize = ""

	current_parent = get_parent_name(current_parent)

	# Retrieve the previous offset values:
	current_offset       = saved_offset[current_parent]
	current_const_offset = saved_const_offset[current_parent]

	# Update of it:
	update_current_offset(size, asize)

	no_access = 0
}

##############################################################################
##
## Evaluation and Dependency check Functions:
##
##############################################################################


# Update the dependency of the node.
# Update the formula, converting data variable to proper offset
function check_dependency(node, formula, _,
						  wndx, retstr, deps, type, word, repl_value, dot, chk, fname,
						  offset, variable, short)
{
	# First:  the variable needs to have their full named
	# Second: change the variables to offset -- need to add dependencies.

	# First pass:
	wndx = match(formula, "[$@]")
	retstr   = ""

	while ( wndx  )
	{
		retstr = retstr substr(formula, 1, wndx-1)
		formula = substr(formula, wndx)

		# Formula is now starting with a word
		deps = gensub("^([$@][a-zA-Z0-9_.]+).*$", "\\1", "s", formula)
		sub("^[$@]?[a-zA-Z0-9_.]+", "", formula)

		type = substr(deps, 1, 1)
		word = substr(deps, 2)

		## Available cases: 

		#  +------------+----------------------------------------------+
		#  |  Script    | Description                                  |
		#  +------------+----------------------------------------------+
		#  | $varname   | Variable representing a data in the set      |
		#  |            | The parents of this data will be considered  |
		#  |            | as a depende for the current node            |
		#  |            |                                              |
		#  | $n1.n2     | This variable represents the data n2, the    |
		#  |            | parent of n2 has to be n1. Also the parent   |
		#  |            | of n1 will be considered as a dependency of  |
		#  |            | the current node                             |
		#  |            | Note: if n1 is an array, n1.n2 = n1[0].n2    |
		#  |            |                                              |
		#  | $this      | Special variable which represents the        |
		#  |            | current variable                             |
		#  |            |                                              |
		#  | @data      | User data!, those data will be requested in  | 
		#  |            | the args list of the function use to have an |
		#  |            | access to the current node (always U32)      |
		#  +------------+----------------------------------------------+

		# this will most likely be treated as a separate case! due to some dependency issue
		# (should not be used in the first offset value)

		# There is no post-dependency! All dependents data must already exist!

		if ( type == "@" )
		{
			add_dependency(node,deps)

			repl_value = "(" word ")"
		}
		else
		{
			if ( word != "this" )
			{
				dot =  index(word, ".")
				if ( dot > 0 )
				{
					chk = substr(word, 1, dot-1)
				}
				else
				{
					chk = word
				}

				fname  = node_lookup[chk]
				if ( fname == "" )
				{
					error("Unknown dependency: " chk)
					return
				}
				add_sub_dependency(node, fname)
			}
			else
			{
				dot   = 0
				chk   = get_short_name(node)
				fname = node
			}

			# Replace the dependency string by a correct offset / cast
			# Unused value so far: next offset, even for an array, we can only have deps on
			# the first value
			# If the deps valus is an array, the value will stay as uintptr_t, otherwise
			# the value will be casted in his own type
			if ( dot == 0 )
			{
				offset     = node_first_offset[fname]
				#variable   = get_short_name(get_parent_name(fname)) 
				#repl_value = "(" variable "+" offset ")"
				repl_value = "(" offset ")"
			}
			else
			{
				#variable   = get_short_name(get_parent_name(fname)) 
				repl_value = "(" node_first_offset[fname]
				do
				{
					word       = substr(word, dot+1)
					dot        = index(word,".")						
					chk        = substr(word, 1, dot-1)
					if ( chk != "" )
					{
						short      = get_short_name(fname)
						fname      = fname "." chk

						if ( node_ovr_foffset[fname] )
						{
							error("Not yet handled")
							return
						}
						
						offset     = node_first_offset[fname]
						sub("^" short"\\+", "", offset)
						if ( offset != "" )
							repl_value = repl_value "+" offset
					}
				}
				while ( dot > 0 )

				short      = get_short_name(fname)
				fname      = fname "." word
				offset     = node_first_offset[fname]
				sub("^" short"\\+", "", offset)

				if ( node_ovr_foffset[fname] )
				{
					error("Not yet handled")
					return
				}

				if ( offset != "" )
					repl_value = repl_value "+" offset

				repl_value = repl_value ")"

			}

			# Here: fname if the full name of the requested variable
			# The type can then be easily checked:

			# If it's an array we just leave the type a intptr.
			# It's up to the user to know what to do with it
			if ( !node_array[fname] && word != "this" )
			{
				if ( node_mapping[fname] != "" )
				{
					repl_value = "((" node_mapping[fname] "*)(" repl_value "))"
				}
				else if ( node_type[fname] == "UPTR" )
				{
					error("You can't use a EA (pointer to the main memory) here!")
				}
				else
				{
					if ( node_type[fname] != "set" ) # a set without mapping -> stay uintptr_t
					{
						type       = node_type[fname]
						conv       = types_repr[type]
						repl_value = "(*(" conv "*)(" repl_value "))"
					}
				}
			}
		}

		retstr = retstr repl_value
	    wndx   = match(formula, "[$@]")

	}

	retstr = retstr formula
	return retstr;
}


# Add the dependency of the node: 'deps_node' to the node 'node'
function add_sub_dependency(node, deps_node, _,
							i)
{
	for ( i = 0; i < node_deps_count[deps_node]; i++ )
	{
		add_dependency(node, node_deps_list[deps_node, i])
	}
}


# Add a dependency to the node in parameter, making sure it doesn't exist already
function add_dependency(node, deps,	_,
						i, flatname)
{
	if ( substr(deps,1,1) == "@" )
	{
		flatname = node_lookup[substr(deps,2)]
		username = deps
	}
	else
	{
		flatname = deps
		username = "@" get_short_name(deps)
	}

	for ( i = 0; i < node_deps_count[node]; i++ )
	{
		if ( node_deps_list[node, i] == flatname || 
			 node_deps_list[node, i] == username )
		{
			node_deps_list[node,i] = flatname
			return
		}
	}
	node_deps_list[node, node_deps_count[node]++] = deps
}

# Due to the fact that the dependencies are inherited from the previous node and dependendant nodes, 
# some dependencies might not be useful for the new node, therefore this function is meant to check
# if the found dep is actually in use in the current node
function check_if_use(node, deps, _,
					  shortname, result)
{
	shortname = get_short_name(deps);
	if ( !node_array[node] )
	{
		return index(node_first_offset[node], shortname) > 0
	}
	else
	{
		result = index(node_first_offset[node], shortname) > 0
		if ( node_next_offset[node] != "" )
		{
			result = result || index(node_next_offset[node], shortname) > 0
		}
		else
		{
			result = result || index(node_size[node], shortname) > 0
		}
		return result
	}
}


##############################################################################
##
## Math Functions:
##
##############################################################################

# Cf comment inside
function evaluate(formula)
{
	# Place holder in case, I have time to factorize it, to reduce the number of calcul
	# done in real time.
	#   e.g.: 
	#     1+x+2     => 3+x
	#     2*x + 3*x => 5*x
	return formula
}

# Check if the string is a number
function is_number(str)
{
	return str ~ "^[ \\t0-9]+$"
}

# Return a sum of const_offset and offset
function combine_offset(const_offset, offset)
{
	if ( offset == "" )
		return const_offset
	else
	{
		if ( const_offset == 0 )
			return evaluate(offset)
		else
			return evaluate(const_offset "+" offset "")
	}
}


# Update the two offsets variables: current_const_offset and current_offset
# adding to them the product: csize * casize
# Change:
#  current_const_offset,
#  current_offset
function update_current_offset(csize, casize)
{
	if ( casize != "" )
	{
		if ( is_number(casize) && is_number(csize) ) 
		{
			csize = csize * casize
		}
		else
		{
			if ( is_number(casize) )
				csize = casize "*(" csize ")"
			else if ( is_number(csize) )
				csize = csize "*(" casize ")"
			else				
				csize = "(" csize ")*(" casize ")"
		}
	}

	if ( is_number(csize) )
	{
		current_const_offset += csize;
	}
	else
	{
		if ( current_offset == "" )
			current_offset = evaluate(csize)
		else
			current_offset = evaluate(current_offset "+" csize)
	}
}


##############################################################################
##
## Debug Functions:
##
##############################################################################

function export_header_file(_,
							include_guard, 
							data_line, prefix, i,
							max_lgt_rettype, max_lgt_fname, cur_lgt,
							parent, node, count)
{
	print "/*"
	print " * Copyright (c) 2003-2005 Naughty Dog, Inc. "
	print " * A Wholly Owned Subsidiary of Sony Computer Entertainment, Inc."
	print " * Use and distribution without consent strictly prohibited"
	print " */"
	print ""
	print "/* "
	print " * This file is autogenerated. Don't modify it!"
	print " * It contains a set of functions to extract data from a described buffer"
	print " */"
	print ""
	print "/*"
	print " * Note: In oder to differentiate a pointer from the main memory (or EA)"
	print " * and a pointer from the local memory, the main memory pointer will always"
	print " * be returned as U32, whereas the local memory integer representation will"
	print " * be using the type: uintptr_t"
	print " */"
	print ""

	# include guard:
	include_guard = "ICE_" toupper(root) "_DDF"
	print "#ifndef " include_guard
	print "#define " include_guard
	print ""


	# Data format in a comment block:
	print "/*"
	print " * --- Data set -----------------------------------------------------"
	print " *"

	prefix = " *  "	
	for ( i = 0; i < data_text_count; i++ )
	{
		data_line = data_text_list[i]
		if ( data_line == "}" )
			prefix = substr(prefix, 1, length(prefix)-4)

		if ( data_line == "show:" || data_line == "hide:" ) # special cases: 'hide:' and 'show:'
			print substr(prefix,1,length(prefix)-4) data_line
		else
			print prefix data_line
		if ( data_line == "{" )
			prefix = prefix "    "
	}
	# note: here prefix should be the same as before the loop!
	if ( prefix != " *  " )
		warning("Something seems wrong here!")

	print " * ------------------------------------------------------------------"
	print " */"
	print ""

	# namespace:
	print "#ifdef __cplusplus"
	print "namespace Ice"
	print "{"
	print "#endif // __cplusplus"
	print ""
	

	# Compute the max length between all return types
	#         the max length between all function names
	parent          = root
	count           = 0
	max_lgt_rettype = 0
	max_lgt_fname   = 0
	while ( parent )
	{
		node = child_node[parent, count++]
		if ( node_access[node] )
		{
			cur_lgt = length(get_returntype(node))
			if ( cur_lgt > max_lgt_rettype )
				max_lgt_rettype = cur_lgt
			cur_lgt = length(fullname_to_functionname(node))
			if ( cur_lgt > max_lgt_fname )
				max_lgt_fname = cur_lgt
		}

		if ( child_count[node] > 0 )
		{
			stack[parent] = count
			count         = 0
			parent        = node
		}
		else
		{
			while ( count == child_count[parent] )
			{
				parent = get_parent_name(parent)
				count  = stack[parent]
			}
		}
	}



	print "/*"
	print " *                    ==================================="
	print " *                            Function Prototypes"
	print " *                    ==================================="
	print " */"
	print ""


	# Function prototypes:
	parent = root
	count  = 0
	while ( parent )
	{
		node = child_node[parent, count++]
		if ( node_access[node] )
		{
			print fullname_to_prototype(node, max_lgt_rettype, max_lgt_fname) ";"
		}

		if ( child_count[node] > 0 )
		{
			stack[parent] = count
			count         = 0
			parent        = node
		}
		else
		{
			while ( count == child_count[parent] )
			{
				parent = get_parent_name(parent)
				count  = stack[parent]
			}
		}
	}



	print ""
	print "/*"
	print " *                  ==============================="
	print " *                          Function Bodies"
	print " *                  ==============================="
	print " */"
	print ""

	parent = root
	count  = 0
	while ( parent )
	{
		node = child_node[parent, count++]
		if ( node_access[node] )
		{
			dump_function_body(node) 
			print ""
		}

		if ( child_count[node] > 0 )
		{
			stack[parent] = count
			count         = 0
			parent        = node
		}
		else
		{
			while ( count == child_count[parent] )
			{
				parent = get_parent_name(parent)
				count  = stack[parent]
			}
		}
	}


	print ""
	print "#ifdef __cplusplus"
	print "} // namespace Ice"
	print "#endif // __cplusplus"

	# end of the include guard:
	print ""
	print "#endif // " include_guard
	print ""
}


function dump_function_body(fullname)
{
	print fullname_to_prototype(fullname, 0, 0)
	print "{"
	if ( !node_array[fullname] )
	{
		print "    const uintptr_t offset = " node_first_offset[fullname] ";"
	}
	else
	{
		print "    const uintptr_t first_offset = " node_first_offset[fullname] ";"
		if ( node_next_offset[fullname] != "" )
		{
			print "    const uintptr_t next_offset  = " node_next_offset[fullname] ";"
			print "    const uintptr_t offset       = ndx ? next_offset : first_offset;"
		}
		else
		{
			print "    const uintptr_t size         = " node_size[fullname] ";"
			print "    const uintptr_t offset       = first_offset + size * ndx;"
		}
	}

	if ( node_mapping[fullname] != "" )
	{
		print "    return (" node_mapping[fullname] "*)offset;"
	}
	else
	{
		if ( node_type[fullname] == "set" )
			print "    return offset;"
		else
			print "    return *(" types_repr[node_type[fullname]] "*)offset;"
	}

	print "}"
}


function fullname_to_prototype(fullname, rt_lgt, fn_lgt, _,
							   prototype, rt, fn)
{
	rt = fill_with_space(get_returntype(fullname), rt_lgt)
	fn = fill_with_space(fullname_to_functionname(fullname), fn_lgt)

	prototype = "static inline"
	prototype = prototype " " rt
	prototype = prototype " " fn
	prototype = prototype " ("
	prototype = prototype build_arglist(fullname)
	prototype = prototype ")"

	return prototype 
}


function build_arglist(fullname, _,
					   retstr, i, deps, used)
{
	retstr = ""
	for ( i = 0; i < node_deps_count[fullname]; i++ )
	{
		deps = node_deps_list[fullname, i]

		# ndx is a common deps, used to index array
		if ( deps == "@ndx" && node_array[fullname] )
			continue

		if ( substr(deps, 1, 1) == "@" )
		{
			if ( !check_if_use(fullname, substr(deps, 2)) )
				continue

			deps = "const U32 " substr(deps, 2)
		}
		else
		{
			if ( !check_if_use(fullname, deps) )
				continue

			deps = "const uintptr_t " get_short_name(deps)
		}
		retstr = retstr ", " deps
	}
	if ( node_array[fullname] )
		retstr = retstr ", const U32 ndx"
	
	return substr(retstr,3)
}

# Return the proper return type for the function 
function get_returntype(fullname)
{
	if ( node_mapping[fullname] != "" )
	{
		return "const " node_mapping[fullname] "* const" 
	}
	else
	{
		return types_repr[node_type[fullname]]
	}
}


# Convert a full name into function name
function fullname_to_functionname(fullname, _,
								  fname, ndx)
{
	fname = "Get_" 
	ndx   = index(fullname, ".")
	while ( ndx > 0 )
	{
		fname    = fname substr(fullname, 1, ndx-1) "_"
		fullname = substr(fullname, ndx+1)
		ndx      = index(fullname, ".")
	}
	fname = fname fullname
	return fname
}

function fill_with_space(str, minlgt, _,
						 lgt)
{
	lgt = length(str) 
	while ( lgt++ < minlgt )
		str = str " " 
	return str
}


##############################################################################
##
## Debug Functions:
##
##############################################################################


# Display a warning on the screen, increase the warning count
# Change:
#  warning_count
function warning(str)
{
	print "*** Warning(line " NR "): " str
	warning_count++;
}

# Display a warning on the screen, increase the warning count
# Change:
#  error_count
function error(str)
{
	print "*** Error(line " NR "): " str
	error_count++;
}

# Dump tree
function dump_tree(_,
				   parent, node, pref, count, i)
{
	# Tree dump:
	parent  = root
	node = parent

	print( "+---- " get_short_name(node) " - Root",
		   "T:" node_type[node],
		   "S:" node_size[node],
		   "A?:" node_array[node],
		   "A#:" node_array_size[node],
		   "FO:" node_first_offset[node],
		   "NO:" node_next_offset[node],
		   "NM:" node_mapping[node],
		   "NA:" node_access[node])


	pref  = "| "
	count = 0

	while ( parent != "" )
	{
		node = child_node[parent, count++]
		if ( child_count[node] > 0 )
		{
			stack[parent] = count
			count = 0
			print( pref "+---- "  get_short_name(node) ,
				   "(" node_type[node] (node_access[node] ? "" : " - hidden") ")")

			print ( pref "| .  first offset: "node_first_offset[node])
			if ( node_next_offset[node] )
				print ( pref "| .  next offset : "node_next_offset[node])

			print ( pref "| .  size        : "node_size[node])
			if ( node_array[node] )
				print ( pref "| .  array size  : "node_array_size[node])
			if ( node_mapping[node] )
				print ( pref "| .  map to      : "node_mapping[node])
			if ( node_cond[node] )
				print ( pref "| .  cond        : "node_cond[node])

			for ( i = 0; i < node_deps_count[node]; i++)
			{
				print ( pref "| .  deps #"i"     : "node_deps_list[node,i])
			}
		

			pref = pref "| "
			parent = node
		}
		else
		{

			print( pref get_short_name(node),
				   "(" node_type[node] (node_access[node] ? "" : " - hidden") ")")

			print ( pref "   first offset: "node_first_offset[node])
			if ( node_next_offset[node] )
				print ( pref "   next offset : "node_next_offset[node])

			print ( pref "   size        : "node_size[node])
			if ( node_array[node] )
				print ( pref "   array size  : "node_array_size[node])
			if ( node_mapping[node] )
				print ( pref "   map to      : "node_mapping[node])
			if ( node_cond[node] )
				print ( pref "   cond        : "node_cond[node])

			for ( i = 0; i < node_deps_count[node]; i++)
			{
				print ( pref "   deps #"i"     : "node_deps_list[node, i])
			}

			while ( count == child_count[parent] )
			{
				pref = substr(pref, 1, length(pref)-2)
				print pref "+----"
				parent = get_parent_name(parent)
				count  = stack[parent]
			}
		}
	}
}
