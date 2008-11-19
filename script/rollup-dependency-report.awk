# Script to reformat the code.
# Kind of spu-pretty-printer
#
# Syntax:
#   gawk -f rollup_dependency_report -f spu_instr < code_to_format > formatted_code
# 
# Note: 
#   The comments will be stripped out.
#   Multiline comment using { } are unrecognized.
#   /* */ is also not recognized
#   The code will ignore the ',' therefore the next instruction can be
#   consider has an argument for the previous one
#   Doesn't implement optional parameter!

BEGIN {

   spu_instr_count = 0
   spu_instr_list[0] = "<None>"
   spu_instr_pipe[0] = "<None>"
   
   # Add "{lnop}" and "{nop}" to the command list
   # Command: LNOP
   spu_cmd_latency["{NOP}"] = 1
   spu_cmd_pipe["{NOP}"] = "even"
   spu_cmd_argc["{NOP}"] = 0

   # Command: NOP
   spu_cmd_latency["{LNOP}"] = 1
   spu_cmd_pipe["{LNOP}"] = "odd"
   spu_cmd_argc["{LNOP}"] = 0
   
   # Command: TMP
   spu_cmd_latency["{TMP}"] = 1
   spu_cmd_pipe["{TMP}"] = "placeholder"
   spu_cmd_argc["{TMP}"] = 0
   
   
   # Build a list of variables...
   dependency_count = 0;
   dependency_list[0] = "";
   known_variables[""] = ""
   
   swap_pipe["even"] = "odd"
   swap_pipe["odd" ] = "even"
   nop_value["even"] = "{nop}"
   nop_value["odd" ] = "{lnop}"
   
}

{
   # Clean each line: 
   sub(/\r$/,"");                 # Dos 2 Unix
   gsub(/^;>/,".global" ,$0)      # ;> will define a global variable ;)
   gsub(/;.*$/,"" ,$0)            # Strip comments
   gsub(/\/\/.*$/,"" ,$0)         # Strip comments
   
   gsub(/\{[nN][oO][Pp]\}/,"{!nop}" ,$0)     # Strip comments (exclude - dep comments)
   gsub(/\{[lL][nN][oO][Pp]\}/,"{!lnop}" ,$0) # Replace dependencies with a TMP arg
   gsub(/\{[^\}~!]*\}/,"" ,$0)     # Strip comments (exclude - dep comments)
   gsub(/\{[^\}!]*\}/,"{TMP}" ,$0) # Replace dependencies with a TMP arg
   gsub(/\{!nop\}/,"{nop}" ,$0)   # Replace dependencies with a TMP arg
   gsub(/\{!lnop\}/,"{lnop}" ,$0) # Replace dependencies with a TMP arg
   gsub(/,/, " ", $0)
   gsub(/\/\*[^*]*\*\//, "", $0)

   if ( $1 == ".global" )
   {
        for ( i = 2 ; i <= NF ; i++ )
            known_variables[$i] = "0"
   }

   gsub(/\..*$/, "", $0)          # for now, let get rid of the prepro..
 
   gsub(/^[ \t]+|[ \t]+$/,"", $0) # Trim white spaces

   # Either one or two instructions per lines...
   start = 1
   while ( start <= NF )
   {
      instr = toupper($start)
      
        if ( instr in spu_cmd_pipe )
        {
            nb_params = spu_cmd_minc[instr]
            op_count  = (NF - start)

            if ( nb_params > op_count )
            {
                print "** ERROR(line: " NR ") ** " instr " : Missing " (nb_params - op_count) " argument(s)"
                start += op_count + 1 ;
            }
            else
            {
                spu_instr_pipe[spu_instr_count] = spu_cmd_pipe[instr];

                var = start + 1
                deps = ""
                for ( i = 0 ; i < nb_params; i++ )
                {
                    type = spu_cmd_argv[instr, i];
                    if ( type == "ro" || type == "rw" )
                    {
                        if ( substr($(var+i), 1, 2) != "s_" ) # Ignore masks...
                        {
                            if ( !($(var+i) in known_variables) )
                            {
                                dependency_list[dependency_count++] = $(var+i)
                                known_variables[$(var+i)] = 1
                                if ( deps == "" )
                                    deps = $(var+i)
                                else
                                    deps = deps ", " $(var+i) 
                            }
                            else
                            {
                                if ( known_variables[$(var+i)] != 0 )
                                {
                                    known_variables[$(var+i)] = known_variables[$(var+i)] + 1;
                                    if ( deps == "" )
                                        deps = $(var+i)
                                    else
                                        deps = deps ", " $(var+i) 
                                }
                            }
                        }
                    }
                    else if ( type == "wo" )
                    {
                        known_variables[$(var+i)] = 0
                    }
                }

                spu_instr_list[spu_instr_count] = format_instruction(instr, deps)
                spu_instr_count++

                start += nb_params + 1
            }
        }
        else
        {
            if ( substr($start, length($start)) == ":" )
            {
                spu_instr_list[spu_instr_count] = $start
                spu_instr_pipe[spu_instr_count] = "label"
                spu_instr_count++
            }
            else
            {
                print "Unknown: " $start
            }
            start++
        }
    }
}

END {
   
    # replace {tmp} by the appropriate nop/lnop:
    next_pipe = "even"

    for ( i = 0; i < spu_instr_count; i++ )
    {
        if ( spu_instr_pipe[i] == "placeholder" )
        {
            spu_instr_pipe[i] = next_pipe;
            spu_instr_list[i] = "\t" nop_value[next_pipe];
            next_pipe = swap_pipe[next_pipe];
        }
        else
        {
            next_pipe = swap_pipe[spu_instr_pipe[i]]
        }
    }
   
    max_length = 0
    for ( i = 0; i < spu_instr_count ; i++ )
    {
        cur_length = length(spu_instr_list[i]) - index(spu_instr_list[i], "\t");

        if ( max_length < cur_length )
            max_length = cur_length
    }
    max_length = int((max_length + 8) / 8 ) * 8

   
   # List the dependencies:
   print ";;;;;;;;; Dependencies: " dependency_count
   for ( i = 0; i < dependency_count; i++ )
   {
      print ";; - " dependency_list[i] " {" known_variables[dependency_list[i]] "}"
   }
   print ""

    line = 0
   for ( ndx = 0; ndx < spu_instr_count; ndx++ )
   {
      if ( spu_instr_pipe[ndx] == "label" )
      {
         if ( ndx > 1 && (ndx+1) < spu_instr_count && spu_instr_pipe[ndx-1] == "even" && spu_instr_pipe[ndx+1] == "odd" )
         {
            even = fill_instr(spu_instr_list[ndx], max_length)
            odd  = spu_instr_list[++ndx]
         }
         else
         {
            even = spu_instr_list[ndx]
            odd  = ""
         }
         print even "\t\t" odd
      }
      else if ( spu_instr_pipe[ndx] == "odd" )
      {
         even = fill_instr("\tnop", max_length)
         odd = spu_instr_list[ndx]
         print "\t"even odd
      }
      else
      {
         even = fill_instr(spu_instr_list[ndx], max_length)
         odd  = "\tlnop"
         if ( (ndx+1) < spu_instr_count )
         {
            if ( spu_instr_pipe[ndx+1] == "odd" )
               odd  = spu_instr_list[++ndx]
            else if ( spu_instr_pipe[ndx+1] == "label" )
            {
               if ( (ndx+1) < spu_instr_count && spu_instr_pipe[ndx+2] == "odd"  )
                  odd = ""
            }
         }
         print "\t"even odd
      }
      
      line++
      if ( line % 5 == 0 )
        print ""
   }
}

#*******************************************************************************


function format_instruction(instr, deps)
{
    fi_pipe    = spu_cmd_pipe[instr]
    if ( instr == "NOP" || instr == "LNOP" || substr(instr, 1, 1) == "{" )
        fi_instr = "\t" tolower(instr)
    else 
    {
        if ( deps == "" )
        {
            fi_instr = "\t" nop_value[fi_pipe]
        }
        else
        {
            fi_instr   = "\t" nop_value[fi_pipe]" /* " deps " */"
        }
    }

    return fi_instr
}

function fill_instr(instr, max_length)
{
   fi_length = length(instr) - index(instr, "\t")
   fi_toadd  = int((max_length - fi_length + 7)/8);
   fi_instr  = instr;
   
   for ( fi_i = 0; fi_i < fi_toadd ; fi_i++ )
         fi_instr = fi_instr "\t"
         
   return fi_instr
}
