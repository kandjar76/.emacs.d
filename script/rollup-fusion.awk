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
#
# To define a block of code with his #:
# ;loop #

BEGIN {

    #spu_instr_count[0] = 0           # spu_instr_count[loop#]
    #spu_instr_list[0, 0] = "<None>"  # spu_instr_list[loop#, instr_ndx]
    #spu_instr_pipe[0, 0] = "<None>"  # spu_instr_pipe[loop#, instr_ndx]
   
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

    swap_pipe["even"] = "odd"
    swap_pipe["odd" ] = "even"
    nop_value["even"] = "{nop}"
    nop_value["odd" ] = "{lnop}"

    loop_ndx = 0
    error_count = 0
}

{
    # Clean each line: 
    sub(/\r$/,"");                 # Dos 2 Unix
    gsub(/^;loop/,".loop" ,$0)      # ;global will define a global variable ;)
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

    if ( $1 == ".loop" )
    {
        loop_ndx = int($2)
        spu_instr_count[loop_ndx] = 0
    }

    gsub(/\..*$/, "", $0)          # for now, let get rid of the prepro..
 
    gsub(/^[ \t]+|[ \t]+$/,"", $0) # Trim white spaces

    if ( loop_ndx > 0 )
    {
        # Either one or two instructions per lines...
        start = 1
        while ( start <= NF )
        {
            instr = toupper($start)

            if ( instr in spu_cmd_pipe )
            {
                nb_params = spu_cmd_argc[instr]
                op_count  = (NF - start)
                if ( nb_params > op_count )
                {
                    print "** ERROR(line: " NR ") ** " instr " : Missing " (nb_params - op_count) " argument(s)"
                    start += op_count + 1 ;
                    error_count++
                }
                else
                {
                    spu_instr_list[loop_ndx, spu_instr_count[loop_ndx]] = format_instruction(instr, start)
                    spu_instr_pipe[loop_ndx, spu_instr_count[loop_ndx]] = spu_cmd_pipe[instr];
                    spu_instr_count[loop_ndx]++
                    start += nb_params + 1
                }
            }
            else
            {
                if ( substr($start, length($start)) == ":" )
                {
                    # The label value is currently ignored to ease the process :)
                
                    #spu_instr_list[loop_ndx, spu_instr_count] = $start
                    #spu_instr_pipe[loop_ndx, spu_instr_count] = "label"
                    #spu_instr_count[loop_ndx]++
                
                }
                else
                {
                    print "Unknown: " $start
                }
                start++
            }
        }
    }
    
    #
    # TO DEBUG AN INCORRECT LOOP COUNT:
    #
    #if ( loop_ndx == 2 )
    #{
    #    print $0 "\t@@ " spu_instr_count[2]
    #}
}

END {
   
    # replace {tmp} by the appropriate nop/lnop:
    for ( loop in spu_instr_count )
    {
        next_pipe = "even"
        for ( i = 0; i < spu_instr_count[loop]; i++ )
        {
            if ( spu_instr_pipe[loop,i] == "placeholder" )
            {
                spu_instr_pipe[loop,i] = next_pipe;
                spu_instr_list[loop,i] = nop_value[next_pipe];
                next_pipe = swap_pipe[next_pipe];
            }
            else
            {
                next_pipe = swap_pipe[spu_instr_pipe[loop,i]]
            }
        }
    }
    
    # Compute the max length of a instruction:
    max_length = 0
    for ( loop in spu_instr_count )
    {
        for ( i = 0; i < spu_instr_count[loop] ; i++ )
        {
            cur_length = length(spu_instr_list[loop,i]) - index(spu_instr_list[loop,i], "\t");

            if ( max_length < cur_length )
                max_length = cur_length
        }
        max_length = int((max_length + 8) / 8 ) * 8
    }
    
   
    # Make sure all loop instructions count are the same:
    instruction_count = -1
    for ( loop in spu_instr_count )
    {
        if ( instruction_count == -1 )
            instruction_count = spu_instr_count[loop]
        else if ( instruction_count != spu_instr_count[loop] )
            instruction_count = -2
    }
    if ( instruction_count == -2 )
    {
        print "** ERROR ** The instruction count is not the same accross the different loops:"
        for ( loop in spu_instr_count )
            print "Loop #" loop ": Instruction count = " spu_instr_count[loop]
            
        error_count++
    }
    
    # Make sure all pipe values are the sames:
    for ( i = 0; error_count == 0 && i < instruction_count; i++ )
    {
        p = ""
        for ( loop in spu_instr_count )
        {
            if ( p == "" ) 
                p = spu_instr_pipe[loop, i]
            else if ( p != spu_instr_pipe[loop, i] )
            {
                print "** ERROR ** The instruction's pipe doesn't match accross the loop"
                error_count++
            }
        }
    }

    # Dump the loop:
    line = 0
    for ( ndx = 0; error_count == 0 && ndx < instruction_count; ndx++ )
    {
        if ( get_pipe(ndx) == "odd" )
        {
            # Should not be... but in case it happens:
            even = fill_instr("\tnop", max_length)
            odd = get_instr(ndx)
            print "\t"even odd
        }
        else
        {
            even = fill_instr(get_instr(ndx), max_length)
            odd  = "\tlnop"
            if ( (ndx+1) < instruction_count )
            {
                if ( get_pipe(ndx+1) == "odd" )
                    odd  = get_instr(++ndx)
            }
            print "\t"even odd
        }

        line++
        if ( line % 5 == 0 )
            print ""
   }
}

#*******************************************************************************


function format_instruction(instr, start)
{
   #fi_latency = spu_cmd_latency[instr];
   #fi_pipe    = spu_cmd_pipe[instr]
   #if ( instr == "NOP" || instr == "LNOP" )
   #   fi_instr = "\t"
   #else
   #   fi_instr   = "{" substr(fi_pipe,1,1) fi_latency "}\t"
   fi_instr = ""
   fi_instr   = fi_instr tolower(instr)
   if ( spu_cmd_argc[instr] > 0 )
   {
      for ( fi_i = 0; fi_i < (10 - length(instr)); fi_i++ )
         fi_instr = fi_instr " "

      for ( fi_i = 1; fi_i <= spu_cmd_argc[instr]; fi_i++ )
      {
         fi_instr = fi_instr $(start+fi_i)
         if ( fi_i != spu_cmd_argc[instr] )
            fi_instr = fi_instr ", "
      }
   }
   #print fi_instr
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

function get_instr(ndx)
{
    gi_instruction = ""
    gi_loop_ndx = 0
    for ( gi_loop in spu_instr_count )
    {
        gi_candidate = spu_instr_list[gi_loop, ndx];
        if ( gi_instruction == "" )
        {
            gi_instruction = gi_candidate
            gi_loop_ndx    = gi_loop
        }
        else if ( substr(gi_instruction, 1, 1) == "{" && substr(gi_candidate, 1, 1) != "{" )
        {
            gi_instruction = gi_candidate
            gi_loop_ndx    = gi_loop
        }
        else if ( (tolower(gi_instruction) == "nop" || 
                   tolower(gi_instruction) == "lnop") &&
                   substr(gi_candidate, 1, 1) != "{" )
        {
            gi_instruction = gi_candidate
            gi_loop_ndx    = gi_loop
        }
        else if ( tolower(gi_candidate) != "nop" && tolower(gi_candidate) != "lnop" && substr(gi_candidate, 1, 1) != "{" )
        {
            print "** ERROR ** Conflict of instructions... (ndx = " ndx ")"
            print "** Current:   " gi_instruction
            print "** Candidate: " gi_candidate
            error_count++
        }
    }

    if ( (substr(gi_instruction, 1, 1) == "{" && 
          tolower(gi_instruction) != "{nop}" && 
          tolower(gi_instruction) != "{lnop}" ) ||
          gi_instruction == ""  )
    {
        print "** ERROR ** Missing instructions... (ndx = " ndx ")"
        error_count++
    }
    else 
    {
        if ( gi_instruction == "nop" || gi_instruction == "lnop" || gi_instruction == "{nop}" || gi_instruction == "{lnop}" )
        {
            return "\t" + gi_instruction
        }
        else
        {
            split(gi_instruction, gi_instr_array)
            gi_instr_name = toupper(gi_instr_array[1])
            gi_latency    = spu_cmd_latency[gi_instr_name]
            gi_pipe       = spu_cmd_pipe[gi_instr_name]
            
            gi_prefix     = "{" substr(gi_pipe,1,1) gi_latency " " gi_loop_ndx"}\t"
            return gi_prefix gi_instruction
        }
    }
    return gi_instruction
}

function get_pipe(ndx)
{
    for ( gi_loop in spu_instr_count )
    {
        return spu_instr_pipe[gi_loop,ndx];
    }
}
