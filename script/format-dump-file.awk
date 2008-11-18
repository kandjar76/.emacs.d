# Script to reformat a dump output from the compiler.
# Kind of spu-pretty-printer
#
# Syntax:
#   gawk -f reformat -f spu_instr < code_to_format > formatted_code
# 
# Note: 
#   The comments will be stripped out.
#   Multiline comment using { } are unrecognized.
#   /* */ is also not recognized
#   The code will ignore the ',' therefore the next instruction can be
#   consider has an argument for the previous one

BEGIN {
   spu_instr_count = 0
   spu_instr_list[0] = "<None>"
   spu_instr_pipe[0] = "<None>"
}

{
   # Clean each line: 
   sub(/\r$/,"")                                        # Dos 2 Unix
   gsub(/#.*$/,"")                                      # Strip comments
   $0 = gensub(/[abcdef0-9]+ <([^+>]+)\+(0x[abcdef0-9]+)>/, "\\1_\\2", "g", $0); # Strip ascii code
   $0 = gensub(/[abcdef0-9]+ <([^>]+)>/, "\\1", "g", $0); # Strip ascii code
   $0 = gensub(/<([^>]+)>/, "\\1", "g", $0); # Strip ascii code
   gsub(/^[ \t]+|[ \t]+$/,"", $0) # Trim white spaces
   gsub(/^[abcdef0-9]+:\t[abcdef0-9][abcdef0-9] [abcdef0-9][abcdef0-9] [abcdef0-9][abcdef0-9] [abcdef0-9][abcdef0-9]/, "")
   if ( $0 ~ /SPU_REL/ || $0 ~ /SPU_ADDR/ || $0 ~ /Disassembly of/ || $0 ~ /file format/ )
	 $0 = ""

   #   gsub(/nop[ \t]+\$[0-9]+$/, "nop")

   gsub(/\([^\$][^\)]*\)/, "", $0)      # treat (..) as comment
   gsub(/\..*$/, "", $0)          # for now, let get rid of the prepro..
   gsub(/^[ \t]+|[ \t]+$/,"", $0) # Trim white spaces
   gsub(/,/, " ", $0)

   # Either one or two instructions per lines...
   start =  1
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
		    nb_params += check_optional_argument(instr, start);
            spu_instr_list[spu_instr_count] = format_instruction(instr, start, nb_params)
            spu_instr_pipe[spu_instr_count] = spu_cmd_pipe[instr];
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
            print "Unknown: " $start " ; line: " $0
         }
         start++
      }
   }
}

END {
   max_length = 0
   for ( i = 0; i < spu_instr_count ; i++ )
   {
	 if ( spu_instr_pipe[i] == "even" )
	   cur_length = length(spu_instr_list[i]) - index(spu_instr_list[i], "\t");
	 else
	   cur_length = 16
	 if ( max_length < cur_length )
	   max_length = cur_length
   }
   max_length = int((max_length + 8) / 8 ) * 8

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
         even = fill_instr("\t{nop}", max_length)
         odd = spu_instr_list[ndx]
         print "\t"even odd
      }
      else
      {
         even = fill_instr(spu_instr_list[ndx], max_length)
         odd  = "\t{lnop}"
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
   }
}

#*******************************************************************************

function check_optional_argument(instr, start)
{
   coa_optional_argument = 0;
   if ( spu_cmd_minc[instr] != spu_cmd_maxc[instr] )
   {
	  coa_start = start + spu_cmd_minc[instr]+1
	  coa_end   = start + spu_cmd_maxc[instr]
	  if ( coa_end > NF )
	  {
		 coa_end = NF
	  }
	  for ( coa_i = coa_start; coa_i <= coa_end; coa_i++) 
	  {
		if ( toupper($coa_i) in spu_cmd_pipe )
		  return coa_optional_argument
		else
		  coa_optional_argument++
	  }
   }
   return coa_optional_argument
}

function format_instruction(instr, start, count)
{
   fi_latency = spu_cmd_latency[instr];
   fi_pipe    = spu_cmd_pipe[instr]
   if ( instr == "NOP" || instr == "LNOP" )
      fi_instr = "\t"
   else
      fi_instr   = "{" substr(fi_pipe,1,1) fi_latency "}\t"

   fi_instr   = fi_instr tolower(instr)
   if ( count > 0 )
   {
      for ( fi_i = 0; fi_i < (10 - length(instr)); fi_i++ )
         fi_instr = fi_instr " "

      for ( fi_i = 1; fi_i <= count; fi_i++ )
      {
         fi_instr = fi_instr $(start+fi_i)
         if ( fi_i != count )
            fi_instr = fi_instr ", "
      }
   }

   return fi_instr
}

function fill_instr(instr, max_length)
{
   fi_length = length(instr) - index(instr, "\t")
   fi_toadd  = int((max_length - fi_length + 7)/8);
   fi_instr  = instr;
   #print instr "(" fi_length "," fi_toadd ")"
   
   for ( fi_i = 0; fi_i < fi_toadd ; fi_i++ )
         fi_instr = fi_instr "\t"
         
   return fi_instr
}

