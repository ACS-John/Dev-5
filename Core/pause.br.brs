20000 ! pauses (puts into step mode), but only for developers
20020   def library fnpause(;unused)
20040     if env$("ACSDeveloper")="" then 
20060       goto XIT
20080     else 
20100       execute 'Go XIT Step'
20120     end if 
20140 XIT: fnend 
