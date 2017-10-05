00010 ! 
00020 ! ______________________________________________________________________
20000 def library fnlog(log$*190;log_type)
20020   ! Log_Type = 0 = why do i even bother logging this crap.
20040   !          = 1 = chained somewhere
20060   !          = 2 = you had an error.
20080   ! ______________________________________________________________________
20100   library 'S:\Core\Library': fngethandle
20120   on error goto XIT
20140   delim$=chr$(9)
20160   if ~exists(env$('Temp')&'\acs\Log.txt') then
20180     needsHeader=1
20200   else
20220     needsHeader=0
20240   end if
20260   open #h_log:=fngethandle: "Name="&env$('Temp')&"\acs\Log.txt,RecL=1024,EOL=CRLF,Use",display,output ioerr XIT
20280   if needsHeader then
20300     pr #h_log: 'acsUserId/Session'&delim$;
20320     pr #h_log: 'logType'&delim$;
20340     pr #h_log: 'Program_Caption'&delim$;
20360     pr #h_log: 'Core_Program_Current'&delim$;
20380     pr #h_log: 'Date'&delim$;
20400     pr #h_log: 'Time'&delim$;
20420     pr #h_log: 'Comment'
20440   end if
20460   pr #h_log: env$('acsUserId')&'/'&session$&delim$;
20480   pr #h_log: str$(log_type)&delim$;
20500   pr #h_log: env$('Program_Caption')&delim$;
20520   pr #h_log: env$('Core_Program_Current')&delim$;
20540   pr #h_log: date$("mm/dd/ccyy")&delim$;
20560   pr #h_log: time$&delim$;
20580   pr #h_log: log$ 
20600   FINIS: ! 
20620   close #h_log: ioerr ignore
20640   XIT: ! 
20660 fnend 
