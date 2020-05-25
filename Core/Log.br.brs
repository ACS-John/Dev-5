def library fnlog(log$*512;log_type)
	! Log_Type = 0 = why do i even bother logging this crap.
	!          = 1 = chained somewhere
	!          = 2 = you had an error.
	!
	autoLibrary
	on error goto Xit
	delim$=chr$(9)
	if ~exists(env$('temp')&'\acs\Log.txt') then
		needsHeader=1
	else
		needsHeader=0
	end if
	open #h_log:=fngethandle: "Name="&env$('temp')&"\acs\Log.txt,RecL=1024,EOL=CRLF,Use",display,output ioerr Xit
	if needsHeader then
		pr #h_log: 'acsUserId/Session'&delim$;
		pr #h_log: 'logType'&delim$;
		pr #h_log: 'Program_Caption'&delim$;
		pr #h_log: 'Core_Program_Current'&delim$;
		pr #h_log: 'Date'&delim$;
		pr #h_log: 'Time'&delim$;
		pr #h_log: 'Comment'
	end if
	pr #h_log: env$('acsUserId')&'/'&session$&delim$;
	pr #h_log: str$(log_type)&delim$;
	pr #h_log: env$('Program_Caption')&delim$;
	pr #h_log: env$('Core_Program_Current')&delim$;
	pr #h_log: date$("mm/dd/ccyy")&delim$;
	pr #h_log: time$&delim$;
	pr #h_log: log$ 
	FINIS: ! 
	close #h_log: ioerr ignore
	Xit: ! 
fnend 
