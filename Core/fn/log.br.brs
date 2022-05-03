library program$: fnLog
fnLog('test')
exe 'dir [Q]\[cursys]mstr\ErrorLog.h[cno]'
end


def library fnLog(log$*512;logType,___,delim$,hLog,logFileName$*256,needsHeader)
	! logType = 0 = why do i even bother logging this crap.
	!          = 1 = chained somewhere
	!          = 2 = you had an error.

	autoLibrary
	on error goto Xit

	delim$=chr$(9)
	logFileName$=env$('CurSysData')&'\ErrorLog.h[cno]' 	! previously: '[Temp]\acs\Log.txt'
	if ~exists(logFileName$) then needsHeader=1
	open #hLog=fnH: 'Name='&logFileName$&',RecL=1024,EOL=CRLF,Use',d,o ioerr Xit
	! pr 'hLog=';hLog : pause
	if needsHeader then
		pr #hLog: 'acsUserId/Session'&delim$;
		pr #hLog: 'logType'&delim$;
		pr #hLog: 'Program_Caption'&delim$;
		pr #hLog: 'Core_Program_Current'&delim$;
		pr #hLog: 'Date'&delim$;
		pr #hLog: 'Time'&delim$;
		pr #hLog: 'Comment'
	end if
	pr #hLog: env$('acsUserId')&'/'&session$&delim$;
	pr #hLog: str$(logType)&delim$;
	pr #hLog: env$('Program_Caption')&delim$;
	pr #hLog: env$('Core_Program_Current')&delim$;
	pr #hLog: date$("mm/dd/ccyy")&delim$;
	pr #hLog: time$&delim$;
	pr #hLog: log$
	Finis: !
	close #hLog: ioerr ignore
	Xit: !
fnend
