! Replace S:\Core\GetDir.br
! reads a directory into an array

def library fngetdir(&dir$,mat filename$; option$,filter$*40)
		autoLibrary
		on error goto Ertn

		! Dir$=Directory to Read
		!              does not require but will accept a \ on the end
		! filename$(x)=file names (includes path if /s option is used)
		! option$: /s or /o-g or what ever you want use "dir /?"
		!          at dos prompt for complete list of options.
		mat filename$=("")
		filter$=trim$(filter$) : if filter$="" then filter$="*.*"
		option$=trim$(option$)
		dir$=trim$(dir$)
		if dir$(len(dir$):len(dir$))<>"\" then dir$=dir$&"\"
		! _____________
		execute 'free '&env$('temp')&'\GetDir"&session$&".tmp -n' ioerr ignore
		dim tmp$*255
		tmp$='Sy -s -M Dir "'&rtrm$(os_filename$(dir$))&'\'&filter$&'" /b '&option$&' >"'&env$('temp')&'\GetDir'&session$&'.tmp"'
		execute tmp$
		open #tf1:=fngethandle: "Name="&env$('temp')&'\'&"GetDir"&session$&".tmp",display,input 
		for x=1 to udim(filename$)
			linput #tf1: tmp$ eof Xit
			filename$(x)=rtrm$(tmp$)
			if filename$(x)=uprc$(filename$(x)) then ! never all caps-anything but
				filename$(x)=lwrc$(filename$(x))
			end if 
		next x
	goto Xit

	Xit: ! 
		close #tf1,free: ioerr ignore
fnend 
include: Ertn
