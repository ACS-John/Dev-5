! Replace S:\Core\GetDir.br
! reads a directory into an array

def library fnGetDir(&dir$,mat filename$; option$,filter$*40,___,returnN,x,h,tmp$*255)
		autoLibrary
		on error goto Ertn
		! Dir$=Directory to Read
		!              does not require but will accept a \ on the end
		! filename$(x)=file names (includes path if /s option is used)
		! option$: /s or /o-g or what ever you want use "dir /?"
		!          at dos prompt for complete list of options.
		mat filename$=('')
		filter$=trim$(filter$) : if filter$='' then filter$='*.*'
		option$=trim$(option$)
		dir$=trim$(dir$)
		if dir$(len(dir$):len(dir$))<>'\' then dir$&='\'

		fnFree('[temp]\GetDir[Session].tmp')
		tmp$='Sy -s -M Dir "'&rtrm$(os_filename$(dir$))&'\'&filter$&'" /b '&option$&' >"[temp]\GetDir[session].tmp"'
		execute tmp$
		open #h=fnH: 'Name=[temp]\GetDir[Session].tmp',display,input
		for x=1 to udim(mat filename$)
			linput #h: tmp$ eof Xit
			filename$(x)=rtrm$(tmp$)
			if filename$(x)=uprc$(filename$(x)) then ! never all caps-anything but
				filename$(x)=lwrc$(filename$(x))
			end if
		next x
		returnN=udim(mat filename$)
	goto Xit

	Xit: !
		close #h,free: ioerr ignore
		fngetdir=returnN
fnend
include: ertn
