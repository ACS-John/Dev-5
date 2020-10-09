! Replace S:\acsUB\ubcass_convert
 
	autoLibrary
	on error goto Ertn
 
	dim a$*111
 
	close #24: ioerr L90
L90: close #22: ioerr L100
L100: open #24: "Name=X,RecL=112,EOL=NONE,Replace",external,output
	open #22: "Name=a:ubcass1.dat,RecL=111",external,input
L120: read #22,using "Form pos 1,C 111": a$ eof L160
	write #24,using "Form POS 1,C 111,C 1": a$,chr$(10)
	goto L120
 
L160: close #24:
	close #22:
	execute "COPY x a:ubcass2.dat -n"
 
Xit: stop
 
include: ertn
 
