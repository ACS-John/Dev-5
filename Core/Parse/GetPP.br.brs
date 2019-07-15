def library fnGetPp(&input$,&path$,&prog$,&ext$)
	on error goto ERTN
	! ______________________________________________________________________
	! Dim Note: Please Dim you Path$ and Prog$ as long as your Input$
	! Input$: this is what you want parsed...
	!         supported formats:  progam/dir 
	!                             program.ext/dir 
	!                             dir\program
	!                             dir\program.ext
	!                             dir\dir\program.ext
	! path$:  the path to the input program (i.e. "S:\acsUB\")
	! prog$:  the file name (without it's extension) (i.e. "ubmenu")
	! ext$:   the input progams extension (period included) (i.e. ".wb")
	! The -1 in pos(x,y,-1) causes search to run backwards from end to front
	! ______________________________________________________________________
	input$=trim$(input$) : path$=prog$=ext$=""
	fslash_pos=pos(input$,"/",1) : bslash_pos=pos(input$,"\",-1)
	if fslash_pos>0 then gosub FSLASH_PARSE
	if bslash_pos>0 then gosub BSLASH_PARSE
	if fslash_pos<=0 and bslash_pos<=0 then gosub NOSLASH_PARSE
	dot_pos=pos(prog$,".",-1)
	if dot_pos>0 then gosub RIP_EXT
	goto XIT
	! 
	FSLASH_PARSE: ! r: front slash parse 
		prog$=input$(1:fslash_pos-1) 
		path$=input$(fslash_pos+1:len(input$))
	return ! /r
	BSLASH_PARSE: ! r: Back slash parse 
		prog$=input$(bslash_pos+1:len(input$)) 
		path$=input$(1:bslash_pos)
	return ! /r
	NOSLASH_PARSE: ! r: No slash parse 
		prog$=input$(1:len(input$)) 
		path$=""
	return ! /r
	RIP_EXT: ! r: Extract Ext$ from Prog$
		ext$=prog$(dot_pos:len(prog$))
		prog$=prog$(1:dot_pos-1)
	return ! /r
	XIT: ! 
	path$=trim$(path$)
	if path$(len(path$):len(path$))<>"\" then path$=trim$(path$)&"\"
fnend 
include: ertn
