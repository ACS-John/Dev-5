!  Replace S:\Core\Search.br
! calling program should: dim selection$*70
def library fnTmSearch(hFile,form$*128,numeric_format$*20,&selection$,key_length)

	! hFile      	- use the file # of the file in your main program which is to          be searched
	! heading$   	- define as x characters - this is the heading on the search co      columns  - use format simular to: HEADING$=" Account #횼ame컴컴컴컴컴컴컴컴컴컴컴Meter Address컴컴횭alance"
	! fs_info$(3)	- fs_info$(1) must always be the numeric key to the main       file - fs_info$(2) must always be the alpha index (name used for search) -      fs_info must always be numeric and be the 4th thing read - these are contr      olled by the form statement you send to this library
	! form$      	- form statement for reading master record - must be full form      at such as:  form pos 1,c 10,pos 11,c 30,pos 89,c 10,pos 102,n 6  - must        have the key in the first position statement and a numeric in the fourth
	! key_length 	- numeric LENGTH of the index used to access the actual            record in the main prgram
	! you can have one column of numeric information on the search screen - it      must always be the fourth item - fs_info - enter the format that would be        used in a cnvrt$ statement after the open (   such as "pic(zz/zz/zz)"

	autoLibrary
	selection$=''

	ASK_NAS: !
		alpha_key_length=kln(hFile)
		fnwin3b(win:=104,env$('program_caption'),4,45,0,2,5,2)
		bk1=prtall=0
		pr #win,fields "2,2,Cc 40,N": "Enter Search Criteria (blank for all):"
		key_position=22-round((alpha_key_length/2),0)
		na$(1)="3,"&str$(key_position)&",C "&str$(alpha_key_length)&",UT,N"
		dim na1$*30
		input #win,fields mat na$: na1$
		if cmdkey=5 then goto Finis
		! If CMDKEY><1 Then Goto 260
		close #win:
		if len(rtrm$(na1$))=0 and len(rtrm$(na2$))=0 then prtall=1
	goto Screen2Ask ! /r
	Screen2Ask: !
		fnwin3b(win,env$('program_caption'),22,70,0,2,5,2)
		cde=0
		dim fs_sw$(22)*70
		mat fs_sw$(22)
		for j=1 to 20
			if j>1 or skipRestore=1 then
				goto L380
			restore #hFile,search>=na1$(1:len(rtrm$(na1$))),release: nokey ASK_NAS
			L380: !
			dim fs_info$(3)*30
			read #hFile,using form$,release: mat fs_info$,fs_info eof L490
			if prtall=1 or len(rtrm$(na1$))=0 then
				goto L420
			else if fs_info$(1)(1:len(rtrm$(na1$)))>na1$(1:len(rtrm$(na1$))) then 
				goto L490
			end if
			L420: !
			cde=cde+1
			numeric_value$="": numeric_value$=cnvrt$(numeric_format$,fs_info) conv L430
			L430: !
			fs_sw$(j)=(fs_info$(1)(1:key_length)&" "&fs_info$(2)(1:25)&" "&fs_info$(3)(1:12)&" "&numeric_value$)(1:70)
			dim fs_sk$(22)*30
			fs_sk$(j)=fs_info$(1)
			if j>1 then goto L480
			bk1+=1
			dim fs_bk$(99)*30
			if kps(hFile)=1 then fs_bk$(bk1)=fs_info$(1) else fs_bk$(bk1)=fs_info$(2) ! when backup a screen, use the numeric key if no alpha key being used for the search (eg G/L) (assuming numeric key starts in position 1)
		L480: !
		next j
	L490: !
		if cde=0 then goto BackToAskNas
		mat fs_sw$(cde)
		for j=1 to 20
			dim fs_sfl$(22)
			fs_sfl$(j)=str$(j)&",1,C 70,N"
		next j
		mat fkey$=("")
		fkey$(1)="Next" : fkey$(2)="Back" : fkey$(5)="Cancel"
		em$="": es=0
		fnfkey(24,mat fkey$,mat disfk,em$,es)
		rinput #win,select mat fs_sfl$,attr "H": mat fs_sw$
		if cmdkey=5 then goto ASK_NAS
		if cmdkey=1 then goto L590
		if cmdkey=2 then goto BACK
		selection$=fs_sk$(curfld)
		if rtrm$(selection$)><"" then
			alp=1
			selection$=selection$(1:key_length) ! carry selection$ back; must pull key out in main program
			goto Finis
		end if
	L590: !
		skipRestore=1
	goto Screen2Ask

	BackToAskNas: ! r:
		skipRestore=0
	goto ASK_NAS ! /r

	BACK: ! r:
		bk1-=1
		if bk1<1 then goto BackToAskNas
		restore #hFile,key=fs_bk$(bk1)(1:alpha_key_length): nokey BackToAskNas
		bk1-=1
	goto Screen2Ask ! /r

	Finis: !
	close #win:
fnend

