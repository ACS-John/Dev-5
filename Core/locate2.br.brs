!  Replace S:\Core\Locate2
autoLibrary
on error goto Ertn
 
dim a$*132,prg$*40,lc$*40,dur$*40,rep$*40,resp$(20)*100,txt$*100
dim brfn$(1000)*255,brsfn$(1000)*255,dur$*200,item$(1000)*255
dim report$*256,subprocfile$*256,procfile$*256,tempfile1$*256,tempfile2$*256
 
fngetcd(dur$)
fnTop(program$,'Locate2')
filter$="*.br, *.br"
cancel=5
report$=env$('temp')&"\LocRpt-"&session$&".txt"
subprocfile$=env$('temp')&"\loc3-"&session$&".tmp"
procfile$=env$('temp')&"\Loc0-"&session$&".prc"
tempfile1$=env$('temp')&"\Loc1-"&session$&".tmp"
tempfile2$=env$('temp')&"\Loc2-"&session$&".tmp"
 
MAIN: !
	fnTos
	lngth=8 : ps=lngth+2 : rc=lc=0
	fnLbl(lc+=1,1,'Find:',lngth,1)
	fnTxt(lc,ps,16,40)
	resp$(rc+=1)=lc$
	fnLbl(lc,ps+18,'and',lngth)
	fnTxt(lc,ps+22,16,40)
	resp$(rc+=1)=lc2$
	fnLbl(lc+=1,1,'Path:',lngth,1)
	fnTxt(lc,ps,38,66,0,'72')
	resp$(rc+=1)=dur$
	lc+=1 ! blank line
	fnLbl(lc+=1,1,'Replace:',lngth,1)
	fnTxt(lc,ps,38)
	resp$(rc+=1)=rep$
	fnLbl(lc+=1,1,"Filter:",lngth,1)
	fnTxt(lc,ps,38)
	resp$(rc+=1)=filter$
	fnChk(lc+=1,ps,'Append Previous Report',0)
	resp$(rc+=1)="FALSE"
	fnChk(lc+=1,ps,'Renumber all Programs',0)
	resp$(rc+=1)="FALSE"
!  fnTxt(LC,PS,18,40)
	!  rESP$(RC+=1)=LC2$
	lc+=1 ! blank line
	fnLbl(lc+=1,1,"Leave Replace blank to locate only" )
	fnLbl(lc+=1,1,"Do NOT try to use Secondary Find if using Replace")
	fnLbl(lc+=1,1,"In Windows XP I can use '*.br,*.br'")
	fnCmdSet(2)
	fnAcs("Locate",0,mat resp$,ck)
	if ck=cancel then goto Xit
	lc$=trim$(resp$(1))
	lc2$=trim$(resp$(2))
	dur$=trim$(resp$(3))
	rep$=trim$(resp$(4))
	filter$=trim$(resp$(5))
	app_prev$=resp$(6)
	if lc2$<>"" and rep$<>"" then goto MAIN
!
	fngetdir(dur$,mat brfn$," /s ",filter$)
	for j=1 to udim(brfn$)
		if trim$(brfn$(j))="" then mat brfn$(j-1) : goto L490
	next j
L490: pr "Found "&str$(j-1)&" files."
	open #2: "Name="&procfile$&",Replace",display,output
	if uprc$(app_prev$)="FALSE" then           execute "free Locate-Report.txt -n" ioerr L520
L520: pr #2: "print border: 'Locating...'"
! pr #2: "ProcErr Return" ! quietly continue on error ! XXX
	for j=1 to udim(brfn$)
		pr #2: "Load "&brfn$(j)
		pr #2: "Load "&brfn$(j)
		pr #2: "Load "&brfn$(j)
		pr #2: "Load "&brfn$(j)
		pr #2: 'List >'&tempfile1$
		pr #2: 'Load '&tempfile1$&',Source'
!  record program name
		pr #2: 'list 1 >'&tempfile2$
		pr #2: 'type '&tempfile2$&' >>'&report$
		if rep$<>"" then : _
			pr #2: "List '"&lc$&"' >>"&report$ : _
			pr #2: "List '"&lc$&"' Replace '"&rep$&"' >>"&report$ : _
			pr #2: "List '"&lc$&"' Replace '"&rep$&"' >"&subprocfile$ : _
			pr #2: "SubProc "&subprocfile$ : _
			pr #2: "Replace "&brfn$(j)
		if rep$="" and lc2$="" then             pr #2: "list '"&lc$&"' >>"&report$
		if rep$="" and lc2$<>"" then            pr #2: "list '"&lc$&"' '"&lc2$&"' >>"&report$
	next j
	pr #2: "Print Border: 'Location Complete'"
	pr #2: "sy -w -C Notepad "&report$
	pr #2: "Load "&prg$
	pr #2: "Load "&prg$
	pr #2: "Load "&prg$
	pr #2: "Load "&prg$
	pr #2: "Run"
	close #2:
execute "Proc "&procfile$
 
Xit: stop  ! fnXit("")
include: Ertn
