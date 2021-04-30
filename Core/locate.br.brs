!  Replace S:\Core\Locate

autoLibrary
on error goto Ertn
 
dim a$*132,prg$*40,lc$*80,dur$*40,rep$*40,resp$(20)*100,txt$*100
dim brfn$(1000)*255,brsfn$(1000)*255,dur$*200,item$(1000)*255
dim report$*256,subprocfile$*256,procfile$*256,tempfile1$*256,tempfile2$*256
dim insline$*78
dim filter$*38
 
dur$=fnAcsInstallationPath$
fnTop(program$,"Locate 1")
filter$="*.br"
report$=env$('temp')&"\LocRpt-"&session$&".txt"
subprocfile$=env$('temp')&"\loc3-"&session$&".tmp"
procfile$=env$('temp')&"\Loc0-"&session$&".prc"
tempfile1$=env$('temp')&"\Loc1-"&session$&".tmp"
tempfile2$=env$('temp')&"\Loc2-"&session$&".tmp"
 
MAIN: !
	fnTos
	lngth=17 : ps=lngth+2 : rc=lc=0
	fnLbl(lc+=1,1,'Find:',lngth,1)
	fnTxt(lc,ps,16,63)
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
	fnLbl(lc,ps+40,"Leave Replace blank to locate only" )
	fnLbl(lc+=1,1,"Filter:",lngth,1)
	fnTxt(lc,ps,38)
	resp$(rc+=1)=filter$
	fnChk(lc+=1,ps,'Append Previous Report',0)
	resp$(rc+=1)="False"
	fnChk(lc+=1,ps,'Renumber all Programs',0)
	resp$(rc+=1)="False"
	lc+=1 ! blank line
	fnLbl(lc+=1,1,"Insert this Line:",lngth,1)
	fnTxt(lc,ps,40,78,0,"",0,"This will be executed after Renumber, if you choose to, a good example is '45 ! this is a dumb comment'" )
	resp$(rc+=1)=""
	lc+=1 ! blank line
	fnLbl(lc+=1,1,"Do NOT try to use Secondary Find if using Replace")
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	lc$=trim$(resp$(1))
	lc2$=trim$(resp$(2))
	dur$=trim$(resp$(3))
	rep$=trim$(resp$(4))
	filter$=trim$(resp$(5))
	app_prev$=resp$(6)
	rnm$=resp$(7)
	insline$=trim$(resp$(8))
	if lc2$<>"" and rep$<>"" then goto MAIN

	fngetdir(dur$,mat brfn$," /s ",filter$)
	for j=1 to udim(brfn$)
		if trim$(brfn$(j))="" then mat brfn$(j-1) : goto L500
	next j
	L500: pr "Found "&str$(j-1)&" files."
	open #2: "Name="&procfile$&",Replace",display,output
	if uprc$(app_prev$)="FALSE" then fnFree(report$)
	L530: pr #2: "print border: 'Locating...'"
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
		if rnm$="True" then              pr #2: "Renum"
		if insline$<>"" then             pr #2: insline$
		if rep$<>"" or insline$<>"" then pr #2: "Replace "&brfn$(j)
		if rep$="" and lc2$="" then      pr #2: "List '"&lc$&"' >>"&report$
		if rep$="" and lc2$<>"" then     pr #2: "List '"&lc$&"' '"&lc2$&"' >>"&report$
	next j
	pr #2: "Print Border: 'Location Complete'"
	pr #2: "sy -w Notepad "&report$
	prg$="S:\Core\Locate"
	pr #2: "Load "&prg$
	pr #2: "Load "&prg$
	pr #2: "Load "&prg$
	pr #2: "Load "&prg$
	pr #2: "Run"
	close #2:
	execute "Proc NoEcho"
execute "Proc "&procfile$
 
Xit: fnXit
include: ertn
