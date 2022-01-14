! Replace S:\acsGL\ratio
! Ratio File  (was: form pos 1,G 3,C 40,280*PD 4',Key=AC$: HAC$,NA$,MAT R  Now:  form pos 1,G 3,C 40,80*c 12',Key=AC$: HAC$,NA$,MAT gl$

	autoLibrary
	on error goto Ertn

	dim gln(80,3),k4$*2,message$*40
	dim gl$(80)*12,dat$*20,hac$*3,na$*40
	dim e$(2)*12,option$(6)*60,item$(7)*80
	dim heading$*70,form$*80,numeric_format$*20,selection$*70,resp$(90)*50

	fnTop(program$)
	fndat(dat$)
	ratiomst=10
	if exists("[Q]\GLmstr\ratiomst.h[cno]")=0 then gosub CREATE_FILES
	if exists("[Q]\GLmstr\ratioidx.h[cno]")=0 then gosub INDEX
	if exists("[Q]\GLmstr\schindx2.h[cno]")=0 then gosub INDEX
L180: open #ratiomst: "Name=[Q]\GLmstr\RatioMST.h[cno],KFName=[Q]\GLmstr\RatioIDX.h[cno],Shr",internal,outIn,keyed ioerr L1380
	goto RATIOMSTGRID
	close #ratiomst: ioerr L210
L210: execute "Index [Q]\GLmstr\RatioMST.h[cno]"&' '&"[Q]\GLmstr\SchIndX2.h[cno] 3 30 Replace DupKeys -n"
	goto L180
RATIOMSTGRID: !
	fnTos
	respc=0
	mat chdr$(3) : mat cmask$(3) : mat flxitm$(3)
	chdr$(1)="Rec"
	chdr$(2)="Ratio #" : chdr$(3)="Ratio Name"
	cmask$(1)='30' : cmask$(2)='': cmask$(3)=''
	frame=0
	restore #ratiomst:
	fnflexinit1('Ratiomst1',lc=1,1,10,50,mat chdr$,mat cmask$,1)
READ_RATIOMST: ! read Ratiomst file
	read #ratiomst,using 'form pos 1,G 3,C 40,80*c 12': hac$,na$,mat gl$ eof EO_RATIOMST_GRID
	item$(1)=str$(rec(ratiomst))
	item$(2)=hac$: item$(3)=na$
	fnflexadd1(mat item$)
	goto READ_RATIOMST
EO_RATIOMST_GRID: !
	fnLbl(11,1,"")
	fnCmdKey("&Add",1,0,0,"Allows you to add new Ratios.")

	fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing Ratio.")
	fnCmdKey("&Review G/L #",4,0,0,"Click to review the general ledger numbers used in this ratio.")
	fnCmdKey("&Delete",8,0,0,"Highlight any record and click Delete to remove the Ratio.")
! fnCmdKey("&Print",3,0,0,"Takes you directly to the pr Ratios option")
	fnCmdKey("E&xit",5,0,1,"Exits to main menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	add=edit=0
	editrec=val(resp$(1))
	if ckey=2 then edit=1
	if ckey=4 then goto GL_NUMBERS
! If CKEY=3 Then Chain "S:\acsGL\acglschp" ! prints prints a Ratiomst
	if ckey=1 then
		add=1
		hac$=na$=""
		mat gl$=("")
		goto ADD_EDIT_RATIOMST ! add
	end if
! to ADD_EDIT_Ratiomst ! add
	if ckey=2 then
		read #ratiomst,using 'form pos 1,G 3,C 40,80*c 12',rec=editrec: hac$,na$,mat gl$ noRec RATIOMSTGRID
		holdsn=sn
		goto ADD_EDIT_RATIOMST
	else if ckey=8 then
		read #ratiomst,using 'form pos 1,G 3,C 40,80*c 12',rec=editrec,release: hac$,na$,mat gl$ noRec RATIOMSTGRID
		delete #ratiomst,rec=editrec:
		goto RATIOMSTGRID
	end if
	pause

ADD_EDIT_RATIOMST: !
	fnTos
	mylen=20: mypos=mylen+3 : right=1
	fnLbl(1,1,"Ratio Number:",mylen,right)
	fncombof('glRatiomst',1,mypos,0,"[Q]\GLmstr\ratiomst.h[cno]",1,3,4,40,"[Q]\GLmstr\ratioidx.h[cno]",add_all)
	if edit=1 then resp$(1)=hac$
	if add=1 then resp$(1)=""
	fnLbl(2,1,"Ratio Nane::",mylen,right)
	fnTxt(2,mypos,40,0,left,"",0,"",0 )
	resp$(2)=na$
	fnCmdKey("&Next",1,1,0,"Save the ratio.")
	fnCmdKey("&Cancel",5,0,1,"Returns to list of Ratios withouit saving any changes.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto RATIOMSTGRID
	hac$=resp$(1)(1:3) conv ADD_EDIT_RATIOMST
	na$=resp$(2)
	if edit=1 then goto REWRITE_EXISTING_RATIOMST
	if add=1 then goto WRITE_NEW_RATIOMST
	pause

REWRITE_EXISTING_RATIOMST: !
	if hac$="" or trim$(hac$)="0" then goto ADD_EDIT_RATIOMST
	if holdhac$<>hac$ and holdhac$<>"" then goto MSGBOX1 else goto L780
MSGBOX1: !
	mat ml$(3)
	ml$(1)="You are changing Ratio # "&holdhac$&" to "
	ml$(2)="Ratio # "&hac$&".  Click OK to continue, "
	ml$(3)="else Cancel to prevent changing the #."
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$="OK" then goto L780 else goto ADD_EDIT_RATIOMST
L780: rewrite #ratiomst,using 'form pos 1,G 3,C 40,80*c 12',rec=editrec: hac$,na$,mat gl$
	goto L830

WRITE_NEW_RATIOMST: write #ratiomst,using 'form pos 1,G 3,C 40,80*c 12',rec=editrec: hac$,na$,mat gl$
	new1=1
L830: goto RATIOMSTGRID

	close #ratiomst:
	if new1=1 then gosub L940
	goto Xit

CREATE_FILES: !
	close #ratiomst: ioerr L910
L910: open #ratiomst: "Name=[Q]\GLmstr\RatioMST.h[cno],KFName=[Q]\GLmstr\RatioIDX.h[cno]",internal,outIn,keyed ioerr L930
	close #ratiomst,free: ioerr L930
L930: open #ratiomst: "Name=[Q]\GLmstr\ratiomst.h[cno],KFName=[Q]\GLmstr\ratioidx.h[cno],RecL=1163,KPs=1,KLn=3,replace",internal,outIn,keyed
L940: close #ratiomst: ioerr L950
L950: close #11: ioerr L970
INDEX: ! (main Ratio files)
L970: execute "Index [Q]\GLmstr\RatioMST.h[cno]"&' '&"[Q]\GLmstr\RatioIDX.h[cno] 1 3 Replace DupKeys -n"
return

PROOF: restore #ratiomst,key>="   ": eof L1010 ioerr RATIOMSTGRID
L1010: on fkey 5 goto L1330
	fnopenprn
L1030: read #ratiomst,using 'form pos 1,G 3,C 40,80*c 12',key=ac$: hac$,na$,mat gl$ eof L1330
	pr #255,using L1050: date$('mm/dd/yy'),time$,"Print Ratio File Proof List"
L1050: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos 51,c 31,skip 1
	pr #255,using L1070: env$('cnam'),dat$
L1070: form pos 1,cc 122,skip 1,pos 1,cc 122,skip 2
	pr #255,using L1090: "ratiomst Number",sn
L1090: form pos 1,c 15,pos 20,pic(zz),skip 1
	pr #255,using L1110: "ratiomst Name  ",schnam$
L1110: form pos 1,c 15,pos 20,c 80,skip 1
	pr #255,using L1110: "FootNote       ",ft$
	pr #255,using L1160: "Dollar Sign Print",dp
	pr #255,using L1160: "Reverse Sign",rs
	pr #255,using L1160: "Print Current Month Figures",cm
L1160: form pos 1,c 27,pos 30,pic(#),skip 1
	pr #255: tab(29);"Dept  Account Sub"
	for j=1 to 80
		if gl$(j)="  0     0  0" then goto L1270
		if j1><48 then goto L1250
		pr #255: newpage
		pr #255,using L1230: "G/L Account Number",gl$(j)(1:3),gl$(j)(4:9),gl$(j)(10:12)
L1230: form skip 6,pos 1,c 18,pos 30,c 3,x 2,c 6,x 2,c 3,skip 1
		goto L1270
L1250: pr #255,using L1260: "G/L Account Number",gl$(j)(1:3),gl$(j)(4:9),gl$(j)(10:12)
L1260: form pos 1,c 18,pos 30,c 3,x 2,c 6,x 2,c 3,skip 1
L1270: j1=j1+1
	next j
	j1=0
	pr #255: newpage
goto L1030

L1330: fncloseprn
	on fkey 5 ignore
	if fnprocess=1 then goto Xit
goto ADD_EDIT_RATIOMST

L1380: if err=4152 then goto L930 else goto ERTN

Xit: fnXit


GL_NUMBERS: ! r:
LEFT_SIDE: !
	fnTos
	resp=0
	fnLbl(1,35,"Left Side Of Ratio",30,0)
	mypos(1)=1: mypos (2)=50
	for j=2 to 40 step 2
		for x=1 to 2
			fnqgl(j/2+1,mypos(x),0,2)
			if x =1 then resp$(resp+=1)=fnrgl$(gl$(j-1)) else resp$(resp+=1)=fnrgl$(gl$(j))
		next x
	next j
	fnCmdKey("&Left Side",2,0,0,"Enter all G/L Numbers to be used on the left side of the ratio.")
	fnCmdKey("&Rignt Side",3,0,0,"Enter all G/L Numbers to be used on the right side of the ratio.")
	fnCmdKey("&Finished",6,0,0,"Finished with general ledger assignments.")
	fnCmdKey("E&xit",5,0,1,"Exits to main menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto RATIOMSTGRID
	for j=1 to 40
		gl$(j)=fnagl$(resp$(j))
	next j
	if ckey=2 then goto LEFT_SIDE
	if ckey=3 then goto RIGHT_SIDE
	if ckey=6 then goto REWRITE_EXISTING_RATIOMST
RIGHT_SIDE: !
	resp=0
	fnTos
	mypos(1)=1: mypos (2)=50
	fnLbl(1,35,"Right Side Of Ratio",30,0)
	for j=2 to 40 step 2
		for x=1 to 2
			fnqgl(j/2+1,mypos(x),0,2)
			if x=1 then resp$(resp+=1)=fnrgl$(gl$(40+j-1)) else resp$(resp+=1)=fnrgl$(gl$(40+j))
		next x
	next j
	fnCmdKey("&Left Side",2,0,0,"Enter all G/L Numbers to be used on the left side of the ratio.")
	fnCmdKey("&Rignt Side",3,0,0,"Enter all G/L Numbers to be used on the right side of the ratio.")
	fnCmdKey("&Finished",6,0,0,"Finished with general ledger assignments.")
	fnCmdKey("E&xit",5,0,1,"Exits to main menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto RATIOMSTGRID
	for j=1 to 40
		gl$(j+40)=fnagl$(resp$(j))
	next j
	if ckey=2 then goto LEFT_SIDE
	if ckey=3 then goto RIGHT_SIDE
	if ckey=6 then goto REWRITE_EXISTING_RATIOMST
goto RATIOMSTGRID ! /r

include: ertn
