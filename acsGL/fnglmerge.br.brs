! Replace S:\acsGL\fnglMerge
! GL Merge program, chained to from other systems, like Checkbook-post to GL
def library fnglmerge
	autoLibrary
	on error goto Ertn
 
	dim adr(2),ta(2),prg$*20,k(10,8),gl$(5)*12,gl1(5)
	dim t$*12,n(2),l$*12,p$*30,ven$*8,zo(50),d$*50
	dim nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,fl1$(6),cap$*128
	dim ml$(10)*80 ! for fnMsgBox
	dim resp$(10)*80 ! for Screen Ace
 
	on=1 : off=0 : cancel=5 : delete=4 : selbyrow=1
	limit_to_list=1 : add_all=2 : right=1 : disable=1
	center=2 : pointtwo$='32' : mmddyy$='1'
	fnTop(program$,cap$="GL Merge")
	if fncursys$='GL' then let fncno(cno) else gosub ASK_GLCNO
	fnprg(prg$)
	if fnstyp=99 then goto L200
	if fnstyp=9 then prg$="S:\acsTM\tmMenu" else prg$="S:\acsGL\acGLAuto"
	fnprg(prg$,2)
	L200: !
	open #glmstr:=fngethandle: "Name=[Q]\GLmstr\GLmstr.H[cno],KFName=[Q]\GLmstr\GLIndex.H[cno],Shr",internal,outIn,keyed ioerr GLMSTR_OPEN_ERR
	open #gltrans:=fngethandle: "Name=[Q]\GLmstr\GLTrans.H[cno],Shr",internal,outIn,relative
	open #glwk1:=fngethandle: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".dat,NoShr",internal,outIn
	open #gl1099:=fngethandle: "Name=[Q]\GLmstr\GL1099.H[cno],KFName=[Q]\GLmstr\gl109idx.H[cno],Shr",internal,outIn,keyed
	open #gltr1099:=fngethandle: "Name=[Q]\GLmstr\GLTR1099.H[cno],Shr",internal,outIn,relative
	! fnwait
	READ_GLWK1: !
	read #glwk1,using 'Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,C 8': t$,s,k,mat n,l$,p$,ven$ eof EO_GLWK1
	if n(2)=9 then goto READ_GLWK1 ! CHECK PREVIOUS POST
	if k=0 and uprc$(trim$(p$))<>"VOID" then goto READ_GLWK1
	HERE_A: !
	if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 and k=0 then goto READ_GLWK1
	if t$(3:3)=" " then t$(3:3)="0"
	if t$(12:12)=" " then t$(12:12)="0"
	read #glmstr,using 'Form POS 87,PD 6.2,POS 333,2*PD 3',key=t$: cb,mat ta nokey REJECT_GL
	READ_GLTRANS: !
	! READ #gltrans,USING 460,REC=1: LR2
	lr2=lrec(gltrans)+1
	write #gltrans,using 'Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,PD 3',rec=lr2: t$,s,k,mat n,l$,p$,0 duprec READ_GLTRANS
	if ta(1)=0 then ta(1)=lr2
	if ta(2)>0 then		rewrite #gltrans,using 'Form POS 71,PD 3',rec=ta(2): lr2
	ta(2)=lr2
	cb+=k
	rewrite #glmstr,using 'Form POS 87,PD 6.2,POS 333,2*PD 3',key=t$: cb,mat ta
	! REWRITE #gltrans,USING 460,REC=1,RELEASE: LR2
	rewrite #glwk1,using 'Form POS 27,N 2': 9
	L460: !
	if trim$(ven$)="" or trim$(ven$)="0" then goto READ_GLWK1
	read #gl1099,using 'Form POS 104,PD 5.2,POS 122,2*PD 3',key=ven$: ytdp ,mat adr nokey L1250
	ytdp+=k
	! READ #GLTR1099,USING 260,REC=1,RESERVE: LR5
	L500: lr5=lrec(gltr1099)+1
	write #gltr1099,using 'Form POS 1,C 8,N 6,PD 5.2,C 12,C 30,PD 3',rec=lr5,reserve: ven$,s,k,l$,p$,0 duprec L500
	if adr(2)=0 then adr(1)=lr5 else		rewrite #gltr1099,using 'Form POS 62,PD 3',rec=adr(2),reserve: lr5
	rewrite #gltr1099,using 'Form POS 62,PD 3',rec=1,release: lr5
	adr(2)=lr5
	rewrite #gl1099,using 'Form POS 104,PD 5.2,POS 122,2*PD 3',key=ven$: ytdp,mat adr
	goto READ_GLWK1

	REJECT_GL: !
		fnTos(sn$='GLMerge-Reject_GL')
		lc=0 : mylen=20 : mypos=mylen+2
		fnLbl(lc+=1,1,'GL Account Reject',80,center)
		fnLbl(lc+=1,1,'Account Number:',mylen,right)
		fnTxt(lc,mypos,12,0,right,'',disable)
		resp$(1)=t$
		fnLbl(lc+=1,1,'Date:',mylen,right)
		fnTxt(lc,mypos,0,0,right,mmddyy$,disable)
		resp$(2)=str$(s)
		fnLbl(lc+=1,1,'Amount:',mylen,right)
		fnTxt(lc,mypos,11,0,right,pointtwo$,disable)
		resp$(3)=str$(k)
		fnLbl(lc+=1,1,'Reference Number:',mylen,right)
		fnTxt(lc,mypos,8,0,right,'',disable)
		resp$(4)=l$
		fnLbl(lc+=1,1,'Description:',mylen,right)
		fnTxt(lc,mypos,30,0,right,'',disable)
		resp$(5)=p$
		fnLbl(lc+=1,1,"Account Number "&trim$(t$)&" is not in the GL Master File",80,center)
		fnLbl(lc+=1,1,'Do you wish to add this account now?',80,center)
		fnCmdKey('&Yes',1,1,0)
		fnCmdKey('&No',5,0,1)
		fnAcs2(mat resp$,ckey)
		if ckey=1 then in1$='Yes' else if ckey=5 then in1$='No'
	if in1$="Yes" then goto ADD_GL else goto CORRECT_GL
 
CORRECT_GL: !
	fnTos
	lc=0 : mylen=31 : mypos=mylen+2
	fnLbl(lc+=1,1,'GL Account Reject',80,center)
	fnLbl(lc+=1,1,'Correct General Ledger Account:',mylen,right)
	fncombof("gla-[cno]",lc,mypos,0,"[Q]\GLmstr\GLmstr.h[cno]",13,20,1,12,"[Q]\GLmstr\glIndx2.h[cno]",limit_to_list)
	resp$(1)=''
	fnCmdKey('&Okay',1,1,1)
	fnAcs2(mat resp$,ckey)
	t$=resp$(1)(22:33)
	goto HERE_A
 
ADD_GL: !
	fnTos
	lc=0 : mylen=32 : mypos=mylen+2
	fnLbl(lc+=1,1,'GL Account Reject',80,center)
	fnLbl(lc+=1,1,'New General Ledger Account Name:',mylen,right)
	fnTxt(lc,mypos,30)
	resp$(1)=''
	fnCmdKey('&Okay',1,1,1)
	fnAcs2(mat resp$,ckey)
	d$=resp$(1)
	mat ta=(0) : cb=0
	write #glmstr,using 'Form POS 1,C 12,C 50,6*PD 3,42*PD 6.2,2*PD 3': t$,d$,mat zo
	new1=1
	goto READ_GLTRANS
 
EO_GLWK1: !
	if fnstyp><92 then goto DONE
	open #20: "Name=CNo.H"&wsid$,internal,outIn,relative 
	read #20,using "Form POS 239,5*C 12,5*N 10.2",rec=1: mat gl$,mat gl1 conv DONE
	close #20:
	ckgl=0
	for j=1 to 5
		if val(gl$(j)(4:9))=0 then goto L1090 else gl2=0
		read #glmstr,using 'Form POS 87,PD 6.2',key=gl$(j): gl2 nokey L1050
L1050: if gl1(j)=gl2 then goto L1090
		if ckgl=0 then pr newpage; bell
		pr using 'Form POS 1,C 11,C 14,C 15,N 12.2,X 4,C 12,N 12.2': "Account #:",gl$(j),"Client Balance:",gl1(j),"GL Balance:",gl2
		ckgl=1
L1090: next j
	if ckgl=0 then goto DONE
	pr f "24,35,Cc 10,B,1": "Next  (F1)"
L1120: input fields "24,2,C 1,AE,N": pause$
	if cmdkey=1 then goto DONE else goto L1120
 
DONE: close #glmstr:
	close #gltrans:
	close #glwk1:
	fnFree("[Q]\GLmstr\GLPT"&wsid$&".H[cno]")
	L1180: !
	close #gl1099: ioerr ignore
	if new1=1 or new2=1 then
		fnIndex("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")
	end if
	if new1=1 then 
		fnIndex('[Q]\GLmstr\GLmstr.H[cno]','[Q]\GLmstr\GLIndex.H[cno]','1 12')
	end if
	if new2=1 then 
		fnIndex('[Q]\GLmstr\GL1099.H[cno]','[Q]\GLmstr\GL109IDX.H[cno]','1 8')
	end if
	if fnprocess=1 then 
		fnchain("S:\acsGL\acGLAuto") 
	else if fnprocess=2 then 
		fnchain("S:\acsGL\glMenu") 
	else if fnprocess=4 then 
		fnchain("S:\acsGL\prMerge") 
	end if
goto Xit
 
L1250: cap$="GL Account Reject"
	fnwin3(win=101,cap$,15,70,1,0,5)
	pr #win,fields "4,2,C 50,N": "    Account Number: "&t$
	pr #win,fields "5,2,C 50,N": "              Date: "&str$(s)
	pr #win,fields "6,2,C 50,N": "            Amount: "&str$(k)
	pr #win,fields "7,2,C 50,N": "  Reference Number: "&l$
	pr #win,fields "8,2,C 50,N": "       Description: "&p$
	pr #win,fields "9,2,C 50,N": "    Vendor Number: "&ven$
	pr #win,fields "11,2,Cc 60,N": "Vendor Number "&ven$&" is not on the 1099 Master File"
	pr #win,fields "12,13,C 36,N": "Do you want to add the Vendor (Y/N)?"
L1290: input #win,fields "12,50,Cu 1,UAET,N": in2$ conv L1290
	close #win:
	if in2$="Y" then goto L1420
	if in2$<>"N" then goto L1250 else goto ASK_CORRECT_VN
 
ASK_CORRECT_VN: !
	cap$="GL Account Reject"
	fnwin3(win=101,cap$,5,33,1,0,5)
	pr #win,fields "4,2,C 22,N": "Correct Vendor Number:"
L1370: input #win,fields "4,25,Cu 8,UT,N": ven$ conv L1370
	close #win:
	ven$=lpad$(trim$(ven$),8)
	goto L460
 
L1420: cap$="Vendor Information"
	fnwin3(win=101,cap$,10,56,1,0,5)
	pr #win,fields "4,2,Cr 18,N": "Vendor Name:"
	pr #win,fields "5,2,Cr 18,N": "Vendor Address:"
	pr #win,fields "6,2,Cr 18,N": "Vendor Address:"
	pr #win,fields "7,2,Cr 18,N": "City, State Zip:"
	pr #win,fields "8,2,Cr 18,N": "1099 Type:"
	pr #win,fields "9,2,Cr 18,N": "Federal ID Number:"
	fl1$(1)="4,21,C 35,UT,N" : fl1$(2)="5,21,C 20,UT,N"
	fl1$(3)="6,21,C 20,UT,N" : fl1$(4)="7,21,C 20,UT,N"
	fl1$(5)="8,21,N 02,UT,N" : fl1$(6)="9,21,C 11,UT,N"
	pr f "18,35,C 09,B,1": "Next (F1)"
L1460: input #win,fields mat fl1$: nam$,ad1$,ad2$,csz$,typ,ss$ conv CONV1
	if ce>0 then fl1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1550 else ce=curfld+1
	if ce>udim(fl1$) then ce=1
L1500: fl1$(ce)=rtrm$(uprc$(fl1$(ce))) : ce1=pos(fl1$(ce),"U",1)
	ce2=ce1+1 : fl1$(ce)(ce1:ce1)="UC" : goto L1460
CONV1: if ce>0 then fl1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L1500
L1550: !
	mat adr=(0)
	if k=0 then goto L1630
	read #gltr1099,using 'Form POS 62,PD 3',rec=1,reserve: lr5
L1590: lr5=lrec(gltr1099)+1
	write #gltr1099,using 'Form POS 1,C 8,N 6,PD 5.2,C 12,C 30,PD 3',rec=lr5,reserve: ven$,s,k,l$,p$,0 duprec L1590
	rewrite #gltr1099,using 'Form POS 62,PD 3',rec=1,release: lr5
	mat adr=(lr5)
L1630: write #gl1099,using 'Form POS 1,C 8,C 35,3*C 20,PD 5.2,N 2,C 11,2*PD 3': ven$,nam$,ad1$,ad2$,csz$,k,typ,ss$,mat adr
	new2=1
	goto READ_GLWK1
 
GLMSTR_OPEN_ERR: !
	mat ml$(3)
	ml$(1)='Company Number [cno] does not exists!'
	ml$(2)='Please try again.'
	ml$(3)='Nothing Posted.'
	fnmsgbox(mat ml$,ok$,cap$,16)
	goto Xit
 
include: Ertn
 
ASK_GLCNO: !
	fnTos
	lc=0
	fnLbl(lc+=1,1,'Select the General Ledger Company to Post to')
	fncmbcno(lc+=1,5,'GL')
	fnCmdKey('&Okay',1,1,1)
	fnAcs2(mat resp$,ckey)
	cno=val(resp$(1)(43:47))
 
return
 
Xit: fnend
 
