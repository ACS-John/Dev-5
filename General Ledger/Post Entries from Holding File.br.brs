! formerly S:\acsGL\ACGLPOST
! Post Entries from Holding File
	autoLibrary
	on error goto Ertn
 
	dim ta(2)
	dim t$*12,n(2),l$*12,p$*30,ven$*8,zo(50),d$*50
	dim k(10,8),filename$(0)*132 ,dir(200),ml$(4)*80,item$(3)

	mat chdr$(3) : mat cmask$(3)
	chdr$(1)="Date" 
	chdr$(2)="Creation Date" 
	chdr$(3)="Creation Time" 
	cmask$(1)='30'
	cmask$(2)=''
	cmask$(3)=''

	fnTop(program$)
	open #4: "Name=[Q]\GLmstr\GLmstr.H[cno],Shr,KFName=[Q]\GLmstr\GLINDEX.H[cno],Shr",internal,outIn,keyed 
	open #2: "Name=[Q]\GLmstr\GLTRANS.H[cno],Shr",internal,outIn,relative 
	gosub BUILD_LAYOUT
MAIN: ! r:
	fnTos
	mylen=10: mypos=mylen+3 : right=1
	fnLbl(1,8,"Date Range to Post")
	fnLbl(2,1,"From:",mylen,right)
	fnTxt(2,mypos,8,0,right,"1001",0,"'From' date must always be answered and will be the first date you wish to review for posting..",0 ) 
	resp$(1)=str$(from)
	fnLbl(3,1,"To:",mylen,right)
	fnTxt(3,mypos,8,0,right,"1001",0,"'To' date must always be answered and will be the last day of the month or the last day of the period being processed..",0 ) 
	resp$(2)=str$(to)
	fnLbl(4,37,"")
	fnCmdKey("&Next",1,1,0,"Allows you to select files to be posted.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	from =val(resp$(1))
	to =val(resp$(2))
	from$=cnvrt$("PIC(######)",from): to$=cnvrt$("PIC(######)",to)
! DATE_LIST: !
	fnTos
	mylen=10: mypos=mylen+3 : right=1
	fnflexinit1('acglpost2',lc=1,1,15,30,mat chdr$,mat cmask$,1)
	fngetdir2('[Q]\GLmstr\',mat filename$,'','GL*.H[cno]',mat filedate$,mat filetime$)
	dircount=0
	for filenameItem=1 to udim(mat filename$)
		x=val(filename$(filenameItem)(3:8)) conv L420
		if x<10100 or x>123199 then goto L420
		if fndate_mmddyy_to_ccyymmdd(x)>=fndate_mmddyy_to_ccyymmdd(val(from$)) and fndate_mmddyy_to_ccyymmdd(x)<=fndate_mmddyy_to_ccyymmdd(val(to$)) then 
			mat dir(dircount+=1) : dir(dircount)=val(filename$(filenameItem)(3:8))
			item$(1)=filename$(filenameItem)(3:8) 
			item$(2)=filedate$(filenameItem)
			item$(3)=filetime$(filenameItem)
			fnflexadd1(mat item$)
		end if
		L420: !
	nex filenameItem
	fnLbl(17,30,"")
	fnCmdKey("&Post",1,1,0,"Post entries from the holding files that are displayed.")
	fnCmdKey("&Review",2,0,0,"Review entries before posting.")
	fnCmdKey("&Print",3,0,0,"Prints list of entries.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=1 then listing$='N' : goto PRINT_POST
	if ckey=2 then goto REVIEW
	if ckey=3 then listing$="Y" : goto PRINT_POST
	if ckey=5 then goto Xit
! /r
PRINT_POST: !
	if listing$="Y" then listing=1 else listing=0
	if listing=1 then gosub L1230
	for j3=1 to dircount
		if dir(j3)<>0 then 
			close #3: ioerr ignore
			open #3: "Name=[Q]\GLmstr\GL"&cnvrt$("PIC(######)",dir(j3))&".H[cno],RecL=104,USE",internal,outIn,relative 
			if listing=1 then 
				pr #255,using 'form pos 1,c 11,pic(zz/zz/zz),skip 2': "File Date: ",dir(j3)
			end if
			do
				L670: !
				read #3,using L680: t$,s,k,mat n,l$,p$,ven$ eof L1050
				L680: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8
				! If N(2)=9 Then Goto 660 ! CHECK PREVIOUS POST
				if k=0 and uprc$(ltrm$(rtrm$(p$)))<>"VOID" then goto L670
				if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 then goto L670
				if t$(3:3)=" " then t$(3:3)="0"
				if t$(12:12)=" " then t$(12:12)="0"
				read #4,using L750,key=t$: cb,mat ta nokey BAD_ACCOUNT
				L750: form pos 87,pd 6.2,pos 333,2*pd 3
				L760: !
				read #2,using L900,rec=1: lr2
				lr2=lrec(2)+1
				write #2,using L880,rec=lr2,reserve: t$,s,k,mat n,l$,p$,0 duprec L760
				if k>0 then x=25 else x=40
				if listing=1 then pr #255,using L810: t$,s,k,l$,p$,n(1) pageoflow L1290
				L810: form pos 1,c 12,x 3,pic(zz/zz/zz),pos x,pic(---,---,---.##),pos 56,c 15,c 33,n 1,skip 1
				if k<0 then totalcr=totalcr+k else totaldr=totaldr+k
				if ta(1)=0 then ta(1)=lr2
				if ta(2)>0 then rewrite #2,using L900,rec=ta(2),reserve: lr2
				ta(2)=lr2
				cb=cb+k
				rewrite #4,using L750,key=t$: cb,mat ta
				L880: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
				rewrite #2,using L900,rec=1,release: lr2
				L900: form pos 71,pd 3
				rewrite #3,using L920: 9
				L920: form pos 27,n 2
			loop
		BAD_ACCOUNT: ! r:
			mat ml$(4) 
			ml$(1)="Account # "&t$&" is not in the general ledger file." 
			ml$(2)="Transaction information: Date "&str$(s)&"; Amount "&str$(k) 
			ml$(3)="Description "&trim$(p$)&". Yes to setup this account" 
			ml$(4)="or No to change account #" 
			fnmsgbox(mat ml$,resp$,'',52)
			if resp$="Yes" then in1=1 else in1=0 ! one is to set up the account
			if resp$="Yes" then gosub ADD
			if resp$="No" then gosub CHANGE_ACCOUNT
			mat ta=(0)
			cb=0
			write #4,using L1020: t$,d$,mat zo
			L1020: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3
			goto L760 ! /r
		L1050: !
			close #3: 
			if listing =1 then pr #255,using L1070: "------------","------------","DAILY TOTALS",totaldr,totalcr,"------------","------------"
			L1070: form pos 27,c 12,x 3,c 12,skip 1,pos 5,c 20,pos 24,2*pic(----,---,---.##),skip 1,pos 27,c 12,x 3,c 12,skip 1
			gtdr=gtdr+totaldr: gtcr=gtcr+totalcr
			totaldr=totalcr=0
		end if
	next j3
	close #1: ioerr ignore
	close #2: ioerr ignore
	close #3: ioerr ignore
	if listing=1 then pr #255,using L1070: "            ","            ","GRAND TOTALS",gtdr,gtcr,"============","============" else goto L1170
	fncloseprn
	if listing=1 then goto MAIN
L1170: !
	for j=1 to dircount
		if dir(j)<>0 then 
			fnFree("[Q]\GLmstr\GL"&cnvrt$("PIC(######)",dir(j))&".H[cno]")
		end if 
	next j
Xit: fnXit

L1230: ! r:
	fnopenprn
	pr #255,using L1250: env$('cnam'),"General Ledger Posting","From: "&from$&"   To: "&to$
	L1250: form pos 1,cc 80,skip 1,pos 30,c 30,skip 1,pos 28,c 40,skip 1
	pr #255,using L1270: "  ACCOUNT #       DATE          DEBITS        CREDITS   REFERENCE #   DESCRIPTION                   SOURCE"
	L1270: form pos 1,c 132,skip 1
return ! /r
L1290: ! r:
	pr #255: newpage
	gosub L1230
continue ! /r

REVIEW: ! r:
	fnTos
	mylen=20: mypos=mylen+3 : right=1
	fnLbl(1,1,"Date to Review:",mylen,right)
	fnTxt(1,mypos,8,0,right,"1001",0,"Enter the file date to be reviewed.",0 ) 
	resp$(1)=""
	fnLbl(2,40,"")
	fnCmdKey("&Next",1,1,0,"Review the entries for the date entered.")
	fnCmdKey("&Cancel",5,0,1,"Returns to listing of dates.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto MAIN
	review=val(resp$(1))
	!  use hamster to review entries
	! Holding file Transactions - Hamster
	dim lbl$(8)*38,tln(8),p$(8)*160,fltyp$(8),sln(8),mask(8),sp(8),c$(8,8)*256
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE 
	fnHamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
	gosub CLOSE_FILE
	! Open #3: "Name=[Q]\GLmstr\GL"&CNVRT$("PIC(######)",DIR(J3))&".H[cno],RecL=104,USE",Internal,outIn,Relative
goto MAIN ! /r

OPEN_FILE: ! r:
	open_file_count=1 : close #3: ioerr ignore ! this value is used in the close_file sub routine
	if exists("[Q]\GLmstr\GL"&cnvrt$("PIC(######)",review)&".H[cno]")=0 then gosub INDEX
	open #open_file_count: "Name=[Q]\GLmstr\GL"&cnvrt$("PIC(######)",review)&".H[cno],KFName=[Q]\GLmstr\GL"&cnvrt$("PIC(######)",review)&"-idx.H[cno],RecL=104,kps=1,kln=12,USE",internal,outIn,keyed 
return  ! /r

CLOSE_FILE: ! r:
	for j=1 to open_file_count
		close #j: ioerr ignore
	next j
return  ! /r
INDEX: ! r:
	if ~fnIndex("[Q]\GLmstr\GL"&cnvrt$("PIC(######)",review)&".H[cno]","[Q]\GLmstr\GL"&cnvrt$("PIC(######)",review)&"-idx.H[cno]","1 12") then goto MAIN
	return  ! /r

ADD: ! r:
	fnTos
	mylen=23: mypos=mylen+3 : right=1: rc=0
	if use_dept =1 then let fnLbl(1,26,"Fund #",6,2)
	if use_sub =1 then let fnLbl(1,40,"Sub #",6,2)
	fnLbl(2,1,"General Ledger Number:",mylen,right)
	if use_dept=1 then 
		fnTxt(2,26,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",0 ) 
		resp$(rc+=1)=str$(dno)
	end if
	fnTxt(2,31,6,0,right,"30",0,"Enter the main part of the general ledger number.",0 ) 
	resp$(rc+=1)=str$(ano)
	if use_sub=1 then 
		fnTxt(2,40,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",0 ) 
		resp$(rc+=1)=str$(sno)
	end if
	fnLbl(3,1,"Description:",mylen,right)
	fnTxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
	resp$(rc+=1)=""
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	pas=0
	if ckey=5 then goto MAIN
	dno=ano=sno=0
	if use_dept=1 then dno=val(resp$(1)) : ano=val(resp$(2))
	if use_dept=0 then ano=val(resp$(1))
	if use_dept=1 and use_sub=1 then sno=val(resp$(3))
	if use_dept=0 and use_sub=1 then sno=val(resp$(2))

	if use_dept=1 and use_sub=1 then d$=resp$(4)
	if use_dept=0 and use_sub=1 then d$=resp$(3)
	if use_dept=0 and use_sub=0 then d$=resp$(2)
	if use_dept=1 and use_sub=0 then d$=resp$(3)
	key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
	read #4,using 'Form POS 1,N 3',key=key$: dno nokey L2310 ! 
! MSGBOX2: !
	mat ml$(3) 
	ml$(1)="General ledger account # "&key$&" already " 
	ml$(2)="exists. Take OK to change the account." 
	ml$(3)="Take Cancel to set the account up." 
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$="Cancel" then goto ADD
	if resp$="Ok" then gosub CHANGE_ACCOUNT
L2310: return  ! /r

CHANGE_ACCOUNT: ! r:
	fnTos
	mylen=23: mypos=mylen+3
	fnLbl(1,1,"Bank Account #:",mylen,right)
	fnqgl(1,mypos,0,2,pas) 
	resp$(1)=fnrgl$(bankgl$)
	fnCmdKey("&Next",1,1,0,"Continue posting.")
	fnAcs(mat resp$,ckey)
	key$=bankgl$=fnagl$(resp$(1)) ! gl number
return  ! /r
BUILD_LAYOUT: ! r:
! ** Field Labels    **
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="G/L"
	lbl$(ic+=1)="Date"
	lbl$(ic+=1)="Amount"
	lbl$(ic+=1)="TCode"
	lbl$(ic+=1)="PCode"
	lbl$(ic+=1)="Ref#"
	lbl$(ic+=1)="Description"
	lbl$(ic+=1)="Vendor"
! ** Text Box / Field Display   Lengths   **
	ic=0 ! temporary Item Counter
	mmddyy=8
	ccyymmdd=10
	tln(ic+=1)=12
	tln(ic+=1)=6
	tln(ic+=1)=12
	tln(ic+=1)=2
	tln(ic+=1)=2
	tln(ic+=1)=12
	tln(ic+=1)=30
	tln(ic+=1)=8
! ** Field Types **
	ic=0
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='PD'
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths **
	ic=0
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=12
	sln(ic+=1)=6
	sln(ic+=1)=6.2
	sln(ic+=1)=2
	sln(ic+=1)=2
	sln(ic+=1)=12
	sln(ic+=1)=30
	sln(ic+=1)=8
! ** Field Masks **
	ic=0
	pointtwo=32 : number=30
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=0
	mask(ic+=1)=1
	mask(ic+=1)=10 ! mask 10 is 2 decimals and commas
	mask(ic+=1)=30
	mask(ic+=1)=30
	mask(ic+=1)=0
	mask(ic+=1)=0
	mask(ic+=1)=0
! ** Storage Positions **
! starting field position - default to the same as order displayed
	ic=0
	sp(ic+=1)=1
	sp(ic+=1)=13
	sp(ic+=1)=19 ! 
	sp(ic+=1)=25
	sp(ic+=1)=27
	sp(ic+=1)=29
	sp(ic+=1)=41
	sp(ic+=1)=71
	! ** Combo Boxes **
	cl=1 : c$(cl,1)='ComboF'
	c$(cl,2)="[Q]\GLmstr\GLmstr.h[cno]"
	c$(cl,3)="1" : c$(cl,4)="12"
	c$(cl,5)="13": c$(cl,6)="40"
	c$(cl,7)="[Q]\GLmstr\glindex.h[cno]"
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
	limit_to_list$='1'
return  ! /r
include: ertn
