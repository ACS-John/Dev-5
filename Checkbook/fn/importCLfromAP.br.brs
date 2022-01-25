! Pull stuff from old Accounts Payable into new Checkbook company
def library fnImportCLfromAP
autoLibrary
on error goto Ertn

dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,ph$*12,a(9),dt(5),cd(4)
dim gl(3),ta(2),aa(2),gl$(5)*12,gld$(5)*20,gla(5),id$*20
dim up$(4)*18
dim resp$(20)*80

SCR1: !
	fnTos
	lc=0
	mylen=40
	mypos=mylen+2
	fnLbl(lc+=1,1,'Path to Accounts Payable Data Files:',mylen,1,0,0,0,'without trailing backslash')
	fnTxt(lc,mypos,40,58,0,'70',0,'Pick any file in the directory, it doesn''t matter which one - Only the directory name matters')
	resp$(1)='C:\vol002\APmstr'
	fnLbl(lc+=1,1,'Old Accounts Payable Company Number:',mylen,1)
	fnLbl(2,90,'') ! work around to make the little button show up
	fnLbl(3,80,'') ! work around to make the little button show up
	fnTxt(lc,mypos,2,0,1,'30')
	resp$(2)='1'
	fnCmdSet(5)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		apcno=val(resp$(2))
		if ~exists('[Q]\tmpAP') then
			execute 'mkdir [Q]\tmpAP'
		else
			fnFree('[Q]\tmpAP\*.*')
		end if
		fnCopy(resp$(1)&'\*.h'&str$(apcno),'[Q]\tmpAP\*.*')
		if exists('[Q]\tmpAP\apcoinfo.h'&str$(apcno))=0 then goto SCR1
		open #apmstr=fnH: 'Name=[Q]\tmpAP\APmstr.h'&str$(apcno),i,i  ! &',KFName=[Q]\tmpAP\apIndex.h'&str$(apcno) ,keyed
		open #aptrans=10: 'Name=[Q]\tmpAP\apTrans.H'&str$(apcno),i,outi,r
		open #paymstr=fnH: 'Name=[Q]\CLmstr\PayMstr.h[cno],Version=1,size=0,RecL=276,Replace',i,outi,r
		open #payalloc=fnH: 'Name=[Q]\CLmstr\PayAlloc.h[cno],Size=0,RecL=56,Replace',i,outi,r
		open #paytrans=fnH: 'Name=[Q]\CLmstr\PayTrans.h[cno],Version=2,Size=0,RecL=114,Replace',i,outi,r
		open #unpdaloc=fnH: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],SIZE=0,RecL=70,Replace',i,outi,r
		do
			read #apmstr,using 'form pos 1,C 8,4*C 30,pos 159,C 12,pos 176,PD 5.2,pos 219,N 2,C 11,pos 213,2*PD 3': vn$,nam$,ad1$,ad2$,csz$,ph$,ytdp,typ,ss$,mat ta eof EO_11
			gosub UNPDMSTR
			mat ta=(0)
			lr2=lrec(payalloc)+1
			write #payalloc,using 'form pos 1,C 8,N 3,N 6,N 3,PD 3.2,C 30,PD 3',rec=lr2: vn$,mat gl,100,'',0
			mat ta=(lr2)
			lr1=lrec(paymstr)+1
			write #paymstr,using 'form pos 1,C 8,4*C 30,PD 5.2,N 2,C 11,2*PD 3,C 12',rec=lr1: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat ta,ph$
		loop
		EO_11: ! r:
		close #paymstr:
		close #payalloc:
		close #paytrans:
		close #unpdaloc:
		close #aptrans,free:
		close #apmstr,free:
		fnFree('[Q]\tmpAP\*.*')
		execute 'RmDir "[Q]\tmpAP"'
		fnIndex('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\PayIdx1.h[cno]','1 8')
		fnIndex('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\PayIdx2.h[cno]','9 30')
		fnIndex('[Q]\CLmstr\PayTrans.h[cno]','[Q]\CLmstr\UnPdIdx1.h[cno]','1,20')
		fnIndex('[Q]\CLmstr\PayTrans.h[cno]','[Q]\CLmstr\UnPdIdx2.h[cno]','31/27/1 2/4/26')
		fnIndex('[Q]\CLmstr\Unpdaloc.h[cno]','[Q]\CLmstr\Uaidx1.h[cno]','9 12')
		fnIndex('[Q]\CLmstr\Unpdaloc.h[cno]','[Q]\CLmstr\Uaidx2.h[cno]','1 20')
		! /r
		
		! r: glbld-cnv
		open #h2=fnH: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr',i,i,k
		open #h1=fnH: 'Name=[Q]\CLmstr\GLmstr.h[cno],RecL=62,Replace',internal,output
		do
			dim gl$*12,de$*50
			read #h2,using 'form pos 1,C 12,C 50': gl$,de$ eof GlBldCnvFinis
			write #h1,using 'form pos 1,C 12,C 50': gl$,de$
		loop
		GlBldCnvFinis: !
		close #h1:
		close #h2:
		fnIndex('[Q]\CLmstr\GLmstr.h[cno]','[Q]\CLmstr\GLINDEX.h[cno]','1 12')
		! /r
		
	end if
fnend
UNPDMSTR: ! r: BUILD UNPAID FILE
	adr=ta(1)
	ReadApTrans: !
	if adr=0 then goto EO_UNPDMSTR
	read #aptrans,using 'form pos 1,C 8,C 12,C 20,8*PD 5.2,6*PD 4,3*N 1,N 2,6*C 12,5*C 20,5*PD 5.2,PD 3',rec=adr,reserve: v$,iv$,id$,mat a,mat dt,mat cd,dgl$,mat gl$,mat gld$,mat gla,nta noRec EO_UNPDMSTR
	if dt(4)>0 then adr=nta: goto ReadApTrans ! only unpaids
	if a(2)=0 then goto UNPDMSTR_ATZ
	mat aa=(0)
	for j=1 to 5
		if gla(j)=0 then goto NXJ
		on cd(2) goto XB,XA,XA,XB,XA,XA none XB
		XA: !
		gla(j)=-gla(j)
		XB: !
		lr4=lrec(unpdaloc)+1
		write #unpdaloc,using 'form pos 1,C 8,2*C 12,PD 5.2,C 30,PD 3',rec=lr4: vn$,iv$,gl$(j),gla(j),gld$(j),0
		if aa(1)=0 then aa(1)=lr4
		if aa(2)>0 then
			rewrite #unpdaloc,using 'form pos 68,PD 3',rec=aa(2): lr4
		end if
		aa(2)=lr4
		NXJ: !
	next j
	if cd(2)=1 or cd(2)=4 or cd(2)=0 then
		goto XD
	else if cd(2)=2 or cd(2)=3 or cd(2)=5 or cd(6)=6 then
		goto XC
	end if
	XC: !
	a(2)=-a(2)
	XD: !
	if dt(1)=0 then dt(1)=dt(5)
	vn$=v$
	upa=a(2) ! unpaid amount
	up$(1)=str$(dt(1)) ! invoice date
	up$(2)=str$(dt(2)) ! due date
	up$(3)='' ! po #
	up$(4)=id$(1:18)
	lr3=lrec(paytrans)+1
	write #paytrans,using 'form pos 1,C 8,c 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
	UNPDMSTR_ATZ: !
	adr=nta
	goto ReadApTrans
	EO_UNPDMSTR: !
return  ! /r
include: ertn
