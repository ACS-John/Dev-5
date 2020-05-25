! Replace S:\acsPR\Conversion\prGL-Fix
! CREATE GL ENTRIEX
 
	autoLibrary
	on error goto Ertn
 
	dim tdc(6),tcp(22),em$*30,tgl(3),tdet(3)
 
	open #2: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",internal,outIn,keyed
	open #3: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",internal,outIn,relative
	open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],KFName=[Q]\PRmstr\PRCKINDX.h[cno],Shr",internal,outIn,keyed
	open #5: "Name="&env$('Temp')&"\Work."&session$&",KFName="&env$('Temp')&"\Addr."&session$&",RecL=30,KPS=1,KLN=12,Replace",internal,outIn,keyed
	io5$(1)="11,55,N 6,U,N"
	io5$(2)="12,55,N 6,U,N"
	pr newpage
	close #101: ioerr ignore
	open #101: "SROW=10,SCOL=16,EROW=13,ECOL=62,BORDER=DR,CAPTION=CREATE GL ENTRIEX",display,outIn
	pr f "11,18,C 40": "ENTER LOWEST  DATE OR BLANK FOR ALL:"
	pr f "12,18,C 40": "ENTER HIGHEST DATE OR BLANK FOR ALL:"
	pr f "14,22,C 34,R,N": "Press F1 to continue or F5 to stop"
L250: input fields mat io5$,attr "R": prd1,prd2 conv L250
	if ce>0 then io5$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L340 else ce=curfld+1
	if ce>udim(io5$) then ce=1
L290: io5$(ce)=rtrm$(uprc$(io5$(ce))) : ce1=pos(io5$(ce),"U",1)
	ce2=ce1+1 : io5$(ce)(ce1:ce1)="UC" : goto L250
CONV5: if ce>0 then io5$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR5: pr f "24,78,C 1": bell : goto L290
L340: !
	if prd2=0 then prd2=prd1
	if fncd(prd2)<fncd(prd1) then goto L250
	if cmdkey=5 then goto Xit
	gosub L760
L390: read #4,using L400: eno,prd,ckno,mat tdc,mat tcp eof END1
L400: form pos 1,n 8,n 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2
	if fncd(prd)<fncd(prd1) or fncd(prd)>fncd(prd2) then goto L390
	eno$=lpad$(str$(eno),8)
	read #2,using L440,key=eno$: em$,ta1 nokey L390
L440: form pos 9,c 30,pos 173,pd 3
	if ta1=0 then goto L390
	read #3,using L470,rec=ta1: mat tgl,mat tdet
L470: form pos 12,n 3,n 6,n 3,pos 58,3*pd 4.2
	ot=round(tdc(2)*tdet(3),2)
	other=round((tdc(3)+tdc(4)+tdc(5))*tdet(2),2)
	bonus=tcp(18)
	reg=tcp(21)-ot-other-bonus
	accum(1)=accum(1)+reg
	accum(2)=accum(2)+ot
	accum(3)=accum(3)+other
	accum(4)=accum(4)+bonus
	pr #255,using L710: eno,em$,tgl(1),tgl(2),tgl(3),reg pageoflow L730
	gl$=cnvrt$("N 3",tgl(1))&cnvrt$("N 6",tgl(2))&cnvrt$("N 3",tgl(3))
	ga0=reg : gosub TOTGL
	if ot=0 then goto L630
	pr #255,using L710: eno,em$,tgl(1),tgl(2)+1,tgl(3),ot pageoflow L730
	gl$=cnvrt$("N 3",tgl(1))&cnvrt$("N 6",tgl(2)+1)&cnvrt$("N 3",tgl(3))
	ga0=ot : gosub TOTGL
L630: if other=0 then goto L670
	pr #255,using L710: eno,em$,tgl(1),tgl(2)+2,tgl(3),other pageoflow L730
	gl$=cnvrt$("N 3",tgl(1))&cnvrt$("N 6",tgl(2)+2)&cnvrt$("N 3",tgl(3))
	ga0=other : gosub TOTGL
L670: if bonus=0 then goto L710
	pr #255,using L710: eno,em$,tgl(1),tgl(2)+3,tgl(3),bonus pageoflow L730
	gl$=cnvrt$("N 3",tgl(1))&cnvrt$("N 6",tgl(2)+3)&cnvrt$("N 3",tgl(3))
	ga0=bonus : gosub TOTGL
L710: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),n 12.2,skip 1
	goto L390
L730: pr #255: newpage
	gosub L760
	continue
L760: p1=p1+1
	pr #255,using L780: date$,a$,"PAGE",p1
L780: form skip 1,pos 1,c 8,pos nametab,c 40,pos 77,c 5,pic(zzz),skip 1
	pr #255,using L800: time$,"GENERAL LEDGER DISTRIBUTION FOR PAYROLL",dat1
L800: form pos 1,c 8,pos 17,c 40,skip 1,pos 29,pic(zz/zz/zz),skip 2
	pr #255: "EMPLOYEE                                               G/L                 AMOUNT"
	pr #255: " NUMBER        NAME                                  ACCOUNT         DEBITS     CREDITS"
	pr #255:
	return
TOTGL: read #5,using L860,key=gl$: gl$,ga1 nokey L900
L860: form pos 1,c 12,pd 5.2
	ga1=ga1+ga0
	rewrite #5,using L860: gl$,ga1
	goto L910
L900: write #5,using L860: gl$,ga0
L910: return
END1: pr #255: " "
	pr #255: "  GL NUMBER          TOTAL"
	pr #255: "------------   -----------------"
	restore #5,key>="            ": nokey Xit
L960: read #5,using L860: gl$,ga1 eof Xit
	pr #255,using L980: gl$,ga1
L980: form pos 1,c 12,pic(zzz,zzz,zzz,zzz.##bcr),skip 1
	goto L960
Xit: stop
include: Ertn
