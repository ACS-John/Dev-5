! Replace S:\acsGL\PRTexasUC
! Quarterly UC Report (From the after-the-fact payroll files in gl) for Texas
 
	autoLibrary
	on error goto Ertn
 
	dim k(1),k$(3)*25,l$(1)*11,d(14),m(36),n(2),cap$*128
	dim fa$(3),sa$(3)*40,cnam$*40
	dim a$(3)*40,b$(2)*12,c$*5,e(2),e$(2)*11,pedat$*5
	dim resp$(3)*255,csvpath$*255
 
	fnTop(program$,cap$="Print Texas Unemployment Report")
	fncno(cno,cnam$)
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input  : _
	read #1,using 'form pos 1,3*C 40,2*C 12,C 5,pos 188,PD 7.2,pos 658,10*N 1': mat a$,mat b$,c$,ucm,mat deduc : _
	close #1:
	if fnprocess=1 then goto L280
 
L170: fnTos
	mylen=60: mypos=mylen+3 : right=1
	fnLbl(1,1,"Quarter Ending Date(mm-yy):",mylen,right)
	fnTxt(1,mypos,5,0,left,"",0,"Enter the date as two numeric digits for the month, then a dash, and two digits for the year." ,0 ) : _
	resp$(1)=""
	fnLbl(2,1,"Name Format (F=first name first; L=Last name first):",mylen,right)
	fnTxt(2,mypos,1,0,left,"",0,"Enter 'F' if first name first; else 'L' if last name shown first." ,0 ) : _
	resp$(2)=""
	fnLbl(3,1,"Enter the location to save employees to CSV for QuickFile",mylen,right)
	fnTxt(3,mypos,60,0,left,"",0,"Enter a CSV file path.",0)
	fnreg_read("TexasUCFile",csvpath$)
	if trim$(csvpath$)="" then csvpath$="[Q]\GLmstr\txuc.csv"
	resp$(3)=csvpath$
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	pedat$=resp$(1)
	namcde$=uprc$(resp$(2))
	csvpath$=resp$(3)
	if trim$(namcde$)="" then goto L170
L280: open #2: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",i,i,k
	open #(h_csv:=fnH): "Name="&csvpath$&",REPLACE",d,o
	fnopenprn
	gosub HDR
L310: read #2,using L320: mat k,mat k$,mat l$,mat m eof L600
L320: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
	if m(2)=0 or k(1)=0 then goto L500
	deducy=deducq=0
	for j=1 to 10
		if deduc(j)=1 then deducy=deducy+m(j*2+9)
		if deduc(j)=1 then deducq=deducq+m(j*2+10)
	next j
	m(1)=m(1)-deducy
	m(2)=m(2)-deducq
	if p1<57 then goto L450
	gosub L820
	pr #255: newpage
	gosub HDR
L450: gosub L670
	t1=t1+m(2)
	t2=t2+h3
	t3=t3+h2
	t4=t4+m(34)
L500: goto L310
 
HDR: !
	pr #255,using L540: b$(2),b$(1),pedat$
L540: form skip 5,pos 2,c 12,pos 52,c 12,pos 67,c 6,skip 3
	pr #255,using L560: a$(1),a$(2),a$(3)
L560: form pos 22,c 40,skip 1,pos 22,c 40,skip 1,pos 22,c 40,skip 6
	p1=16
return
 
L600: gosub L820
	close #2:
	fncloseprn
	fnreg_write("TexasUCFile",csvpath$)
	close #h_csv:
	goto Xit
 
Xit: fnXit
 
L670: p3=p3+1
	if m(1)<ucm then goto L740
	if m(1)-m(2)>ucm then goto L720
	h2=ucm-(m(1)-m(2))
	goto L750
L720: h2=0
	goto L750
L740: h2=m(2)
L750: h3=m(2)-h2
	gosub L1000 ! break name down
	f$=first$(1:1): m$=mid$(1:1)
	pr #255,using L790: l$(1),f$,m$,last$,m(2)
	pr #h_csv: l$(1)&","""&srep$(first$,"""","""""")&""","&m$&","""&srep$(last$,"""","""""")&""","&str$(m(2))
L790: form pos 4,c 11,pos 21,c 1,pos 24,c 1,pos 27,c 16,pos 51,pic(zzz,zzz.##),skip 2
	p1=p1+2
return
L820: j1=68-p1
	pr #255,using L840: t1
L840: form skip j1,pos 47,pic(zzz,zzz,zzz.##)
	p3=0
	t1=0
	t2=0
	t3=0
	t4=0
return
 
include: ertn
 
L1000: dim first$*15,mid$*15,last$*20,em$(3)*30
	k$(1)=uprc$(rtrm$(k$(1))): ! nAMCDE$="s"
	x1=pos(k$(1)," ",1)
	x2=pos(k$(1)," ",x1+1)
	x3=pos(k$(1)," ",x2+1)
	if uprc$(namcde$)="L" then goto L1100
	first$=k$(1)(1:max(min(15,x1-1),1))
	if x2>0 then mid$=k$(1)(x1+1:x2-1): last$=k$(1)(x2+1:len(k$(1)))
	if x2=0 then last$=k$(1)(x1+1:len(k$(1))): mid$=""
	goto L1140
L1100: ! last name first
	if x1>0 and k$(1)(x1-1:x1-1)="," then last$=k$(1)(1:x1-2) else last$=k$(1)(1:max(x1-1,1))
	if x2>0 then first$=k$(1)(x1+1:x2-1): mid$=k$(1)(x2+1:len(k$(1)))
	if x2=0 then first$=k$(1)(x1+1:len(k$(1))): mid$=""
L1140: ! pr FIRST$,MID$,LAST$
return
