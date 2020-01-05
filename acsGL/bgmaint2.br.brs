! Replace S:\acsGL\BgMaint2
! Budget File (2nd screen of budget management system; range of gl #)
! r: setup
	library 'S:\Core\Library': fntop,fnxit,fnerror,fnopenprn,fncloseprn,fnTos,fnLbl,fnCmdKey,fnAcs,fncmbbud,fncomboa,fnTxt,fnCmdSet,fnrgl$,fnqgl,fnagl$,fnflexinit1,fnflexadd1,fnChk,fnmsgbox,fndat,fndate_mmddyy_to_ccyymmdd
	on error goto Ertn

	dim dat$*20,bg(6),bm(13),io1$(3),g1(3),aa(2),gld$*30,ln$*132
	dim chdr$(23)*20,cmask$(23)*20,option2$(6)*18,item$(23)*20,holdcmask$(23)*20
	dim name$*30,ml$(3)*80,maxan2(11)
	dim gd$*50,ex$*50,ios$(22),ins$(22),sc3$(9)*22,io3$(9),fl3$(9),k$(22)
	dim bk$(50)*12,t1(6),t(6),s(6),oldbg(6),type$(6)*20,fkey$(12)*30
	dim iom$(5),scm$(5)*40,io5$(2),options$(50)*100,resp$(100)*100
	dim io4$(25),an1$(11),an2(11),rdate$*40,bud$*50
	dim header$*244,line$*244,uline$*244,dline$*244,dline2$*244,uline2$*244
	dim btnfld01$(6),dosfld01$(6),btnfld03$(3),dosfld03$(3),item$(14)*50
	dim fa$(6),fb$(122),gl$(40)*12,bud$*50,gln(40,3)
	dim indexfile$*200
	dim exec$*132,prg$*20,cogl$(3)*12,pedat$*20,cch$*20,cdk$(22)
	dim iom2$(1),scm2$(1)*40,iom3$(2),scm3$(2)*40,btnfld02$(4),dosfld02$(4)
! /r
! r: initialize
	fntop(program$,"Budget Management")
	fndat(dat$,1)
	gosub GRID_SPECS
	for j=1 to 22: ios$(j)=str$(j+1)&",2,C 12,N" : next j
	type$(1)="Heading" 
	type$(2)="Budget item" 
	type$(3)="Subtotal" 
	type$(4)="Total" 
	type$(5)="Blank line" 
	type$(6)="New page"
	an2(1)=40
	for j=2 to 7 : an2(j)=12 : next j ! defaults for column widths
	mat maxan2=an2
	an2(8)=40 : an2(9)=12 : an2(10)=12: an2(11)=12
	open #company=1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input  
	read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub 
	close #1: 
	open #1: "Name=[Q]\GLmstr\GLmstr.H[cno],KFName=[Q]\GLmstr\GLIndex.H[cno],Shr",internal,outIn,keyed 
! format:  budget #^bud^n 2,budget name^bud$^c 50,range of g/l #^gl$(40) (1 to 2, 3 to 4, etc)
	open #5: "Name=[Q]\GLmstr\BudInfo.H[cno],use,KFName=[Q]\GLmstr\BudInfo_Index.H[cno],RecL=532,KPs=1,KLn=2",internal,outIn,keyed 
	read #5,using 'Form N 2,C 50,40*C 12': bud,bud$,mat gl$ noRec MAINTAIN_RANGE_FILE, eof MAINTAIN_RANGE_FILE
	open #12: "Name=[Q]\GLmstr\BudgetInfo.h[cno],KFName=[Q]\GLmstr\BudIndx.h[cno],Use,RecL=28,KPs=1,KLn=14,Shr",internal,outIn,keyed 
! ______________________________________________________________________
	open #2: "Name=[Q]\GLmstr\Budget"&str$(bud)&".H[cno],KFName=[Q]\GLmstr\BgIndx"&str$(bud)&".H[cno],Shr",internal,outIn,keyed ioerr MENU1
! ______________________________________________________________________
	mat ml$(2) 
	ml$(1)="Budget Management is a separately licensed product. " 
	ml$(2)="Contact your representative or ACS to license the product. " 
	fnmsgbox(mat ml$,resp$,'',0)
	goto XIT
L470: gosub DATE_SCREEN
! /r
! ______________________________________________________________________
MENU1: ! r:
	fnTos(sn$="Budget_File") 
	respc=0
	fnLbl(1,1,"Budget File #:",14,right)
	execute "Dir [Q]\GLmstr\budget*.H[cno] >"&env$('temp')&"\FlexWork.tmp" ioerr L530
	L530: j=0
	open #13: "Name="&env$('temp')&"\FlexWork.tmp",display,input ioerr L690
	L550: linput #13: ln$ eof L680
	ln$=uprc$(ln$)
	x=pos(ln$,"KILOBYTES",1)
	if x>0 then goto L680
	x=pos(ln$(1:3),"DIR",1)
	if x>0 or ln$(1:1)="." then goto L550 else goto L610
	L610: x=pos(ln$,".",1)
	bud=val(ln$(x-1:x-1)) conv L670
	bud=val(ln$(x-2:x-1)) conv L640
	L640: k$=lpad$(str$(bud),2) 
	read #5,using L650,key=k$: k$,bud$,mat gl$ nokey L670
L650: form c 2,c 50,40*c 12
	options$(j+=1)=str$(bud)&" "&bud$
L670: goto L550
L680: close #13: 
L690: if j<=0 then j=1
	mat options$(j)
	fen$="CBud.h[cno]"
	fncomboa(fen$,1,16,mat options$,"Select from the list of budget files. To add a new budget file, take the Add option.",40,container)
! fnCMBBUD(INDEXFILE$)
	if hact$="" then 
		resp$(respc+=1)="" 
	else 
		resp$(respc+=1)=hact$
	end if
	fnCmdKey("&Display",4,1,0,"Display the budget files for this budget.") 
	fnCmdKey("&Add",1,0,0,"Add a new budget file" ) 
	fnCmdKey("E&dit",2,0,0,"Edit the highlited record") 
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	fnAcs(sn$,0,mat resp$,ckey) ! ask budget file #
	ad1=0
	hg1$=g1$=resp$(1)(1:12)
	bud=val(resp$(1)(1:2))
	if ckey=1 then 
		ad1=1 : goto MAINTAIN_RANGE_FILE 
	else if ckey=2 then 
		goto MAINTAIN_RANGE_FILE 
	else if ckey=4 then 
		goto ASK_BEGINNING_ITEM 
	else if ckey=5 then 
		goto XIT 
	end if
goto MENU1 ! /r
! ______________________________________________________________________
DATE_SCREEN: ! r:
fd1=0101*100+val(date$(1:2))
fd2=1231*100+val(date$(1:2))
fnTos(sn$="bgmaint2") 
respc=0 : right=1
fnLbl(1,47," ",1,1)
fnLbl(1,1,"Beginning Day Of Year:",30,1)
fnTxt(1,34,12,0,0,"3",0,"") 
resp$(respc+=1)=str$(fd1)
fnLbl(2,1,"Ending Day of Year:",30,1)
fnTxt(2,34,12,0,0,"3",0,"") 
resp$(respc+=1)=str$(fd2)
fnCmdSet(2): fnAcs(sn$,0,mat resp$,ck)
if ck=5 then goto MENU1
fd1=val(resp$(1)) ! beginning of year
fd2=val(resp$(2)) ! ending day of year
return ! /r
CREATE_NEW_FILE: ! r:
close #2: ioerr L1020
open #2: "Name=[Q]\GLmstr\BUDGET"&str$(bud)&".H[cno],KFName=[Q]\GLmstr\BGINDX"&str$(bud)&".H[cno],Shr",internal,outIn,keyed ioerr L1000
goto L1020
L1000: mat ml$(2) 
ml$(1)="You already have a budget file # "&str$(bud) ! " 
ml$(2)="Take YES to continue, else NO to retain the old file." 
fnmsgbox(mat ml$,resp$,'',52)
if resp$="Yes" then goto L1020 else goto MENU1
L1020: open #2: "Name=[Q]\GLmstr\Budget"&str$(bud)&".H[cno],Replace,KFName=[Q]\GLmstr\BgIndx"&str$(bud)&".H[cno],RecL=149,KPS=1/149,KLN=12/1",internal,outIn,keyed 
close #2: ioerr L1040
L1040: open #2: "Name=[Q]\GLmstr\Budget"&str$(bud)&".H[cno],use,KFName=[Q]\GLmstr\BgIndx"&str$(bud)&".H[cno],RecL=149,KPS=1/149,KLN=12/1",internal,outIn,keyed 
return ! /r
READD_TOTALS: ! r:
! from general ledger __________________________________________________
g1$=""
restore #1,search>="": nokey END1
L1100: read #1,using 'Form POS 1,C 12,C 50,POS 87,PD 6.2,POS 249,13*PD 6.2': g1$,gd$,cb,mat bm eof END1
for j=1 to 40 step 2
	if g1$>=gl$(j) and g1$<=gl$(j+1) then goto L1150
next j
goto L1100
L1150: mat bg=(0) : ex$="" : cd$="B"
! If TI1=5 Then Goto 1170
read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&cd$: g1$,mat bg,gd$,ex$,cd$ nokey L1210
bg(1)=sum(bm) : bg(2)=cb : bg(3)=0 
bg(5)=bg(1)+bg(4) ! NEW BUDGET = BUDGET FROM FILE + CHANGES
rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&cd$: g1$,mat bg,gd$,ex$,cd$ nokey L1220
goto L1100
L1210: bg(1)=sum(bm) : bg(2)=cb : bg(3)=0 
bg(5)=bg(1)+bg(4) : gd$=gd$ : cd$="B"
L1220: write #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
goto L1100
! ______________________________________________________________________
END1: ! end on g/l
cd$="X": mat bg=(0): gd$=ex$="" ! write one blank line at end of file
g1$="999999999999"
! Write #2,Using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': G1$,MAT BG,GD$,EX$,CD$    why was it doing this  ??? kj
close #2: 
execute "Index [Q]\GLmstr\Budget"&str$(bud)&".H[cno],[Q]\GLmstr\BGINDX"&str$(bud)&".H[cno],1/149,12/1,Replace,DupKeys -n"
open #2: "Name=[Q]\GLmstr\Budget"&str$(bud)&".H[cno],KFName=[Q]\GLmstr\BgIndx"&str$(bud)&".H[cno],Shr",internal,outIn,keyed 
! holding files in gl___________________________________________________
execute "DIR [Q]\GLmstr\GL*.H[cno] >"&env$('temp')&"\Work."&session$
open #3: "Name="&env$('temp')&"\Work."&session$,display,input 
L1350: linput #3: ln$ eof L1530
ln$=uprc$(ln$)
if ln$(1:2)><"GL" then goto L1350
d1=val(ln$(3:8)) conv L1350
if fndate_mmddyy_to_ccyymmdd(d1)<fd1 or fndate_mmddyy_to_ccyymmdd(d1)>fd2 then goto L1350
open #4: "Name=[Q]\GLmstr\"&ln$(1:8)&".H[cno]",internal,input 
L1410: read #4,using L1420: g1$,d1,amt,tcde eof L1500
L1420: form pos 1,c 12,n 6,pd 6.2,n 2
if g1$(3:3)=" " then g1$(3:3)="0"
if g1$(12:12)=" " then g1$(12:12)="0"
read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$ nokey L1410
bg(2)=bg(2)+amt
rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$
goto L1410
! ______________________________________________________________________
L1500: close #4: 
goto L1350
! from unpaid invoice file _____________________________________________
L1530: close #3: ioerr L1540
L1540: close #4: ioerr L1550
L1550: open #3: "Name=[Q]\CLmstr\PAYTRANS.H[cno],Shr",internal,input,relative ioerr L1740
open #4: "Name=[Q]\CLmstr\UnPdAloc.H[cno],KFName=[Q]\CLmstr\Uaidx2.H[cno],Shr",internal,outIn,keyed 
for j=1 to lrec(3)
	read #3,using L1590,rec=j: vn$,iv$,d1,gde noRec L1720
L1590: form pos 1,c 8,c 12,pos 27,n 6,pos 96,n 1
	if gde>0 then goto L1720 ! ALREADY POSTED TO GL AS A/P
	if fndate_mmddyy_to_ccyymmdd(d1)<fd1 or fndate_mmddyy_to_ccyymmdd(d1)>fd2 then goto L1720
! Read #4,Using 1600,Rec=AA: G1$,AMT,NAA
	restore #4,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey L1720
L1640: read #4,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,g1$,amt,gd$ eof L1720
	if vn$=hvn$ and iv$<>hiv$ then goto L1640
	if vn$<>hvn$ or iv$<>hiv$ then goto L1720
	form pos 21,c 12,pd 5.2,x 30,pd 3
	read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$ nokey L1710
	bg(3)=bg(3)+amt
	rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$
L1710: goto L1640
L1720: next j
! from check history (checks not posted)____________________________
L1740: close #3: ioerr L1750
L1750: close #4: ioerr L1760
L1760: open #3: "Name=[Q]\CLmstr\TRMSTR.H[cno],KFName=[Q]\CLmstr\TRIDX1.H[cno],Shr",internal,input,keyed ioerr L1930
open #4: "Name=[Q]\CLmstr\TRALLOC.H[cno],Shr",internal,input,relative 
L1780: read #3,using L1790: bcde,tcde,iv$,d1,pcde,scd eof L1930
L1790: form pos 1,n 2,n 1,c 8,pos 12,g 6,pos 71,n 1,x 6,n 1
if pcde=1 or pcde=3 then goto L1780
if d1<fd1 or d1>fd2 then goto L1780
key$=lpad$(str$(bcde),2)&str$(tcde)&rpad$(iv$,8) 
restore #4,key>=key$: nokey L1780
L1830: read #4,using 'Form Pos 1,C 11,c 12,pd 5.2,pos 65,pd 3,pos 80,n 1': newkey$,g1$,amt,naa,gde
if newkey$<>key$ then goto L1780
read #4,using L1860,rec=aa: g1$,amt,naa,gde noRec L1780
L1860: form pos 12,c 12,pd 5.2,pos 65,pd 3,pos 80,n 1
if gde>0 then goto L1910
read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$ nokey L1910
if tcde=2 or tcde=3 then bg(3)=bg(3)-amt else bg(3)=bg(3)+amt
rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$
L1910: goto L1830
! from purchase order file_________________________________________
L1930: close #3: ioerr L1940
L1940: close #4: ioerr L1950
L1950: open #3: "Name=[Q]\POmstr\POmstr.H[cno],KFName=[Q]\POmstr\POMSIDX.H[cno],Shr",internal,input,keyed ioerr L2080
open #4: "Name=[Q]\POmstr\POTRANS.H[cno],Shr",internal,input,relative 
L1970: read #3,using L1980: d1,mat aa eof L2080
L1980: form pos 10,n 6,pos 161,2*pd 3
if fndate_mmddyy_to_ccyymmdd(d1)<fd1 or fndate_mmddyy_to_ccyymmdd(d1)>fd2 then goto L1970
aa=aa(1)
L2010: if aa=0 then goto L1970
read #4,using L2030,rec=aa: pt1,pt2,pt3,g1$,naa
L2030: form pos 52,pd 3,pd 5.2,pd 3,pos 91,c 12,pos 103,pd 3
read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$ nokey L2070
bg(3)=bg(3)+(pt1-pt3)*pt2
rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$
L2070: aa=naa : goto L2010
L2080: !
close #3: ioerr ignore
close #4: ioerr ignore
close #2: ioerr ignore
open #2: "Name=[Q]\GLmstr\Budget"&str$(bud)&".H[cno],KFName=[Q]\GLmstr\BgIndx"&str$(bud)&".H[cno],Shr",internal,outIn,keyed 
do
	read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$ eof READD_SUB_TOTALS
	bg(5)=bg(1)+bg(4) ! update new balance for any changes
	rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
loop
READD_SUB_TOTALS: ! 
mat t=(0): mat s=(0)
restore #2: 
L2190: read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$ eof L2240
if cd$="B" then mat t=t+bg: mat s=s+bg
if cd$="S" then 
	mat bg=s: mat s=(0)
	rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
end if
if cd$="T" then 
	mat bg=t: mat t=(0) 
	rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
end if
goto L2190
L2240: restore #2: 
goto DISPLAY_GRID ! /r (end of updating balances)
! ______________________________________________________________________
ASK_BEGINNING_ITEM: ! r:
goto DISPLAY_GRID 
! r: skip this screen
fnTos(sn$="bgmaint3") 
respc=0
fnLbl(1,1,"Beginning General Ledger #:",30,1)
fnqgl(1,33) 
resp$(1)=fnrgl$(gl$(1))
fnCmdKey("&Next",1,1,0,"Display budget file starting with this account. ") 
fnCmdKey("&Back",2,0,0,"Takes you back one screen.") 
fnCmdKey("E&xit",5,0,1,"Returns to main menu")
fnAcs(sn$,0,mat resp$,ckey) ! ask general ledger #
if ckey=5 then goto MENU1
g1$=startgl$=fnagl$(resp$(1))
restore #2,key>=g1$&"B": nokey L2370
L2370: if ckey=2 then goto MENU1
bk=0 ! /r
DISPLAY_GRID: ! r:
restore #2: 
x=13
if trim$(startgl$)<>"" then restore #2,key>=startgl$&"B": nokey L2430
L2430: fnTos(sn$="Bgmaint4") 
respc=0
fnLbl(1,1,bud$,50,right) ! move cmdkeys down
frame=0
fnflexinit1('bgmaintgrid',2,1,20,95,mat chdr$,mat cmask$,1,0)
READ_BUDGET_GRID: ! read budget items
read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$ eof EO_BUDGET_GRID noRec L2650
if needactual=0 and needbudget=0 then goto L2610
read #1,using "form pos 13,c 30,pos 237,pd 6.2",key=g1$: name$,prioryear nokey L2610
restore #12,search>=g1$&"  ": nokey L2610 eof L2610
L2520: read #12,using "form pos 1,c 12,c 2,2*pd 6.2": acno$,oldyr$,cb,oldbud eof L2610
if acno$<>g1$ then goto L2610
for j=1 to 5
	if val(oldyr$)=year(j) and needactual=1 and needbudget=1 then item$(x+j*2-1)=str$(cb) ! balance side of showing both
	if val(oldyr$)=year(j) and needbudget=1 and needactual=1 then item$(x+j*2)=str$(oldbud) ! budget side of showing both
	if val(oldyr$)=year(j) and needbudget=1 and needactual=0 then item$(x+j)=str$(oldbud) ! only budget
	if val(oldyr$)=year(j) and needbudget=0 and needactual=1 then item$(x+j)=str$(oldbud) ! only actual
next j
goto L2520
L2610: if cd$="A" or cd$="X" then mat cmask$=(""): cmask$(1)="30": : mat item$=(""): item$(1)=str$(rec(2)): item$(2)=gd$: item$(10)=g1$: goto L2640
mat cmask$=holdcmask$
item$(1)=str$(rec(2)) 
item$(10)=g1$ 
item$(3)=str$(bg(1)): item$(4)=str$(bg(2)) 
item$(5)=str$(bg(3)) : item$(6)=str$(bg(4)) : item$(7)=str$(bg(1)-bg(2)-bg(3)-bg(4)) 
item$(8)=str$(bg(5)) 
item$(9)=str$(bg(6)) : item$(2)=gd$(1:30) : item$(11)=ex$ 
item$(12)=cd$ 
item$(13)=str$(prioryear)
L2640: fnflexadd1(mat item$)
L2650: goto READ_BUDGET_GRID
EO_BUDGET_GRID: ! /r
fnLbl(22,40," ",mylen,right) ! move cmdkeys down
fnCmdKey("&Add",1,0,0,"Allows you to add new lines to the budget file.")
fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing budget record.")
fnCmdKey("&Calculate New Balance",6,0,0,"Will re-calculate new balances if any changes have been made to general ledger or checkbook.")
fnCmdKey("&Pull More History",9,0,0,"Pull expenditures and budgets from prior years.")
fnCmdKey("&Listing",3,0,0,"Prints listings of the budget")
fnCmdKey("&Delete",7,0,0,"Deletes this line item")
fnCmdKey("&Back",8,0,0,"Reselect starting budget item to display.")
fnCmdKey("E&xit",5,0,1,"Exits back to main budget screen.")
fnAcs(sn$,0,mat resp$,ck) ! GRID
add=edit=0
if ck=5 then goto MENU1
if ck=2 then edit=1
if ck=3 then goto PRNT1 ! pr listings of unpaid invoice file
if ck=1 then 
	add=1: insrt=1: jb1=1 
	g1$=k$(max(1,curfld-1)): mat bg=(0): gd$=ex$=cd$="" 
	mat resp$=(""): goto MAINTAIN_NONB_RECORDS
end if
if ck=2 then 
	rec2=val(resp$(1)) : read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',rec=rec2,release: g1$,mat bg,gd$,ex$,cd$ 
	mat resp$=("") 
	holdkey$=g1$&cd$ 
else 
	holdkey$=''
end if
if ck=2 then 
	resp$(1)=g1$: resp$(2)=str$(bg(1)): resp$(3)=str$(bg(2)) 
	resp$(4)=str$(bg(3)): resp$(5)=str$(bg(4)) : resp$(6)=str$(bg(5)) 
	resp$(7)=gd$: resp$(8)=ex$ 
	resp$(9)=cd$ 
	goto MAINTAIN_LINE_ITEM
end if
if ck=6 then goto INCLUDE_CHANGES
if ck=7 then goto L2860 else goto L2890
L2860: mat ml$(2) 
ml$(1)="You have chosen to delete the highlighted budget item." 
ml$(2)="Take YES to continue, else NO to retain the record." 
fnmsgbox(mat ml$,resp$,'',52)
if resp$="Yes" then goto L2880 else goto DISPLAY_GRID
L2880: delete #2,rec=val(resp$(1)): 
restore #2: 
goto DISPLAY_GRID
L2890: if ck=8 then goto MENU1
if ck=9 then goto ASK_ABOUT_HISTORY
goto DISPLAY_GRID
MAINTAIN_NONB_RECORDS: ! 
if add=1 then g1$=gd$=cd$=""
fnTos(sn$="Add_line_item") 
mylen=23: mypos=mylen+3 : right=1: rc=0
if use_dept =1 then let fnLbl(1,26,"Fund #",6,2)
if use_sub =1 then let fnLbl(1,40,"Sub #",6,2)
fnLbl(2,1,"General Ledger Number:",mylen,right)
if use_dept=1 then 
	let fnTxt(2,26,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",0 ) 
	resp$(rc+=1)=g1$(1:3)
end if
fnTxt(2,31,6,0,right,"30",0,"Enter the main part of the general ledger number.",0 ) 
resp$(rc+=1)=g1$(4:9)
if use_sub=1 then 
	fnTxt(2,40,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",0 ) 
	resp$(rc+=1)=g1$(10:12)
end if
fnLbl(3,1,"Description:",mylen,right)
fnTxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 ) 
resp$(rc+=1)=gd$
fnLbl(4,1,"Type of Entry:",mylen,right)
option2$(1)="A "&type$(1) 
option2$(2)="B "&type$(2) 
option2$(3)="S "&type$(3) : option2$(4)="T "&type$(4)
option2$(5)="X "&type$(5) : option2$(6)="Z "&type$(6)
fncomboa("Types",4,mypos,mat option2$,"Select the type of entry.",20,container)
resp$(rc+=1)=cd$
fnCmdSet(2)
fnAcs(sn$,0,mat resp$,ckey)
if ckey=5 then goto DISPLAY_GRID
dno=ano=sno=0
if use_dept=1 then dno=val(resp$(1)) : ano=val(resp$(2))
if use_dept=0 then ano=val(resp$(1))
if use_dept=1 and use_sub=1 then sno=val(resp$(3))
if use_dept=0 and use_sub=1 then sno=val(resp$(2))
if use_dept=1 and use_sub=1 then gd$=resp$(4): cd$=resp$(5)(1:1)
if use_dept=0 and use_sub=1 then gd$=resp$(3): cd$=resp$(4)(1:1)
if use_dept=0 and use_sub=0 then gd$=resp$(2): cd$=resp$(3)(1:1)
if use_dept=1 and use_sub=0 then gd$=resp$(3): cd$=resp$(4)(1:1)
key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)&"B"
read #2,using 'Form POS 1,c 13',key=key$: oldkey$ nokey L3240
MSGBOX1: ! 
mat ml$(3) 
ml$(1)="General ledger account # "&key$&" already " 
ml$(2)="exists. Take OK to add a different account." 
ml$(3)="Take Cancel to return to main screen." 
fnmsgbox(mat ml$,resp$,'',49)
if resp$="OK" then goto MAINTAIN_NONB_RECORDS else goto MENU1
L3240: if add=1 then goto WRITE_NEW_RECORD else goto REWRITE_EXISTING_RECORD
WRITE_NEW_RECORD: ! 
mat bg=(0) : ex$="": edit=1 
g1$=key$(1:12)
write_new_record
write #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
if cd$="B" then goto MAINTAIN_LINE_ITEM else goto DISPLAY_GRID
REWRITE_EXISTING_RECORD: ! 
rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',rec=rec2: g1$,mat bg,gd$,ex$,cd$
goto DISPLAY_GRID ! /r
MAINTAIN_LINE_ITEM: ! r:
if cd$="B" then goto L3350 else goto MAINTAIN_NONB_RECORDS
L3350: holdg1$=g1$: holdcd$=cd$
fnTos(sn$="Budget_Edit") 
respc=0: mylen=20: mypos=mylen+3
fnLbl(1,1,"Account #:",mylen,right)
fnqgl(1,mypos) 
resp$(respc+=1)=fnrgl$(g1$)
fnLbl(2,1,"Budget:",mylen,right)
fnTxt(2,mypos,12,0,0,"10",0,"Approved budget at the beginning of the year") 
resp$(respc+=1)=str$(bg(1))
fnLbl(3,1,"Actual Amount:",mylen,right)
fnTxt(3,mypos,12,0,0,"10",0,"Actual expenditures or receiptsfor the year.") 
resp$(respc+=1)=str$(bg(2))
fnLbl(4,1,"Unpaid Expenses:",mylen,right)
fnTxt(4,mypos,12,0,0,"10",0,"Accounts payable, etc.") 
resp$(respc+=1)=str$(bg(3))
fnLbl(5,1,"Changes:",mylen,right)
fnTxt(5,mypos,12,0,0,"10",0,"Changes for the year.") 
resp$(respc+=1)=str$(bg(4))
! If TI3=2 Then rEMAINING= BG(1)-BG(2)-BG(3) Else rEMAINING=BG(1)-BG(2)-BG(3)+BG(4)
! fnLbl(6,1,"Remaining:",MYLEN,RIGHT)
! fnTxt(6,MYPOS,12,0,0,"10",0,"Balance remaining on the budget.") 
! rESP$(RESPC+=1)=STR$(REMAINING)
fnLbl(6,1,"New Budget:",mylen,right)
fnTxt(6,mypos,12,0,0,"10",0,"New Budget for this year.") 
resp$(respc+=1)=str$(bg(5))
fnLbl(8,1,"Description:",mylen,right)
fnTxt(8,mypos,50,0,0,"",0,"Description of Line Item.") 
resp$(respc+=1)=gd$
fnLbl(10,1,"Reason for Change:",mylen,right)
fnTxt(10,mypos,50,0,0,"",0,"Brief reason for change in budget.") 
resp$(respc+=1)=ex$
fnLbl(12,1,"Type of Entry:",mylen,right)
if cd$="A" or trim$(cd$)="" then 
	option2$(1)="A "&type$(1) 
	option2$(2)="B "&type$(2) 
	option2$(3)="S "&type$(3) : option2$(4)="T "&type$(4)
	option2$(5)="X "&type$(5) : option2$(6)="Z "&type$(6)
end if
if cd$="B" then 
	option2$(1)="B "&type$(2) 
	option2$(2)="A "&type$(1) 
	option2$(3)="S "&type$(3) : option2$(4)="T "&type$(4)
	option2$(5)="X "&type$(5) : option2$(6)="Z "&type$(6)
end if
if cd$="S" then 
	option2$(1)="S "&type$(3) 
	option2$(2)="A "&type$(1) 
	option2$(3)="B "&type$(2) : option2$(4)="T "&type$(4)
	option2$(5)="X "&type$(5) : option2$(6)="z "&type$(6)
end if
if cd$="T" then 
	option2$(4)="T "&type$(4) 
	option2$(2)="A "&type$(1) 
	option2$(3)="B "&type$(2) : option2$(4)="S "&type$(3)
	option2$(5)="X "&type$(5) : option2$(6)="Z "&type$(6)
end if
if cd$="X" then 
	option2$(1)="X "&type$(5) 
	option2$(2)="A "&type$(1) 
	option2$(3)="B "&type$(2) : option2$(4)="S "&type$(3)
	option2$(5)="T "&type$(4) : option2$(6)="Z "&type$(6)
end if
if cd$="Z" then 
	option2$(1)="Z "&type$(6) : option2$(2)="A "&type$(1) 
	option2$(3)="B "&type$(2) : option2$(4)="S "&type$(3)
	option2$(5)="T "&type$(4) : option2$(6)="X "&type$(5)
end if
resp$(respc+=1)=option2$(1)
fncomboa("Types",12,mypos,mat option2$,"Select the type of entry.",20,container)
fnCmdKey("&Save",1,1,0,"Save any changes." ) 
fnCmdKey("E&xit",5,0,1,"Exit without saving any changes.")
fnAcs(sn$,0,mat resp$,ck) ! EDIT SCREEN
if ckey=5 then goto MENU1
g1$=fnagl$(resp$(1))
bg(1)=val(resp$(2))
bg(2)=val(resp$(3))
bg(3)=val(resp$(4))
bg(4)=val(resp$(5))
bg(5)=val(resp$(6))
gd$=resp$(7)
ex$=resp$(8)
for j=1 to 6
	if resp$(9)(1:1)=option2$(j)(1:1) then cd$=option2$(j)(1:1)
next j
remaining=bg(1)-bg(2)-bg(3)-bg(4) ! remaining balance
bg(5)=bg(1)+bg(4) ! make new budget equal the old budget plus changes
if holdg1$<>g1$ then read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&cd$: oldg1$ nokey L3850
goto L3850
mat ml$(2) 
ml$(1)="You are attempting to change the record number to  an existing" 
ml$(2)="record number.  Take OK to change to a different number. " 
fnmsgbox(mat ml$,resp$,'',0)
goto MAINTAIN_LINE_ITEM
L3850: if add=1 then goto L3880
rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',rec=rec2: g1$,mat bg,gd$,ex$,cd$
goto L3900
L3880: write #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
add=0
L3900: restore #2: 
goto DISPLAY_GRID ! /r
! ______________________________________________________________________
PRNT1: ! r: John's pr Routine ... yeah, i made this mess
header$="" : opr=255 : ps$="############" 
screen=cp=page=bob1=bob2=bob3=bob4=lyne=0
uline2$=rpt$("_",244) : dline2$=rpt$("=",244)
fnTos(sn$="bgprint") 
respc=0 : right=1 : mylen=25 : mypos=mylen+3
fnLbl(lyne+=1,1,"Check the columns your want prirted",38,1)
fnChk(lyne+=2,mypos,"GL Description:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"Budget Amount:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"Paid / Received:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"Unpaid Expenses:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"Changed Amount:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"New Budget:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"Next Years Budget:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"Reason for Change:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"Budget Remaining:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"General Ledger Number:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnChk(lyne+=1,mypos,"% of Budget Used:",1) 
resp$(respc+=1)="False"
fnTxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") 
resp$(respc+=1)=str$(an2(lyne-2))
fnLbl(lyne+=1,1,"Report Heading Date:",mylen,1)
fnTxt(lyne,mypos,30,0,0,"",0,"Date you want printed in heading of the report.") 
resp$(respc+=1)=dat$
fnCmdKey("&Next",1,1,0,"Display ") 
fnCmdKey("E&xit",5,0,1,"Returns to main menu")
fnAcs(sn$,0,mat resp$,ck) ! pr setup
if ck=5 then goto DISPLAY_GRID
if resp$(1)="True" then 	an1$(1)="Y" : an2(1)=val(resp$(2))
if resp$(3) ="True" then an1$(2)= "Y" : an2(2)=val( resp$(4))
if resp$(5) ="True" then an1$(3)= "Y" : an2(3)=val( resp$(6))
if resp$(7) ="True" then an1$(4)= "Y" : an2(4)=val( resp$(8))
if resp$(9) ="True" then an1$(5)= "Y" : an2(5)=val(resp$(10))
if resp$(11)="True" then an1$(6)= "Y" : an2(6)=val(resp$(12))
if resp$(13)="True" then an1$(7)= "Y" : an2(7)=val(resp$(14))
if resp$(15)="True" then an1$(8)= "Y" : an2(8)=val(resp$(16))
if resp$(17)="True" then an1$(9)= "Y" : an2(9)=val(resp$(18))
if resp$(19)="True" then an1$(10)="Y": an2(10)=val(resp$(20))
if resp$(21)="True" then an1$(11)="Y": an2(11)=val(resp$(22))
rdate$=resp$(23)
for j=1 to 11
	if an2(1)>maxan2(j) then 
		mat ml$(2) 
		ml$(1)="You have selected a column width longer than the maximum allowed." 
		ml$(2)="Select a smaller width." 
		fnmsgbox(mat ml$,resp$,'',0) 
		goto PRNT1
	end if
next j
fnopenprn
restore #2,search>="": nokey MENU1 ioerr L4420
goto L4430
L4420: !
open #2: "Name=[Q]\GLmstr\BUDGET"&str$(bud)&".H[cno],KFName=[Q]\GLmstr\BGINDX"&str$(bud)&".H[cno],Shr",internal,outIn,keyed ioerr MENU1
L4430: if an1$(1)<>"Y" then goto L4470
header$="GL Description                            "(1:an2(1))&"  "
uline$="                                          "(1:an2(1)+2)
dline$="                                          "(1:an2(1)+2)
L4470: !
if an1$(2) ="Y" then header$=header$&"  Budget Amt  "(1:an2(2))&"  "
if an1$(3) ="Y" then header$=header$&" Expenses Pd  "(1:an2(3))&"  "
if an1$(4) ="Y" then header$=header$&" Exp. Unpaid  "(1:an2(4))&"  "
if an1$(5) ="Y" then header$=header$&" Changed Amt  "(1:an2(5))&"  "
if an1$(6) ="Y" then header$=header$&"  New Budget"(1:an2(6))&"  "
if an1$(7) ="Y" then header$=header$&"Nxt Yrs Bdgt"(1:an2(7))&"  "
if an1$(8) ="Y" then header$=header$&"Reason for Change                       "(1:an2(8))&"  "
if an1$(9) ="Y" then header$=header$&" Bdgt Remain  "(1:an2(9))&"  "
if an1$(10)="Y" then header$=header$&"   GL Number  "(1:an2(10))&"  "
if an1$(11)="Y" then header$=header$&" % of Budget "(1:an2(11))&"  "
for j=2 to 11
	if j=8 and an1$(8)="Y" then dline$=dline$&rpt$(" ",42)(1:an2(8)+2) : uline$=uline$&rpt$(" ",42)(1:an2(8)+2) : goto L4620
	if j=10 and an1$(10)="Y" then dline$=dline$&"            "(1:an2(10)+2) : uline$=uline$&"            "(1:an2(10)+2) : goto L4620
	if an1$(j)="Y" then dline$=dline$&dline2$(1:an2(j))&"  "
	if an1$(j)="Y" then uline$=uline$&uline2$(1:an2(j))&"  "
	L4620: !
next j
lhdr=len(header$)
bob1=lhdr-11
bob2=int((lhdr-len(rtrm$(env$('cnam'))))/2)
bob3=int((lhdr-len(rtrm$(rdate$)))/2)
bob4=int((lhdr-len(rtrm$(bud$)))/2)
restore #2,search>="": nokey MENU1 ioerr L4420
gosub HDR
L4700: ! r:
read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$ eof L4970
L4710: form pos 1,c lhdr,skip 1,pos 1,c lhdr,skip 1
line$=""
if an1$(1)="Y" then line$=line$&gd$(1:an2(1))&"  "
for j=2 to 7
	if an1$(j)="Y" and len(ltrm$(cnvrt$("N 12.2",(bg(j-1)))))>an2(j) then line$=line$&ps$(13-an2(j):12)&"  ": goto L4770
	if an1$(j)="Y" then line$=line$&cnvrt$("N 12.2",bg(j-1))(13-an2(j):12)&"  "
L4770: next j
if an1$(8)="Y" then line$=line$&ex$(1:an2(8))&"  "
if ti3=2 then goto L4830
if an1$(9)="Y" and len(ltrm$(cnvrt$("N 12.2",bg(1)-bg(2)-bg(3)+bg(4))))>an2(9) then line$=line$&ps$(13-an2(9):12)&"  ": goto L4850
if an1$(9)="Y" then line$=line$&cnvrt$("N 12.2",bg(1)-bg(2)-bg(3)+bg(4))(13-an2(9):12)&"  "
goto L4850
L4830: if an1$(9)="Y" and len(ltrm$(cnvrt$("N 12.2",bg(1)-bg(2)-bg(3))))>an2(9) then line$=line$&ps$(13-an2(9):12)&"  ": goto L4850
if an1$(9)="Y" then line$=line$&cnvrt$("N 12.2",bg(1)-bg(2)-bg(3))(13-an2(9):12)&"  "
L4850: if an1$(10)="Y" and cd$<>"T" and cd$<>"H" then line$=line$&g1$(1:an2(10)) ! THIS WONT WORK IF YOU ADD ANYTHING AFTER IT!!!
if an1$(10)="Y" and cd$="T" then line$=line$&"                                        "(1:an2(10))
if bg(1)<>0 then pused=(bg(2)+bg(3))/bg(1) else pused=0
if an1$(11)="Y" then line$=line$&cnvrt$("N 12",pused*100)(13-an2(11):12)&"  "
bobtom=len(line$)
if cd$="A" and an1$(1)="Y" then pr #255,using L5120: gd$ pageoflow NWPGE
if cd$="B" then pr #255,using L5130: line$(1:len(line$)) pageoflow NWPGE
if cd$="S" then pr #255,using L5140: uline$(1:len(line$)),line$,sline$(1:len(line$)) pageoflow NWPGE
if cd$="T" then pr #255,using L5150: uline$(1:len(line$)),line$,dline$(1:len(line$)) pageoflow NWPGE
if cd$="X" then pr #255,using L5120: "" pageoflow NWPGE
if cd$="Z" then pr #255: newpage : gosub HDR
goto L4700 ! /r
L4970: ! r:
fncloseprn
goto DISPLAY_GRID ! /r
! ______________________________________________________________________
NWPGE: ! r:
pr #255: newpage
gosub HDR
continue ! /r
! ______________________________________________________________________
HDR: ! r:
page=page+1
pr #255,using L5080: "Page: ",page,env$('cnam'),rdate$,bud$
L5080: form pos bob1,c 6,n 4,skip 1,pos bob2,c 40,skip 1,pos bob3,c 40,skip 1,pos bob4,c 50,skip 2
pr #255,using L4710: header$,uline2$(1:lhdr-2)
return ! /r
! ______________________________________________________________________
L5120: form pos 1,c 80,skip 1
L5130: form pos 1,c bobtom,skip 1
L5140: form pos 1,c bobtom,skip 1,pos 1,c bobtom,skip 1,pos 1,c bobtom,skip 1
L5150: form pos 1,c bobtom,skip 1,pos 1,c bobtom,skip 1,pos 1,c bobtom,skip 2
! ______________________________________________________________________
MAINTAIN_RANGE_FILE: ! r:
if ad1=1 then bud=0: bud$="": mat gl$=("")
fnTos(sn$="bgmaintrange") 
respc=0 : mylen=20 : mypos=20: mypos2=60: lyne=3
fnLbl(1,1,"Budget #:",mylen,right)
fnTxt(1,mylen+3,2,0,0,"30",0,"") 
resp$(respc+=1)=str$(bud)
fnLbl(2,1,"Description:",14,right)
fnTxt(2,mylen+3,50,0,0,"",0,"Enter your budget file name. Enter the range of general ledger numbers below to be included in this budget.") 
resp$(respc+=1)=bud$
fnLbl(3,30,"Range From:",mylen,0)
fnLbl(3,70,"Range To:",mylen,0)
for j=1 to 40 step 2
	fnqgl(lyne+=1,mypos,0,2) 
	resp$(respc+=1)=fnrgl$(gl$(j))
	fnqgl(lyne,mypos2,0,2) 
	resp$(respc+=1)=fnrgl$(gl$(j+1))
next j
fnCmdKey("&Complete",1,1,0,"Saves the changes and and builds the new file from the general ledger.") 
!
fnCmdKey("&Delete",6,0,0,"Deletes this complete budget file.") 
fnCmdKey("&Cancel",5,0,1,"Returns to main budget file menu.")
fnAcs(sn$,0,mat resp$,ck) ! gl breakdown screen
if ck=5 then goto MENU1
bud=val(resp$(1))
bud$=resp$(2)
k$=lpad$(str$(bud),2)
for j=1 to 40
	gl$(j)=fnagl$(resp$(j+2))
next j
if add=1 then goto L5470
if ck=6 then goto L5410 else goto L5450
L5410: mat ml$(2) 
ml$(1)="You have chosen to delete the budget file. Click Yes " 
ml$(2)="to continue, else No to retain the file." 
fnmsgbox(mat ml$,resp$,'',52)
if resp$="Yes" then goto L5430 else goto L5510
L5430: delete #5,key=lpad$(str$(bud),2): 
goto L5510
L5450: rewrite #5,using L5490,key=k$: bud,bud$,mat gl$ nokey L5470
goto L5490
L5470: gosub CREATE_NEW_FILE
write #5,using L5490: bud,bud$,mat gl$
L5490: form pos 1,n 2,c 50,40*c 12
! mat GL$=("  0     0  0")
L5510: goto READD_TOTALS ! /r
! ______________________________________________________________________
XIT: fnxit
! ______________________________________________________________________
INCLUDE_CHANGES: ! r:
fnTos(sn$="Include_Changes") 
respc=0: mylen=40: mypos=mylen+3 : lyne=0
fnChk(lyne+=1,mypos,"Include Changes in Remaining Balance:",1) 
resp$(respc+=1)="True"
fnCmdKey("&Next",1,1,0,"Continue with re-calculations. ") 
fnCmdKey("E&xit",5,0,1,"Returns to main menu")
fnAcs(sn$,0,mat resp$,ck) ! include changes if remaining balance
if ck=5 then goto MENU1
if resp$(1)="True" then ti3=1 else ti3=2
chg=1
goto READD_TOTALS ! /r
! ______________________________________________________________________
ASK_ABOUT_HISTORY: ! r:
fnTos(sn$="Ask_about") 
respc=0: mylen=40: mypos=mylen+3 : lyne=0
fnChk(lyne+=1,mypos,"Include Actual Receitps and Expenditures:",1) 
resp$(respc+=1)="True"
fnChk(lyne+=1,mypos,"Include Budget Amounts:",1) 
resp$(respc+=1)="True"
fnLbl(lyne+=1,1,"Years to Review:",mylen,1)
fnTxt(lyne,mypos,2,0,0,"30",0,"Year code in YY format.") 
resp$(respc+=1)=""
for j=1 to 4
	fnTxt(lyne+=1,mypos,2,0,0,"30",0,"Year code in YY format.") 
	resp$(respc+=1)=""
next j
fnCmdKey("&Next",1,1,0,"Display ") 
fnCmdKey("E&xit",5,0,1,"Returns to main menu")
fnAcs(sn$,0,mat resp$,ck) ! ask prior years
if ck=5 then goto MENU1
if resp$(1)="True" then needactual=1 else needactual=0
if resp$(2)="True" then needbudget=1 else needbudget=0
totalextra=0
for j=1 to 5
	year(j)=val(resp$(j+2))
	if year(j)>0 and needactual=1 then totalextra+=1
	if year(j)>0 and needbudget=1 then totalextra+=1
next j
! Gosub GRID_SPECS
goto DISPLAY_GRID ! /r
GRID_SPECS: ! r:
	mat chdr$(13+totalextra) : mat cmask$(13+totalextra) : mat item$(13+totalextra): mat holdcmask$(13+totalextra) 
	chdr$(1)="Rec" : chdr$(2)="Description" : chdr$(3)="Budget" 
	chdr$(4)="Actual" : chdr$(5)="Unpaid" 
	chdr$(6)="Changes" 
	chdr$(7)="Remaining" : chdr$(8)="New Budget" 
	chdr$(9)="Next Years Budget" 
	chdr$(10)="GL #"
	chdr$(11)="Reason" 
	chdr$(12)="TOE" 
	chdr$(13)="Prior Amount"
	cmask$(1)='' : cmask$(2)='10' : cmask$(3)="10" 
	cmask$(4)='10' 
	cmask$(5)='10' : cmask$(6)='10': cmask$(7)='10' 
	cmask$(8)='10': cmask$(9)='10' : cmask$(10)='' 
	cmask$(11)='': cmask$(12)='' : cmask$(13)='10'
	x=13 ! add extra columns for old history
	for j=1 to 5
		if needactual=1 and year(j)>0 then chdr$(x+=1)="Amount-"&cnvrt$("pic(##)",year(j)) 
			cmask$(x)="10"
		if needbudget=1 and year(j)>0 then chdr$(x+=1)="Budget-"&cnvrt$("pic(##)",year(j)) 
			cmask$(x)="10"
	next j
	x=0
	mat holdcmask$=cmask$
return ! /r
include: ertn
