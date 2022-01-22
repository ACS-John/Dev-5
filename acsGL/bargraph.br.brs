! Replace S:\acsGL\bargraph
! pr bar graph of earnings by month for current year of prior year.
!
	autoLibrary
	on error goto Ertn
!
	dim acno$*12,bc(13),bp(13),wrd2$(2)*54,cap$*128,bud(13)
	dim resp$(10)*80,profit(12),txt$*80
	dim month(13), month$(13)*25,month$*25
!
	right=1 : center=2
	fnTop(program$,cap$="Print Bar Graph of Earnings")
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,outi,r
	read #20,using 'form pos 296,pos 384,n 2': lmu,nap : close #20:
!
	open #1: "Name=[Q]\GLmstr\Period.h[cno],Version=1,KFName=[Q]\GLmstr\Period-Idx.h[cno],Use,RecL=35,KPs=1,KLn=2,Shr",i,outIn,k
L170: read #1,using "form pos 1, n 2,c 25": month,month$ eof L210 noRec L210
	if month<1 or month>13 then goto L170
	month(month)=month
	month$(month)=month$
	goto L170
L210: close #1:
!
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",i,outIn,k
	open #11: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndx2.h[cno],Shr",i,outIn,k
	open #12: "Name=[Q]\GLmstr\BudgetInfo.h[cno],KFName=[Q]\GLmstr\BudIndx.h[cno],Use,RecL=28,KPs=1,KLn=14,Shr",i,outIn,k
SCR1: !
	t5=0
	fnTos(sn$='CloseYear3')
	lc=0 : mylen=20 : mypos=mylen+2 : width=50
	fnLbl(lc+=1,1,"Year to Print:",18,1)
	fnTxt(lc,mypos,4,0,1,'30',0,"You must choose the year to use printing a chart.")
	resp$(1)=""
	fnFra(lc+=1,1,2,45,"Current Year or Prior Year","Indicate if the information is to be pulled form the current files or prior files.",0)
	fnOpt(1,2,"Pull from Current",0,1)
	fnOpt(2,2,"Pull from Prior Year",0,1)
	fnLbl(lc+=4,1,"Enter the Last Retained Earnings Account",width,0)
	fnLbl(lc+=1,1,"or Equity Account:",width,0)
	fnQgl(lc,mypos)
	resp$(2)=""
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	year=val(resp$(1))
	if resp$(2)='True' then pullfrom$="Current"
	if resp$(3)='True' then pullfrom$="Prior"
	glnumber$=fnagl$(resp$(4))
	read #1,using L430,key=glnumber$: dno$,ano$,sno$ nokey SCR1
L430: form pos 1,c 3,c 6,c 3
	acno$=glnumber$(1:3)&"         "
!
	read #1,using L480,key>=glnumber$: acno$,bb,cb,mat bc,mat bp,mat bud nokey SCR1
L470: read #1,using L480: acno$,bb,cb,mat bc,mat bp, mat bud eof PRINT_CHART
L480: form pos 1,c 12,pos 81,41*pd 6.2
	if fnUseDeptNo=0 or dn1=1 then goto L510
	if glnumber$(1:3)><acno$(1:3) then goto PRINT_CHART ! may want to quit
L510: ! If ACNO$><GLNUMBER$ Then Goto 830
	for j=1 to 12
! check no further down in current year past last month closed  KJ need to do
		if pullfrom$="Current" then goto L550 else goto L590
L550: if j>lmu then goto L610 ! don't go past last month updated
		if j=1 then profit(j)+=bc(j): goto L610 ! first month
		profit(j)+=bc(j)-bc(j-1) ! 2nd thru 12 th months
		goto L610
L590: if j=1 then profit(j)+=bp(j): goto L610 ! first month prior year
		profit(j)+=bp(j)-bp(j-1) ! 2nd thru 12 th months
L610: next j
	goto L470
PRINT_CHART: !
	gosub VBOPENPRINT
! determine maximum height and depth
	for j=1 to 12
		if profit(j)>0 then maximumdepth=max(profit(j),maximumdepth) ! largest loss any one month either this year or last year
		if profit(j)<0 then maximumheight=min(profit(j),maximumheight) ! largest profit by month for either year  (profit is negative figure
	next j
! determine top line and bottom line
	if maximumheight<-1000000 then top=1000000 : x=10000: goto DETERMINE_BOTTOM_LINE
	if maximumheight>-50000 and maximumheight<-10000 then top=50000: x=500: goto DETERMINE_BOTTOM_LINE
	if maximumheight<-10000 then top=100000: x=1000: goto DETERMINE_BOTTOM_LINE
	if maximumheight<-1000 then top=10000: x=100: goto DETERMINE_BOTTOM_LINE
	if maximumheight<-100 then top=1000 : x=10: goto DETERMINE_BOTTOM_LINE
DETERMINE_BOTTOM_LINE: !
	if maximumdepth<1000 then bottom=-1000 : goto L800
	if maximumdepth<10000 then bottom=-10000 : goto L800
	if maximumdepth<100000 then bottom=-100000 : goto L800
L800: spacing=10 : lyne=10
	cnam=(len(trim$(env$('cnam')))/2)+50
	pr #20: 'Call Print.MyFontsize(14)'
	pr #20: 'Call Print.AddText("'&env$('cnam')&'",'&str$(cnam)&','&str$(1)&')'
	pr #20: 'Call Print.MyFontsize(12)'
	txt$="Earnings By Month "
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(80)&','&str$(5)&')'
	txt$="For the Year "&str$(year)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(80)&','&str$(10)&')'
	pr #20: 'Call Print.MyFontsize(9)'
! pr #20: 'Call Print.AddText("'&month$(1)&'",'&STR$(10)&','&STR$(11)&')'
	for j=1 to 12
		txt$=trim$(month$(j))(1:3)
		indent=8+(10*j)
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent)&','&str$(15)&')'
	next j
	txt$=cnvrt$("pic(--------)",top)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	for j=1 to 9
		txt$=cnvrt$("pic(--------)",top-((.10*j)*top))
		pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
		pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	next j
	goto L1140
	txt$=cnvrt$("pic(--------)",top*.80)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	txt$=cnvrt$("pic(--------)",top*.70)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	txt$=cnvrt$("pic(--------)",top*.60)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	txt$=cnvrt$("pic(--------)",top*.50)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	txt$=cnvrt$("pic(--------)",top*.40)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	txt$=cnvrt$("pic(--------)",top*.30)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	txt$=cnvrt$("pic(--------)",top*.20)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	txt$=cnvrt$("pic(--------)",top*.10)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
L1140: ! zero line starts right here
	for j=1 to 12
		txt$=trim$(month$(j))(1:3)
		indent=8+(10*j)
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent)&','&str$(120)&')'
	next j
	txt$=cnvrt$("pic(-------#)",0)
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	for j=1 to 10
		txt$=cnvrt$("pic(-------#)",-top*(.10*j))
		pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
		pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
	next j
	linezero=(spacing*10)+20
	column=18 ! spacing sideways
	pr #20: 'Call Print.MyFontBold(1)'
	for j=1 to 12
		homedot=linezero+(profit(j)/x)
! never could make the line chart work!! kj
! If PROFIT(J+1)<=0 Then lINELENGTH=SQR((10*10)+(ABS(PROFIT(J+1)/X)-LINEZERO+HOMEDOT)*(ABS(PROFIT(J+1)/X)-LINEZERO+HOMEDOT)) ! profit
! If PROFIT(J+1)>0 Then lINELENGTH=SQR((10*10)+(ABS(PROFIT(J+1)/X)+LINEZERO)*(ABS(PROFIT(J+1)/X)+LINEZERO)) ! loss
! nEXTDOT=LINEZERO+(PROFIT(J+1)/X)-HOMEDOT
! \If NEXTDOT=0 Then nEXTDOT=1
		if homedot<10 then homedot=10
		if homedot>220 then homedot=220
		pr #20: 'Call Print.AddLine('&str$(column-1)&','&str$(homedot)&','&str$(7)&','&str$(linezero-homedot)&',1)'
		for q=1 to 6
			pr #20: 'Call Print.AddLine('&str$(column-1+q)&','&str$(homedot)&','&str$(7-q)&','&str$(linezero-homedot)&',1)'
		next q
		pr #20: 'Call Print.MyFontsize(6)'
		if profit(j)<0 then txt$=cnvrt$("pic(--------#)",-round(profit(j),0)): pr #20: 'Call Print.AddText('&txt$&','&str$(column)&','&str$(homedot-3)&')'
		if profit(j)>0 then txt$=cnvrt$("pic(--------#)",-round(profit(j),0)): pr #20: 'Call Print.AddText('&txt$&','&str$(column)&','&str$(homedot+2)&')'
		pr #20: 'Call Print.MyFontsize(9)'
		column+=10
	next j
	for j=1 to nap
		txt$=trim$(month$(j))(1:3)
		indent=8+(10*j)
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent)&','&str$(230)&')'
	next j
	gosub RELEASE_PRINT
	close #1:
	goto Xit
!
Xit: fnXit
!
include: ertn
!
VBOPENPRINT: !
	if file(20)=-1 then
		open #20: "Name=[Q]\GLmstr\linechart"&wsid$&".txt,Replace,RecL=5000",d,o
		pr #20: 'Call Print.MyOrientation("Portrait")'
		lyne=margin ! starting of 1st line
		column1=16
		column2=103
		column3=153
	end if
return
RELEASE_PRINT: !
	fnpa_finis
return
