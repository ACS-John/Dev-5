! pr bar graph of eye improvement
! ______________________________________________________________________Designated SPEC file does not exist.
	library 'S:\Core\Library': fntop,fnxit, fnTos,fnLbl,fnCmdSet,fnAcs,fnTxt, fnOpt,fnFra,fncomboa
	library 'S:\Core\Library': fnmsgbox,fndate_mmddyy_to_ccyymmdd,fnpa_finis
	fntop(program$,"Bar Graph")
	on error goto ERTN
! ______________________________________________________________________
	dim acno$*12,bc(13),bp(13),wrd2$(2)*54,bud(13)
	dim month(13), month$(24)*25,month$*25,actualdate$(24)
	dim cd1(24),rw(8,13),e$*30,u1(24),u2(24),u3(24,13),message$*60
	dim n2(24),n3(24,13),resp$(27),txt$*80
	dim serviceName$(10)*20
	dim msgline$(2)*40,tg(11),opt$(3)*20
	dim srv$(10)*2,dollars(24)
	right=1 : center=2

! 
	magicdate=fndate_mmddyy_to_ccyymmdd(d1)-20000 ! don't start with anything older that two years ago
	goto SCREEN1
	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed 
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
L260: read #1,using L1050,release: z$,e$,bildat eof SCREEN1
	if bildat<>d1 then goto L260 ! current customer
	restore #2,key>=z$&"         ": nokey L260
L290: read #2,using L1100: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof SCREEN1
	if p$<>z$ then goto L360           ! history record must belong to this customer
	if tcode<>1 then goto L290 ! charge transaction
	if tdate<magicdate then goto L290
	j=j+1 
	if j>24 then goto SCREEN1
	resp$(j)=str$(tdate)
	goto L290
L360: if resp$(12)="" then goto L260 ! try another customer
! ______________________________________________________________________
SCREEN1: ! 
! add dates here kj
	mat resp$=("")
	resp$(1)="20110112"
	resp$(2)="20110126"
	resp$(3)="20110131"
	resp$(4)="20110201"
	resp$(5)="20110202"
	resp$(6)="20110207"
	resp$(7)="20110209"
	resp$(8)="20110215"
	fnTos(sn$="ubbargraph") 
	rc=0
	fnLbl(1,1,"Billing dates to be used:",35,1)
	fnTxt(2,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(2,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(2,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(2,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(2,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(2,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(4,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(4,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(4,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(4,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(4,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(4,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(6,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(6,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(6,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(6,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(6,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(6,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(8,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(8,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(8,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(8,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(8,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnTxt(8,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") 
	rc+=1 : resp$(rc)=resp$(rc)
	fnLbl(10,1,"Service to Analyze:",24,1,0)
	opt$(1)="Water"
	if srv$(3)="EL" then opt$(2)= serviceName$(3)
	if srv$(4)="GA" then opt$(3)= serviceName$(4)
	fncomboa("ubbargraph",10,26,mat opt$,"",13) 
	rc+=1 : resp$(rc)=opt$(1)
	fnFra(12,1,2,45,"Base graph on usage or dollars","You can either analyze dollars or usage.",0) 
	fnOpt(1,2,"Use Usage",0,1) 
	resp$(rc+=1)="True" 
	fnOpt(2,2,"Use Dollars",0,1)
	fnCmdSet(2) 
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT
	for j=1 to 24
L800: x=pos(resp$(j),"/",1) 
		if x>0 then resp$(j)(x:x)="": goto L800
		cd1(j)=val(resp$(j)) conv MSGBOX
		y=val(resp$(j)(5:6))
		if y=1 then month$(j)="Jan"
		if y=2 then month$(j)="Feb"
		if y=3 then month$(j)="Mar"
		if y=4 then month$(j)="Apr"
		if y=5 then month$(j)="May"
		if y=6 then month$(j)="Jun"
		if y=7 then month$(j)="Jul"
		if y=8 then month$(j)="Aug"
		if y=9 then month$(j)="Sep"
		if y=10 then month$(j)="Oct"
		if y=11 then month$(j)="Nov"
		if y=12 then month$(j)="Dec"
	next j
	if cd1(1)=0 then goto MSGBOX
	if resp$(25)="Water" then codepos=143: service=1: opt=1
	if resp$(25)=trim$(opt$(2)) then codepos=147: service=3: opt=2
	if resp$(25)=trim$(opt$(3)) then codepos=149 : service=4 : opt=3
	if resp$(26)="True" then baseon=1 else baseon =2 ! 1=usage  2=dollars
	for j=1 to 24
		actualdate$(j)=resp$(j)
	next j
	goto SETDOLLARS
L1040: read #1,using L1050: z$,e$,servicecode eof STORE_GRAPH_INFO
L1050: form pos 1,c 10,x 30,c 30,pos 296,pd 4
	restore #2,key>=z$&"         ": nokey L1040
L1070: read #2,using L1100: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof STORE_GRAPH_INFO
	if p$<>z$ then goto L1040 ! history record must belong to this customer
	if tcode<>1 then goto L1070 ! charge transactions only
L1100: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
! ______________________________________________________________________
	if service=1 and baseon=1 then usage=wu ! analyzing water
	if service=1 and baseon=2 then usage=tg(1) ! analyzing water dollars
	if service=3 and baseon=1 then usage=eu ! analyzing electric
	if service=3 and baseon=2 then usage=tg(3) ! analyzing electric dollars
	if service=4 and baseon=1 then usage=gu ! analyzing gas
	if service=4 and baseon=2 then usage=tg(4) ! analyzing gas dollars
	for j=1 to 24
		if cd1(j)><tdate then goto L1210
		n2(j)=n2(j)+1 
		u1(j)=u1(j)+usage 
		u2(j)=u2(j)+usage
L1210: next j
	goto L1070 ! read next transaction
STORE_GRAPH_INFO: ! 
SETDOLLARS: ! for J=1 To 24  set calculated vision kj
	dollars(1)=100 ! first day tested
	dollars(2)=93 ! first time checked 1-10
	dollars(3)=88 ! 1/26
	dollars(4)=85 ! 2/1/11
	dollars(5)=82 ! 2/2/11
	dollars(6)=77 ! 2/3/11
	dollars(7)=71 ! 2/9/11
	dollars(8)=57 ! 2/15/11
! dOLLARS(J)=U1(J)
! Next J
PRINT_CHART: ! 
	gosub VBOPENPRINT
! determine maximum height and depth
	for j=1 to 24
		if dollars(j)>0 then maximumheight=max(dollars(j),maximumheight) ! largest dollars by month for either year  (dollars is negative figure
	next j
! determine top line and bottom line
	if baseon=1 then top$=str$(maximumheight): toplen=len(top$): top=toplen*10
	if baseon=2 then top$=str$(round(maximumheight,0)): toplen=len(top$): top=toplen*10
	toplen$=str$(val(top$(1:1))+1)
	for j=1 to toplen-1
		toplen$=toplen$&str$(0)
	next j
	top=val(toplen$) : top=100 ! change top here kj
	x=top*.10
DETERMINE_BOTTOM_LINE: ! 
	spacing=10 : lyne=30
	cnam=(len(trim$(env$('program_caption')))/2)+120
	pr #20: 'Call Print.MyFontsize(14)'
	txt$=env$('program_caption')
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(cnam)&','&str$(10)&')'
	pr #20: 'Call Print.MyFontsize(12)'
	txt$="Eye Chart"
	servicetype=(len(trim$(txt$))/2)+120
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(servicetype)&','&str$(16)&')'
	if baseon=1 then txt$=""
	if baseon=2 then txt$=trim$(opt$(opt))
	servicetype=(len(trim$(txt$))/2)+140
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(servicetype)&','&str$(20)&')'
	pr #20: 'Call Print.MyFontsize(9)'
	txt$=cnvrt$("pic(--------)",top) 
	pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
	pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',240,0)' ! left,up/down,lenght of top line on chart
	for j=1 to 10 ! wording down side
		txt$=cnvrt$("pic(-------#)",top-((.10*j)*top)) 
		pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
		pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',240,0)'
	next j
! zero line starts right here
	linezero=(spacing*10)+40
	column=18 ! spacing sideways
	pr #20: 'Call Print.MyFontBold(1)'
	for j=1 to 24
		homedot=140-((dollars(j)/top)*100)
		if homedot<0 then homedot=0
		if homedot>140 then homedot=140
		pr #20: 'Call Print.AddLine('&str$(column-1)&','&str$(homedot)&','&str$(7)&','&str$(linezero-homedot)&',1)'
		for q=1 to 6
			pr #20: 'Call Print.AddLine('&str$(column-1+q)&','&str$(homedot)&','&str$(7-q)&','&str$(linezero-homedot)&',1)'
		next q
		pr #20: 'Call Print.MyFontsize(6)'
		if dollars(j)>0 then txt$=cnvrt$("pic(--------#)",round(dollars(j),0)): pr #20: 'Call Print.AddText('&txt$&','&str$(column-2)&','&str$(homedot-2)&')'
		pr #20: 'Call Print.MyFontsize(9)'
		column+=10
	next j
	pr #20: 'Call Print.MyFontBold(0)'
	for j=1 to 24 ! month wording at bottom of page
		txt$=trim$(month$(j))(1:3) 
		indent=8+(10*j) 
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent)&','&str$(linezero+5)&')'
		txt$=actualdate$(j)(7:8)
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent+1)&','&str$(linezero+8)&')'
		txt$=actualdate$(j)(3:4)
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent+1)&','&str$(linezero+11)&')'
	next j
	gosub RELEASE_PRINT
	close #1: 
	goto XIT
! ______________________________________________________________________
XIT: fnxit
! ______________________________________________________________________
VBOPENPRINT: ! r:
	if file(20)=-1 then 
		open #20: "Name=[Q]\UBmstr\linechart"&wsid$&".txt,Replace,RecL=5000",display,output 
		pr #20: 'Call Print.MyOrientation("Landscape")'
		lyne=margin ! starting of 1st line
		column1=16
		column2=103
		column3=153
	end if 
return  ! /r
RELEASE_PRINT: ! r:
	fnpa_finis
return  ! /r
MSGBOX: ! 
	msgline$(1)="You have entered dates in an"
	msgline$(2)="invalid format.  Use mmddyy format."
	fnmsgbox(mat msgline$,resp$,'',1)
goto SCREEN1
include: ertn
