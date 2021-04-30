! Prtflex2 ! DO NOT RENUMBER
! Replace S:\Core\PrtFlex\PrtFlex2
! r: libraries
	autoLibrary
! /r
	on error goto Ertn
! r: dims
	dim programfolder$*60,datafolder$*256,gridname$*40
	dim name$*30,colmask$*3,tt$*200,colhdr$(80)*30,colmask$(80)*3
	dim response$(87)*80,text$*40, lastgridresponse$*87,resp$(10)*60
	dim options$(300)*87,ln$*132,item$(80)*80,abbrev$*20,open_read$*80,tg(11)
	dim z$*8,rp$*3,py$(8)*25,ss$*11,pb$(10)*2,pl$(5)*13,pf$(7)*6,pa$(8)*1
	dim pm(40),adr(2)
	dim dg$(6)*6,pc$(5)*5,pcd(5),oc$(5)*2,ocd(5),va$(5)*2,vaa(5)
	dim df$*1,dr$*9,dc$*2,da$*17,extra(23),extra$(11)*30,item$(80)*50
	dim abbrev$*20,open_read$*80,tg(11)
	dim saddr$*40,scity$*20,sstate$*2,szip$*11,msgnum$*12,maddr$*39,mcity$*20,mstate$*2,mzip$*11,atime$*8,crn$*9,dtl$*8,name$(3)*25,ss$*11,race$*18,sex$*1
	dim tg(11),p$*10
	dim ck1$*1,ck2$*1,ck3$*1,ck4$*1,ck5$*1,ck5d$*60,amt(7),amt2(5),cksa$*1,eshome$*1,esstreet$*30,weather$*1,sign$*1,signhelp$*30,witname$*30
	dim ifnot$*80,comment$*80,worker$*20,amt3(12),hes$(10)*10
	dim chk4b1$*1,status$*10,payee$(3)*5,payamt(3),chk4c1$*1,chk4c2$*1
	dim vod$*80,chk4d1$*1,chk4d2$*1,chk4d3$*1,chk4d4$*1,existingss$*12
	dim chk4d5$*1,chk4d6$*1,chk4d7$*1,chk4d8$*1,otherspec$*40,chk4d9$*1
	dim heap$*1,chk4d21$*1,comment2$*150,votime$*8,holdname$(3)*30
! /r
	fnTop(program$,"Print Flex")
	programfolder$=env$('cursys')&"mstr"
	datafolder$='[Q]\'&env$('cursys')&"mstr"
	dataext$='.h[cno]'
	columns=1
! r: OPENFILES: The following lines will be proc in from a display file                          you have created. They are in the same file as the read                         statements explained above.  Don't forget the del lines to
!               remove the old reads in case they dont match
	open #1: "name="&datafolder$&"\ubtransvb.h[cno],kfname="&datafolder$&"\ubtrindx.h[cno],Use,RecL=102,KPs=1,KLn=19",internal,input,keyed
! /r
	open #11: "Name=[Temp]\Gridname.tmp",internal,input,relative
	read #11,using 'Form POS 1,C 40',rec=1: gridname$
	close #11:
	if env$('cursys')='UB' and rln(1)=102 then gosub ASKTRANSET
PRINTGRID: ! r: Prints the grid
	mat item$(columns)
	mat item$=("")
	mat colhdr$(columns)
	mat colhdr$=("")
	mat colmask$(columns)
	mat colmask$=("")
	fnTos
	fnLbl(1,1,uprc$(gridname$),20,2,3)
	gosub GRIDHEADING ! reads the headings that were created above
	fnflexinit1("flexprint",3,1,10,70,mat colhdr$,mat colmask$,1)
! Restore #1:
READ_NEXT: gosub READDATAFILES ! reads the database for the grid information                                     These read statements and form statements must
!                     be in a dispaly file in the data base folder with                               a "_read" on the end of the file name.  The file                                name must be the same as the database name + _read
	if trim$(c$)<>"[All]" and trim$(c$)<>trim$(p$) then goto READ_NEXT
	if beg_date<>0 and beg_date>tdate then goto READ_NEXT
	if end_date<>0 and end_date<tdate then goto READ_NEXT
	if sel_code=2 and tcode<>1 then goto READ_NEXT
	gosub GRIDDETAILS ! Assign the variable names to                                each column
	fnflexadd1(mat item$)
	goto READ_NEXT
	EOFONREAD: ! Complete the grid once all data has been read
	! fnLbl(15,1,"Export the grid to a fixed width file, for later use.")
	fnCmdSet(52)
	fnAcs(mat response$,ckey) ! CALL items selected
	lastgridresponse$=response$(1)
	if ckey=5 then chain "S:\Core\prtflex\Grids",programfolder$,datafolder$
! fnXit(CURSYS$)
! /r
 
READDATAFILES: !  r: These read statements will be contained in a display                            file that matches the data base name plus _info
L9010: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	read #1,using L9010: p$,tdate,tcode,tamount,mat tg,we,wu,er,eu,gr,gu,tbal,pcode eof EOFONREAD
	return  ! /r
GRIDHEADING: ! r: The following lines will be generated each time a grid is                        printed.  Don't ever renumber this program unless you are                       prepared to spend some time figuring out where lines are!
	colhdr$(1)="Name" : colmask$(1)="80"
	return  ! /r
GRIDDETAILS: ! r: The following lines are generated lines.  They will be                          removed and added back just before each grid is printed
	item$(1)=e$(2)
	return  ! /r
DONE: close #1: ioerr ignore
Xit: fnXit
 
ASKTRANSET: ! r:
	transtype$(1)="Charge"
	transtype$(2)="Penalty"
	transtype$(3)="Collection"
	transtype$(4)="Credit Memo"
	transtype$(5)="Debit Memo"
	fnTos
	rc=cf=0
	fnFra(1,1,6,23,"Transaction Type","You can review all transactions or any specific type of transaction",0)
	cf+=1 : fratype=cf
	fnOpt(1,3,"[All]",0,fratype)
	if sel_code=1 or sel_code=0 then
		resp$(rc+=1)="True"
	else
		resp$(rc+=1)="False"
	end if
	fnOpt(2,3,"Charges",0,fratype)
	if sel_code=2 then
		resp$(rc+=1)="True"
	else
		resp$(rc+=1)="False"
	end if
	fnOpt(3,3,"Penalties",0,fratype)
	if sel_code=3 then
		resp$(rc+=1)="True"
	else
		resp$(rc+=1)="False"
	end if
	fnOpt(4,3,"Collections",0,fratype)
	if sel_code=4 then
		resp$(rc+=1)="True"
	else
		resp$(rc+=1)="False"
	end if
	fnOpt(5,3,"Credit Memos",0,fratype)
	if sel_code=5 then
		resp$(rc+=1)="True"
	else
		resp$(rc+=1)="False"
	end if
	fnOpt(6,3,"Debit Memos",0,fratype)
	if sel_code=6 then
		resp$(rc+=1)="True"
	else
		resp$(rc+=1)="False"
	end if
	fnFra(1,30,3,42,"Date Range","You can transactions for any date range or leave these blank to see all transactions.")
	cf+=1 : fradate=cf : mylen=26 : mypos=mylen+2
	fnLbl(1,1,"Starting Date:",mylen,1,0,fradate)
	fnTxt(1,mypos,10,0,1,"3",0,empty$,fradate)
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,fradate)
	fnTxt(2,mypos,10,0,1,"3",0,empty$,fradate)
	resp$(rc+=1)=str$(end_date)
	fnFra(6,30,2,60,"Account","You review transactions for all accounts or for an individual.")
	cf+=1 : fraaccount=cf
	fnLbl(1,1,"Account:",8,1,0,fraaccount)
	fncmbact(1,10,1,fraaccount)
	rc+=1
	if trim$(hact$)<>"" then
		resp$(rc)=hact$
	else if resp$(rc)="" then
		resp$(rc)="[All]"
	end if
	fnCmdKey("&Display",1,1,0,"Displays a list of transactions on the screen")
	fnCmdKey("&Cancel",5,0,1,"Returns to customer record")
	ckey=fnAcs(mat resp$)
	if ckey=cancel then goto Xit_ASKTRANSET
	if resp$(1)="True" then
		sel_code=1
	else if resp$(2)="True" then
		sel_code=2
	else if resp$(3)="True" then
		sel_code=3
	else if resp$(4)="True" then
		sel_code=4
	else if resp$(5)="True" then
		sel_code=5
	else if resp$(6)="True" then
		sel_code=6
	end if
	beg_date=val(resp$(7))
	end_date=val(resp$(8))
	c$=resp$(9)(1:10)
XIT_ASKTRANSET: return  ! /r
include: ertn
