autoLibrary
on error goto Ertn
! r: dims
	dim response$(87)*80
	dim resp$(10)*60
	dim tg(11)
	dim p$*10
! /r
fnTop(program$,"Print Flex")
dim programfolder$*256
programfolder$='[cursys]mstr'
dim datafolder$*256
datafolder$='[Q]\[cursys]mstr'
dataext$='.h[cno]'
columns=1
! r: OPENFILES: The following lines will be proc in from a display file                          you have created. They are in the same file as the read                         statements explained above.  Don't forget the del lines to
!               remove the old reads in case they dont match
	open #1: "name="&datafolder$&"\ubtransvb.h[cno],kfname="&datafolder$&"\ubtrindx.h[cno],Use,RecL=102,KPs=1,KLn=19",i,i,k
! /r
	open #11: "Name=[Temp]\Gridname.tmp",i,i,r
	dim gridname$*40
	read #11,using 'Form POS 1,C 40',rec=1: gridname$
	close #11:
	if env$('cursys')='UB' and rln(1)=102 then gosub ASKTRANSET
PRINTGRID: ! r: Prints the grid
	dim item$(0)*50
	mat item$(columns)
	mat item$=("")
	dim colhdr$(0)*30
	mat colhdr$(columns)
	mat colhdr$=("")
	dim colmask$(0)*3
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
	dim lastgridresponse$*87
	lastgridresponse$=response$(1)
	if ckey=5 then chain "S:\Core\prtflex\Grids",programfolder$,datafolder$
! fnXit
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
