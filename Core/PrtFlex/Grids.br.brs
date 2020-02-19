! Replace S:\Core\PrtFlex\prtflex1
!
	library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnTos
	library 'S:\Core\Library': fncomboa
	library 'S:\Core\Library': fnflexadd1,fnflexinit1,fnCmdKey
	library 'S:\Core\Library': fntop,fngetdir2,fnCopy,fnfree
	library 'S:\Core\Library': fnxit
	library 'S:\Core\Library': fngetdir,fnCmdSet
	on error goto Ertn
	fntop(program$)
! r: dims
	dim gridname$*40,gridindx$*40,filename$*60
	dim resp$(87)*80,message$*40
	dim ln$*132
	dim fullgridname$*60,fullgridindx$*60
	dim gridinfo$(5)*87,item$(80)*30
	dim name$*30,vname$*20,colmask$*3,tt$*200
	dim colhdr$(80)*30,colmask$(80)*3
	dim open_read$*80,abbrev$*20,filename$(10)*40
! /r
	dim datafolder$*256
	datafolder$='[Q]\[cursys]mstr' ! data folder grid folder is for use and updating by customer
	dim programfolder$*256
	programfolder$=os_filename$('S:\acs'&env$('cursys')) ! program grid folder (and sub-folder and files) are for distribution.  files and folders only distribute if they are missing.  makes updating them difficult.
! r: make any missing folders in the data directory
	if ~exists(datafolder$&"\Grid") then 
		execute 'MkDir "'&datafolder$&'\Grid"'
	end if 
	dim tmp_directory_list$(1)*256
	fngetdir2(programfolder$&'\Grid\',mat tmp_directory_list$, '/ad')
	for tmp_directory_list_item=1 to udim(mat tmp_directory_list$)
		if ~exists(datafolder$&'\Grid\'&tmp_directory_list$(tmp_directory_list_item)) then 
			execute 'MkDir "'&datafolder$&"\Grid\"&tmp_directory_list$(tmp_directory_list_item)&'"'
		end if 
	next tmp_directory_list_item
! /r
! r: copy any missing files to the data folder
	dim tmp_file_list$(1)*256
	fngetdir2(programfolder$&'\Grid\',mat tmp_file_list$, '/s /b','*.*')
	for tmp_file_list_item=1 to udim(mat tmp_file_list$)
!       pr '1'; tmp_file_list$(tmp_file_list_item)
		tmp_file_list$(tmp_file_list_item)(1:len(programfolder$))='' ! remove the program folder prefix from it - keep \Grid\...
!      pr '2'; tmp_file_list$(tmp_file_list_item)
		if ~exists(datafolder$&tmp_file_list$(tmp_file_list_item)) then 
!      pr '3'; 'copy "'&programfolder$&tmp_file_list$(tmp_file_list_item)&'" "'&datafolder$&tmp_file_list$(tmp_file_list_item)&'"' : pause
			execute 'copy "'&programfolder$&tmp_file_list$(tmp_file_list_item)&'" "'&datafolder$&tmp_file_list$(tmp_file_list_item)&'"'
		end if 
	next tmp_file_list_item
! /r
SelectDataBase: ! 
!  allows you to search the grid folder for any subfolders
! (You must create a sub-folder for each data base you can
!  access)
	dim database_list$(1)*256
	fngetdir2(datafolder$&'\Grid\',mat database_list$)
! currently you cannot have more than 8 character database names
! error 4323 avoided
! database folders must be created by the programmer before
! you can run this program
! database folders must be under the grid folder.
	fnTos
	mat resp$=("")
	fnLbl(1,1,"Data Base Name:",16,1)
	fnLbl(2,1,"Current System: "&env$('cursys'))
	fncomboa("OTHER",1,18,mat database_list$,empty$,20)
	resp$(1)=database_list$(1)
	fnCmdSet(2)
	fnAcs('',0,mat resp$,ckey)
	if ckey=5 then goto XIT
	database$=resp$(1)
!
GRIDSELECTION: ! r:
! Allows you to select any grid that previously been created
! or allows you to create a new one.  All grids end with
!    .grd and will automatically be placed in database file
! that is selected at the time
	mat resp$=("")
	dim gridname_list$(300)*87
	fngetdir2(datafolder$&"\Grid\"&database$ ,mat gridname_list$, '','*.grd') ! ,mat gd2_date$,mat gd2_time$,gd2_full_path)
	for gridname_list_item=1 to udim(mat gridname_list$) ! remove all the .grd extensions
		gridname_list$(gridname_list_item)=gridname_list$(gridname_list_item)(1:len(gridname_list$(gridname_list_item))-4)
	next gridname_list_item
	fnTos
	fnLbl(1,1,"Flexgrid name:",16,1)
	fncomboa("GridNames",1,18,mat gridname_list$,"Choose a grid or click add to add a new grid",20)
	resp$(1)=gridname_list$(1)
	fnCmdKey("&Select",1,1)
! fnCmdKey("&Back",2)
	fnCmdKey("&Add",3)
	fnCmdKey("&Delete",4)
	fnCmdKey("&Cancel",5,0,1)
	fnAcs('',0,mat resp$,ckey)
	resp$(1)=trim$(resp$(1)(1:20))
	if ckey=5 then goto SelectDataBase
	if ckey=3 then goto AddGridName
	! if ckey=2 or trim$(resp$(1))="" then goto SelectDataBase ! back or no grid name
	L780: !
	fullgridname$=datafolder$&"\Grid\"&database$&"\"&resp$(1)&".grd"
	fullgridindx$=datafolder$&"\Grid\"&database$&"\"&resp$(1)&".idx"
	gridname$=resp$(1)
	open_read$=datafolder$&"\Grid\"&database$&"\"&database$&"_info"
	if ckey=4 then 
		close #hgridfile: ioerr ignore
		if fnFree(fullgridname$)<=0 then goto SelectDataBase
		fnFree(fullgridindx$)
		goto SelectDataBase
	end if 
	close #1: ioerr ignore
	if ckey=1 then goto GridColumns
! If CKEY=4 Then Goto DisplayGrid
	if ckey=5 then goto XIT
	goto SelectDataBase ! /r
!
GridColumns: !  r: Displays all vaiables in the data base and allows you to                        choose the ones you want in your grid
	mat item$(2)
	close #hgridfile: ioerr ignore
	open #hgridfile:=15: "Name="&fullgridname$&",KFName="&fullgridindx$&",RecL=80,KPs=1,KLn=3,use",internal,outIn,keyed ioerr SelectDataBase
	fnTos
	fnLbl(1,1,uprc$(gridname$),20,2,3)
	mat colhdr$(2)
	colhdr$(1)="Column #"
	colhdr$(2)="Description"
	mat colmask$(2)
	colmask$(1)="30"
	colmask$(2)=""
	filename$="flexreview"
	fnflexinit1(filename$,2,1,10,72,mat colhdr$,mat colmask$,1)
	if lrec(hgridfile)=0 then goto DisplayGrid
	do 
		read #hgridfile,using fGridFile: columnnum,name$,vname$,fieldlen,colmask$,abbrev$ eof L1050
		item$(1)=str$(columnnum)
		item$(2)=name$
		fGridFile: form pos 1,n 3,c 30,c 20,n 4,c 3,c 20
		fnflexadd1(mat item$)
	loop 
	L1050: !
	fnCmdKey("&Add Column",1,1)
	fnCmdKey("&Delete Column",2)
	fnCmdKey("Display &Grid",3)
	fnCmdKey("&Back",4)
	fnCmdKey("&Cancel",5,0,1)
	fnAcs('',0,mat resp$,ckey) ! CALL items selected
	if ckey=5 then goto XIT
	if ckey=1 then goto DisplayGrid
	if ckey=3 then goto PrintGrid
	if ckey=4 then goto SelectDataBase ! select a different grid
	if ckey=2 then 
		deletekey$=cnvrt$("n 3",val(resp$(1)))
		delete #hgridfile,key=deletekey$: ioerr GridColumns
	end if 
	if ckey<>2 then goto SelectDataBase
	restore #hgridfile: : newcolumn=0
	do
		read #hgridfile,using fGridFile: columnnum eof GridColumns
		newcolumn=newcolumn+1
		rewrite #hgridfile,using fGridFile: newcolumn
	loop
DisplayGrid: ! 
	restore #hgridfile: ! determine next available column number
	do
		read #hgridfile,using fGridFile: lastcolumn eof L1250
	loop
	L1250: !
	fnTos
	respc=0
	fnLbl(1,1,"Data Base File:",20,1)
	fnTxt(1,22,20,20,0,"",1)
	gridinfo$(respc+=1)=database$
	fnLbl(2,1,"Grid Name:",20,1)
	fnTxt(2,22,20,20,0,"",1)
	gridinfo$(respc+=1)=gridname$
	fnLbl(3,1,"Column Number:",20,1)
	tt$="Change column # if default not acceptable"
	fnTxt(3,22,2,2,0,"30",0,tt$)
	gridinfo$(respc+=1)=str$(lastcolumn+1)
	fnLbl(5,1,"Grid Options:",14,1)
	x=0
	mat gridname_list$(300)
	mat gridname_list$=("")
	close #16: ioerr ignore
	open #16: 'Name=S:\acs[cursys]\Grid\'&database$&"\"&database$&'.fil',display,input 
	do
		linput #16: ln$ eof L1420
		gridname_list$(x+=1)=trim$(ln$)
	loop
	L1420: ! 
	mat gridname_list$(x)
	close #16: ioerr ignore
	tt$="Highlite any column heading you wish to add to your grid"
	fncomboa("Grrr",5,16,mat gridname_list$,tt$,80)
	mat gridinfo$(respc+=1)
	if udim(mat gridname_list$)=>1 then gridinfo$(respc)=gridname_list$(1) else gridinfo$(respc)=''
	fnCmdKey("&Add Column",1,1)
	fnCmdKey("&Finish",5,0,1)
	fnAcs('',0,mat gridinfo$,ckey) ! data options available
	if ckey=1 then goto AddToGrid
	if ckey=5 then goto GridColumns
goto GridColumns ! /r
AddToGrid: ! r: add items to individual grids
	columnnum=val(gridinfo$(3)(1:3))
	name$=gridinfo$(4)(1:30)
	vname$=gridinfo$(4)(31:50)
	fieldlen=val(gridinfo$(4)(51:54))
	maskinfo$=gridinfo$(4)(55:67)
	abbrev$=trim$(gridinfo$(4)(68:87))
	maskinfo$=uprc$(maskinfo$)
	maskformat$=maskinfo$(3:4) ! determine if pd,c,n etc format
	decimalposition=val(maskinfo$(1:2))
	x=pos(uprc$(name$),"DATE",1)
	if x>0 then itisadate$="Y" else itisadate$="N"
	if trim$(maskformat$)="N" and fieldlen<8 and itisadate$="Y" then 
		colmask$="1" ! DATE IN MMDDYY FORMAT
	else if trim$(maskformat$)="N" and fieldlen>7 and itisadate$="Y" then 
		colmask$="3"
	else if trim$(maskformat$)="PD" and fieldlen>7 and itisadate$="Y" then 
		colmask$="3"! DATE IN CCYYMMDD FORMAT
	else if trim$(maskformat$)="PD" and fieldlen<8 and itisadate$="Y" then 
		colmask$="1" ! DATE IN MMDDYY FORMAT
	else if trim$(maskformat$)="N" and decimalposition=2 and itisadate$="N" then 
		colmask$="10" ! AMERICAN CURRENCY
	else if trim$(maskformat$)="N" and decimalposition>0 and itisadate$="N" then 
		colmask$=str$(30+decimalposition) ! NUMERIC WITH DECIMALS
	else if trim$(maskformat$)="PD" and decimalposition>0 and itisadate$="N" then 
		colmask$="10" ! NUMERIC WITH DECIMALS
	else if (trim$(maskformat$)="N" or trim$(maskformat$)="PD") and decimalposition=0 and itisadate$="N" then 
		colmask$=str$(30+decimalposition) ! NUMERIC WITH DECIMALS
	else if (trim$(maskformat$)="C" or trim$(maskformat$)="G") then 
		colmask$="80" ! NORMAL CHARACTER
	end if 
	read #hgridfile,using fGridFile,key=cnvrt$("pic(zzz)",val(gridinfo$(3))): oldcolumn nokey WriteNewRecord ! CHECK TO SEE IF ALREADY EXITS
goto InsertGridColumn
WriteNewRecord: !
	write #hgridfile,using fGridFile: columnnum,name$,vname$,fieldlen,colmask$,abbrev$
goto DisplayGrid ! /r
InsertGridColumn: ! r: Renumbers the selected grid columns if one is deleted
	read #hgridfile,using fGridFile,last: lastcolumn eof ignore
	! restore #hgridfile: 
	! do
	!   read #hgridfile,using fGridFile: lastcolumn eof L1810
	! loop
	L1810: !
	read #hgridfile,using fGridFile,key=cnvrt$("pic(zzz)",lastcolumn): newcolumn nokey AddToGrid eof AddToGrid
	newcolumn=newcolumn+1
	rewrite #hgridfile,using fGridFile: newcolumn
	lastcolumn=lastcolumn-1
	if lastcolumn>0 and lastcolumn>=columnnum then goto L1810
goto WriteNewRecord ! /r
PrintGrid: ! r: Creates grid lines for prtflex2
	mat item$(80)
	mat item$=("")
	mat colhdr$(80)
	mat colhdr$=("")
	mat colmask$(80)
	mat colmask$=("")
	columns=0
	specline=10010
	dataline=10510
	!____
	! This section generates the program lines needed to create the column            headings and column masks
	close #hgridfile: ioerr ignore
	open #hgridfile:=15: "Name="&fullgridname$&",KFName="&fullgridindx$&",RecL=80,KPs=1,KLn=3,use",internal,outIn,keyed ioerr SelectDataBase
	open #h_gridspecs1:=10: "Name="&env$('temp')&"\GridSpecs1.tmp,RecL=255,Replace",display,output  ! temporary file to hold generated lines for grid specifications
	pr #h_gridspecs1,using F_GRIDSPECS1: "procerr return" ! skip next line if no lines exist
	pr #h_gridspecs1,using F_GRIDSPECS1: "del 10010,10480" ! delete any lines from previous grid
	pr #h_gridspecs1,using F_GRIDSPECS1: "procerr return" ! skip next line if no lines exist
	pr #h_gridspecs1,using F_GRIDSPECS1: "del 10510,10980"
	do
		read #hgridfile,using fGridFile: columnnum,name$,vname$,fieldlen,colmask$,abbrev$ eof L2160
		if vname$(1:1)='(' then vname$(1:1)=''
		columns=columns+1
		pr #h_gridspecs1,using F_GRIDSPECS1: str$(specline)& " colHdr$("&str$(columns)&")="&'"'&trim$(abbrev$)&'"'&" : ColMask$("&str$(columns)&")="&'"'&trim$(colmask$)&'"'
		F_GRIDSPECS1: form pos 1,c 255
		specline=specline+10
		if pos(vname$,"$",1) then  ! determine if numeric or character
			pr #h_gridspecs1,using F_GRIDSPECS1: str$(dataline)& " item$("&str$(columns)&")="&trim$(vname$)
		else 
			pr #h_gridspecs1,using F_GRIDSPECS1: str$(dataline)& " item$("&str$(columns)&")= str$("&trim$(vname$)&")"
		end if
		dataline=dataline+10
	loop
	L2160: ! 
	close #h_gridspecs1: ioerr L2180
	if columns=0 then goto DisplayGrid
	L2180: ! 
	dim new_prtflex2_name$*512
	new_prtflex2_name$=env$('temp')&'\PrtFlex2_'&session$&'.br'
	fnCopy("S:\Core\PrtFlex\PrtFlex2.br",new_prtflex2_name$)
	open #h_gridspecs2:=10: "Name="&env$('temp')&"\GridSpecs2.tmp,RecL=255,Replace",display,output 
	pr #h_gridspecs2,using F_GRIDSPECS1: "PROC NOECHO"
	pr #h_gridspecs2,using F_GRIDSPECS1: "Load "&new_prtflex2_name$ ! S:\Core\PrtFlex\PrtFlex2"
	pr #h_gridspecs2,using F_GRIDSPECS1: "Load "&new_prtflex2_name$ ! S:\Core\PrtFlex\PrtFlex2"
	pr #h_gridspecs2,using F_GRIDSPECS1: "subproc "&env$('temp')&"\gridspecs1.tmp"
	pr #h_gridspecs2,using F_GRIDSPECS1: "SubProc "&open_read$
	pr #h_gridspecs2,using F_GRIDSPECS1: "00210 columns = "&str$(columns)
	pr #h_gridspecs2,using F_GRIDSPECS1: "Replace "&new_prtflex2_name$ ! S:\Core\PrtFlex\PrtFlex2"
	pr #h_gridspecs2,using F_GRIDSPECS1: 'chain "'&new_prtflex2_name$&'"' ! S:\Core\PrtFlex\PrtFlex2"'
	! pr #h_gridspecs2,Using 1980: "PROC ECHO"
	close #h_gridspecs2: 
	open #h_gridname:=11: "Name="&env$('temp')&"\Gridname.tmp,RecL=80,Replace",internal,output,relative 
	write #h_gridname,using 'form pos 1,c 40': gridname$
	close #h_gridname: 
	! write fullgridname$,fullgridIndex,columns to file for prtflex2
	! pr NEWPAGE
execute "proc "&env$('temp')&"\gridspecs2.tmp" ! /r
AddGridName: ! r: Allows you to add columns to your grid
	mat resp$=("")
	fnTos
	fnLbl(1,1,"Grid Name:" ,20,1)
	gridinfo$(2)=gridname$
	fnTxt(1,22,11,11,0,"",0,"Limited to 11 characters!" )
	fnCmdSet(2)
	fnAcs('',0,mat resp$,ckey)
	if ckey=5 then goto GRIDSELECTION
	resp$(1)=trim$(resp$(1))
	resp$(1)=trim$(resp$(1))(1:20)
	close #hgridfile: ioerr ignore
	ckey=1 : goto L780
! /r
DONE: close #1: ioerr ignore
XIT: fnxit
include: ertn
