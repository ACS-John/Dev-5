library program$: fnWorkOrderList
library 'S:\Core\Library': fnxit,fntop
fntop(program$)
fnWorkOrderList('[All]')
fnxit
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnerror,fnopenprn,fncloseprn,fnAcs,fnTos
		library 'S:\Core\Library': fnLbl,fnTxt,fnmsgbox,fncomboa,fnButton,fnFra
		library 'S:\Core\Library': fncmbact
		library 'S:\Core\Library': fnCmdSet,fngethandle
		library 'S:\Core\Library': fncreg_read,fncreg_write
		library 'S:\Core\Library': fnureg_write,fnureg_read
		on error goto Ertn
		! r: dims
		dim resp$(50)*320
		dim nam$*30,line$(5)*100
		! /r
	end if
fnend 
def library fnWorkOrderList(; z$*10)
	if ~setup then let fn_setup
	fnTos(sn$="workorder")
	respc=0
	fnLbl(1,43," ",1,1)
	fnLbl(1,1,"Beginning Date to Review:",32,1)
	fnTxt(1,35,12,0,0,"3",0,"")
	resp$(respc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date to Review:",32,1)
	fnTxt(2,35,12,0,0,"3",0,"")
	resp$(respc+=1)=str$(end_date)
	fnLbl(3,1,"Customer to Print:",32,1)
	fncmbact(3,35,1)
	resp$(respc+=1)=z$
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto PWL_XIT
	beg_date=val(resp$(1)) ! beginning of year
	end_date=val(resp$(2)) ! ending day of year
	askz$=lpad$(trim$(resp$(3)(1:10)),10)
	fnopenprn
	open #h_workorder:=fngethandle: "Name=[Q]\UBmstr\WorkOrder.h[cno],KFName=[Q]\UBmstr\wkIndex.h[cno],Shr",internal,outIn,keyed
	gosub PWL_HDR
	if trim$(askz$)="[All]" or trim$(askz$)="" then 
		restore #h_workorder: 
	else
		restore #h_workorder,key>=askz$&"        ": nokey ignore
	end if
	do
		read #h_workorder,using "form pos 1,c 10,n 8,c 30,5*c 100": wkz$,wkdat,nam$,mat line$ eof PWL_FINIS
		! pr wkz$,wkdat,nam$ : pause
		if trim$(askz$)="[All]" or trim$(askz$)="" or trim$(askz$)=trim$(wkz$) then 
			if beg_date=0 or wkdat=>beg_date then 
				if end_date=0 or wkdate<=end_date then
					pr #255,using "form pos 1,c 10,x 1,pic(zzzz/zz/zz),x 1,c 30": wkz$,wkdat,nam$
					for j=1 to 5
						if trim$(line$(j))<>"" then 
							pr #255,using 'form pos 5,c 100': line$(j) pageoflow PWL_PGOFLOW
						end if
					next j
				end if
			end if
		end if
	loop while trim$(askz$)=trim$(wkz$) or trim$(askz$)="[All]" or trim$(askz$)=""
	PWL_FINIS: ! 
	fncloseprn
	PWL_XIT: ! 
	pgno=0
	close #h_workorder: 
fnend
PWL_HDR: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc {\f181 {\fs24 {\b Work Order Listing}"
	pr #255: "{\fs20 "&env$('cnam')&"}}}"
	pr #255: "\qc {\fs18 From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
	pr #255: "\ql "
return  ! /r
PWL_PGOFLOW: ! r:
	pr #255: newpage
	gosub PWL_HDR
continue  ! /r
include: Ertn
