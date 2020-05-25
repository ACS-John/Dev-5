 
! gosub TestSimple
gosub TestAcs
end
 
TestAcs: ! r:
	autoLibrary
	dim resp$(30)*512
	fnTop(program$)
	fnTos
	fnLbl(1,1,"File:",5,1)
	fnTxt(1,8,40,256,right,"1070",0,"",0 )
	resp$(1)='' !
	fnCmdKey("mmmK",1,1)
	fnAcs2(mat resp$,ckey)
	pr resp$(1)
return ! /r
 
TestSimple: ! r:
	dim filename$*512
	dim default$*512
	default$='' ! 'D:\CM\Stern and Stern\Premier Cardiology (AllData)\all files to date-CM_EDI - work copy.xlsx'
	filename$=''
	hSelect=1 ! fngethandle
	ope #hSelect: 'Name=OPEN:'&default$&'All documents (*.*) |*.*,RecL=1,Shr',external,input ! ioerr ignore
	if file(hSelect)=0 then
		filename$=os_filename$(file$(hSelect))
		close #hSelect:
	end if
	pr filename$
return ! /r
 
 
