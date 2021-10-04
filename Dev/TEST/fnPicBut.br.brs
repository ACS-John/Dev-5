 
! gosub TestSimple
gosub TestAcs
end
 
TestSimple: ! r:
	dim filename$*512
	dim default$*512
	default$='' ! 'D:\CM\Stern and Stern\Premier Cardiology (AllData)\all files to date-CM_EDI - work copy.xlsx'
	filename$=''
	hSelect=1 ! fnH
	ope #hSelect: 'Name=OPEN:'&default$&'All documents (*.*) |*.*,RecL=1,Shr',external,input ! ioerr ignore
	if file(hSelect)=0 then
		filename$=os_filename$(file$(hSelect))
		close #hSelect:
	end if
	pr filename$
return ! /r
 
TestAcs: ! r:
	autoLibrary
	dim resp$(30)*512
	fnTop(program$)
	fnTos
	! fnPic(1,1,3,3,'S:\Core\Icon\calendar_icon.png')
	! fnpic(1,1,4,30,"Logo.bmp")
	! fnPicBut(1,1,'Facebook',3,'S:\Core\Icon\calendar_icon.png',2,2)
	! fnPicBut(1,1,'Facebook',3,'S:\Core\Icon\blue_folder.ico',2,2)
	fnPicBut(1,1,'',3,'S:\Core\Icon\facebook.png',2,2)
	fnLbl(6,1,"File:",5,1)
	fnTxt(6,8,40,256,right,"1070",0,"",0 )
	resp$(1)='' !
	fnCmdKey("mmmK",1,1)
	ckey=fnAcs(mat resp$)
	pr resp$(1)
return ! /r
