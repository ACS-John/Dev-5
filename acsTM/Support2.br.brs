autoLibrary
on error goto Ertn
fnTop(program$)
! r: fn_setup_hamster
	mask_pointtwo=32 : mask_number=30
	mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
	textlen_mmddyy=8 : textlen_ccyymmdd=10
	storage_len_mmddyy=6 : storage_len_ccyymmdd=8
	dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40 ! SP(1) - not used
	mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
	fnH2Init

	fnH2AddText("Client ID"       ,6               ,'N'                     ,mask_number   )
	fnH2AddText("Sys#"            ,2               ,'N',0                   ,mask_number   )
	fnH2AddText("System ID"       ,2               ,'C'                                    )
	fnH2AddText("Starting Date"   ,textlen_ccyymmdd,'N',storage_len_ccyymmdd,mask_ccyymmdd )
	fnH2AddText("Time Frame"      ,2               ,'C'                                    )
	fnH2AddText("Ending Date"     ,textlen_ccyymmdd,'N',storage_len_ccyymmdd,mask_ccyymmdd )
	fnH2AddText("Cost to User"    ,10              ,'N'                                    )
	fnH2AddText("Name"            ,50              ,'C'                                    )
	! fnH2AddText("Contact (1)"     ,50              ,'C'                                    )
	! fnH2AddText("Contact (2)"     ,50              ,'C'                                    )
	! fnH2AddText("Contact (3)"     ,50              ,'C'                                    )
	!
	fnH2AddComboF(1,'S:\Core\Data\acsllc\Client.h420',1,5,6,30,'S:\Core\Data\acsllc\Client-Idx.h420',1)
! /r
open #h=1: "Name=S:\Core\Data\acsllc\Support.h420,Version=2,KFName=S:\Core\Data\acsllc\Support-Idx.h420,Use,RecL=246,KPs=1/7,KLn=6/2,Shr",internal,outIn,keyed
! fnHamster("Client",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
fnHamster2("support")
close #h: 
goto Xit
Xit: fnXit
include: ertn
