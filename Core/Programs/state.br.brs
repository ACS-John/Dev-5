! Replace S:\Core\Programs\State
! r: General Setup
	autoLibrary
	dim cap$*128
	fnTop(program$,cap$="State")
	on error goto Ertn
	layoutName$='CO State'
! /r
 
! r: FileIO stuff
	library 'S:\Core\FileIO\fileio': fnReadEntireLayout,fnReadLayoutArrays
	dim Filename$*256
	! takes forever ! fnReadEntireLayout(layoutName$, Filename$,Prefix$,Mat Keys$,Mat KeyDescription$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos)
	fnReadLayoutArrays(Layoutname$,Prefix$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos,FileNumber)
! pause
! /r
! r: hamster stuff
	dim p$(3)*25  ! should be (number of items in file) * longest length
	dim lbl$(3)   ! should contain field descriptions (w/o :s)
	dim fltyp$(3) ! field type
	dim sln(3)    ! Storage Length
	dim mask(3)   ! input mask
	dim fln(3)    ! Field Length
	lbl$(1)="Abreviation"
	lbl$(2)="Name"
	lbl$(3)="Code"
	fln(1)=2
	fln(2)=25
	fln(3)=2
	mask(1)=2000
	mask(3)=30
	hState=fnOpenFile(layoutName$,mat statedata$,mat statedatan,mat form$)
	! open #1: "Name=S:\Core\Data\State.dat,KFName=S:\Core\Data\State.Idx,Use,RecL=29,KPs=1,KLn=2,Shr",internal,outIn,keyed
	fnHamster(layoutName$,mat lbl$,mat fln,hState,mat p$,mat fltyp$,mat sln,mat mask)
goto Xit ! /r
Xit: fnXit
include: ertn
