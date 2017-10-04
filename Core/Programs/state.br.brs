00010 ! Replace S:\Core\Programs\State
20000 ! r: General Setup
20020   library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamster,fnopenfile
20040   dim cap$*128
20060   fntop(program$,cap$="State")
20080   on error goto ERTN
20100   layoutName$='CO State'
20120 ! /r

30000 ! r: FileIO stuff
30020   library 'S:\Core\FileIO\fileio': fnReadEntireLayout,fnReadLayoutArrays
30040   dim Filename$*256
30060   ! takes forever ! fnReadEntireLayout(layoutName$, Filename$,Prefix$,Mat Keys$,Mat KeyDescription$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos)
30070   fnReadLayoutArrays(Layoutname$,Prefix$,Mat Ssubs$,Mat Nsubs$,Mat Sspec$,Mat Nspec$,Mat Sdescription$,Mat Ndescription$,Mat Spos,Mat Npos,FileNumber)
30080 ! pause
30100 ! /r
40000 ! r: hamster stuff
40020   dim p$(3)*25  ! should be (number of items in file) * longest length
40040   dim lbl$(3)   ! should contain field descriptions (w/o :s)
40060   dim fltyp$(3) ! field type
40080   dim sln(3)    ! Storage Length
40100   dim mask(3)   ! input mask
40120   dim fln(3)    ! Field Length
40140   lbl$(1)="Abreviation" 
40160   lbl$(2)="Name" 
40180   lbl$(3)="Code"
40200   fln(1)=2 
40220   fln(2)=25
40240   fln(3)=2
40260   let mask(1)=2000 
40280   let mask(3)=30
40300   hState=fnOpenFile(layoutName$,mat statedata$,mat statedatan,mat form$)
40320   ! open #1: "Name=S:\Core\Data\State.dat,KFName=S:\Core\Data\State.Idx,Use,RecL=29,KPs=1,KLn=2,Shr",internal,outin,keyed 
40340   fnhamster(layoutName$,mat lbl$,mat fln,hState,mat p$,mat fltyp$,mat sln,mat mask)
40360 goto XIT ! /r
70000 XIT: let fnxit
80000 ! <Updateable Region: ERTN>
80020 ERTN: let fnerror(program$,err,line,act$,"xit")
80040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
80060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
80080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
80100 ERTN_EXEC_ACT: execute act$ : goto ERTN
80120 ! /region
