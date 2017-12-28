00010 ! Replace Test\ApplicantSearch
00020 ! Def Library FNAPPLLICANT_SEARCH(&X$,FILE_NUM;FIXGRID)
00030   library 'S:\Core\Library': fntop,fnapplicant_search,fnTos,fnAcs,fnCmdSet
00040   fntop(program$,"Test Applicant Search")
00050 ! ____________-
00060   open #1: "Name="&env$('Q')&"\EAmstr\Applicant.h1,KFName="&env$('Q')&"\EAmstr\AppIdx1.h1,Shr",internal,outIn,keyed 
00070   fnapplicant_search(x$,1,1)
00080 ! third paramater - 0=don't rebuild, use if possible !:
        !                   1=rebuild !:
        !                   99=rebuild it, but don't show it
00090   pr x$
00100 ! 
00110   stop 
