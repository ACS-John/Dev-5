10000 ! Replace S:\acsPR\adjustpaydate.br
10100 ! This program corrects a pay date in history even after later payrolls have been processed
10200 ! ______________________________________________________________________
10300   library 'S:\Core\Library': fntop,fnxit,fnerror,fncno,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fngethandle
10400   fntop("S:\acsPR\adjustpaydate","Adjust Historical Pay Date")
10500   on error goto ERTN
10600   fn_adjustpaydate
10700 ! ______________________________________________________________________
10800   def fn_adjustpaydate
10900 ! main routine
11000     if fn_getdates then 
11100       fncno(cno,cnam$)
11200       open #(h_prchecks:=fngethandle): "Name=PRmstr\Payrollchecks.h[cno],KFName=PRmstr\checkidx3.h[cno]",internal,outIn,keyed 
11300 CHECKSFORM: form pos 1,n 8,n 3,pd 6
11400       do 
11500         read #h_prchecks,using CHECKSFORM: eno,tdn,prd eof CHECKSDONE
11600         if prd=val(prdate$(1)) then prd=val(prdate$(2))
11700         rewrite #h_prchecks,using CHECKSFORM: eno,tdn,prd
11800       loop 
11900 CHECKSDONE: ! 
12000       close #h_prchecks: 
12100     end if 
12200   fnend  ! fn_adjustpaydate
12300   def fn_getdates
12400 ! gets the old and new payroll dates
12500     dim prdate$(2)*8
12600     fnTos(sn$="getpradjustdates")
12700     mylen=42 : mypos=45
12800     fnLbl(1,1,"Payroll Date to Adjust:",mylen)
12900     fnTxt(1,mypos,10,0,1,"3",0,"Enter the payroll date you want to change.")
13000     prdate$(1)=""
13100     fnLbl(2,1,"New Payroll Date:",mylen)
13200     fnTxt(2,mypos,10,0,1,"3",0,"To reset checks on the above date to a new payroll date, enter it here.")
13300     prdate$(2)=""
13400     fnCmdKey("Next",1,1,0,"Proceed with date adjustment.")
13500     fnCmdKey("Cancel",5,0,1,"Return to menu without changing the payroll date as indicated.")
13600     fnAcs(sn$,0,mat prdate$,ckey)
13700     if ckey=5 then 
13800       fn_getdates=0
13900     else 
14000       if prdate$(1)="00000000" or prdate$(2)="00000000" then let fn_getdates=fn_getdates else let fn_getdates=1
14100     end if 
14200   fnend  ! fn_getdates
14300 XIT: fnxit
14400 ERTN: fnerror(program$,err,line,act$,"NO")
14500   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
14600   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
14700   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
14800 ERTN_EXEC_ACT: execute act$ : goto ERTN
