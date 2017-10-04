10000 ! formerly S:\acsPR\Department
10200 ! Department names for payroll
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fntop,fnxit,fnerror,fnhamster
10800   fntop(program$,cap$="Employee Departments")
11000   on error goto ERTN
11200 ! ______________________________________________________________________
11400   dim cap$*128,mask(1),p$(1)*25,lbl$(1)*40
11600 ! ______________________________________________________________________
12000   add_count=0
12200   fn_add('Emp',8, '',0,30)       ! used
12400   fn_add('Dept',3)                ! used
12600   fn_add('gl no 1',3)
12800   fn_add('gl no 2',6)
13000   fn_add('gl no 3',3)
13200   fn_add('last review date',6)
13400   fn_add('next review date',6)
13600   fn_add('last increase date',6)
13800   fn_add('last payroll date',6)    !  used
14000   fn_add('state code',2)
14200   fn_add('workmans comp code',2)
14400   fn_add('union code',2)
14600   fn_add('Salary',10, 'PD',4.2)
14800   fn_add('Hourly Rate - Regular',10, 'PD',4.2)
15000   fn_add('Hourly Rate - Overtime',10, 'PD',4.2)
15200   fn_add('Misc 1 ',10, 'PD',4.2)
15400   fn_add('Misc 2 ',10, 'PD',4.2)
15600   fn_add('Misc 3 ',10, 'PD',4.2)
15800   fn_add('Misc 4 ',10, 'PD',4.2)
16000   fn_add('Misc 5 ',10, 'PD',4.2)
16200   fn_add('Misc 6 ',10, 'PD',4.2)
16400   fn_add('Misc 7 ',10, 'PD',4.2)
16600   fn_add('Misc 8 ',10, 'PD',4.2)
16800   fn_add('Misc 9 ',10, 'PD',4.2)
17000   fn_add('Misc 10',10, 'PD',4.2)
17200   fn_add('Misc 11',10, 'PD',4.2)
17400   fn_add('Misc 12',10, 'PD',4.2)
17600   fn_add('Misc 13',10, 'PD',4.2)
17800   fn_add('Misc 14',10, 'PD',4.2)
18000   fn_add('Misc 15',10, 'PD',4.2)
18200   fn_add('Misc 16',10, 'PD',4.2)
18400   fn_add('Misc 17',10, 'PD',4.2)
18600   fn_add('Misc 18',10, 'PD',4.2)
18800   fn_add('Misc 19',10, 'PD',4.2)
19000   fn_add('Misc 20',10, 'PD',4.2)
19210   open #1: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",use,RecL=149,kps=1/9,kln=8/3,Shr",internal,outin,keyed 
19400   fnhamster("department",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
19600   close #1: 
19800   execute "Index "&env$('Q')&"\PRmstr\department.h"&env$('cno')&' '&env$('Q')&"\PRmstr\deptidx.h"&env$('cno')&" 1 11,Replace" ioerr XIT
20000 XIT: let fnxit
20200   def fn_add(lbl$*40,fln; field_type$,storage_length,mask)
20400     add_count+=1
20600     mat lbl$(add_count)
20800     mat fln(add_count)
21000     mat p$(add_count)
21200     mat fltyp$(add_count)
21400     mat sln(add_count)
21600     mat mask(add_count)
21800     let lbl$(add_count)=lbl$
22000     let fln(add_count)=fln
22200     let fltyp$(add_count)=field_type$
22400     let sln(add_count)=storage_length
22600     let mask(add_count)=mask
22800   fnend 
80000 ! ______________________________________________________________________
80190 ! <Updateable Region: ERTN>
80200 ERTN: let fnerror(program$,err,line,act$,"xit")
80210   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
80220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
80230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
80240 ERTN_EXEC_ACT: execute act$ : goto ERTN
80250 ! /region
80260 ! ______________________________________________________________________
